with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Strings.Fixed;

with Rho.Logging;

with Css.Parser;
with Rho.Toolkit.Widget_Registry;

package body Rho.Toolkit.Builder is

   function Http_Decode
     (Text : String)
      return String;

   function Decode_Http_Escape (Escape : String) return String;

   ------------------------
   -- Decode_Http_Escape --
   ------------------------

   function Decode_Http_Escape (Escape : String) return String is
   begin
      if Escape = "" then
         return Escape;
      elsif Escape (Escape'First) = '#'
        and then Escape'Length > 1
        and then (for all I in Escape'Range =>
                    I = Escape'First or else Escape (I) in '0' .. '9')
      then
         declare
            Code : constant Natural :=
                     Natural'Value (Escape (Escape'First + 1 .. Escape'Last));
         begin
            if Code < 16#007F# then
               return (1 => Character'Val (Code));
            elsif Code < 16#07FF# then
               return (1 => Character'Val (16#C0# + Code / 64),
                       2 => Character'Val (16#80# + Code mod 64));
            elsif Code < 16#FFFF# then
               return (1 => Character'Val (16#E0# + Code / 4096),
                       2 => Character'Val (16#80# + Code / 64 mod 64),
                       3 => Character'Val (16#80# + Code mod 64));
            else
               return (1 => Character'Val (16#F0# + Code / 64 / 4096),
                       2 => Character'Val (16#80# + Code / 4096 mod 64),
                       3 => Character'Val (16#80# + Code / 64 mod 64),
                       4 => Character'Val (16#80# + Code mod 64));
            end if;
         end;
      else
         return '&' & Escape & ';';
      end if;
   end Decode_Http_Escape;

   ---------------
   -- Full_Path --
   ---------------

   function Full_Path
     (Builder : Rho_Builder_Record'Class;
      Path    : String)
      return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Builder.Containing_Path)
        & "/" & Path;
   end Full_Path;

   ---------
   -- Get --
   ---------

   function Get
     (Builder : Rho_Builder_Record'Class;
      Name    : String)
      return Rho.Toolkit.Widget.Rho_Widget
   is
   begin
      if Builder.Widgets.Contains (Name) then
         return Builder.Widgets.Element (Name);
      else
         return null;
      end if;
   end Get;

   -----------------
   -- Http_Decode --
   -----------------

   function Http_Decode
     (Text : String)
      return String
   is
      function Decode
        (S     : String)
         return String;

      ------------
      -- Decode --
      ------------

      function Decode
        (S     : String)
         return String
      is
         Index  : Positive := S'First;
      begin
         while Index <= S'Last loop
            declare
               Ch : constant Character := S (Index);
            begin
               if Ch = '&' then
                  declare
                     Start : constant Positive := Index;
                  begin
                     Index := Index + 1;
                     while Index <= S'Last
                       and then S (Index) /= ';'
                     loop
                        Index := Index + 1;
                     end loop;
                     if Index <= S'Last then
                        declare
                           Front : constant String :=
                                     S (S'First .. Start - 1);
                           Middle : constant String :=
                                      Decode_Http_Escape
                                        (S (Start + 1 .. Index - 1));
                           Back   : constant String :=
                                      Decode (S (Index + 1 .. S'Last));
                        begin
                           return Front & Middle & Back;
                        end;
                     else
                        return S;
                     end if;
                  end;
               end if;
            end;
            Index := Index + 1;
         end loop;
         return S;
      end Decode;

   begin
      return Decode (Text);
   end Http_Decode;

   ----------
   -- Load --
   ----------

   procedure Load
     (Builder : in out Rho_Builder_Record'Class;
      Node    : not null access Partoe.DOM.Root_Partoe_Node'Class;
      Parent  : Rho.Toolkit.Widget.Rho_Widget;
      Index   : Natural := 0)
   is
      pragma Assert (Node.Name /= "");

      function Apply_Index (Value : String) return String;

      -----------------
      -- Apply_Index --
      -----------------

      function Apply_Index (Value : String) return String is
         use Ada.Strings.Fixed;
      begin
         if Index > 0 then
            declare
               Hash_Index : constant Natural :=
                              Ada.Strings.Fixed.Index (Value, "#");
            begin
               if Hash_Index > 0 then
                  return Value (Value'First .. Hash_Index - 1)
                    & Trim (Positive'Image (Index), Ada.Strings.Left)
                    & Value (Hash_Index + 1 .. Value'Last);
               else
                  return Value;
               end if;
            end;
         else
            return Value;
         end if;
      end Apply_Index;

   begin
      if Node.Name = "html" then
         Builder.Load_Children (Node, Parent);
      elsif Node.Name = "head" then
         Builder.Load_Children (Node, Parent);
      elsif Node.Name = "body" then
         Builder.Load_Children (Node, Parent);
      elsif Node.Name = "link" then
         Builder.Load_Link
           (Node.Attribute ("rel").Text,
            Node.Attribute ("href").Text);
      elsif Index = 0 and then Node.Has_Attribute ("data-xtk-repeat") then
         declare
            Repeat_Count : constant Natural :=
                             Natural'Value
                               (Node.Attribute ("data-xtk-repeat").Text);
         begin
            for I in 1 .. Repeat_Count loop
               Builder.Load (Node, Parent, I);
            end loop;
         end;
      else
         declare
            use type Rho.Toolkit.Widget.Rho_Widget;
            Top_Level : constant Boolean :=
                          Rho.Toolkit.Widget.Rho_Widget (Builder.Page)
                          = Parent;
            Inner_Text : constant String :=
                           Http_Decode
                             (Ada.Strings.Fixed.Trim
                                (Node.Text, Ada.Strings.Both));
            Tag : constant String :=
                    (if Node.Has_Attribute ("data-Rho-tag")
                     then Node.Attribute ("data-Rho-tag").Text
                     else Node.Name);
            Widget : constant Rho.Toolkit.Widget.Rho_Widget :=
                       Rho.Toolkit.Widget_Registry.Create
                         (Tag        => Tag,
                          Parent     => Parent,
                          Top_Level  => Top_Level,
                          Inner_Text => Inner_Text);
         begin
            if Widget = null then
               Rho.Logging.Put_Line
                 ("Warning: unrecognised tag: " & Node.Name);
            else
               for I in 1 .. Node.Attribute_Count loop
                  Widget.Set_Attribute (Node.Attribute (I).Name,
                                        Apply_Index (Node.Attribute (I).Text));
               end loop;

               if Widget.Id /= ""
                 and then not Builder.Widgets.Contains (Widget.Id)
               then
                  Builder.Widgets.Insert (Widget.Id, Widget);
               end if;
               Builder.Load_Children (Node, Widget);
            end if;
         end;
      end if;
   end Load;

   -------------------
   -- Load_Children --
   -------------------

   procedure Load_Children
     (Builder : in out Rho_Builder_Record'Class;
      Node    : not null access Partoe.DOM.Root_Partoe_Node'Class;
      Parent  : Rho.Toolkit.Widget.Rho_Widget)
   is
   begin
      for I in 1 .. Node.Child_Count loop
         Builder.Load (Node.Child (I), Parent);
      end loop;
   end Load_Children;

   ---------------
   -- Load_Link --
   ---------------

   procedure Load_Link
     (Builder : in out Rho_Builder_Record'Class;
      Rel     : String;
      Href    : String)
   is
   begin
      if Ada.Characters.Handling.To_Lower (Rel) = "stylesheet" then
         declare
            Path : constant String := Builder.Full_Path (Href);
         begin
            Rho.Logging.Put_Line ("loading: " & Path);
            Css.Parser.Load_Css_File (Path);
         end;
      end if;
   end Load_Link;

   -----------------------
   -- Rho_New_From_File --
   -----------------------

   function Rho_New_From_File
     (Path : String)
      return Rho_Builder
   is
      Result : constant Rho_Builder := new Rho_Builder_Record;
   begin
      Result.Containing_Path :=
        Ada.Strings.Unbounded.To_Unbounded_String
          (Ada.Directories.Containing_Directory (Path));
      Result.Document := Partoe.DOM.Load (Path);
      Rho.Toolkit.Page.Rho_New (Result.Page);
      Rho.Toolkit.Widget_Registry.Register_Standard_Widgets;
      Result.Load (Result.Document.Child (1),
                   Rho.Toolkit.Widget.Rho_Widget (Result.Page));
      return Result;
   end Rho_New_From_File;

end Rho.Toolkit.Builder;
