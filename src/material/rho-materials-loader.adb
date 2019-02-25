with Ada.Directories;
with Ada.Strings.Fixed;

with Tropos.Reader;

with Rho.Color;

with Rho.Materials.Technique;
with Rho.Materials.Pass;

with Rho.Texture;

with Rho.Value;

with Rho.Context;

package body Rho.Materials.Loader is

   ----------
   -- Load --
   ----------

   function Load
     (Context : not null access Rho.Context.Rho_Context_Record'Class;
      Path    : String)
      return Rho.Materials.Material.Rho_Material
   is
      use Rho.Materials.Material;

      Material : constant Rho_Material :=
                   Rho.Materials.Material.Rho_New_With_Defaults
                     (Context);
      Technique : constant Rho.Materials.Technique.Rho_Technique :=
                    Material.Technique (1);
      Pass      : constant Rho.Materials.Pass.Rho_Material_Pass :=
                    Technique.Pass (1);

      Config   : constant Tropos.Configuration :=
                   Tropos.Reader.Read_Config (Path);

      function Color_Coord
        (Config_Color : Tropos.Configuration;
         Index         : Positive;
         Default       : Unit_Float := 0.0)
         return Unit_Float;

      function Config_Color
        (Child_Name : String)
         return Rho.Color.Rho_Color;

      procedure Configure_Color
        (Child_Name : String;
         Property_Name : String;
         Constant_Set  : not null access
           procedure (Pass  : in out
                        Rho.Materials.Pass.Rho_Material_Pass_Record'Class;
                      Color : Rho.Color.Rho_Color));

      procedure Configure_Parameters
        (Parameter_Config : Tropos.Configuration);

      -----------------
      -- Color_Coord --
      -----------------

      function Color_Coord
        (Config_Color : Tropos.Configuration;
         Index         : Positive;
         Default       : Unit_Float := 0.0)
         return Unit_Float
      is
      begin
         if Index <= Config_Color.Child_Count then
            declare
               F : constant Float := Config_Color.Get (Index);
            begin
               return Clamp (Rho_Float (F), 0.0, 1.0);
            end;
         elsif Config_Color.Child_Count > 0 then
            return Color_Coord (Config_Color, Config_Color.Child_Count);
         else
            return Default;
         end if;
      end Color_Coord;

      ------------------
      -- Config_Color --
      ------------------

      function Config_Color
        (Child_Name : String)
         return Rho.Color.Rho_Color
      is
         Child : constant Tropos.Configuration :=
                   Config.Child (Child_Name);
      begin
         return (Red => Color_Coord (Child, 1),
                 Green => Color_Coord (Child, 2),
                 Blue  => Color_Coord (Child, 3),
                 Alpha => Color_Coord (Child, 4, 1.0));
      end Config_Color;

      ---------------------
      -- Configure_Color --
      ---------------------

      procedure Configure_Color
        (Child_Name    : String;
         Property_Name : String;
         Constant_Set  : not null access
           procedure (Pass  : in out
                        Rho.Materials.Pass.Rho_Material_Pass_Record'Class;
                      Color : Rho.Color.Rho_Color))
      is
      begin
         if not Config.Contains (Child_Name) then
            return;
         end if;

         declare
            Child : constant Tropos.Configuration := Config.Child (Child_Name);
         begin
            if Child.Child_Count = 1 then
               Pass.Set_Property
                 (Property_Name,
                  Rho.Value.Identifier_Value
                    ("Rho_param_" & Child.Value));
            else
               Constant_Set (Pass.all, Config_Color (Child_Name));
            end if;
         end;
      end Configure_Color;

      --------------------------
      -- Configure_Parameters --
      --------------------------

      procedure Configure_Parameters
        (Parameter_Config : Tropos.Configuration)
      is
      begin
         for Config of Parameter_Config loop
            declare
               Parameter_Name : constant String := Config.Get ("name");
               Type_Name      : constant String := Config.Get ("type");
               Parameter_Type : constant Rho.Value.Value_Type :=
                                  Rho.Value.Value_Type'Value
                                    (Type_Name & "_Value");
            begin
               Material.Add_Parameter (Parameter_Name, Parameter_Type);
            end;
         end loop;
      end Configure_Parameters;

   begin

      Material.Set_Name (Config.Get ("name", Config.Config_Name));

      if Config.Contains ("parameters") then
         Configure_Parameters (Config.Child ("parameters"));
      end if;

      Configure_Color
        ("color", "ambient-color", Rho.Materials.Pass.Set_Ambient'Access);

      Configure_Color
        ("ambient", "ambient-color", Rho.Materials.Pass.Set_Ambient'Access);

      if Config.Contains ("diffuse") then
         Pass.Set_Diffuse (Config_Color ("diffuse"));
      end if;

      if Config.Contains ("emissive") then
         Pass.Set_Emissive (Config_Color ("emissive"));
      end if;

      if Config.Contains ("specular") then
         Pass.Set_Specular (Config_Color ("specular"));
      end if;

      Pass.Set_Lighting_Enabled
        (Config.Contains ("lighting"));

      if Config.Contains ("color-discard") then
         Pass.Color_Discard
           (Config_Color ("color-discard"));
      end if;

      if Config.Contains ("polygon-mode") then
         Pass.Set_Polygon_Mode
           (Material_Polygon_Mode'Value (Config.Get ("polygon-mode")));
      end if;

      if Config.Contains ("texture") then
         declare
            use Ada.Directories;
            Texture_Id : constant String := Config.Get ("texture");
         begin
            if Ada.Strings.Fixed.Index (Texture_Id, "/") = 0 then
               declare
                  Possible_Path : constant String :=
                                    Compose
                                      (Containing_Directory (Path),
                                       Texture_Id);
                  Possible_Indirect_Path : constant String :=
                                             Compose
                                               (Compose
                                                  (Compose
                                                     (Containing_Directory
                                                        (Path),
                                                      ".."),
                                                   "textures"),
                                                Texture_Id);
               begin
                  if Exists (Possible_Path) then
                     Pass.Set_Texture
                       (Rho.Texture.Create_From_Png
                          (Material.Context,
                           Base_Name (Texture_Id), Possible_Path));
                  elsif Exists (Possible_Indirect_Path) then
                     Pass.Set_Texture
                       (Rho.Texture.Create_From_Png
                          (Material.Context,
                           Base_Name (Texture_Id), Possible_Indirect_Path));
                  else
                     Pass.Set_Texture (Context.Texture (Texture_Id));
                  end if;
               end;
            else
               Pass.Set_Texture (Context.Texture (Texture_Id));
            end if;
         end;
      end if;

      return Material;

   end Load;

end Rho.Materials.Loader;
