with Rho.Materials.Pass;

package body Rho.Materials.Material is

   --------------
   -- Activate --
   --------------

   procedure Activate
     (Material : in out Rho_Material_Record;
      Target   : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class)
   is
      pragma Unreferenced (Material);
      pragma Unreferenced (Target);
   begin
      null;
   end Activate;

   -------------------
   -- Add_Parameter --
   -------------------

   procedure Add_Parameter
     (Material       : in out Rho_Material_Record;
      Name           : String;
      Parameter_Type : Rho.Value.Value_Type)
   is
   begin
      Material.Parameters.Append
        ((Parameter_Name => Ada.Strings.Unbounded.To_Unbounded_String (Name),
          Value          => Rho.Value.Default_Value (Parameter_Type)));
   end Add_Parameter;

   ----------------
   -- Deactivate --
   ----------------

   procedure Deactivate
     (Material : in out Rho_Material_Record;
      Target   : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class)
   is
      pragma Unreferenced (Material);
      pragma Unreferenced (Target);
   begin
      null;
   end Deactivate;

   -----------------
   -- Instantiate --
   -----------------

   function Instantiate
     (Material : not null access Rho_Material_Record'Class)
      return Rho_Material
   is
      Result : constant Rho_Material :=
                 new Rho_Material_Record'Class'(Material.all);
   begin
      for I in 1 .. Result.Technique_Count loop
         Result.Techniques (I) :=
           Result.Techniques.Element (I).Instantiate (Result);
      end loop;
      Result.Instantiation := Rho_Material (Material);
      Result.Set_Name (Material.Name & "[i]");
      Result.Loaded := False;
      return Result;
   end Instantiate;

   ----------
   -- Load --
   ----------

   procedure Load
     (Material : in out Rho_Material_Record)
   is
   begin
      if Material.Instantiation /= null then
         if not Material.Instantiation.Loaded then
            Material.Instantiation.Load;
         end if;
      end if;

      for Technique of Material.Techniques loop
         Technique.Load;
      end loop;

      Material.Loaded := True;
   end Load;

   ------------
   -- Loaded --
   ------------

   function Loaded
     (Material : Rho_Material_Record)
      return Boolean
   is
   begin
      return Material.Loaded;
   end Loaded;

   -------------------
   -- New_Technique --
   -------------------

   function New_Technique
     (Material : not null access Rho_Material_Record'Class)
      return Rho.Materials.Technique.Rho_Technique
   is
      Technique : Rho.Materials.Technique.Rho_Technique;
   begin
      Rho.Materials.Technique.Rho_New (Technique, Material);
      Material.Techniques.Append (Technique);
      return Technique;
   end New_Technique;

   ---------------------
   -- Parameter_Value --
   ---------------------

   function Parameter_Value
     (Material : Rho_Material_Record'Class;
      Index    : Positive)
      return Rho.Value.Rho_Value
   is
   begin
      return Material.Parameters.Element (Index).Value;
   end Parameter_Value;

   ------------
   -- Rho_New --
   ------------

   procedure Rho_New (Material : in out Rho_Material) is
   begin
      Material := new Rho_Material_Record;
   end Rho_New;

   --------------------------
   -- Rho_New_With_Defaults --
   --------------------------

   function Rho_New_With_Defaults
     return Rho_Material
   is
      Material  : Rho_Material;
      Technique : Rho.Materials.Technique.Rho_Technique;
      Pass      : Rho.Materials.Pass.Rho_Material_Pass;
      pragma Unreferenced (Pass);
   begin
      Rho_New (Material);
      Technique := Material.New_Technique;
      Pass := Technique.New_Pass;
      return Material;
   end Rho_New_With_Defaults;

   -------------------------
   -- Rho_New_With_Texture --
   -------------------------

   function Rho_New_With_Texture
     (Name     : String;
      Texture  : Rho.Texture.Rho_Texture;
      Lighting : Boolean := True)
      return Rho_Material
   is
      Material : constant Rho_Material :=
                   Rho_New_With_Defaults;
   begin
      Material.Set_Name (Name);
      Material.Technique (1).Pass (1).Set_Texture (Texture);
      Material.Technique (1).Pass (1).Set_Lighting_Enabled (Lighting);
      return Material;
   end Rho_New_With_Texture;

   ---------------------
   -- Scan_Parameters --
   ---------------------

   procedure Scan_Parameters
     (Material : Rho_Material_Record'Class;
      Process  : not null access
        procedure (Parameter_Name : String;
                   Parameter_Value : Rho.Value.Rho_Value))
   is
   begin
      for Parameter of Material.Parameters loop
         Process (Ada.Strings.Unbounded.To_String
                  (Parameter.Parameter_Name),
                  Parameter.Value);
      end loop;
   end Scan_Parameters;

   -------------------------
   -- Set_Parameter_Value --
   -------------------------

   procedure Set_Parameter_Value
     (Material       : in out Rho_Material_Record;
      Parameter_Name : String;
      Value          : Rho.Value.Rho_Value)
   is
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin
      for I in 1 .. Material.Parameters.Last_Index loop
         declare
            Parameter : Parameter_Record renames Material.Parameters (I);
         begin
            if Parameter.Parameter_Name = Parameter_Name then
               Parameter.Value := Value;
               return;
            end if;
         end;
      end loop;
      raise Constraint_Error with
        "no parameter name '" & Parameter_Name & "' in material "
        & Material.Name;
   end Set_Parameter_Value;

   ---------------
   -- Technique --
   ---------------

   function Technique
     (Material : Rho_Material_Record'Class;
      Index    : Positive)
      return Rho.Materials.Technique.Rho_Technique
   is
   begin
      return Material.Techniques.Element (Index);
   end Technique;

   ---------------------
   -- Technique_Count --
   ---------------------

   function Technique_Count
     (Material : Rho_Material_Record'Class)
      return Natural
   is
   begin
      return Material.Techniques.Last_Index;
   end Technique_Count;

end Rho.Materials.Material;
