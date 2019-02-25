package body Rho.Materials.Technique is

   --------------
   -- Activate --
   --------------

   procedure Activate
     (Technique : in out Rho_Technique_Record'Class;
      Target    : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class)
   is
      pragma Unreferenced (Technique);
      pragma Unreferenced (Target);
   begin
      null;
   end Activate;

   ----------------
   -- Deactivate --
   ----------------

   procedure Deactivate
     (Technique : in out Rho_Technique_Record'Class;
      Target    : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class)
   is
      pragma Unreferenced (Technique);
      pragma Unreferenced (Target);
   begin
      null;
   end Deactivate;

   -----------------
   -- Instantiate --
   -----------------

   function Instantiate
     (Technique : not null access Rho_Technique_Record'Class;
      Material  : not null access
        Rho.Materials.Material.Rho_Material_Record'Class)
      return Rho_Technique
   is
   begin
      return Result : constant Rho_Technique :=
        new Rho_Technique_Record'Class'(Technique.all)
      do
         for I in 1 .. Result.Passes.Last_Index loop
            Result.Passes (I) :=
              Result.Passes.Element (I).Instantiate (Result);
         end loop;
         Result.Material := Material;
         Result.Instantiation := Rho_Technique (Technique);
      end return;
   end Instantiate;

   ----------
   -- Load --
   ----------

   procedure Load
     (Technique : in out Rho_Technique_Record'Class)
   is
   begin
      for Pass of Technique.Passes loop
         Pass.Load;
      end loop;
   end Load;

   --------------
   -- Material --
   --------------

   function Material
     (Technique : Rho_Technique_Record'Class)
      return access Rho.Materials.Material.Rho_Material_Record'Class
   is
   begin
      return Technique.Material;
   end Material;

   --------------
   -- New_Pass --
   --------------

   function New_Pass
     (Technique : not null access Rho_Technique_Record'Class)
      return Rho.Materials.Pass.Rho_Material_Pass
   is
      Pass : Rho.Materials.Pass.Rho_Material_Pass;
   begin
      Rho.Materials.Pass.Rho_New (Pass, Technique);
      Technique.Passes.Append (Pass);
      return Pass;
   end New_Pass;

   ----------
   -- Pass --
   ----------

   function Pass
     (Technique : Rho_Technique_Record'Class;
      Index     : Positive)
      return Rho.Materials.Pass.Rho_Material_Pass
   is
   begin
      return Technique.Passes (Index);
   end Pass;

   ----------------
   -- Pass_Count --
   ----------------

   function Pass_Count
     (Technique : Rho_Technique_Record'Class)
      return Natural
   is
   begin
      return Technique.Passes.Last_Index;
   end Pass_Count;

   ------------
   -- Rho_New --
   ------------

   procedure Rho_New
     (Technique : in out Rho_Technique;
      Material  : access Rho.Materials.Material.Rho_Material_Record'Class)
   is
   begin
      Technique := new Rho_Technique_Record;
      Technique.Material := Material;
   end Rho_New;

end Rho.Materials.Technique;
