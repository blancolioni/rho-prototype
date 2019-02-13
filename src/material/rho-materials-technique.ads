private with Ada.Containers.Vectors;

limited with Rho.Materials.Material;

with Rho.Materials.Pass;
with Rho.Render_Target;

package Rho.Materials.Technique is

   type Rho_Technique_Record is tagged private;

   type Rho_Technique is access all Rho_Technique_Record'Class;

   procedure Rho_New
     (Technique : in out Rho_Technique;
      Material  : access Rho.Materials.Material.Rho_Material_Record'Class);

   function Material
     (Technique : Rho_Technique_Record'Class)
      return access Rho.Materials.Material.Rho_Material_Record'Class;

   procedure Activate
     (Technique : in out Rho_Technique_Record'Class;
      Target    : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class);

   procedure Deactivate
     (Technique : in out Rho_Technique_Record'Class;
      Target    : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class);

   function Pass_Count
     (Technique : Rho_Technique_Record'Class)
      return Natural;

   function Pass
     (Technique : Rho_Technique_Record'Class;
      Index     : Positive)
      return Rho.Materials.Pass.Rho_Material_Pass;

   function New_Pass
     (Technique : not null access Rho_Technique_Record'Class)
      return Rho.Materials.Pass.Rho_Material_Pass;

   procedure Load
     (Technique : in out Rho_Technique_Record'Class);

   function Instantiate
     (Technique : not null access Rho_Technique_Record'Class;
      Material  : not null access
        Rho.Materials.Material.Rho_Material_Record'Class)
      return Rho_Technique;

private

   package Pass_Vectors is
     new Ada.Containers.Vectors
       (Positive, Rho.Materials.Pass.Rho_Material_Pass,
        Rho.Materials.Pass."=");

   type Rho_Technique_Record is tagged
      record
         Material      : access Materials.Material.Rho_Material_Record'Class;
         Instantiation : Rho_Technique;
         Passes        : Pass_Vectors.Vector;
      end record;

end Rho.Materials.Technique;
