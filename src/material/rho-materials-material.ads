private with Ada.Containers.Vectors;
private with Ada.Strings.Unbounded;

limited with Rho.Context;

with Rho.Object;
with Rho.Render_Target;
with Rho.Resource;
with Rho.Texture;
with Rho.Value;

with Rho.Materials.Technique;

package Rho.Materials.Material is

   Rho_Material_Class_Name : constant String := "material";

   type Rho_Material_Record is
     new Rho.Object.Rho_Resource_Record
     and Rho.Resource.Rho_Resource_Interface
   with private;

   overriding function Class_Name
     (Material : Rho_Material_Record)
      return String
   is (Rho_Material_Class_Name);

   function Context
     (Material : Rho_Material_Record'Class)
      return access Rho.Context.Rho_Context_Record'Class;

   procedure Activate
     (Material : in out Rho_Material_Record;
      Target   : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class);

   procedure Deactivate
     (Material : in out Rho_Material_Record;
      Target   : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class);

   function Technique_Count
     (Material : Rho_Material_Record'Class)
      return Natural;

   function Technique
     (Material : Rho_Material_Record'Class;
      Index    : Positive)
      return Rho.Materials.Technique.Rho_Technique;

   function New_Technique
     (Material : not null access Rho_Material_Record'Class)
      return Rho.Materials.Technique.Rho_Technique;

   procedure Load
     (Material : in out Rho_Material_Record);

   function Loaded
     (Material : Rho_Material_Record)
      return Boolean;

   procedure Add_Parameter
     (Material       : in out Rho_Material_Record;
      Name           : String;
      Parameter_Type : Rho.Value.Value_Type);

   procedure Set_Parameter_Value
     (Material : in out Rho_Material_Record;
      Parameter_Name : String;
      Value          : Rho.Value.Rho_Value);

   procedure Scan_Parameters
     (Material : Rho_Material_Record'Class;
      Process  : not null access
        procedure (Parameter_Name : String;
                   Parameter_Value : Rho.Value.Rho_Value));

   function Parameter_Value
     (Material : Rho_Material_Record'Class;
      Index    : Positive)
      return Rho.Value.Rho_Value;

   type Rho_Material is access all Rho_Material_Record'Class;

   function Instantiate
     (Material : not null access Rho_Material_Record'Class)
      return Rho_Material;

   procedure Rho_New
     (Material : in out Rho_Material;
      Context  : not null access Rho.Context.Rho_Context_Record'Class);

   function Rho_New_With_Defaults
     (Context  : not null access Rho.Context.Rho_Context_Record'Class)
      return Rho_Material;
   --  creates the material with 1 selection and 1 pass which does nothing.

   function Rho_New_With_Texture
     (Context  : not null access Rho.Context.Rho_Context_Record'Class;
      Name      : String;
      Texture  : Rho.Texture.Rho_Texture;
      Lighting : Boolean := True)
      return Rho_Material;
   --  creates the material with 1 selection and 1 pass which
   --  applies the texture

private

   type Parameter_Record is
      record
         Parameter_Name : Ada.Strings.Unbounded.Unbounded_String;
         Value          : Rho.Value.Rho_Value;
      end record;

   package Parameter_Vectors is
     new Ada.Containers.Vectors (Positive, Parameter_Record);

   package Technique_Vectors is
     new Ada.Containers.Vectors
       (Positive, Rho.Materials.Technique.Rho_Technique,
        Rho.Materials.Technique."=");

   type Rho_Material_Record is
     new Rho.Object.Rho_Resource_Record
     and Rho.Resource.Rho_Resource_Interface with
      record
         Loaded            : Boolean := False;
         Instantiation     : Rho_Material := null;
         Context           : access Rho.Context.Rho_Context_Record'Class;
         Parameters        : Parameter_Vectors.Vector;
         Techniques        : Technique_Vectors.Vector;
         Instantiated_From : Rho_Material;
      end record;

   function Context
     (Material : Rho_Material_Record'Class)
      return access Rho.Context.Rho_Context_Record'Class
   is (Material.Context);

end Rho.Materials.Material;
