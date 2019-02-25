with Rho.Matrices;
with Rho.Object;

package Rho.Shaders is

   type Rho_Shader_Type is (Fragment_Shader, Vertex_Shader);

   type Shader_Array is array (Positive range <>) of Rho_Shader_Id;

   type Uniform_Integer_Array is
     array (Positive range <>) of Integer;

   type Uniform_Float_Array is
     array (Positive range <>) of Rho_Float;

   type Uniform_Vector_3_Array is
     array (Positive range <>) of Rho.Matrices.Vector_3;

   type Rho_Shader_Interface is interface;

   procedure Activate (Shader : in out Rho_Shader_Interface) is abstract;
   procedure Deactivate (Shader : in out Rho_Shader_Interface) is abstract;

   type Rho_Shader is access all Rho_Shader_Interface'Class;

   type Rho_Has_Shader_Interface is interface;

   function Has_Shader (Item : Rho_Has_Shader_Interface) return Boolean
                        is abstract;

   function Get_Shader
     (Item : in out Rho_Has_Shader_Interface)
      return Rho_Shader
      is abstract;

   procedure Set_Shader
     (Item   : in out Rho_Has_Shader_Interface;
      Shader : Rho_Shader)
   is abstract;

   type Rho_Shader_Value_Record is
     new Rho.Object.Rho_Object_Record with private;

   function Location
     (Shader_Value : Rho_Shader_Value_Record'Class)
      return Natural;

   function Exists
     (Shader_Value : Rho_Shader_Value_Record)
      return Boolean;

--     function Shader
--       (Shader_Value : Rho_Shader_Value_Record'Class)
--        return Rho_Shader;

private

   type Shader_Value_Class is (Uniform, Attribute);

   type Rho_Shader_Value_Record is new Rho.Object.Rho_Object_Record with
      record
         Class    : Shader_Value_Class;
         Shader   : Rho_Shader;
         Location : Uniform_Location_Type;
      end record;

end Rho.Shaders;
