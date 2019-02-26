limited with Rho.Context;

with Rho.Matrices;
with Rho.Object;

package Rho.Shaders is

   type Rho_Shader_Type is (Fragment_Shader, Vertex_Shader);

   type Shader_Array is array (Positive range <>) of Rho_Shader_Id;

   type Rho_Shader_Interface is interface;

   procedure Activate (Shader : in out Rho_Shader_Interface) is abstract;
   procedure Deactivate (Shader : in out Rho_Shader_Interface) is abstract;
   function Context
     (Shader : Rho_Shader_Interface)
      return not null access Rho.Context.Rho_Context_Record'Class
      is abstract;

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

   function Exists
     (Shader_Value : Rho_Shader_Value_Record)
      return Boolean;

   function Is_Attribute
     (Shader_Value : Rho_Shader_Value_Record)
      return Boolean
     with Post => not (Is_Attribute'Result and then Shader_Value.Is_Uniform);

   function Is_Uniform
     (Shader_Value : Rho_Shader_Value_Record)
      return Boolean
     with Post => not (Is_Uniform'Result and then Shader_Value.Is_Attribute);

   function Attribute_Location
     (Shader_Value : Rho_Shader_Value_Record'Class)
      return Rho_Attribute_Id
     with Pre => Shader_Value.Exists
     and then Shader_Value.Is_Attribute;

   function Uniform_Location
     (Shader_Value : Rho_Shader_Value_Record'Class)
      return Rho_Uniform_Id
     with Pre => Shader_Value.Exists
     and then Shader_Value.Is_Uniform;

private

   type Shader_Value_Class is (Uniform, Attribute);

   type Rho_Shader_Value_Record is new Rho.Object.Rho_Object_Record with
      record
         Class   : Shader_Value_Class;
         Shader  : Rho_Shader;
         Loc     : Rho_Shader_Location_Id;
         Context : access Rho.Context.Rho_Context_Record'Class;
      end record;

   function Exists
     (Shader_Value : Rho_Shader_Value_Record)
      return Boolean
   is (Shader_Value.Loc >= 0);

   function Is_Attribute
     (Shader_Value : Rho_Shader_Value_Record)
      return Boolean
   is (Shader_Value.Class = Attribute);

   function Is_Uniform
     (Shader_Value : Rho_Shader_Value_Record)
      return Boolean
   is (Shader_Value.Class = Attribute);

   function Attribute_Location
     (Shader_Value : Rho_Shader_Value_Record'Class)
      return Rho_Attribute_Id
   is (Rho_Attribute_Id (Shader_Value.Loc));

   function Uniform_Location
     (Shader_Value : Rho_Shader_Value_Record'Class)
      return Rho_Uniform_Id
   is (Rho_Uniform_Id (Shader_Value.Loc));

end Rho.Shaders;
