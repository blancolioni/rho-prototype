private with Ada.Containers.Vectors;

private with Rho.Limits;

limited with Rho.Materials.Technique;

with Rho.Color;
with Rho.Properties;
with Rho.Render_Target;
with Rho.Shaders.Values;
with Rho.Texture;
with Rho.Value;

package Rho.Materials.Pass is

   type Rho_Material_Pass_Record is
     new Rho.Properties.Rho_Property_Container_Interface
     and Rho.Shaders.Rho_Has_Shader_Interface
   with private;

   type Rho_Material_Pass is access all Rho_Material_Pass_Record'Class;

   procedure Rho_New
     (Pass      : in out Rho_Material_Pass;
      Technique : not null access
        Rho.Materials.Technique.Rho_Technique_Record'Class);

   function Technique
     (Pass : Rho_Material_Pass_Record'Class)
      return access Rho.Materials.Technique.Rho_Technique_Record'Class;

   procedure Activate
     (Pass     : in out Rho_Material_Pass_Record;
      Target   : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class);

   procedure Deactivate
     (Pass     : in out Rho_Material_Pass_Record;
      Target   : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class);

   function Lighting_Enabled
     (Pass : Rho_Material_Pass_Record)
      return Boolean;

   procedure Set_Lighting_Enabled
     (Pass    : in out Rho_Material_Pass_Record;
      Enabled : Boolean);

   function Has_Ambient_Light
     (Pass : Rho_Material_Pass_Record)
      return Boolean;

   function Has_Diffuse_Light
     (Pass : Rho_Material_Pass_Record)
      return Boolean;

   function Has_Emissive_Light
     (Pass : Rho_Material_Pass_Record)
      return Boolean;

   function Has_Specular_Light
     (Pass : Rho_Material_Pass_Record)
      return Boolean;

   function Ambient
     (Pass : Rho_Material_Pass_Record)
      return Rho.Color.Rho_Color;

   procedure Set_Ambient
     (Pass    : in out Rho_Material_Pass_Record'Class;
      Color   : Rho.Color.Rho_Color);

   function Diffuse
     (Pass : Rho_Material_Pass_Record)
      return Rho.Color.Rho_Color;

   procedure Set_Diffuse
     (Pass    : in out Rho_Material_Pass_Record;
      Color   : Rho.Color.Rho_Color);

   function Emissive
     (Pass : Rho_Material_Pass_Record)
      return Rho.Color.Rho_Color;

   procedure Set_Emissive
     (Pass    : in out Rho_Material_Pass_Record;
      Color   : Rho.Color.Rho_Color);

   function Specular
     (Pass : Rho_Material_Pass_Record)
      return Rho.Color.Rho_Color;

   procedure Set_Specular
     (Pass     : in out Rho_Material_Pass_Record;
      Value    : Rho.Color.Rho_Color);

   function Has_Shininess
     (Pass : Rho_Material_Pass_Record)
      return Boolean;

   function Shininess
     (Pass : Rho_Material_Pass_Record)
      return Rho_Float;

   procedure Set_Shininess
     (Pass  : in out Rho_Material_Pass_Record;
      Value : Non_Negative_Float);

   overriding function Get_Shader
     (Pass : in out Rho_Material_Pass_Record)
      return Rho.Shaders.Rho_Shader;

   overriding function Has_Shader
     (Pass : Rho_Material_Pass_Record)
      return Boolean
   is (True);

   overriding procedure Set_Shader
     (Pass   : in out Rho_Material_Pass_Record;
      Shader : Rho.Shaders.Rho_Shader);

   function Texture
     (Pass : Rho_Material_Pass_Record'Class)
      return Rho.Texture.Rho_Texture;

   procedure Set_Texture
     (Pass    : in out Rho_Material_Pass_Record'Class;
      Texture : Rho.Texture.Rho_Texture);

   function Texture_Address_Mode
     (Pass : Rho_Material_Pass_Record'Class)
      return Rho.Texture.Texture_Address_Mode;

   procedure Set_Texture_Address_Mode
     (Pass : in out Rho_Material_Pass_Record'Class;
      Mode : Rho.Texture.Texture_Address_Mode);

--     procedure Set_Texture_Scale
--       (Pass    : in out Rho_Material_Pass_Record'Class;
--        U, V    : Rho_Float);
--
--     procedure Get_Texture_Scale
--       (Pass    : Rho_Material_Pass_Record'Class;
--        U, V    : out Rho_Float);

   function Position_Attribute
     (Pass : Rho_Material_Pass_Record'Class)
      return Rho.Shaders.Values.Rho_Attribute_Value;

   function Has_Normal_Attribute
     (Pass : Rho_Material_Pass_Record'Class)
      return Boolean;

   function Normal_Attribute
     (Pass : Rho_Material_Pass_Record'Class)
      return Rho.Shaders.Values.Rho_Attribute_Value;

   function Has_Texture_Coordinate_Attribute
     (Pass : Rho_Material_Pass_Record'Class)
      return Boolean;

   function Texture_Coordinate_Attribute
     (Pass : Rho_Material_Pass_Record'Class)
      return Rho.Shaders.Values.Rho_Attribute_Value;

   function Has_Color_Attribute
     (Pass : Rho_Material_Pass_Record'Class)
      return Boolean;

   function Color_Attribute
     (Pass : Rho_Material_Pass_Record'Class)
      return Rho.Shaders.Values.Rho_Attribute_Value
     with Pre => Pass.Has_Color_Attribute;

   function Instanced_Position_Attribute
     (Pass : Rho_Material_Pass_Record'Class)
      return Rho.Shaders.Values.Rho_Attribute_Value;

   function Instanced_Color_Attribute
     (Pass : Rho_Material_Pass_Record'Class)
      return Rho.Shaders.Values.Rho_Attribute_Value;

   procedure Alpha_Discard
     (Pass     : in out Rho_Material_Pass_Record'Class;
      Operator : Material_Operator;
      Value    : Rho.Unit_Float);

   procedure Color_Discard
     (Pass     : in out Rho_Material_Pass_Record'Class;
      Value    : Rho.Color.Rho_Color);

   function Polygon_Mode
     (Pass : Rho_Material_Pass_Record'Class)
      return Material_Polygon_Mode;

   procedure Set_Polygon_Mode
     (Pass : in out Rho_Material_Pass_Record'Class;
      Mode : Material_Polygon_Mode);

   procedure Load
     (Pass : in out Rho_Material_Pass_Record'Class);

   function Instantiate
     (Pass      : not null access Rho_Material_Pass_Record'Class;
      Technique : not null access
        Rho.Materials.Technique.Rho_Technique_Record'Class)
      return Rho_Material_Pass;

   overriding function Has_Property
     (Pass      : Rho_Material_Pass_Record;
      Name      : String)
      return Boolean;

   overriding function Get_Property
     (Pass      : Rho_Material_Pass_Record;
      Name      : String)
      return Rho.Value.Rho_Value;

   overriding procedure Set_Property
     (Pass      : in out Rho_Material_Pass_Record;
      Name      : String;
      Value     : Rho.Value.Rho_Value);

private

   package Attribute_Bindings is
     new Ada.Containers.Vectors
       (Positive, Rho.Shaders.Values.Rho_Attribute_Value,
        Rho.Shaders.Values."=");

   package Uniform_Bindings is
     new Ada.Containers.Vectors
       (Positive, Rho.Shaders.Values.Rho_Uniform_Value,
        Rho.Shaders.Values."=");

   type Light_Uniforms is
      record
         Position            : Rho.Shaders.Values.Rho_Uniform_Value;
         Intensities         : Rho.Shaders.Values.Rho_Uniform_Value;
         Attenuation         : Rho.Shaders.Values.Rho_Uniform_Value;
         Ambient_Coefficient : Rho.Shaders.Values.Rho_Uniform_Value;
      end record;

   type Array_Of_Light_Uniforms is
     array (Rho.Limits.Light_Index) of Light_Uniforms;

   type Condition_Type is (Alpha_Discard, Color_Discard);

   type Condition_Record is
      record
         Condition : Condition_Type;
         Operator  : Material_Operator;
         Value     : Rho.Value.Rho_Value;
      end record;

   package Condition_Vectors is
     new Ada.Containers.Vectors (Positive, Condition_Record);

   type Rho_Material_Pass_Record is
     new Rho.Properties.Rho_Property_Container_Interface
     and Rho.Shaders.Rho_Has_Shader_Interface with
      record
         Technique          : access
           Rho.Materials.Technique.Rho_Technique_Record'Class;
         Instantiation      : Rho_Material_Pass;
         Lighting           : Boolean := False;
         Has_Ambient        : Boolean := False;
         Has_Diffuse        : Boolean := False;
         Has_Emissive       : Boolean := False;
         Has_Shininess      : Boolean := False;
         Has_Specular       : Boolean := False;
         Has_Texture        : Boolean := False;
         Properties         : Rho.Properties.Rho_Property_Container;
         Texture            : Rho.Texture.Rho_Texture := null;
         Texture_Scale_U    : Rho_Float;
         Texture_Scale_V    : Rho_Float;
         Tex_Address_Mode   : Rho.Texture.Texture_Address_Mode :=
                                Rho.Texture.Wrap;
         Ambient            : Rho.Color.Rho_Color  := (0.0, 0.0, 0.0, 0.0);
         Diffuse            : Rho.Color.Rho_Color  := (1.0, 1.0, 1.0, 1.0);
         Emissive           : Rho.Color.Rho_Color  := (0.0, 0.0, 0.0, 0.0);
         Specular           : Rho.Color.Rho_Color  := (0.0, 0.0, 0.0, 0.0);
         Shininess          : Non_Negative_Float := 1.0;
         Conditions         : Condition_Vectors.Vector;
         Polygon_Mode       : Material_Polygon_Mode := Solid;
         Shader             : Rho.Shaders.Rho_Shader := null;
         Texture_Uniform    : Rho.Shaders.Values.Rho_Uniform_Value := null;
         Shininess_Uniform  : Rho.Shaders.Values.Rho_Uniform_Value;
         Specular_Uniform   : Rho.Shaders.Values.Rho_Uniform_Value;
         Camera_Uniform     : Rho.Shaders.Values.Rho_Uniform_Value;
         Attributes         : Attribute_Bindings.Vector;
         Parameter_Uniforms : Uniform_Bindings.Vector;
         Lights             : Array_Of_Light_Uniforms;
      end record;

   procedure Create_Shader
     (Pass : in out Rho_Material_Pass_Record'Class);

   procedure Instantiate_Shader
     (Pass : in out Rho_Material_Pass_Record'Class);

   procedure Bind_Standard_Attributes
     (Pass : in out Rho_Material_Pass_Record'Class);

end Rho.Materials.Pass;
