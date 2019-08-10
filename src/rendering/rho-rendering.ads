private with Ada.Containers.Doubly_Linked_Lists;

with Cairo;

with Rho.Color;
with Rho.Float_Arrays;
with Rho.Float_Buffer;
with Rho.Matrices;
with Rho.Rectangle;
with Rho.Render_Target;
with Rho.Render_Window;
with Rho.Shaders;
with Rho.Texture;

package Rho.Rendering is

   Render_Error : exception;

   type Integer_Array is array (Positive range <>) of Integer;

   type Rho_Renderer_Record is
     abstract new Rho.Matrices.Rho_Matrix_Operation_Record with private;

   procedure Render_Loop
     (Renderer : not null access Rho_Renderer_Record)
   is abstract;

   procedure Exit_Render_Loop
     (Renderer : in out Rho_Renderer_Record)
   is abstract;

   function Create_Top_Level_Window
     (Renderer : in out Rho_Renderer_Record)
      return Rho.Render_Window.Rho_Render_Window
   is abstract;

   function Current_Render_Target
     (Renderer : Rho_Renderer_Record)
      return Rho.Render_Target.Rho_Render_Target
      is abstract;

   procedure Render_One_Frame
     (Renderer : in out Rho_Renderer_Record)
   is abstract;

   function Create_Surface
     (Renderer : in out Rho_Renderer_Record;
      Width, Height : Rho_Float)
      return Cairo.Cairo_Surface
      is abstract;

   function Load_Buffer
     (Renderer : in out Rho_Renderer_Record;
      Buffer   : Rho.Float_Buffer.Rho_Float_Buffer_Record'Class)
      return Rho.Float_Buffer.Rho_Float_Buffer_Id
      is abstract;

   function Load_Texture
     (Renderer : in out Rho_Renderer_Record;
      S_Wrap       : Rho.Texture.Texture_Address_Mode;
      T_Wrap       : Rho.Texture.Texture_Address_Mode;
      Mag_Filter   : Rho.Texture.Texture_Filter_Type)
      return Rho.Texture.Texture_Id
      is abstract;

   procedure Load_Texture_Data_By_External_Id
     (Renderer     : in out Rho_Renderer_Record;
      Texture_Id   : Rho.Texture.Texture_Id;
      External_Id  : String)
   is abstract;

   procedure Load_Texture_Data
     (Renderer     : in out Rho_Renderer_Record;
      Texture_Id   : Rho.Texture.Texture_Id;
      From_Surface : Cairo.Cairo_Surface;
      Region       : Rho.Rectangle.Rho_Rectangle)
   is abstract;

   procedure Load_Texture_Data
     (Renderer     : in out Rho_Renderer_Record;
      Texture_Id   : Rho.Texture.Texture_Id;
      From_Data    : Rho.Color.Rho_Color_2D_Array)
   is abstract;

   function Load_Shader
     (Renderer : in out Rho_Renderer_Record;
      Shader   : Rho.Shaders.Rho_Shader_Type;
      Source   : String)
      return Rho_Shader_Id
      is abstract;

   procedure Use_Shader
     (Renderer : in out Rho_Renderer_Record;
      Shader   : Rho_Program_Id)
   is abstract;

   function Create_Program
     (Renderer : in out Rho_Renderer_Record;
      Shaders  : Rho.Shaders.Shader_Array)
      return Rho_Program_Id
      is abstract;

   function Get_Attribute_Location
     (Renderer : Rho_Renderer_Record;
      Program  : Rho_Program_Id;
      Name     : String)
      return Rho_Attribute_Id
      is abstract;

   function Get_Uniform_Location
     (Renderer : Rho_Renderer_Record;
      Program  : Rho_Program_Id;
      Name     : String)
      return Rho_Uniform_Id
      is abstract;

   function Create_Vertex_Array
     (Renderer       : in out Rho_Renderer_Record)
      return Rho_Vertex_Array_Id
      is abstract;

   procedure Enable_Vertex_Array
     (Renderer : in out Rho_Renderer_Record;
      Id       : Rho_Vertex_Array_Id)
   is abstract;

   procedure Disable_Vertex_Array
     (Renderer : in out Rho_Renderer_Record;
      Id       : Rho_Vertex_Array_Id)
   is abstract;

   procedure Bind_Vertex_Buffer
     (Renderer       : in out Rho_Renderer_Record;
      Attribute      : Rho_Attribute_Id;
      Buffer         : Rho.Float_Buffer.Rho_Float_Buffer;
      Start          : Positive;
      Component_Size : Positive)
   is abstract;

   procedure Set_Uniform_Value
     (Renderer   : in out Rho_Renderer_Record;
      Id         : Rho_Uniform_Id;
      Value      : Integer)
   is abstract;

   procedure Set_Uniform_Value
     (Renderer   : in out Rho_Renderer_Record;
      Id         : Rho_Uniform_Id;
      Value      : Rho_Float)
   is abstract;

   procedure Set_Uniform_Value
     (Renderer   : in out Rho_Renderer_Record;
      Id         : Rho_Uniform_Id;
      Value      : Rho.Float_Arrays.Real_Vector)
   is abstract;

   procedure Set_Uniform_Value
     (Renderer   : in out Rho_Renderer_Record;
      Id         : Rho_Uniform_Id;
      Value      : Rho.Float_Arrays.Real_Matrix)
   is abstract;

   procedure Set_Uniform_Value
     (Renderer   : in out Rho_Renderer_Record;
      Id         : Rho_Uniform_Id;
      Value      : Integer_Array)
   is abstract;

   procedure Set_Uniform_Vector_Array
     (Renderer     : in out Rho_Renderer_Record;
      Id           : Rho_Uniform_Id;
      Element_Size : Positive;
      Value        : Rho.Float_Arrays.Real_Vector)
   is abstract;

   function Matrix_Saved
     (Target : Rho_Renderer_Record'Class;
      Matrix : Rho.Matrices.Matrix_Mode_Type)
      return Boolean;

   procedure Clear_Matrix_Saved
     (Target : in out Rho_Renderer_Record'Class);

   procedure Set_Matrix_Saved
     (Target : in out Rho_Renderer_Record'Class;
      Matrix : Rho.Matrices.Matrix_Mode_Type);

   procedure Save_Matrix
     (Renderer : in out Rho_Renderer_Record;
      Matrix   : Rho.Matrices.Matrix_Mode_Type);

   procedure Push_Shader
     (Target : in out Rho_Renderer_Record;
      Shader : Rho.Shaders.Rho_Shader)
     with Pre => Rho.Shaders."/=" (Shader, null);

   procedure Pop_Shader
     (Target : in out Rho_Renderer_Record);

   function Current_Shader
     (Target : Rho_Renderer_Record)
      return Rho.Shaders.Rho_Shader;

   procedure Activate_Shader
     (Target : in out Rho_Renderer_Record);

   type Rho_Renderer is access all Rho_Renderer_Record'Class;

private

   package List_Of_Shaders is
     new Ada.Containers.Doubly_Linked_Lists
       (Rho.Shaders.Rho_Shader, Rho.Shaders."=");

   type Matrix_Flags is array (Rho.Matrices.Matrix_Mode_Type) of Boolean;

   type Rho_Renderer_Record is
     abstract new Rho.Matrices.Rho_Matrix_Operation_Record with
      record
         Shaders        : List_Of_Shaders.List;
         Active_Shader  : Boolean := False;
         Current_Shader : Rho.Shaders.Rho_Shader;
         Matrix_Saved   : Matrix_Flags := (others => False);
      end record;

end Rho.Rendering;
