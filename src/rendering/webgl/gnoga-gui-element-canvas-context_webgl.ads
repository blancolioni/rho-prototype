with System.Storage_Elements;
with Ada.Numerics.Generic_Real_Arrays;
private with Ada.Containers.Indefinite_Vectors;

package Gnoga.Gui.Element.Canvas.Context_WebGL is

   pragma Style_Checks (Off);

   type GLuchar is mod 2 ** 8;
   type GLchar is range -128 .. 127;
   type GLint8 is range -2 ** 15 .. 2 ** 15 - 1;
   type GLuint8 is mod 2 ** 16;
   type GLint16 is range -2 ** 15 .. 2 ** 15 - 1;
   type GLuint16 is mod 2 ** 16;
   type GLint is range -2 ** 31 .. 2 ** 31 - 1;
   type GLuint is mod 2 **32;
   type GLenum is new GLint;
   type GLfloat is new Float;
   type GLsizei is new GLuint;
   type GLintptr is new GLuint;
   type GLvoidptr is new System.Address;

   type Float_Array is array (Positive range <>) of GLfloat;
   type Int_Array is array (Positive range <>) of GLint;
   type Uint16_Array is array (Positive range <>) of GLuint16;
   type Uint8_Array is array (Positive range <>) of GLuint8;
   subtype Element_Array is Uint16_Array;
   type CheckedInt32 is array (Positive range <>) of GLint;
   type ColorF is new GLfloat;
   type Texture is new GLuint;
   type BufferOffset is access GLint;
   package Matrices is
     new Ada.Numerics.Generic_Real_Arrays (GLfloat);

   subtype Matrix_4 is
     Matrices.Real_Matrix (1 .. 4, 1 .. 4);

   subtype Matrix_3 is
     Matrices.Real_Matrix (1 .. 3, 1 .. 3);

   subtype Vector_4 is
     Matrices.Real_Vector (1 .. 4);

   subtype Vector_3 is
     Matrices.Real_Vector (1 .. 3);


   type Attrib_Mask is
     (GL_Depth_Buffer_Bit,
      GL_Stencil_Buffer_Bit,
      GL_Color_Buffer_Bit);
   type Attrib_Mask_Array is array (Positive range <>) of Attrib_Mask;

   type Alpha_Function is
     (GL_Never,
      GL_Less,
      GL_Equal,
      GL_Lequal,
      GL_Greater,
      GL_Notequal,
      GL_Gequal,
      GL_Always);

   type Blend_Equation_Mode_EXT is
     (GL_Func_Add,
      GL_Func_Subtract,
      GL_Func_Reverse_Subtract);

   type Buffer_Target_ARB is
     (GL_Array_Buffer,
      GL_Element_Array_Buffer);

   type Buffer_Usage_ARB is
     (GL_Stream_Draw,
      GL_Static_Draw,
      GL_Dynamic_Draw);

   type Clear_Buffer_Mask is
     (GL_Depth_Buffer_Bit,
      GL_Stencil_Buffer_Bit,
      GL_Color_Buffer_Bit);
   type Clear_Buffer_Mask_Array is array (Positive range <>) of Clear_Buffer_Mask;

   type Color_Material_Face is
     (GL_Front,
      GL_Back,
      GL_Front_And_Back);

   type Color_Pointer_Type is
     (GL_Byte,
      GL_Unsigned_Byte,
      GL_Short,
      GL_Unsigned_Short,
      GL_Int,
      GL_Unsigned_Int,
      GL_Float);

   type Cull_Face_Mode is
     (GL_Front,
      GL_Back,
      GL_Front_And_Back);

   type Depth_Function is
     (GL_Never,
      GL_Less,
      GL_Equal,
      GL_Lequal,
      GL_Greater,
      GL_Notequal,
      GL_Gequal,
      GL_Always);

   type Draw_Buffer_Mode is
     (GL_None,
      GL_Front,
      GL_Back,
      GL_Front_And_Back);

   type Draw_Elements_Type is
     (GL_Unsigned_Byte,
      GL_Unsigned_Short,
      GL_Unsigned_Int);

   type Enable_Cap is
     (GL_Cull_Face,
      GL_Depth_Test,
      GL_Stencil_Test,
      GL_Dither,
      GL_Blend,
      GL_Scissor_Test,
      GL_Texture_2d,
      GL_Polygon_Offset_Fill,
      GL_Sample_Alpha_To_Coverage,
      GL_Sample_Coverage);

   type Error_Code is
     (GL_No_Error,
      GL_Invalid_Enum,
      GL_Invalid_Value,
      GL_Invalid_Operation,
      GL_Out_Of_Memory,
      GL_Invalid_Framebuffer_Operation);

   type Fog_Coordinate_Pointer_Type is
     (GL_Float);

   type Fog_Mode is
     (GL_Linear);

   type Fog_Pointer_Type_EXT is
     (GL_Float);

   type Fog_Pointer_Type_IBM is
     (GL_Float);

   type Front_Face_Direction is
     (GL_Cw,
      GL_Ccw);

   type Get_PName is
     (GL_Line_Width,
      GL_Cull_Face,
      GL_Cull_Face_Mode,
      GL_Front_Face,
      GL_Depth_Range,
      GL_Depth_Test,
      GL_Depth_Writemask,
      GL_Depth_Clear_Value,
      GL_Depth_Func,
      GL_Stencil_Test,
      GL_Stencil_Clear_Value,
      GL_Stencil_Func,
      GL_Stencil_Value_Mask,
      GL_Stencil_Fail,
      GL_Stencil_Pass_Depth_Fail,
      GL_Stencil_Pass_Depth_Pass,
      GL_Stencil_Ref,
      GL_Stencil_Writemask,
      GL_Viewport,
      GL_Dither,
      GL_Blend,
      GL_Scissor_Box,
      GL_Scissor_Test,
      GL_Color_Clear_Value,
      GL_Color_Writemask,
      GL_Unpack_Alignment,
      GL_Pack_Alignment,
      GL_Max_Texture_Size,
      GL_Max_Viewport_Dims,
      GL_Subpixel_Bits,
      GL_Red_Bits,
      GL_Green_Bits,
      GL_Blue_Bits,
      GL_Alpha_Bits,
      GL_Depth_Bits,
      GL_Stencil_Bits,
      GL_Texture_2d,
      GL_Polygon_Offset_Units,
      GL_Blend_Color,
      GL_Blend_Equation_Rgb,
      GL_Polygon_Offset_Fill,
      GL_Polygon_Offset_Factor,
      GL_Texture_Binding_2d,
      GL_Sample_Buffers,
      GL_Samples,
      GL_Sample_Coverage_Value,
      GL_Sample_Coverage_Invert,
      GL_Blend_Dst_Rgb,
      GL_Blend_Src_Rgb,
      GL_Blend_Dst_Alpha,
      GL_Blend_Src_Alpha,
      GL_Aliased_Point_Size_Range,
      GL_Aliased_Line_Width_Range,
      GL_Active_Texture,
      GL_Max_Renderbuffer_Size,
      GL_Texture_Binding_Cube_Map,
      GL_Max_Cube_Map_Texture_Size,
      GL_Num_Compressed_Texture_Formats,
      GL_Compressed_Texture_Formats,
      GL_Stencil_Back_Func,
      GL_Stencil_Back_Fail,
      GL_Stencil_Back_Pass_Depth_Fail,
      GL_Stencil_Back_Pass_Depth_Pass,
      GL_Blend_Equation_Alpha,
      GL_Max_Vertex_Attribs,
      GL_Max_Texture_Image_Units,
      GL_Array_Buffer_Binding,
      GL_Element_Array_Buffer_Binding,
      GL_Max_Vertex_Texture_Image_Units,
      GL_Max_Combined_Texture_Image_Units,
      GL_Current_Program,
      GL_Implementation_Color_Read_Type,
      GL_Implementation_Color_Read_Format,
      GL_Stencil_Back_Ref,
      GL_Stencil_Back_Value_Mask,
      GL_Stencil_Back_Writemask,
      GL_Renderbuffer_Binding,
      GL_Num_Shader_Binary_Formats,
      GL_Shader_Compiler,
      GL_Max_Vertex_Uniform_Vectors,
      GL_Max_Varying_Vectors,
      GL_Max_Fragment_Uniform_Vectors);

   type Get_Texture_Parameter is
     (GL_Texture_Mag_Filter,
      GL_Texture_Min_Filter,
      GL_Texture_Wrap_S,
      GL_Texture_Wrap_T);

   type Hint_Mode is
     (GL_Dont_Care,
      GL_Fastest,
      GL_Nicest);

   type Hint_Target is
     (GL_Generate_Mipmap_Hint);

   type Index_Pointer_Type is
     (GL_Short,
      GL_Int,
      GL_Float);

   type Light_Env_Mode_SGIX is
     (GL_Replace);

   type List_Name_Type is
     (GL_Byte,
      GL_Unsigned_Byte,
      GL_Short,
      GL_Unsigned_Short,
      GL_Int,
      GL_Unsigned_Int,
      GL_Float);

   type Logic_Op is
     (GL_Invert);

   type Material_Face is
     (GL_Front,
      GL_Back,
      GL_Front_And_Back);

   type Matrix_Mode is
     (GL_Texture);

   type Normal_Pointer_Type is
     (GL_Byte,
      GL_Short,
      GL_Int,
      GL_Float);

   type Pixel_Format is
     (GL_Unsigned_Short,
      GL_Unsigned_Int,
      GL_Depth_Component,
      GL_Alpha,
      GL_Rgb,
      GL_Rgba,
      GL_Luminance,
      GL_Luminance_Alpha);

   type Internal_Format is
     (GL_Depth_Component,
      GL_Rgb,
      GL_Rgba,
      GL_Rgba4,
      GL_Rgb5_A1,
      GL_Depth_Component16);

   type Pixel_Store_Parameter is
     (GL_Unpack_Alignment,
      GL_Pack_Alignment);

   type Pixel_Tex_Gen_Mode is
     (GL_None,
      GL_Rgb,
      GL_Rgba,
      GL_Luminance,
      GL_Luminance_Alpha);

   type Pixel_Type is
     (GL_Byte,
      GL_Unsigned_Byte,
      GL_Short,
      GL_Unsigned_Short,
      GL_Int,
      GL_Unsigned_Int,
      GL_Float,
      GL_Unsigned_Short_4_4_4_4,
      GL_Unsigned_Short_5_5_5_1);

   type Primitive_Type is
     (GL_Points,
      GL_Lines,
      GL_Line_Loop,
      GL_Line_Strip,
      GL_Triangles,
      GL_Triangle_Strip,
      GL_Triangle_Fan);

   type Read_Buffer_Mode is
     (GL_Front,
      GL_Back);

   type Stencil_Face_Direction is
     (GL_Front,
      GL_Back,
      GL_Front_And_Back);

   type Stencil_Function is
     (GL_Never,
      GL_Less,
      GL_Equal,
      GL_Lequal,
      GL_Greater,
      GL_Notequal,
      GL_Gequal,
      GL_Always);

   type Stencil_Op is
     (GL_Zero,
      GL_Invert,
      GL_Keep,
      GL_Replace,
      GL_Incr,
      GL_Decr,
      GL_Incr_Wrap,
      GL_Decr_Wrap);

   type String_Name is
     (GL_Vendor,
      GL_Renderer,
      GL_Version,
      GL_Extensions,
      GL_Shading_Language_Version);

   type Tex_Coord_Pointer_Type is
     (GL_Short,
      GL_Int,
      GL_Float);

   type Texture_Env_Mode is
     (GL_Blend);

   type Texture_Mag_Filter is
     (GL_Nearest,
      GL_Linear);

   type Texture_Min_Filter is
     (GL_Nearest,
      GL_Linear,
      GL_Nearest_Mipmap_Nearest,
      GL_Linear_Mipmap_Nearest,
      GL_Nearest_Mipmap_Linear,
      GL_Linear_Mipmap_Linear);

   type Texture_Parameter_Name is
     (GL_Texture_Mag_Filter,
      GL_Texture_Min_Filter,
      GL_Texture_Wrap_S,
      GL_Texture_Wrap_T);

   type Texture_Target is
     (GL_Texture_2d,
      GL_Texture_Cube_Map,
      GL_Texture_Cube_Map_Positive_X,
      GL_Texture_Cube_Map_Negative_X,
      GL_Texture_Cube_Map_Positive_Y,
      GL_Texture_Cube_Map_Negative_Y,
      GL_Texture_Cube_Map_Positive_Z,
      GL_Texture_Cube_Map_Negative_Z);

   type Texture_Wrap_Mode is
     (GL_Repeat,
      GL_Clamp_To_Edge);

   type Vertex_Pointer_Type is
     (GL_Short,
      GL_Int,
      GL_Float);

   type Framebuffer_Attachment is
     (GL_Color_Attachment0,
      GL_Depth_Attachment);

   type Renderbuffer_Target is
     (GL_Renderbuffer);

   type Framebuffer_Target is
     (GL_Framebuffer);

   type Texture_Unit is
     (GL_Texture0,
      GL_Texture1,
      GL_Texture2,
      GL_Texture3,
      GL_Texture4,
      GL_Texture5,
      GL_Texture6,
      GL_Texture7,
      GL_Texture8,
      GL_Texture9,
      GL_Texture10,
      GL_Texture11,
      GL_Texture12,
      GL_Texture13,
      GL_Texture14,
      GL_Texture15,
      GL_Texture16,
      GL_Texture17,
      GL_Texture18,
      GL_Texture19,
      GL_Texture20,
      GL_Texture21,
      GL_Texture22,
      GL_Texture23,
      GL_Texture24,
      GL_Texture25,
      GL_Texture26,
      GL_Texture27,
      GL_Texture28,
      GL_Texture29,
      GL_Texture30,
      GL_Texture31);

   type Framebuffer_Status is
     (GL_Framebuffer_Complete,
      GL_Framebuffer_Incomplete_Attachment,
      GL_Framebuffer_Incomplete_Missing_Attachment,
      GL_Framebuffer_Unsupported);

   type Graphics_Reset_Status is
     (GL_No_Error);

   type Path_Fill_Mode is
     (GL_Invert);

   type Vertex_Buffer_Object_Parameter is
     (GL_Buffer_Size,
      GL_Buffer_Usage);

   type Renderbuffer_Parameter_Name is
     (GL_Renderbuffer_Width,
      GL_Renderbuffer_Height,
      GL_Renderbuffer_Internal_Format,
      GL_Renderbuffer_Red_Size,
      GL_Renderbuffer_Green_Size,
      GL_Renderbuffer_Blue_Size,
      GL_Renderbuffer_Alpha_Size,
      GL_Renderbuffer_Depth_Size,
      GL_Renderbuffer_Stencil_Size);

   type Vertex_Buffer_Object_Usage is
     (GL_Stream_Draw,
      GL_Static_Draw,
      GL_Dynamic_Draw);

   type Blending_Factor is
     (GL_Zero,
      GL_One,
      GL_Src_Color,
      GL_One_Minus_Src_Color,
      GL_Src_Alpha,
      GL_One_Minus_Src_Alpha,
      GL_Dst_Alpha,
      GL_One_Minus_Dst_Alpha,
      GL_Dst_Color,
      GL_One_Minus_Dst_Color,
      GL_Src_Alpha_Saturate,
      GL_Constant_Color,
      GL_One_Minus_Constant_Color,
      GL_Constant_Alpha,
      GL_One_Minus_Constant_Alpha);

   type Blit_Framebuffer_Filter is
     (GL_Nearest,
      GL_Linear);

   type Buffer_Storage_Target is
     (GL_Array_Buffer,
      GL_Element_Array_Buffer);

   type Check_Framebuffer_Status_Target is
     (GL_Framebuffer);

   type Copy_Buffer_Sub_Data_Target is
     (GL_Array_Buffer,
      GL_Element_Array_Buffer);

   type Shader_Type is
     (GL_Fragment_Shader,
      GL_Vertex_Shader);

   type Debug_Source is
     (GL_Dont_Care);

   type Debug_Type is
     (GL_Dont_Care);

   type Debug_Severity is
     (GL_Dont_Care);

   type Sampler_Parameter_Name is
     (GL_Texture_Mag_Filter,
      GL_Texture_Min_Filter,
      GL_Texture_Wrap_S,
      GL_Texture_Wrap_T);

   type Object_Identifier is
     (GL_Texture,
      GL_Framebuffer,
      GL_Renderbuffer);

   type Color_Buffer is
     (GL_None,
      GL_Front,
      GL_Back,
      GL_Front_And_Back,
      GL_Color_Attachment0);

   type Vertex_Array_PName is
     (GL_Vertex_Attrib_Array_Enabled,
      GL_Vertex_Attrib_Array_Size,
      GL_Vertex_Attrib_Array_Stride,
      GL_Vertex_Attrib_Array_Type,
      GL_Vertex_Attrib_Array_Normalized);

   type Shader_Parameter_Name is
     (GL_Shader_Type,
      GL_Delete_Status,
      GL_Compile_Status,
      GL_Info_Log_Length,
      GL_Shader_Source_Length);

   type Pipeline_Parameter_Name is
     (GL_Fragment_Shader,
      GL_Vertex_Shader,
      GL_Info_Log_Length);

   type Vertex_Attrib_Enum is
     (GL_Vertex_Attrib_Array_Enabled,
      GL_Vertex_Attrib_Array_Size,
      GL_Vertex_Attrib_Array_Stride,
      GL_Vertex_Attrib_Array_Type,
      GL_Current_Vertex_Attrib,
      GL_Vertex_Attrib_Array_Normalized,
      GL_Vertex_Attrib_Array_Buffer_Binding);

   type Vertex_Attrib_Type is
     (GL_Byte,
      GL_Unsigned_Byte,
      GL_Short,
      GL_Unsigned_Short,
      GL_Int,
      GL_Unsigned_Int,
      GL_Float,
      GL_Fixed);

   type Attribute_Type is
     (GL_Float_Vec2,
      GL_Float_Vec3,
      GL_Float_Vec4,
      GL_Int_Vec2,
      GL_Int_Vec3,
      GL_Int_Vec4,
      GL_Bool,
      GL_Bool_Vec2,
      GL_Bool_Vec3,
      GL_Bool_Vec4,
      GL_Float_Mat2,
      GL_Float_Mat3,
      GL_Float_Mat4,
      GL_Sampler_2d,
      GL_Sampler_Cube);

   type Internal_Format_PName is
     (GL_Samples);

   type Framebuffer_Attachment_Parameter_Name is
     (GL_Framebuffer_Attachment_Object_Name,
      GL_Framebuffer_Attachment_Texture_Level,
      GL_Framebuffer_Attachment_Texture_Cube_Map_Face);

   type Precision_Type is
     (GL_Low_Float,
      GL_Medium_Float,
      GL_High_Float,
      GL_Low_Int,
      GL_Medium_Int,
      GL_High_Int);

   type Vertex_Attrib_Pointer_Type is
     (GL_Byte,
      GL_Unsigned_Byte,
      GL_Short,
      GL_Unsigned_Short,
      GL_Int,
      GL_Unsigned_Int,
      GL_Float,
      GL_Fixed);

   type Get_Framebuffer_Parameter is
     (GL_Sample_Buffers,
      GL_Samples,
      GL_Implementation_Color_Read_Type,
      GL_Implementation_Color_Read_Format);

   type Path_Gen_Mode is
     (GL_None);

   type Path_Transform_Type is
     (GL_None);

   type Path_Font_Style is
     (GL_None);

   type Program_Property_ARB is
     (GL_Delete_Status,
      GL_Link_Status,
      GL_Validate_Status,
      GL_Info_Log_Length,
      GL_Attached_Shaders,
      GL_Active_Uniforms,
      GL_Active_Uniform_Max_Length,
      GL_Active_Attributes,
      GL_Active_Attribute_Max_Length);

   type Context_WebGL_Type is new Context_Type with private;
   type Context_WebGL_Access is access all Context_WebGL_Type;
   type Pointer_To_Context_WebGL_Class is access all Context_WebGL_Type'Class;

   procedure Get_Drawing_Context_WebGL
     (Context   : in out Context_WebGL_Type;
      Canvas  : in out Canvas_Type'Class);

   procedure Begin_Render
     (Context : in out Context_WebGL_Type);

   procedure End_Render
     (Context : in out Context_WebGL_Type);

   procedure Perspective
     (Context       : in out Context_WebGL_Type'Class;
      Matrix        : out Matrix_4;
      Field_Of_View : GLfloat;
      Aspect_Ratio  : GLfloat;
      Near, Far     : GLfloat);

   procedure Translate
     (Context : in out Context_WebGL_Type'Class;
      Matrix  : in out Matrix_4;
      X, Y, Z : GLfloat);

   procedure Rotate
     (Context : in out Context_WebGL_Type'Class;
      Matrix  : in out Matrix_4;
      Angle   : GLfloat;
      X, Y, Z : GLfloat);

   procedure Uniform_Matrix
     (Context  : in out Context_WebGL_Type'Class;
      Location : GLint;
      Matrix   : Matrix_4);

   procedure Active_Texture
     (Context : in out Context_WebGL_Type'Class;
      Texture : Texture_Unit);

   procedure Attach_Shader
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint;
      Shader : GLuint);

   procedure Bind_Attrib_Location
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint;
      Index : GLuint;
      Name : String);

   procedure Bind_Buffer
     (Context : in out Context_WebGL_Type'Class;
      Target : Buffer_Target_ARB;
      Buffer : GLuint);

   procedure Bind_Framebuffer
     (Context : in out Context_WebGL_Type'Class;
      Target : Framebuffer_Target;
      Framebuffer : GLuint);

   procedure Bind_Renderbuffer
     (Context : in out Context_WebGL_Type'Class;
      Target : Renderbuffer_Target;
      Renderbuffer : GLuint);

   procedure Bind_Texture
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Texture : GLuint);

   procedure Blend_Color
     (Context : in out Context_WebGL_Type'Class;
      Red : GLfloat;
      Green : GLfloat;
      Blue : GLfloat;
      Alpha : GLfloat);

   procedure Blend_Equation
     (Context : in out Context_WebGL_Type'Class;
      Mode : Blend_Equation_Mode_EXT);

   procedure Blend_Equation_Separate
     (Context : in out Context_WebGL_Type'Class;
      ModeRGB : Blend_Equation_Mode_EXT;
      ModeAlpha : Blend_Equation_Mode_EXT);

   procedure Blend_Func
     (Context : in out Context_WebGL_Type'Class;
      Sfactor : Blending_Factor;
      Dfactor : Blending_Factor);

   procedure Blend_Func_Separate
     (Context : in out Context_WebGL_Type'Class;
      SfactorRGB : Blending_Factor;
      DfactorRGB : Blending_Factor;
      SfactorAlpha : Blending_Factor;
      DfactorAlpha : Blending_Factor);

   procedure Buffer_Data
     (Context : in out Context_WebGL_Type'Class;
      Target : Buffer_Target_ARB;
      Data : Float_Array;
      Usage : Buffer_Usage_ARB);

   procedure Buffer_Data
     (Context : in out Context_WebGL_Type'Class;
      Target : Buffer_Target_ARB;
      Data : Uint16_Array;
      Usage : Buffer_Usage_ARB);

   procedure Buffer_Data
     (Context : in out Context_WebGL_Type'Class;
      Target : Buffer_Target_ARB;
      Data : Uint8_Array;
      Usage : Buffer_Usage_ARB);

   procedure Buffer_Sub_Data
     (Context : in out Context_WebGL_Type'Class;
      Target : Buffer_Target_ARB;
      Offset : GLintptr;
      Data : Float_Array);

   procedure Buffer_Sub_Data
     (Context : in out Context_WebGL_Type'Class;
      Target : Buffer_Target_ARB;
      Offset : GLintptr;
      Data : Uint16_Array);

   procedure Buffer_Sub_Data
     (Context : in out Context_WebGL_Type'Class;
      Target : Buffer_Target_ARB;
      Offset : GLintptr;
      Data : Uint8_Array);

   procedure Clear
     (Context : in out Context_WebGL_Type'Class;
      Mask : Clear_Buffer_Mask_Array);

   procedure Clear_Color
     (Context : in out Context_WebGL_Type'Class;
      Red : GLfloat;
      Green : GLfloat;
      Blue : GLfloat;
      Alpha : GLfloat);

   procedure Clear_Depth
     (Context : in out Context_WebGL_Type'Class;
      D : GLfloat);

   procedure Clear_Stencil
     (Context : in out Context_WebGL_Type'Class;
      S : GLint);

   procedure Color_Mask
     (Context : in out Context_WebGL_Type'Class;
      Red : Boolean;
      Green : Boolean;
      Blue : Boolean;
      Alpha : Boolean);

   procedure Compile_Shader
     (Context : in out Context_WebGL_Type'Class;
      Shader : GLuint);

   procedure Compressed_Tex_Image_2D
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Level : GLint;
      Internalformat : Internal_Format;
      Width : GLsizei;
      Height : GLsizei;
      Border : GLint;
      ImageSize : GLsizei;
      Data : Float_Array);

   procedure Compressed_Tex_Image_2D
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Level : GLint;
      Internalformat : Internal_Format;
      Width : GLsizei;
      Height : GLsizei;
      Border : GLint;
      ImageSize : GLsizei;
      Data : Uint16_Array);

   procedure Compressed_Tex_Image_2D
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Level : GLint;
      Internalformat : Internal_Format;
      Width : GLsizei;
      Height : GLsizei;
      Border : GLint;
      ImageSize : GLsizei;
      Data : Uint8_Array);

   procedure Compressed_Tex_Sub_Image_2D
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Level : GLint;
      Xoffset : GLint;
      Yoffset : GLint;
      Width : GLsizei;
      Height : GLsizei;
      Format : Pixel_Format;
      ImageSize : GLsizei;
      Data : Float_Array);

   procedure Compressed_Tex_Sub_Image_2D
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Level : GLint;
      Xoffset : GLint;
      Yoffset : GLint;
      Width : GLsizei;
      Height : GLsizei;
      Format : Pixel_Format;
      ImageSize : GLsizei;
      Data : Uint16_Array);

   procedure Compressed_Tex_Sub_Image_2D
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Level : GLint;
      Xoffset : GLint;
      Yoffset : GLint;
      Width : GLsizei;
      Height : GLsizei;
      Format : Pixel_Format;
      ImageSize : GLsizei;
      Data : Uint8_Array);

   procedure Copy_Tex_Image_2D
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Level : GLint;
      Internalformat : Internal_Format;
      X : GLint;
      Y : GLint;
      Width : GLsizei;
      Height : GLsizei;
      Border : GLint);

   procedure Copy_Tex_Sub_Image_2D
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Level : GLint;
      Xoffset : GLint;
      Yoffset : GLint;
      X : GLint;
      Y : GLint;
      Width : GLsizei;
      Height : GLsizei);

   function Create_Program
     (Context : in out Context_WebGL_Type'Class)
     return GLuint;

   function Create_Shader
     (Context : in out Context_WebGL_Type'Class;
      Item_Type : Shader_Type)
     return GLuint;

   procedure Cull_Face
     (Context : in out Context_WebGL_Type'Class;
      Mode : Cull_Face_Mode);

   procedure Delete_Buffers
     (Context : in out Context_WebGL_Type'Class;
      N : GLsizei;
      Buffers : GLuint);

   procedure Delete_Framebuffers
     (Context : in out Context_WebGL_Type'Class;
      N : GLsizei;
      Framebuffers : GLuint);

   procedure Delete_Program
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint);

   procedure Delete_Renderbuffers
     (Context : in out Context_WebGL_Type'Class;
      N : GLsizei;
      Renderbuffers : GLuint);

   procedure Delete_Shader
     (Context : in out Context_WebGL_Type'Class;
      Shader : GLuint);

   procedure Delete_Textures
     (Context : in out Context_WebGL_Type'Class;
      N : GLsizei;
      Textures : GLuint);

   procedure Depth_Func
     (Context : in out Context_WebGL_Type'Class;
      Func : Depth_Function);

   procedure Depth_Mask
     (Context : in out Context_WebGL_Type'Class;
      Flag : Boolean);

   procedure Depth_Rangef
     (Context : in out Context_WebGL_Type'Class;
      N : GLfloat;
      F : GLfloat);

   procedure Detach_Shader
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint;
      Shader : GLuint);

   procedure Disable
     (Context : in out Context_WebGL_Type'Class;
      Cap : Enable_Cap);

   procedure Disable_Vertex_Attrib_Array
     (Context : in out Context_WebGL_Type'Class;
      Index : GLuint);

   procedure Draw_Arrays
     (Context : in out Context_WebGL_Type'Class;
      Mode : Primitive_Type;
      First : GLint;
      Count : GLsizei);

   procedure Draw_Elements
     (Context : in out Context_WebGL_Type'Class;
      Mode : Primitive_Type;
      Count : GLsizei;
      ElementType : Draw_Elements_Type;
      Offset : System.Storage_Elements.Storage_Offset);

   procedure Enable
     (Context : in out Context_WebGL_Type'Class;
      Cap : Enable_Cap);

   procedure Enable_Vertex_Attrib_Array
     (Context : in out Context_WebGL_Type'Class;
      Index : GLuint);

   procedure Finish
     (Context : in out Context_WebGL_Type'Class);

   procedure Flush
     (Context : in out Context_WebGL_Type'Class);

   procedure Framebuffer_Renderbuffer
     (Context : in out Context_WebGL_Type'Class;
      Target : Framebuffer_Target;
      Attachment : Framebuffer_Attachment;
      Renderbuffertarget : Renderbuffer_Target;
      Renderbuffer : GLuint);

   procedure Framebuffer_Texture_2D
     (Context : in out Context_WebGL_Type'Class;
      Target : Framebuffer_Target;
      Attachment : Framebuffer_Attachment;
      Textarget : Texture_Target;
      Texture : GLuint;
      Level : GLint);

   procedure Front_Face
     (Context : in out Context_WebGL_Type'Class;
      Mode : Front_Face_Direction);

   procedure Gen_Buffers
     (Context : in out Context_WebGL_Type'Class;
      N : GLsizei;
      Buffers : GLuint);

   procedure Gen_Framebuffers
     (Context : in out Context_WebGL_Type'Class;
      N : GLsizei;
      Framebuffers : GLuint);

   procedure Gen_Renderbuffers
     (Context : in out Context_WebGL_Type'Class;
      N : GLsizei;
      Renderbuffers : GLuint);

   procedure Gen_Textures
     (Context : in out Context_WebGL_Type'Class;
      N : GLsizei;
      Textures : GLuint);

   procedure Generate_Mipmap
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target);

   procedure Get_Active_Attrib
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint;
      Index : GLuint;
      BufSize : GLsizei;
      Length : GLsizei;
      Size : GLint;
      Item_Type : Attribute_Type;
      Name : String);

   procedure Get_Active_Uniform
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint;
      Index : GLuint;
      BufSize : GLsizei;
      Length : GLsizei;
      Size : GLint;
      Item_Type : Attribute_Type;
      Name : String);

   procedure Get_Attached_Shaders
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint;
      MaxCount : GLsizei;
      Count : GLsizei;
      Shaders : GLuint);

   function Get_Attrib_Location
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint;
      Name : String)
     return GLuint;

   procedure Get_Booleanv
     (Context : in out Context_WebGL_Type'Class;
      Pname : Get_PName;
      Data : Boolean);

   procedure Get_Buffer_Parameteriv
     (Context : in out Context_WebGL_Type'Class;
      Target : Buffer_Target_ARB;
      Pname : GLenum;
      Params : GLint);

   procedure Get_Floatv
     (Context : in out Context_WebGL_Type'Class;
      Pname : Get_PName;
      Data : GLfloat);

   procedure Get_Framebuffer_Attachment_Parameteriv
     (Context : in out Context_WebGL_Type'Class;
      Target : Framebuffer_Target;
      Attachment : Framebuffer_Attachment;
      Pname : Framebuffer_Attachment_Parameter_Name;
      Params : GLint);

   procedure Get_Integerv
     (Context : in out Context_WebGL_Type'Class;
      Pname : Get_PName;
      Data : GLint);

   procedure Get_Program_Info_Log
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint;
      BufSize : GLsizei;
      Length : GLsizei;
      InfoLog : String);

   procedure Get_Programiv
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint;
      Pname : Program_Property_ARB;
      Params : GLint);

   procedure Get_Renderbuffer_Parameteriv
     (Context : in out Context_WebGL_Type'Class;
      Target : Renderbuffer_Target;
      Pname : Renderbuffer_Parameter_Name;
      Params : GLint);

   procedure Get_Shader_Info_Log
     (Context : in out Context_WebGL_Type'Class;
      Shader : GLuint;
      BufSize : GLsizei;
      Length : GLsizei;
      InfoLog : String);

   procedure Get_Shader_Precision_Format
     (Context : in out Context_WebGL_Type'Class;
      Shadertype : Shader_Type;
      Precisiontype : Precision_Type;
      Item_Range : GLint;
      Precision : GLint);

   procedure Get_Shader_Source
     (Context : in out Context_WebGL_Type'Class;
      Shader : GLuint;
      BufSize : GLsizei;
      Length : GLsizei;
      Source : String);

   procedure Get_Shaderiv
     (Context : in out Context_WebGL_Type'Class;
      Shader : GLuint;
      Pname : Shader_Parameter_Name;
      Params : GLint);

   procedure Get_Tex_Parameterfv
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Pname : Get_Texture_Parameter;
      Params : GLfloat);

   procedure Get_Tex_Parameteriv
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Pname : Get_Texture_Parameter;
      Params : GLint);

   function Get_Uniform_Location
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint;
      Name : String)
     return GLuint;

   procedure Get_Uniformfv
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint;
      Location : GLint;
      Params : GLfloat);

   procedure Get_Uniformiv
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint;
      Location : GLint;
      Params : GLint);

   procedure Get_Vertex_Attribfv
     (Context : in out Context_WebGL_Type'Class;
      Index : GLuint;
      Pname : GLenum;
      Params : GLfloat);

   procedure Get_Vertex_Attribiv
     (Context : in out Context_WebGL_Type'Class;
      Index : GLuint;
      Pname : GLenum;
      Params : GLint);

   procedure Hint
     (Context : in out Context_WebGL_Type'Class;
      Target : Hint_Target;
      Mode : Hint_Mode);

   function Is_Buffer
     (Context : in out Context_WebGL_Type'Class;
      Buffer : GLuint)
     return Boolean;

   function Is_Enabled
     (Context : in out Context_WebGL_Type'Class;
      Cap : Enable_Cap)
     return Boolean;

   function Is_Framebuffer
     (Context : in out Context_WebGL_Type'Class;
      Framebuffer : GLuint)
     return Boolean;

   function Is_Program
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint)
     return Boolean;

   function Is_Renderbuffer
     (Context : in out Context_WebGL_Type'Class;
      Renderbuffer : GLuint)
     return Boolean;

   function Is_Shader
     (Context : in out Context_WebGL_Type'Class;
      Shader : GLuint)
     return Boolean;

   function Is_Texture
     (Context : in out Context_WebGL_Type'Class;
      Texture : GLuint)
     return Boolean;

   procedure Line_Width
     (Context : in out Context_WebGL_Type'Class;
      Width : GLfloat);

   procedure Link_Program
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint);

   procedure Pixel_Storei
     (Context : in out Context_WebGL_Type'Class;
      Pname : Pixel_Store_Parameter;
      Param : GLint);

   procedure Polygon_Offset
     (Context : in out Context_WebGL_Type'Class;
      Factor : GLfloat;
      Units : GLfloat);

   procedure Release_Shader_Compiler
     (Context : in out Context_WebGL_Type'Class);

   procedure Renderbuffer_Storage
     (Context : in out Context_WebGL_Type'Class;
      Target : Renderbuffer_Target;
      Internalformat : Internal_Format;
      Width : GLsizei;
      Height : GLsizei);

   procedure Sample_Coverage
     (Context : in out Context_WebGL_Type'Class;
      Value : GLfloat;
      Invert : Boolean);

   procedure Scissor
     (Context : in out Context_WebGL_Type'Class;
      X : GLint;
      Y : GLint;
      Width : GLsizei;
      Height : GLsizei);

   procedure Shader_Binary
     (Context : in out Context_WebGL_Type'Class;
      Count : GLsizei;
      Shaders : GLuint;
      Binaryformat : GLenum;
      Binary : Float_Array;
      Length : GLsizei);

   procedure Shader_Binary
     (Context : in out Context_WebGL_Type'Class;
      Count : GLsizei;
      Shaders : GLuint;
      Binaryformat : GLenum;
      Binary : Uint16_Array;
      Length : GLsizei);

   procedure Shader_Binary
     (Context : in out Context_WebGL_Type'Class;
      Count : GLsizei;
      Shaders : GLuint;
      Binaryformat : GLenum;
      Binary : Uint8_Array;
      Length : GLsizei);

   procedure Shader_Source
     (Context : in out Context_WebGL_Type'Class;
      Shader : GLuint;
      Source : String);

   procedure Stencil_Func
     (Context : in out Context_WebGL_Type'Class;
      Func : Stencil_Function;
      Ref : GLint;
      Mask : GLuint);

   procedure Stencil_Func_Separate
     (Context : in out Context_WebGL_Type'Class;
      Face : Stencil_Face_Direction;
      Func : Stencil_Function;
      Ref : GLint;
      Mask : GLuint);

   procedure Stencil_Mask
     (Context : in out Context_WebGL_Type'Class;
      Mask : GLuint);

   procedure Stencil_Mask_Separate
     (Context : in out Context_WebGL_Type'Class;
      Face : Stencil_Face_Direction;
      Mask : GLuint);

   procedure Set_Stencil_Op
     (Context : in out Context_WebGL_Type'Class;
      Fail : Stencil_Op;
      Zfail : Stencil_Op;
      Zpass : Stencil_Op);

   procedure Stencil_Op_Separate
     (Context : in out Context_WebGL_Type'Class;
      Face : Stencil_Face_Direction;
      Sfail : Stencil_Op;
      Dpfail : Stencil_Op;
      Dppass : Stencil_Op);

   procedure Tex_Image_2D
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Level : GLint;
      Internalformat : Internal_Format;
      Width : GLsizei;
      Height : GLsizei;
      Border : GLint;
      Format : Pixel_Format;
      Item_Type : Pixel_Type;
      Pixels : Float_Array);

   procedure Tex_Image_2D
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Level : GLint;
      Internalformat : Internal_Format;
      Width : GLsizei;
      Height : GLsizei;
      Border : GLint;
      Format : Pixel_Format;
      Item_Type : Pixel_Type;
      Pixels : Uint16_Array);

   procedure Tex_Image_2D
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Level : GLint;
      Internalformat : Internal_Format;
      Width : GLsizei;
      Height : GLsizei;
      Border : GLint;
      Format : Pixel_Format;
      Item_Type : Pixel_Type;
      Pixels : Uint8_Array);

   procedure Tex_Parameterf
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Pname : Texture_Parameter_Name;
      Param : GLfloat);

   procedure Tex_Parameterfv
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Pname : Texture_Parameter_Name;
      Params : GLfloat);

   procedure Tex_Parameteri
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Pname : Texture_Parameter_Name;
      Param : GLint);

   procedure Tex_Parameteriv
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Pname : Texture_Parameter_Name;
      Params : GLint);

   procedure Tex_Sub_Image_2D
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Level : GLint;
      Xoffset : GLint;
      Yoffset : GLint;
      Width : GLsizei;
      Height : GLsizei;
      Format : Pixel_Format;
      Item_Type : Pixel_Type;
      Pixels : Float_Array);

   procedure Tex_Sub_Image_2D
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Level : GLint;
      Xoffset : GLint;
      Yoffset : GLint;
      Width : GLsizei;
      Height : GLsizei;
      Format : Pixel_Format;
      Item_Type : Pixel_Type;
      Pixels : Uint16_Array);

   procedure Tex_Sub_Image_2D
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Level : GLint;
      Xoffset : GLint;
      Yoffset : GLint;
      Width : GLsizei;
      Height : GLsizei;
      Format : Pixel_Format;
      Item_Type : Pixel_Type;
      Pixels : Uint8_Array);

   procedure Uniform
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      V0 : GLfloat);

   procedure Uniform
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      Value : Float_Array);

   procedure Uniform
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      V0 : GLint);

   procedure Uniform
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      Value : Int_Array);

   procedure Uniform
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      Value_1 : GLfloat;
      Value_2 : GLfloat);

   procedure Uniform_2v
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      Value : Float_Array);

   procedure Uniform_2i
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      V0 : GLint;
      V1 : GLint);

   procedure Uniform_2iv
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      Count : GLsizei;
      Value : GLint);

   procedure Uniform_3f
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      V0 : GLfloat;
      V1 : GLfloat;
      V2 : GLfloat);

   procedure Uniform_3v
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      Value : Float_Array);

   procedure Uniform_3i
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      V0 : GLint;
      V1 : GLint;
      V2 : GLint);

   procedure Uniform_3iv
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      Count : GLsizei;
      Value : GLint);

   procedure Uniform_4f
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      V0 : GLfloat;
      V1 : GLfloat;
      V2 : GLfloat;
      V3 : GLfloat);

   procedure Uniform_4v
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      Value : Float_Array);

   procedure Uniform_4i
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      V0 : GLint;
      V1 : GLint;
      V2 : GLint;
      V3 : GLint);

   procedure Uniform_4iv
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      Count : GLsizei;
      Value : GLint);

   procedure Uniform_Matrix_2fv
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      Count : GLsizei;
      Transpose : Boolean;
      Value : GLfloat);

   procedure Uniform_Matrix_3fv
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      Count : GLsizei;
      Transpose : Boolean;
      Value : GLfloat);

   procedure Uniform_Matrix_4fv
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      Count : GLsizei;
      Transpose : Boolean;
      Value : GLfloat);

   procedure Use_Program
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint);

   procedure Validate_Program
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint);

   procedure Vertex_Attrib_1f
     (Context : in out Context_WebGL_Type'Class;
      Index : GLuint;
      X : GLfloat);

   procedure Vertex_Attrib_1fv
     (Context : in out Context_WebGL_Type'Class;
      Index : GLuint;
      V : GLfloat);

   procedure Vertex_Attrib_2f
     (Context : in out Context_WebGL_Type'Class;
      Index : GLuint;
      X : GLfloat;
      Y : GLfloat);

   procedure Vertex_Attrib_2fv
     (Context : in out Context_WebGL_Type'Class;
      Index : GLuint;
      V : GLfloat);

   procedure Vertex_Attrib_3f
     (Context : in out Context_WebGL_Type'Class;
      Index : GLuint;
      X : GLfloat;
      Y : GLfloat;
      Z : GLfloat);

   procedure Vertex_Attrib_3fv
     (Context : in out Context_WebGL_Type'Class;
      Index : GLuint;
      V : GLfloat);

   procedure Vertex_Attrib_4f
     (Context : in out Context_WebGL_Type'Class;
      Index : GLuint;
      X : GLfloat;
      Y : GLfloat;
      Z : GLfloat;
      W : GLfloat);

   procedure Vertex_Attrib_4fv
     (Context : in out Context_WebGL_Type'Class;
      Index : GLuint;
      V : GLfloat);

   procedure Vertex_Attrib_Pointer
     (Context : in out Context_WebGL_Type'Class;
      Index : GLuint;
      Size : GLuint;
      Element_Type : Vertex_Attrib_Pointer_Type;
      Normalized : Boolean;
      Stride : GLsizei;
      Pointer : System.Storage_Elements.Storage_Offset);

   procedure Viewport
     (Context : in out Context_WebGL_Type'Class;
      X : GLint;
      Y : GLint;
      Width : GLsizei;
      Height : GLsizei);

   function Create_Buffer
     (Context : in out Context_WebGL_Type'Class)
     return GLuint;

   function Create_Texture
     (Context : in out Context_WebGL_Type'Class)
     return GLuint;

   function Get_Shader_Parameter
     (Context : in out Context_WebGL_Type'Class;
      Shader : GLuint;
      Parameter : Shader_Parameter_Name)
     return Boolean;

   function Get_Program_Parameter
     (Context : in out Context_WebGL_Type'Class;
      Progam : GLuint;
      Parameter : Program_Property_ARB)
     return Boolean;

   procedure Set_Texture_Mag_Filter
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Param : Texture_Mag_Filter);

   procedure Set_Texture_Min_Filter
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Param : Texture_Min_Filter);

   procedure Texture_Wrap_S
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Param : Texture_Wrap_Mode);

   procedure Texture_Wrap_T
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Param : Texture_Wrap_Mode);

   procedure Tex_Image_2D
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Level : GLint;
      Internalformat : Internal_Format;
      Format : Pixel_Format;
      Item_Type : Pixel_Type;
      Image_Id : String);

private

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors
       (Positive, String);

   type Context_WebGL_Type is new Context_Type with
      record
         Web_Object_Ids : String_Vectors.Vector;
         Render_Script  : String_Vectors.Vector;
         Rendering      : Boolean := False;
         View_Width     : Natural;
         View_Height    : Natural;
      end record;

end Gnoga.Gui.Element.Canvas.Context_WebGL;
