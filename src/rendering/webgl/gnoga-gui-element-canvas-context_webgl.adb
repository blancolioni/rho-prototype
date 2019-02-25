with Gnoga.Gui.Element.Canvas.Context_WebGL.Support;

package body Gnoga.Gui.Element.Canvas.Context_WebGL is

   pragma Style_Checks (Off);
   pragma Warnings (Off);
   function GLEnum_Property
     (Context : Context_WebGL_Type'Class;
      Name    : String)
     return String
   is (if Name = "TRUE" then "true"
       elsif Name = "FALSE" then "false"
       else Context.Property (Name (Name'First + 3 .. Name'Last)));

   procedure Get_Drawing_Context_WebGL
     (Context : in out Context_WebGL_Type;
      Canvas  : in out Canvas_Type'Class)
   renames Support.Get_Drawing_Context_WebGL;

   procedure Begin_Render
     (Context : in out Context_WebGL_Type)
   renames Support.Begin_Render;

   procedure End_Render
     (Context : in out Context_WebGL_Type)
   renames Support.End_Render;

   procedure Perspective
     (Context       : in out Context_WebGL_Type'Class;
      Matrix        : out Matrix_4;
      Field_Of_View : GLfloat;
      Aspect_Ratio  : GLfloat;
      Near, Far     : GLfloat)
   renames Support.Perspective;

   procedure Translate
     (Context : in out Context_WebGL_Type'Class;
      Matrix  : in out Matrix_4;
      X, Y, Z : GLfloat)
   renames Support.Translate;

   procedure Rotate
     (Context : in out Context_WebGL_Type'Class;
      Matrix  : in out Matrix_4;
      Angle   : GLfloat;
      X, Y, Z : GLfloat)
   renames Support.Rotate;

   procedure Uniform_Matrix
     (Context  : in out Context_WebGL_Type'Class;
      Location : GLuint;
      Matrix   : Matrix_4)
   renames Support.Uniform_Matrix;


   Attrib_Mask_Values : constant array (Attrib_Mask) of GLuint := (
      GL_DEPTH_BUFFER_BIT => 256,
      GL_STENCIL_BUFFER_BIT => 1024,
      GL_COLOR_BUFFER_BIT => 16384);

   Alpha_Function_Values : constant array (Alpha_Function) of GLuint := (
      GL_NEVER => 512,
      GL_LESS => 513,
      GL_EQUAL => 514,
      GL_LEQUAL => 515,
      GL_GREATER => 516,
      GL_NOTEQUAL => 517,
      GL_GEQUAL => 518,
      GL_ALWAYS => 519);

   Blend_Equation_Mode_EXT_Values : constant array (Blend_Equation_Mode_EXT) of GLuint := (
      GL_FUNC_ADD => 32774,
      GL_FUNC_SUBTRACT => 32778,
      GL_FUNC_REVERSE_SUBTRACT => 32779);

   Buffer_Target_ARB_Values : constant array (Buffer_Target_ARB) of GLuint := (
      GL_ARRAY_BUFFER => 34962,
      GL_ELEMENT_ARRAY_BUFFER => 34963);

   Buffer_Usage_ARB_Values : constant array (Buffer_Usage_ARB) of GLuint := (
      GL_STREAM_DRAW => 35040,
      GL_STATIC_DRAW => 35044,
      GL_DYNAMIC_DRAW => 35048);

   Clear_Buffer_Mask_Values : constant array (Clear_Buffer_Mask) of GLuint := (
      GL_DEPTH_BUFFER_BIT => 256,
      GL_STENCIL_BUFFER_BIT => 1024,
      GL_COLOR_BUFFER_BIT => 16384);

   Color_Material_Face_Values : constant array (Color_Material_Face) of GLuint := (
      GL_FRONT => 1028,
      GL_BACK => 1029,
      GL_FRONT_AND_BACK => 1032);

   Color_Pointer_Type_Values : constant array (Color_Pointer_Type) of GLuint := (
      GL_BYTE => 5120,
      GL_UNSIGNED_BYTE => 5121,
      GL_SHORT => 5122,
      GL_UNSIGNED_SHORT => 5123,
      GL_INT => 5124,
      GL_UNSIGNED_INT => 5125,
      GL_FLOAT => 5126);

   Cull_Face_Mode_Values : constant array (Cull_Face_Mode) of GLuint := (
      GL_FRONT => 1028,
      GL_BACK => 1029,
      GL_FRONT_AND_BACK => 1032);

   Depth_Function_Values : constant array (Depth_Function) of GLuint := (
      GL_NEVER => 512,
      GL_LESS => 513,
      GL_EQUAL => 514,
      GL_LEQUAL => 515,
      GL_GREATER => 516,
      GL_NOTEQUAL => 517,
      GL_GEQUAL => 518,
      GL_ALWAYS => 519);

   Draw_Buffer_Mode_Values : constant array (Draw_Buffer_Mode) of GLuint := (
      GL_NONE => 0,
      GL_FRONT => 1028,
      GL_BACK => 1029,
      GL_FRONT_AND_BACK => 1032);

   Draw_Elements_Type_Values : constant array (Draw_Elements_Type) of GLuint := (
      GL_UNSIGNED_BYTE => 5121,
      GL_UNSIGNED_SHORT => 5123,
      GL_UNSIGNED_INT => 5125);

   Enable_Cap_Values : constant array (Enable_Cap) of GLuint := (
      GL_CULL_FACE => 2884,
      GL_DEPTH_TEST => 2929,
      GL_STENCIL_TEST => 2960,
      GL_DITHER => 3024,
      GL_BLEND => 3042,
      GL_SCISSOR_TEST => 3089,
      GL_TEXTURE_2D => 3553,
      GL_POLYGON_OFFSET_FILL => 32823,
      GL_SAMPLE_ALPHA_TO_COVERAGE => 32926,
      GL_SAMPLE_COVERAGE => 32928);

   Error_Code_Values : constant array (Error_Code) of GLuint := (
      GL_NO_ERROR => 0,
      GL_INVALID_ENUM => 1280,
      GL_INVALID_VALUE => 1281,
      GL_INVALID_OPERATION => 1282,
      GL_OUT_OF_MEMORY => 1285,
      GL_INVALID_FRAMEBUFFER_OPERATION => 1286);

   Fog_Coordinate_Pointer_Type_Values : constant array (Fog_Coordinate_Pointer_Type) of GLuint := (
      GL_FLOAT => 5126);

   Fog_Mode_Values : constant array (Fog_Mode) of GLuint := (
      GL_LINEAR => 9729);

   Fog_Pointer_Type_EXT_Values : constant array (Fog_Pointer_Type_EXT) of GLuint := (
      GL_FLOAT => 5126);

   Fog_Pointer_Type_IBM_Values : constant array (Fog_Pointer_Type_IBM) of GLuint := (
      GL_FLOAT => 5126);

   Front_Face_Direction_Values : constant array (Front_Face_Direction) of GLuint := (
      GL_CW => 2304,
      GL_CCW => 2305);

   Get_PName_Values : constant array (Get_PName) of GLuint := (
      GL_LINE_WIDTH => 2849,
      GL_CULL_FACE => 2884,
      GL_CULL_FACE_MODE => 2885,
      GL_FRONT_FACE => 2886,
      GL_DEPTH_RANGE => 2928,
      GL_DEPTH_TEST => 2929,
      GL_DEPTH_WRITEMASK => 2930,
      GL_DEPTH_CLEAR_VALUE => 2931,
      GL_DEPTH_FUNC => 2932,
      GL_STENCIL_TEST => 2960,
      GL_STENCIL_CLEAR_VALUE => 2961,
      GL_STENCIL_FUNC => 2962,
      GL_STENCIL_VALUE_MASK => 2963,
      GL_STENCIL_FAIL => 2964,
      GL_STENCIL_PASS_DEPTH_FAIL => 2965,
      GL_STENCIL_PASS_DEPTH_PASS => 2966,
      GL_STENCIL_REF => 2967,
      GL_STENCIL_WRITEMASK => 2968,
      GL_VIEWPORT => 2978,
      GL_DITHER => 3024,
      GL_BLEND => 3042,
      GL_SCISSOR_BOX => 3088,
      GL_SCISSOR_TEST => 3089,
      GL_COLOR_CLEAR_VALUE => 3106,
      GL_COLOR_WRITEMASK => 3107,
      GL_UNPACK_ALIGNMENT => 3317,
      GL_PACK_ALIGNMENT => 3333,
      GL_MAX_TEXTURE_SIZE => 3379,
      GL_MAX_VIEWPORT_DIMS => 3386,
      GL_SUBPIXEL_BITS => 3408,
      GL_RED_BITS => 3410,
      GL_GREEN_BITS => 3411,
      GL_BLUE_BITS => 3412,
      GL_ALPHA_BITS => 3413,
      GL_DEPTH_BITS => 3414,
      GL_STENCIL_BITS => 3415,
      GL_TEXTURE_2D => 3553,
      GL_POLYGON_OFFSET_UNITS => 10752,
      GL_BLEND_COLOR => 32773,
      GL_BLEND_EQUATION_RGB => 32777,
      GL_POLYGON_OFFSET_FILL => 32823,
      GL_POLYGON_OFFSET_FACTOR => 32824,
      GL_TEXTURE_BINDING_2D => 32873,
      GL_SAMPLE_BUFFERS => 32936,
      GL_SAMPLES => 32937,
      GL_SAMPLE_COVERAGE_VALUE => 32938,
      GL_SAMPLE_COVERAGE_INVERT => 32939,
      GL_BLEND_DST_RGB => 32968,
      GL_BLEND_SRC_RGB => 32969,
      GL_BLEND_DST_ALPHA => 32970,
      GL_BLEND_SRC_ALPHA => 32971,
      GL_ALIASED_POINT_SIZE_RANGE => 33901,
      GL_ALIASED_LINE_WIDTH_RANGE => 33902,
      GL_ACTIVE_TEXTURE => 34016,
      GL_MAX_RENDERBUFFER_SIZE => 34024,
      GL_TEXTURE_BINDING_CUBE_MAP => 34068,
      GL_MAX_CUBE_MAP_TEXTURE_SIZE => 34076,
      GL_NUM_COMPRESSED_TEXTURE_FORMATS => 34466,
      GL_COMPRESSED_TEXTURE_FORMATS => 34467,
      GL_STENCIL_BACK_FUNC => 34816,
      GL_STENCIL_BACK_FAIL => 34817,
      GL_STENCIL_BACK_PASS_DEPTH_FAIL => 34818,
      GL_STENCIL_BACK_PASS_DEPTH_PASS => 34819,
      GL_BLEND_EQUATION_ALPHA => 34877,
      GL_MAX_VERTEX_ATTRIBS => 34921,
      GL_MAX_TEXTURE_IMAGE_UNITS => 34930,
      GL_ARRAY_BUFFER_BINDING => 34964,
      GL_ELEMENT_ARRAY_BUFFER_BINDING => 34965,
      GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS => 35660,
      GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS => 35661,
      GL_CURRENT_PROGRAM => 35725,
      GL_IMPLEMENTATION_COLOR_READ_TYPE => 35738,
      GL_IMPLEMENTATION_COLOR_READ_FORMAT => 35739,
      GL_STENCIL_BACK_REF => 36003,
      GL_STENCIL_BACK_VALUE_MASK => 36004,
      GL_STENCIL_BACK_WRITEMASK => 36005,
      GL_RENDERBUFFER_BINDING => 36007,
      GL_NUM_SHADER_BINARY_FORMATS => 36345,
      GL_SHADER_COMPILER => 36346,
      GL_MAX_VERTEX_UNIFORM_VECTORS => 36347,
      GL_MAX_VARYING_VECTORS => 36348,
      GL_MAX_FRAGMENT_UNIFORM_VECTORS => 36349);

   Get_Texture_Parameter_Values : constant array (Get_Texture_Parameter) of GLuint := (
      GL_TEXTURE_MAG_FILTER => 10240,
      GL_TEXTURE_MIN_FILTER => 10241,
      GL_TEXTURE_WRAP_S => 10242,
      GL_TEXTURE_WRAP_T => 10243);

   Hint_Mode_Values : constant array (Hint_Mode) of GLuint := (
      GL_DONT_CARE => 4352,
      GL_FASTEST => 4353,
      GL_NICEST => 4354);

   Hint_Target_Values : constant array (Hint_Target) of GLuint := (
      GL_GENERATE_MIPMAP_HINT => 33170);

   Index_Pointer_Type_Values : constant array (Index_Pointer_Type) of GLuint := (
      GL_SHORT => 5122,
      GL_INT => 5124,
      GL_FLOAT => 5126);

   Light_Env_Mode_SGIX_Values : constant array (Light_Env_Mode_SGIX) of GLuint := (
      GL_REPLACE => 7681);

   List_Name_Type_Values : constant array (List_Name_Type) of GLuint := (
      GL_BYTE => 5120,
      GL_UNSIGNED_BYTE => 5121,
      GL_SHORT => 5122,
      GL_UNSIGNED_SHORT => 5123,
      GL_INT => 5124,
      GL_UNSIGNED_INT => 5125,
      GL_FLOAT => 5126);

   Logic_Op_Values : constant array (Logic_Op) of GLuint := (
      GL_INVERT => 5386);

   Material_Face_Values : constant array (Material_Face) of GLuint := (
      GL_FRONT => 1028,
      GL_BACK => 1029,
      GL_FRONT_AND_BACK => 1032);

   Matrix_Mode_Values : constant array (Matrix_Mode) of GLuint := (
      GL_TEXTURE => 5890);

   Normal_Pointer_Type_Values : constant array (Normal_Pointer_Type) of GLuint := (
      GL_BYTE => 5120,
      GL_SHORT => 5122,
      GL_INT => 5124,
      GL_FLOAT => 5126);

   Pixel_Format_Values : constant array (Pixel_Format) of GLuint := (
      GL_UNSIGNED_SHORT => 5123,
      GL_UNSIGNED_INT => 5125,
      GL_DEPTH_COMPONENT => 6402,
      GL_ALPHA => 6406,
      GL_RGB => 6407,
      GL_RGBA => 6408,
      GL_LUMINANCE => 6409,
      GL_LUMINANCE_ALPHA => 6410);

   Internal_Format_Values : constant array (Internal_Format) of GLuint := (
      GL_DEPTH_COMPONENT => 6402,
      GL_RGB => 6407,
      GL_RGBA => 6408,
      GL_RGBA4 => 32854,
      GL_RGB5_A1 => 32855,
      GL_DEPTH_COMPONENT16 => 33189);

   Pixel_Store_Parameter_Values : constant array (Pixel_Store_Parameter) of GLuint := (
      GL_UNPACK_ALIGNMENT => 3317,
      GL_PACK_ALIGNMENT => 3333);

   Pixel_Tex_Gen_Mode_Values : constant array (Pixel_Tex_Gen_Mode) of GLuint := (
      GL_NONE => 0,
      GL_RGB => 6407,
      GL_RGBA => 6408,
      GL_LUMINANCE => 6409,
      GL_LUMINANCE_ALPHA => 6410);

   Pixel_Type_Values : constant array (Pixel_Type) of GLuint := (
      GL_BYTE => 5120,
      GL_UNSIGNED_BYTE => 5121,
      GL_SHORT => 5122,
      GL_UNSIGNED_SHORT => 5123,
      GL_INT => 5124,
      GL_UNSIGNED_INT => 5125,
      GL_FLOAT => 5126,
      GL_UNSIGNED_SHORT_4_4_4_4 => 32819,
      GL_UNSIGNED_SHORT_5_5_5_1 => 32820);

   Primitive_Type_Values : constant array (Primitive_Type) of GLuint := (
      GL_POINTS => 0,
      GL_LINES => 1,
      GL_LINE_LOOP => 2,
      GL_LINE_STRIP => 3,
      GL_TRIANGLES => 4,
      GL_TRIANGLE_STRIP => 5,
      GL_TRIANGLE_FAN => 6);

   Read_Buffer_Mode_Values : constant array (Read_Buffer_Mode) of GLuint := (
      GL_FRONT => 1028,
      GL_BACK => 1029);

   Stencil_Face_Direction_Values : constant array (Stencil_Face_Direction) of GLuint := (
      GL_FRONT => 1028,
      GL_BACK => 1029,
      GL_FRONT_AND_BACK => 1032);

   Stencil_Function_Values : constant array (Stencil_Function) of GLuint := (
      GL_NEVER => 512,
      GL_LESS => 513,
      GL_EQUAL => 514,
      GL_LEQUAL => 515,
      GL_GREATER => 516,
      GL_NOTEQUAL => 517,
      GL_GEQUAL => 518,
      GL_ALWAYS => 519);

   Stencil_Op_Values : constant array (Stencil_Op) of GLuint := (
      GL_ZERO => 0,
      GL_INVERT => 5386,
      GL_KEEP => 7680,
      GL_REPLACE => 7681,
      GL_INCR => 7682,
      GL_DECR => 7683,
      GL_INCR_WRAP => 34055,
      GL_DECR_WRAP => 34056);

   String_Name_Values : constant array (String_Name) of GLuint := (
      GL_VENDOR => 7936,
      GL_RENDERER => 7937,
      GL_VERSION => 7938,
      GL_EXTENSIONS => 7939,
      GL_SHADING_LANGUAGE_VERSION => 35724);

   Tex_Coord_Pointer_Type_Values : constant array (Tex_Coord_Pointer_Type) of GLuint := (
      GL_SHORT => 5122,
      GL_INT => 5124,
      GL_FLOAT => 5126);

   Texture_Env_Mode_Values : constant array (Texture_Env_Mode) of GLuint := (
      GL_BLEND => 3042);

   Texture_Mag_Filter_Values : constant array (Texture_Mag_Filter) of GLuint := (
      GL_NEAREST => 9728,
      GL_LINEAR => 9729);

   Texture_Min_Filter_Values : constant array (Texture_Min_Filter) of GLuint := (
      GL_NEAREST => 9728,
      GL_LINEAR => 9729,
      GL_NEAREST_MIPMAP_NEAREST => 9984,
      GL_LINEAR_MIPMAP_NEAREST => 9985,
      GL_NEAREST_MIPMAP_LINEAR => 9986,
      GL_LINEAR_MIPMAP_LINEAR => 9987);

   Texture_Parameter_Name_Values : constant array (Texture_Parameter_Name) of GLuint := (
      GL_TEXTURE_MAG_FILTER => 10240,
      GL_TEXTURE_MIN_FILTER => 10241,
      GL_TEXTURE_WRAP_S => 10242,
      GL_TEXTURE_WRAP_T => 10243);

   Texture_Target_Values : constant array (Texture_Target) of GLuint := (
      GL_TEXTURE_2D => 3553,
      GL_TEXTURE_CUBE_MAP => 34067,
      GL_TEXTURE_CUBE_MAP_POSITIVE_X => 34069,
      GL_TEXTURE_CUBE_MAP_NEGATIVE_X => 34070,
      GL_TEXTURE_CUBE_MAP_POSITIVE_Y => 34071,
      GL_TEXTURE_CUBE_MAP_NEGATIVE_Y => 34072,
      GL_TEXTURE_CUBE_MAP_POSITIVE_Z => 34073,
      GL_TEXTURE_CUBE_MAP_NEGATIVE_Z => 34074);

   Texture_Wrap_Mode_Values : constant array (Texture_Wrap_Mode) of GLuint := (
      GL_REPEAT => 10497,
      GL_CLAMP_TO_EDGE => 33071);

   Vertex_Pointer_Type_Values : constant array (Vertex_Pointer_Type) of GLuint := (
      GL_SHORT => 5122,
      GL_INT => 5124,
      GL_FLOAT => 5126);

   Framebuffer_Attachment_Values : constant array (Framebuffer_Attachment) of GLuint := (
      GL_COLOR_ATTACHMENT0 => 36064,
      GL_DEPTH_ATTACHMENT => 36096);

   Renderbuffer_Target_Values : constant array (Renderbuffer_Target) of GLuint := (
      GL_RENDERBUFFER => 36161);

   Framebuffer_Target_Values : constant array (Framebuffer_Target) of GLuint := (
      GL_FRAMEBUFFER => 36160);

   Texture_Unit_Values : constant array (Texture_Unit) of GLuint := (
      GL_TEXTURE0 => 33984,
      GL_TEXTURE1 => 33985,
      GL_TEXTURE2 => 33986,
      GL_TEXTURE3 => 33987,
      GL_TEXTURE4 => 33988,
      GL_TEXTURE5 => 33989,
      GL_TEXTURE6 => 33990,
      GL_TEXTURE7 => 33991,
      GL_TEXTURE8 => 33992,
      GL_TEXTURE9 => 33993,
      GL_TEXTURE10 => 33994,
      GL_TEXTURE11 => 33995,
      GL_TEXTURE12 => 33996,
      GL_TEXTURE13 => 33997,
      GL_TEXTURE14 => 33998,
      GL_TEXTURE15 => 33999,
      GL_TEXTURE16 => 34000,
      GL_TEXTURE17 => 34001,
      GL_TEXTURE18 => 34002,
      GL_TEXTURE19 => 34003,
      GL_TEXTURE20 => 34004,
      GL_TEXTURE21 => 34005,
      GL_TEXTURE22 => 34006,
      GL_TEXTURE23 => 34007,
      GL_TEXTURE24 => 34008,
      GL_TEXTURE25 => 34009,
      GL_TEXTURE26 => 34010,
      GL_TEXTURE27 => 34011,
      GL_TEXTURE28 => 34012,
      GL_TEXTURE29 => 34013,
      GL_TEXTURE30 => 34014,
      GL_TEXTURE31 => 34015);

   Framebuffer_Status_Values : constant array (Framebuffer_Status) of GLuint := (
      GL_FRAMEBUFFER_COMPLETE => 36053,
      GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT => 36054,
      GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT => 36055,
      GL_FRAMEBUFFER_UNSUPPORTED => 36061);

   Graphics_Reset_Status_Values : constant array (Graphics_Reset_Status) of GLuint := (
      GL_NO_ERROR => 0);

   Path_Fill_Mode_Values : constant array (Path_Fill_Mode) of GLuint := (
      GL_INVERT => 5386);

   Vertex_Buffer_Object_Parameter_Values : constant array (Vertex_Buffer_Object_Parameter) of GLuint := (
      GL_BUFFER_SIZE => 34660,
      GL_BUFFER_USAGE => 34661);

   Renderbuffer_Parameter_Name_Values : constant array (Renderbuffer_Parameter_Name) of GLuint := (
      GL_RENDERBUFFER_WIDTH => 36162,
      GL_RENDERBUFFER_HEIGHT => 36163,
      GL_RENDERBUFFER_INTERNAL_FORMAT => 36164,
      GL_RENDERBUFFER_RED_SIZE => 36176,
      GL_RENDERBUFFER_GREEN_SIZE => 36177,
      GL_RENDERBUFFER_BLUE_SIZE => 36178,
      GL_RENDERBUFFER_ALPHA_SIZE => 36179,
      GL_RENDERBUFFER_DEPTH_SIZE => 36180,
      GL_RENDERBUFFER_STENCIL_SIZE => 36181);

   Vertex_Buffer_Object_Usage_Values : constant array (Vertex_Buffer_Object_Usage) of GLuint := (
      GL_STREAM_DRAW => 35040,
      GL_STATIC_DRAW => 35044,
      GL_DYNAMIC_DRAW => 35048);

   Blending_Factor_Values : constant array (Blending_Factor) of GLuint := (
      GL_ZERO => 0,
      GL_ONE => 1,
      GL_SRC_COLOR => 768,
      GL_ONE_MINUS_SRC_COLOR => 769,
      GL_SRC_ALPHA => 770,
      GL_ONE_MINUS_SRC_ALPHA => 771,
      GL_DST_ALPHA => 772,
      GL_ONE_MINUS_DST_ALPHA => 773,
      GL_DST_COLOR => 774,
      GL_ONE_MINUS_DST_COLOR => 775,
      GL_SRC_ALPHA_SATURATE => 776,
      GL_CONSTANT_COLOR => 32769,
      GL_ONE_MINUS_CONSTANT_COLOR => 32770,
      GL_CONSTANT_ALPHA => 32771,
      GL_ONE_MINUS_CONSTANT_ALPHA => 32772);

   Blit_Framebuffer_Filter_Values : constant array (Blit_Framebuffer_Filter) of GLuint := (
      GL_NEAREST => 9728,
      GL_LINEAR => 9729);

   Buffer_Storage_Target_Values : constant array (Buffer_Storage_Target) of GLuint := (
      GL_ARRAY_BUFFER => 34962,
      GL_ELEMENT_ARRAY_BUFFER => 34963);

   Check_Framebuffer_Status_Target_Values : constant array (Check_Framebuffer_Status_Target) of GLuint := (
      GL_FRAMEBUFFER => 36160);

   Copy_Buffer_Sub_Data_Target_Values : constant array (Copy_Buffer_Sub_Data_Target) of GLuint := (
      GL_ARRAY_BUFFER => 34962,
      GL_ELEMENT_ARRAY_BUFFER => 34963);

   Shader_Type_Values : constant array (Shader_Type) of GLuint := (
      GL_FRAGMENT_SHADER => 35632,
      GL_VERTEX_SHADER => 35633);

   Debug_Source_Values : constant array (Debug_Source) of GLuint := (
      GL_DONT_CARE => 4352);

   Debug_Type_Values : constant array (Debug_Type) of GLuint := (
      GL_DONT_CARE => 4352);

   Debug_Severity_Values : constant array (Debug_Severity) of GLuint := (
      GL_DONT_CARE => 4352);

   Sampler_Parameter_Name_Values : constant array (Sampler_Parameter_Name) of GLuint := (
      GL_TEXTURE_MAG_FILTER => 10240,
      GL_TEXTURE_MIN_FILTER => 10241,
      GL_TEXTURE_WRAP_S => 10242,
      GL_TEXTURE_WRAP_T => 10243);

   Object_Identifier_Values : constant array (Object_Identifier) of GLuint := (
      GL_TEXTURE => 5890,
      GL_FRAMEBUFFER => 36160,
      GL_RENDERBUFFER => 36161);

   Color_Buffer_Values : constant array (Color_Buffer) of GLuint := (
      GL_NONE => 0,
      GL_FRONT => 1028,
      GL_BACK => 1029,
      GL_FRONT_AND_BACK => 1032,
      GL_COLOR_ATTACHMENT0 => 36064);

   Vertex_Array_PName_Values : constant array (Vertex_Array_PName) of GLuint := (
      GL_VERTEX_ATTRIB_ARRAY_ENABLED => 34338,
      GL_VERTEX_ATTRIB_ARRAY_SIZE => 34339,
      GL_VERTEX_ATTRIB_ARRAY_STRIDE => 34340,
      GL_VERTEX_ATTRIB_ARRAY_TYPE => 34341,
      GL_VERTEX_ATTRIB_ARRAY_NORMALIZED => 34922);

   Shader_Parameter_Name_Values : constant array (Shader_Parameter_Name) of GLuint := (
      GL_SHADER_TYPE => 35663,
      GL_DELETE_STATUS => 35712,
      GL_COMPILE_STATUS => 35713,
      GL_INFO_LOG_LENGTH => 35716,
      GL_SHADER_SOURCE_LENGTH => 35720);

   Pipeline_Parameter_Name_Values : constant array (Pipeline_Parameter_Name) of GLuint := (
      GL_FRAGMENT_SHADER => 35632,
      GL_VERTEX_SHADER => 35633,
      GL_INFO_LOG_LENGTH => 35716);

   Vertex_Attrib_Enum_Values : constant array (Vertex_Attrib_Enum) of GLuint := (
      GL_VERTEX_ATTRIB_ARRAY_ENABLED => 34338,
      GL_VERTEX_ATTRIB_ARRAY_SIZE => 34339,
      GL_VERTEX_ATTRIB_ARRAY_STRIDE => 34340,
      GL_VERTEX_ATTRIB_ARRAY_TYPE => 34341,
      GL_CURRENT_VERTEX_ATTRIB => 34342,
      GL_VERTEX_ATTRIB_ARRAY_NORMALIZED => 34922,
      GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING => 34975);

   Vertex_Attrib_Type_Values : constant array (Vertex_Attrib_Type) of GLuint := (
      GL_BYTE => 5120,
      GL_UNSIGNED_BYTE => 5121,
      GL_SHORT => 5122,
      GL_UNSIGNED_SHORT => 5123,
      GL_INT => 5124,
      GL_UNSIGNED_INT => 5125,
      GL_FLOAT => 5126,
      GL_FIXED => 5132);

   Attribute_Type_Values : constant array (Attribute_Type) of GLuint := (
      GL_FLOAT_VEC2 => 35664,
      GL_FLOAT_VEC3 => 35665,
      GL_FLOAT_VEC4 => 35666,
      GL_INT_VEC2 => 35667,
      GL_INT_VEC3 => 35668,
      GL_INT_VEC4 => 35669,
      GL_BOOL => 35670,
      GL_BOOL_VEC2 => 35671,
      GL_BOOL_VEC3 => 35672,
      GL_BOOL_VEC4 => 35673,
      GL_FLOAT_MAT2 => 35674,
      GL_FLOAT_MAT3 => 35675,
      GL_FLOAT_MAT4 => 35676,
      GL_SAMPLER_2D => 35678,
      GL_SAMPLER_CUBE => 35680);

   Internal_Format_PName_Values : constant array (Internal_Format_PName) of GLuint := (
      GL_SAMPLES => 32937);

   Framebuffer_Attachment_Parameter_Name_Values : constant array (Framebuffer_Attachment_Parameter_Name) of GLuint := (
      GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME => 36049,
      GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL => 36050,
      GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE => 36051);

   Precision_Type_Values : constant array (Precision_Type) of GLuint := (
      GL_LOW_FLOAT => 36336,
      GL_MEDIUM_FLOAT => 36337,
      GL_HIGH_FLOAT => 36338,
      GL_LOW_INT => 36339,
      GL_MEDIUM_INT => 36340,
      GL_HIGH_INT => 36341);

   Vertex_Attrib_Pointer_Type_Values : constant array (Vertex_Attrib_Pointer_Type) of GLuint := (
      GL_BYTE => 5120,
      GL_UNSIGNED_BYTE => 5121,
      GL_SHORT => 5122,
      GL_UNSIGNED_SHORT => 5123,
      GL_INT => 5124,
      GL_UNSIGNED_INT => 5125,
      GL_FLOAT => 5126,
      GL_FIXED => 5132);

   Get_Framebuffer_Parameter_Values : constant array (Get_Framebuffer_Parameter) of GLuint := (
      GL_SAMPLE_BUFFERS => 32936,
      GL_SAMPLES => 32937,
      GL_IMPLEMENTATION_COLOR_READ_TYPE => 35738,
      GL_IMPLEMENTATION_COLOR_READ_FORMAT => 35739);

   Path_Gen_Mode_Values : constant array (Path_Gen_Mode) of GLuint := (
      GL_NONE => 0);

   Path_Transform_Type_Values : constant array (Path_Transform_Type) of GLuint := (
      GL_NONE => 0);

   Path_Font_Style_Values : constant array (Path_Font_Style) of GLuint := (
      GL_NONE => 0);

   Program_Property_ARB_Values : constant array (Program_Property_ARB) of GLuint := (
      GL_DELETE_STATUS => 35712,
      GL_LINK_STATUS => 35714,
      GL_VALIDATE_STATUS => 35715,
      GL_INFO_LOG_LENGTH => 35716,
      GL_ATTACHED_SHADERS => 35717,
      GL_ACTIVE_UNIFORMS => 35718,
      GL_ACTIVE_UNIFORM_MAX_LENGTH => 35719,
      GL_ACTIVE_ATTRIBUTES => 35721,
      GL_ACTIVE_ATTRIBUTE_MAX_LENGTH => 35722);
   ---------------------
   -- Active_Texture --
   ---------------------

   procedure Active_Texture
     (Context : in out Context_WebGL_Type'Class;
      Texture : Texture_Unit)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.activeTexture("
         & Texture_Unit_Values (Texture)'Image
           & ")");
      else
Context.Execute (
        "activeTexture("
         & Texture_Unit_Values (Texture)'Image
        & ")");
      end if;
   end Active_Texture;

   --------------------
   -- Attach_Shader --
   --------------------

   procedure Attach_Shader
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint;
      Shader : GLuint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.attachShader("
         & Support.Indexed_Object_Reference (Context, Program) & ","
         & Support.Indexed_Object_Reference (Context, Shader)
           & ")");
      else
Context.Execute (
        "attachShader("
         & Support.Indexed_Object_Reference (Context, Program) & ","
         & Support.Indexed_Object_Reference (Context, Shader)
        & ")");
      end if;
   end Attach_Shader;

   --------------------------
   -- Bind_Attrib_Location --
   --------------------------

   procedure Bind_Attrib_Location
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint;
      Index : GLuint;
      Name : String)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.bindAttribLocation("
         & Support.Indexed_Object_Reference (Context, Program) & ","
         & Index'Image & ","
         & "'" & Escape_Quotes (Name) & "'"
           & ")");
      else
Context.Execute (
        "bindAttribLocation("
         & Support.Indexed_Object_Reference (Context, Program) & ","
         & Index'Image & ","
         & "'" & Escape_Quotes (Name) & "'"
        & ")");
      end if;
   end Bind_Attrib_Location;

   ------------------
   -- Bind_Buffer --
   ------------------

   procedure Bind_Buffer
     (Context : in out Context_WebGL_Type'Class;
      Target : Buffer_Target_ARB;
      Buffer : GLuint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.bindBuffer("
         & Buffer_Target_ARB_Values (Target)'Image & ","
         & Support.Indexed_Object_Reference (Context, Buffer)
           & ")");
      else
Context.Execute (
        "bindBuffer("
         & Buffer_Target_ARB_Values (Target)'Image & ","
         & Support.Indexed_Object_Reference (Context, Buffer)
        & ")");
      end if;
   end Bind_Buffer;

   -----------------------
   -- Bind_Framebuffer --
   -----------------------

   procedure Bind_Framebuffer
     (Context : in out Context_WebGL_Type'Class;
      Target : Framebuffer_Target;
      Framebuffer : GLuint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.bindFramebuffer("
         & Framebuffer_Target_Values (Target)'Image & ","
         & Framebuffer'Image
           & ")");
      else
Context.Execute (
        "bindFramebuffer("
         & Framebuffer_Target_Values (Target)'Image & ","
         & Framebuffer'Image
        & ")");
      end if;
   end Bind_Framebuffer;

   ------------------------
   -- Bind_Renderbuffer --
   ------------------------

   procedure Bind_Renderbuffer
     (Context : in out Context_WebGL_Type'Class;
      Target : Renderbuffer_Target;
      Renderbuffer : GLuint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.bindRenderbuffer("
         & Renderbuffer_Target_Values (Target)'Image & ","
         & Renderbuffer'Image
           & ")");
      else
Context.Execute (
        "bindRenderbuffer("
         & Renderbuffer_Target_Values (Target)'Image & ","
         & Renderbuffer'Image
        & ")");
      end if;
   end Bind_Renderbuffer;

   -------------------
   -- Bind_Texture --
   -------------------

   procedure Bind_Texture
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Texture : GLuint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.bindTexture("
         & Texture_Target_Values (Target)'Image & ","
         & Support.Indexed_Object_Reference (Context, Texture)
           & ")");
      else
Context.Execute (
        "bindTexture("
         & Texture_Target_Values (Target)'Image & ","
         & Support.Indexed_Object_Reference (Context, Texture)
        & ")");
      end if;
   end Bind_Texture;

   ------------------
   -- Blend_Color --
   ------------------

   procedure Blend_Color
     (Context : in out Context_WebGL_Type'Class;
      Red : GLfloat;
      Green : GLfloat;
      Blue : GLfloat;
      Alpha : GLfloat)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.blendColor("
         & Support.Image (Red) & ","
         & Support.Image (Green) & ","
         & Support.Image (Blue) & ","
         & Support.Image (Alpha)
           & ")");
      else
Context.Execute (
        "blendColor("
         & Support.Image (Red) & ","
         & Support.Image (Green) & ","
         & Support.Image (Blue) & ","
         & Support.Image (Alpha)
        & ")");
      end if;
   end Blend_Color;

   ---------------------
   -- Blend_Equation --
   ---------------------

   procedure Blend_Equation
     (Context : in out Context_WebGL_Type'Class;
      Mode : Blend_Equation_Mode_EXT)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.blendEquation("
         & Blend_Equation_Mode_EXT_Values (Mode)'Image
           & ")");
      else
Context.Execute (
        "blendEquation("
         & Blend_Equation_Mode_EXT_Values (Mode)'Image
        & ")");
      end if;
   end Blend_Equation;

   -----------------------------
   -- Blend_Equation_Separate --
   -----------------------------

   procedure Blend_Equation_Separate
     (Context : in out Context_WebGL_Type'Class;
      ModeRGB : Blend_Equation_Mode_EXT;
      ModeAlpha : Blend_Equation_Mode_EXT)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.blendEquationSeparate("
         & Blend_Equation_Mode_EXT_Values (ModeRGB)'Image & ","
         & Blend_Equation_Mode_EXT_Values (ModeAlpha)'Image
           & ")");
      else
Context.Execute (
        "blendEquationSeparate("
         & Blend_Equation_Mode_EXT_Values (ModeRGB)'Image & ","
         & Blend_Equation_Mode_EXT_Values (ModeAlpha)'Image
        & ")");
      end if;
   end Blend_Equation_Separate;

   -----------------
   -- Blend_Func --
   -----------------

   procedure Blend_Func
     (Context : in out Context_WebGL_Type'Class;
      Sfactor : Blending_Factor;
      Dfactor : Blending_Factor)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.blendFunc("
         & Blending_Factor_Values (Sfactor)'Image & ","
         & Blending_Factor_Values (Dfactor)'Image
           & ")");
      else
Context.Execute (
        "blendFunc("
         & Blending_Factor_Values (Sfactor)'Image & ","
         & Blending_Factor_Values (Dfactor)'Image
        & ")");
      end if;
   end Blend_Func;

   -------------------------
   -- Blend_Func_Separate --
   -------------------------

   procedure Blend_Func_Separate
     (Context : in out Context_WebGL_Type'Class;
      SfactorRGB : Blending_Factor;
      DfactorRGB : Blending_Factor;
      SfactorAlpha : Blending_Factor;
      DfactorAlpha : Blending_Factor)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.blendFuncSeparate("
         & Blending_Factor_Values (SfactorRGB)'Image & ","
         & Blending_Factor_Values (DfactorRGB)'Image & ","
         & Blending_Factor_Values (SfactorAlpha)'Image & ","
         & Blending_Factor_Values (DfactorAlpha)'Image
           & ")");
      else
Context.Execute (
        "blendFuncSeparate("
         & Blending_Factor_Values (SfactorRGB)'Image & ","
         & Blending_Factor_Values (DfactorRGB)'Image & ","
         & Blending_Factor_Values (SfactorAlpha)'Image & ","
         & Blending_Factor_Values (DfactorAlpha)'Image
        & ")");
      end if;
   end Blend_Func_Separate;

   ------------------
   -- Buffer_Data --
   ------------------

   procedure Buffer_Data
     (Context : in out Context_WebGL_Type'Class;
      Target : Buffer_Target_ARB;
      Data : Float_Array;
      Usage : Buffer_Usage_ARB)
   is
      use Ada.Strings.Unbounded;
      Data_Image : Unbounded_String;
   begin
      for X of data loop
         if Data_Image /= "" then
            Data_Image := Data_Image & ",";
         end if;
         Data_Image := Data_Image & X'Image;
      end loop;
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.bufferData("
         & Buffer_Target_ARB_Values (Target)'Image & ","
         & "new Float32Array([" & To_String (Data_Image) & "])" & ","
         & Buffer_Usage_ARB_Values (Usage)'Image
           & ")");
      else
Context.Execute (
        "bufferData("
         & Buffer_Target_ARB_Values (Target)'Image & ","
         & "new Float32Array([" & To_String (Data_Image) & "])" & ","
         & Buffer_Usage_ARB_Values (Usage)'Image
        & ")");
      end if;
   end Buffer_Data;

   ------------------
   -- Buffer_Data --
   ------------------

   procedure Buffer_Data
     (Context : in out Context_WebGL_Type'Class;
      Target : Buffer_Target_ARB;
      Data : Uint16_Array;
      Usage : Buffer_Usage_ARB)
   is
      use Ada.Strings.Unbounded;
      Data_Image : Unbounded_String;
   begin
      for X of data loop
         if Data_Image /= "" then
            Data_Image := Data_Image & ",";
         end if;
         Data_Image := Data_Image & X'Image;
      end loop;
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.bufferData("
         & Buffer_Target_ARB_Values (Target)'Image & ","
         & "new Uint16Array([" & To_String (Data_Image) & "])" & ","
         & Buffer_Usage_ARB_Values (Usage)'Image
           & ")");
      else
Context.Execute (
        "bufferData("
         & Buffer_Target_ARB_Values (Target)'Image & ","
         & "new Uint16Array([" & To_String (Data_Image) & "])" & ","
         & Buffer_Usage_ARB_Values (Usage)'Image
        & ")");
      end if;
   end Buffer_Data;

   ------------------
   -- Buffer_Data --
   ------------------

   procedure Buffer_Data
     (Context : in out Context_WebGL_Type'Class;
      Target : Buffer_Target_ARB;
      Data : Uint8_Array;
      Usage : Buffer_Usage_ARB)
   is
      use Ada.Strings.Unbounded;
      Data_Image : Unbounded_String;
   begin
      for X of data loop
         if Data_Image /= "" then
            Data_Image := Data_Image & ",";
         end if;
         Data_Image := Data_Image & X'Image;
      end loop;
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.bufferData("
         & Buffer_Target_ARB_Values (Target)'Image & ","
         & "new Uint8Array([" & To_String (Data_Image) & "])" & ","
         & Buffer_Usage_ARB_Values (Usage)'Image
           & ")");
      else
Context.Execute (
        "bufferData("
         & Buffer_Target_ARB_Values (Target)'Image & ","
         & "new Uint8Array([" & To_String (Data_Image) & "])" & ","
         & Buffer_Usage_ARB_Values (Usage)'Image
        & ")");
      end if;
   end Buffer_Data;

   ---------------------
   -- Buffer_Sub_Data --
   ---------------------

   procedure Buffer_Sub_Data
     (Context : in out Context_WebGL_Type'Class;
      Target : Buffer_Target_ARB;
      Offset : GLintptr;
      Data : Float_Array)
   is
      use Ada.Strings.Unbounded;
      Data_Image : Unbounded_String;
   begin
      for X of data loop
         if Data_Image /= "" then
            Data_Image := Data_Image & ",";
         end if;
         Data_Image := Data_Image & X'Image;
      end loop;
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.bufferSubData("
         & Buffer_Target_ARB_Values (Target)'Image & ","
         & Offset'Image & ","
         & "new Float32Array([" & To_String (Data_Image) & "])"
           & ")");
      else
Context.Execute (
        "bufferSubData("
         & Buffer_Target_ARB_Values (Target)'Image & ","
         & Offset'Image & ","
         & "new Float32Array([" & To_String (Data_Image) & "])"
        & ")");
      end if;
   end Buffer_Sub_Data;

   ---------------------
   -- Buffer_Sub_Data --
   ---------------------

   procedure Buffer_Sub_Data
     (Context : in out Context_WebGL_Type'Class;
      Target : Buffer_Target_ARB;
      Offset : GLintptr;
      Data : Uint16_Array)
   is
      use Ada.Strings.Unbounded;
      Data_Image : Unbounded_String;
   begin
      for X of data loop
         if Data_Image /= "" then
            Data_Image := Data_Image & ",";
         end if;
         Data_Image := Data_Image & X'Image;
      end loop;
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.bufferSubData("
         & Buffer_Target_ARB_Values (Target)'Image & ","
         & Offset'Image & ","
         & "new Uint16Array([" & To_String (Data_Image) & "])"
           & ")");
      else
Context.Execute (
        "bufferSubData("
         & Buffer_Target_ARB_Values (Target)'Image & ","
         & Offset'Image & ","
         & "new Uint16Array([" & To_String (Data_Image) & "])"
        & ")");
      end if;
   end Buffer_Sub_Data;

   ---------------------
   -- Buffer_Sub_Data --
   ---------------------

   procedure Buffer_Sub_Data
     (Context : in out Context_WebGL_Type'Class;
      Target : Buffer_Target_ARB;
      Offset : GLintptr;
      Data : Uint8_Array)
   is
      use Ada.Strings.Unbounded;
      Data_Image : Unbounded_String;
   begin
      for X of data loop
         if Data_Image /= "" then
            Data_Image := Data_Image & ",";
         end if;
         Data_Image := Data_Image & X'Image;
      end loop;
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.bufferSubData("
         & Buffer_Target_ARB_Values (Target)'Image & ","
         & Offset'Image & ","
         & "new Uint8Array([" & To_String (Data_Image) & "])"
           & ")");
      else
Context.Execute (
        "bufferSubData("
         & Buffer_Target_ARB_Values (Target)'Image & ","
         & Offset'Image & ","
         & "new Uint8Array([" & To_String (Data_Image) & "])"
        & ")");
      end if;
   end Buffer_Sub_Data;

   -------------
   -- Clear --
   -------------

   procedure Clear
     (Context : in out Context_WebGL_Type'Class;
      Mask : Clear_Buffer_Mask_Array)
   is
      Mask_Uint : GLuint := 0;
   begin
      for X of Mask loop
         Mask_Uint := Mask_Uint or Clear_Buffer_Mask_Values (X);
      end loop;
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.clear("
         & Mask_Uint'Image
           & ")");
      else
Context.Execute (
        "clear("
         & Mask_Uint'Image
        & ")");
      end if;
   end Clear;

   ------------------
   -- Clear_Color --
   ------------------

   procedure Clear_Color
     (Context : in out Context_WebGL_Type'Class;
      Red : GLfloat;
      Green : GLfloat;
      Blue : GLfloat;
      Alpha : GLfloat)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.clearColor("
         & Support.Image (Red) & ","
         & Support.Image (Green) & ","
         & Support.Image (Blue) & ","
         & Support.Image (Alpha)
           & ")");
      else
Context.Execute (
        "clearColor("
         & Support.Image (Red) & ","
         & Support.Image (Green) & ","
         & Support.Image (Blue) & ","
         & Support.Image (Alpha)
        & ")");
      end if;
   end Clear_Color;

   -------------------
   -- Clear_Depth --
   -------------------

   procedure Clear_Depth
     (Context : in out Context_WebGL_Type'Class;
      D : GLfloat)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.clearDepth("
         & Support.Image (D)
           & ")");
      else
Context.Execute (
        "clearDepth("
         & Support.Image (D)
        & ")");
      end if;
   end Clear_Depth;

   --------------------
   -- Clear_Stencil --
   --------------------

   procedure Clear_Stencil
     (Context : in out Context_WebGL_Type'Class;
      S : GLint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.clearStencil("
         & S'Image
           & ")");
      else
Context.Execute (
        "clearStencil("
         & S'Image
        & ")");
      end if;
   end Clear_Stencil;

   -----------------
   -- Color_Mask --
   -----------------

   procedure Color_Mask
     (Context : in out Context_WebGL_Type'Class;
      Red : Boolean;
      Green : Boolean;
      Blue : Boolean;
      Alpha : Boolean)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.colorMask("
         & GLEnum_Property (Context, Red'Image) & ","
         & GLEnum_Property (Context, Green'Image) & ","
         & GLEnum_Property (Context, Blue'Image) & ","
         & GLEnum_Property (Context, Alpha'Image)
           & ")");
      else
Context.Execute (
        "colorMask("
         & GLEnum_Property (Context, Red'Image) & ","
         & GLEnum_Property (Context, Green'Image) & ","
         & GLEnum_Property (Context, Blue'Image) & ","
         & GLEnum_Property (Context, Alpha'Image)
        & ")");
      end if;
   end Color_Mask;

   ---------------------
   -- Compile_Shader --
   ---------------------

   procedure Compile_Shader
     (Context : in out Context_WebGL_Type'Class;
      Shader : GLuint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.compileShader("
         & Support.Indexed_Object_Reference (Context, Shader)
           & ")");
      else
Context.Execute (
        "compileShader("
         & Support.Indexed_Object_Reference (Context, Shader)
        & ")");
      end if;
   end Compile_Shader;

   ----------------------------
   -- Compressed_Tex_Image_2D --
   ----------------------------

   procedure Compressed_Tex_Image_2D
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Level : GLint;
      Internalformat : Internal_Format;
      Width : GLsizei;
      Height : GLsizei;
      Border : GLint;
      ImageSize : GLsizei;
      Data : Float_Array)
   is
      use Ada.Strings.Unbounded;
      Data_Image : Unbounded_String;
   begin
      for X of data loop
         if Data_Image /= "" then
            Data_Image := Data_Image & ",";
         end if;
         Data_Image := Data_Image & X'Image;
      end loop;
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.compressedTexImage2D("
         & Texture_Target_Values (Target)'Image & ","
         & Level'Image & ","
         & Internal_Format_Values (Internalformat)'Image & ","
         & Width'Image & ","
         & Height'Image & ","
         & Border'Image & ","
         & ImageSize'Image & ","
         & "new Float32Array([" & To_String (Data_Image) & "])"
           & ")");
      else
Context.Execute (
        "compressedTexImage2D("
         & Texture_Target_Values (Target)'Image & ","
         & Level'Image & ","
         & Internal_Format_Values (Internalformat)'Image & ","
         & Width'Image & ","
         & Height'Image & ","
         & Border'Image & ","
         & ImageSize'Image & ","
         & "new Float32Array([" & To_String (Data_Image) & "])"
        & ")");
      end if;
   end Compressed_Tex_Image_2D;

   ----------------------------
   -- Compressed_Tex_Image_2D --
   ----------------------------

   procedure Compressed_Tex_Image_2D
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Level : GLint;
      Internalformat : Internal_Format;
      Width : GLsizei;
      Height : GLsizei;
      Border : GLint;
      ImageSize : GLsizei;
      Data : Uint16_Array)
   is
      use Ada.Strings.Unbounded;
      Data_Image : Unbounded_String;
   begin
      for X of data loop
         if Data_Image /= "" then
            Data_Image := Data_Image & ",";
         end if;
         Data_Image := Data_Image & X'Image;
      end loop;
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.compressedTexImage2D("
         & Texture_Target_Values (Target)'Image & ","
         & Level'Image & ","
         & Internal_Format_Values (Internalformat)'Image & ","
         & Width'Image & ","
         & Height'Image & ","
         & Border'Image & ","
         & ImageSize'Image & ","
         & "new Uint16Array([" & To_String (Data_Image) & "])"
           & ")");
      else
Context.Execute (
        "compressedTexImage2D("
         & Texture_Target_Values (Target)'Image & ","
         & Level'Image & ","
         & Internal_Format_Values (Internalformat)'Image & ","
         & Width'Image & ","
         & Height'Image & ","
         & Border'Image & ","
         & ImageSize'Image & ","
         & "new Uint16Array([" & To_String (Data_Image) & "])"
        & ")");
      end if;
   end Compressed_Tex_Image_2D;

   ----------------------------
   -- Compressed_Tex_Image_2D --
   ----------------------------

   procedure Compressed_Tex_Image_2D
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Level : GLint;
      Internalformat : Internal_Format;
      Width : GLsizei;
      Height : GLsizei;
      Border : GLint;
      ImageSize : GLsizei;
      Data : Uint8_Array)
   is
      use Ada.Strings.Unbounded;
      Data_Image : Unbounded_String;
   begin
      for X of data loop
         if Data_Image /= "" then
            Data_Image := Data_Image & ",";
         end if;
         Data_Image := Data_Image & X'Image;
      end loop;
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.compressedTexImage2D("
         & Texture_Target_Values (Target)'Image & ","
         & Level'Image & ","
         & Internal_Format_Values (Internalformat)'Image & ","
         & Width'Image & ","
         & Height'Image & ","
         & Border'Image & ","
         & ImageSize'Image & ","
         & "new Uint8Array([" & To_String (Data_Image) & "])"
           & ")");
      else
Context.Execute (
        "compressedTexImage2D("
         & Texture_Target_Values (Target)'Image & ","
         & Level'Image & ","
         & Internal_Format_Values (Internalformat)'Image & ","
         & Width'Image & ","
         & Height'Image & ","
         & Border'Image & ","
         & ImageSize'Image & ","
         & "new Uint8Array([" & To_String (Data_Image) & "])"
        & ")");
      end if;
   end Compressed_Tex_Image_2D;

   -------------------------------
   -- Compressed_Tex_Sub_Image_2D --
   -------------------------------

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
      Data : Float_Array)
   is
      use Ada.Strings.Unbounded;
      Data_Image : Unbounded_String;
   begin
      for X of data loop
         if Data_Image /= "" then
            Data_Image := Data_Image & ",";
         end if;
         Data_Image := Data_Image & X'Image;
      end loop;
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.compressedTexSubImage2D("
         & Texture_Target_Values (Target)'Image & ","
         & Level'Image & ","
         & Xoffset'Image & ","
         & Yoffset'Image & ","
         & Width'Image & ","
         & Height'Image & ","
         & Pixel_Format_Values (Format)'Image & ","
         & ImageSize'Image & ","
         & "new Float32Array([" & To_String (Data_Image) & "])"
           & ")");
      else
Context.Execute (
        "compressedTexSubImage2D("
         & Texture_Target_Values (Target)'Image & ","
         & Level'Image & ","
         & Xoffset'Image & ","
         & Yoffset'Image & ","
         & Width'Image & ","
         & Height'Image & ","
         & Pixel_Format_Values (Format)'Image & ","
         & ImageSize'Image & ","
         & "new Float32Array([" & To_String (Data_Image) & "])"
        & ")");
      end if;
   end Compressed_Tex_Sub_Image_2D;

   -------------------------------
   -- Compressed_Tex_Sub_Image_2D --
   -------------------------------

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
      Data : Uint16_Array)
   is
      use Ada.Strings.Unbounded;
      Data_Image : Unbounded_String;
   begin
      for X of data loop
         if Data_Image /= "" then
            Data_Image := Data_Image & ",";
         end if;
         Data_Image := Data_Image & X'Image;
      end loop;
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.compressedTexSubImage2D("
         & Texture_Target_Values (Target)'Image & ","
         & Level'Image & ","
         & Xoffset'Image & ","
         & Yoffset'Image & ","
         & Width'Image & ","
         & Height'Image & ","
         & Pixel_Format_Values (Format)'Image & ","
         & ImageSize'Image & ","
         & "new Uint16Array([" & To_String (Data_Image) & "])"
           & ")");
      else
Context.Execute (
        "compressedTexSubImage2D("
         & Texture_Target_Values (Target)'Image & ","
         & Level'Image & ","
         & Xoffset'Image & ","
         & Yoffset'Image & ","
         & Width'Image & ","
         & Height'Image & ","
         & Pixel_Format_Values (Format)'Image & ","
         & ImageSize'Image & ","
         & "new Uint16Array([" & To_String (Data_Image) & "])"
        & ")");
      end if;
   end Compressed_Tex_Sub_Image_2D;

   -------------------------------
   -- Compressed_Tex_Sub_Image_2D --
   -------------------------------

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
      Data : Uint8_Array)
   is
      use Ada.Strings.Unbounded;
      Data_Image : Unbounded_String;
   begin
      for X of data loop
         if Data_Image /= "" then
            Data_Image := Data_Image & ",";
         end if;
         Data_Image := Data_Image & X'Image;
      end loop;
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.compressedTexSubImage2D("
         & Texture_Target_Values (Target)'Image & ","
         & Level'Image & ","
         & Xoffset'Image & ","
         & Yoffset'Image & ","
         & Width'Image & ","
         & Height'Image & ","
         & Pixel_Format_Values (Format)'Image & ","
         & ImageSize'Image & ","
         & "new Uint8Array([" & To_String (Data_Image) & "])"
           & ")");
      else
Context.Execute (
        "compressedTexSubImage2D("
         & Texture_Target_Values (Target)'Image & ","
         & Level'Image & ","
         & Xoffset'Image & ","
         & Yoffset'Image & ","
         & Width'Image & ","
         & Height'Image & ","
         & Pixel_Format_Values (Format)'Image & ","
         & ImageSize'Image & ","
         & "new Uint8Array([" & To_String (Data_Image) & "])"
        & ")");
      end if;
   end Compressed_Tex_Sub_Image_2D;

   ----------------------
   -- Copy_Tex_Image_2D --
   ----------------------

   procedure Copy_Tex_Image_2D
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Level : GLint;
      Internalformat : Internal_Format;
      X : GLint;
      Y : GLint;
      Width : GLsizei;
      Height : GLsizei;
      Border : GLint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.copyTexImage2D("
         & Texture_Target_Values (Target)'Image & ","
         & Level'Image & ","
         & Internal_Format_Values (Internalformat)'Image & ","
         & X'Image & ","
         & Y'Image & ","
         & Width'Image & ","
         & Height'Image & ","
         & Border'Image
           & ")");
      else
Context.Execute (
        "copyTexImage2D("
         & Texture_Target_Values (Target)'Image & ","
         & Level'Image & ","
         & Internal_Format_Values (Internalformat)'Image & ","
         & X'Image & ","
         & Y'Image & ","
         & Width'Image & ","
         & Height'Image & ","
         & Border'Image
        & ")");
      end if;
   end Copy_Tex_Image_2D;

   -------------------------
   -- Copy_Tex_Sub_Image_2D --
   -------------------------

   procedure Copy_Tex_Sub_Image_2D
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Level : GLint;
      Xoffset : GLint;
      Yoffset : GLint;
      X : GLint;
      Y : GLint;
      Width : GLsizei;
      Height : GLsizei)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.copyTexSubImage2D("
         & Texture_Target_Values (Target)'Image & ","
         & Level'Image & ","
         & Xoffset'Image & ","
         & Yoffset'Image & ","
         & X'Image & ","
         & Y'Image & ","
         & Width'Image & ","
         & Height'Image
           & ")");
      else
Context.Execute (
        "copyTexSubImage2D("
         & Texture_Target_Values (Target)'Image & ","
         & Level'Image & ","
         & Xoffset'Image & ","
         & Yoffset'Image & ","
         & X'Image & ","
         & Y'Image & ","
         & Width'Image & ","
         & Height'Image
        & ")");
      end if;
   end Copy_Tex_Sub_Image_2D;

   ---------------------
   -- Create_Program --
   ---------------------

   function Create_Program
     (Context : in out Context_WebGL_Type'Class)
     return GLuint
   is
   begin
      if Context.Rendering then
         raise Constraint_Error with
            "Illegal call to glCreateProgram during render";
      else
         declare
            Result : constant GLuint := 
           Support.Indexed_Javascript_Object (Context,
        "createProgram("

        & ")");
      begin
      return Result;
   end;
      end if;
   end Create_Program;

   --------------------
   -- Create_Shader --
   --------------------

   function Create_Shader
     (Context : in out Context_WebGL_Type'Class;
      Item_Type : Shader_Type)
     return GLuint
   is
   begin
      if Context.Rendering then
         raise Constraint_Error with
            "Illegal call to glCreateShader during render";
      else
         declare
            Result : constant GLuint := 
           Support.Indexed_Javascript_Object (Context,
        "createShader("
         & Shader_Type_Values (Item_Type)'Image
        & ")");
      begin
      return Result;
   end;
      end if;
   end Create_Shader;

   ----------------
   -- Cull_Face --
   ----------------

   procedure Cull_Face
     (Context : in out Context_WebGL_Type'Class;
      Mode : Cull_Face_Mode)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.cullFace("
         & Cull_Face_Mode_Values (Mode)'Image
           & ")");
      else
Context.Execute (
        "cullFace("
         & Cull_Face_Mode_Values (Mode)'Image
        & ")");
      end if;
   end Cull_Face;

   ---------------------
   -- Delete_Buffers --
   ---------------------

   procedure Delete_Buffers
     (Context : in out Context_WebGL_Type'Class;
      N : GLsizei;
      Buffers : GLuint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.deleteBuffers("
         & N'Image & ","
         & Buffers'Image
           & ")");
      else
Context.Execute (
        "deleteBuffers("
         & N'Image & ","
         & Buffers'Image
        & ")");
      end if;
   end Delete_Buffers;

   --------------------------
   -- Delete_Framebuffers --
   --------------------------

   procedure Delete_Framebuffers
     (Context : in out Context_WebGL_Type'Class;
      N : GLsizei;
      Framebuffers : GLuint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.deleteFramebuffers("
         & N'Image & ","
         & Framebuffers'Image
           & ")");
      else
Context.Execute (
        "deleteFramebuffers("
         & N'Image & ","
         & Framebuffers'Image
        & ")");
      end if;
   end Delete_Framebuffers;

   ---------------------
   -- Delete_Program --
   ---------------------

   procedure Delete_Program
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.deleteProgram("
         & Support.Indexed_Object_Reference (Context, Program)
           & ")");
      else
Context.Execute (
        "deleteProgram("
         & Support.Indexed_Object_Reference (Context, Program)
        & ")");
      end if;
   end Delete_Program;

   ---------------------------
   -- Delete_Renderbuffers --
   ---------------------------

   procedure Delete_Renderbuffers
     (Context : in out Context_WebGL_Type'Class;
      N : GLsizei;
      Renderbuffers : GLuint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.deleteRenderbuffers("
         & N'Image & ","
         & Renderbuffers'Image
           & ")");
      else
Context.Execute (
        "deleteRenderbuffers("
         & N'Image & ","
         & Renderbuffers'Image
        & ")");
      end if;
   end Delete_Renderbuffers;

   --------------------
   -- Delete_Shader --
   --------------------

   procedure Delete_Shader
     (Context : in out Context_WebGL_Type'Class;
      Shader : GLuint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.deleteShader("
         & Support.Indexed_Object_Reference (Context, Shader)
           & ")");
      else
Context.Execute (
        "deleteShader("
         & Support.Indexed_Object_Reference (Context, Shader)
        & ")");
      end if;
   end Delete_Shader;

   ----------------------
   -- Delete_Textures --
   ----------------------

   procedure Delete_Textures
     (Context : in out Context_WebGL_Type'Class;
      N : GLsizei;
      Textures : GLuint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.deleteTextures("
         & N'Image & ","
         & Textures'Image
           & ")");
      else
Context.Execute (
        "deleteTextures("
         & N'Image & ","
         & Textures'Image
        & ")");
      end if;
   end Delete_Textures;

   -----------------
   -- Depth_Func --
   -----------------

   procedure Depth_Func
     (Context : in out Context_WebGL_Type'Class;
      Func : Depth_Function)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.depthFunc("
         & Depth_Function_Values (Func)'Image
           & ")");
      else
Context.Execute (
        "depthFunc("
         & Depth_Function_Values (Func)'Image
        & ")");
      end if;
   end Depth_Func;

   -----------------
   -- Depth_Mask --
   -----------------

   procedure Depth_Mask
     (Context : in out Context_WebGL_Type'Class;
      Flag : Boolean)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.depthMask("
         & GLEnum_Property (Context, Flag'Image)
           & ")");
      else
Context.Execute (
        "depthMask("
         & GLEnum_Property (Context, Flag'Image)
        & ")");
      end if;
   end Depth_Mask;

   -------------------
   -- Depth_Range --
   -------------------

   procedure Depth_Range
     (Context : in out Context_WebGL_Type'Class;
      N : GLfloat;
      F : GLfloat)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.depthRange("
         & Support.Image (N) & ","
         & Support.Image (F)
           & ")");
      else
Context.Execute (
        "depthRange("
         & Support.Image (N) & ","
         & Support.Image (F)
        & ")");
      end if;
   end Depth_Range;

   --------------------
   -- Detach_Shader --
   --------------------

   procedure Detach_Shader
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint;
      Shader : GLuint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.detachShader("
         & Support.Indexed_Object_Reference (Context, Program) & ","
         & Support.Indexed_Object_Reference (Context, Shader)
           & ")");
      else
Context.Execute (
        "detachShader("
         & Support.Indexed_Object_Reference (Context, Program) & ","
         & Support.Indexed_Object_Reference (Context, Shader)
        & ")");
      end if;
   end Detach_Shader;

   ---------------
   -- Disable --
   ---------------

   procedure Disable
     (Context : in out Context_WebGL_Type'Class;
      Cap : Enable_Cap)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.disable("
         & Enable_Cap_Values (Cap)'Image
           & ")");
      else
Context.Execute (
        "disable("
         & Enable_Cap_Values (Cap)'Image
        & ")");
      end if;
   end Disable;

   --------------------------------
   -- Disable_Vertex_Attrib_Array --
   --------------------------------

   procedure Disable_Vertex_Attrib_Array
     (Context : in out Context_WebGL_Type'Class;
      Index : GLuint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.disableVertexAttribArray("
         & Index'Image
           & ")");
      else
Context.Execute (
        "disableVertexAttribArray("
         & Index'Image
        & ")");
      end if;
   end Disable_Vertex_Attrib_Array;

   ------------------
   -- Draw_Arrays --
   ------------------

   procedure Draw_Arrays
     (Context : in out Context_WebGL_Type'Class;
      Mode : Primitive_Type;
      First : GLint;
      Count : GLsizei)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.drawArrays("
         & Primitive_Type_Values (Mode)'Image & ","
         & First'Image & ","
         & Count'Image
           & ")");
      else
Context.Execute (
        "drawArrays("
         & Primitive_Type_Values (Mode)'Image & ","
         & First'Image & ","
         & Count'Image
        & ")");
      end if;
   end Draw_Arrays;

   --------------------
   -- Draw_Elements --
   --------------------

   procedure Draw_Elements
     (Context : in out Context_WebGL_Type'Class;
      Mode : Primitive_Type;
      Count : GLsizei;
      ElementType : Draw_Elements_Type;
      Offset : System.Storage_Elements.Storage_Offset)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.drawElements("
         & Primitive_Type_Values (Mode)'Image & ","
         & Count'Image & ","
         & Draw_Elements_Type_Values (ElementType)'Image & ","
         & Offset'Image
           & ")");
      else
Context.Execute (
        "drawElements("
         & Primitive_Type_Values (Mode)'Image & ","
         & Count'Image & ","
         & Draw_Elements_Type_Values (ElementType)'Image & ","
         & Offset'Image
        & ")");
      end if;
   end Draw_Elements;

   --------------
   -- Enable --
   --------------

   procedure Enable
     (Context : in out Context_WebGL_Type'Class;
      Cap : Enable_Cap)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.enable("
         & Enable_Cap_Values (Cap)'Image
           & ")");
      else
Context.Execute (
        "enable("
         & Enable_Cap_Values (Cap)'Image
        & ")");
      end if;
   end Enable;

   -------------------------------
   -- Enable_Vertex_Attrib_Array --
   -------------------------------

   procedure Enable_Vertex_Attrib_Array
     (Context : in out Context_WebGL_Type'Class;
      Index : GLuint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.enableVertexAttribArray("
         & Index'Image
           & ")");
      else
Context.Execute (
        "enableVertexAttribArray("
         & Index'Image
        & ")");
      end if;
   end Enable_Vertex_Attrib_Array;

   --------------
   -- Finish --
   --------------

   procedure Finish
     (Context : in out Context_WebGL_Type'Class)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.finish("

           & ")");
      else
Context.Execute (
        "finish("

        & ")");
      end if;
   end Finish;

   -------------
   -- Flush --
   -------------

   procedure Flush
     (Context : in out Context_WebGL_Type'Class)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.flush("

           & ")");
      else
Context.Execute (
        "flush("

        & ")");
      end if;
   end Flush;

   -------------------------------
   -- Framebuffer_Renderbuffer --
   -------------------------------

   procedure Framebuffer_Renderbuffer
     (Context : in out Context_WebGL_Type'Class;
      Target : Framebuffer_Target;
      Attachment : Framebuffer_Attachment;
      Renderbuffertarget : Renderbuffer_Target;
      Renderbuffer : GLuint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.framebufferRenderbuffer("
         & Framebuffer_Target_Values (Target)'Image & ","
         & Framebuffer_Attachment_Values (Attachment)'Image & ","
         & Renderbuffer_Target_Values (Renderbuffertarget)'Image & ","
         & Renderbuffer'Image
           & ")");
      else
Context.Execute (
        "framebufferRenderbuffer("
         & Framebuffer_Target_Values (Target)'Image & ","
         & Framebuffer_Attachment_Values (Attachment)'Image & ","
         & Renderbuffer_Target_Values (Renderbuffertarget)'Image & ","
         & Renderbuffer'Image
        & ")");
      end if;
   end Framebuffer_Renderbuffer;

   ----------------------------
   -- Framebuffer_Texture_2D --
   ----------------------------

   procedure Framebuffer_Texture_2D
     (Context : in out Context_WebGL_Type'Class;
      Target : Framebuffer_Target;
      Attachment : Framebuffer_Attachment;
      Textarget : Texture_Target;
      Texture : GLuint;
      Level : GLint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.framebufferTexture2D("
         & Framebuffer_Target_Values (Target)'Image & ","
         & Framebuffer_Attachment_Values (Attachment)'Image & ","
         & Texture_Target_Values (Textarget)'Image & ","
         & Texture'Image & ","
         & Level'Image
           & ")");
      else
Context.Execute (
        "framebufferTexture2D("
         & Framebuffer_Target_Values (Target)'Image & ","
         & Framebuffer_Attachment_Values (Attachment)'Image & ","
         & Texture_Target_Values (Textarget)'Image & ","
         & Texture'Image & ","
         & Level'Image
        & ")");
      end if;
   end Framebuffer_Texture_2D;

   -----------------
   -- Front_Face --
   -----------------

   procedure Front_Face
     (Context : in out Context_WebGL_Type'Class;
      Mode : Front_Face_Direction)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.frontFace("
         & Front_Face_Direction_Values (Mode)'Image
           & ")");
      else
Context.Execute (
        "frontFace("
         & Front_Face_Direction_Values (Mode)'Image
        & ")");
      end if;
   end Front_Face;

   ------------------
   -- Gen_Buffers --
   ------------------

   procedure Gen_Buffers
     (Context : in out Context_WebGL_Type'Class;
      N : GLsizei;
      Buffers : GLuint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.genBuffers("
         & N'Image & ","
         & Buffers'Image
           & ")");
      else
Context.Execute (
        "genBuffers("
         & N'Image & ","
         & Buffers'Image
        & ")");
      end if;
   end Gen_Buffers;

   -----------------------
   -- Gen_Framebuffers --
   -----------------------

   procedure Gen_Framebuffers
     (Context : in out Context_WebGL_Type'Class;
      N : GLsizei;
      Framebuffers : GLuint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.genFramebuffers("
         & N'Image & ","
         & Framebuffers'Image
           & ")");
      else
Context.Execute (
        "genFramebuffers("
         & N'Image & ","
         & Framebuffers'Image
        & ")");
      end if;
   end Gen_Framebuffers;

   ------------------------
   -- Gen_Renderbuffers --
   ------------------------

   procedure Gen_Renderbuffers
     (Context : in out Context_WebGL_Type'Class;
      N : GLsizei;
      Renderbuffers : GLuint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.genRenderbuffers("
         & N'Image & ","
         & Renderbuffers'Image
           & ")");
      else
Context.Execute (
        "genRenderbuffers("
         & N'Image & ","
         & Renderbuffers'Image
        & ")");
      end if;
   end Gen_Renderbuffers;

   -------------------
   -- Gen_Textures --
   -------------------

   procedure Gen_Textures
     (Context : in out Context_WebGL_Type'Class;
      N : GLsizei;
      Textures : GLuint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.genTextures("
         & N'Image & ","
         & Textures'Image
           & ")");
      else
Context.Execute (
        "genTextures("
         & N'Image & ","
         & Textures'Image
        & ")");
      end if;
   end Gen_Textures;

   ----------------------
   -- Generate_Mipmap --
   ----------------------

   procedure Generate_Mipmap
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.generateMipmap("
         & Texture_Target_Values (Target)'Image
           & ")");
      else
Context.Execute (
        "generateMipmap("
         & Texture_Target_Values (Target)'Image
        & ")");
      end if;
   end Generate_Mipmap;

   -----------------------
   -- Get_Active_Attrib --
   -----------------------

   procedure Get_Active_Attrib
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint;
      Index : GLuint;
      BufSize : GLsizei;
      Length : GLsizei;
      Size : GLint;
      Item_Type : Attribute_Type;
      Name : String)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.getActiveAttrib("
         & Support.Indexed_Object_Reference (Context, Program) & ","
         & Index'Image & ","
         & BufSize'Image & ","
         & Length'Image & ","
         & Size'Image & ","
         & Attribute_Type_Values (Item_Type)'Image & ","
         & "'" & Escape_Quotes (Name) & "'"
           & ")");
      else
Context.Execute (
        "getActiveAttrib("
         & Support.Indexed_Object_Reference (Context, Program) & ","
         & Index'Image & ","
         & BufSize'Image & ","
         & Length'Image & ","
         & Size'Image & ","
         & Attribute_Type_Values (Item_Type)'Image & ","
         & "'" & Escape_Quotes (Name) & "'"
        & ")");
      end if;
   end Get_Active_Attrib;

   ------------------------
   -- Get_Active_Uniform --
   ------------------------

   procedure Get_Active_Uniform
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint;
      Index : GLuint;
      BufSize : GLsizei;
      Length : GLsizei;
      Size : GLint;
      Item_Type : Attribute_Type;
      Name : String)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.getActiveUniform("
         & Support.Indexed_Object_Reference (Context, Program) & ","
         & Index'Image & ","
         & BufSize'Image & ","
         & Length'Image & ","
         & Size'Image & ","
         & Attribute_Type_Values (Item_Type)'Image & ","
         & "'" & Escape_Quotes (Name) & "'"
           & ")");
      else
Context.Execute (
        "getActiveUniform("
         & Support.Indexed_Object_Reference (Context, Program) & ","
         & Index'Image & ","
         & BufSize'Image & ","
         & Length'Image & ","
         & Size'Image & ","
         & Attribute_Type_Values (Item_Type)'Image & ","
         & "'" & Escape_Quotes (Name) & "'"
        & ")");
      end if;
   end Get_Active_Uniform;

   --------------------------
   -- Get_Attached_Shaders --
   --------------------------

   procedure Get_Attached_Shaders
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint;
      MaxCount : GLsizei;
      Count : GLsizei;
      Shaders : GLuint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.getAttachedShaders("
         & Support.Indexed_Object_Reference (Context, Program) & ","
         & MaxCount'Image & ","
         & Count'Image & ","
         & Shaders'Image
           & ")");
      else
Context.Execute (
        "getAttachedShaders("
         & Support.Indexed_Object_Reference (Context, Program) & ","
         & MaxCount'Image & ","
         & Count'Image & ","
         & Shaders'Image
        & ")");
      end if;
   end Get_Attached_Shaders;

   -------------------------
   -- Get_Attrib_Location --
   -------------------------

   function Get_Attrib_Location
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint;
      Name : String)
     return GLuint
   is
   begin
      if Context.Rendering then
         raise Constraint_Error with
            "Illegal call to glGetAttribLocation during render";
      else
         declare
            Result : constant GLuint := 
           GLuint'Value (Context.Execute (
        "getAttribLocation("
         & Support.Indexed_Object_Reference (Context, Program) & ","
         & "'" & Escape_Quotes (Name) & "'"
        & ")"));
      begin
      return Result;
   end;
      end if;
   end Get_Attrib_Location;

   -------------------
   -- Get_Booleanv --
   -------------------

   procedure Get_Booleanv
     (Context : in out Context_WebGL_Type'Class;
      Pname : Get_PName;
      Data : Boolean)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.getBooleanv("
         & Get_PName_Values (Pname)'Image & ","
         & GLEnum_Property (Context, Data'Image)
           & ")");
      else
Context.Execute (
        "getBooleanv("
         & Get_PName_Values (Pname)'Image & ","
         & GLEnum_Property (Context, Data'Image)
        & ")");
      end if;
   end Get_Booleanv;

   ----------------------------
   -- Get_Buffer_Parameteriv --
   ----------------------------

   procedure Get_Buffer_Parameteriv
     (Context : in out Context_WebGL_Type'Class;
      Target : Buffer_Target_ARB;
      Pname : GLenum;
      Params : GLint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.getBufferParameteriv("
         & Buffer_Target_ARB_Values (Target)'Image & ","
         & GLEnum_Property (Context, Pname'Image) & ","
         & Params'Image
           & ")");
      else
Context.Execute (
        "getBufferParameteriv("
         & Buffer_Target_ARB_Values (Target)'Image & ","
         & GLEnum_Property (Context, Pname'Image) & ","
         & Params'Image
        & ")");
      end if;
   end Get_Buffer_Parameteriv;

   -----------------
   -- Get_Floatv --
   -----------------

   procedure Get_Floatv
     (Context : in out Context_WebGL_Type'Class;
      Pname : Get_PName;
      Data : GLfloat)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.getFloatv("
         & Get_PName_Values (Pname)'Image & ","
         & Support.Image (Data)
           & ")");
      else
Context.Execute (
        "getFloatv("
         & Get_PName_Values (Pname)'Image & ","
         & Support.Image (Data)
        & ")");
      end if;
   end Get_Floatv;

   -------------------------------------------
   -- Get_Framebuffer_Attachment_Parameteriv --
   -------------------------------------------

   procedure Get_Framebuffer_Attachment_Parameteriv
     (Context : in out Context_WebGL_Type'Class;
      Target : Framebuffer_Target;
      Attachment : Framebuffer_Attachment;
      Pname : Framebuffer_Attachment_Parameter_Name;
      Params : GLint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.getFramebufferAttachmentParameteriv("
         & Framebuffer_Target_Values (Target)'Image & ","
         & Framebuffer_Attachment_Values (Attachment)'Image & ","
         & Framebuffer_Attachment_Parameter_Name_Values (Pname)'Image & ","
         & Params'Image
           & ")");
      else
Context.Execute (
        "getFramebufferAttachmentParameteriv("
         & Framebuffer_Target_Values (Target)'Image & ","
         & Framebuffer_Attachment_Values (Attachment)'Image & ","
         & Framebuffer_Attachment_Parameter_Name_Values (Pname)'Image & ","
         & Params'Image
        & ")");
      end if;
   end Get_Framebuffer_Attachment_Parameteriv;

   -------------------
   -- Get_Integerv --
   -------------------

   procedure Get_Integerv
     (Context : in out Context_WebGL_Type'Class;
      Pname : Get_PName;
      Data : GLint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.getIntegerv("
         & Get_PName_Values (Pname)'Image & ","
         & Data'Image
           & ")");
      else
Context.Execute (
        "getIntegerv("
         & Get_PName_Values (Pname)'Image & ","
         & Data'Image
        & ")");
      end if;
   end Get_Integerv;

   -------------------------
   -- Get_Program_Info_Log --
   -------------------------

   procedure Get_Program_Info_Log
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint;
      BufSize : GLsizei;
      Length : GLsizei;
      InfoLog : String)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.getProgramInfoLog("
         & Support.Indexed_Object_Reference (Context, Program) & ","
         & BufSize'Image & ","
         & Length'Image & ","
         & "'" & Escape_Quotes (InfoLog) & "'"
           & ")");
      else
Context.Execute (
        "getProgramInfoLog("
         & Support.Indexed_Object_Reference (Context, Program) & ","
         & BufSize'Image & ","
         & Length'Image & ","
         & "'" & Escape_Quotes (InfoLog) & "'"
        & ")");
      end if;
   end Get_Program_Info_Log;

   --------------------
   -- Get_Programiv --
   --------------------

   procedure Get_Programiv
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint;
      Pname : Program_Property_ARB;
      Params : GLint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.getProgramiv("
         & Support.Indexed_Object_Reference (Context, Program) & ","
         & Program_Property_ARB_Values (Pname)'Image & ","
         & Params'Image
           & ")");
      else
Context.Execute (
        "getProgramiv("
         & Support.Indexed_Object_Reference (Context, Program) & ","
         & Program_Property_ARB_Values (Pname)'Image & ","
         & Params'Image
        & ")");
      end if;
   end Get_Programiv;

   ----------------------------------
   -- Get_Renderbuffer_Parameteriv --
   ----------------------------------

   procedure Get_Renderbuffer_Parameteriv
     (Context : in out Context_WebGL_Type'Class;
      Target : Renderbuffer_Target;
      Pname : Renderbuffer_Parameter_Name;
      Params : GLint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.getRenderbufferParameteriv("
         & Renderbuffer_Target_Values (Target)'Image & ","
         & Renderbuffer_Parameter_Name_Values (Pname)'Image & ","
         & Params'Image
           & ")");
      else
Context.Execute (
        "getRenderbufferParameteriv("
         & Renderbuffer_Target_Values (Target)'Image & ","
         & Renderbuffer_Parameter_Name_Values (Pname)'Image & ","
         & Params'Image
        & ")");
      end if;
   end Get_Renderbuffer_Parameteriv;

   ------------------------
   -- Get_Shader_Info_Log --
   ------------------------

   procedure Get_Shader_Info_Log
     (Context : in out Context_WebGL_Type'Class;
      Shader : GLuint;
      BufSize : GLsizei;
      Length : GLsizei;
      InfoLog : String)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.getShaderInfoLog("
         & Support.Indexed_Object_Reference (Context, Shader) & ","
         & BufSize'Image & ","
         & Length'Image & ","
         & "'" & Escape_Quotes (InfoLog) & "'"
           & ")");
      else
Context.Execute (
        "getShaderInfoLog("
         & Support.Indexed_Object_Reference (Context, Shader) & ","
         & BufSize'Image & ","
         & Length'Image & ","
         & "'" & Escape_Quotes (InfoLog) & "'"
        & ")");
      end if;
   end Get_Shader_Info_Log;

   --------------------------------
   -- Get_Shader_Precision_Format --
   --------------------------------

   procedure Get_Shader_Precision_Format
     (Context : in out Context_WebGL_Type'Class;
      Shadertype : Shader_Type;
      Precisiontype : Precision_Type;
      Item_Range : GLint;
      Precision : GLint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.getShaderPrecisionFormat("
         & Shader_Type_Values (Shadertype)'Image & ","
         & Precision_Type_Values (Precisiontype)'Image & ","
         & Item_Range'Image & ","
         & Precision'Image
           & ")");
      else
Context.Execute (
        "getShaderPrecisionFormat("
         & Shader_Type_Values (Shadertype)'Image & ","
         & Precision_Type_Values (Precisiontype)'Image & ","
         & Item_Range'Image & ","
         & Precision'Image
        & ")");
      end if;
   end Get_Shader_Precision_Format;

   -----------------------
   -- Get_Shader_Source --
   -----------------------

   procedure Get_Shader_Source
     (Context : in out Context_WebGL_Type'Class;
      Shader : GLuint;
      BufSize : GLsizei;
      Length : GLsizei;
      Source : String)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.getShaderSource("
         & Support.Indexed_Object_Reference (Context, Shader) & ","
         & BufSize'Image & ","
         & Length'Image & ","
         & "'" & Escape_Quotes (Source) & "'"
           & ")");
      else
Context.Execute (
        "getShaderSource("
         & Support.Indexed_Object_Reference (Context, Shader) & ","
         & BufSize'Image & ","
         & Length'Image & ","
         & "'" & Escape_Quotes (Source) & "'"
        & ")");
      end if;
   end Get_Shader_Source;

   -------------------
   -- Get_Shaderiv --
   -------------------

   procedure Get_Shaderiv
     (Context : in out Context_WebGL_Type'Class;
      Shader : GLuint;
      Pname : Shader_Parameter_Name;
      Params : GLint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.getShaderiv("
         & Support.Indexed_Object_Reference (Context, Shader) & ","
         & Shader_Parameter_Name_Values (Pname)'Image & ","
         & Params'Image
           & ")");
      else
Context.Execute (
        "getShaderiv("
         & Support.Indexed_Object_Reference (Context, Shader) & ","
         & Shader_Parameter_Name_Values (Pname)'Image & ","
         & Params'Image
        & ")");
      end if;
   end Get_Shaderiv;

   -------------------------
   -- Get_Tex_Parameterfv --
   -------------------------

   procedure Get_Tex_Parameterfv
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Pname : Get_Texture_Parameter;
      Params : GLfloat)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.getTexParameterfv("
         & Texture_Target_Values (Target)'Image & ","
         & Get_Texture_Parameter_Values (Pname)'Image & ","
         & Support.Image (Params)
           & ")");
      else
Context.Execute (
        "getTexParameterfv("
         & Texture_Target_Values (Target)'Image & ","
         & Get_Texture_Parameter_Values (Pname)'Image & ","
         & Support.Image (Params)
        & ")");
      end if;
   end Get_Tex_Parameterfv;

   -------------------------
   -- Get_Tex_Parameteriv --
   -------------------------

   procedure Get_Tex_Parameteriv
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Pname : Get_Texture_Parameter;
      Params : GLint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.getTexParameteriv("
         & Texture_Target_Values (Target)'Image & ","
         & Get_Texture_Parameter_Values (Pname)'Image & ","
         & Params'Image
           & ")");
      else
Context.Execute (
        "getTexParameteriv("
         & Texture_Target_Values (Target)'Image & ","
         & Get_Texture_Parameter_Values (Pname)'Image & ","
         & Params'Image
        & ")");
      end if;
   end Get_Tex_Parameteriv;

   --------------------------
   -- Get_Uniform_Location --
   --------------------------

   function Get_Uniform_Location
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint;
      Name : String)
     return GLuint
   is
   begin
      if Context.Rendering then
         raise Constraint_Error with
            "Illegal call to glGetUniformLocation during render";
      else
         declare
            Result : constant GLuint := 
           Support.Indexed_Javascript_Object (Context,
        "getUniformLocation("
         & Support.Indexed_Object_Reference (Context, Program) & ","
         & "'" & Escape_Quotes (Name) & "'"
        & ")");
      begin
      return Result;
   end;
      end if;
   end Get_Uniform_Location;

   --------------------
   -- Get_Uniformfv --
   --------------------

   procedure Get_Uniformfv
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint;
      Location : GLint;
      Params : GLfloat)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.getUniformfv("
         & Support.Indexed_Object_Reference (Context, Program) & ","
         & Location'Image & ","
         & Support.Image (Params)
           & ")");
      else
Context.Execute (
        "getUniformfv("
         & Support.Indexed_Object_Reference (Context, Program) & ","
         & Location'Image & ","
         & Support.Image (Params)
        & ")");
      end if;
   end Get_Uniformfv;

   --------------------
   -- Get_Uniformiv --
   --------------------

   procedure Get_Uniformiv
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint;
      Location : GLint;
      Params : GLint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.getUniformiv("
         & Support.Indexed_Object_Reference (Context, Program) & ","
         & Location'Image & ","
         & Params'Image
           & ")");
      else
Context.Execute (
        "getUniformiv("
         & Support.Indexed_Object_Reference (Context, Program) & ","
         & Location'Image & ","
         & Params'Image
        & ")");
      end if;
   end Get_Uniformiv;

   -------------------------
   -- Get_Vertex_Attribfv --
   -------------------------

   procedure Get_Vertex_Attribfv
     (Context : in out Context_WebGL_Type'Class;
      Index : GLuint;
      Pname : GLenum;
      Params : GLfloat)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.getVertexAttribfv("
         & Index'Image & ","
         & GLEnum_Property (Context, Pname'Image) & ","
         & Support.Image (Params)
           & ")");
      else
Context.Execute (
        "getVertexAttribfv("
         & Index'Image & ","
         & GLEnum_Property (Context, Pname'Image) & ","
         & Support.Image (Params)
        & ")");
      end if;
   end Get_Vertex_Attribfv;

   -------------------------
   -- Get_Vertex_Attribiv --
   -------------------------

   procedure Get_Vertex_Attribiv
     (Context : in out Context_WebGL_Type'Class;
      Index : GLuint;
      Pname : GLenum;
      Params : GLint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.getVertexAttribiv("
         & Index'Image & ","
         & GLEnum_Property (Context, Pname'Image) & ","
         & Params'Image
           & ")");
      else
Context.Execute (
        "getVertexAttribiv("
         & Index'Image & ","
         & GLEnum_Property (Context, Pname'Image) & ","
         & Params'Image
        & ")");
      end if;
   end Get_Vertex_Attribiv;

   ------------
   -- Hint --
   ------------

   procedure Hint
     (Context : in out Context_WebGL_Type'Class;
      Target : Hint_Target;
      Mode : Hint_Mode)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.hint("
         & Hint_Target_Values (Target)'Image & ","
         & Hint_Mode_Values (Mode)'Image
           & ")");
      else
Context.Execute (
        "hint("
         & Hint_Target_Values (Target)'Image & ","
         & Hint_Mode_Values (Mode)'Image
        & ")");
      end if;
   end Hint;

   ----------------
   -- Is_Buffer --
   ----------------

   function Is_Buffer
     (Context : in out Context_WebGL_Type'Class;
      Buffer : GLuint)
     return Boolean
   is
   begin
      if Context.Rendering then
         raise Constraint_Error with
            "Illegal call to glIsBuffer during render";
      else
         declare
            Result : constant Boolean := 
           Boolean'Value (Context.Execute (
        "isBuffer("
         & Buffer'Image
        & ")"));
      begin
      return Result;
   end;
      end if;
   end Is_Buffer;

   -----------------
   -- Is_Enabled --
   -----------------

   function Is_Enabled
     (Context : in out Context_WebGL_Type'Class;
      Cap : Enable_Cap)
     return Boolean
   is
   begin
      if Context.Rendering then
         raise Constraint_Error with
            "Illegal call to glIsEnabled during render";
      else
         declare
            Result : constant Boolean := 
           Boolean'Value (Context.Execute (
        "isEnabled("
         & Enable_Cap_Values (Cap)'Image
        & ")"));
      begin
      return Result;
   end;
      end if;
   end Is_Enabled;

   ---------------------
   -- Is_Framebuffer --
   ---------------------

   function Is_Framebuffer
     (Context : in out Context_WebGL_Type'Class;
      Framebuffer : GLuint)
     return Boolean
   is
   begin
      if Context.Rendering then
         raise Constraint_Error with
            "Illegal call to glIsFramebuffer during render";
      else
         declare
            Result : constant Boolean := 
           Boolean'Value (Context.Execute (
        "isFramebuffer("
         & Framebuffer'Image
        & ")"));
      begin
      return Result;
   end;
      end if;
   end Is_Framebuffer;

   -----------------
   -- Is_Program --
   -----------------

   function Is_Program
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint)
     return Boolean
   is
   begin
      if Context.Rendering then
         raise Constraint_Error with
            "Illegal call to glIsProgram during render";
      else
         declare
            Result : constant Boolean := 
           Boolean'Value (Context.Execute (
        "isProgram("
         & Support.Indexed_Object_Reference (Context, Program)
        & ")"));
      begin
      return Result;
   end;
      end if;
   end Is_Program;

   ----------------------
   -- Is_Renderbuffer --
   ----------------------

   function Is_Renderbuffer
     (Context : in out Context_WebGL_Type'Class;
      Renderbuffer : GLuint)
     return Boolean
   is
   begin
      if Context.Rendering then
         raise Constraint_Error with
            "Illegal call to glIsRenderbuffer during render";
      else
         declare
            Result : constant Boolean := 
           Boolean'Value (Context.Execute (
        "isRenderbuffer("
         & Renderbuffer'Image
        & ")"));
      begin
      return Result;
   end;
      end if;
   end Is_Renderbuffer;

   ----------------
   -- Is_Shader --
   ----------------

   function Is_Shader
     (Context : in out Context_WebGL_Type'Class;
      Shader : GLuint)
     return Boolean
   is
   begin
      if Context.Rendering then
         raise Constraint_Error with
            "Illegal call to glIsShader during render";
      else
         declare
            Result : constant Boolean := 
           Boolean'Value (Context.Execute (
        "isShader("
         & Support.Indexed_Object_Reference (Context, Shader)
        & ")"));
      begin
      return Result;
   end;
      end if;
   end Is_Shader;

   -----------------
   -- Is_Texture --
   -----------------

   function Is_Texture
     (Context : in out Context_WebGL_Type'Class;
      Texture : GLuint)
     return Boolean
   is
   begin
      if Context.Rendering then
         raise Constraint_Error with
            "Illegal call to glIsTexture during render";
      else
         declare
            Result : constant Boolean := 
           Boolean'Value (Context.Execute (
        "isTexture("
         & Texture'Image
        & ")"));
      begin
      return Result;
   end;
      end if;
   end Is_Texture;

   -----------------
   -- Line_Width --
   -----------------

   procedure Line_Width
     (Context : in out Context_WebGL_Type'Class;
      Width : GLfloat)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.lineWidth("
         & Support.Image (Width)
           & ")");
      else
Context.Execute (
        "lineWidth("
         & Support.Image (Width)
        & ")");
      end if;
   end Line_Width;

   -------------------
   -- Link_Program --
   -------------------

   procedure Link_Program
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.linkProgram("
         & Support.Indexed_Object_Reference (Context, Program)
           & ")");
      else
Context.Execute (
        "linkProgram("
         & Support.Indexed_Object_Reference (Context, Program)
        & ")");
      end if;
   end Link_Program;

   -------------------
   -- Pixel_Storei --
   -------------------

   procedure Pixel_Storei
     (Context : in out Context_WebGL_Type'Class;
      Pname : Pixel_Store_Parameter;
      Param : GLint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.pixelStorei("
         & Pixel_Store_Parameter_Values (Pname)'Image & ","
         & Param'Image
           & ")");
      else
Context.Execute (
        "pixelStorei("
         & Pixel_Store_Parameter_Values (Pname)'Image & ","
         & Param'Image
        & ")");
      end if;
   end Pixel_Storei;

   ---------------------
   -- Polygon_Offset --
   ---------------------

   procedure Polygon_Offset
     (Context : in out Context_WebGL_Type'Class;
      Factor : GLfloat;
      Units : GLfloat)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.polygonOffset("
         & Support.Image (Factor) & ","
         & Support.Image (Units)
           & ")");
      else
Context.Execute (
        "polygonOffset("
         & Support.Image (Factor) & ","
         & Support.Image (Units)
        & ")");
      end if;
   end Polygon_Offset;

   -----------------------------
   -- Release_Shader_Compiler --
   -----------------------------

   procedure Release_Shader_Compiler
     (Context : in out Context_WebGL_Type'Class)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.releaseShaderCompiler("

           & ")");
      else
Context.Execute (
        "releaseShaderCompiler("

        & ")");
      end if;
   end Release_Shader_Compiler;

   ---------------------------
   -- Renderbuffer_Storage --
   ---------------------------

   procedure Renderbuffer_Storage
     (Context : in out Context_WebGL_Type'Class;
      Target : Renderbuffer_Target;
      Internalformat : Internal_Format;
      Width : GLsizei;
      Height : GLsizei)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.renderbufferStorage("
         & Renderbuffer_Target_Values (Target)'Image & ","
         & Internal_Format_Values (Internalformat)'Image & ","
         & Width'Image & ","
         & Height'Image
           & ")");
      else
Context.Execute (
        "renderbufferStorage("
         & Renderbuffer_Target_Values (Target)'Image & ","
         & Internal_Format_Values (Internalformat)'Image & ","
         & Width'Image & ","
         & Height'Image
        & ")");
      end if;
   end Renderbuffer_Storage;

   ----------------------
   -- Sample_Coverage --
   ----------------------

   procedure Sample_Coverage
     (Context : in out Context_WebGL_Type'Class;
      Value : GLfloat;
      Invert : Boolean)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.sampleCoverage("
         & Support.Image (Value) & ","
         & GLEnum_Property (Context, Invert'Image)
           & ")");
      else
Context.Execute (
        "sampleCoverage("
         & Support.Image (Value) & ","
         & GLEnum_Property (Context, Invert'Image)
        & ")");
      end if;
   end Sample_Coverage;

   ---------------
   -- Scissor --
   ---------------

   procedure Scissor
     (Context : in out Context_WebGL_Type'Class;
      X : GLint;
      Y : GLint;
      Width : GLsizei;
      Height : GLsizei)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.scissor("
         & X'Image & ","
         & Y'Image & ","
         & Width'Image & ","
         & Height'Image
           & ")");
      else
Context.Execute (
        "scissor("
         & X'Image & ","
         & Y'Image & ","
         & Width'Image & ","
         & Height'Image
        & ")");
      end if;
   end Scissor;

   --------------------
   -- Shader_Binary --
   --------------------

   procedure Shader_Binary
     (Context : in out Context_WebGL_Type'Class;
      Count : GLsizei;
      Shaders : GLuint;
      Binaryformat : GLenum;
      Binary : Float_Array;
      Length : GLsizei)
   is
      use Ada.Strings.Unbounded;
      Data_Image : Unbounded_String;
   begin
      for X of binary loop
         if Data_Image /= "" then
            Data_Image := Data_Image & ",";
         end if;
         Data_Image := Data_Image & X'Image;
      end loop;
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.shaderBinary("
         & Count'Image & ","
         & Shaders'Image & ","
         & Binaryformat'Image & ","
         & "new Float32Array([" & To_String (Data_Image) & "])" & ","
         & Length'Image
           & ")");
      else
Context.Execute (
        "shaderBinary("
         & Count'Image & ","
         & Shaders'Image & ","
         & Binaryformat'Image & ","
         & "new Float32Array([" & To_String (Data_Image) & "])" & ","
         & Length'Image
        & ")");
      end if;
   end Shader_Binary;

   --------------------
   -- Shader_Binary --
   --------------------

   procedure Shader_Binary
     (Context : in out Context_WebGL_Type'Class;
      Count : GLsizei;
      Shaders : GLuint;
      Binaryformat : GLenum;
      Binary : Uint16_Array;
      Length : GLsizei)
   is
      use Ada.Strings.Unbounded;
      Data_Image : Unbounded_String;
   begin
      for X of binary loop
         if Data_Image /= "" then
            Data_Image := Data_Image & ",";
         end if;
         Data_Image := Data_Image & X'Image;
      end loop;
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.shaderBinary("
         & Count'Image & ","
         & Shaders'Image & ","
         & Binaryformat'Image & ","
         & "new Uint16Array([" & To_String (Data_Image) & "])" & ","
         & Length'Image
           & ")");
      else
Context.Execute (
        "shaderBinary("
         & Count'Image & ","
         & Shaders'Image & ","
         & Binaryformat'Image & ","
         & "new Uint16Array([" & To_String (Data_Image) & "])" & ","
         & Length'Image
        & ")");
      end if;
   end Shader_Binary;

   --------------------
   -- Shader_Binary --
   --------------------

   procedure Shader_Binary
     (Context : in out Context_WebGL_Type'Class;
      Count : GLsizei;
      Shaders : GLuint;
      Binaryformat : GLenum;
      Binary : Uint8_Array;
      Length : GLsizei)
   is
      use Ada.Strings.Unbounded;
      Data_Image : Unbounded_String;
   begin
      for X of binary loop
         if Data_Image /= "" then
            Data_Image := Data_Image & ",";
         end if;
         Data_Image := Data_Image & X'Image;
      end loop;
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.shaderBinary("
         & Count'Image & ","
         & Shaders'Image & ","
         & Binaryformat'Image & ","
         & "new Uint8Array([" & To_String (Data_Image) & "])" & ","
         & Length'Image
           & ")");
      else
Context.Execute (
        "shaderBinary("
         & Count'Image & ","
         & Shaders'Image & ","
         & Binaryformat'Image & ","
         & "new Uint8Array([" & To_String (Data_Image) & "])" & ","
         & Length'Image
        & ")");
      end if;
   end Shader_Binary;

   --------------------
   -- Shader_Source --
   --------------------

   procedure Shader_Source
     (Context : in out Context_WebGL_Type'Class;
      Shader : GLuint;
      Source : String)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.shaderSource("
         & Support.Indexed_Object_Reference (Context, Shader) & ","
         & "'" & Escape_Quotes (Source) & "'"
           & ")");
      else
Context.Execute (
        "shaderSource("
         & Support.Indexed_Object_Reference (Context, Shader) & ","
         & "'" & Escape_Quotes (Source) & "'"
        & ")");
      end if;
   end Shader_Source;

   -------------------
   -- Stencil_Func --
   -------------------

   procedure Stencil_Func
     (Context : in out Context_WebGL_Type'Class;
      Func : Stencil_Function;
      Ref : GLint;
      Mask : GLuint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.stencilFunc("
         & Stencil_Function_Values (Func)'Image & ","
         & Ref'Image & ","
         & Mask'Image
           & ")");
      else
Context.Execute (
        "stencilFunc("
         & Stencil_Function_Values (Func)'Image & ","
         & Ref'Image & ","
         & Mask'Image
        & ")");
      end if;
   end Stencil_Func;

   ---------------------------
   -- Stencil_Func_Separate --
   ---------------------------

   procedure Stencil_Func_Separate
     (Context : in out Context_WebGL_Type'Class;
      Face : Stencil_Face_Direction;
      Func : Stencil_Function;
      Ref : GLint;
      Mask : GLuint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.stencilFuncSeparate("
         & Stencil_Face_Direction_Values (Face)'Image & ","
         & Stencil_Function_Values (Func)'Image & ","
         & Ref'Image & ","
         & Mask'Image
           & ")");
      else
Context.Execute (
        "stencilFuncSeparate("
         & Stencil_Face_Direction_Values (Face)'Image & ","
         & Stencil_Function_Values (Func)'Image & ","
         & Ref'Image & ","
         & Mask'Image
        & ")");
      end if;
   end Stencil_Func_Separate;

   -------------------
   -- Stencil_Mask --
   -------------------

   procedure Stencil_Mask
     (Context : in out Context_WebGL_Type'Class;
      Mask : GLuint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.stencilMask("
         & Mask'Image
           & ")");
      else
Context.Execute (
        "stencilMask("
         & Mask'Image
        & ")");
      end if;
   end Stencil_Mask;

   ---------------------------
   -- Stencil_Mask_Separate --
   ---------------------------

   procedure Stencil_Mask_Separate
     (Context : in out Context_WebGL_Type'Class;
      Face : Stencil_Face_Direction;
      Mask : GLuint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.stencilMaskSeparate("
         & Stencil_Face_Direction_Values (Face)'Image & ","
         & Mask'Image
           & ")");
      else
Context.Execute (
        "stencilMaskSeparate("
         & Stencil_Face_Direction_Values (Face)'Image & ","
         & Mask'Image
        & ")");
      end if;
   end Stencil_Mask_Separate;

   -----------------
   -- Set_Stencil_Op --
   -----------------

   procedure Set_Stencil_Op
     (Context : in out Context_WebGL_Type'Class;
      Fail : Stencil_Op;
      Zfail : Stencil_Op;
      Zpass : Stencil_Op)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.stencilOp("
         & Stencil_Op_Values (Fail)'Image & ","
         & Stencil_Op_Values (Zfail)'Image & ","
         & Stencil_Op_Values (Zpass)'Image
           & ")");
      else
Context.Execute (
        "stencilOp("
         & Stencil_Op_Values (Fail)'Image & ","
         & Stencil_Op_Values (Zfail)'Image & ","
         & Stencil_Op_Values (Zpass)'Image
        & ")");
      end if;
   end Set_Stencil_Op;

   -------------------------
   -- Stencil_Op_Separate --
   -------------------------

   procedure Stencil_Op_Separate
     (Context : in out Context_WebGL_Type'Class;
      Face : Stencil_Face_Direction;
      Sfail : Stencil_Op;
      Dpfail : Stencil_Op;
      Dppass : Stencil_Op)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.stencilOpSeparate("
         & Stencil_Face_Direction_Values (Face)'Image & ","
         & Stencil_Op_Values (Sfail)'Image & ","
         & Stencil_Op_Values (Dpfail)'Image & ","
         & Stencil_Op_Values (Dppass)'Image
           & ")");
      else
Context.Execute (
        "stencilOpSeparate("
         & Stencil_Face_Direction_Values (Face)'Image & ","
         & Stencil_Op_Values (Sfail)'Image & ","
         & Stencil_Op_Values (Dpfail)'Image & ","
         & Stencil_Op_Values (Dppass)'Image
        & ")");
      end if;
   end Stencil_Op_Separate;

   ------------------
   -- Tex_Image_2D --
   ------------------

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
      Pixels : Float_Array)
   is
      use Ada.Strings.Unbounded;
      Data_Image : Unbounded_String;
   begin
      for X of pixels loop
         if Data_Image /= "" then
            Data_Image := Data_Image & ",";
         end if;
         Data_Image := Data_Image & X'Image;
      end loop;
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.texImage2D("
         & Texture_Target_Values (Target)'Image & ","
         & Level'Image & ","
         & Internal_Format_Values (Internalformat)'Image & ","
         & Width'Image & ","
         & Height'Image & ","
         & Border'Image & ","
         & Pixel_Format_Values (Format)'Image & ","
         & Pixel_Type_Values (Item_Type)'Image & ","
         & "new Float32Array([" & To_String (Data_Image) & "])"
           & ")");
      else
Context.Execute (
        "texImage2D("
         & Texture_Target_Values (Target)'Image & ","
         & Level'Image & ","
         & Internal_Format_Values (Internalformat)'Image & ","
         & Width'Image & ","
         & Height'Image & ","
         & Border'Image & ","
         & Pixel_Format_Values (Format)'Image & ","
         & Pixel_Type_Values (Item_Type)'Image & ","
         & "new Float32Array([" & To_String (Data_Image) & "])"
        & ")");
      end if;
   end Tex_Image_2D;

   ------------------
   -- Tex_Image_2D --
   ------------------

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
      Pixels : Uint16_Array)
   is
      use Ada.Strings.Unbounded;
      Data_Image : Unbounded_String;
   begin
      for X of pixels loop
         if Data_Image /= "" then
            Data_Image := Data_Image & ",";
         end if;
         Data_Image := Data_Image & X'Image;
      end loop;
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.texImage2D("
         & Texture_Target_Values (Target)'Image & ","
         & Level'Image & ","
         & Internal_Format_Values (Internalformat)'Image & ","
         & Width'Image & ","
         & Height'Image & ","
         & Border'Image & ","
         & Pixel_Format_Values (Format)'Image & ","
         & Pixel_Type_Values (Item_Type)'Image & ","
         & "new Uint16Array([" & To_String (Data_Image) & "])"
           & ")");
      else
Context.Execute (
        "texImage2D("
         & Texture_Target_Values (Target)'Image & ","
         & Level'Image & ","
         & Internal_Format_Values (Internalformat)'Image & ","
         & Width'Image & ","
         & Height'Image & ","
         & Border'Image & ","
         & Pixel_Format_Values (Format)'Image & ","
         & Pixel_Type_Values (Item_Type)'Image & ","
         & "new Uint16Array([" & To_String (Data_Image) & "])"
        & ")");
      end if;
   end Tex_Image_2D;

   ------------------
   -- Tex_Image_2D --
   ------------------

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
      Pixels : Uint8_Array)
   is
      use Ada.Strings.Unbounded;
      Data_Image : Unbounded_String;
   begin
      for X of pixels loop
         if Data_Image /= "" then
            Data_Image := Data_Image & ",";
         end if;
         Data_Image := Data_Image & X'Image;
      end loop;
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.texImage2D("
         & Texture_Target_Values (Target)'Image & ","
         & Level'Image & ","
         & Internal_Format_Values (Internalformat)'Image & ","
         & Width'Image & ","
         & Height'Image & ","
         & Border'Image & ","
         & Pixel_Format_Values (Format)'Image & ","
         & Pixel_Type_Values (Item_Type)'Image & ","
         & "new Uint8Array([" & To_String (Data_Image) & "])"
           & ")");
      else
Context.Execute (
        "texImage2D("
         & Texture_Target_Values (Target)'Image & ","
         & Level'Image & ","
         & Internal_Format_Values (Internalformat)'Image & ","
         & Width'Image & ","
         & Height'Image & ","
         & Border'Image & ","
         & Pixel_Format_Values (Format)'Image & ","
         & Pixel_Type_Values (Item_Type)'Image & ","
         & "new Uint8Array([" & To_String (Data_Image) & "])"
        & ")");
      end if;
   end Tex_Image_2D;

   ---------------------
   -- Tex_Parameter --
   ---------------------

   procedure Tex_Parameter
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Pname : Texture_Parameter_Name;
      Param : GLfloat)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.texParameter("
         & Texture_Target_Values (Target)'Image & ","
         & Texture_Parameter_Name_Values (Pname)'Image & ","
         & Support.Image (Param)
           & ")");
      else
Context.Execute (
        "texParameter("
         & Texture_Target_Values (Target)'Image & ","
         & Texture_Parameter_Name_Values (Pname)'Image & ","
         & Support.Image (Param)
        & ")");
      end if;
   end Tex_Parameter;

   ----------------------
   -- Tex_Parameterfv --
   ----------------------

   procedure Tex_Parameterfv
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Pname : Texture_Parameter_Name;
      Params : GLfloat)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.texParameterfv("
         & Texture_Target_Values (Target)'Image & ","
         & Texture_Parameter_Name_Values (Pname)'Image & ","
         & Support.Image (Params)
           & ")");
      else
Context.Execute (
        "texParameterfv("
         & Texture_Target_Values (Target)'Image & ","
         & Texture_Parameter_Name_Values (Pname)'Image & ","
         & Support.Image (Params)
        & ")");
      end if;
   end Tex_Parameterfv;

   ---------------------
   -- Tex_Parameteri --
   ---------------------

   procedure Tex_Parameteri
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Pname : Texture_Parameter_Name;
      Param : GLint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.texParameteri("
         & Texture_Target_Values (Target)'Image & ","
         & Texture_Parameter_Name_Values (Pname)'Image & ","
         & Param'Image
           & ")");
      else
Context.Execute (
        "texParameteri("
         & Texture_Target_Values (Target)'Image & ","
         & Texture_Parameter_Name_Values (Pname)'Image & ","
         & Param'Image
        & ")");
      end if;
   end Tex_Parameteri;

   ----------------------
   -- Tex_Parameteriv --
   ----------------------

   procedure Tex_Parameteriv
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Pname : Texture_Parameter_Name;
      Params : GLint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.texParameteriv("
         & Texture_Target_Values (Target)'Image & ","
         & Texture_Parameter_Name_Values (Pname)'Image & ","
         & Params'Image
           & ")");
      else
Context.Execute (
        "texParameteriv("
         & Texture_Target_Values (Target)'Image & ","
         & Texture_Parameter_Name_Values (Pname)'Image & ","
         & Params'Image
        & ")");
      end if;
   end Tex_Parameteriv;

   ---------------------
   -- Tex_Sub_Image_2D --
   ---------------------

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
      Pixels : Float_Array)
   is
      use Ada.Strings.Unbounded;
      Data_Image : Unbounded_String;
   begin
      for X of pixels loop
         if Data_Image /= "" then
            Data_Image := Data_Image & ",";
         end if;
         Data_Image := Data_Image & X'Image;
      end loop;
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.texSubImage2D("
         & Texture_Target_Values (Target)'Image & ","
         & Level'Image & ","
         & Xoffset'Image & ","
         & Yoffset'Image & ","
         & Width'Image & ","
         & Height'Image & ","
         & Pixel_Format_Values (Format)'Image & ","
         & Pixel_Type_Values (Item_Type)'Image & ","
         & "new Float32Array([" & To_String (Data_Image) & "])"
           & ")");
      else
Context.Execute (
        "texSubImage2D("
         & Texture_Target_Values (Target)'Image & ","
         & Level'Image & ","
         & Xoffset'Image & ","
         & Yoffset'Image & ","
         & Width'Image & ","
         & Height'Image & ","
         & Pixel_Format_Values (Format)'Image & ","
         & Pixel_Type_Values (Item_Type)'Image & ","
         & "new Float32Array([" & To_String (Data_Image) & "])"
        & ")");
      end if;
   end Tex_Sub_Image_2D;

   ---------------------
   -- Tex_Sub_Image_2D --
   ---------------------

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
      Pixels : Uint16_Array)
   is
      use Ada.Strings.Unbounded;
      Data_Image : Unbounded_String;
   begin
      for X of pixels loop
         if Data_Image /= "" then
            Data_Image := Data_Image & ",";
         end if;
         Data_Image := Data_Image & X'Image;
      end loop;
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.texSubImage2D("
         & Texture_Target_Values (Target)'Image & ","
         & Level'Image & ","
         & Xoffset'Image & ","
         & Yoffset'Image & ","
         & Width'Image & ","
         & Height'Image & ","
         & Pixel_Format_Values (Format)'Image & ","
         & Pixel_Type_Values (Item_Type)'Image & ","
         & "new Uint16Array([" & To_String (Data_Image) & "])"
           & ")");
      else
Context.Execute (
        "texSubImage2D("
         & Texture_Target_Values (Target)'Image & ","
         & Level'Image & ","
         & Xoffset'Image & ","
         & Yoffset'Image & ","
         & Width'Image & ","
         & Height'Image & ","
         & Pixel_Format_Values (Format)'Image & ","
         & Pixel_Type_Values (Item_Type)'Image & ","
         & "new Uint16Array([" & To_String (Data_Image) & "])"
        & ")");
      end if;
   end Tex_Sub_Image_2D;

   ---------------------
   -- Tex_Sub_Image_2D --
   ---------------------

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
      Pixels : Uint8_Array)
   is
      use Ada.Strings.Unbounded;
      Data_Image : Unbounded_String;
   begin
      for X of pixels loop
         if Data_Image /= "" then
            Data_Image := Data_Image & ",";
         end if;
         Data_Image := Data_Image & X'Image;
      end loop;
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.texSubImage2D("
         & Texture_Target_Values (Target)'Image & ","
         & Level'Image & ","
         & Xoffset'Image & ","
         & Yoffset'Image & ","
         & Width'Image & ","
         & Height'Image & ","
         & Pixel_Format_Values (Format)'Image & ","
         & Pixel_Type_Values (Item_Type)'Image & ","
         & "new Uint8Array([" & To_String (Data_Image) & "])"
           & ")");
      else
Context.Execute (
        "texSubImage2D("
         & Texture_Target_Values (Target)'Image & ","
         & Level'Image & ","
         & Xoffset'Image & ","
         & Yoffset'Image & ","
         & Width'Image & ","
         & Height'Image & ","
         & Pixel_Format_Values (Format)'Image & ","
         & Pixel_Type_Values (Item_Type)'Image & ","
         & "new Uint8Array([" & To_String (Data_Image) & "])"
        & ")");
      end if;
   end Tex_Sub_Image_2D;

   -----------------
   -- Uniform_1 --
   -----------------

   procedure Uniform_1
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      V0 : GLfloat)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.uniform1("
         & Location'Image & ","
         & Support.Image (V0)
           & ")");
      else
Context.Execute (
        "uniform1("
         & Location'Image & ","
         & Support.Image (V0)
        & ")");
      end if;
   end Uniform_1;

   ------------------
   -- Uniform_1fv --
   ------------------

   procedure Uniform_1fv
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      Count : GLsizei;
      Value : GLfloat)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.uniform1fv("
         & Location'Image & ","
         & Count'Image & ","
         & Support.Image (Value)
           & ")");
      else
Context.Execute (
        "uniform1fv("
         & Location'Image & ","
         & Count'Image & ","
         & Support.Image (Value)
        & ")");
      end if;
   end Uniform_1fv;

   -----------------
   -- Uniform_1i --
   -----------------

   procedure Uniform_1i
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      V0 : GLint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.uniform1i("
         & Location'Image & ","
         & V0'Image
           & ")");
      else
Context.Execute (
        "uniform1i("
         & Location'Image & ","
         & V0'Image
        & ")");
      end if;
   end Uniform_1i;

   ------------------
   -- Uniform_1iv --
   ------------------

   procedure Uniform_1iv
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      Count : GLsizei;
      Value : GLint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.uniform1iv("
         & Location'Image & ","
         & Count'Image & ","
         & Value'Image
           & ")");
      else
Context.Execute (
        "uniform1iv("
         & Location'Image & ","
         & Count'Image & ","
         & Value'Image
        & ")");
      end if;
   end Uniform_1iv;

   -----------------
   -- Uniform_2 --
   -----------------

   procedure Uniform_2
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      V0 : GLfloat;
      V1 : GLfloat)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.uniform2("
         & Location'Image & ","
         & Support.Image (V0) & ","
         & Support.Image (V1)
           & ")");
      else
Context.Execute (
        "uniform2("
         & Location'Image & ","
         & Support.Image (V0) & ","
         & Support.Image (V1)
        & ")");
      end if;
   end Uniform_2;

   ------------------
   -- Uniform_2fv --
   ------------------

   procedure Uniform_2fv
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      Count : GLsizei;
      Value : GLfloat)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.uniform2fv("
         & Location'Image & ","
         & Count'Image & ","
         & Support.Image (Value)
           & ")");
      else
Context.Execute (
        "uniform2fv("
         & Location'Image & ","
         & Count'Image & ","
         & Support.Image (Value)
        & ")");
      end if;
   end Uniform_2fv;

   -----------------
   -- Uniform_2i --
   -----------------

   procedure Uniform_2i
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      V0 : GLint;
      V1 : GLint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.uniform2i("
         & Location'Image & ","
         & V0'Image & ","
         & V1'Image
           & ")");
      else
Context.Execute (
        "uniform2i("
         & Location'Image & ","
         & V0'Image & ","
         & V1'Image
        & ")");
      end if;
   end Uniform_2i;

   ------------------
   -- Uniform_2iv --
   ------------------

   procedure Uniform_2iv
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      Count : GLsizei;
      Value : GLint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.uniform2iv("
         & Location'Image & ","
         & Count'Image & ","
         & Value'Image
           & ")");
      else
Context.Execute (
        "uniform2iv("
         & Location'Image & ","
         & Count'Image & ","
         & Value'Image
        & ")");
      end if;
   end Uniform_2iv;

   -----------------
   -- Uniform_3 --
   -----------------

   procedure Uniform_3
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      V0 : GLfloat;
      V1 : GLfloat;
      V2 : GLfloat)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.uniform3("
         & Location'Image & ","
         & Support.Image (V0) & ","
         & Support.Image (V1) & ","
         & Support.Image (V2)
           & ")");
      else
Context.Execute (
        "uniform3("
         & Location'Image & ","
         & Support.Image (V0) & ","
         & Support.Image (V1) & ","
         & Support.Image (V2)
        & ")");
      end if;
   end Uniform_3;

   ------------------
   -- Uniform_3fv --
   ------------------

   procedure Uniform_3fv
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      Count : GLsizei;
      Value : GLfloat)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.uniform3fv("
         & Location'Image & ","
         & Count'Image & ","
         & Support.Image (Value)
           & ")");
      else
Context.Execute (
        "uniform3fv("
         & Location'Image & ","
         & Count'Image & ","
         & Support.Image (Value)
        & ")");
      end if;
   end Uniform_3fv;

   -----------------
   -- Uniform_3i --
   -----------------

   procedure Uniform_3i
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      V0 : GLint;
      V1 : GLint;
      V2 : GLint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.uniform3i("
         & Location'Image & ","
         & V0'Image & ","
         & V1'Image & ","
         & V2'Image
           & ")");
      else
Context.Execute (
        "uniform3i("
         & Location'Image & ","
         & V0'Image & ","
         & V1'Image & ","
         & V2'Image
        & ")");
      end if;
   end Uniform_3i;

   ------------------
   -- Uniform_3iv --
   ------------------

   procedure Uniform_3iv
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      Count : GLsizei;
      Value : GLint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.uniform3iv("
         & Location'Image & ","
         & Count'Image & ","
         & Value'Image
           & ")");
      else
Context.Execute (
        "uniform3iv("
         & Location'Image & ","
         & Count'Image & ","
         & Value'Image
        & ")");
      end if;
   end Uniform_3iv;

   -----------------
   -- Uniform_4 --
   -----------------

   procedure Uniform_4
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      V0 : GLfloat;
      V1 : GLfloat;
      V2 : GLfloat;
      V3 : GLfloat)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.uniform4("
         & Location'Image & ","
         & Support.Image (V0) & ","
         & Support.Image (V1) & ","
         & Support.Image (V2) & ","
         & Support.Image (V3)
           & ")");
      else
Context.Execute (
        "uniform4("
         & Location'Image & ","
         & Support.Image (V0) & ","
         & Support.Image (V1) & ","
         & Support.Image (V2) & ","
         & Support.Image (V3)
        & ")");
      end if;
   end Uniform_4;

   ------------------
   -- Uniform_4fv --
   ------------------

   procedure Uniform_4fv
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      Count : GLsizei;
      Value : GLfloat)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.uniform4fv("
         & Location'Image & ","
         & Count'Image & ","
         & Support.Image (Value)
           & ")");
      else
Context.Execute (
        "uniform4fv("
         & Location'Image & ","
         & Count'Image & ","
         & Support.Image (Value)
        & ")");
      end if;
   end Uniform_4fv;

   -----------------
   -- Uniform_4i --
   -----------------

   procedure Uniform_4i
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      V0 : GLint;
      V1 : GLint;
      V2 : GLint;
      V3 : GLint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.uniform4i("
         & Location'Image & ","
         & V0'Image & ","
         & V1'Image & ","
         & V2'Image & ","
         & V3'Image
           & ")");
      else
Context.Execute (
        "uniform4i("
         & Location'Image & ","
         & V0'Image & ","
         & V1'Image & ","
         & V2'Image & ","
         & V3'Image
        & ")");
      end if;
   end Uniform_4i;

   ------------------
   -- Uniform_4iv --
   ------------------

   procedure Uniform_4iv
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      Count : GLsizei;
      Value : GLint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.uniform4iv("
         & Location'Image & ","
         & Count'Image & ","
         & Value'Image
           & ")");
      else
Context.Execute (
        "uniform4iv("
         & Location'Image & ","
         & Count'Image & ","
         & Value'Image
        & ")");
      end if;
   end Uniform_4iv;

   ------------------------
   -- Uniform_Matrix_2fv --
   ------------------------

   procedure Uniform_Matrix_2fv
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      Count : GLsizei;
      Transpose : Boolean;
      Value : GLfloat)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.uniformMatrix2fv("
         & Location'Image & ","
         & Count'Image & ","
         & GLEnum_Property (Context, Transpose'Image) & ","
         & Support.Image (Value)
           & ")");
      else
Context.Execute (
        "uniformMatrix2fv("
         & Location'Image & ","
         & Count'Image & ","
         & GLEnum_Property (Context, Transpose'Image) & ","
         & Support.Image (Value)
        & ")");
      end if;
   end Uniform_Matrix_2fv;

   ------------------------
   -- Uniform_Matrix_3fv --
   ------------------------

   procedure Uniform_Matrix_3fv
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      Count : GLsizei;
      Transpose : Boolean;
      Value : GLfloat)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.uniformMatrix3fv("
         & Location'Image & ","
         & Count'Image & ","
         & GLEnum_Property (Context, Transpose'Image) & ","
         & Support.Image (Value)
           & ")");
      else
Context.Execute (
        "uniformMatrix3fv("
         & Location'Image & ","
         & Count'Image & ","
         & GLEnum_Property (Context, Transpose'Image) & ","
         & Support.Image (Value)
        & ")");
      end if;
   end Uniform_Matrix_3fv;

   ------------------------
   -- Uniform_Matrix_4fv --
   ------------------------

   procedure Uniform_Matrix_4fv
     (Context : in out Context_WebGL_Type'Class;
      Location : GLint;
      Count : GLsizei;
      Transpose : Boolean;
      Value : GLfloat)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.uniformMatrix4fv("
         & Location'Image & ","
         & Count'Image & ","
         & GLEnum_Property (Context, Transpose'Image) & ","
         & Support.Image (Value)
           & ")");
      else
Context.Execute (
        "uniformMatrix4fv("
         & Location'Image & ","
         & Count'Image & ","
         & GLEnum_Property (Context, Transpose'Image) & ","
         & Support.Image (Value)
        & ")");
      end if;
   end Uniform_Matrix_4fv;

   ------------------
   -- Use_Program --
   ------------------

   procedure Use_Program
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.useProgram("
         & Support.Indexed_Object_Reference (Context, Program)
           & ")");
      else
Context.Execute (
        "useProgram("
         & Support.Indexed_Object_Reference (Context, Program)
        & ")");
      end if;
   end Use_Program;

   -----------------------
   -- Validate_Program --
   -----------------------

   procedure Validate_Program
     (Context : in out Context_WebGL_Type'Class;
      Program : GLuint)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.validateProgram("
         & Support.Indexed_Object_Reference (Context, Program)
           & ")");
      else
Context.Execute (
        "validateProgram("
         & Support.Indexed_Object_Reference (Context, Program)
        & ")");
      end if;
   end Validate_Program;

   ----------------------
   -- Vertex_Attrib_1 --
   ----------------------

   procedure Vertex_Attrib_1
     (Context : in out Context_WebGL_Type'Class;
      Index : GLuint;
      X : GLfloat)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.vertexAttrib1("
         & Index'Image & ","
         & Support.Image (X)
           & ")");
      else
Context.Execute (
        "vertexAttrib1("
         & Index'Image & ","
         & Support.Image (X)
        & ")");
      end if;
   end Vertex_Attrib_1;

   -----------------------
   -- Vertex_Attrib_1fv --
   -----------------------

   procedure Vertex_Attrib_1fv
     (Context : in out Context_WebGL_Type'Class;
      Index : GLuint;
      V : GLfloat)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.vertexAttrib1fv("
         & Index'Image & ","
         & Support.Image (V)
           & ")");
      else
Context.Execute (
        "vertexAttrib1fv("
         & Index'Image & ","
         & Support.Image (V)
        & ")");
      end if;
   end Vertex_Attrib_1fv;

   ----------------------
   -- Vertex_Attrib_2 --
   ----------------------

   procedure Vertex_Attrib_2
     (Context : in out Context_WebGL_Type'Class;
      Index : GLuint;
      X : GLfloat;
      Y : GLfloat)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.vertexAttrib2("
         & Index'Image & ","
         & Support.Image (X) & ","
         & Support.Image (Y)
           & ")");
      else
Context.Execute (
        "vertexAttrib2("
         & Index'Image & ","
         & Support.Image (X) & ","
         & Support.Image (Y)
        & ")");
      end if;
   end Vertex_Attrib_2;

   -----------------------
   -- Vertex_Attrib_2fv --
   -----------------------

   procedure Vertex_Attrib_2fv
     (Context : in out Context_WebGL_Type'Class;
      Index : GLuint;
      V : GLfloat)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.vertexAttrib2fv("
         & Index'Image & ","
         & Support.Image (V)
           & ")");
      else
Context.Execute (
        "vertexAttrib2fv("
         & Index'Image & ","
         & Support.Image (V)
        & ")");
      end if;
   end Vertex_Attrib_2fv;

   ----------------------
   -- Vertex_Attrib_3 --
   ----------------------

   procedure Vertex_Attrib_3
     (Context : in out Context_WebGL_Type'Class;
      Index : GLuint;
      X : GLfloat;
      Y : GLfloat;
      Z : GLfloat)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.vertexAttrib3("
         & Index'Image & ","
         & Support.Image (X) & ","
         & Support.Image (Y) & ","
         & Support.Image (Z)
           & ")");
      else
Context.Execute (
        "vertexAttrib3("
         & Index'Image & ","
         & Support.Image (X) & ","
         & Support.Image (Y) & ","
         & Support.Image (Z)
        & ")");
      end if;
   end Vertex_Attrib_3;

   -----------------------
   -- Vertex_Attrib_3fv --
   -----------------------

   procedure Vertex_Attrib_3fv
     (Context : in out Context_WebGL_Type'Class;
      Index : GLuint;
      V : GLfloat)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.vertexAttrib3fv("
         & Index'Image & ","
         & Support.Image (V)
           & ")");
      else
Context.Execute (
        "vertexAttrib3fv("
         & Index'Image & ","
         & Support.Image (V)
        & ")");
      end if;
   end Vertex_Attrib_3fv;

   ----------------------
   -- Vertex_Attrib_4 --
   ----------------------

   procedure Vertex_Attrib_4
     (Context : in out Context_WebGL_Type'Class;
      Index : GLuint;
      X : GLfloat;
      Y : GLfloat;
      Z : GLfloat;
      W : GLfloat)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.vertexAttrib4("
         & Index'Image & ","
         & Support.Image (X) & ","
         & Support.Image (Y) & ","
         & Support.Image (Z) & ","
         & Support.Image (W)
           & ")");
      else
Context.Execute (
        "vertexAttrib4("
         & Index'Image & ","
         & Support.Image (X) & ","
         & Support.Image (Y) & ","
         & Support.Image (Z) & ","
         & Support.Image (W)
        & ")");
      end if;
   end Vertex_Attrib_4;

   -----------------------
   -- Vertex_Attrib_4fv --
   -----------------------

   procedure Vertex_Attrib_4fv
     (Context : in out Context_WebGL_Type'Class;
      Index : GLuint;
      V : GLfloat)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.vertexAttrib4fv("
         & Index'Image & ","
         & Support.Image (V)
           & ")");
      else
Context.Execute (
        "vertexAttrib4fv("
         & Index'Image & ","
         & Support.Image (V)
        & ")");
      end if;
   end Vertex_Attrib_4fv;

   ---------------------------
   -- Vertex_Attrib_Pointer --
   ---------------------------

   procedure Vertex_Attrib_Pointer
     (Context : in out Context_WebGL_Type'Class;
      Index : GLuint;
      Size : GLuint;
      Element_Type : Vertex_Attrib_Pointer_Type;
      Normalized : Boolean;
      Stride : GLsizei;
      Pointer : System.Storage_Elements.Storage_Offset)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.vertexAttribPointer("
         & Index'Image & ","
         & Size'Image & ","
         & Vertex_Attrib_Pointer_Type_Values (Element_Type)'Image & ","
         & GLEnum_Property (Context, Normalized'Image) & ","
         & Stride'Image & ","
         & Pointer'Image
           & ")");
      else
Context.Execute (
        "vertexAttribPointer("
         & Index'Image & ","
         & Size'Image & ","
         & Vertex_Attrib_Pointer_Type_Values (Element_Type)'Image & ","
         & GLEnum_Property (Context, Normalized'Image) & ","
         & Stride'Image & ","
         & Pointer'Image
        & ")");
      end if;
   end Vertex_Attrib_Pointer;

   ----------------
   -- Viewport --
   ----------------

   procedure Viewport
     (Context : in out Context_WebGL_Type'Class;
      X : GLint;
      Y : GLint;
      Width : GLsizei;
      Height : GLsizei)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.viewport("
         & X'Image & ","
         & Y'Image & ","
         & Width'Image & ","
         & Height'Image
           & ")");
      else
Context.Execute (
        "viewport("
         & X'Image & ","
         & Y'Image & ","
         & Width'Image & ","
         & Height'Image
        & ")");
      end if;
   end Viewport;

   --------------------
   -- Create_Buffer --
   --------------------

   function Create_Buffer
     (Context : in out Context_WebGL_Type'Class)
     return GLuint
   is
   begin
      if Context.Rendering then
         raise Constraint_Error with
            "Illegal call to glCreateBuffer during render";
      else
         declare
            Result : constant GLuint := 
           Support.Indexed_Javascript_Object (Context,
        "createBuffer("

        & ")");
      begin
      return Result;
   end;
      end if;
   end Create_Buffer;

   ---------------------
   -- Create_Texture --
   ---------------------

   function Create_Texture
     (Context : in out Context_WebGL_Type'Class)
     return GLuint
   is
   begin
      if Context.Rendering then
         raise Constraint_Error with
            "Illegal call to glCreateTexture during render";
      else
         declare
            Result : constant GLuint := 
           Support.Indexed_Javascript_Object (Context,
        "createTexture("

        & ")");
      begin
      return Result;
   end;
      end if;
   end Create_Texture;

   --------------------------
   -- Get_Shader_Parameter --
   --------------------------

   function Get_Shader_Parameter
     (Context : in out Context_WebGL_Type'Class;
      Shader : GLuint;
      Parameter : Shader_Parameter_Name)
     return Boolean
   is
   begin
      if Context.Rendering then
         raise Constraint_Error with
            "Illegal call to glGetShaderParameter during render";
      else
         declare
            Result : constant Boolean := 
           Boolean'Value (Context.Execute (
        "getShaderParameter("
         & Support.Indexed_Object_Reference (Context, Shader) & ","
         & Shader_Parameter_Name_Values (Parameter)'Image
        & ")"));
      begin
      return Result;
   end;
      end if;
   end Get_Shader_Parameter;

   ---------------------------
   -- Get_Program_Parameter --
   ---------------------------

   function Get_Program_Parameter
     (Context : in out Context_WebGL_Type'Class;
      Progam : GLuint;
      Parameter : Program_Property_ARB)
     return Boolean
   is
   begin
      if Context.Rendering then
         raise Constraint_Error with
            "Illegal call to glGetProgramParameter during render";
      else
         declare
            Result : constant Boolean := 
           Boolean'Value (Context.Execute (
        "getProgramParameter("
         & Support.Indexed_Object_Reference (Context, Progam) & ","
         & Program_Property_ARB_Values (Parameter)'Image
        & ")"));
      begin
      return Result;
   end;
      end if;
   end Get_Program_Parameter;

   --------------------
   -- Set_Texture_Mag_Filter --
   --------------------

   procedure Set_Texture_Mag_Filter
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Param : Texture_Mag_Filter)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.texParameter("
         & Texture_Target_Values (Target)'Image & ","
         & Texture_Parameter_Name_Values (GL_TEXTURE_MAG_FILTER)'Image & ","
         & Texture_Mag_Filter_Values (Param)'Image
           & ")");
      else
Context.Execute (
        "texParameter("
         & Texture_Target_Values (Target)'Image & ","
         & Texture_Parameter_Name_Values (GL_TEXTURE_MAG_FILTER)'Image & ","
         & Texture_Mag_Filter_Values (Param)'Image
        & ")");
      end if;
   end Set_Texture_Mag_Filter;

   --------------------
   -- Set_Texture_Min_Filter --
   --------------------

   procedure Set_Texture_Min_Filter
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Param : Texture_Min_Filter)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.texParameter("
         & Texture_Target_Values (Target)'Image & ","
         & Texture_Parameter_Name_Values (GL_TEXTURE_MIN_FILTER)'Image & ","
         & Texture_Min_Filter_Values (Param)'Image
           & ")");
      else
Context.Execute (
        "texParameter("
         & Texture_Target_Values (Target)'Image & ","
         & Texture_Parameter_Name_Values (GL_TEXTURE_MIN_FILTER)'Image & ","
         & Texture_Min_Filter_Values (Param)'Image
        & ")");
      end if;
   end Set_Texture_Min_Filter;

   --------------------
   -- Texture_Wrap_S --
   --------------------

   procedure Texture_Wrap_S
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Param : Texture_Wrap_Mode)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.texParameter("
         & Texture_Target_Values (Target)'Image & ","
         & Texture_Parameter_Name_Values (GL_TEXTURE_WRAP_S)'Image & ","
         & Texture_Wrap_Mode_Values (Param)'Image
           & ")");
      else
Context.Execute (
        "texParameter("
         & Texture_Target_Values (Target)'Image & ","
         & Texture_Parameter_Name_Values (GL_TEXTURE_WRAP_S)'Image & ","
         & Texture_Wrap_Mode_Values (Param)'Image
        & ")");
      end if;
   end Texture_Wrap_S;

   --------------------
   -- Texture_Wrap_T --
   --------------------

   procedure Texture_Wrap_T
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Param : Texture_Wrap_Mode)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.texParameter("
         & Texture_Target_Values (Target)'Image & ","
         & Texture_Parameter_Name_Values (GL_TEXTURE_WRAP_T)'Image & ","
         & Texture_Wrap_Mode_Values (Param)'Image
           & ")");
      else
Context.Execute (
        "texParameter("
         & Texture_Target_Values (Target)'Image & ","
         & Texture_Parameter_Name_Values (GL_TEXTURE_WRAP_T)'Image & ","
         & Texture_Wrap_Mode_Values (Param)'Image
        & ")");
      end if;
   end Texture_Wrap_T;

   ------------------
   -- Tex_Image_2D --
   ------------------

   procedure Tex_Image_2D
     (Context : in out Context_WebGL_Type'Class;
      Target : Texture_Target;
      Level : GLint;
      Internalformat : Internal_Format;
      Format : Pixel_Format;
      Item_Type : Pixel_Type;
      Image_Id : String)
   is
   begin
      if Context.Rendering then
         Context.Render_Script.Append (
           "gl.texImage2D("
         & Texture_Target_Values (Target)'Image & ","
         & Level'Image & ","
         & Internal_Format_Values (Internalformat)'Image & ","
         & Pixel_Format_Values (Format)'Image & ","
         & Pixel_Type_Values (Item_Type)'Image & ","
         & "$('#" & Image_Id & "')" & ".get(0)"
           & ")");
      else
Context.Execute (
        "texImage2D("
         & Texture_Target_Values (Target)'Image & ","
         & Level'Image & ","
         & Internal_Format_Values (Internalformat)'Image & ","
         & Pixel_Format_Values (Format)'Image & ","
         & Pixel_Type_Values (Item_Type)'Image & ","
         & "$('#" & Image_Id & "')" & ".get(0)"
        & ")");
      end if;
   end Tex_Image_2D;

end Gnoga.Gui.Element.Canvas.Context_WebGL;
