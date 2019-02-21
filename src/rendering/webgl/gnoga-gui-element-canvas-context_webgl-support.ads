private package Gnoga.Gui.Element.Canvas.Context_WebGL.Support is

   procedure Get_Drawing_Context_WebGL
     (Context   : in out Context_WebGL_Type;
      Canvas    : in out Canvas_Type'Class);

   procedure Begin_Render
     (Context : in out Context_WebGL_Type);

   procedure End_Render
     (Context : in out Context_WebGL_Type);

   function Indexed_Javascript_Object
     (Context : in out Context_WebGL_Type'Class;
      Method  : String)
      return GLuint;

   function Indexed_Object_Reference
     (Context : in out Context_WebGL_Type'Class;
      Index   : GLuint)
      return String;

   procedure Destroy_Indexed_Javascript_Object
     (Context   : in out Context_WebGL_Type'Class;
      Index     : GLuint;
      Script    : String);

   procedure Frustum
     (Context       : in out Context_WebGL_Type'Class;
      Matrix        : out Matrix_4;
      Left, Right   : GLfloat;
      Bottom, Top   : GLfloat;
      Near, Far     : GLfloat);

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
      Location : GLuint;
      Matrix   : Matrix_4);

   function Image (Value : GLfloat) return String;

end Gnoga.Gui.Element.Canvas.Context_WebGL.Support;
