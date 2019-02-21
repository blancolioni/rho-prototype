with Ada.Characters.Latin_1;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Strings.Fixed;

with Gnoga.Server.Connection;

package body Gnoga.Gui.Element.Canvas.Context_WebGL.Support is

   package GLfloat_Functions is
     new Ada.Numerics.Generic_Elementary_Functions (GLfloat);

   function Rotation_Matrix
     (Angle     : GLfloat;
      Direction : Vector_3)
      return Matrix_4;

   function Cross_Product_Matrix
     (V : Vector_3)
      return Matrix_3;

   function Tensor_Product
     (X, Y : Vector_3)
      return Matrix_3;

   ------------------
   -- Begin_Render --
   ------------------

   procedure Begin_Render
     (Context : in out Context_WebGL_Type)
   is
   begin
      Context.Rendering := True;
      Context.Render_Script.Clear;
   end Begin_Render;

   --------------------------
   -- Cross_Product_Matrix --
   --------------------------

   function Cross_Product_Matrix
     (V : Vector_3)
      return Matrix_3
   is
      Result : Matrix_3 := (others => (others => 0.0));
   begin
      Result (1, 2) := -V (3);
      Result (1, 3) := V (2);
      Result (2, 1) := V (3);
      Result (2, 3) := -V (1);
      Result (3, 1) := -V (2);
      Result (3, 2) := V (1);
      return Result;
   end Cross_Product_Matrix;

   ---------------------------------------
   -- Destroy_Indexed_Javascript_Object --
   ---------------------------------------

   procedure Destroy_Indexed_Javascript_Object
     (Context   : in out Context_WebGL_Type'Class;
      Index     : GLuint;
      Script    : String)
   is null;

   ----------------
   -- End_Render --
   ----------------

   procedure End_Render
     (Context : in out Context_WebGL_Type)
   is
      use Ada.Strings.Unbounded;
      LF : constant String := (1 => Ada.Characters.Latin_1.LF);
      Script : Unbounded_String :=
                 To_Unbounded_String
                   ("requestAnimationFrame(function(now){" & LF
                    & "   var gl = gnoga['" & Context.ID & "'];" & LF);
   begin
      for Item of Context.Render_Script loop
         Script := Script & Item & ";" & LF;
      end loop;
      Script := Script & "   })";
      Gnoga.Server.Connection.Execute_Script
        (ID     => Context.Connection_ID,
         Script => To_String (Script));
      Context.Rendering := False;
   end End_Render;

   -------------
   -- Frustum --
   -------------

   procedure Frustum
     (Context       : in out Context_WebGL_Type'Class;
      Matrix        : out Matrix_4;
      Left, Right   : GLfloat;
      Bottom, Top   : GLfloat;
      Near, Far     : GLfloat)
   is
      pragma Unreferenced (Context);
      A : constant GLfloat := (Right + Left) / (Right - Left);
      B : constant GLfloat := (Top + Bottom) / (Top - Bottom);
      C : constant GLfloat := (Far + Near) / (Far - Near);
      D : constant GLfloat := 2.0 * Far * Near / (Far - Near);
   begin
      Matrix :=
        ((2.0 * Near / (Right - Left), 0.0, A, 0.0),
         (0.0, 2.0 * Near / (Top - Bottom), B, 0.0),
         (0.0, 0.0, C, D),
         (0.0, 0.0, -1.0, 0.0));
   end Frustum;

   -------------------------------
   -- Get_Drawing_Context_WebGL --
   -------------------------------

   procedure Get_Drawing_Context_WebGL
     (Context   : in out Context_WebGL_Type;
      Canvas    : in out Canvas_Type'Class)
   is
      GID : constant String := Gnoga.Server.Connection.New_GID;
   begin
      Context.Context_ID := Ada.Strings.Unbounded.To_Unbounded_String (GID);
      Context.Connection_ID := Canvas.Connection_ID;
      Context.View_Width := Canvas.Width;
      Context.View_Height := Canvas.Height;
      Gnoga.Server.Connection.Execute_Script
        (Context.Connection_ID,
         "gnoga['" & GID & "'] = " &
           Canvas.jQuery &
           ".get(0).getContext('webgl');");
      Context.Viewport (0, 0,
                        GLsizei (Context.View_Width),
                        GLsizei (Context.View_Height));
   end Get_Drawing_Context_WebGL;

   -----------
   -- Image --
   -----------

   function Image (Value : GLfloat) return String is
   begin
      return Ada.Strings.Fixed.Trim (Value'Image, Ada.Strings.Both);
   end Image;

   -------------------------------
   -- Indexed_Javascript_Object --
   -------------------------------

   function Indexed_Javascript_Object
     (Context : in out Context_WebGL_Type'Class;
      Method  : String)
      return GLuint
   is
      GID : constant String := Gnoga.Server.Connection.New_GID;
   begin
      Gnoga.Server.Connection.Execute_Script
        (Context.Connection_ID,
         "gnoga['" & GID & "'] = "
         & "gnoga['" & Context.ID & "']." & Method);
      Context.Web_Object_Ids.Append (GID);
      return GLuint (Context.Web_Object_Ids.Last_Index);
   end Indexed_Javascript_Object;

   ------------------------------
   -- Indexed_Object_Reference --
   ------------------------------

   function Indexed_Object_Reference
     (Context : in out Context_WebGL_Type'Class;
      Index   : GLuint)
      return String
   is
      GID : constant String :=
              Context.Web_Object_Ids.Element (Positive (Index));
   begin
      return "gnoga['" & GID & "']";
   end Indexed_Object_Reference;

   -----------------
   -- Perspective --
   -----------------

   procedure Perspective
     (Context       : in out Context_WebGL_Type'Class;
      Matrix        : out Matrix_4;
      Field_Of_View : GLfloat;
      Aspect_Ratio  : GLfloat;
      Near, Far     : GLfloat)
   is
      pragma Unreferenced (Context);
      use GLfloat_Functions;
      F     : constant GLfloat := Tan (Field_Of_View / 2.0, 360.0);
   begin
      Matrix := ((1.0 / (Aspect_Ratio * F), 0.0, 0.0, 0.0),
                 (0.0, 1.0 / F, 0.0, 0.0),
                 (0.0, 0.0, -(Near - Far) / (Near - Far),
                  (2.0 * Far * Near) / (Near - Far)),
                 (0.0, 0.0, -1.0, 0.0));
   end Perspective;

   ------------
   -- Rotate --
   ------------

   procedure Rotate
     (Context : in out Context_WebGL_Type'Class;
      Matrix  : in out Matrix_4;
      Angle   : GLfloat;
      X, Y, Z : GLfloat)
   is
      pragma Unreferenced (Context);
      use Matrices;
      V : constant Vector_3 := (X, Y, Z);
   begin
      Matrix := Matrix * Rotation_Matrix (Angle, V / abs V);
   end Rotate;

   ---------------------
   -- Rotation_Matrix --
   ---------------------

   function Rotation_Matrix
     (Angle     : GLfloat;
      Direction : Vector_3)
      return Matrix_4
   is
      use GLfloat_Functions;
      use Matrices;
      Cos_Theta : constant GLfloat := Cos (Angle, 360.0);
      Sin_Theta : constant GLfloat := Sin (Angle, 360.0);
      Cross     : constant Matrix_3 := Cross_Product_Matrix (Direction);
      Tensor    : constant Matrix_3 := Tensor_Product (Direction, Direction);
      Rot       : constant Matrix_3 :=
                    Unit_Matrix (3) * Cos_Theta
                    + Sin_Theta * Cross
                    + (1.0 - Cos_Theta) * Tensor;
   begin
      return Matrix : Matrix_4 := Unit_Matrix (4) do
         for I in 1 .. 3 loop
            for J in 1 .. 3 loop
               Matrix (I, J) := Rot (I, J);
            end loop;
         end loop;
      end return;
   end Rotation_Matrix;

   --------------------
   -- Tensor_Product --
   --------------------

   function Tensor_Product
     (X, Y : Vector_3)
      return Matrix_3
   is
      Result : Matrix_3;
   begin
      for I in 1 .. 3 loop
         for J in 1 .. 3 loop
            Result (I, J) := X (I) * Y (J);
         end loop;
      end loop;
      return Result;
   end Tensor_Product;

   ---------------
   -- Translate --
   ---------------

   procedure Translate
     (Context : in out Context_WebGL_Type'Class;
      Matrix  : in out Matrix_4;
      X, Y, Z : GLfloat)
   is
      pragma Unreferenced (Context);
      use Matrices;
      M : Matrix_4 := Unit_Matrix (4);
   begin
      M (1, 4) := X;
      M (2, 4) := Y;
      M (3, 4) := Z;
      Matrix := Matrix * M;
   end Translate;

   --------------------
   -- Uniform_Matrix --
   --------------------

   procedure Uniform_Matrix
     (Context  : in out Context_WebGL_Type'Class;
      Location : GLuint;
      Matrix   : Matrix_4)
   is
      use Ada.Strings.Unbounded;
      Data_Image : Unbounded_String;
   begin
      for I in Matrix'Range (2) loop
         for J in Matrix'Range (1) loop
            if Data_Image /= "" then
               Data_Image := Data_Image & ",";
            end if;
            Data_Image := Data_Image & Image (Matrix (J, I));
         end loop;
      end loop;

      declare
         Script : constant String :=
                    "uniformMatrix4fv("
                       & Indexed_Object_Reference (Context, Location)
                       & ","
                       & "false"
                       & ","
                       & "new Float32Array([" & To_String (Data_Image) & "])"
                    & ")";
      begin
         if Context.Rendering then
            Context.Render_Script.Append ("gl." & Script);
         else
            Context.Execute (Script);
         end if;
      end;
   end Uniform_Matrix;

end Gnoga.Gui.Element.Canvas.Context_WebGL.Support;
