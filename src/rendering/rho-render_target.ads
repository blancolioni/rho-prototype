with Rho.Color;
with Rho.Float_Buffer;
with Rho.Limits;
with Rho.Matrices;
with Rho.Rectangle;
with Rho.Render_Operation;
with Rho.Shaders.Values;
with Rho.Texture;
with Rho.Viewport;

package Rho.Render_Target is

   type Rho_Blend_Function is
     (Zero, One,
      Source_Alpha, Destination_Alpha,
      One_Minus_Source_Alpha, One_Minus_Destination_Alpha);

   type Rho_Render_Target_Record is
     abstract limited new Rho.Rectangle.Rho_Rectangle_Interface
   with private;

   type Rho_Render_Target is access all Rho_Render_Target_Record'Class;

   procedure Set_Viewport
     (Target   : in out Rho_Render_Target_Record;
      Viewport : in Rho.Viewport.Rho_Viewport);

   function Viewport
     (Target : Rho_Render_Target_Record)
      return Rho.Viewport.Rho_Viewport;

   function Full_Viewport
     (Target : Rho_Render_Target_Record)
      return Rho.Viewport.Rho_Viewport;

   function Camera_Position
     (Target : Rho_Render_Target_Record)
      return Rho.Matrices.Vector_3;

   procedure Set_Camera_Position
     (Target   : in out Rho_Render_Target_Record;
      Position : Rho.Matrices.Vector_3);

   procedure Render
     (Target : not null access Rho_Render_Target_Record)
   is abstract;

   procedure After_Render
     (Target : not null access Rho_Render_Target_Record)
   is null;

   procedure Before_Render
     (Target : not null access Rho_Render_Target_Record)
   is null;

   procedure Set_Output_Position
     (Target : not null access Rho_Render_Target_Record;
      X, Y   : Rho_Float)
   is abstract;

   procedure Set_Raster_Position
     (Target : not null access Rho_Render_Target_Record;
      Position : Rho.Matrices.Vector_3)
   is abstract;

   procedure Set_Light
     (Target              : not null access Rho_Render_Target_Record;
      Index               : Rho.Limits.Light_Index;
      Ambient_Intensity   : Rho.Color.Rho_Color := (0.0, 0.0, 0.0, 1.0);
      Diffuse_Intensity   : Rho.Color.Rho_Color := (1.0, 1.0, 1.0, 1.0);
      Specular_Intensity  : Rho.Color.Rho_Color := (1.0, 1.0, 1.0, 1.0);
      Position            : Rho.Matrices.Vector_3 := (0.0, 0.0, 1.0);
      Spot_Direction      : Rho.Matrices.Vector_3 := (0.0, 0.0, -1.0);
      Spot_Exponent       : Rho_Float := 0.0;
      Spot_Cutoff         : Rho_Float := 180.0;
      Attenuation         : Unit_Float := 0.0;
      Ambient_Coefficient : Unit_Float := 0.0);

   function Active_Light
     (Target : Rho_Render_Target_Record;
      Index  : Rho.Limits.Light_Index)
      return Boolean;

   function Light_Position
     (Target : Rho_Render_Target_Record;
      Index  : Rho.Limits.Light_Index)
      return Rho.Matrices.Vector_3;

   function Light_Ambient_Intensity
     (Target : Rho_Render_Target_Record;
      Index  : Rho.Limits.Light_Index)
      return Rho.Color.Rho_Color;

   function Light_Diffuse_Intensity
     (Target : Rho_Render_Target_Record;
      Index  : Rho.Limits.Light_Index)
      return Rho.Color.Rho_Color;

   function Light_Attenuation
     (Target : Rho_Render_Target_Record;
      Index  : Rho.Limits.Light_Index)
      return Unit_Float;

   function Light_Ambient_Coefficient
     (Target : Rho_Render_Target_Record;
      Index  : Rho.Limits.Light_Index)
      return Unit_Float;

   procedure Set_Texture
     (Target : not null access Rho_Render_Target_Record;
      Texture : Rho.Texture.Rho_Texture)
   is null;

   procedure Draw_Buffer
     (Target          : not null access Rho_Render_Target_Record;
      Buffer          : Rho.Float_Buffer.Rho_Float_Buffer;
      Operation       : Rho.Render_Operation.Operation_Type;
      Count           : Natural;
      Instance_Count  : Positive := 1)
   is abstract;

   procedure Bind_Vertex_Attribute
     (Target         : not null access Rho_Render_Target_Record;
      Buffer         : Rho.Float_Buffer.Rho_Float_Buffer;
      Attribute      : Rho.Shaders.Values.Rho_Attribute_Value;
      Start          : Positive;
      Component_Size : Positive;
      Is_Array       : Boolean)
   is abstract;

   function Clear_Color
     (Target : Rho_Render_Target_Record)
      return Rho.Color.Rho_Color;

   procedure Set_Clear_Color
     (Target      : in out Rho_Render_Target_Record;
      Clear_Color : Rho.Color.Rho_Color);

   procedure Blend
     (Target               : in out Rho_Render_Target_Record;
      Enabled              : Boolean;
      Source_Function      : Rho_Blend_Function := One;
      Destination_Function : Rho_Blend_Function := One)
   is abstract;

   procedure Blend
     (Target               : in out Rho_Render_Target_Record'Class;
      Source_Function      : Rho_Blend_Function;
      Destination_Function : Rho_Blend_Function);

   function Back_Face_Removal
     (Target : Rho_Render_Target_Record)
      return Boolean;

   function Double_Buffered
     (Target : Rho_Render_Target_Record)
      return Boolean;

   function Wireframe
     (Target  : Rho_Render_Target_Record)
      return Boolean;

   function Depth_Test
     (Target : Rho_Render_Target_Record)
      return Boolean;

   procedure Set_Back_Face_Removal
     (Target  : in out Rho_Render_Target_Record;
      Enabled : Boolean);

   procedure Set_Double_Buffered
     (Target  : in out Rho_Render_Target_Record;
      Enabled : Boolean);

   procedure Set_Wireframe
     (Target  : in out Rho_Render_Target_Record;
      Enabled : Boolean);

   procedure Set_Depth_Test
     (Target  : in out Rho_Render_Target_Record;
      Enabled : Boolean);

   procedure Enable_Point_Size
     (Target  : in out Rho_Render_Target_Record;
      Enabled : Boolean);

   type Render_Target_Resize_Handler is access
     procedure (Target : not null access Rho_Render_Target_Record'Class);

   procedure On_Resize
     (Target  : not null access Rho_Render_Target_Record'Class;
      Handler : Render_Target_Resize_Handler);

   procedure After_Resize
     (Target : not null access Rho_Render_Target_Record'Class);

   procedure Lock
     (Target : in out Rho_Render_Target_Record'Class);

   procedure Unlock
     (Target : in out Rho_Render_Target_Record'Class);

private

   type Light_Info is
      record
         Active              : Boolean := False;
         Ambient_Intensity   : Rho.Color.Rho_Color := (0.0, 0.0, 0.0, 1.0);
         Diffuse_Intensity   : Rho.Color.Rho_Color := (1.0, 1.0, 1.0, 1.0);
         Specular_Intensity  : Rho.Color.Rho_Color := (1.0, 1.0, 1.0, 1.0);
         Position            : Rho.Matrices.Vector_3 := (0.0, 0.0, 1.0);
         Spot_Direction      : Rho.Matrices.Vector_3 := (0.0, 0.0, -1.0);
         Spot_Exponent       : Rho_Float := 0.0;
         Spot_Cutoff         : Rho_Float := 180.0;
         Attenuation         : Unit_Float := 0.0;
         Ambient_Coefficient : Unit_Float := 0.0;
      end record;

   type Array_Of_Lights is
     array (Rho.Limits.Light_Index) of Light_Info;

   type Rho_Render_Target_Record is
     abstract limited new Rho.Rectangle.Rho_Rectangle_Interface with
      record
         Entire_Target     : Rho.Viewport.Rho_Viewport :=
                               Rho.Viewport.New_Viewport
                                 (0.0, 0.0, 100.0, 100.0);
         Viewport          : Rho.Viewport.Rho_Viewport;
         Clear_Color       : Rho.Color.Rho_Color;
         Back_Face_Removal : Boolean := True;
         Double_Buffered   : Boolean := True;
         Wireframe         : Boolean := False;
         Z_Buffer          : Boolean := True;
         Point_Size        : Boolean := False;
         Resize_Handler    : Render_Target_Resize_Handler;
         Current_Lights    : Array_Of_Lights;
         Camera_Position   : Rho.Matrices.Vector_3;
      end record;

end Rho.Render_Target;
