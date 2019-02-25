with Ada.Strings.Unbounded;

with Rho.Logging;

with Rho.Float_Arrays;
with Rho.Shaders.Loader;
with Rho.Shaders.Program;
with Rho.Shaders.Shader;

with Rho.Materials.Material;
with Rho.Materials.Templates;

with Rho.Context;

with Rho.Paths;

package body Rho.Materials.Pass is

   Write_Shader_Source : constant Boolean := False;

   Position_Attribute_Index : constant := 1;
   Normal_Attribute_Index   : constant := 2;
   Texture_Attribute_Index  : constant := 3;
   Color_Attribute_Index    : constant := 4;
   Instanced_Position_Index : constant := 5;
   Instanced_Color_Index    : constant := 6;

   package Shader_Caches is
     new Rho.Materials.Templates.Material_Template_Caches
       (Rho.Shaders.Rho_Shader_Interface, Rho.Shaders.Rho_Shader);

   Shader_Cache : Shader_Caches.Cache;

   procedure Add_Condition_Flags
     (Template  : in out Rho.Materials.Templates.Rho_Material_Template;
      Condition : Condition_Record);

   function Alpha_Discard_Condition
     (Operator : Material_Operator;
      Value    : Rho.Value.Rho_Value)
      return String;

   --------------
   -- Activate --
   --------------

   procedure Activate
     (Pass   : in out Rho_Material_Pass_Record;
      Target    : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class)
   is
   begin
      Target.Push_Shader (Pass.Shader);
      Target.Activate_Shader;

      if Pass.Has_Texture then
         Target.Set_Texture (Pass.Texture);
         Pass.Texture_Uniform.Set_Value (0);
      end if;
      if Pass.Lighting_Enabled then
         Pass.Lights (1).Position.Set_Value
           (Target.Light_Position (1));
         Pass.Lights (1).Intensities.Set_Value
           (Rho.Color.To_Vector_3
              (Target.Light_Diffuse_Intensity (1)));
         Pass.Lights (1).Attenuation.Set_Value
           (Target.Light_Attenuation (1));
         Pass.Lights (1).Ambient_Coefficient.Set_Value
           (Target.Light_Ambient_Coefficient (1));
         Pass.Specular_Uniform.Set_Value
           (Rho.Color.To_Vector_3 (Pass.Specular));
         Pass.Shininess_Uniform.Set_Value
           (Pass.Shininess);
         Pass.Camera_Uniform.Set_Value
           (Target.Camera_Position);
      end if;

      case Pass.Polygon_Mode is
         when Solid =>
            null;
         when Wireframe =>
            Target.Set_Wireframe (True);
         when Points =>
            null;
      end case;

      for I in 1 .. Pass.Parameter_Uniforms.Last_Index loop
         Pass.Parameter_Uniforms.Element (I).Set_Value
           (Pass.Technique.Material.Parameter_Value (I));
      end loop;

   end Activate;

   -------------------------
   -- Add_Condition_Flags --
   -------------------------

   procedure Add_Condition_Flags
     (Template  : in out Rho.Materials.Templates.Rho_Material_Template;
      Condition : Condition_Record)
   is
   begin
      case Condition.Condition is
         when Alpha_Discard =>
            Template.Set_Value
              ("alpha-discard",
               Alpha_Discard_Condition
                 (Condition.Operator,
                  Condition.Value));
         when Color_Discard =>
            Template.Set_Value
              ("color-discard",
               "if (finalColor.xyz == " & Condition.Value.Shader_Constant
               & ") discard;");
      end case;
   end Add_Condition_Flags;

   -------------------
   -- Alpha_Discard --
   -------------------

   procedure Alpha_Discard
     (Pass     : in out Rho_Material_Pass_Record'Class;
      Operator : Material_Operator;
      Value    : Rho.Unit_Float)
   is
   begin
      Pass.Conditions.Append
        ((Alpha_Discard, Operator,
         Rho.Value.Float_Value (Value)));
   end Alpha_Discard;

   -----------------------------
   -- Alpha_Discard_Condition --
   -----------------------------

   function Alpha_Discard_Condition
     (Operator : Material_Operator;
      Value    : Rho.Value.Rho_Value)
      return String
   is
   begin
      case Operator is
         when Always =>
            return "discard;";
         when Never =>
            return "";
         when others =>
            return "if (finalColor.w " & Operator_Symbol (Operator)
              & Value.Shader_Constant & ") discard;";
      end case;
   end Alpha_Discard_Condition;

   -------------
   -- Ambient --
   -------------

   function Ambient
     (Pass : Rho_Material_Pass_Record)
      return Rho.Color.Rho_Color
   is
   begin
      return Pass.Ambient;
   end Ambient;

   ------------------------------
   -- Bind_Standard_Attributes --
   ------------------------------

   procedure Bind_Standard_Attributes
     (Pass : in out Rho_Material_Pass_Record'Class)
   is
      Shader : constant Rho.Shaders.Program.Rho_Program :=
                 Rho.Shaders.Program.Rho_Program (Pass.Shader);
   begin
      Pass.Attributes.Append (Shader.Attribute_Value ("vert"));
      Pass.Attributes.Append (Shader.Attribute_Value ("vertNormal"));
      Pass.Attributes.Append (Shader.Attribute_Value ("vertTexCoord"));
      Pass.Attributes.Append (Shader.Attribute_Value ("vertColor"));

      if Pass.Has_Texture then
         Pass.Texture_Uniform := Shader.Uniform_Value ("materialTex");
      end if;

      if Pass.Lighting_Enabled then
         declare
            Uniforms : Light_Uniforms renames Pass.Lights (1);
         begin
            Uniforms.Position := Shader.Uniform_Value ("light.position");
            Uniforms.Intensities := Shader.Uniform_Value ("light.intensities");
            Uniforms.Attenuation := Shader.Uniform_Value ("light.attenuation");
            Uniforms.Ambient_Coefficient :=
              Shader.Uniform_Value ("light.ambientCoefficient");
         end;

         Pass.Shininess_Uniform :=
           Shader.Uniform_Value ("materialShininess");
         Pass.Specular_Uniform :=
           Shader.Uniform_Value ("materialSpecularColor");
         Pass.Camera_Uniform :=
           Shader.Uniform_Value ("cameraPosition");
      end if;

   end Bind_Standard_Attributes;

   ---------------------
   -- Color_Attribute --
   ---------------------

   function Color_Attribute
     (Pass : Rho_Material_Pass_Record'Class)
      return Rho.Shaders.Values.Rho_Attribute_Value
   is
   begin
      return Pass.Attributes (Color_Attribute_Index);
   end Color_Attribute;

   -------------------
   -- Color_Discard --
   -------------------

   procedure Color_Discard
     (Pass     : in out Rho_Material_Pass_Record'Class;
      Value    : Rho.Color.Rho_Color)
   is
   begin
      Pass.Conditions.Append
        ((Color_Discard, Equal,
         Rho.Value.Vector_Value ((Value.Red, Value.Green, Value.Blue))));
   end Color_Discard;

   -------------------
   -- Create_Shader --
   -------------------

   procedure Create_Shader
     (Pass : in out Rho_Material_Pass_Record'Class)
   is
      use type Rho.Shaders.Rho_Shader;

      use all type Rho.Texture.Texture_Address_Mode;

      Vertex   : Rho.Shaders.Shader.Rho_Shader;
      Fragment : Rho.Shaders.Shader.Rho_Shader;

      function Unit_Float_Image (Value : Unit_Float) return String;

      function Vector_Image
        (Value : Rho.Float_Arrays.Real_Vector)
         return String
        with Unreferenced;

      function Color_Image
        (Color : Rho.Color.Rho_Color)
         return String;

      -----------------
      -- Color_Image --
      -----------------

      function Color_Image
        (Color : Rho.Color.Rho_Color)
         return String
      is
      begin
         return "vec4 ("
           & Unit_Float_Image (Color.Red)
           & ","
           & Unit_Float_Image (Color.Green)
           & ","
           & Unit_Float_Image (Color.Blue)
           & ","
           & Unit_Float_Image (Color.Alpha)
           & ")";
      end Color_Image;

      ----------------------
      -- Unit_Float_Image --
      ----------------------

      function Unit_Float_Image (Value : Unit_Float) return String is
      begin
         if Value = 1.0 then
            return "1.0";
         elsif Value = 0.0 then
            return "0.0";
         else
            declare
               Image : String :=
                         "0"
                         & Natural'Image (Natural (Value * 1_000_000.0));
               Last  : Positive := Image'Last;
            begin
               Image (2) := '.';
               while Image (Last) = '0' loop
                  Last := Last - 1;
               end loop;
               if Image (Last) = '.' then
                  Last := Last + 1;
               end if;
               return Image (1 .. Last);
            end;
         end if;
      end Unit_Float_Image;

      ------------------
      -- Vector_Image --
      ------------------

      function Vector_Image
        (Value : Rho.Float_Arrays.Real_Vector)
         return String
      is
         use Ada.Strings.Unbounded;
         Result : Unbounded_String;
      begin
         for V of Value loop
            if Result = Null_Unbounded_String then
               Result := Result & "(";
            else
               Result := Result & ", ";
            end if;
            Result := Result & Unit_Float_Image (V);
         end loop;
         return To_String (Result) & ")";
      end Vector_Image;

      Template : Rho.Materials.Templates.Rho_Material_Template;

   begin

      Template.Set_Flag ("light", Pass.Lighting_Enabled);
      Template.Set_Flag ("ambient", Pass.Has_Ambient_Light);
      Template.Set_Flag ("diffuse", Pass.Has_Diffuse_Light);
      Template.Set_Flag ("emissive", Pass.Has_Emissive_Light);
      Template.Set_Flag ("specular", Pass.Has_Specular_Light);
      Template.Set_Flag ("shininess", Pass.Has_Shininess);
      Template.Set_Flag ("texture", Pass.Has_Texture);
      Template.Set_Flag ("mirror",
                         Pass.Has_Texture
                         and then Pass.Texture_Address_Mode = Mirror);

      if Pass.Has_Property ("ambient-color") then
         Template.Set_Value
           ("ambient-color",
            Pass.Get_Property ("ambient-color").Shader_Constant);
      else
         Template.Set_Value ("ambient-color", Color_Image (Pass.Ambient));
      end if;

      for Condition of Pass.Conditions loop
         Add_Condition_Flags (Template, Condition);
      end loop;

      declare
         Index : Natural := 0;

         procedure Add_Parameter
           (Name  : String;
            Value : Rho.Value.Rho_Value);

         -------------------
         -- Add_Parameter --
         -------------------

         procedure Add_Parameter
           (Name  : String;
            Value : Rho.Value.Rho_Value)
         is
         begin
            Index := Index + 1;
            Template.Set_Indexed_Value
              ("parameters", Index,
               "uniform " & Value.Shader_Type_Name
               & " Rho_param_" & Name & ";");
         end Add_Parameter;

      begin
         Pass.Technique.Material.Scan_Parameters
           (Add_Parameter'Access);
      end;

      Pass.Shader := Shader_Cache.Get (Template);

      if Pass.Shader = null then

         declare
            Vertex_Source : constant String :=
                              Template.Execute
                                (Rho.Paths.Config_File
                                   ("shaders/vertex_shader_template.vert"),
                                 Write_Result => Write_Shader_Source);
         begin
            Vertex :=
              Rho.Shaders.Loader.Create_From_Source
                (Pass.Technique.Material.Context.Renderer,
                 Rho.Shaders.Vertex_Shader,
                 Vertex_Source);
         end;

         declare
            Fragment_Source : constant String :=
                                Template.Execute
                                  (Rho.Paths.Config_File
                                     ("shaders/fragment_shader_template.frag"),
                                   Write_Result => Write_Shader_Source);
         begin
            Fragment :=
              Rho.Shaders.Loader.Create_From_Source
                (Pass.Technique.Material.Context.Renderer,
                 Rho.Shaders.Fragment_Shader, Fragment_Source);
         end;

         declare
            Program : constant Rho.Shaders.Program.Rho_Program :=
                        Rho.Shaders.Program.Create
                          (Pass.Technique.Material.Context.Renderer);
         begin
            Program.Add (Vertex);
            Program.Add (Fragment);
            Program.Compile;
            Pass.Shader := Rho.Shaders.Rho_Shader (Program);

            Shader_Cache.Insert (Template, Pass.Shader);
         end;

      end if;

      Pass.Bind_Standard_Attributes;

   end Create_Shader;

   ----------------
   -- Deactivate --
   ----------------

   procedure Deactivate
     (Pass     : in out Rho_Material_Pass_Record;
      Target    : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class)
   is
   begin
      Target.Pop_Shader;
      case Pass.Polygon_Mode is
         when Solid =>
            null;
         when Wireframe =>
            Target.Set_Wireframe (False);
         when Points =>
            null;
      end case;
   end Deactivate;

   -------------
   -- Diffuse --
   -------------

   function Diffuse
     (Pass : Rho_Material_Pass_Record)
      return Rho.Color.Rho_Color
   is
   begin
      return Pass.Diffuse;
   end Diffuse;

   --------------
   -- Emissive --
   --------------

   function Emissive
     (Pass : Rho_Material_Pass_Record)
      return Rho.Color.Rho_Color
   is
   begin
      return Pass.Emissive;
   end Emissive;

   ------------------
   -- Get_Property --
   ------------------

   overriding function Get_Property
     (Pass      : Rho_Material_Pass_Record;
      Name      : String)
      return Rho.Value.Rho_Value
   is
   begin
      return Pass.Properties.Get_Property (Name);
   end Get_Property;

   ----------------
   -- Get_Shader --
   ----------------

   overriding function Get_Shader
     (Pass : in out Rho_Material_Pass_Record)
      return Rho.Shaders.Rho_Shader
   is
      use type Rho.Shaders.Rho_Shader;
   begin
      if Pass.Shader = null then
         Rho.Logging.Put_Line
           (Pass.Technique.Material.Name
            & ": creating shader");
         if Pass.Instantiation = null then
            Pass.Create_Shader;
         else
            Pass.Instantiate_Shader;
         end if;
      end if;
      return Pass.Shader;
   end Get_Shader;

   -----------------------
   -- Has_Ambient_Light --
   -----------------------

   function Has_Ambient_Light
     (Pass : Rho_Material_Pass_Record)
      return Boolean
   is
   begin
      return Pass.Lighting_Enabled
        and then Pass.Has_Ambient;
   end Has_Ambient_Light;

   -------------------------
   -- Has_Color_Attribute --
   -------------------------

   function Has_Color_Attribute
     (Pass : Rho_Material_Pass_Record'Class)
      return Boolean
   is
      use type Rho.Shaders.Values.Rho_Attribute_Value;
   begin
      return Pass.Attributes (Color_Attribute_Index) /= null;
   end Has_Color_Attribute;

   -----------------------
   -- Has_Diffuse_Light --
   -----------------------

   function Has_Diffuse_Light
     (Pass : Rho_Material_Pass_Record)
      return Boolean
   is
   begin
      return Pass.Lighting_Enabled
        and then Pass.Has_Diffuse;
   end Has_Diffuse_Light;

   ------------------------
   -- Has_Emissive_Light --
   ------------------------

   function Has_Emissive_Light
     (Pass : Rho_Material_Pass_Record)
      return Boolean
   is
   begin
      return Pass.Lighting_Enabled
        and then Pass.Has_Emissive;
   end Has_Emissive_Light;

   --------------------------
   -- Has_Normal_Attribute --
   --------------------------

   function Has_Normal_Attribute
     (Pass : Rho_Material_Pass_Record'Class)
      return Boolean
   is
      use type Rho.Shaders.Values.Rho_Attribute_Value;
   begin
      return Pass.Attributes (Normal_Attribute_Index) /= null;
   end Has_Normal_Attribute;

   ------------------
   -- Has_Property --
   ------------------

   overriding function Has_Property
     (Pass      : Rho_Material_Pass_Record;
      Name      : String)
      return Boolean
   is
   begin
      return Pass.Properties.Has_Property (Name);
   end Has_Property;

   -------------------
   -- Has_Shininess --
   -------------------

   function Has_Shininess
     (Pass : Rho_Material_Pass_Record)
      return Boolean
   is
   begin
      return Pass.Has_Shininess;
   end Has_Shininess;

   ------------------------
   -- Has_Specular_Light --
   ------------------------

   function Has_Specular_Light
     (Pass : Rho_Material_Pass_Record)
      return Boolean
   is
   begin
      return Pass.Lighting_Enabled
        and then Pass.Has_Specular;
   end Has_Specular_Light;

   --------------------------------------
   -- Has_Texture_Coordinate_Attribute --
   --------------------------------------

   function Has_Texture_Coordinate_Attribute
     (Pass : Rho_Material_Pass_Record'Class)
      return Boolean
   is
      use type Rho.Shaders.Values.Rho_Attribute_Value;
   begin
      return Pass.Attributes (Texture_Attribute_Index) /= null;
   end Has_Texture_Coordinate_Attribute;

   -------------------------------
   -- Instanced_Color_Attribute --
   -------------------------------

   function Instanced_Color_Attribute
     (Pass : Rho_Material_Pass_Record'Class)
      return Rho.Shaders.Values.Rho_Attribute_Value
   is
   begin
      return Pass.Attributes (Instanced_Color_Index);
   end Instanced_Color_Attribute;

   ----------------------------------
   -- Instanced_Position_Attribute --
   ----------------------------------

   function Instanced_Position_Attribute
     (Pass : Rho_Material_Pass_Record'Class)
      return Rho.Shaders.Values.Rho_Attribute_Value
   is
   begin
      return Pass.Attributes (Instanced_Position_Index);
   end Instanced_Position_Attribute;

   -----------------
   -- Instantiate --
   -----------------

   function Instantiate
     (Pass : not null access Rho_Material_Pass_Record'Class;
      Technique : not null access
        Rho.Materials.Technique.Rho_Technique_Record'Class)
      return Rho_Material_Pass
   is
   begin
      return Result : constant Rho_Material_Pass :=
        new Rho_Material_Pass_Record'Class'(Pass.all)
      do
         Result.Technique := Technique;
         Result.Instantiation := Rho_Material_Pass (Pass);
         Result.Shader := null;
      end return;
   end Instantiate;

   ------------------------
   -- Instantiate_Shader --
   ------------------------

   procedure Instantiate_Shader
     (Pass : in out Rho_Material_Pass_Record'Class)
   is
      procedure Add_Parameter_Uniform
        (Name  : String;
         Value : Rho.Value.Rho_Value);

      ---------------------------
      -- Add_Parameter_Uniform --
      ---------------------------

      procedure Add_Parameter_Uniform
        (Name  : String;
         Value : Rho.Value.Rho_Value)
      is
         pragma Unreferenced (Value);
         Program : constant Rho.Shaders.Program.Rho_Program :=
                     Rho.Shaders.Program.Rho_Program (Pass.Shader);
         Uniform : constant Rho.Shaders.Values.Rho_Uniform_Value :=
                     Program.Uniform_Value
                       ("Rho_param_" & Name);
      begin
         Pass.Parameter_Uniforms.Append (Uniform);
      end Add_Parameter_Uniform;

   begin
      Pass.Shader := Pass.Instantiation.Shader;
      Pass.Bind_Standard_Attributes;
      Pass.Technique.Material.Scan_Parameters
        (Add_Parameter_Uniform'Access);
   end Instantiate_Shader;

   ----------------------
   -- Lighting_Enabled --
   ----------------------

   function Lighting_Enabled
     (Pass : Rho_Material_Pass_Record)
      return Boolean
   is
   begin
      return Pass.Lighting;
   end Lighting_Enabled;

   ----------
   -- Load --
   ----------

   procedure Load
     (Pass : in out Rho_Material_Pass_Record'Class)
   is
      use type Rho.Shaders.Rho_Shader;
   begin
      if Pass.Shader = null then
         if Pass.Instantiation = null then
            Pass.Create_Shader;
         else
            Pass.Instantiate_Shader;
         end if;
      end if;
   end Load;

   ----------------------
   -- Normal_Attribute --
   ----------------------

   function Normal_Attribute
     (Pass : Rho_Material_Pass_Record'Class)
      return Rho.Shaders.Values.Rho_Attribute_Value
   is
   begin
      return Pass.Attributes (Normal_Attribute_Index);
   end Normal_Attribute;

   ------------------
   -- Polygon_Mode --
   ------------------

   function Polygon_Mode
     (Pass : Rho_Material_Pass_Record'Class)
      return Material_Polygon_Mode
   is
   begin
      return Pass.Polygon_Mode;
   end Polygon_Mode;

   ------------------------
   -- Position_Attribute --
   ------------------------

   function Position_Attribute
     (Pass : Rho_Material_Pass_Record'Class)
      return Rho.Shaders.Values.Rho_Attribute_Value
   is
   begin
      return Pass.Attributes (Position_Attribute_Index);
   end Position_Attribute;

   ------------
   -- Rho_New --
   ------------

   procedure Rho_New
     (Pass      : in out Rho_Material_Pass;
      Technique : not null access
        Rho.Materials.Technique.Rho_Technique_Record'Class) is
   begin
      Pass := new Rho_Material_Pass_Record;
      Pass.Technique := Technique;
      Pass.Properties := new Rho.Properties.Rho_Property_Container_Record;
   end Rho_New;

   -----------------
   -- Set_Ambient --
   -----------------

   procedure Set_Ambient
     (Pass    : in out Rho_Material_Pass_Record'Class;
      Color   : Rho.Color.Rho_Color)
   is
   begin
      Pass.Ambient := Color;
      Pass.Has_Ambient := True;
   end Set_Ambient;

   -----------------
   -- Set_Diffuse --
   -----------------

   procedure Set_Diffuse
     (Pass    : in out Rho_Material_Pass_Record;
      Color   : Rho.Color.Rho_Color)
   is
   begin
      Pass.Diffuse := Color;
      Pass.Has_Diffuse := True;
   end Set_Diffuse;

   ------------------
   -- Set_Emissive --
   ------------------

   procedure Set_Emissive
     (Pass    : in out Rho_Material_Pass_Record;
      Color   : Rho.Color.Rho_Color)
   is
   begin
      Pass.Emissive := Color;
      Pass.Has_Emissive := True;
   end Set_Emissive;

   --------------------------
   -- Set_Lighting_Enabled --
   --------------------------

   procedure Set_Lighting_Enabled
     (Pass    : in out Rho_Material_Pass_Record;
      Enabled : Boolean)
   is
   begin
      Pass.Lighting := Enabled;
   end Set_Lighting_Enabled;

   ----------------------
   -- Set_Polygon_Mode --
   ----------------------

   procedure Set_Polygon_Mode
     (Pass : in out Rho_Material_Pass_Record'Class;
      Mode : Material_Polygon_Mode)
   is
   begin
      Pass.Polygon_Mode := Mode;
   end Set_Polygon_Mode;

   ------------------
   -- Set_Property --
   ------------------

   overriding procedure Set_Property
     (Pass      : in out Rho_Material_Pass_Record;
      Name      : String;
      Value     : Rho.Value.Rho_Value)
   is
   begin
      Pass.Properties.Set_Property (Name, Value);
   end Set_Property;

   ----------------
   -- Set_Shader --
   ----------------

   overriding procedure Set_Shader
     (Pass   : in out Rho_Material_Pass_Record;
      Shader : Rho.Shaders.Rho_Shader)
   is
   begin
      Pass.Shader := Shader;
      Pass.Bind_Standard_Attributes;
   end Set_Shader;

   -------------------
   -- Set_Shininess --
   -------------------

   procedure Set_Shininess
     (Pass  : in out Rho_Material_Pass_Record;
      Value : Non_Negative_Float)
   is
   begin
      Pass.Shininess := Value;
      Pass.Has_Shininess := True;
   end Set_Shininess;

   ------------------
   -- Set_Specular --
   ------------------

   procedure Set_Specular
     (Pass     : in out Rho_Material_Pass_Record;
      Value    : Rho.Color.Rho_Color)
   is
   begin
      Pass.Specular := Value;
      Pass.Has_Specular := True;
   end Set_Specular;

   -----------------
   -- Set_Texture --
   -----------------

   procedure Set_Texture
     (Pass    : in out Rho_Material_Pass_Record'Class;
      Texture : Rho.Texture.Rho_Texture)
   is
   begin
      Pass.Texture := Texture;
      Pass.Has_Texture := True;
   end Set_Texture;

   ------------------------------
   -- Set_Texture_Address_Mode --
   ------------------------------

   procedure Set_Texture_Address_Mode
     (Pass : in out Rho_Material_Pass_Record'Class;
      Mode : Rho.Texture.Texture_Address_Mode)
   is
   begin
      Pass.Tex_Address_Mode := Mode;
   end Set_Texture_Address_Mode;

   ---------------
   -- Shininess --
   ---------------

   function Shininess
     (Pass : Rho_Material_Pass_Record)
      return Rho_Float
   is
   begin
      return Pass.Shininess;
   end Shininess;

   --------------
   -- Specular --
   --------------

   function Specular
     (Pass : Rho_Material_Pass_Record)
      return Rho.Color.Rho_Color
   is
   begin
      return Pass.Specular;
   end Specular;

   ---------------
   -- Technique --
   ---------------

   function Technique
     (Pass : Rho_Material_Pass_Record'Class)
      return access Rho.Materials.Technique.Rho_Technique_Record'Class
   is
   begin
      return Pass.Technique;
   end Technique;

   -------------
   -- Texture --
   -------------

   function Texture
     (Pass : Rho_Material_Pass_Record'Class)
      return Rho.Texture.Rho_Texture
   is
   begin
      return Pass.Texture;
   end Texture;

   --------------------------
   -- Texture_Address_Mode --
   --------------------------

   function Texture_Address_Mode
     (Pass : Rho_Material_Pass_Record'Class)
      return Rho.Texture.Texture_Address_Mode
   is
   begin
      return Pass.Tex_Address_Mode;
   end Texture_Address_Mode;

   ----------------------------------
   -- Texture_Coordinate_Attribute --
   ----------------------------------

   function Texture_Coordinate_Attribute
     (Pass : Rho_Material_Pass_Record'Class)
      return Rho.Shaders.Values.Rho_Attribute_Value
   is
   begin
      return Pass.Attributes (Texture_Attribute_Index);
   end Texture_Coordinate_Attribute;

end Rho.Materials.Pass;
