private with WL.String_Maps;

private package Rho.Materials.Templates is

   type Rho_Material_Template is tagged limited private;

   procedure Set_Flag
     (Template : in out Rho_Material_Template'Class;
      Name     : String;
      Value    : Boolean);

   procedure Set_Value
     (Template : in out Rho_Material_Template'Class;
      Name     : String;
      Value    : String);

   procedure Set_Indexed_Value
     (Template : in out Rho_Material_Template'Class;
      Name     : String;
      Index    : Positive;
      Value    : String);

   function Execute
     (Template     : Rho_Material_Template'Class;
      Source_Path  : String;
      Write_Result : Boolean := False)
      return String;

   generic
      type Element_Type is tagged private;
      type Element_Access is access all Element_Type'Class;
   package Material_Template_Caches is

      type Cache is tagged private;

      function Get (From     : Cache;
                    Template : Rho_Material_Template'Class)
                    return Element_Access;

      procedure Insert (To       : in out Cache;
                        Template : Rho_Material_Template'Class;
                        Value    : Element_Access);

   private

      package Cache_Maps is
        new WL.String_Maps (Element_Access);

      type Cache is tagged
         record
            Map : Cache_Maps.Map;
         end record;

   end Material_Template_Caches;

private

   package Value_Maps is
     new WL.String_Maps (String);

   type Rho_Material_Template is tagged limited
      record
         Values : Value_Maps.Map;
      end record;

   function Filter_Line
     (Template : Rho_Material_Template;
      Line     : String)
      return String;

   function Apply_Line
     (Template    : Rho_Material_Template;
      Shader_Line : String)
      return String;

end Rho.Materials.Templates;
