private with Rho.String_Maps;

with Rho.Value;

package Rho.Properties is

   type Rho_Property_Container_Interface is interface;

   type Rho_Property_Container is
     access all Rho_Property_Container_Interface'Class;

   function Has_Property
     (Container : Rho_Property_Container_Interface;
      Name      : String)
      return Boolean
      is abstract;

   function Get_Property
     (Container : Rho_Property_Container_Interface;
      Name      : String)
      return Rho.Value.Rho_Value
      is abstract;

   procedure Set_Property
     (Container : in out Rho_Property_Container_Interface;
      Name      : String;
      Value     : Rho.Value.Rho_Value)
   is abstract;

   type Rho_Property_Container_Record is
     new Rho_Property_Container_Interface with private;

   overriding function Has_Property
     (Container : Rho_Property_Container_Record;
      Name      : String)
      return Boolean;

   overriding function Get_Property
     (Container : Rho_Property_Container_Record;
      Name      : String)
      return Rho.Value.Rho_Value;

   overriding procedure Set_Property
     (Container : in out Rho_Property_Container_Record;
      Name      : String;
      Value     : Rho.Value.Rho_Value);

private

   package Property_Maps is
     new Rho.String_Maps (Rho.Value.Rho_Value, Rho.Value."=");

   type Rho_Property_Container_Record is
     new Rho_Property_Container_Interface with
      record
         Map : Property_Maps.Map;
      end record;

end Rho.Properties;
