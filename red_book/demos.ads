with Rho.Scene;

package Demos is

   type Root_Demo_Type is abstract tagged private;

   function Identity (Demo : Root_Demo_Type) return String is abstract;
   function Name (Demo : Root_Demo_Type) return String is abstract;
   function Description (Demo : Root_Demo_Type) return String is abstract;
   function Scene
     (Demo : in out Root_Demo_Type'Class)
      return Rho.Scene.Rho_Scene;

   function Create_Scene
     (Demo : in out Root_Demo_Type)
      return Rho.Scene.Rho_Scene
      is abstract;

   procedure Update
     (Demo : Root_Demo_Type)
   is null;

   type Rho_Demo_Type is access all Root_Demo_Type'Class;

   procedure Load (Demo : in out Root_Demo_Type'Class);

   procedure Add_Demo (Demo : not null access Root_Demo_Type'Class);

   function Demo_Count return Natural;
   function Demo (Index : Positive) return Rho_Demo_Type;
   function Demo (Identity : String) return Rho_Demo_Type;

private

   type Root_Demo_Type is abstract tagged
      record
         Scene : Rho.Scene.Rho_Scene;
      end record;

end Demos;
