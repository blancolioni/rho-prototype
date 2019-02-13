with Ada.Containers.Vectors;
with WL.String_Maps;

package body Demos is

   package Demo_Maps is
     new WL.String_Maps (Positive);

   package Demo_Vectors is
     new Ada.Containers.Vectors (Positive, Rho_Demo_Type);

   Demo_Map    : Demo_Maps.Map;
   Demo_Vector : Demo_Vectors.Vector;

   --------------
   -- Add_Demo --
   --------------

   procedure Add_Demo (Demo : not null access Root_Demo_Type'Class) is
   begin
      Demo_Vector.Append (Rho_Demo_Type (Demo));
      Demo_Map.Insert (Demo.Identity, Demo_Vector.Last_Index);
   end Add_Demo;

   ----------
   -- Demo --
   ----------

   function Demo (Index : Positive) return Rho_Demo_Type is
   begin
      return Demo_Vector (Index);
   end Demo;

   ----------
   -- Demo --
   ----------

   function Demo (Identity : String) return Rho_Demo_Type is
   begin
      return Demo_Vector (Demo_Map (Identity));
   end Demo;

   ----------------
   -- Demo_Count --
   ----------------

   function Demo_Count return Natural is
   begin
      return Demo_Vector.Last_Index;
   end Demo_Count;

   ----------
   -- Load --
   ----------

   procedure Load (Demo : in out Root_Demo_Type'Class) is null;

   -----------
   -- Scene --
   -----------

   function Scene
     (Demo : in out Root_Demo_Type'Class)
      return Rho.Scene.Rho_Scene
   is
      use type Rho.Scene.Rho_Scene;
   begin
      if Demo.Scene = null then
         Demo.Scene := Demo.Create_Scene;
      end if;
      return Demo.Scene;
   end Scene;

end Demos;
