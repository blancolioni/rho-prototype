with Ada.Containers.Vectors;

package Rho.Entity.Entity_List is

   type Rho_Entity_List_Interface is interface;

   function Entity (List : Rho_Entity_List_Interface;
                    Name : String)
                    return Rho_Entity
                    is abstract;

   procedure Append (List : in out Rho_Entity_List_Interface;
                     Item : in     Rho_Entity)
   is abstract;

   type Rho_Entity_List is new Rho_Entity_List_Interface with private;

   overriding
   function Entity (List : Rho_Entity_List;
                    Name : String)
                    return Rho_Entity;

   overriding
   procedure Append (List : in out Rho_Entity_List;
                     Item : in     Rho_Entity);

private

   package Entity_Vectors is
     new Ada.Containers.Vectors (Positive, Rho_Entity);

   type Rho_Entity_List is new Rho_Entity_List_Interface with
      record
         Entity_Vector : Entity_Vectors.Vector;
      end record;

end Rho.Entity.Entity_List;
