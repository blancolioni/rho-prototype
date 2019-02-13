private with Ada.Strings.Unbounded;

with Rho.Fonts;
with Rho.Moveable;
with Rho.Object;
with Rho.Renderable;

package Rho.Text is

   type Rho_Text_Record is
     new Rho.Object.Rho_Object_Record
     and Rho.Moveable.Rho_Moveable
     and Rho.Renderable.Rho_Renderable with private;

   overriding
   procedure Render (Item : in Rho_Text_Record);

   overriding
   procedure Set_Position (Text     : in out Rho_Text_Record;
                           X, Y, Z  : in     Float);

   overriding
   procedure Get_Position (Text     : in     Rho_Text_Record;
                           X, Y, Z  :    out Float);

   type Rho_Text is access all Rho_Text_Record'Class;

   function Create_Text (Name : String;
                         Text : String;
                         Font : Rho.Fonts.Rho_Font)
                         return Rho_Text;

private

   type Rho_Text_Record is
     new Rho.Object.Rho_Object_Record
     and Rho.Moveable.Rho_Moveable
     and Rho.Renderable.Rho_Renderable with
      record
         X, Y, Z : Float;
         Text    : Ada.Strings.Unbounded.Unbounded_String;
         Font    : Rho.Fonts.Rho_Font;
      end record;

end Rho.Text;
