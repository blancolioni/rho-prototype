with Rho.Render_Target;

package Rho.Renderable is

   type Rho_Renderable is interface;

   function Loaded (Item : Rho_Renderable) return Boolean
                    is abstract;

   procedure Load (Item     : in out Rho_Renderable)
   is null;

   procedure Render
     (Item   : in out Rho_Renderable'Class;
      Target : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class);

   procedure Before_Render
     (Item   : in out Rho_Renderable;
      Target : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class)
   is null;

   procedure Execute_Render
     (Item   : in out Rho_Renderable;
      Target : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class)
   is abstract;

   procedure After_Render
     (Item   : in out Rho_Renderable;
      Target : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class)
   is null;

end Rho.Renderable;
