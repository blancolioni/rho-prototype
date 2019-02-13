package body Rho.Renderable is

   ------------
   -- Render --
   ------------

   procedure Render
     (Item   : in out Rho_Renderable'Class;
      Target : not null access
        Rho.Render_Target.Rho_Render_Target_Record'Class)
   is
   begin
      Item.Before_Render (Target);
      Item.Execute_Render (Target);
      Item.After_Render (Target);
   end Render;

end Rho.Renderable;
