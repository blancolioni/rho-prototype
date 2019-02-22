with Gnoga.Application.Multi_Connect;

with Rho_Gnoga_Demo;

procedure Gnoga_Demo is
begin
   Gnoga.Application.Title ("Rho/Gnoga Demo");

   Gnoga.Application.HTML_On_Close ("Application ended.");

   Gnoga.Application.Multi_Connect.Initialize;

   Gnoga.Application.Multi_Connect.On_Connect_Handler
     (Event => Rho_Gnoga_Demo.On_Connect'Access,
      Path  => "default");

   Gnoga.Application.Multi_Connect.Message_Loop;
end Gnoga_Demo;
