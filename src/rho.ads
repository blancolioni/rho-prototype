package Rho is

   pragma Pure (Rho);

   type Rho_Float is new Long_Float range Long_Float'Range;
   subtype Unit_Float is Rho_Float range 0.0 .. 1.0;
   subtype Signed_Unit_Float is Rho_Float range -1.0 .. 1.0;
   subtype Non_Negative_Float is Rho_Float range 0.0 .. Rho_Float'Last;

   function Clamp (X, Low, High : Rho_Float) return Rho_Float
   is (Rho_Float'Max (Rho_Float'Min (X, High), Low));

   function Clamp (X : Rho_Float) return Unit_Float
   is (Clamp (X, 0.0, 1.0));

   type Rho_Shader_Location_Type is (Attribute_Value, Uniform_Value);
   type Rho_Shader_Location_Id is new Integer;
   type Rho_Attribute_Id is new Rho_Shader_Location_Id;
   type Rho_Uniform_Id is new Rho_Shader_Location_Id;

   type Rho_Shader_Id is new Natural;
   type Rho_Program_Id is new Natural;

   type Rho_Vertex_Array_Id is new Natural;

end Rho;
