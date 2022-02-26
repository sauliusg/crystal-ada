
package Crystal_Unit_Cell is
   
   type Unit_Cell_Type is array (1..6) of Long_Float;
   
   procedure Parse_Unit_Cell ( Line : String; Cell : in out Unit_Cell_Type );
      
   type Matrix3x3 is array (1..3,1..3) of Long_Float;
   
   function Matrix_Ortho_From_Fract ( Cell : Unit_Cell_Type ) return Matrix3x3;   
   function Matrix_Fract_From_Ortho ( Cell : in Unit_Cell_Type ) return Matrix3x3;

end;
