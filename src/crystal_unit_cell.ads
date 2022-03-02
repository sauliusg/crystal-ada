
package Crystal_Unit_Cell is
   
   type Unit_Cell_Type is array (1..6) of Long_Float;
   
   type Matrix3x3 is array (1..3,1..3) of Long_Float;
   
   function "*" (M1, M2 : Matrix3x3) return Matrix3x3;
   
   procedure Parse_Unit_Cell ( Line : String; Cell : in out Unit_Cell_Type );
   procedure Parse_Lattice ( Line : String; Lattice : in out Matrix3x3 );
      
   function Unit_Cell_From_Vectors (M : Matrix3x3) return Unit_Cell_Type;
   function Matrix_Ortho_From_Fract ( Cell : Unit_Cell_Type ) return Matrix3x3;   
   function Matrix_Fract_From_Ortho ( Cell : in Unit_Cell_Type ) return Matrix3x3;

   function Det (M : Matrix3x3) return Long_Float;
   function Invert (M : Matrix3x3) return Matrix3x3;
end;
