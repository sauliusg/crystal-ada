with Ada.Long_Float_Text_IO; use Ada.Long_Float_Text_IO;
with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;

with String_Functions;

package body Crystal_Unit_Cell is
   
   package Long_Float_Elementary_Functions is 
      new Ada.Numerics.Generic_Elementary_Functions (Long_Float);
   
   use Long_Float_Elementary_Functions;
   
   procedure Parse_Unit_Cell ( Line : String; Cell : in out Unit_Cell_Type ) is
      Position : Integer := Line'First - 1;
      Line_Without_Commas : String := Line;
      
      procedure Replace (S : in out String; Chr_From, Chr_To : Character)
        renames String_Functions.Replace;
   
   begin
      Replace (Line_Without_Commas, ',', ' ');
      for I in Cell'Range loop
         Get (Line_Without_Commas(Position+1..Line'Last), Cell(I), Position);
      end loop;
   end;
   
   function Matrix_Ortho_From_Fract ( Cell : Unit_Cell_Type ) 
                                    return Matrix3x3
   is
      A : Long_Float := Cell(1);
      B : Long_Float := Cell(2);
      C : Long_Float := Cell(3);
      Alpha : Long_Float := Cell(4) * Ada.Numerics.Pi / 180.0; -- in radians;
      Beta  : Long_Float := Cell(5) * Ada.Numerics.Pi / 180.0;
      Gamma : Long_Float := Cell(6) * Ada.Numerics.Pi / 180.0;
      CA : Long_Float := Cos(Alpha);
      CB : Long_Float := Cos(Beta);
      CG : Long_Float := Cos(Gamma);
      SG : Long_Float := Sin(Gamma);
   begin
      return ( 
               (   A, B * CG, C * CB                ),
               ( 0.0, B * SG, C * (CA - CB*CG) / SG ), 
               ( 0.0,    0.0, 
                 C * Sqrt (SG*SG - CA*CA - CB*CB + 2.0*CA*CB*CG)/SG)
             );
   end;
   
   function Matrix_Fract_From_Ortho ( Cell : in Unit_Cell_Type ) 
                                    return Matrix3x3
   is
      A : Long_Float := Cell(1);
      B : Long_Float := Cell(2);
      C : Long_Float := Cell(3);
      Alpha : Long_Float := Cell(4) * Ada.Numerics.Pi / 180.0; -- in radians;
      Beta  : Long_Float := Cell(5) * Ada.Numerics.Pi / 180.0;
      Gamma : Long_Float := Cell(6) * Ada.Numerics.Pi / 180.0;
      CA : Long_Float := Cos(Alpha);
      CB : Long_Float := Cos(Beta);
      CG : Long_Float := Cos(Gamma);
      SG : Long_Float := Sin(Gamma);
      CTG : Long_Float := CG/SG;
      D : Long_Float := Sqrt(SG**2 - CB**2 - CA**2 + 2.0*CA*CB*CG);
   begin
      return ( 
               ( 1.0/A, -(1.0/A)*CTG,     (CA*CG-CB)/(A*D) ),
               (   0.0,   1.0/(B*SG), -(CA-CB*CG)/(B*SG*D) ), 
               (   0.0,          0.0,             SG/(C*D) )
             );
   end;
   
end;
