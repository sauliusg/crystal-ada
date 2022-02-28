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
   
   procedure Parse_Lattice ( Line : String; Lattice : in out Matrix3x3 ) is
      Position : Integer := Line'First - 1;
      Line_Without_Commas : String := Line;
      
      procedure Replace (S : in out String; Chr_From, Chr_To : Character)
        renames String_Functions.Replace;
   
   begin
      Replace (Line_Without_Commas, ',', ' ');
      for J in Lattice'Range(2) loop
         for I in Lattice'Range(1) loop
            Get (Line_Without_Commas(Position+1..Line'Last),
                 Lattice(I,J), Position);
         end loop;
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
               ( 1.0/A, -(1.0/A)*CTG, (CA*CG-CB)/(A*SG*D) ),
               (   0.0,   1.0/(B*SG), (CB*CG-CA)/(B*SG*D) ), 
               (   0.0,          0.0,            SG/(C*D) )
             );
   end;
   
   function Det (M : Matrix3x3) return Long_Float is
      ( M(1,1)*M(2,2)*M(3,3) + M(1,2)*M(2,3)*M(3,1) + M(2,1)*M(3,2)*M(1,3) -
          M(1,3)*M(2,2)*M(3,1) - M(2,1)*M(1,2)*M(3,3) - M(1,1)*M(2,3)*M(3,2) );
      
   function Invert (M : Matrix3x3) return Matrix3x3 is
      D : Long_Float := Det (M);
      
      function Minor (P, Q : Integer) return Long_Float is
         A : array (1..2,1..2) of Long_Float;
         K, L : Integer;
         Coeff : Long_Float;
      begin
         K := 1; L := 1;
         for I in M'Range(1) loop
            if I /= P then 
               for J in M'Range(2) loop
                  if J /= Q then
                     A(K,L) := M(I,J);
                     L := L + 1;
                  end if;
               end loop;
               L := 1;
               K := K + 1;
            end if;
         end loop;
         if (P + Q) mod 2 = 0 then
            Coeff := 1.0;
         else
            Coeff := -1.0;
         end if;
         return Coeff * (A(1,1)*A(2,2) - A(1,2)*A(2,1));
      end;
      
   begin
      return (
              ( Minor(1,1)/D, Minor(1,2)/D, Minor(1,3)/D ),
              ( Minor(2,1)/D, Minor(2,2)/D, Minor(2,3)/D ),
              ( Minor(3,1)/D, Minor(3,2)/D, Minor(3,3)/D )
             );
   end;
   
end;
