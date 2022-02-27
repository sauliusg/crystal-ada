with Ada.Integer_Text_Io;    use Ada.Integer_Text_Io;

with String_Functions;

package body Float_Format_Option is
   
   procedure Parse_Float_Format ( Line : String;
                                  Integer_Size : in out Integer;
                                  Fraction_Size : in out Integer;
                                  Exponent_Size : in out Integer
                                ) is
      Line_Without_Commas : String := Line;
      Position : Integer := Line_Without_Commas'First - 1;
      
      procedure Replace (S : in out String; Chr_From, Chr_To : Character)
        renames String_Functions.Replace;
      
   begin
      Replace (Line_Without_Commas, ',', ' ');
      Get (Line_Without_Commas(Position+1..Line_Without_Commas'Last),
           Integer_Size, Position );
      if Position < Line_Without_Commas'Last then
         Get (Line_Without_Commas(Position+1..Line_Without_Commas'Last),
              Fraction_Size, Position );
      end if;
      if Position < Line_Without_Commas'Last then
         Get (Line_Without_Commas(POsition+1..Line_Without_Commas'Last),
              Exponent_Size, Position );
      end if;
   end;
   
end Float_Format_Option;
