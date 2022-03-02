package Float_Format_Option is
   
   -- Format number for printing out floating point numbers:
   Integer_Size : Integer := 2;
   Fraction_Size : Integer := 14;
   Exponent_Size : Integer := 3;
   
   procedure Parse_Float_Format ( Line : String;
                                  Integer_Size : in out Integer;
                                  Fraction_Size : in out Integer;
                                  Exponent_Size : in out Integer
                                );
      
end Float_Format_Option;
