with Text_Io;                use Text_Io;
with Ada.Long_Float_Text_IO; use Ada.Long_Float_Text_IO;
with Ada.Command_Line;       use Ada.Command_Line;

with Crystal_Unit_Cell; use Crystal_Unit_Cell;

procedure Invert is
   -- Calculate and printe determinant of a 3x3 matrix. Matrix is
   --  given as a set of 9 numbers on the command line

   -- Example:
   --  det 1 0 0  0 1 0  0 0 1
   
   procedure Put (M : Matrix3x3) is
   begin
      for I in M'Range(1) loop
         for J in M'Range(2) loop
            Put (M(I,J));
            Put (" ");
         end loop;
         New_Line;
      end loop;
   end;
   
   N : Integer := 1;   
   M : Matrix3x3;
   
begin
   
   for I in M'Range(1) loop
      for J in M'Range(2) loop
         declare
            Position : Integer := 1;
         begin
            Get (Argument(N), M(I,J), Position);
         end;
         N := N + 1;
      end loop;
   end loop;
   
   Put (Invert (M));
   
   New_Line;
   
end Invert;
