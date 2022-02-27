-- ----------------------------------------------------------------------------
-- $Author$
-- $Date$
-- $Revision$
-- $URL$
-- ----------------------------------------------------------------------------

-- Take an XYZ format molecular file
--  (https://en.wikipedia.org/wiki/XYZ_file_format) and convert
--  orthogonal coordinates to fractional ones. Unit cell constants can
--  be given as command line option parameters.

-- Option processing example taken from:
-- https://www.adacore.com/gems/gem-138-gnatcoll.command-line
-- S.G. 2021-12-26

with Text_Io;                use Text_Io;
with Ada.Integer_Text_Io;    use Ada.Integer_Text_Io;
with Ada.Long_Float_Text_IO; use Ada.Long_Float_Text_IO;
with Ada.Command_Line;       use Ada.Command_Line;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;      use Ada.Strings.Fixed;
with GNAT.Command_Line;      use GNAT.Command_Line;

with Crystal_Unit_Cell;      use Crystal_Unit_Cell;
with Float_Format_Option;    use Float_Format_Option;
with Xyz_File;               use Xyz_File;
with File_Selector;          use File_Selector;

procedure Xyz2fract is
   
   Unit_Cell : Unit_Cell_Type := (others => 0.0);

   Unit_Cell_Given : Boolean := False;
   
   procedure Process_Options is
      Help_Option : String := "-help -hel -he -h h ";
      Cell_Option : String := "c: -cell= -cel= -ce= -c= ";
      Float_Format_Option : String := "f: " &
        "-float-format= -float-forma= -float-form= -float-for= " &
        "-float-fo= -float-f= -float= -floa= -flo= -fl= -f= ";
   begin
      loop
         case Getopt (Help_Option & Cell_Option & Float_Format_Option) is
            when 'f' =>
               Parse_Float_Format (Parameter, Integer_Size, 
                                   Fraction_Size, Exponent_Size);
            when 'c' =>
               -- Put_Line("Seen '-c' with parameter '" & Parameter & "'") ;
               Parse_Unit_Cell (Parameter, Unit_Cell);
               Unit_Cell_Given := True;
            when '-' =>
               if Index("-cell", Full_Switch) = 1 then
                  -- Put_Line ("Seen --cell with arg='" & Parameter & "'");
                  Parse_Unit_Cell (Parameter, Unit_Cell);
                  Unit_Cell_Given := True;
               elsif Index("-float-format", Full_Switch) = 1 then
                  Parse_Float_Format (Parameter, Integer_Size, 
                                      Fraction_Size, Exponent_Size);
               elsif Index("-help", Full_Switch) = 1 then
                  Put_Line ("Seen --help");
               end if;
            when others =>
               exit;
         end case;
      end loop;   
   end Process_Options;
   
   procedure Put_Atoms ( Molecule : XYZ_File_Atoms ) is
   begin
      Xyz_File.Put_Atoms ( Molecule, Unit_Cell, Unit_Cell_Given,
                           Integer_Size, Fraction_Size, Exponent_Size );
   end;
   
   Current_File : Access_File_Type;

   F4O : Matrix3x3;
   
begin
   
   Process_Options;
   
   F4O := Matrix_Fract_From_Ortho (Unit_Cell);
   
   declare
      File_Name : Unbounded_String;
      Is_File_Processed : Boolean := False;
      Is_Last_File: Boolean;
   begin
      loop
         File_Name := To_Unbounded_String(Get_Argument);
         Select_File (File_Name, Is_File_Processed => Is_File_Processed,
                      Is_Last_File => Is_Last_File,
                      Current_File => Current_File);
         exit when Is_Last_File;

         while not End_Of_File (Current_File.all) loop
            declare
               N : Integer := Integer'Value (Get_Line (Current_File.all));
               Comment : String := Get_Line (Current_File.all);
               XYZ_Atoms : XYZ_File_Atoms (N);
            begin
               XYZ_Atoms.Comment := To_Unbounded_String (Comment);
               for I in 1..N loop
                  Get (Current_File.all, XYZ_Atoms.Atoms(I).Atom_Type);
                  Get (Current_File.all, XYZ_Atoms.Atoms(I).X, Width => 0);
                  Get (Current_File.all, XYZ_Atoms.Atoms(I).Y, Width => 0);
                  Get (Current_File.all, XYZ_Atoms.Atoms(I).Z, Width => 0);
               end loop;
               -- Read the remaining EOL marker:
               Skip_Line (Current_File.all);
               
               for I in XYZ_Atoms.Atoms'Range loop
                  XYZ_Atoms.Atoms(I) := F4O * XYZ_Atoms.Atoms(I);
               end loop;
               
               Put_Atoms (XYZ_Atoms);
            end;
         end loop;
         
         if Is_Open (Current_File.all) then
            Close (Current_File.all);
         end if;
         Free_File (Current_File);
         
      end loop;
      
   end;

end Xyz2fract;
