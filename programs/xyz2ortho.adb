-- ----------------------------------------------------------------------------
-- $Author$
-- $Date$
-- $Revision$
-- $URL$
-- ----------------------------------------------------------------------------

-- Take an XYZ format molecular file
--  (https://en.wikipedia.org/wiki/XYZ_file_format) and convert
--  coordinates to orthogonal ones, assuming that the file contained
--  fractional coordinates. Unit cell constants can be given as
--  command line option parameters.

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

procedure Xyz2ortho is
   
   Unit_Cell : Unit_Cell_Type := (others => 0.0);

   Unit_Cell_Given : Boolean := False;
   
   HELP_PRINTED : exception ;
   
   procedure Print_Help is
      procedure P( S : String ) renames Put_Line;
   begin
      P("Read an XYZ format file (https://en.wikipedia.org/wiki/XYZ_file_format) from");
      P("input files or from STDIN (file name ""-"" as an argument refers to STDIN)");
      P("and convert coordinates to orthogonal ones, assuming that the file contained");
      P("fractional coordinates.");
      New_Line;
      P("USAGE:");
      P("    " & Command_Name & " --options inputs*.xyz");
      P("    " & Command_Name & " --options < inp.xyz");
      New_Line;
      P("OPTIONS:");
      P("    -c, --cell ""10 10 10 90 90 90""  Specify unit cell for conversion");
      P("    --help                          Print a short help message and exit;");
      raise HELP_PRINTED;
   end;
   
   procedure Process_Options is
      Help_Option : String := "-help -hel -he -h ";
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
                  Print_Help;
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

   O4F : Matrix3x3;
   
begin
   
   Process_Options;
   
   O4F := Matrix_Ortho_From_Fract (Unit_Cell);
   
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
               XYZ_Atoms : XYZ_File_Atoms := Load_Atoms (Current_File.all);
            begin
               for I in XYZ_Atoms.Atoms'Range loop
                  XYZ_Atoms.Atoms(I) := O4F * XYZ_Atoms.Atoms(I);
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

exception
   when HELP_PRINTED => null;
         
end Xyz2ortho;
