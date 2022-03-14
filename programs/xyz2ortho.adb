-- ----------------------------------------------------------------------------
-- $Author$
-- $Date$
-- $Revision$
-- $URL$
-- ----------------------------------------------------------------------------

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
with Ada.Exceptions;         use Ada.Exceptions;

with Crystal_Unit_Cell;      use Crystal_Unit_Cell;
with Float_Format_Option;    use Float_Format_Option;
with Xyz_File;               use Xyz_File;
with File_Selector;          use File_Selector;
with User_Error_Messages;    use User_Error_Messages;

procedure Xyz2ortho is
   
   Unit_Matrix : Matrix3x3 := ((1.0,0.0,0.0),(0.0,1.0,0.0),(0.0,0.0,1.0));
   
   Unit_Cell : Unit_Cell_Type := (others => 0.0);

   Unit_Cell_Given : Boolean := False;
   
   O4F : Matrix3x3;
   
   HELP_PRINTED : exception ;
   UNKNOWN_UNIT_CELL : exception ;
   
   Unknown_Unit_Cell_Status : 
     constant Ada.Command_Line.Exit_Status := 1;
   
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
      New_Line;
      P("    -l, --lattice ""0.1 0 0  0 0.2 0  0 0 0.15""");
      P("        Specify unit cell vectors in ortho frame for conversion");
      New_Line;
      P("    -f, --float-format 2,14,3       Specify format for floating point output");
      P("        For Ada, floating point format consists of three numbers:");
      P("        the integer part length, the fraction part length and the exponent length.");
      P("        Specifying exponent part as 0 outputs no exponent at all (as with C '%f' format).");
      New_Line;
      P("    -H, --human-readable            Use format 8,6,0 for better human readability");
      P("    -M, --machine-readable          Use format 2,14,3 to maintain precision");
      New_Line;
      P("    --help                          Print a short help message and exit;");
      raise HELP_PRINTED;
   end;
   
   procedure Process_Options is
      Help_Option : String := "-help -hel -he -h ";
      Cell_Option : String := "c: -cell= -cel= -ce= -c= ";
      Float_Format_Option : String := "f: " &
        "-float-format= -float-forma= -float-form= -float-for= " &
        "-float-fo= -float-f= -float= -floa= -flo= -fl= -f= ";
      Human_Readable_Option : String := "H " &
        "-human-readable -human-readabl -human-readab -human-reada " &
        "-human-read -human-rea -human-re -human-r -human -huma " &
        "-hum -hu -h ";
      Machine_Readable_Option : String := "M " &
        "-machine-readable -machine-readabl -machine-readab -machine-reada " &
        "-machine-read -machine-rea -machine-re -machine-r -machine -machin " &
        "-machi -mach -mac -ma -m ";
      Lattice_Option : String := "l: " &
        "-lattice= -lattic= -latti= -latt= -lat= -la= -l=";
   begin
      loop
         case Getopt (Help_Option & Cell_Option & Float_Format_Option &
                        Human_Readable_Option & Machine_Readable_Option &
                        Lattice_Option) is
            when 'f' =>
               Parse_Float_Format (Parameter, Integer_Size, 
                                   Fraction_Size, Exponent_Size);
            when 'H' =>
               Integer_Size := 8;
               Fraction_Size := 6;
               Exponent_Size := 0;
            when 'M' =>
               Integer_Size := 2;
               Fraction_Size := 14;
               Exponent_Size := 3;
            when 'c' =>
               Parse_Unit_Cell (Parameter, Unit_Cell);
               O4F := Matrix_Ortho_From_Fract (Unit_Cell);
               Unit_Cell_Given := True;
            when 'l' =>
               declare
                  Lattice_Vectors : Matrix3x3;
               begin
                  Parse_Lattice (Parameter, Lattice_Vectors);
                  O4F := Lattice_Vectors;
                  Unit_Cell := Unit_Cell_From_Vectors (Lattice_Vectors);
                  Unit_Cell_Given := True;
               end;
            when '-' =>
               if Index("-cell", Full_Switch) = 1 then
                  Parse_Unit_Cell (Parameter, Unit_Cell);
                  O4F := Matrix_Ortho_From_Fract (Unit_Cell);
                  Unit_Cell_Given := True;
               elsif Index("-lattice", Full_Switch) = 1 then
                  declare
                     Lattice_Vectors : Matrix3x3;
                  begin
                     Parse_Lattice (Parameter, Lattice_Vectors);
                     O4F := Lattice_Vectors;
                     Unit_Cell := Unit_Cell_From_Vectors (Lattice_Vectors);
                     Unit_Cell_Given := True;
                  end;
               elsif Index("-float-format", Full_Switch) = 1 then
                  Parse_Float_Format (Parameter, Integer_Size, 
                                      Fraction_Size, Exponent_Size);
               elsif Index("-human-readable", Full_Switch) = 1 then
                  Integer_Size := 8;
                  Fraction_Size := 6;
                  Exponent_Size := 0;
               elsif Index("-machine-readable", Full_Switch) = 1 then
                  Integer_Size := 2;
                  Fraction_Size := 14;
                  Exponent_Size := 3;
               elsif Index("-help", Full_Switch) = 1 then
                  Print_Help;
               end if;
            when others =>
               exit;
         end case;
      end loop;   
   end Process_Options;
   
   procedure Put_Atoms ( Molecule : XYZ_File_Atoms;
                         Lattice : Matrix3x3;
                         Unit_Cell_Known : Boolean) is
   begin
      Xyz_File.Put_Atoms ( Molecule, Lattice, Unit_Cell_Known,
                           Integer_Size, Fraction_Size, Exponent_Size );
   end;
   
   File_Name : Unbounded_String;
   Current_File : Access_File_Type;

begin
   
   Process_Options;
   
   declare
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
               Unit_Cell_Known : Boolean := Unit_Cell_Given;
            begin
               declare
                  Cell_Keyword : String := "CELL:";
                  Comment : String := To_String (XYZ_Atoms.Comment);
                  Cell_Keyword_Index : Integer := Index (Comment, Cell_Keyword);
                  Parse_Start : Integer := Cell_Keyword_Index + Cell_Keyword'Last;
                  File_Unit_Cell : Unit_Cell_Type;
               begin
                  if Cell_Keyword_Index > 0 then
                     Parse_Unit_Cell (Comment(Parse_Start..Comment'Last),
                                      File_Unit_Cell);
                     
                     if Unit_Cell_Given then
                        if Unit_Cell /= File_Unit_Cell then
                           Warning ("Unit cell given in the file '" &
                                      To_String (File_Name) &
                                      "' is different from the one " &
                                      "provided on the command line. " &
                                      "Taking unit cell from the command line");
                        end if;
                     else
                        O4F := Matrix_Ortho_From_Fract (File_Unit_Cell);
                        Unit_Cell := File_Unit_Cell;
                     end if;
                     
                     Unit_Cell_Known := True;
                  end if;
               end;
               
               if not Unit_Cell_Known then
                  raise UNKNOWN_UNIT_CELL with To_String (XYZ_Atoms.Comment);
               end if;
               
               for I in XYZ_Atoms.Atoms'Range loop
                  XYZ_Atoms.Atoms(I) := O4F * XYZ_Atoms.Atoms(I);
               end loop;
               
               Put_Atoms (XYZ_Atoms, O4F, Unit_Cell_Known);
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
         
   when Exception_Occurence : UNKNOWN_UNIT_CELL =>
      declare
         Message : String := Exception_Message(Exception_Occurence);
         Msg_String : String :=
           (if Message'Last < 20 then Message else Message(1..20) & " ...");
      begin
         Error ("unit cell not known when processing file " &
                  "'" & To_String(File_Name) & "' " &
                  "('" & Msg_String & "')",
                Unknown_Unit_Cell_Status);
      end;
      
end Xyz2ortho;
