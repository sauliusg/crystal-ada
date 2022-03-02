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

with Crystal_Unit_Cell;      use Crystal_Unit_Cell;
with Float_Format_Option;    use Float_Format_Option;
with Xyz_File;               use Xyz_File;
with File_Selector;          use File_Selector;
with User_Error_Messages;    use User_Error_Messages;

procedure Xyzpermute is
   
   Unit_Matrix : Matrix3x3 := ((1.0,0.0,0.0),(0.0,1.0,0.0),(0.0,0.0,1.0));
   
   Permutation : Matrix3x3 := Unit_Matrix;
   
   HELP_PRINTED : exception ;
   
   procedure Print_Help is
      procedure P( S : String ) renames Put_Line;
   begin
      P("Read an XYZ format file (https://en.wikipedia.org/wiki/XYZ_file_format) from");
      P("input files or from STDIN (file name ""-"" as an argument refers to STDIN)");
      P("and permute coordinates and lattice axes according to given permutation.");
      New_Line;
      P("USAGE:");
      P("    " & Command_Name & " --options inputs*.xyz");
      P("    " & Command_Name & " --options < inp.xyz");
      New_Line;
      P("OPTIONS:");
      P("    -p, --permutation ""1 0 0   0 1 0  0 0 1""");
      P("        Specify permutation matrix");
      New_Line;
      P("    -f, --float-format 2,14,3       Specify format for floating point output");
      P("        For Ada, floating point format consists of three numbers:");
      P("        the integer part length, the fraction part length and the exponent length.");
      P("        Specifying exponent part as 0 outputs no exponent at all (as with C '%f' format).");
      New_Line;
      P("    --help                          Print a short help message and exit;");
      raise HELP_PRINTED;
   end;
   
   procedure Process_Options is
      Help_Option : String := "-help -hel -he -h ";
      Float_Format_Option : String := "f: " &
        "-float-format= -float-forma= -float-form= -float-for= " &
        "-float-fo= -float-f= -float= -floa= -flo= -fl= -f= ";
      Permutation_Option : String := "p: " &
        "-permutation= -permutatio= -permutati= -permutat= " &
        "-permuta= -permut= -permu= -perm= -per= -pe= -p=";
   begin
      loop
         case Getopt (Help_Option & Float_Format_Option &
                        Permutation_Option) is
            when 'f' =>
               Parse_Float_Format (Parameter, Integer_Size, 
                                   Fraction_Size, Exponent_Size);
            when 'p' =>
               Parse_Lattice (Parameter, Permutation);
            when '-' =>
               if Index("-permutation", Full_Switch) = 1 then
                  Parse_Lattice (Parameter, Permutation);
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
               File_Lattice : Matrix3x3;
               Lattice_Known : Boolean := False;
            begin
               declare
                  Lattice_Keyword : String := "LATTICE:";
                  Comment : String := To_String (XYZ_Atoms.Comment);
                  
                  Lattice_Keyword_Index : Integer :=
                    Index (Comment, Lattice_Keyword);
                  
                  Parse_Start : Integer :=
                    Lattice_Keyword_Index + Lattice_Keyword'Last;
               begin
                  if Lattice_Keyword_Index > 0 then
                     Parse_Lattice (Comment(Parse_Start..Comment'Last), 
                                    File_Lattice);
                     
                     
                     File_Lattice := Permutation * File_Lattice;
                     Lattice_Known := True;
                  else
                     Warning ("Lattice vectors are not known in the file '" &
                                To_String(File_Name) & "' -- your result " &
                                "may loose track of the unit cell vectors.");
                  end if;
               end;
               
               for I in XYZ_Atoms.Atoms'Range loop
                  XYZ_Atoms.Atoms(I) := Permutation * XYZ_Atoms.Atoms(I);
               end loop;
               
               Put_Atoms (XYZ_Atoms, File_Lattice, Lattice_Known);
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
      
end Xyzpermute;
