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

with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Unchecked_Deallocation;

with Float_Format_Option;    use Float_Format_Option;
with Xyz_File;               use Xyz_File;
with File_Name_Selector;     use File_Name_Selector;
with User_Error_Messages;    use User_Error_Messages;

with Project_Version;        use Project_Version;

procedure XyzFormat is
   
   HELP_PRINTED : exception;
   VERSION_PRINTED : exception;
   
   Unhandled_Exception_Status:
     constant Ada.Command_Line.Exit_Status := 255;

   procedure Print_Help is
      procedure P( S : String ) renames Put_Line;
   begin
      P("Take an XYZ format molecular file (https://en.wikipedia.org/wiki/XYZ_file_format)");
      P("and print it out using Ada format");
      New_Line;
      P("USAGE:");
      P("    " & Command_Name & " --options inputs*.xyz");
      P("    " & Command_Name & " --options < inp.xyz");
      New_Line;
      P("OPTIONS:");
      P("    -f, --float-format 2,14,3       Specify format for floating point output");
      P("        For Ada, floating point format consists of three numbers:");
      P("        the integer part length, the fraction part length and the exponent length.");
      P("        Specifying exponent part as 0 outputs no exponent at all (as with C '%f' format).");
      New_Line;
      P("    -H, --human-readable            Use format 8,6,0 for better human readability");
      P("    -M, --machine-readable          Use format 2,14,3 to maintain precision");
      New_Line;
      P("    --help                          Print a short help message and exit;");
      P("    --version                       Print program project version and exit;");
      raise HELP_PRINTED;
   end;
   
   procedure Process_Options is
      Help_Option : String := "-help -hel -he -h h ";
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
      Version_Option : String := "-version -versio -versi -vers -ver -ve -v";
   begin
      loop
         case Getopt (Help_Option & Float_Format_Option &
                        Human_Readable_Option & Machine_Readable_Option &
                        Version_Option) is
            when 'h' =>
               Print_Help;
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
            when '-' =>
               if Index("-help", Full_Switch) = 1 then
                  Print_Help;
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
               elsif Index("-version", Full_Switch) = 1 then
                  Put_Line (Command_Name & " version " & Version);
                  raise VERSION_PRINTED;
               end if;
            when others =>
               exit;
         end case;
      end loop;   
   end Process_Options;
   
   Current_File : Access_File_Type;
       
   procedure Free_File is
      new Ada.Unchecked_Deallocation(File_Type, Access_File_Type);
   
   File_Name : Unbounded_String;
   
begin
   
   Process_Options;
   
   declare
      Is_File_Processed : Boolean := False;   
      Is_Last_File : Boolean := False;   
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
               Xyz_File.Put_Atoms ( XYZ_Atoms, Integer_Size,
                                    Fraction_Size, Exponent_Size );
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
   when VERSION_PRINTED => null;
      
   when Exception_Occurence : others =>
      declare
         Message : String := Exception_Message (Exception_Occurence);
         Exception_Name_String : String := Exception_Name (Exception_Occurence);
      begin
         Error ((if File_Name /= "" then "in file '" & To_String (File_Name) & "': " else "") & 
           Exception_Name_String & ": " & Message,
           Unhandled_Exception_Status);
      end;

end XyzFormat;
