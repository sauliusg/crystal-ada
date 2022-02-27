-- ----------------------------------------------------------------------------
-- $Author$
-- $Date$
-- $Revision$
-- $URL$
-- ----------------------------------------------------------------------------

-- Take an XYZ format molecular file
--  (https://en.wikipedia.org/wiki/XYZ_file_format) print it out using
--  Ada format

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

with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Unchecked_Deallocation;

with Float_Format_Option;    use Float_Format_Option;
with Xyz_File;               use Xyz_File;
with File_Selector;          use File_Selector;

procedure XyzFormat is
   
   procedure Process_Options is
      Help_Option : String := "-help -hel -he -h h";
      Float_Format_Option : String := "f: " &
        "-float-format= -float-forma= -float-form= -float-for= " &
        "-float-fo= -float-f= -float= -floa= -flo= -fl= -f=";
   begin
      loop
         case Getopt (Help_Option & " " & Float_Format_Option) is
            when 'f' =>
               Parse_Float_Format (Parameter, Integer_Size, 
                                   Fraction_Size, Exponent_Size);
            when '-' =>
               if Index("-help", Full_Switch) = 1 then
                  Put_Line ("Seen --help");
               elsif Index("-float-format", Full_Switch) = 1 then
                  Parse_Float_Format (Parameter, Integer_Size, 
                                      Fraction_Size, Exponent_Size);
               end if;
            when others =>
               exit;
         end case;
      end loop;   
   end Process_Options;
   
   Current_File : Access_File_Type;
       
   procedure Free_File is
      new Ada.Unchecked_Deallocation(File_Type, Access_File_Type);
   
begin
   
   Process_Options;
   
   declare
      File_Name : Unbounded_String;
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

end XyzFormat;
