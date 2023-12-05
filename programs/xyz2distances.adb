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
-- with GNAT.Command_Line;      use GNAT.Command_Line;
with Ada.Exceptions;         use Ada.Exceptions;

with Crystal_Unit_Cell;      use Crystal_Unit_Cell;
with Float_Format_Option;    use Float_Format_Option;
with Xyz_File;               use Xyz_File;
-- with File_Selector;          use File_Selector;
with User_Error_Messages;    use User_Error_Messages;

with Option_Processor;       use Option_Processor;
with File_Selector;          use File_Selector;

with Project_Version;        use Project_Version;

procedure Xyz2Distances is
   
   procedure Help is
      procedure P (S : String) renames Put_Line;
   begin
      P ("Convert an XYZ file into the distance matrix, output the CSV");
   end;
   
   Options : Option_Array :=
     (
      1 => Help_Option ("", "--help")
     );
   
   Current_File : File_Selector.File_Access;

begin
   
   Process_Options (Options, Help'Access);
   
   -- FI is File Index:
   for FI in 1 .. File_Name_Count loop
      Current_File := Select_File (FI);
      
      while not End_Of_File (Current_File.all) loop
         declare
            XYZ_Atoms : XYZ_File_Atoms := Load_Atoms (Current_File.all);
         begin
            Put_Line (XYZ_Atoms.Size'Image & " " & Get_File_Name(FI));
         end;    
      end loop;

      Close (Current_File);
   end loop;
   
exception
   when HELP_PRINTED => null;
      
   -- when VERSION_PRINTED => null;
      
   -- when Exception_Occurence : UNKNOWN_UNIT_CELL =>
   --    declare
   --       Message : String := Exception_Message(Exception_Occurence);
   --       Msg_String : String :=
   --         (if Message'Last < 20 then Message else Message(1..20) & " ...");
   --    begin
   --       Error ("lattice vectors not known when processing file " &
   --                "'" & To_String(File_Name) & "' " &
   --                "('" & Msg_String & "')", 
   --              Unknown_Unit_Cell_Status);
   --    end;
                  
end Xyz2Distances;
