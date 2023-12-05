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
   
   Current_File : File_Selector.File_Access;

begin
   
   Current_File := null;
   
--    Process_Options;
--    
--    declare
--       Is_File_Processed : Boolean := False;
--       Is_Last_File: Boolean;
--    begin
--       loop
--          File_Name := To_Unbounded_String(Get_Argument);
--          Select_File (File_Name, Is_File_Processed => Is_File_Processed,
--                       Is_Last_File => Is_Last_File,
--                       Current_File => Current_File);
--          exit when Is_Last_File;
-- 
--          while not End_Of_File (Current_File.all) loop
--             declare
--                XYZ_Atoms : XYZ_File_Atoms := Load_Atoms (Current_File.all);
--                Unit_Cell_Known : Boolean := Unit_Cell_Given;
--             begin
--                declare
--                   Lattice_Keyword : String := "LATTICE:";
--                   Comment : String := To_String (XYZ_Atoms.Comment);
--                   Lattice_Keyword_Index : Integer := Index (Comment, Lattice_Keyword);
--                   Parse_Start : Integer := Lattice_Keyword_Index + Lattice_Keyword'Last;
--                   Cell_Vectors : Matrix3x3;
--                begin
--                   if Lattice_Keyword_Index > 0 then
--                      Parse_Lattice (Comment(Parse_Start..Comment'Last), Cell_Vectors);
--                         
--                      if Unit_Cell_Given then
--                         if Unit_Cell /= Unit_Cell_From_Vectors (Cell_Vectors) then
--                            Warning ("unit cell given in the file '" &
--                                       To_String (File_Name) &
--                                       "' is different from the one " &
--                                       "provided on the command line. " &
--                                       "Taking unit cell from the command line.");
--                         end if;
--                      else
--                         F4O := Invert (Cell_Vectors);
--                         Unit_Cell := Unit_Cell_From_Vectors (Cell_Vectors);
--                      end if;
--                      
--                      Unit_Cell_Known := True;
--                   end if;
--                end;
--                
--                if not Unit_Cell_Known then
--                   raise UNKNOWN_UNIT_CELL with To_String (XYZ_Atoms.Comment);
--                end if;
--                
--                for I in XYZ_Atoms.Atoms'Range loop
--                   XYZ_Atoms.Atoms(I) := F4O * XYZ_Atoms.Atoms(I);
--                end loop;
--                
--                Put_Atoms (XYZ_Atoms, Unit_Cell, Unit_Cell_Known);
--             end;
--          end loop;
--          
--          if Is_Open (Current_File.all) then
--             Close (Current_File.all);
--          end if;
--          Free_File (Current_File);
--          
--       end loop;
--       
--   end;
   
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
