-- ----------------------------------------------------------------------------
-- $Author$
-- $Date$
-- $Revision$
-- $URL$
-- ----------------------------------------------------------------------------

pragma Ada_2022;

with Text_Io;                use Text_Io;
with Ada.Integer_Text_Io;    use Ada.Integer_Text_Io;
with Ada.Long_Float_Text_IO; use Ada.Long_Float_Text_IO;
with Ada.Command_Line;       use Ada.Command_Line;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;      use Ada.Strings.Fixed;
with Ada.Exceptions;         use Ada.Exceptions;

with Crystal_Unit_Cell;      use Crystal_Unit_Cell;
with Float_Format_Option;    use Float_Format_Option;
with Xyz_File;               use Xyz_File;
with User_Error_Messages;    use User_Error_Messages;

with Option_Processor;       use Option_Processor;
with File_Selector;          use File_Selector;

with Ada.Numerics.Long_Elementary_Functions;

with Atomic_Numbers;         use Atomic_Numbers;

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
   
   function Hypot (X1, Y1, Z1, X2, Y2, Z2 : Long_Float) return Long_Float is
   begin
      return (X1 - X2)**2 + (Y1 - Y2)**2 + (Z1 - Z2)**2;
   end;
   
   function Sqrt (X : Long_Float) return Long_Float renames
     Ada.Numerics.Long_Elementary_Functions.Sqrt;
      
   function Distance (A1, A2 : Atom_Descriptor) return Long_Float is
   begin
      return Sqrt (Hypot (A1.X, A1.Y, A1.Z, A2.X, A2.Y, A2.Z));
   end;
   
   function Distance
     (
      A1 : Atom_Descriptor;
      V  : Matrix3x3;
      ICol : Integer
     ) return Long_Float is
   begin
      return Sqrt (Hypot
                     (
                      A1.X, A1.Y, A1.Z,
                      V(1,ICol), V(2,ICol), V(3,ICol)
                     )
                  );
   end;
   
   function Distance
     (
      V  : Matrix3x3;
      C1, C2 : Integer
     ) return Long_Float is
   begin
      return Sqrt (Hypot
                     (
                      V(1,C1), V(2,C1), V(3,C1),
                      V(1,C2), V(2,C2), V(3,C2)
                     )
                  );
   end;
   
   function Origin_Distance
     (
      V : Matrix3x3;
      ICol : Integer
     ) return Long_Float is
   begin
      return Sqrt (Hypot
                     (
                      0.0, 0.0, 0.0,
                      V(1,ICol), V(2,ICol), V(3,ICol)
                     )
                  );
   end;
   
   function Origin_Distance (A : Atom_Descriptor) return Long_Float is
   begin
      return Sqrt (Hypot (0.0, 0.0, 0.0, A.X, A.Y, A.Z));
   end;
   
begin
   
   Process_Options (Options, Help'Access);
   
   -- FI is File Index:
   for FI in 1 .. File_Name_Count loop
      Current_File := Select_File (FI);
      
      while not End_Of_File (Current_File.all) loop
         declare
            XYZ : XYZ_File_Atoms := Load_Atoms (Current_File.all);
         begin
            -- Put_Line (XYZ.Size'Image & " " & Get_File_Name(FI));
            declare
               Lattice_Keyword : String := "LATTICE:";
               Comment : String := To_String (XYZ.Comment);
               Lattice_Keyword_Index : Integer := Index (Comment, Lattice_Keyword);
               Parse_Start : Integer := Lattice_Keyword_Index + Lattice_Keyword'Last;
               Cell_Vectors : Matrix3x3;
               function Have_Lattice return Boolean is
               begin
                  return Lattice_Keyword_Index >= Comment'First;
               end;
            begin
               -- Print Header:
               Put ("atmid");
               for I in reverse XYZ.Atoms'Range loop
                  Put (",");
                  -- Put (To_String (XYZ.Atoms(I).Atom_Name));
                  Put (Atomic_Number(XYZ.Atoms(I).Atom_Type), 0);
               end loop;
               if Have_Lattice then
                  Parse_Lattice (Comment(Parse_Start..Comment'Last), Cell_Vectors);
                  Put (",-3,-2,-1");
               end if;
               New_Line;
               -- Distance to the Origin:
               Put ("0");
               for I in reverse XYZ.Atoms'Range loop
                  Put (",");
                  Put (Origin_Distance (XYZ.Atoms(I)), 0, 6);
               end loop;
               if Have_Lattice then
                  for ICol in reverse Cell_Vectors'Range(2) loop
                     Put (",");
                     Put (Origin_Distance (Cell_Vectors, Icol), 0, 6);
                  end loop;
               end if;
               New_Line;
               -- Distance to cell vector ends:
               if Have_Lattice then
                  for IV in Cell_Vectors'Range(1) loop
                     Put (-IV,0);
                     for I in reverse XYZ.Atoms'Range loop
                        Put (",");
                        Put (Distance (XYZ.Atoms(I), Cell_Vectors, IV), 0, 6);
                     end loop;
                     for ICol in reverse IV + 1 .. Cell_Vectors'Last(2) loop
                        Put (",");
                        Put (Distance (Cell_Vectors, IV, ICol), 0, 6);
                     end loop;
                     for X in 1..IV loop
                        Put (",");
                     end loop;
                     New_Line;
                  end loop;
               end if;
               -- Distances between atoms:
               for I in XYZ.Atoms'First .. XYZ.Atoms'Last - 1 loop
                  -- Put (To_String (XYZ.Atoms(I).Atom_Name));
                  Put (Atomic_Number(XYZ.Atoms(I).Atom_Type), 0);
                  for J in reverse I + 1 .. XYZ.Atoms'Last loop
                     Put (",");
                     Put (Distance (XYZ.Atoms(I), XYZ.Atoms(J)), 0, 6);
                  end loop;
                  for X in 1..I loop
                     Put (",");
                  end loop;
                  if Have_Lattice then
                     for X in Cell_Vectors'Range(2) loop
                        Put (",");
                     end loop;
                  end if;
                  New_Line;
               end loop;
            end;
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
