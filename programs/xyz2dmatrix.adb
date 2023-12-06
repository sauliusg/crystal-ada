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

procedure Xyz2DMatrix is
   
   Matrix_Size : Positive := 80;
   
   Identifier_Opt : Option_Value_Access :=
     new Option_Value_Type (STRING_OPT);
   
   procedure Help is
      procedure P (S : String) renames Put_Line;
   begin
      P ("Convert an XYZ file into the distance matrix, output the CSV");
   end;
   
   Options : Option_Array :=
     (
      Help_Option ("", "--help"),
      Option ("-I", "--identifier", Identifier_Opt)
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
      -- Put ("|A1 -- A2: " & To_String (A1.Atom_Name) & " -- " & To_String (A2.Atom_Name) & "|");
      return Sqrt (Hypot (A1.X, A1.Y, A1.Z, A2.X, A2.Y, A2.Z));
   end;
   
   function Distance
     (
      A1 : Atom_Descriptor;
      V  : Matrix3x3;
      ICol : Integer
     ) return Long_Float is
   begin
      -- Put ("|Cell -- Atom: " & ICol'Image & " -- "  & To_String (A1.Atom_Name) & "|");
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
      -- Put ("|Cell vectrors: " & C1'Image & C2'Image & "|");
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
      -- Put ("|Cell -- Origin: " & ICol'Image & "|");
      return Sqrt (Hypot
                     (
                      0.0, 0.0, 0.0,
                      V(1,ICol), V(2,ICol), V(3,ICol)
                     )
                  );
   end;
   
   function Origin_Distance (A : Atom_Descriptor) return Long_Float is
   begin
      -- Put ("|Atom -- Origin: " & To_String (A.Atom_Name) & "|");
      return Sqrt (Hypot (0.0, 0.0, 0.0, A.X, A.Y, A.Z));
   end;
   
begin
   
   Process_Options (Options, Help'Access);
   
   -- Header:
   Put_Line ("id,distances");
   
   -- FI is File Index:
   for FI in 1 .. File_Name_Count loop
      Current_File := Select_File (FI);
      
      while not End_Of_File (Current_File.all) loop
         declare
            XYZ : XYZ_File_Atoms := Load_Atoms (Current_File.all);
         begin
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
               -- Distance to the Origin:
               -- Put (Identifier_Opt.String_Value.all & ",");
               declare
                  Id_End_Index : Integer := Index (Comment, " ");
                  Identifier : String := Comment (Comment'First .. Id_End_Index - 1);
               begin
                  if XYZ.Atoms'Length > Matrix_Size then
                     Put_Line (Standard_Error, "WARNING, structure '" & Identifier & "' " &
                                 "has too many atoms (" & XYZ.Atoms'Length'Image & 
                                 " instead of " & Matrix_Size'Image & ")");
                     -- raise CONSTRAINT_ERROR with
                     --  "too many atoms for structure " & Identifier;
                  end if;
                  Put (Identifier);
                  Put (",");
               end;
               -- Put ("0");
               for I in XYZ.Atoms'Range loop
                  if I > XYZ.Atoms'First then
                     Put (" ");
                  end if;
                  Put (Origin_Distance (XYZ.Atoms(I)), 0, 6);
               end loop;
               for I in XYZ.Atoms'Last + 1 .. Matrix_Size loop
                  Put (" -1");
               end loop;
               if Have_Lattice then
                  Parse_Lattice (Comment(Parse_Start..Comment'Last), Cell_Vectors);
                  for ICol in reverse Cell_Vectors'Range(2) loop
                     Put (" ");
                     Put (Origin_Distance (Cell_Vectors, ICol), 0, 6);
                  end loop;
               end if;
               -- -- -- New_Line;
               -- Distance to cell vector ends:
               if Have_Lattice then
                  for IV in Cell_Vectors'Range(1) loop
                     -- Put (-IV,0);
                     for I in XYZ.Atoms'First .. XYZ.Atoms'Last loop
                        Put (" ");
                        Put (Distance (XYZ.Atoms(I), Cell_Vectors, IV), 0, 6);
                     end loop;
                     for I in XYZ.Atoms'Last + 1 .. Matrix_Size loop
                        Put (" -1");
                     end loop;
                     for ICol in IV + 1 .. Cell_Vectors'Last(2) loop
                        Put (" ");
                        Put (Distance (Cell_Vectors, IV, ICol), 0, 6);
                     end loop;
                     -- New_Line;
                  end loop;
               end if;
               -- Distances between atoms:
               for I in XYZ.Atoms'First .. XYZ.Atoms'Last loop
                  -- Put (To_String (XYZ.Atoms(I).Atom_Name));
                  -- Put (Atomic_Number(XYZ.Atoms(I).Atom_Type), 0);
                  for J in I + 1 .. XYZ.Atoms'Last loop
                     Put (" ");
                     Put (Distance (XYZ.Atoms(I), XYZ.Atoms(J)), 0, 6);
                  end loop;
                  for I in XYZ.Atoms'Last + 1 .. Matrix_Size loop
                     Put (" -1");
                  end loop;
                  -- New_Line;
               end loop;
               for I in XYZ.Atoms'Last + 1 .. Matrix_Size loop
                  for J in I + 1 .. Matrix_Size loop
                     Put (" -1");
                  end loop;
               end loop;
               New_Line;
            end;
         end;    
      end loop;

      Close (Current_File);
   end loop;
   
exception
   when HELP_PRINTED => null;
      
end Xyz2DMatrix;
