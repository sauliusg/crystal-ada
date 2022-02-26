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
   
   type Access_File_Type is access File_Type;
   Current_File : Access_File_Type;
       
   procedure Free_File is
      new Ada.Unchecked_Deallocation(File_Type, Access_File_Type);
   
   type Atom_Descriptor is
      record
         Atom_Type : String(1..2);
         X, Y, Z : Long_Float;
      end record;
   
   type Atom_Descriptor_Array
      is array ( Integer range <> ) of Atom_Descriptor;
   
   type XYZ_File_Atoms (Size : Integer) is
      record
         Comment : Unbounded_String;
         Atoms : Atom_Descriptor_Array (1..Size);
      end record;
   
   procedure Put_Atoms ( Molecule : XYZ_File_Atoms ) is
   begin
      Put (Molecule.Atoms'Last, 1); New_Line;
      Put ( To_String (Molecule.Comment) );
      New_Line;

      for I in Molecule.Atoms'Range loop
         Put (Molecule.Atoms(I).Atom_Type);
         Put (" ");
         Put (Molecule.Atoms(I).X, Integer_Size, Fraction_Size, Exponent_Size);
         Put (" ");
         Put (Molecule.Atoms(I).Y, Integer_Size, Fraction_Size, Exponent_Size);
         Put (" ");
         Put (Molecule.Atoms(I).Z, Integer_Size, Fraction_Size, Exponent_Size);
         New_Line;
      end loop;
   end;
   
begin
   
   Process_Options;
   
   declare
      File_Name : Unbounded_String;
      File_Processed : Boolean := False;   
   begin
      loop
         File_Name := To_Unbounded_String(Get_Argument);
         -- Put_Line ("Current file name: '" & To_String(File_Name) & "'");
         if File_Name = To_Unbounded_String ("") then
            if not File_Processed then
               File_Name := To_Unbounded_String ("-");
               Current_File := new File_Type'(Standard_Input);
            else
               exit;
            end if;
         else
            if File_Name = To_Unbounded_String ("-") then
               Current_File := new File_Type'(Standard_Input);
            else 
               Current_File := new File_Type;
               Open (Current_File.all, In_File, To_String(File_Name));
            end if;
         end if;
         File_Processed := True;
         
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
               Put_Atoms (XYZ_Atoms);
            end;
         end loop;
         
         if Is_Open (Current_File.all) then
            Close (Current_File.all);
         end if;
         Free_File (Current_File);
         
      end loop;
      
   end;

end XyzFormat;
