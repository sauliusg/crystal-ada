-- ----------------------------------------------------------------------------
-- $Author$
-- $Date$
-- $Revision$
-- $URL$
-- ----------------------------------------------------------------------------

-- Take an XYZ format molecular file
--  (https://en.wikipedia.org/wiki/XYZ_file_format) and convert
--  orthogonal coordinates to fractional ones. Unit cell constants can
--  be given as command line option parameters.

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

with Ada.Unchecked_Deallocation;

procedure Xyz2fract is
   
   Unit_Cell : Unit_Cell_Type := (others => 0.0);

   Unit_Cell_Given : Boolean := False;
   
   procedure Process_Options is
      Help_Option : String := "-help -hel -he -h h ";
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
                  Put_Line ("Seen --help");
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
   
   function "*" ( M : Matrix3x3; A : Atom_Descriptor )
                return Atom_Descriptor is
      R : Atom_Descriptor;
   begin
      R.Atom_Type := A.Atom_Type;
      R.X := A.X*M(1,1) + A.Y*M(1,2) + A.Z*M(1,3);
      R.Y := A.X*M(2,1) + A.Y*M(2,2) + A.Z*M(2,3);
      R.Z := A.X*M(3,1) + A.Y*M(3,2) + A.Z*M(3,3);
      return R;
   end;
   
   procedure Put_Atoms ( Molecule : XYZ_File_Atoms ) is
   begin
      Put (Molecule.Atoms'Last, 1); New_Line;
      Put ( To_String (Molecule.Comment) );
      
      if Unit_Cell_Given then
         Put (" CELL: ");
         for I in Unit_Cell'Range loop
            Put (Unit_Cell(I), 2, Fraction_Size, Exponent_Size);
            Put (" ");
         end loop;
      end if;
      
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
   
   F4O : Matrix3x3;
   
begin
   
   Process_Options;
   
   F4O := Matrix_Fract_From_Ortho (Unit_Cell);
   
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
               
               for I in XYZ_Atoms.Atoms'Range loop
                  XYZ_Atoms.Atoms(I) := F4O * XYZ_Atoms.Atoms(I);
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

end Xyz2fract;
