with Text_Io;                  use Text_Io;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Integer_Text_Io;      use Ada.Integer_Text_Io;
with Ada.Long_Float_Text_IO;   use Ada.Long_Float_Text_IO;
with Ada.Strings;              use Ada.Strings;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;
with Ada.Strings.Maps;         use Ada.Strings.Maps;
with Crystal_Unit_Cell;        use Crystal_Unit_Cell;

package body Xyz_File is
   
   function "*" ( M : Matrix3x3; A : Atom_Descriptor )
                return Atom_Descriptor is
      R : Atom_Descriptor;
   begin
      R.Atom_Type := A.Atom_Type;
      R.Atom_Name := A.Atom_Name;
      R.X := A.X*M(1,1) + A.Y*M(1,2) + A.Z*M(1,3);
      R.Y := A.X*M(2,1) + A.Y*M(2,2) + A.Z*M(2,3);
      R.Z := A.X*M(3,1) + A.Y*M(3,2) + A.Z*M(3,3);
      return R;
   end;
   
   procedure Put_Non_Cell_Comment (Molecule : XYZ_File_Atoms;
                                   Unit_Cell_Known : Boolean) is
      Cell_Index : Integer := Index (Molecule.Comment, "CELL:");
      Lattice_Index : Integer := Index (Molecule.Comment, "LATTICE:");
      Min_Index : Integer;
   begin
      if Cell_Index = 0 then
         Min_Index := Lattice_Index;
      elsif Lattice_Index = 0 then
         Min_Index := Cell_Index;
      else
         Min_Index :=
           (if Cell_Index < Lattice_Index then Cell_Index else Lattice_Index);
      end if;
      if Min_Index > 0 and Unit_Cell_Known then
         if Min_Index > 1 and then
           To_String (Molecule.Comment)(Min_Index-1) = ' ' then
            Min_Index := Min_Index - 1;
         end if;
         if Min_Index > 1 then
            Put (To_String (Molecule.Comment)(1..Min_Index-1));
         else
            null; -- do not print anything if we only have "CELL:" or
                  --  only "LATTICE:" on the comment line.
         end if;
      else
         Put (To_String (Molecule.Comment));
      end if;
   end;
   
   procedure Put_Cell_keyword (Molecule : XYZ_File_Atoms; 
                               Unit_Cell : Unit_Cell_Type;
                               Integer_Size, Fraction_Size,
                                 Exponent_Size : Integer) is
   begin
      Put (" CELL: ");
      for I in Unit_Cell'Range loop
         Put (Unit_Cell(I), 2, Fraction_Size, Exponent_Size);
         Put (" ");
      end loop;
   end;
   
   procedure Put_Lattice_keyword (Molecule : XYZ_File_Atoms; 
                                  Lattice : Matrix3x3;
                                  Integer_Size, Fraction_Size,
                                    Exponent_Size : Integer) is
   begin
      Put (" LATTICE: ");
      for J in Lattice'Range(2) loop
         for I in Lattice'Range(1) loop
            Put (Lattice(I,J), 2, Fraction_Size, Exponent_Size);
            Put (" ");
         end loop;
      end loop;
   end;
   
   procedure Put_Atom_Comment ( Molecule : XYZ_File_Atoms;
                                Unit_Cell : Unit_Cell_Type;
                                Unit_Cell_Known : Boolean;
                                Integer_Size, Fraction_Size, 
                                  Exponent_Size : Integer ) is
   begin
      Put_Non_Cell_Comment (Molecule, Unit_Cell_Known);
      
      if Unit_Cell_Known then
         Put_Cell_Keyword (Molecule, Unit_Cell,
                           Integer_Size => Integer_Size,
                           Fraction_Size => Fraction_Size,
                           Exponent_Size => Exponent_Size);
      end if;
   end Put_Atom_Comment;
   
   procedure Put_Atom_Comment ( Molecule : XYZ_File_Atoms;
                                Lattice : Matrix3x3;
                                Unit_Cell_Known : Boolean;
                                Integer_Size, Fraction_Size, 
                                  Exponent_Size : Integer ) is
   begin
      Put_Non_Cell_Comment (Molecule, Unit_Cell_Known);
      
      if Unit_Cell_Known then
         Put_Lattice_Keyword (Molecule, Lattice,
                              Integer_Size => Integer_Size,
                              Fraction_Size => Fraction_Size,
                              Exponent_Size => Exponent_Size);
      end if;
   end Put_Atom_Comment;
   
   procedure Put_Atom_Coordinates (Molecule : XYZ_File_Atoms;
                                   Integer_Size, 
                                     Fraction_Size, 
                                     Exponent_Size : Integer) is
   begin
      for I in Molecule.Atoms'Range loop
         -- Put (Molecule.Atoms(I).Atom_Type);
         Put (Molecule.Atoms(I).Atom_Name);
         if To_String(Molecule.Atoms(I).Atom_Name)'Last = 1 then
            Put (" ");
         end if;
         Put (" ");
         Put (Molecule.Atoms(I).X, Integer_Size, Fraction_Size, Exponent_Size);
         Put (" ");
         Put (Molecule.Atoms(I).Y, Integer_Size, Fraction_Size, Exponent_Size);
         Put (" ");
         Put (Molecule.Atoms(I).Z, Integer_Size, Fraction_Size, Exponent_Size);
         New_Line;
      end loop;
   end;
   
   procedure Put_Atoms ( Molecule : XYZ_File_Atoms;
                         Unit_Cell : Unit_Cell_Type;
                         Unit_Cell_Known : Boolean;
                         Integer_Size, Fraction_Size, 
                           Exponent_Size : Integer ) is
   begin
      Put (Molecule.Atoms'Last, 1);
      New_Line;
      Put_Atom_Comment (Molecule, Unit_Cell, Unit_Cell_Known,
                        Integer_Size => Integer_Size,
                        Fraction_Size => Fraction_Size,
                        Exponent_Size => Exponent_Size
                       );
      New_Line;
      Put_Atom_Coordinates (Molecule, 
                            Integer_Size => Integer_Size,
                            Fraction_Size => Fraction_Size,
                            Exponent_Size => Exponent_Size
                           );
   end;
   
   procedure Put_Atoms ( Molecule : XYZ_File_Atoms;
                         Lattice : Matrix3x3;
                         Unit_Cell_Known : Boolean;
                         Integer_Size, Fraction_Size, 
                           Exponent_Size : Integer ) is
   begin
      Put (Molecule.Atoms'Last, 1);
      New_Line;
      Put_Atom_Comment (Molecule, Lattice, Unit_Cell_Known,
                        Integer_Size => Integer_Size,
                        Fraction_Size => Fraction_Size,
                        Exponent_Size => Exponent_Size
                       );
      New_Line;
      Put_Atom_Coordinates (Molecule, 
                            Integer_Size => Integer_Size,
                            Fraction_Size => Fraction_Size,
                            Exponent_Size => Exponent_Size
                           );
   end;
   
   procedure Put_Atoms ( Molecule : XYZ_File_Atoms;
                         Integer_Size, Fraction_Size, 
                           Exponent_Size : Integer ) is
      
      Dummy : Unit_Cell_Type := (others => 0.0);
   begin
      Put_Atoms ( Molecule,
                  Unit_Cell => Dummy,
                  Unit_Cell_Known => False,
                  Integer_Size => Integer_size,
                  Fraction_Size => Fraction_Size,
                  Exponent_Size => Exponent_Size );
   end;
   
   function Load_Atoms( File : in File_Type ) return XYZ_File_Atoms is
      N : Integer := Integer'Value (Get_Line (File));
      Comment : String := Get_Line (File);
      XYZ_Atoms : XYZ_File_Atoms (N);
      Whitespace : constant Character_Set := To_Set (' ');
   begin
      XYZ_Atoms.Comment := To_Unbounded_String (Comment);
      for I in 1..N loop
         declare
            Line : String := Get_Line (File);
            Start, Position : Integer;
         begin
            Find_Token( Line, Set => Whitespace, From => 1, Test => Outside,
                        First => Start, Last => Position );
            XYZ_Atoms.Atoms(I).Atom_Name :=
              To_Unbounded_String (Line (Start..Position));
            if Line(2) = ' ' then
               XYZ_Atoms.Atoms(I).Atom_Type(1) := Line(1);
               XYZ_Atoms.Atoms(I).Atom_Type(2) := ' ';
            else
               XYZ_Atoms.Atoms(I).Atom_Type := Line(1..2);
               if not (Line(2) in 'A' .. 'Z') and then
                  not (Line(2) in  'a' .. 'z') then
                  XYZ_Atoms.Atoms(I).Atom_Type(2) := ' ';
               end if;
            end if;
            Get (Line(Position+1..Line'Last), XYZ_Atoms.Atoms(I).X, Position);
            Get (Line(Position+1..Line'Last), XYZ_Atoms.Atoms(I).Y, Position);
            Get (Line(Position+1..Line'Last), XYZ_Atoms.Atoms(I).Z, Position);
         end;
      end loop;
      return XYZ_Atoms;
   end;
   
end Xyz_File;
