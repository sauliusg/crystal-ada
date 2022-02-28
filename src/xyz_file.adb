with Text_Io;                use Text_Io;
with Ada.Integer_Text_Io;    use Ada.Integer_Text_Io;
with Ada.Long_Float_Text_IO; use Ada.Long_Float_Text_IO;
with Crystal_Unit_Cell;      use Crystal_Unit_Cell;

package body Xyz_File is
   
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
   
   procedure Put_Non_Cell_Comment (Molecule : XYZ_File_Atoms;
                                   Unit_Cell_Given : Boolean) is
      Cell_Index : Integer;
   begin
      Cell_Index := Index (Molecule.Comment, " CELL:");
      if Cell_Index > 0 and Unit_Cell_Given then
         if Cell_Index > 1 then
            Put (To_String (Molecule.Comment)(1..Cell_Index-1));
         else
            null; -- do not print anything of we only have " CELL:" 
                  -- on the comment line.
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
                                Unit_Cell_Given : Boolean;
                                Integer_Size, Fraction_Size, 
                                  Exponent_Size : Integer ) is
   begin
      Put_Non_Cell_Comment (Molecule, Unit_Cell_Given);
      
      if Unit_Cell_Given then
         Put_Cell_Keyword (Molecule, Unit_Cell,
                           Integer_Size => Integer_Size,
                           Fraction_Size => Fraction_Size,
                           Exponent_Size => Exponent_Size);
      end if;
   end Put_Atom_Comment;
   
   procedure Put_Atom_Comment ( Molecule : XYZ_File_Atoms;
                                Lattice : Matrix3x3;
                                Unit_Cell_Given : Boolean;
                                Integer_Size, Fraction_Size, 
                                  Exponent_Size : Integer ) is
   begin
      Put_Non_Cell_Comment (Molecule, Unit_Cell_Given);
      
      if Unit_Cell_Given then
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
   
   procedure Put_Atoms ( Molecule : XYZ_File_Atoms;
                         Unit_Cell : Unit_Cell_Type;
                         Unit_Cell_Given : Boolean;
                         Integer_Size, Fraction_Size, 
                           Exponent_Size : Integer ) is
   begin
      Put (Molecule.Atoms'Last, 1);
      New_Line;
      Put_Atom_Comment (Molecule, Unit_Cell, Unit_Cell_Given,
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
                         Unit_Cell_Given : Boolean;
                         Integer_Size, Fraction_Size, 
                           Exponent_Size : Integer ) is
   begin
      Put (Molecule.Atoms'Last, 1);
      New_Line;
      Put_Atom_Comment (Molecule, Lattice, Unit_Cell_Given,
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
                  Unit_Cell_Given => False,
                  Integer_Size => Integer_size,
                  Fraction_Size => Fraction_Size,
                  Exponent_Size => Exponent_Size );
   end;
   
   function Load_Atoms( File : in File_Type ) return XYZ_File_Atoms is
      N : Integer := Integer'Value (Get_Line (File));
      Comment : String := Get_Line (File);
      XYZ_Atoms : XYZ_File_Atoms (N);
   begin
      XYZ_Atoms.Comment := To_Unbounded_String (Comment);
      for I in 1..N loop
         declare
            Line : String := Get_Line (File);
            Position : Integer := Line'First;
         begin
            if Line(2) = ' ' then
               XYZ_Atoms.Atoms(I).Atom_Type(1) := Line(1);
               XYZ_Atoms.Atoms(I).Atom_Type(2) := ' ';
               Position := Line'First;
            else
               XYZ_Atoms.Atoms(I).Atom_Type := Line(1..2);
               Position := Line'First + 1;
            end if;
            Get (Line(Position+1..Line'Last), XYZ_Atoms.Atoms(I).X, Position);
            Get (Line(Position+1..Line'Last), XYZ_Atoms.Atoms(I).Y, Position);
            Get (Line(Position+1..Line'Last), XYZ_Atoms.Atoms(I).Z, Position);
         end;
      end loop;
      return XYZ_Atoms;
   end;
   
end Xyz_File;
