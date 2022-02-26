with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Crystal_Unit_Cell;      use Crystal_Unit_Cell;

package Xyz_File is
   
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
                return Atom_Descriptor;

   procedure Put_Atoms ( Molecule : XYZ_File_Atoms;
                         Unit_Cell : Unit_Cell_Type;
                         Unit_Cell_Given : Boolean;
                         Integer_Size, Fraction_Size, 
                           Exponent_Size : Integer );
   
end Xyz_File;
