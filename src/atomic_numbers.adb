package body Atomic_Numbers is
   
   function Atomic_Number ( Atom_Name : String ) return Positive is
      Name : String (1..2) := "  ";
   begin
      if Atom_Name'Length = 1 then
         Name (1) := Atom_Name (Atom_Name'First);
      else
         Name := Atom_Name;
      end if;
      for A of Atomic_Numbers loop
         if A.Atom_Name = Name then
            return A.Atomic_NUmber;
         end if;
      end loop;
      raise UNKNOWN_ATOM_NAME with
        "atom name """ & Atom_Name & """ could not be found";
   end;
   
   function Atom_Name ( Atomic_Number : Positive ) return String is
   begin
      for A of Atomic_Numbers loop
         if A.Atomic_Number = Atomic_Number then
            return A.Atom_Name;
         end if;
      end loop;
      raise UNKNOWN_ATOMIC_NUMBER with
        "atom number " & Atomic_Number'Image & " could not be found";
   end;
   
end;
