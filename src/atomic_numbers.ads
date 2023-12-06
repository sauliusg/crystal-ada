package Atomic_Numbers is
   
   UNKNOWN_ATOM_NAME : exception;
   UNKNOWN_ATOMIC_NUMBER : exception;
   
   type Atomic_Number_Type is record
      Atom_Name : String (1..2);
      Atomic_Number : Positive;
   end record;   
   
   type Atomic_Number_Array is array (Positive range <>) of Atomic_Number_Type;
   
   Atomic_Numbers : constant Atomic_Number_Array :=
     (
      ("Ac",89),	("Cr",24),	("I ",53),	("Os",76),	("Sm",62),
      ("Ag",47),	("Cs",55),	("In",49),	("P ",15),	("Sn",50),
      ("Al",13),	("Cu",29),	("Ir",77),	("Pa",91),	("Sr",38),
      ("Am",95),	("D ",1),	("K ",19),	("Pb",82),	("Ta",73),
      ("Ar",18),	("Db",105),	("Kr",36),	("Pd",46),	("Tb",65),
      ("As",33),	("Ds",110),	("La",57),	("Pm",61),	("Tc",43),
      ("At",85),	("Dy",66),	("Li",3),	("Po",84),	("Te",52),
      ("Au",79),	("Er",68),	("Lr",103),	("Pr",59),	("Th",90),
      ("B ",5),	        ("Es",99),	("Lu",71),	("Pt",78),	("Ti",22),
      ("Ba",56),	("Eu",63),	("Md",101),	("Pu",94),	("Tl",81),
      ("Be",4),	        ("F ",9),	("Mg",12),	("Ra",88),	("Tm",69),
      ("Bh",107),	("Fe",26),	("Mn",25),	("Rb",37),	("U ",92),
      ("Bi",83),	("Fm",100),	("Mo",42),	("Re",75),	("V ",23),
      ("Bk",97),	("Fr",87),	("Mt",109),	("Rf",104),	("W ",74),
      ("Br",35),	("Ga",31),	("N ",7),	("Rh",45),	("Xe",54),
      ("C ",6),	        ("Gd",64),	("Na",11),	("Rn",86),	("Y ",39),
      ("Ca",20),	("Ge",32),	("Nb",41),	("Ru",44),	("Yb",70),
      ("Cd",48),	("H ",1),	("Nd",60),	("S ",16),	("Zn",30),
      ("Ce",58),	("He",2),	("Ne",10),	("Sb",51),	("Zr",40),
      ("Cf",98),	("Hf",72),	("Ni",28),	("Sc",21),
      ("Cl",17),	("Hg",80),	("No",102),	("Se",34),
      ("Cm",96),	("Ho",67),	("Np",93),	("Sg",106),
      ("Co",27),	("Hs",108),	("O ",8),	("Si",14)
     );
   
   function Atomic_Number ( Atom_Name : String ) return Positive;
   
   function Atom_Name ( Atomic_Number : Positive ) return String;
   
end Atomic_Numbers;
