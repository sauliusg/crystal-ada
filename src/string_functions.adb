package body String_Functions is

   procedure Replace ( S : in out String; Chr_From, Chr_To : Character ) is
   begin
      for I in S'Range loop
         if S(I) = Chr_From then
            S(I) := Chr_To;
         end if;
      end loop;
   end;

end String_Functions;
