with Interfaces; use Interfaces;

package body Lithium.Comment_Cookie_Smaz_Hash is

   P : constant array (0 .. 2) of Natural :=
     (1, 2, 4);

   T1 : constant array (0 .. 2) of Unsigned_8 :=
     (44, 115, 66);

   T2 : constant array (0 .. 2) of Unsigned_8 :=
     (27, 43, 74);

   G : constant array (0 .. 122) of Unsigned_8 :=
     (0, 0, 25, 0, 0, 0, 55, 0, 0, 19, 0, 0, 36, 0, 0, 0, 0, 0, 0, 0, 28, 2,
      0, 0, 0, 0, 29, 45, 0, 0, 14, 59, 0, 60, 15, 2, 0, 38, 0, 0, 0, 25,
      52, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 41, 0, 0, 35, 27, 5, 43,
      32, 0, 0, 22, 0, 7, 21, 50, 0, 18, 0, 0, 0, 17, 0, 8, 37, 57, 0, 0, 0,
      1, 8, 4, 3, 58, 27, 7, 0, 49, 0, 37, 54, 10, 0, 0, 0, 21, 41, 13, 30,
      12, 19, 0, 0, 44, 3, 35, 26, 46, 54, 0, 0, 0, 37, 0, 0, 24, 41, 6);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 123;
         F2 := (F2 + Natural (T2 (K)) * J) mod 123;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 61;
   end Hash;

end Lithium.Comment_Cookie_Smaz_Hash;
