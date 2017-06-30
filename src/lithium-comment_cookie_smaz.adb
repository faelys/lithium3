------------------------------------------------------------------------------
-- Copyright (c) 2017, Natacha Porté                                        --
--                                                                          --
-- Permission to use, copy, modify, and distribute this software for any    --
-- purpose with or without fee is hereby granted, provided that the above   --
-- copyright notice and this permission notice appear in all copies.        --
--                                                                          --
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES --
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF         --
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR  --
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES   --
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN    --
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF  --
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.           --
------------------------------------------------------------------------------

with Natools.Smaz_64;

with Lithium.Comment_Cookie_Smaz_Hash;

package body Lithium.Comment_Cookie_Smaz is

   package Sx renames Natools.S_Expressions;

   Dict : constant Natools.Smaz_64.Dictionary
     := (Last_Code => 60,
         Values_Last => 128,
         Variable_Length_Verbatim => False,
         Max_Word_Length => 16,
         Offsets => (15, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43,
            44, 45, 46, 48, 49, 50, 51, 52, 55, 56, 58, 60, 62, 63, 64, 65, 66,
            68, 69, 71, 73, 75, 78, 82, 86, 91, 94, 97, 98, 100, 102, 104, 106,
            107, 109, 111, 112, 115, 116, 117, 119, 120, 122, 124, 126, 128),
         Values => "legacy-commentcomment-markdowneoatmcn rlghsi.omp-u/comyen"
            & "leacf""@dmabfrroou://httpbloggmailwww.frkinrior ""zonanwnetxjr"
            & "eveecheris_",
         Hash => Lithium.Comment_Cookie_Smaz_Hash.Hash'Access);

   function Encoder (Data : in Natools.S_Expressions.Atom) return String is
   begin
      return Key & Sx.To_String (Natools.Smaz_64.Compress
        (Dict, Sx.To_String (Data)));
   end Encoder;


   function Decoder (Data : in String) return Natools.S_Expressions.Atom is
   begin
      if Data'Length > 0 then
         return Sx.To_Atom (Natools.Smaz_64.Decompress
           (Dict, Sx.To_Atom (Data (Data'First + 1 .. Data'Last))));
      else
         return Sx.Null_Atom;
      end if;
   end Decoder;

end Lithium.Comment_Cookie_Smaz;
