------------------------------------------------------------------------------
-- Copyright (c) 2017-2019, Natacha Porté                                   --
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

------------------------------------------------------------------------------
-- Lithium.Spoiler_Filters provides a filter to encode spoilers in rot13,   --
-- for situations where CSS might not apply (e.g. ATOM feeds).              --
------------------------------------------------------------------------------

with Ada.Streams;
with Natools.S_Expressions.Lockable;
with Natools.Web.Filters;

package Lithium.Spoiler_Filters is
   pragma Preelaborate;

   type Spoiler_Filter is new Natools.Web.Filters.Filter with private;

   overriding procedure Apply
     (Object : in Spoiler_Filter;
      Output : in out Ada.Streams.Root_Stream_Type'Class;
      Data : in Ada.Streams.Stream_Element_Array);

   function Create
     (Arguments : in out Natools.S_Expressions.Lockable.Descriptor'Class)
     return Natools.Web.Filters.Filter'Class;


   Begin_HTML : constant Natools.S_Expressions.Atom
     := (1 => Character'Pos ('<'),
         2 => Character'Pos ('s'),
         3 => Character'Pos ('p'),
         4 => Character'Pos ('a'),
         5 => Character'Pos ('n'),
         6 => Character'Pos (' '),
         7 => Character'Pos ('c'),
         8 => Character'Pos ('l'),
         9 => Character'Pos ('a'),
        10 => Character'Pos ('s'),
        11 => Character'Pos ('s'),
        12 => Character'Pos ('='),
        13 => Character'Pos ('"'),
        14 => Character'Pos ('s'),
        15 => Character'Pos ('p'),
        16 => Character'Pos ('o'),
        17 => Character'Pos ('i'),
        18 => Character'Pos ('l'),
        19 => Character'Pos ('e'),
        20 => Character'Pos ('r'),
        21 => Character'Pos ('"'),
        22 => Character'Pos ('>'));

   End_HTML : constant Natools.S_Expressions.Atom
     := (1 => Character'Pos ('<'),
         2 => Character'Pos ('/'),
         3 => Character'Pos ('s'),
         4 => Character'Pos ('p'),
         5 => Character'Pos ('a'),
         6 => Character'Pos ('n'),
         7 => Character'Pos ('>'));

   Begin_Filtered : constant Natools.S_Expressions.Atom
     := (1 => Character'Pos ('['),
         2 => Character'Pos ('S'),
         3 => Character'Pos ('P'),
         4 => Character'Pos ('O'),
         5 => Character'Pos ('I'),
         6 => Character'Pos ('L'),
         7 => Character'Pos ('E'),
         8 => Character'Pos ('R'),
         9 => Character'Pos (']'),
        10 => Character'Pos ('['));

   End_Filtered : constant Natools.S_Expressions.Atom
     := (1 => Character'Pos (']'));

private

   type Spoiler_Filter is new Natools.Web.Filters.Filter with null record;

end Lithium.Spoiler_Filters;
