------------------------------------------------------------------------------
-- Copyright (c) 2015, Natacha Porté                                        --
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
-- Lithium.Line_Parsers provides a stream-based S-expression parser which   --
-- reads the input stream line by line, to allow mixed content.             --
------------------------------------------------------------------------------

with Ada.Streams;
with Natools.S_Expressions.Atom_Buffers;
with Natools.S_Expressions.Parsers;

package Lithium.Line_Parsers is
   pragma Preelaborate;

   type Parser (Source : access Ada.Streams.Root_Stream_Type'Class)
     is limited new Natools.S_Expressions.Parsers.Parser with private;
   pragma Preelaborable_Initialization (Parser);

   overriding procedure Read_More
     (Self : in out Parser;
      Buffer : out Natools.S_Expressions.Atom_Buffers.Atom_Buffer);

private

   type Parser (Source : access Ada.Streams.Root_Stream_Type'Class)
     is limited new Natools.S_Expressions.Parsers.Parser with null record;

end Lithium.Line_Parsers;
