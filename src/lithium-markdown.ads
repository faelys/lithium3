------------------------------------------------------------------------------
-- Copyright (c) 2015-2017, Natacha Porté                                   --
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
-- Lithium.Markdown provides a worker task that performs markdown rendering --
------------------------------------------------------------------------------

with Ada.Streams;
with Natools.S_Expressions.Atom_Refs;
with Natools.String_Slices;

package Lithium.Markdown is

   task Extended is
      entry Render
        (Source : in out Ada.Streams.Root_Stream_Type'Class;
         Output : out Natools.S_Expressions.Atom_Refs.Immutable_Reference;
         Summary : out Natools.S_Expressions.Atom_Refs.Immutable_Reference);
      entry Render
        (Source : in Natools.String_Slices.Slice;
         Output : out Natools.S_Expressions.Atom_Refs.Immutable_Reference;
         Summary : out Natools.S_Expressions.Atom_Refs.Immutable_Reference);
   end Extended;

   task Comment is
      entry Render
        (Source : in out Ada.Streams.Root_Stream_Type'Class;
         Output : out Natools.S_Expressions.Atom_Refs.Immutable_Reference);
      entry Render
        (Source : in Natools.String_Slices.Slice;
         Output : out Natools.S_Expressions.Atom_Refs.Immutable_Reference);
   end Comment;

end Lithium.Markdown;
