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

package body Lithium.Line_Parsers is

   procedure Append_Line
     (Line : in out Natools.S_Expressions.Atom_Buffers.Atom_Buffer;
      Source : in out Ada.Streams.Root_Stream_Type'Class;
      Finished : out Boolean)
   is
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 1);
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      Finished := False;

      loop
         Source.Read (Buffer, Last);

         if Last not in Buffer'Range then
            Finished := True;
            exit;
         end if;

         Line.Append (Buffer (Last));

         exit when Buffer (Last) in 10 | 13;
      end loop;
   end Append_Line;


   overriding procedure Read_More
     (Self : in out Parser;
      Buffer : out Natools.S_Expressions.Atom_Buffers.Atom_Buffer)
   is
      Finished : Boolean;
   begin
      Append_Line (Buffer, Self.Source.all, Finished);
   end Read_More;

end Lithium.Line_Parsers;

