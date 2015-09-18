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

with Natools.S_Expressions.Atom_Refs;

package body Lithium.Markdown.Filters is

   overriding procedure Read
     (Stream : in out Memory_Stream;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset)
   is
      use type Ada.Streams.Stream_Element_Offset;

      Size : constant Ada.Streams.Stream_Element_Count
        := Ada.Streams.Stream_Element_Count'Min
           (Item'Length, Stream.Size - Stream.Cursor);
   begin
      Last := Item'First + Size - 1;
      Item (Item'First .. Last)
        := Stream.Data (Stream.Cursor + 1 .. Stream.Cursor + Size);
      Stream.Cursor := Stream.Cursor + Size;
   end Read;


   overriding procedure Write
     (Stream : in out Memory_Stream;
      Item : in Ada.Streams.Stream_Element_Array)
   is
      pragma Unreferenced (Stream, Item);
   begin
      raise Program_Error with "Cannot write to memory stream";
   end Write;



   overriding procedure Apply
     (Object : in Extended;
      Output : in out Ada.Streams.Root_Stream_Type'Class;
      Data : in Ada.Streams.Stream_Element_Array)
   is
      pragma Unreferenced (Object);

      Stream : Memory_Stream
        := (Ada.Streams.Root_Stream_Type with
            Size => Data'Length, Data => Data, Cursor => 0);
      Rendered, Summary : Natools.S_Expressions.Atom_Refs.Immutable_Reference;
   begin
      Lithium.Markdown.Extended.Render (Stream, Rendered, Summary);
      Output.Write (Rendered.Query);
   end Apply;


   function Create_Extended
     (Arguments : in out Natools.S_Expressions.Lockable.Descriptor'Class)
     return Natools.Web.Filters.Filter'Class
   is
      pragma Unreferenced (Arguments);
   begin
      return Extended'(null record);
   end Create_Extended;

end Lithium.Markdown.Filters;
