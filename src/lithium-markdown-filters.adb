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

with Natools.S_Expressions.Atom_Refs;
with Natools.String_Slices;

package body Lithium.Markdown.Filters is

   function To_Slice (Data : in Ada.Streams.Stream_Element_Array)
     return Natools.String_Slices.Slice;


   function To_Slice (Data : in Ada.Streams.Stream_Element_Array)
     return Natools.String_Slices.Slice
   is
      procedure Initialize (S : out String);

      procedure Initialize (S : out String) is
         O : Natural := S'First - 1;
      begin
         for I in Data'Range loop
            O := O + 1;
            S (O) := Character'Val (Data (I));
         end loop;
      end Initialize;
   begin
      return Natools.String_Slices.New_Slice
        (1, Data'Length, Initialize'Access);
   end To_Slice;



   overriding procedure Apply
     (Object : in Extended;
      Output : in out Ada.Streams.Root_Stream_Type'Class;
      Data : in Ada.Streams.Stream_Element_Array)
   is
      pragma Unreferenced (Object);

      Rendered, Summary : Natools.S_Expressions.Atom_Refs.Immutable_Reference;
   begin
      Lithium.Markdown.Extended.Render (To_Slice (Data), Rendered, Summary);
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



   overriding procedure Apply
     (Object : in Comment;
      Output : in out Ada.Streams.Root_Stream_Type'Class;
      Data : in Ada.Streams.Stream_Element_Array)
   is
      pragma Unreferenced (Object);

      Rendered : Natools.S_Expressions.Atom_Refs.Immutable_Reference;
   begin
      Lithium.Markdown.Comment.Render (To_Slice (Data), Rendered);
      Output.Write (Rendered.Query);
   end Apply;


   function Create_Comment
     (Arguments : in out Natools.S_Expressions.Lockable.Descriptor'Class)
     return Natools.Web.Filters.Filter'Class
   is
      pragma Unreferenced (Arguments);
   begin
      return Comment'(null record);
   end Create_Comment;

end Lithium.Markdown.Filters;
