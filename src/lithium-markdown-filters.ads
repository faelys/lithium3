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
-- Lithium.Markdown.Filters provides implementations of web filters         --
-- performing markdown to HTML transformation.                              --
------------------------------------------------------------------------------

with Ada.Streams;
with Natools.S_Expressions.Lockable;
with Natools.Web.Filters;

package Lithium.Markdown.Filters is

   type Extended is new Natools.Web.Filters.Filter with private;

   overriding procedure Apply
     (Object : in Extended;
      Output : in out Ada.Streams.Root_Stream_Type'Class;
      Data : in Ada.Streams.Stream_Element_Array);

   function Create_Extended
     (Arguments : in out Natools.S_Expressions.Lockable.Descriptor'Class)
     return Natools.Web.Filters.Filter'Class;



   type Comment is new Natools.Web.Filters.Filter with private;

   overriding procedure Apply
     (Object : in Comment;
      Output : in out Ada.Streams.Root_Stream_Type'Class;
      Data : in Ada.Streams.Stream_Element_Array);

   function Create_Comment
     (Arguments : in out Natools.S_Expressions.Lockable.Descriptor'Class)
     return Natools.Web.Filters.Filter'Class;

private

   type Extended is new Natools.Web.Filters.Filter with null record;
   type Comment is new Natools.Web.Filters.Filter with null record;

end Lithium.Markdown.Filters;
