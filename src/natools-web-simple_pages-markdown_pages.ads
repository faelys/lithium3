------------------------------------------------------------------------------
-- Copyright (c) 2015-2019, Natacha Porté                                   --
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
-- Natools.Web.Simple_Pages.Markdown_Pages extends simple pages with a new  --
-- file format, starting with the usual simple-page S-expression embeded in --
-- a list, followed by markdown text which is rendered as the element       --
-- named "markdown-text".                                                   --
------------------------------------------------------------------------------

with Natools.S_Expressions;
with Natools.Web.Sites;

private with Ada.Calendar;
private with Natools.S_Expressions.Atom_Refs;

package Natools.Web.Simple_Pages.Markdown_Pages is

   type Loader is new Sites.Page_Loader with private;

   overriding procedure Load
     (Object : in out Loader;
      Builder : in out Sites.Site_Builder;
      Path : in S_Expressions.Atom);

   not overriding procedure Force_Load
     (Object : in out Loader;
      Builder : in out Sites.Site_Builder;
      Path : in S_Expressions.Atom);

   function Create (File : in S_Expressions.Atom)
     return Sites.Page_Loader'Class;

private

   type Loader is new Sites.Page_Loader with record
      File_Path : S_Expressions.Atom_Refs.Immutable_Reference;
      File_Time : Ada.Calendar.Time;
      Cache : Page_Ref;
   end record;

end Natools.Web.Simple_Pages.Markdown_Pages;
