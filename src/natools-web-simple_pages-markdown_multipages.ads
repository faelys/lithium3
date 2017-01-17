------------------------------------------------------------------------------
-- Copyright (c) 2016-2017, Natacha Porté                                   --
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
-- Natools.Web.Simple_Pages.Markdown_Multipages extends the markdown_page   --
-- idea by gather several related pages into a single file, separated by    --
-- boundary lines containing only alternating '+' and '-'.                  --
-- Unlike Markdown_Pages where the leading atom is ignored, here it is used --
-- to override the path rendered in templated ("Web_Path") and/or the path  --
-- under which the simple page is regesterd ("Key_Path"), according to the  --
-- following syntax:                                                        --
--  * an atom with a leading '+' has both Web_Path and Key_Path as the      --
--    root multipage path followed by the suffix after '+',                 --
--  * an atom with a leading '-' has the Web_Path overridden by the suffix  --
--    after '-' and is not registerd as responding to any path in the site, --
--  * an atom with a leading '#' has the Web_Path of the root multipage     --
--    followed by the suffix after '#' and is not registered in the site,   --
--  * other atoms override both the Web_Path and the Key_Path.              --
-- So for example, a bunch of pages can be simply merged using unprefixed   --
-- atoms. A page made of several subcomponents that don't have any          --
-- dedicated path but behave as self-contained entities in the rendering    --
-- system can be made with the index page marked as "+" (with an empty      --
-- suffix), and subcomponents as "#subanchor" so that they Web_Path still   --
-- points to the relevant subcomponent.                                     --
------------------------------------------------------------------------------

with Natools.S_Expressions;
with Natools.Web.Sites;

private with Natools.S_Expressions.Atom_Refs;

package Natools.Web.Simple_Pages.Markdown_Multipages is

   type Loader is new Sites.Page_Loader with private;

   overriding procedure Load
     (Object : in out Loader;
      Builder : in out Sites.Site_Builder;
      Path : in S_Expressions.Atom);

   function Create (File : in S_Expressions.Atom)
     return Sites.Page_Loader'Class;

private

   type Loader is new Sites.Page_Loader with record
      File_Path : S_Expressions.Atom_Refs.Immutable_Reference;
   end record;

end Natools.Web.Simple_Pages.Markdown_Multipages;
