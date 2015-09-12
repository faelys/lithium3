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

with Ada.Streams.Stream_IO;
with Natools.File_Streams;
with Natools.S_Expressions.Atom_Ref_Constructors;
with Natools.S_Expressions.Lockable;

with Lithium.Line_Parsers;
with Lithium.Markdown;

package body Natools.Web.Simple_Pages.Markdown_Pages is

   procedure Insert_Text
     (Mutator : in Data_Refs.Mutator;
      Name : in String;
      Text : in S_Expressions.Atom_Refs.Immutable_Reference);



   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   procedure Insert_Text
     (Mutator : in Data_Refs.Mutator;
      Name : in String;
      Text : in S_Expressions.Atom_Refs.Immutable_Reference)
   is
      Expr : S_Expressions.Caches.Reference;
   begin
      Expr.Append_Atom (Text.Query);
      Mutator.Elements := Mutator.Elements.Include
        (S_Expressions.To_Atom (Name),
         Expr.First);
   end Insert_Text;



   ----------------------
   -- Public Interface --
   ----------------------

   function Create (File : in S_Expressions.Atom)
     return Sites.Page_Loader'Class is
   begin
      return Loader'(File_Path
        => S_Expressions.Atom_Ref_Constructors.Create (File));
   end Create;


   overriding procedure Load
     (Object : in out Loader;
      Builder : in out Sites.Site_Builder;
      Path : in S_Expressions.Atom)
   is
      Page : Page_Ref;

      Stream : aliased File_Streams.File_Stream := File_Streams.Open
        (Ada.Streams.Stream_IO.In_File,
         S_Expressions.To_String (Object.File_Path.Query));
      Parser : Lithium.Line_Parsers.Parser (Stream'Access);
      Lock : S_Expressions.Lockable.Lock_State;
      Text : S_Expressions.Atom_Refs.Immutable_Reference;
   begin
      Parser.Next;
      Parser.Lock (Lock);
      Parser.Next;
      Page := Create (Parser);

      Lithium.Markdown.Extended.Render (Stream, Text);

      declare
         Mutator : constant Data_Refs.Mutator := Page.Ref.Update;
      begin
         Insert_Text (Mutator, "markdown-text", Text);

         Mutator.File_Path := Object.File_Path;
         Mutator.Web_Path := S_Expressions.Atom_Ref_Constructors.Create (Path);
         Mutator.Comment_List.Load (Builder, Mutator.Self, Mutator.Web_Path);
      end;

      Sites.Insert (Builder, Path, Page);
      Sites.Insert (Builder, Page.Get_Tags, Page);
   end Load;

end Natools.Web.Simple_Pages.Markdown_Pages;

