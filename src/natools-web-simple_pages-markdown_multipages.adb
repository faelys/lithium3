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

with Ada.Streams.Stream_IO;
with Natools.File_Streams;
with Natools.S_Expressions.Atom_Buffers;
with Natools.S_Expressions.Atom_Ref_Constructors;
with Natools.S_Expressions.Lockable;
with Natools.String_Slices;

with Lithium.Line_Parsers;
with Lithium.Markdown;

package body Natools.Web.Simple_Pages.Markdown_Multipages is

   procedure Insert_Text
     (Mutator : in Data_Refs.Mutator;
      Name : in String;
      Text : in S_Expressions.Atom_Refs.Immutable_Reference);

   function Is_Boundary (Line : S_Expressions.Atom) return Boolean;

   function Is_Boundary (Line : S_Expressions.Atom_Buffers.Atom_Buffer)
     return Boolean;

   function Key_Path (Path, Spec : S_Expressions.Atom)
     return S_Expressions.Atom;

   function To_Slice (Buffer : S_Expressions.Atom_Buffers.Atom_Buffer)
     return Natools.String_Slices.Slice;

   function Web_Path (Path, Spec : S_Expressions.Atom)
     return S_Expressions.Atom_Refs.Immutable_Reference;



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


   function Is_Boundary (Line : S_Expressions.Atom) return Boolean is
      use type S_Expressions.Octet;
      use type S_Expressions.Offset;
      I : S_Expressions.Offset;
   begin
      if Line'Length = 0 or else Line (Line'First)
        not in Character'Pos ('-') | Character'Pos ('+')
      then
         return False;
      end if;

      I := Line'First + 1;
      while I in Line'Range
        and then Line (I) = Character'Pos ('-') + Character'Pos ('+')
                              - Line (I - 1)
      loop
         I := I + 1;
      end loop;

      return I > Line'First + 2
        and then (I > Line'Last or else Line (I) in 10 | 13);
   end Is_Boundary;


   function Is_Boundary (Line : S_Expressions.Atom_Buffers.Atom_Buffer)
     return Boolean is
   begin
      return Is_Boundary (Line.Raw_Query.Data.all);
   end Is_Boundary;


   function Key_Path (Path, Spec : S_Expressions.Atom)
     return S_Expressions.Atom
   is
      use type S_Expressions.Atom;
      use type S_Expressions.Offset;
   begin
      case Spec (Spec'First) is
         when Character'Pos ('+') =>
            return Path & Spec (Spec'First + 1 .. Spec'Last);
         when Character'Pos ('-') | Character'Pos ('#') =>
            return S_Expressions.Null_Atom;
         when others =>
            return Spec;
      end case;
   end Key_Path;


   function To_Slice (Buffer : S_Expressions.Atom_Buffers.Atom_Buffer)
     return Natools.String_Slices.Slice
   is
      procedure Initialize (S : out String);

      procedure Initialize (S : out String) is
         use type S_Expressions.Offset;
         Accessor : constant S_Expressions.Atom_Refs.Accessor
           := Buffer.Raw_Query;
         I : S_Expressions.Offset := Accessor.Data.all'First;
      begin
         for O in S'Range loop
            S (O) := Character'Val (Accessor.Data (I));
            I := I + 1;
         end loop;
      end Initialize;
   begin
      return Natools.String_Slices.New_Slice
        (1, Natural (Buffer.Length), Initialize'Access);
   end To_Slice;


   function Web_Path (Path, Spec : S_Expressions.Atom)
     return S_Expressions.Atom_Refs.Immutable_Reference
   is
      use type S_Expressions.Atom;
      use type S_Expressions.Offset;
   begin
      case Spec (Spec'First) is
         when Character'Pos ('+') | Character'Pos ('#') =>
            return S_Expressions.Atom_Ref_Constructors.Create
              (Path & Spec (Spec'First + 1 .. Spec'Last));
         when Character'Pos ('-') =>
            return S_Expressions.Atom_Ref_Constructors.Create
              (Spec (Spec'First + 1 .. Spec'Last));
         when others =>
            return S_Expressions.Atom_Ref_Constructors.Create (Spec);
      end case;
   end Web_Path;


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
      use type S_Expressions.Events.Event;

      Stream : aliased File_Streams.File_Stream := File_Streams.Open
        (Ada.Streams.Stream_IO.In_File,
         S_Expressions.To_String (Object.File_Path.Query));
      Parser : Lithium.Line_Parsers.Parser (Stream'Access);
      Lock : S_Expressions.Lockable.Lock_State;
      Text : S_Expressions.Atom_Refs.Immutable_Reference;
      Summary : S_Expressions.Atom_Refs.Immutable_Reference;
      Line, Markdown : S_Expressions.Atom_Buffers.Atom_Buffer;
      Event : S_Expressions.Events.Event;
      Finished : Boolean;
   begin
      Read_Pages :
      loop
         Parser.Next (Event);
         exit Read_Pages when Event /= S_Expressions.Events.Open_List;
         Parser.Lock (Lock);
         Parser.Next (Event);
         exit Read_Pages when Event /= S_Expressions.Events.Add_Atom;

         declare
            Path_Spec : constant S_Expressions.Atom := Parser.Current_Atom;
            Page : constant Page_Ref := Create (Parser);
         begin
            Markdown.Soft_Reset;
            Read_Markdown :
            loop
               Line.Soft_Reset;
               Lithium.Line_Parsers.Append_Line (Line, Stream, Finished);

               exit Read_Markdown when Is_Boundary (Line);
               Markdown.Append (Line.Data);
               exit Read_Markdown when Finished;
            end loop Read_Markdown;

            Lithium.Markdown.Extended.Render
              (To_Slice (Markdown), Text, Summary);

            declare
               Mutator : constant Data_Refs.Mutator := Page.Ref.Update;
            begin
               Insert_Text (Mutator, "markdown-text", Text);
               if not Summary.Is_Empty then
                  Insert_Text (Mutator, "markdown-summary", Summary);
               end if;

               Mutator.File_Path := Object.File_Path;
               Mutator.Web_Path := Web_Path (Path, Path_Spec);
            end;

            Register (Page, Builder, Key_Path (Path, Path_Spec));
         end;

         exit Read_Pages when Finished;
         Parser.Reset;
      end loop Read_Pages;
   end Load;

end Natools.Web.Simple_Pages.Markdown_Multipages;
