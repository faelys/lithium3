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

with Ada.Exceptions;
with Ada.Text_IO;
with Markup.Parsers.Markdown.Extensions;
with Markup.Renderers.Html;
with Natools.S_Expressions.Atom_Buffers;
with Natools.String_Slices;

package body Lithium.Markdown is

   package Sx renames Natools.S_Expressions;

   type Buffer_Access is access Sx.Atom_Buffers.Atom_Buffer;

   type Buffer_Copy is new Markup.Element_Callback with record
      Source, Destination : not null Buffer_Access;
   end record;

   procedure Append
     (To : in out Buffer_Access;
      Text : in String);

   function Export (Buffer : Sx.Atom_Buffers.Atom_Buffer)
     return Sx.Atom_Refs.Immutable_Reference;

   overriding procedure Open (Element : in out Buffer_Copy);
   overriding procedure Append
     (Element : in out Buffer_Copy; Text : in String)
     is null;
   overriding procedure Close (Element : in out Buffer_Copy) is null;

   package Renderers is new Markup.Renderers.Html (Buffer_Access);


   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   procedure Append
     (To : in out Buffer_Access;
      Text : in String)
   is
      pragma Unmodified (To);
   begin
      To.Append (Sx.To_Atom (Text));
   end Append;


   function Export (Buffer : Sx.Atom_Buffers.Atom_Buffer)
     return Sx.Atom_Refs.Immutable_Reference
   is
      use type Sx.Offset;

      Accessor : constant Sx.Atom_Refs.Accessor := Buffer.Raw_Query;
      New_Data : constant Sx.Atom_Refs.Data_Access
        := new Sx.Atom (1 .. Buffer.Length);
   begin
      New_Data.all := Accessor.Data.all
        (Accessor.Data.all'First
         .. Accessor.Data.all'First + New_Data'Length - 1);
      return Sx.Atom_Refs.Create (New_Data);
   end Export;


   overriding procedure Open (Element : in out Buffer_Copy) is
   begin
      Element.Destination.Soft_Reset;
      Element.Destination.Append (Element.Source.Data);
   end Open;



   -----------------
   -- Worker Tasks --
   -----------------

   task body Extended is
      Buffer : Sx.Atom_Buffers.Atom_Buffer;
      Renderer : Renderers.Renderer_Ref;
      Parser : Markup.Parsers.Markdown.Extensions.Extended_Parser;
      Parsed : constant Buffer_Access := new Sx.Atom_Buffers.Atom_Buffer;
      Summary_Buf : constant Buffer_Access := new Sx.Atom_Buffers.Atom_Buffer;
   begin
      Renderer.Set_Output (Parsed);
      Renderer.Set_Format (Renderers.Html);
      Renderer.Set_Newline (Renderers.LF);

      Parser.Atx_Header (Renderer.Header);
      Parser.Atx_Header_With_Id (Renderer.Header);
      Parser.Code_Block (Renderer.Code_Block);
      Parser.Horizontal_Rule
        (Buffer_Copy'(Source => Parsed, Destination => Summary_Buf));
      Parser.Html_Block (Renderer.Raw_Html_Block);
      Parser.Html_Tag_Block (Renderer.Raw_Html_Block);
      Parser.Html_Comment_Block (Renderer.Raw_Html_Block);
      Parser.List (Renderer.Ordered_List, Renderer.List_Item,
                   Markup.Parsers.Markdown.Styles.Ordered);
      Parser.List (Renderer.Unordered_List, Renderer.List_Item,
                   Markup.Parsers.Markdown.Styles.Unordered);
      Parser.Paragraph (Renderer.Paragraph);
      Parser.Paragraph_With_Class (Renderer.Paragraph, '(', ')');
      Parser.Quote_Block (Renderer.Quote_Block);
      Parser.Setext_Header (Renderer.Header);

      Parser.Emphasis (Renderer.Emphasis, 1);
      Parser.Emphasis (Renderer.Strong, 2);
      Parser.Emphasis (Renderer.Strong_Emphasis, 3);
      Parser.Entity (Renderer.Raw_Html_Span);
      Parser.Escape (Renderer.Raw_Text_Span);
      Parser.Code_Span (Renderer.Code_Span);
      Parser.Discount_Image (Renderer.Image);
      Parser.Auto_Link (Renderer.Anchor);
      Parser.Html_Span (Renderer.Raw_Html_Span);

      Parser.Discount_Definition_List
        (Renderer.Definition_List,
         Renderer.Definition_Title,
         Renderer.Definition_Description);
      Parser.PME_Definition_List
        (Renderer.Definition_List,
         Renderer.Definition_Title,
         Renderer.Definition_Description);
      Parser.PME_Table
        (Renderer.Table,
         Renderer.Table_Row,
         Renderer.Table_Header_Cell,
         Renderer.Table_Data_Cell);
      Parser.Discount_Centered (Renderer.Paragraph);
      Parser.Discount_Class_Block (Renderer.Division);
      Parser.Discount_Fenced_Code_Block (Renderer.Code_Block);

      Parser.Pseudoprotocol_Link (Renderer.Anchor);
      Parser.Pseudoprotocol_Abbr (Renderer.Abbreviation);
      Parser.Pseudoprotocol_Class (Renderer.Span);
      Parser.Pseudoprotocol_Id (Renderer.Anchor);
      Parser.Pseudoprotocol_Raw (Renderer.Raw_Html_Span);

      Parser.Emphasis (Renderer.Inserted, 2, "+");
      Parser.Emphasis (Renderer.Deleted, 2, "-");
      Parser.Emphasis (Renderer.Span, 1, "|");

      loop
         Buffer.Soft_Reset;
         Parsed.Soft_Reset;
         Summary_Buf.Soft_Reset;
         Parser.Reset;

         select
            accept Render
              (Source : in out Ada.Streams.Root_Stream_Type'Class;
               Output : out Sx.Atom_Refs.Immutable_Reference;
               Summary : out Sx.Atom_Refs.Immutable_Reference)
            do
               Read_Text :
               declare
                  Chunk : Ada.Streams.Stream_Element_Array (1 .. 1024);
                  Last : Ada.Streams.Stream_Element_Offset;
               begin
                  loop
                     Source.Read (Chunk, Last);
                     exit when Last not in Chunk'Range;
                     Buffer.Append (Chunk (Chunk'First .. Last));
                  end loop;
               end Read_Text;

               Parser.Process
                 (Natools.String_Slices.To_Slice (Sx.To_String (Buffer.Data)));

               Output := Export (Parsed.all);

               Export_Summary :
               declare
                  use type Sx.Count;
               begin
                  if Summary_Buf.Length > 0 then
                     Summary := Export (Summary_Buf.all);
                  else
                     Summary := Sx.Atom_Refs.Null_Immutable_Reference;
                  end if;
               end Export_Summary;
            end Render;
         or
            terminate;
         end select;
      end loop;
   exception
      when Ex : others =>
         Ada.Text_IO.Put_Line
           ("Exception raised in Lithium.Markdown.Extended task");
         Ada.Text_IO.Put_Line
           (Ada.Exceptions.Exception_Information (Ex));
   end Extended;


   task body Comment is
      Buffer : Sx.Atom_Buffers.Atom_Buffer;
      Renderer : Renderers.Renderer_Ref;
      Parser : Markup.Parsers.Markdown.Extensions.Extended_Parser;
      Parsed : constant Buffer_Access := new Sx.Atom_Buffers.Atom_Buffer;
   begin
      Renderer.Set_Output (Parsed);
      Renderer.Set_Format (Renderers.Html);
      Renderer.Set_Newline (Renderers.LF);

      Parser.Code_Block (Renderer.Code_Block);
      Parser.List (Renderer.Ordered_List, Renderer.List_Item,
                   Markup.Parsers.Markdown.Styles.Ordered);
      Parser.List (Renderer.Unordered_List, Renderer.List_Item,
                   Markup.Parsers.Markdown.Styles.Unordered);
      Parser.Paragraph (Renderer.Paragraph);
      Parser.Quote_Block (Renderer.Quote_Block);

      Parser.Emphasis (Renderer.Emphasis, 1);
      Parser.Emphasis (Renderer.Strong, 2);
      Parser.Emphasis (Renderer.Strong_Emphasis, 3);
      Parser.Entity (Renderer.Raw_Html_Span);
      Parser.Escape (Renderer.Raw_Text_Span);
      Parser.Code_Span (Renderer.Code_Span);
      Parser.Discount_Image (Renderer.Image);
      Parser.Auto_Link (Renderer.Anchor);

      Parser.PME_Table
        (Renderer.Table,
         Renderer.Table_Row,
         Renderer.Table_Header_Cell,
         Renderer.Table_Data_Cell);
      Parser.Discount_Fenced_Code_Block (Renderer.Code_Block);

      Parser.Link (Renderer.Anchor);

      Parser.Emphasis (Renderer.Inserted, 2, "+");
      Parser.Emphasis (Renderer.Deleted, 2, "-");

      loop
         Buffer.Soft_Reset;
         Parsed.Soft_Reset;
         Parser.Reset;

         select
            accept Render
              (Source : in out Ada.Streams.Root_Stream_Type'Class;
               Output : out Sx.Atom_Refs.Immutable_Reference)
            do
               Read_Text :
               declare
                  Chunk : Ada.Streams.Stream_Element_Array (1 .. 1024);
                  Last : Ada.Streams.Stream_Element_Offset;
               begin
                  loop
                     Source.Read (Chunk, Last);
                     exit when Last not in Chunk'Range;
                     Buffer.Append (Chunk (Chunk'First .. Last));
                  end loop;
               end Read_Text;

               Parser.Process
                 (Natools.String_Slices.To_Slice (Sx.To_String (Buffer.Data)));

               Output := Export (Parsed.all);
            end Render;
         or
            terminate;
         end select;
      end loop;
   exception
      when Ex : others =>
         Ada.Text_IO.Put_Line
           ("Exception raised in Lithium.Markdown.Comment task");
         Ada.Text_IO.Put_Line
           (Ada.Exceptions.Exception_Information (Ex));
   end Comment;

end Lithium.Markdown;
