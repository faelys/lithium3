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

package body Lithium.Legacy_Filters is

   Open_Paragraph : constant Ada.Streams.Stream_Element_Array
     := (1 => Character'Pos ('<'),
         2 => Character'Pos ('p'),
         3 => Character'Pos ('>'));

   Close_Paragraph : constant Ada.Streams.Stream_Element_Array
     := (1 => Character'Pos ('<'),
         2 => Character'Pos ('/'),
         3 => Character'Pos ('p'),
         4 => Character'Pos ('>'),
         5 => 10);

   Open_Emphasis : constant Ada.Streams.Stream_Element_Array
     := (1 => Character'Pos ('<'),
         2 => Character'Pos ('s'),
         3 => Character'Pos ('t'),
         4 => Character'Pos ('r'),
         5 => Character'Pos ('o'),
         6 => Character'Pos ('n'),
         7 => Character'Pos ('g'),
         8 => Character'Pos ('>'));

   Close_Emphasis : constant Ada.Streams.Stream_Element_Array
     := (1 => Character'Pos ('<'),
         2 => Character'Pos ('/'),
         3 => Character'Pos ('s'),
         4 => Character'Pos ('t'),
         5 => Character'Pos ('r'),
         6 => Character'Pos ('o'),
         7 => Character'Pos ('n'),
         8 => Character'Pos ('g'),
         9 => Character'Pos ('>'));

   Open_Anchor : constant Ada.Streams.Stream_Element_Array
     := (1 => Character'Pos ('['),
         2 => Character'Pos ('<'),
         3 => Character'Pos ('a'),
         4 => Character'Pos (' '),
         5 => Character'Pos ('h'),
         6 => Character'Pos ('r'),
         7 => Character'Pos ('e'),
         8 => Character'Pos ('f'),
         9 => Character'Pos ('='),
         10 => Character'Pos ('"'));

   Close_Link : constant Ada.Streams.Stream_Element_Array
     := (1 => Character'Pos ('"'),
         2 => Character'Pos ('>'));

   Close_Anchor : constant Ada.Streams.Stream_Element_Array
     := (1 => Character'Pos ('<'),
         2 => Character'Pos ('/'),
         3 => Character'Pos ('a'),
         4 => Character'Pos ('>'),
         5 => Character'Pos (']'));

   Line_Break : constant Ada.Streams.Stream_Element_Array
     := (1 => Character'Pos ('<'),
         2 => Character'Pos ('b'),
         3 => Character'Pos ('r'),
         4 => Character'Pos ('>'),
         5 => 10);

   Greater_Than : constant Ada.Streams.Stream_Element_Array
     := (1 => Character'Pos ('&'),
         2 => Character'Pos ('g'),
         3 => Character'Pos ('t'),
         4 => Character'Pos (';'));

   Less_Than : constant Ada.Streams.Stream_Element_Array
     := (1 => Character'Pos ('&'),
         2 => Character'Pos ('l'),
         3 => Character'Pos ('t'),
         4 => Character'Pos (';'));

   Quote : constant Ada.Streams.Stream_Element_Array
     := (1 => Character'Pos ('&'),
         2 => Character'Pos ('q'),
         3 => Character'Pos ('u'),
         4 => Character'Pos ('o'),
         5 => Character'Pos ('t'),
         6 => Character'Pos (';'));


   function Is_Space (Octet : Ada.Streams.Stream_Element) return Boolean
     is (Octet in 9 | 10 | 13 | 32);

   function Is_URI_Char (Octet : Ada.Streams.Stream_Element) return Boolean
     is (Octet in Character'Pos ('a') .. Character'Pos ('z')
                | Character'Pos ('A') .. Character'Pos ('Z')
                | Character'Pos ('0') .. Character'Pos ('9')
                | Character'Pos ('-') | Character'Pos ('_')
                | Character'Pos ('.') | Character'Pos ('~')
         | Character'Pos ('!') | Character'Pos ('*') | Character'Pos (''')
         | Character'Pos ('(') | Character'Pos (')') | Character'Pos (';')
         | Character'Pos (':') | Character'Pos ('@') | Character'Pos ('&')
         | Character'Pos ('=') | Character'Pos ('+') | Character'Pos ('$')
         | Character'Pos (',') | Character'Pos ('/') | Character'Pos ('?')
         | Character'Pos ('%') | Character'Pos ('#') | Character'Pos ('[')
         | Character'Pos (']'));

   function Has_Allowed_URI_Prefix
     (Fragment : in Ada.Streams.Stream_Element_Array)
     return Boolean;
      --  Return whether Fragment starts with an allowed URI prefix (scheme)

   procedure Check_URI
     (Data : in Ada.Streams.Stream_Element_Array;
      Found : out Boolean;
      Last : out Ada.Streams.Stream_Element_Offset);
      --  Return whether Data starts with a valid URI followed by ']'
      --  and what is the position of the clsoing mark.



   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   function Has_Allowed_URI_Prefix
     (Fragment : in Ada.Streams.Stream_Element_Array)
     return Boolean
   is
      use type Ada.Streams.Stream_Element;
      use type Ada.Streams.Stream_Element_Offset;
   begin
      return
        (Fragment'Length > 2
           and then Fragment (Fragment'First) = Character'Pos ('/')
           and then Fragment (Fragment'First + 1) = Character'Pos ('/'))
      or else
        (Fragment'Length > 8
           and then Fragment (Fragment'First)
              in Character'Pos ('h') | Character'Pos ('H')
           and then Fragment (Fragment'First + 1)
              in Character'Pos ('t') | Character'Pos ('T')
           and then Fragment (Fragment'First + 2)
              in Character'Pos ('t') | Character'Pos ('T')
           and then Fragment (Fragment'First + 3)
              in Character'Pos ('p') | Character'Pos ('P')
           and then ((Fragment (Fragment'First + 4) = Character'Pos (':')
                 and then Fragment (Fragment'First + 5) = Character'Pos ('/')
                 and then Fragment (Fragment'First + 6) = Character'Pos ('/'))
              or else (Fragment (Fragment'First + 4)
                    in Character'Pos ('s') | Character'Pos ('S')
                 and then Fragment (Fragment'First + 5) = Character'Pos (':')
                 and then Fragment (Fragment'First + 6) = Character'Pos ('/')
                 and then Fragment (Fragment'First + 7)
                    = Character'Pos ('/'))));
   end Has_Allowed_URI_Prefix;


   procedure Check_URI
     (Data : in Ada.Streams.Stream_Element_Array;
      Found : out Boolean;
      Last : out Ada.Streams.Stream_Element_Offset)
   is
      use type Ada.Streams.Stream_Element;
   begin
      Found := False;

      if not Has_Allowed_URI_Prefix (Data) then
         return;
      end if;

      for I in Data'Range loop
         exit when Data (I) = Character'Pos (']');
         Last := I;

         if not Is_URI_Char (Data (I)) then
            return;
         end if;
      end loop;

      Found := True;
   end Check_URI;



   ----------------------
   -- Public Interface --
   ----------------------

   overriding procedure Apply
     (Object : in Filter;
      Output : in out Ada.Streams.Root_Stream_Type'Class;
      Data : in Ada.Streams.Stream_Element_Array)
   is
      pragma Unreferenced (Object);
      use type Ada.Streams.Stream_Element_Offset;

      procedure Catch_Up (To : in Ada.Streams.Stream_Element_Offset);

      First : Ada.Streams.Stream_Element_Offset := Data'First;
      Last : Ada.Streams.Stream_Element_Offset := Data'Last;
      Next : Ada.Streams.Stream_Element_Offset;

      procedure Catch_Up (To : in Ada.Streams.Stream_Element_Offset) is
      begin
         if Next < To then
            Output.Write (Data (Next .. To - 1));
         end if;

         Next := To + 1;
      end Catch_Up;

      End_Of_Line : Boolean := False;
      In_Paragraph : Boolean := True;
      In_Emphasis : Boolean := False;
      In_Link : Boolean := False;
   begin
      Trim_Beginning :
      while First in Data'Range and then Data (First) in 9 | 10 | 13 | 32 loop
         First := First + 1;
      end loop Trim_Beginning;

      Trim_End :
      while Last in Data'Range and then Data (Last) in 9 | 10 | 13 | 32 loop
         Last := Last - 1;
      end loop Trim_End;

      Next := First;

      Output.Write (Open_Paragraph);

      for I in First .. Last loop
         case Data (I) is
            when 13 =>  --  CR
               null;

            when 10 =>  --  LF
               if End_Of_Line then
                  In_Paragraph := False;
                  End_Of_Line := False;
               else
                  End_Of_Line := True;
               end if;

            when others =>
               if End_Of_Line then
                  Output.Write (Line_Break);
                  End_Of_Line := False;
               end if;

               if not In_Paragraph then
                  if In_Emphasis then
                     Output.Write (Close_Emphasis);
                     In_Emphasis := False;
                  end if;

                  Output.Write (Close_Paragraph);
                  Output.Write (Open_Paragraph);
                  In_Paragraph := True;
               end if;
         end case;

         case Data (I) is
            when 10 | 13 =>
               Catch_Up (I);

            when Character'Pos ('"') =>
               Catch_Up (I);
               Output.Write (Quote);

            when Character'Pos ('<') =>
               Catch_Up (I);
               Output.Write (Less_Than);

            when Character'Pos ('>') =>
               Catch_Up (I);
               Output.Write (Greater_Than);

            when Character'Pos ('*') =>
               if In_Emphasis then
                  Catch_Up (I);
                  Output.Write (Close_Emphasis);
                  In_Emphasis := False;
               elsif (I = Data'First or else Is_Space (Data (I - 1)))
                 and then I < Data'Last
                 and then not Is_Space (Data (I + 1))
               then
                  Catch_Up (I);
                  Output.Write (Open_Emphasis);
                  In_Emphasis := True;
               end if;

            when Character'Pos ('[') =>
               declare
                  Found : Boolean;
                  Last : Ada.Streams.Stream_Element_Offset;
               begin
                  Check_URI (Data (I + 1 .. Data'Last), Found, Last);

                  if Found then
                     Catch_Up (I);
                     Output.Write (Open_Anchor);
                     Output.Write (Data (I + 1 .. Last));
                     Output.Write (Close_Link);
                     In_Link := True;
                  end if;
               end;

            when Character'Pos (']') =>
               if In_Link then
                  Catch_Up (I);
                  Output.Write (Close_Anchor);
                  In_Link := False;
               end if;

            when others => null;
         end case;
      end loop;

      Catch_Up (Last + 1);

      if In_Emphasis then
         Output.Write (Close_Emphasis);
      end if;

      if In_Paragraph then
         Output.Write (Close_Paragraph);
      end if;
   end Apply;


   function Create
     (Arguments : in out Natools.S_Expressions.Lockable.Descriptor'Class)
     return Natools.Web.Filters.Filter'Class
   is
      pragma Unreferenced (Arguments);
   begin
      return Filter'(null record);
   end Create;

end Lithium.Legacy_Filters;
