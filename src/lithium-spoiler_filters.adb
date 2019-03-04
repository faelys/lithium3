------------------------------------------------------------------------------
-- Copyright (c) 2017-2019, Natacha Porté                                   --
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

package body Lithium.Spoiler_Filters is
   use type Ada.Streams.Stream_Element;

   function Next
     (Data : in Ada.Streams.Stream_Element_Array;
      Start : in Ada.Streams.Stream_Element_Offset;
      Pattern : Ada.Streams.Stream_Element_Array)
     return Ada.Streams.Stream_Element_Offset;

   function Rot_13 (Source : Ada.Streams.Stream_Element_Array)
     return Ada.Streams.Stream_Element_Array;

   function Rot_13 (Source : Ada.Streams.Stream_Element)
     return Ada.Streams.Stream_Element
     is (case Source is
         when Character'Pos ('A') .. Character'Pos ('M')
            | Character'Pos ('a') .. Character'Pos ('m')
            => Source + 13,
         when Character'Pos ('N') .. Character'Pos ('Z')
            | Character'Pos ('n') .. Character'Pos ('z')
            => Source - 13,
         when others => Source);


   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   function Next
     (Data : in Ada.Streams.Stream_Element_Array;
      Start : in Ada.Streams.Stream_Element_Offset;
      Pattern : Ada.Streams.Stream_Element_Array)
     return Ada.Streams.Stream_Element_Offset
   is
      use type Ada.Streams.Stream_Element_Array;
      use type Ada.Streams.Stream_Element_Offset;
      Index : Ada.Streams.Stream_Element_Offset := Start;
   begin
      while Index + Pattern'Length - 1 in Data'Range loop
         if Data (Index .. Index + Pattern'Length - 1) = Pattern then
            return Index;
         end if;

         Index := Index + 1;
      end loop;

      return Start - 1;
   end Next;


   function Rot_13 (Source : Ada.Streams.Stream_Element_Array)
     return Ada.Streams.Stream_Element_Array is
   begin
      return Result : Ada.Streams.Stream_Element_Array
                        (Source'First .. Source'Last)
      do
         for I in Result'Range loop
            Result (I) := Rot_13 (Source (I));
         end loop;
      end return;
   end Rot_13;



   ----------------------
   -- Public Interface --
   ----------------------

   overriding procedure Apply
     (Object : in Spoiler_Filter;
      Output : in out Ada.Streams.Root_Stream_Type'Class;
      Data : in Ada.Streams.Stream_Element_Array)
   is
      pragma Unreferenced (Object);
      use type Ada.Streams.Stream_Element_Array;
      use type Ada.Streams.Stream_Element_Offset;

      Index : Ada.Streams.Stream_Element_Offset := Data'First;
      First, Last : Ada.Streams.Stream_Element_Offset;
   begin
      while Index in Data'Range loop
         First := Next (Data, Index, Begin_HTML);
         exit when First < Index;

         Last := Next (Data, First + Begin_HTML'Length, End_HTML);
         exit when Last < First;

         if First > Index then
            Output.Write (Data (Index .. First - 1));
         end if;

         Output.Write
           (Begin_Filtered
              & Rot_13 (Data (First + Begin_HTML'Length .. Last - 1))
              & End_Filtered);

         Index := Last + End_HTML'Length;
      end loop;

      if Index in Data'Range then
         Output.Write (Data (Index .. Data'Last));
      end if;
   end Apply;


   function Create
     (Arguments : in out Natools.S_Expressions.Lockable.Descriptor'Class)
     return Natools.Web.Filters.Filter'Class
   is
      pragma Unreferenced (Arguments);
   begin
      return Spoiler_Filter'(null record);
   end Create;

end Lithium.Spoiler_Filters;
