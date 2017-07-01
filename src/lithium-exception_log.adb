------------------------------------------------------------------------------
-- Copyright (c) 2017, Natacha Porté                                        --
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

with Ada.Calendar;
with AWS.Headers;
with AWS.Messages;
with AWS.Parameters;
with Natools.S_Expressions.File_Writers;
with Natools.Time_IO.RFC_3339;
with Natools.Web;

package body Lithium.Exception_Log is

   procedure Report
     (Ex : in Ada.Exceptions.Exception_Occurrence;
      Request : in AWS.Status.Data) is
   begin
      declare
         Writer : Natools.S_Expressions.File_Writers.Writer
           := Natools.S_Expressions.File_Writers.Open_Or_Create
              ("exceptions.sx");
      begin
         Writer.Open_List;
         Writer.Append_String
           (Natools.Time_IO.RFC_3339.Image (Ada.Calendar.Clock));

         Writer.Open_List;
         Writer.Append_String (AWS.Status.Method (Request));
         Writer.Append_String (AWS.Status.URI (Request));
         Writer.Close_List;

         Write_Headers :
         declare
            Headers : constant AWS.Headers.List
              := AWS.Status.Header (Request);
         begin
            Writer.Open_List;
            Writer.Append_String ("headers");

            for I in 1 .. AWS.Headers.Count (Headers) loop
               Writer.Open_List;
               Writer.Append_String (AWS.Headers.Get_Name (Headers, I));
               Writer.Append_String (AWS.Headers.Get_Value (Headers, I));
               Writer.Close_List;
            end loop;

            Writer.Close_List;
         end Write_Headers;

         Write_Parameters :
         declare
            Parameters : constant AWS.Parameters.List
              := AWS.Status.Parameters (Request);
         begin
            Writer.Open_List;
            Writer.Append_String ("parameters");

            for I in 1 .. AWS.Parameters.Count (Parameters) loop
               Writer.Open_List;
               Writer.Append_String (AWS.Parameters.Get_Name (Parameters, I));
               Writer.Append_String (AWS.Parameters.Get_Value (Parameters, I));
               Writer.Close_List;
            end loop;

            Writer.Close_List;
         end Write_Parameters;

         Writer.Open_List;
         Writer.Append_String ("body");
         Writer.Append_Atom (AWS.Status.Binary_Data (Request));
         Writer.Close_List;

         Writer.Open_List;
         Writer.Append_String ("exception");
         Writer.Open_List;
         Writer.Append_String ("name");
         Writer.Append_String (Ada.Exceptions.Exception_Name (Ex));
         Writer.Close_List;
         Writer.Open_List;
         Writer.Append_String ("message");
         Writer.Append_String (Ada.Exceptions.Exception_Message (Ex));
         Writer.Close_List;
         Writer.Open_List;
         Writer.Append_String ("info");
         Writer.Append_String (Ada.Exceptions.Exception_Information (Ex));
         Writer.Close_List;
         Writer.Close_List;

         Writer.Close_List;
         Writer.Newline;
      end;

      Natools.Web.Log
        (Natools.Web.Severities.Error,
         "Exception " & Ada.Exceptions.Exception_Name (Ex)
           & " raised and logged");
   exception
      when Double : others =>
         Last_Chance :
         begin
            Natools.Web.Log
              (Natools.Web.Severities.Critical,
               "Exception " & Ada.Exceptions.Exception_Name (Ex)
                 & " raised but logging raised "
                 & Ada.Exceptions.Exception_Name (Double)
                 & " (" & Ada.Exceptions.Exception_Message (Double) & ")");
         exception
            when others => null;
         end Last_Chance;
   end Report;


   function Respond
     (Ex : in Ada.Exceptions.Exception_Occurrence;
      Request : in AWS.Status.Data)
     return AWS.Response.Data
   is
      pragma Unreferenced (Request);
   begin
      return AWS.Response.Build
        ("text/html",
         "<html><head><title>Error 500 - Internal Server Error</title></head>"
           & "<body><h1>Error 500 - Internal Server Error<h1><pre><code>"
           & Ada.Exceptions.Exception_Information (Ex)
           & "</code></pre></body></html>",
         AWS.Messages.S500);
   end Respond;

end Lithium.Exception_Log;
