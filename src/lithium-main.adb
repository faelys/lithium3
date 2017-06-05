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

with Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO;
with AWS.Config;
with AWS.Server;
with Lithium.Dispatchers;
with Lithium.Log;

procedure Lithium.Main is
   WS : AWS.Server.HTTP;
   Debug : constant Boolean := Ada.Command_Line.Argument_Count >= 2;
   Handler : Lithium.Dispatchers.Handler;
begin
   Lithium.Log.Initialize (Debug);

   if Ada.Command_Line.Argument_Count >= 1 then
      Handler := Lithium.Dispatchers.Create (Ada.Command_Line.Argument (1));
   else
      Handler := Lithium.Dispatchers.Create ("site.sx");
   end if;

   AWS.Server.Start (WS, Handler, AWS.Config.Get_Current);

   if not Debug then
      AWS.Server.Wait;
   elsif Ada.Directories.Exists (Ada.Command_Line.Argument (2)) then
      Ada.Text_IO.Put_Line ("Websever started, waiting for removal of "
        & Ada.Command_Line.Argument (2));
      loop
         delay 1.0;
         exit when not Ada.Directories.Exists (Ada.Command_Line.Argument (2));
      end loop;
   else
      Ada.Text_IO.Put_Line ("Websever started, waiting for Q press");
      AWS.Server.Wait (AWS.Server.Q_Key_Pressed);
   end if;

   AWS.Server.Shutdown (WS);
   Handler.Purge;
end Lithium.Main;
