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

with Ada.Text_IO;
with Syslog.Guess.App_Name;
with Syslog.Guess.Hostname;
with Syslog.Transport.Send_Task;
with Syslog.Transport.UDP;

package body Lithium.Log is

   Severity_Table : constant array (Natools.Web.Severities.Code)
     of Syslog.Severities.Code
     := (Natools.Web.Severities.Critical => Syslog.Severities.Critical,
         Natools.Web.Severities.Error    => Syslog.Severities.Error,
         Natools.Web.Severities.Warning  => Syslog.Severities.Warning,
         Natools.Web.Severities.Info     => Syslog.Severities.Informational);


   procedure Initialize (Debug : in Boolean) is
   begin
      if Debug then
         Natools.Web.Log := Lithium.Log.Text_IO_Log'Access;
      else
         Syslog.Guess.App_Name;
         Syslog.Guess.Hostname;
         Syslog.Transport.UDP.Connect ("127.0.0.1");
         Syslog.Transport.Send_Task.Set_Backend
           (Syslog.Transport.UDP.Transport);
         Syslog.Set_Transport (Syslog.Transport.Send_Task.Transport);
         Syslog.Set_Default_Facility (Syslog.Facilities.Daemon);
         Natools.Web.Log := Lithium.Log.Syslog_Log'Access;
      end if;
   end Initialize;


   procedure Syslog_Log
     (Severity : in Natools.Web.Severities.Code;
      Message : in String) is
   begin
      Syslog.Log (Severity_Table (Severity), Message);
   end Syslog_Log;


   procedure Text_IO_Log
     (Severity : in Natools.Web.Severities.Code;
      Message : in String) is
   begin
      Ada.Text_IO.Put_Line
        ('[' & Natools.Web.Severities.Code'Image (Severity) & "] "
         & Message);
   end Text_IO_Log;

end Lithium.Log;
