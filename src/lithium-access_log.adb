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

------------------------------------------------------------------------------
-- This access log implementation uses a straightforward single-table       --
-- SQLite 3 backend.                                                        --
------------------------------------------------------------------------------


with Ada.Containers.Doubly_Linked_Lists;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with AWS.Headers;
with AWS.Messages;
with Interfaces.C;
with Natools.Web;
with SQLite3;

package body Lithium.Access_Log is

   subtype String_Holder is Ada.Strings.Unbounded.Unbounded_String;
   function Hold (Value : in String) return String_Holder
     renames Ada.Strings.Unbounded.To_Unbounded_String;
   function To_String (Holder : in String_Holder) return String
     renames Ada.Strings.Unbounded.To_String;
   function Is_Empty (Holder : in String_Holder) return Boolean
     is (Ada.Strings.Unbounded.Length (Holder) = 0);
   Empty_Holder : constant String_Holder
     := Ada.Strings.Unbounded.Null_Unbounded_String;

   package String_Tables is
      type Enum is
        (Peer_Name, Method, Path, Http_Version, Referrer,
         User_Agent, Cookies, Host, Real_IP, Forwarded_For);
   end String_Tables;

   type Holder_Array is array (String_Tables.Enum) of String_Holder;

   type Extended_Log_Entry (Is_Empty : Boolean := True) is record
      case Is_Empty is
         when True => null;
         when False =>
            Strings : Holder_Array;
            Status_Code : Integer;
            Bytes : Long_Integer;
            Build_Time : Duration;
            Export_Time : Duration;
      end case;
   end record;

   subtype Log_Entry is Extended_Log_Entry (Is_Empty => False);

   SQLite_Error : exception;

   package Log_Queue is new Ada.Containers.Doubly_Linked_Lists (Log_Entry);

   Create_SQL : constant String := "CREATE TABLE IF NOT EXISTS access ("
     & "time NOT NULL DEFAULT CURRENT_TIMESTAMP, "
     & "peer_name TEXT, "
     & "method TEXT, "
     & "path TEXT, "
     & "http_version TEXT, "
     & "status_code INTEGER, "
     & "bytes INTEGER, "
     & "referrer TEXT, "
     & "user_agent TEXT, "
     & "cookies TEXT, "
     & "build_time REAL, "
     & "export_time REAL, "
     & "host TEXT, "
     & "real_ip TEXT, "
     & "forwarded_for TEXT);";

   Insert_SQL : constant String := "INSERT INTO access "
     & "(peer_name, method, path, http_version, status_code, bytes, referrer, "
     & "user_agent, cookies, build_time, export_time, "
     & "host, real_ip, forwarded_for) "
     & "VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10, ?11, ?12, ?13, ?14);";

   procedure Bind
     (Stmt : in out SQLite3.SQLite3_Statement;
      Values : in Log_Entry);

   procedure Initialize
     (Handle : in out SQLite3.SQLite3_DB;
      Name : in String);

   procedure Run_SQL
     (Handle : in SQLite3.SQLite3_DB;
      Stmt : in out SQLite3.SQLite3_Statement;
      Stmt_Ready : in out Boolean;
      Input : in Log_Entry;
      SQL_String : in String);
      --  Run one attempt of the given statement and handle errors


   protected Queue is
      entry Append (Values : in Log_Entry);
      procedure Next (Values : out Extended_Log_Entry);
   private
      Task_Waiting : Boolean := True;
      List : Log_Queue.List;
   end Queue;

   task Worker is
      entry Run (Values : in Log_Entry);
   end Worker;



   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   procedure Bind
     (Stmt : in out SQLite3.SQLite3_Statement;
      Values : in Log_Entry)
   is
      use type SQLite3.Error_Code;

      generic
         type Value_Type (<>) is limited private;
         with procedure SQLite3_Bind
           (Stmt : in SQLite3.SQLite3_Statement;
            Index : in SQLite3.SQL_Parameter_Index;
            Value : in Value_Type;
            Status : out SQLite3.Error_Code);
      procedure Generic_Bind
        (Index : in SQLite3.SQL_Parameter_Index;
         Value : in Value_Type;
         Name : in String);

      procedure Bind
        (Index : in SQLite3.SQL_Parameter_Index;
         Value : in String_Holder;
         Name : in String);

      Status : SQLite3.Error_Code;

      procedure Generic_Bind
        (Index : in SQLite3.SQL_Parameter_Index;
         Value : in Value_Type;
         Name : in String) is
      begin
         SQLite3_Bind (Stmt, Index, Value, Status);

         if Status /= SQLite3.SQLITE_OK then
            Natools.Web.Log
              (Natools.Web.Severities.Error,
               "Unable to bind " & Name & " to statement: "
               & SQLite3.Error_Code'Image (Status));
            raise SQLite_Error;
         end if;
      end Generic_Bind;

      procedure Bind is new Generic_Bind (String, SQLite3.Bind);
      procedure Bind is new Generic_Bind (Long_Integer, SQLite3.Bind);
      procedure Bind is new Generic_Bind (Interfaces.C.double, SQLite3.Bind);

      procedure Bind
        (Index : in SQLite3.SQL_Parameter_Index;
         Value : in String_Holder;
         Name : in String) is
      begin
         if not Is_Empty (Value) then
            Bind (Index, To_String (Value), Name);
         end if;
      end Bind;

      use String_Tables;
   begin
      Bind (1, Values.Strings (Peer_Name), "peer name");
      Bind (2, Values.Strings (Method), "method");
      Bind (3, Values.Strings (Path), "path");
      Bind (4, Values.Strings (Http_Version), "HTTP version");
      Bind (5, Long_Integer (Values.Status_Code), "status code");
      Bind (6, Values.Bytes, "response size");
      Bind (7, Values.Strings (Referrer), "response size");
      Bind (8, Values.Strings (User_Agent), "response size");
      Bind (9, Values.Strings (Cookies), "response size");
      Bind (10, Interfaces.C.double (Values.Build_Time), "build time");
      Bind (11, Interfaces.C.double (Values.Export_Time), "export time");
      Bind (12, Values.Strings (Host), "host");
      Bind (13, Values.Strings (Real_IP), "real IP");
      Bind (14, Values.Strings (Forwarded_For), "forwarded for");
   end Bind;


   procedure Initialize
     (Handle : in out SQLite3.SQLite3_DB;
      Name : in String)
   is
      use type SQLite3.Error_Code;
      Status : SQLite3.Error_Code;
      Stmt : SQLite3.SQLite3_Statement;
   begin
      SQLite3.Open (Name, Handle, Status);

      if Status /= SQLite3.SQLITE_OK then
         Natools.Web.Log
           (Natools.Web.Severities.Critical,
            "Unable to open """ & Name & """: "
            & SQLite3.Error_Code'Image (Status));
         raise SQLite_Error;
      end if;

      SQLite3.Prepare (Handle, Create_SQL, Stmt, Status);

      if Status /= SQLite3.SQLITE_OK then
         Natools.Web.Log
           (Natools.Web.Severities.Error,
            "Unable to prepare create statement: "
            & SQLite3.Error_Code'Image (Status)
            & ' ' & SQLite3.Error_Message (Handle));
         SQLite3.Close (Handle, Status);
         raise SQLite_Error;
      end if;

      loop
         SQLite3.Step (Stmt, Status);
         exit when Status = SQLite3.SQLITE_DONE;

         if Status /= SQLite3.SQLITE_ROW then
            Natools.Web.Log
              (Natools.Web.Severities.Error,
               "Unable to create: " & SQLite3.Error_Code'Image (Status)
               & ' ' & SQLite3.Error_Message (Handle));
            SQLite3.Close (Handle, Status);
            raise SQLite_Error;
         end if;
      end loop;

      SQLite3.Finish (Stmt, Status);

      if Status /= SQLite3.SQLITE_OK then
         Natools.Web.Log
           (Natools.Web.Severities.Error,
            "Unable to finish create statement: "
            & SQLite3.Error_Code'Image (Status)
            & ' ' & SQLite3.Error_Message (Handle));
         SQLite3.Close (Handle, Status);
         raise SQLite_Error;
      end if;
   end Initialize;


   procedure Run_SQL
     (Handle : in SQLite3.SQLite3_DB;
      Stmt : in out SQLite3.SQLite3_Statement;
      Stmt_Ready : in out Boolean;
      Input : in Log_Entry;
      SQL_String : in String)
   is
      use type SQLite3.Error_Code;
      Status : SQLite3.Error_Code;
      Retry_Left : Natural := 16;
   begin
      Retry_Loop :
      loop
         if not Stmt_Ready then
            SQLite3.Prepare (Handle, SQL_String, Stmt, Status);

            if Status /= SQLite3.SQLITE_OK then
               raise SQLite_Error with
                  "Unable to prepare insert statement: "
                  & SQLite3.Error_Code'Image (Status)
                  & ' ' & SQLite3.Error_Message (Handle);
            end if;

            Stmt_Ready := True;
         end if;

         Run_Statement :
         begin
            Bind (Stmt, Input);

            SQL_Step :
            loop
               SQLite3.Step (Stmt, Status);
               exit SQL_Step when Status = SQLite3.SQLITE_DONE;

               if Status /= SQLite3.SQLITE_ROW then
                  raise SQLite_Error with
                     "Unable to insert: " & SQLite3.Error_Code'Image (Status)
                     & ' ' & SQLite3.Error_Message (Handle);
               end if;
            end loop SQL_Step;

            SQLite3.Reset (Stmt, Status);

            if Status /= SQLite3.SQLITE_OK then
               raise SQLite_Error with
                  "Unable to reset insert statement: "
                  & SQLite3.Error_Code'Image (Status)
                  & ' ' & SQLite3.Error_Message (Handle);
            end if;

            SQLite3.Clear_Bindings (Stmt, Status);

            if Status /= SQLite3.SQLITE_OK then
               raise SQLite_Error with
                  "Unable to reset insert statement: "
                  & SQLite3.Error_Code'Image (Status)
                  & ' ' & SQLite3.Error_Message (Handle);
            end if;

         exception
            when Ex : SQLite_Error =>
               Natools.Web.Log
                 (Natools.Web.Severities.Error,
                  Ada.Exceptions.Exception_Information (Ex));
               SQLite3.Finish (Stmt, Status);
               Stmt_Ready := False;

               Retry_Left := Retry_Left - 1;
               if Retry_Left > 0 then
                  delay 1.0;
               else
                  raise;
               end if;
         end Run_Statement;

         exit Retry_Loop when Stmt_Ready;
      end loop Retry_Loop;
   end Run_SQL;



   ----------------------
   -- Public Interface --
   ----------------------

   procedure Log
     (Request : in AWS.Status.Data;
      Response : in AWS.Response.Data;
      Build_Time, Export_Time : in Duration)
   is
      function Hold_Header (Name : in String) return String_Holder;

      Headers : constant AWS.Headers.List := AWS.Status.Header (Request);

      function Hold_Header (Name : in String) return String_Holder is
      begin
         if AWS.Headers.Exist (Headers, Name) then
            return Hold (AWS.Headers.Get_Values (Headers, Name));
         else
            return Empty_Holder;
         end if;
      end Hold_Header;

      use String_Tables;
   begin
      Queue.Append
       ((Is_Empty => False,
         Strings =>
           (Peer_Name => Hold (AWS.Status.Peername (Request)),
            Method => Hold (AWS.Status.Method (Request)),
            Path => Hold (AWS.Status.URI (Request)),
            Http_Version => Hold (AWS.Status.HTTP_Version (Request)),
            Referrer => Hold_Header ("Referer"),
            User_Agent => Hold_Header ("User-Agent"),
            Cookies => Hold_Header ("Cookie"),
            Host => Hold_Header ("Host"),
            Real_IP => Hold_Header ("X-Real-IP"),
            Forwarded_For => Hold_Header ("X-Forwarded-For")),
         Status_Code => Integer'Value (AWS.Messages.Image
           (AWS.Response.Status_Code (Response))),
         Bytes => Long_Integer (AWS.Response.Content_Length (Response)),
         Build_Time => Build_Time,
         Export_Time => Export_Time));
   end Log;



   ---------------------
   -- Log Entry Queue --
   ---------------------

   protected body Queue is

      entry Append (Values : in Log_Entry) when True is
      begin
         if Task_Waiting then
            Task_Waiting := False;
            requeue Worker.Run;
         else
            List.Append (Values);
         end if;
      end Append;

      procedure Next (Values : out Extended_Log_Entry) is
      begin
         if List.Is_Empty then
            Task_Waiting := True;
            Values := (Is_Empty => True);
         else
            pragma Assert (not Task_Waiting);
            Values := List.First_Element;
            List.Delete_First;
         end if;
      end Next;
   end Queue;



   -----------------
   -- Worker Task --
   -----------------

   task body Worker is
      use type SQLite3.Error_Code;
      Status : SQLite3.Error_Code;
      Current : Extended_Log_Entry;
      Handle : SQLite3.SQLite3_DB;
      Stmt : SQLite3.SQLite3_Statement;
      Stmt_Ready : Boolean := False;
   begin
      select
         accept Run (Values : in Log_Entry) do
            Current := Values;
         end Run;
      or
         terminate;
      end select;

      pragma Assert (not Current.Is_Empty);

      Initialize (Handle, "access.dat");
      SQLite3.Busy_Timeout (Handle, 60_000, Status);

      Main_Loop :
      loop
         Run_SQL (Handle, Stmt, Stmt_Ready, Current, Insert_SQL);

         Queue.Next (Current);

         if Current.Is_Empty then
            select
               accept Run (Values : in Log_Entry) do
                  Current := Values;
               end Run;
            or
               terminate;
            end select;
         end if;
      end loop Main_Loop;
   exception
      when Ex : others =>
         if not Natools.Web."="
           (Natools.Web.Log, Natools.Web.Default_Log'Access)
         then
            Natools.Web.Log
              (Natools.Web.Severities.Critical,
               "Exception raised in Lithium.Access_Log.Worker task");
            Natools.Web.Log
              (Natools.Web.Severities.Critical,
               Ada.Exceptions.Exception_Information (Ex));
         else
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Current_Error,
               "Exception raised in Lithium.Access_Log.Worker task");
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Current_Error,
               Ada.Exceptions.Exception_Information (Ex));
         end if;
   end Worker;
end Lithium.Access_Log;
