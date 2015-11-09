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
with AWS.Headers;
with AWS.Messages;
with Interfaces.C;
with Natools.Web;
with SQLite3;

package body Lithium.Access_Log is

   package Stmt_Lists is new Ada.Containers.Doubly_Linked_Lists
     (SQLite3.SQLite3_Statement, SQLite3."=");

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

   Handle : SQLite3.SQLite3_DB;
   Initialized : Boolean := False;

   procedure Create_Stmt (Stmt : out SQLite3.SQLite3_Statement);
   procedure Initialize (Name : in String);

   protected Stmt_Pool is
      procedure Get (Stmt : out SQLite3.SQLite3_Statement);
      procedure Release (Stmt : out SQLite3.SQLite3_Statement);
   private
      Available : Stmt_Lists.List;
   end Stmt_Pool;


   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   procedure Initialize (Name : in String) is
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
      end if;

      SQLite3.Prepare (Handle, Create_SQL, Stmt, Status);

      if Status /= SQLite3.SQLITE_OK then
         Natools.Web.Log
           (Natools.Web.Severities.Error,
            "Unable to prepare create statement: "
            & SQLite3.Error_Code'Image (Status)
            & ' ' & SQLite3.Error_Message (Handle));
         SQLite3.Close (Handle, Status);
         return;
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
            return;
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
         return;
      end if;

      Initialized := True;
   end Initialize;


   procedure Create_Stmt (Stmt : out SQLite3.SQLite3_Statement) is
      use type SQLite3.Error_Code;
      Status : SQLite3.Error_Code;
   begin
      SQLite3.Prepare (Handle, Insert_SQL, Stmt, Status);

      if Status /= SQLite3.SQLITE_OK then
         Natools.Web.Log
           (Natools.Web.Severities.Error,
            "Unable to prepare insert statement: "
            & SQLite3.Error_Code'Image (Status)
            & ' ' & SQLite3.Error_Message (Handle));
         raise Constraint_Error with "Invalid SQL statement";
      end if;
   end Create_Stmt;



   ----------------------
   -- Public Interface --
   ----------------------

   procedure Log
     (Request : in AWS.Status.Data;
      Response : in AWS.Response.Data;
      Build_Time, Export_Time : in Duration)
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

      procedure Bind_Header
        (Index : in SQLite3.SQL_Parameter_Index;
         Name : in String);

      Stmt : SQLite3.SQLite3_Statement;
      Status : SQLite3.Error_Code;
      Failed : Boolean := False;

      procedure Generic_Bind
        (Index : in SQLite3.SQL_Parameter_Index;
         Value : in Value_Type;
         Name : in String) is
      begin
         if Failed then
            return;
         end if;

         SQLite3_Bind (Stmt, Index, Value, Status);

         if Status /= SQLite3.SQLITE_OK then
            Natools.Web.Log
              (Natools.Web.Severities.Error,
               "Unable to bind " & Name & " to statement: "
               & SQLite3.Error_Code'Image (Status));
            Failed := True;
         end if;
      end Generic_Bind;

      procedure Bind is new Generic_Bind (String, SQLite3.Bind);
      procedure Bind is new Generic_Bind (Long_Integer, SQLite3.Bind);
      procedure Bind is new Generic_Bind (Interfaces.C.double, SQLite3.Bind);

      Headers : constant AWS.Headers.List := AWS.Status.Header (Request);

      procedure Bind_Header
        (Index : in SQLite3.SQL_Parameter_Index;
         Name : in String) is
      begin
         if AWS.Headers.Exist (Headers, Name) then
            Bind (Index, AWS.Headers.Get_Values (Headers, Name), Name);
         end if;
      end Bind_Header;
   begin
      if not Initialized then
         Initialize ("access.dat");

         if not Initialized then
            return;
         end if;
      end if;

      Stmt_Pool.Get (Stmt);

      --  TODO: Read AWS.Status.Request_Time (Request)
      Bind (1, AWS.Status.Peername (Request), "peer name");
      Bind (2, AWS.Status.Method (Request), "method");
      Bind (3, AWS.Status.URI (Request), "path");
      Bind (4, AWS.Status.HTTP_Version (Request), "HTTP version");
      Bind (5, AWS.Messages.Image (AWS.Response.Status_Code (Response)),
        "status code");
      Bind (6, Long_Integer (AWS.Response.Content_Length (Response)),
        "response size");
      Bind_Header (7, "Referer");
      Bind_Header (8, "User-Agent");
      Bind_Header (9, "Cookie");
      Bind (10, Interfaces.C.double (Build_Time), "build time");
      Bind (11, Interfaces.C.double (Export_Time), "export time");
      Bind_Header (12, "Host");
      Bind_Header (13, "X-Real-IP");
      Bind_Header (14, "X-Forwarded-For");

      if Failed then
         SQLite3.Finish (Stmt, Status);
         return;
      end if;

      loop
         SQLite3.Step (Stmt, Status);
         exit when Status = SQLite3.SQLITE_DONE;

         if Status /= SQLite3.SQLITE_ROW then
            Natools.Web.Log
              (Natools.Web.Severities.Error,
               "Unable to insert: " & SQLite3.Error_Code'Image (Status)
               & ' ' & SQLite3.Error_Message (Handle));
            SQLite3.Finish (Stmt, Status);
            return;
         end if;
      end loop;

      SQLite3.Reset (Stmt, Status);

      if Status /= SQLite3.SQLITE_OK then
         Natools.Web.Log
           (Natools.Web.Severities.Critical,
            "Unable to reset insert statement: "
            & SQLite3.Error_Code'Image (Status)
            & ' ' & SQLite3.Error_Message (Handle));
         SQLite3.Finish (Stmt, Status);
         return;
      end if;

      SQLite3.Clear_Bindings (Stmt, Status);

      if Status /= SQLite3.SQLITE_OK then
         Natools.Web.Log
           (Natools.Web.Severities.Critical,
            "Unable to reset insert statement: "
            & SQLite3.Error_Code'Image (Status)
            & ' ' & SQLite3.Error_Message (Handle));
         SQLite3.Finish (Stmt, Status);
         return;
      end if;

      Stmt_Pool.Release (Stmt);
   end Log;



   --------------------
   -- Statement Pool --
   --------------------

   protected body Stmt_Pool is

      procedure Get (Stmt : out SQLite3.SQLite3_Statement) is
      begin
         if Available.Is_Empty then
            Create_Stmt (Stmt);
         else
            Stmt := Available.First_Element;
            Available.Delete_First;
         end if;
      end Get;


      procedure Release (Stmt : out SQLite3.SQLite3_Statement) is
      begin
         Available.Append (Stmt);
      end Release;

   end Stmt_Pool;

end Lithium.Access_Log;
