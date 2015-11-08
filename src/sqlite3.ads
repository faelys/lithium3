pragma Style_Checks (Off);
--
-- Copyright (c) 2009, 2012 Tero Koskinen <tero.koskinen@iki.fi>
--
-- Permission to use, copy, modify, and distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--
with Ada.Strings.Unbounded;
with Interfaces.C;

package SQLite3 is
   type SQLite3_DB is private;
   type SQLite3_Statement is private;
   type Error_Code is new Long_Integer;
   subtype SQL_Parameter_Index is Integer range 1 .. Integer'Last;
   subtype SQL_Column_Index is Integer;
   type Int is new Interfaces.C.Int;

   SQLITE_OK    : constant Error_Code := 0;
   SQLITE_ERROR : constant Error_Code := 1;
   SQLITE_ROW   : constant Error_Code := 100;
   SQLITE_DONE  : constant Error_Code := 101;

   SQLITE_TRANSIENT : constant := -1;

   procedure Open (Filename : in String;
                   Handle   : out SQLite3_DB;
                   Status   : out Error_Code);

   procedure Close (Handle : in out SQLite3_DB; Status : out Error_Code);

   procedure Prepare (Handle     : SQLite3_DB;
                      Sql        : String;
                      SQL_Handle : out SQLite3_Statement;
                      Status     : out Error_Code);

   procedure Step (SQL_Handle : SQLite3_Statement; Status : out Error_Code);

   procedure Finish (SQL_Handle : SQLite3_Statement; Status : out Error_Code);

   procedure Reset (SQL_Handle : SQLite3_Statement; Status : out Error_Code);

   procedure Clear_Bindings (SQL_Handle : SQLite3_Statement;
                             Status     : out Error_Code);

   procedure Bind (SQL_Handle : SQLite3_Statement;
                   Index      : SQL_Parameter_Index;
                   Value      : Integer;
                   Status     : out Error_Code);

   procedure Bind (SQL_Handle : SQLite3_Statement;
                   Index      : SQL_Parameter_Index;
                   Value      : Long_Integer;
                   Status     : out Error_Code);

   procedure Bind (SQL_Handle : SQLite3_Statement;
                   Index      : SQL_Parameter_Index;
                   Value      : Ada.Strings.Unbounded.Unbounded_String;
                   Status     : out Error_Code);

   procedure Bind (SQL_Handle : SQLite3_Statement;
                   Index      : SQL_Parameter_Index;
                   Value      : String;
                   Status     : out Error_Code);

   procedure Bind (SQL_Handle : SQLite3_Statement;
                   Index      : SQL_Parameter_Index;
                   Value      : Interfaces.C.double;
                   Status     : out Error_Code);

   procedure Column (SQL_Handle : SQLite3_Statement;
                     Index      : SQL_Column_Index;
                     Value      : out Int);

   procedure Column (SQL_Handle : SQLite3_Statement;
                     Index      : SQL_Column_Index;
                     Value      : out Ada.Strings.Unbounded.Unbounded_String);

   function Error_Message (Handle : SQLite3_DB) return String;
private
   type DB_Private is null record;
   type DB_Private_Access is access all DB_Private;

   type SQLite3_DB is record
      Ptr : aliased DB_Private_Access;
   end record;

   type Statement_Private is null record;
   type Statement_Private_Access is access all Statement_Private;
   type SQLite3_Statement is record
      Ptr : aliased Statement_Private_Access;
   end record;
end SQLite3;
