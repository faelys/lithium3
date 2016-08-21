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
with Interfaces.C.Strings;

package body SQLite3 is
   procedure Open (Filename : in String;
                   Handle   : out SQLite3_DB;
                   Status   : out Error_Code)
   is
      function sqlite3_open (Filename : Interfaces.C.Char_Array;
                             DB_Handle : access DB_Private_Access)
         return Interfaces.C.Int;
      pragma Import (C, sqlite3_open, "sqlite3_open");

      Code : Interfaces.C.Int;
   begin
      Code := sqlite3_open (Interfaces.C.To_C (Filename), Handle.Ptr'Access);
      Status := Error_Code (Code);
   end Open;

   procedure Close (Handle : in out SQLite3_DB; Status : out Error_Code) is
      function sqlite3_close (DB_Handle : DB_Private_Access)
         return Interfaces.C.Int;
      pragma Import (C, sqlite3_close, "sqlite3_close");

      Code : Interfaces.C.Int;
   begin
      Code := sqlite3_close (Handle.Ptr);
      Status := Error_Code (Code);
   end Close;

   procedure Prepare (Handle     : SQLite3_DB;
                      Sql        : String;
                      SQL_Handle : out SQLite3_Statement;
                      Status     : out Error_Code) is
      type Chars_Ptr_Ptr is access all Interfaces.C.Strings.Chars_Ptr;

      -- int sqlite3_prepare_v2(
      --   sqlite3 *db,            /* Database handle */
      --   const char *zSql,       /* SQL statement, UTF-8 encoded */
      --   int nByte,              /* Maximum length of zSql in bytes. */
      --   sqlite3_stmt **ppStmt,  /* OUT: Statement handle */
      --   const char **pzTail     /* OUT: Pointer to unused portion of zSql */
      -- );
      function sqlite3_prepare_v2
        (DB_Handle : DB_Private_Access;
         zSql      : Interfaces.C.Char_Array;
         nByte     : Interfaces.C.Int;
         ppStmt    : access Statement_Private_Access;
         pzTail    : Chars_Ptr_Ptr)
        return Interfaces.C.Int;
      pragma Import (C, sqlite3_prepare_v2, "sqlite3_prepare_v2");

      Sql_Str : constant Interfaces.C.Char_Array :=
        Interfaces.C.To_C (Sql);
      Code : Interfaces.C.Int;
   begin
      Code := sqlite3_prepare_v2
        (DB_Handle => Handle.Ptr,
         zSql      => Sql_Str,
         nByte     => Sql_Str'Length,
         ppStmt    => Sql_Handle.Ptr'Access,
         pzTail    => null);
      Status := Error_Code (Code);
   end Prepare;

   procedure Step (SQL_Handle : SQLite3_Statement; Status : out Error_Code) is
      -- int sqlite3_step(sqlite3_stmt*);
      function sqlite3_step (Stmt : Statement_Private_Access)
        return Interfaces.C.Int;
      pragma Import (C, sqlite3_step, "sqlite3_step");

      Code : Interfaces.C.Int;
   begin
      Code := sqlite3_step (SQL_Handle.Ptr);
      Status := Error_Code (Code);
   end Step;

   procedure Finish (SQL_Handle : SQLite3_Statement;
                     Status     : out Error_Code) is
      -- int sqlite3_finalize(sqlite3_stmt*);
      function sqlite3_finalize (Stmt : Statement_Private_Access)
        return Interfaces.C.Int;
      pragma Import (C, sqlite3_finalize, "sqlite3_finalize");

      Code : Interfaces.C.Int;
   begin
      Code := sqlite3_finalize (SQL_Handle.Ptr);
      Status := Error_Code (Code);
   end Finish;

   procedure Reset (SQL_Handle : SQLite3_Statement;
                    Status     : out Error_Code) is
      -- int sqlite3_reset(sqlite3_stmt*);
      function sqlite3_reset (Stmt : Statement_Private_Access)
        return Interfaces.C.Int;
      pragma Import (C, sqlite3_reset, "sqlite3_reset");

      Code : Interfaces.C.Int;
   begin
      Code := sqlite3_reset (SQL_Handle.Ptr);
      Status := Error_Code (Code);
   end Reset;

   procedure Clear_Bindings (SQL_Handle : SQLite3_Statement;
                             Status     : out Error_Code)
   is
      -- int sqlite3_clear_bindings(sqlite3_stmt*);
      function sqlite3_clear_bindings (Stmt : Statement_Private_Access)
        return Interfaces.C.Int;
      pragma Import (C, sqlite3_clear_bindings, "sqlite3_clear_bindings");

      Code : Interfaces.C.Int;
   begin
      Code := sqlite3_clear_bindings (SQL_Handle.Ptr);
      Status := Error_Code (Code);
   end Clear_Bindings;

   procedure Bind (SQL_Handle : SQLite3_Statement;
                   Index      : SQL_Parameter_Index;
                   Value      : Integer;
                   Status     : out Error_Code) is
   begin
      Bind (SQL_Handle, Index, Long_Integer (Value), Status);
   end Bind;

   procedure Bind (SQL_Handle : SQLite3_Statement;
                   Index      : SQL_Parameter_Index;
                   Value      : Long_Integer;
                   Status     : out Error_Code) is
      -- int sqlite3_bind_int(sqlite3_stmt*, int, int);
      function sqlite3_bind_int (Stmt  : Statement_Private_Access;
                                 Index : Interfaces.C.Int;
                                 Value : Interfaces.C.Int)
        return Interfaces.C.Int;
      pragma Import (C, sqlite3_bind_int, "sqlite3_bind_int");

      Code : Interfaces.C.Int;
   begin
      Code := sqlite3_bind_int (Stmt  => Sql_Handle.Ptr,
                                Index => Interfaces.C.Int (Index),
                                Value => Interfaces.C.Int (Value));
      Status := Error_Code (Code);
   end Bind;

   procedure Bind (SQL_Handle : SQLite3_Statement;
                   Index      : SQL_Parameter_Index;
                   Value      : Ada.Strings.Unbounded.Unbounded_String;
                   Status     : out Error_Code)
   is

      use Ada.Strings.Unbounded;

      -- int sqlite3_bind_text
      --       (sqlite3_stmt*,
      --        int,
      --        const char*,
      --        int,
      --        void(*)(void*));
      function sqlite3_bind_text (Stmt  : Statement_Private_Access;
                                  Index : Interfaces.C.int;
                                  Value : Interfaces.C.char_array;
                                  Bytes : Interfaces.C.int;
                                  Opt   : Interfaces.C.long)
        return Interfaces.C.Int;
      pragma Import (C, sqlite3_bind_text, "sqlite3_bind_text");

      Code : Interfaces.C.Int;
   begin
      Code := sqlite3_bind_text
        (Stmt  => Sql_Handle.Ptr,
         Index => Interfaces.C.int (Index),
         Value => Interfaces.C.To_C (To_String (Value)),
         Bytes => Interfaces.C.int (Length (Value)),
         Opt   => SQLITE_TRANSIENT);
      Status := Error_Code (Code);
   end Bind;

   procedure Bind (SQL_Handle : SQLite3_Statement;
                   Index      : SQL_Parameter_Index;
                   Value      : String;
                   Status     : out Error_Code)
   is
      -- int sqlite3_bind_text
      --       (sqlite3_stmt*,
      --        int,
      --        const char*,
      --        int,
      --        void(*)(void*));
      function sqlite3_bind_text (Stmt  : Statement_Private_Access;
                                  Index : Interfaces.C.int;
                                  Value : Interfaces.C.char_array;
                                  Bytes : Interfaces.C.int;
                                  Opt   : Interfaces.C.long)
        return Interfaces.C.Int;
      pragma Import (C, sqlite3_bind_text, "sqlite3_bind_text");

      Code : Interfaces.C.Int;
   begin
      Code := sqlite3_bind_text
        (Stmt  => Sql_Handle.Ptr,
         Index => Interfaces.C.int (Index),
         Value => Interfaces.C.To_C (Value),
         Bytes => Interfaces.C.int (Value'Length),
         Opt   => SQLITE_TRANSIENT);
      Status := Error_Code (Code);
   end Bind;

   procedure Bind (SQL_Handle : SQLite3_Statement;
                   Index      : SQL_Parameter_Index;
                   Value      : Interfaces.C.double;
                   Status     : out Error_Code)
   is
      -- int sqlite3_bind_double
      --       (sqlite3_stmt*,
      --        int,
      --        double);
      function sqlite3_bind_double (Stmt  : Statement_Private_Access;
                                    Index : Interfaces.C.int;
                                    Value : Interfaces.C.double)
        return Interfaces.C.Int;
      pragma Import (C, sqlite3_bind_double, "sqlite3_bind_double");

      Code : Interfaces.C.Int;
   begin
      Code := sqlite3_bind_double
        (Stmt  => Sql_Handle.Ptr,
         Index => Interfaces.C.int (Index),
         Value => Value);
      Status := Error_Code (Code);
   end Bind;

   procedure Column (SQL_Handle : SQLite3_Statement;
                     Index      : SQL_Column_Index;
                     Value      : out Int) is
      -- int sqlite3_column_int(sqlite3_stmt*, int iCol);
      function sqlite3_column_int (Stmt  : Statement_Private_Access;
                                   Index : Interfaces.C.Int)
        return Interfaces.C.Int;
      pragma Import (C, sqlite3_column_int, "sqlite3_column_int");

      Ret_Val : Interfaces.C.Int;
   begin
      Ret_Val := sqlite3_column_int (Stmt => Sql_Handle.Ptr,
                                     Index => Interfaces.C.Int (Index));
      Value := Int (Ret_Val);
   end Column;

   procedure Column (SQL_Handle : SQLite3_Statement;
                     Index      : SQL_Column_Index;
                     Value      : out Ada.Strings.Unbounded.Unbounded_String)
   is
      use Ada.Strings.Unbounded;
      use Interfaces;
      use type Interfaces.C.Strings.chars_ptr;

      -- const unsigned char *sqlite3_column_text(sqlite3_stmt*, int iCol);
      function sqlite3_column_text (Stmt  : Statement_Private_Access;
                                    Index : Interfaces.C.Int)
        return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, sqlite3_column_text, "sqlite3_column_text");

      Ret_Val : Interfaces.C.Strings.chars_ptr;
   begin
      Ret_Val := sqlite3_column_text (Stmt => Sql_Handle.Ptr,
                                      Index => Interfaces.C.Int (Index));
      if Ret_Val = C.Strings.Null_Ptr then
         Value := Null_Unbounded_String;
      else
         Value := To_Unbounded_String (C.To_Ada (C.Strings.Value (Ret_Val)));
      end if;
   end Column;

   function Error_Message (Handle : SQLite3_DB) return String is
      use Interfaces;

      -- const char *sqlite3_errmsg(sqlite3*);
      function sqlite3_errmsg(DB_Handle : DB_Private_Access)
        return C.Strings.chars_ptr;
      pragma Import (C, sqlite3_errmsg, "sqlite3_errmsg");
   begin
      return C.To_Ada (C.Strings.Value (sqlite3_errmsg (Handle.Ptr)));
   end Error_Message;

   procedure Busy_Timeout (Handle : SQLite3_DB;
                           ms : Interfaces.C.int;
                           Status : out Error_Code)
   is
      -- int sqlite3_busy_timeout
      --       (sqlite3*,
      --        int);
      function sqlite3_busy_timeout (DB_Handle : DB_Private_Access;
                                     ms : Interfaces.C.int)
        return Interfaces.C.Int;
      pragma Import (C, sqlite3_busy_timeout, "sqlite3_busy_timeout");

      Code : Interfaces.C.Int;
   begin
      Code := sqlite3_busy_timeout
        (DB_Handle => Handle.Ptr,
         ms => ms);
      Status := Error_Code (Code);
   end Busy_Timeout;
end SQLite3;
