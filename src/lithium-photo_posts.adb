------------------------------------------------------------------------------
-- Copyright (c) 2019, Natacha Porté                                        --
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
with Ada.Calendar.Time_Zones;
with Natools.S_Expressions.Atom_Ref_Constructors;
with Natools.S_Expressions.Enumeration_IO;
with Natools.S_Expressions.Interpreter_Loop;
with Natools.S_Expressions.Parsers;
with Natools.Time_IO.RFC_3339;
with Natools.Time_Keys;
with Natools.Web.ACL;
with Natools.Web.Error_Pages;
with Natools.Web.Exchanges;
with Natools.Web.Tags;

package body Lithium.Photo_Posts is

   use type Sx.Atom;

   package Generator_Elements is
      type Enum is
        (Unknown,
         Failure_Redirect,
         Success_Redirect,
         Tag_Prefix,
         Name_Prefix,
         Extra_Tags,
         Yearly_Tags,
         Monthly_Tags,
         Process,
         Allow);
   end Generator_Elements;

   package Generator_IO is new Sx.Enumeration_IO.Typed_IO
     (Generator_Elements.Enum);


   function Monthly_Suffix (Date : Ada.Calendar.Time) return Sx.Atom;

   function Prefixed_Atom
     (Opt_Prefix : in Sx.Atom_Refs.Immutable_Reference;
      Root : in Sx.Atom)
     return Sx.Atom
     is (if Opt_Prefix.Is_Empty then Root else Opt_Prefix.Query & Root);

   procedure Update_Generator
     (Object : in out Generator;
      Context : in Natools.Meaningless_Type;
      Name : in Sx.Atom;
      Arguments : in out Sx.Lockable.Descriptor'Class);

   function Yearly_Suffix (Date : Ada.Calendar.Time) return Sx.Atom;

   procedure Read_Generator is new Sx.Interpreter_Loop
     (Generator, Natools.Meaningless_Type, Update_Generator);


   Field_Date : constant String := "date";
   Field_Elements : constant String := "elements";
   Field_File : constant String := "file";
   Field_Name : constant String := "name";
   Field_Orientation : constant String := "orientation";
   Field_Tag : constant String := "tag";
   Field_Title : constant String := "e_title";

   Given_Date_Key : constant Sx.Atom
     := (1 => Character'Pos ('s'),
         2 => Character'Pos ('h'),
         3 => Character'Pos ('o'),
         4 => Character'Pos ('t'));
   Post_Date_Key : constant Sx.Atom
     := (1 => Character'Pos ('p'),
         2 => Character'Pos ('u'),
         3 => Character'Pos ('b'),
         4 => Character'Pos ('l'),
         5 => Character'Pos ('i'),
         6 => Character'Pos ('s'),
         7 => Character'Pos ('h'),
         8 => Character'Pos ('e'),
         9 => Character'Pos ('d'));
   Title_Element_Key : constant Sx.Atom
     := (1 => Character'Pos ('t'),
         2 => Character'Pos ('i'),
         3 => Character'Pos ('t'),
         4 => Character'Pos ('l'),
         5 => Character'Pos ('e'));



   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   function Monthly_Suffix (Date : Ada.Calendar.Time) return Sx.Atom is
      Month : constant Integer := Ada.Calendar.Month (Date);
   begin
      return Yearly_Suffix (Date)
        & Sx.Atom'(1 => Character'Pos ('/'),
                   2 => Character'Pos (if Month >= 10 then '1' else '0'),
                   3 => Sx.Octet (Character'Pos ('0') + Month mod 10));
   end Monthly_Suffix;


   procedure Update_Generator
     (Object : in out Generator;
      Context : in Natools.Meaningless_Type;
      Name : in Sx.Atom;
      Arguments : in out Sx.Lockable.Descriptor'Class)
   is
      pragma Unreferenced (Context);
      use type Sx.Events.Event;
      use Generator_Elements;
      Element : Generator_Elements.Enum := Unknown;
   begin
      begin
         Element := Generator_IO.Value (Name);
      exception
         when Constraint_Error => null;
      end;

      case Element is
         when Unknown =>
            Natools.Web.Log
              (Natools.Web.Severities.Error,
               "Unknown photo blog generator component """
                 & Sx.To_String (Name) & '"');

         when Success_Redirect =>
            if Arguments.Current_Event = Sx.Events.Add_Atom then
               Object.Success_Redirect
                 := Sx.Atom_Ref_Constructors.Create (Arguments.Current_Atom);
            else
               Object.Success_Redirect.Reset;
            end if;

         when Failure_Redirect =>
            if Arguments.Current_Event = Sx.Events.Add_Atom then
               Object.Failure_Redirect
                 := Sx.Atom_Ref_Constructors.Create (Arguments.Current_Atom);
            else
               Object.Failure_Redirect.Reset;
            end if;

         when Tag_Prefix =>
            if Arguments.Current_Event = Sx.Events.Add_Atom then
               Object.Tag_Prefix
                 := Sx.Atom_Ref_Constructors.Create (Arguments.Current_Atom);
            else
               Object.Tag_Prefix.Reset;
            end if;

         when Name_Prefix =>
            if Arguments.Current_Event = Sx.Events.Add_Atom then
               Object.Name_Prefix
                 := Sx.Atom_Ref_Constructors.Create (Arguments.Current_Atom);
            else
               Object.Name_Prefix.Reset;
            end if;

         when Extra_Tags =>
            Object.Extra_Tags := Natools.Web.Containers.Create (Arguments);

         when Yearly_Tags =>
            Object.Yearly_Tags := Natools.Web.Containers.Create (Arguments);

         when Monthly_Tags =>
            Object.Monthly_Tags := Natools.Web.Containers.Create (Arguments);

         when Process =>
            Object.Process.Append (Photo_Process.Create (Arguments));

         when Allow =>
            Object.Allow := Sx.Caches.Move (Arguments);

      end case;
   end Update_Generator;


   function Yearly_Suffix (Date : Ada.Calendar.Time) return Sx.Atom is
      Year : constant Integer := Ada.Calendar.Year (Date);
      Image : Sx.Atom := Sx.To_Atom (Integer'Image (Year));
   begin
      Image (Image'First) := Character'Pos ('/');
      return Image;
   end Yearly_Suffix;



   ----------------------
   -- Public Interface --
   ----------------------

   overriding procedure Respond
     (Object : in out Generator;
      Exchange : in out Natools.Web.Sites.Exchange;
      Extra_Path : in Sx.Atom;
      Descriptor : out MP.Page_Descriptor;
      Do_Post : out Boolean)
   is
      procedure Process_Field (Name, Value : in String);

      Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Time_Key : constant Sx.Atom_Refs.Immutable_Reference
        := Sx.Atom_Ref_Constructors.Create
           (Sx.To_Atom (Natools.Time_Keys.To_Key (Now)));

      Dates : Natools.Web.Containers.Date_Maps.Unsafe_Maps.Map;
      Elements : Natools.Web.Containers.Expression_Maps.Constant_Map;
      Page_Name : Sx.Atom_Refs.Immutable_Reference;
      File_Name : Sx.Atom_Refs.Immutable_Reference;
      Tags : Natools.Web.Containers.Unsafe_Atom_Lists.List;
      Orientation : Photo_Process.Orientations.Enum
        := Photo_Process.Orientations.Top_Left;

      procedure Process_Field (Name, Value : in String) is
      begin
         if Name = Field_Date then
            if Natools.Time_IO.RFC_3339.Is_Valid (Value) then
               declare
                  Given_Date : Natools.Web.Containers.Date;
               begin
                  Natools.Time_IO.RFC_3339.Value
                    (Value, Given_Date.Time, Given_Date.Offset);
                  Dates.Include (Given_Date_Key, Given_Date);
               end;
            else
               Natools.Web.Log
                 (Natools.Web.Severities.Warning,
                  "Invalid date """ & Value & """ in field """ & Name & '"');
            end if;

         elsif Name = Field_Elements then
            declare
               Parser : Sx.Parsers.Memory_Parser
                 := Sx.Parsers.Create_From_String (Value);
            begin
               Parser.Next;
               Natools.Web.Containers.Add_Expressions (Elements, Parser);
            end;

         elsif Name = Field_Tag then
            declare
               First : Positive;
               Next : Positive := Value'First;
            begin
               while Next in Value'Range loop
                  First := Next;
                  while First in Value'Range and then Value (First) = ' ' loop
                     First := First + 1;
                  end loop;
                  exit when First not in Value'Range;

                  Next := First + 1;
                  while Next in Value'Range and then Value (Next) /= ' ' loop
                     Next := Next + 1;
                  end loop;

                  Tags.Append (Prefixed_Atom
                    (Object.Tag_Prefix,
                     Sx.To_Atom (Value (First .. Next - 1))));
               end loop;
            end;

         elsif Name = Field_Name then
            Page_Name := Sx.Atom_Ref_Constructors.Create
              (Prefixed_Atom (Object.Name_Prefix, Sx.To_Atom (Value)));

         elsif Name = Field_Title then
            declare
               S : Sx.Caches.Reference;
            begin
               S.Append_String (Value);
               Elements := Elements.Include (Title_Element_Key, S.First);
            end;

         elsif Name = Field_File then
            if File_Name.Is_Empty then
               File_Name := Sx.Atom_Ref_Constructors.Create
                 (Sx.To_Atom (Value));
            end if;

         elsif Name = Field_Orientation then
            if Value'Length = 1 and then Value (Value'First) in '1' .. '8' then
               Orientation := Photo_Process.Orientations.Enum'Val
                 (Character'Pos (Value (Value'First)) - Character'Pos ('1'));
            else
               Natools.Web.Log
                 (Natools.Web.Severities.Error,
                  "Invalid orientation field value """ & Value & '"');
            end if;
         end if;
      end Process_Field;
   begin
      Do_Post := False;
      Descriptor := (others => <>);

      if Extra_Path'Length > 0 then
         return;
      end if;

      Check_Method :
      declare
         Allowed : Boolean;
      begin
         Natools.Web.Error_Pages.Check_Method
           (Exchange, Natools.Web.Exchanges.POST, Allowed);

         if not Allowed then
            return;
         end if;
      end Check_Method;

      Check_ACL :
      declare
         Allowed : Boolean := False;
         Expr : Sx.Caches.Cursor := Object.Allow.First;
      begin
         Natools.Web.ACL.Match
           (Natools.Web.Sites.Identity (Exchange), Expr, Allowed);

         if not Allowed then
            Natools.Web.Log
              (Natools.Web.Severities.Error,
               "Posting not allowed by ACL");

            if not Object.Failure_Redirect.Is_Empty then
               Natools.Web.Error_Pages.See_Other
                 (Exchange,
                  Object.Failure_Redirect.Query);
            end if;

            return;
         end if;
      end Check_ACL;

      Dates.Include
        (Key => Post_Date_Key,
         New_Item =>
           (Time => Now,
            Offset => Ada.Calendar.Time_Zones.UTC_Time_Offset (Now)));

      Exchange.Iterate_Parameters (Process_Field'Access);

      if Page_Name.Is_Empty then
         Page_Name := Sx.Atom_Ref_Constructors.Create
           (Prefixed_Atom (Object.Name_Prefix, Time_Key.Query));
      end if;

      if not Object.Extra_Tags.Is_Empty then
         for T of Object.Extra_Tags.Query.Data.all loop
            Tags.Append (T.Query);
         end loop;
      end if;

      if not Object.Yearly_Tags.Is_Empty then
         for T of Object.Yearly_Tags.Query.Data.all loop
            Tags.Append (T.Query & Yearly_Suffix (Now));
         end loop;
      end if;

      if not Object.Monthly_Tags.Is_Empty then
         for T of Object.Monthly_Tags.Query.Data.all loop
            Tags.Append (T.Query & Monthly_Suffix (Now));
         end loop;
      end if;

      if File_Name.Is_Empty then
         if not Object.Process.Is_Empty then
            Natools.Web.Log
              (Natools.Web.Severities.Warning,
               "No file posted for processing");
         end if;
      else
         Run_Process :
         declare
            Page_Name_String : constant String
              := Sx.To_String (Page_Name.Query);
            Base_Name : constant String
              := (if Page_Name_String'Length > 0
                    and then Page_Name_String (Page_Name_String'First)
                       in '+' | '-' | '#'
                  then Page_Name_String (Page_Name_String'First + 1
                                      .. Page_Name_String'Last)
                  else Page_Name_String);
            Input : constant Photo_Process.Image := Photo_Process.Create
              (Sx.To_String (File_Name.Query), Orientation);
         begin
            for Process of Object.Process loop
               Input.Run (Base_Name, Process);
            end loop;
         end Run_Process;
      end if;

      Do_Post := True;
      Descriptor
        := (Name => Page_Name,
            Elements => Elements,
            Tags => Natools.Web.Tags.Create
              (Tag_Names => Natools.Web.Containers.Create (Tags).Query,
               Common_Key => Time_Key),
            Dates => Natools.Web.Containers.Date_Maps.Create (Dates));

      if not Object.Success_Redirect.Is_Empty then
         Natools.Web.Error_Pages.See_Other
           (Exchange,
            Object.Success_Redirect.Query);
      end if;
   end Respond;


   function Create
     (Arguments : in out Sx.Lockable.Descriptor'Class)
     return MP.Generator'Class
   is
      Result : Generator;
   begin
      Read_Generator (Arguments, Result, Natools.Meaningless_Value);
      return Result;
   end Create;

end Lithium.Photo_Posts;
