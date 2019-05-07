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

with Ada.Streams.Stream_IO;
with Natools.S_Expressions.Atom_Ref_Constructors;
with Natools.S_Expressions.Enumeration_IO;
with Natools.S_Expressions.File_Readers;
with Natools.S_Expressions.Interpreter_Loop;
with Natools.Web;

package body Lithium.Photo_Process is

   package C renames Interfaces.C;

   package Description_Elements is
      type Enum is
        (Unknown,
         Max_Width,
         Max_Height,
         Quality,
         Path,
         Prefix,
         Suffix);
   end Description_Elements;

   package Description_IO is new Sx.Enumeration_IO.Typed_IO
     (Description_Elements.Enum);


   function Adjust_Orientation
     (Ref : in Data_Refs.Immutable_Reference;
      Orientation : in Orientations.Enum)
     return Data_Refs.Immutable_Reference;

   procedure Adjust_Plane
     (Plane : in TurboJPEG_Thin.Bytes.Pointer;
      Width, Height : in Positive;
      Old_Plane : in TurboJPEG_Thin.Bytes.Pointer;
      Old_Width, Old_Height : in Positive;
      Orientation : in Orientations.Enum);

   function To_String
     (Atom : in Sx.Atom_Refs.Immutable_Reference;
      Default : in String := "")
     return String
     is (if Atom.Is_Empty then Default else Sx.To_String (Atom.Query));

   procedure Update_Description
     (Process : in out Process_Description;
      Context : in Natools.Meaningless_Type;
      Name : in Sx.Atom;
      Arguments : in out Sx.Lockable.Descriptor'Class);

   function Resize_Image
     (Input : in Image_Data;
      New_Width, New_Height : in Positive)
     return Data_Refs.Immutable_Reference;

   procedure Resize_Plane
     (New_Plane : in Buffer;
      New_Width, New_Height : in Positive;
      Old_Plane : in Buffer;
      Old_Width, Old_Height : in Positive);

   procedure Write
     (File_Name : in String;
      Image : in Image_Data;
      Quality : in Natural);


   procedure Read_Description is new Sx.Interpreter_Loop
     (Process_Description, Natools.Meaningless_Type, Update_Description);



   --------------------------------
   -- Re-orientation expressions --
   --------------------------------

   function New_Height
     (Width, Height : in Positive;
      Orientation : in Orientations.Enum)
     return Positive
     is (case Orientation is
         when Orientations.Top_Left
            | Orientations.Top_Right
            | Orientations.Bottom_Left
            | Orientations.Bottom_Right => Height,
         when Orientations.Left_Top
            | Orientations.Right_Top
            | Orientations.Left_Bottom
            | Orientations.Right_Bottom => Width);

   function New_Width
     (Width, Height : in Positive;
      Orientation : in Orientations.Enum)
     return Positive
     is (case Orientation is
         when Orientations.Top_Left
            | Orientations.Top_Right
            | Orientations.Bottom_Left
            | Orientations.Bottom_Right => Width,
         when Orientations.Left_Top
            | Orientations.Right_Top
            | Orientations.Left_Bottom
            | Orientations.Right_Bottom => Height);

   function Old_X
     (X, Y : in Natural;
      Width, Height : in Positive;
      Orientation : in Orientations.Enum)
     return Natural
     is (case Orientation is
         when Orientations.Top_Left
            | Orientations.Bottom_Left => X,
         when Orientations.Top_Right
            | Orientations.Bottom_Right => Width - X - 1,
         when Orientations.Left_Top
            | Orientations.Right_Top => Y,
         when Orientations.Left_Bottom
            | Orientations.Right_Bottom => Height - Y - 1);

   function Old_Y
     (X, Y : in Natural;
      Width, Height : in Positive;
      Orientation : in Orientations.Enum)
     return Natural
     is (case Orientation is
         when Orientations.Left_Top
            | Orientations.Left_Bottom => X,
         when Orientations.Right_Top
            | Orientations.Right_Bottom => Width - X - 1,
         when Orientations.Top_Left
            | Orientations.Top_Right => Y,
         when Orientations.Bottom_Left
            | Orientations.Bottom_Right => Height - Y - 1);

   function Offset
     (X, Y : in Natural;
      Stride : in Positive)
     return C.ptrdiff_t
     is (C."+" (C."*" (C.ptrdiff_t (Y), C.ptrdiff_t (Stride)),
                C.ptrdiff_t (X)));

   function Plane_Height
     (Label : in TurboJPEG_Thin.Plane_Label;
      Image_Height : in Positive;
      Sub_Samp : in C.int)
     return Positive
     is (Positive (TurboJPEG_Thin.Plane_Height
           (TurboJPEG_Thin.Plane_Label'Pos (Label),
            C.int (Image_Height),
            Sub_Samp)));

   function Plane_Width
     (Label : in TurboJPEG_Thin.Plane_Label;
      Image_Width : in Positive;
      Sub_Samp : in C.int)
     return Positive
     is (Positive (TurboJPEG_Thin.Plane_Width
           (TurboJPEG_Thin.Plane_Label'Pos (Label),
            C.int (Image_Width),
            Sub_Samp)));

   function Ptr
     (Base : in TurboJPEG_Thin.Bytes.Pointer;
      X, Y : in Natural;
      Stride : in Positive)
     return TurboJPEG_Thin.Bytes.Pointer
     is (TurboJPEG_Thin.Bytes."+" (Base, Offset (X, Y, Stride)));


   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   function Adjust_Orientation
     (Ref : in Data_Refs.Immutable_Reference;
      Orientation : in Orientations.Enum)
     return Data_Refs.Immutable_Reference
   is
      use type Orientations.Enum;
      Input : constant Data_Refs.Accessor := Ref.Query;
      Width : constant Natural
        := New_Width (Input.Width, Input.Height, Orientation);
      Height : constant Natural
        := New_Height (Input.Width, Input.Height, Orientation);
   begin
      if Orientation = Orientations.Top_Left then
         return Ref;
      end if;

      declare
         use Orientations;
         use type C.int;
         SS : constant C.int
           := (case Orientation is
               when Top_Left | Top_Right | Bottom_Left | Bottom_Right
                  => Input.Sub_Samp,
               when Left_Top | Right_Top | Left_Bottom | Right_Bottom
                  => (case Input.Sub_Samp is
                      when 0 | 2 => Input.Sub_Samp,
                      when 1 => 4,
                      when 4 => 1,
                      when others => raise Constraint_Error
                                 with "Unsupported sub-sampling orientation"));
         Output : constant Data_Refs.Data_Access
           := new Image_Data'(Create (Width, Height, SS));
         Result : constant Data_Refs.Immutable_Reference
           := Data_Refs.Create (Output);
      begin

         for I in Input.Planes'Range loop
            Adjust_Plane
              (Output.Planes (I).Data,
               Plane_Width (I, Width, Output.Sub_Samp),
               Plane_Height (I, Height, Output.Sub_Samp),
               Input.Planes (I).Data,
               Plane_Width (I, Input.Width, Input.Sub_Samp),
               Plane_Height (I, Input.Height, Input.Sub_Samp),
               Orientation);
         end loop;

         return Result;
      end;
   end Adjust_Orientation;


   procedure Adjust_Plane
     (Plane : in TurboJPEG_Thin.Bytes.Pointer;
      Width, Height : in Positive;
      Old_Plane : in TurboJPEG_Thin.Bytes.Pointer;
      Old_Width, Old_Height : in Positive;
      Orientation : in Orientations.Enum)
   is
      pragma Unreferenced (Old_Height);
      X, Y : Natural;
   begin
      for New_Y in 0 .. Height - 1 loop
         for New_X in 0 .. Width - 1 loop
            X := Old_X (New_X, New_Y, Width, Height, Orientation);
            Y := Old_Y (New_X, New_Y, Width, Height, Orientation);

            Ptr (Plane, New_X, New_Y, Width).all
              := Ptr (Old_Plane, X, Y, Old_Width).all;
         end loop;
      end loop;
   end Adjust_Plane;


   function Resize_Image
     (Input : in Image_Data;
      New_Width, New_Height : in Positive)
     return Data_Refs.Immutable_Reference
   is
      Data : constant Data_Refs.Data_Access
        := new Image_Data'(Create (New_Width, New_Height, Input.Sub_Samp));
      Ref : constant Data_Refs.Immutable_Reference := Data_Refs.Create (Data);
   begin
      for I in Data.Planes'Range loop
         Resize_Plane
           (Data.Planes (I),
            Plane_Width (I, New_Width, Data.Sub_Samp),
            Plane_Height (I, New_Height, Data.Sub_Samp),
            Input.Planes (I),
            Plane_Width (I, Input.Width, Input.Sub_Samp),
            Plane_Height (I, Input.Height, Input.Sub_Samp));
      end loop;

      return Ref;
   end Resize_Image;


   procedure Resize_Plane
     (New_Plane : in Buffer;
      New_Width, New_Height : in Positive;
      Old_Plane : in Buffer;
      Old_Width, Old_Height : in Positive)
   is
      type Accumulator is range 0 .. 2 ** 63 - 1;
   begin
      for New_Y in 0 .. New_Height - 1 loop
         for New_X in 0 .. New_Width - 1 loop
            declare
               First_X : constant Natural
                 := New_X * Old_Width / New_Width;
               First_Xw : constant Natural
                 := New_Width - New_X * Old_Width mod New_Width;
               Last_X : constant Natural
                 := (New_X + 1) * Old_Width / New_Width;
               Last_Xw : constant Natural
                 := (New_X + 1) * Old_Width mod New_Width;
               First_Y : constant Natural
                 := New_Y * Old_Height / New_Height;
               First_Yw : constant Natural
                 := New_Height - New_Y * Old_Height mod New_Height;
               Last_Y : constant Natural
                 := (New_Y + 1) * Old_Height / New_Height;
               Last_Yw : constant Natural
                 := (New_Y + 1) * Old_Height mod New_Height;
               Xw, Yw, W : Accumulator;
               Acc, Acc_W : Accumulator := 0;
            begin
               for Y in First_Y .. Last_Y loop
                  Yw := (if Y = First_Y then Accumulator (First_Yw)
                         elsif Y = Last_Y then Accumulator (Last_Yw)
                         else Accumulator (New_Height));

                  for X in First_X .. Last_X loop
                     Xw := (if X = First_X then Accumulator (First_Xw)
                            elsif X = Last_X then Accumulator (Last_Xw)
                            else Accumulator (New_Width));

                     W := Yw * Xw;
                     Acc_W := Acc_W + W;
                     Acc := Acc + W * Accumulator
                           (Ptr (Old_Plane.Data, X, Y, Old_Width).all);
                  end loop;
               end loop;

               Acc := (Acc + Acc_W / 2) / Acc_W;
               Ptr (New_Plane.Data, New_X, New_Y, New_Width).all
                 := C.unsigned_char (Acc);
            end;
         end loop;
      end loop;
   end Resize_Plane;


   procedure Update_Description
     (Process : in out Process_Description;
      Context : in Natools.Meaningless_Type;
      Name : in Sx.Atom;
      Arguments : in out Sx.Lockable.Descriptor'Class)
   is
      pragma Unreferenced (Context);
      use type Sx.Events.Event;
      use Description_Elements;
      Element : Description_Elements.Enum := Unknown;
   begin
      begin
         Element := Description_IO.Value (Name);
      exception
         when Constraint_Error => null;
      end;

      case Element is
         when Unknown =>
            Natools.Web.Log
              (Natools.Web.Severities.Error,
               "Unknown photo process description element """
               & Sx.To_String (Name) & '"');

         when Max_Width =>
            if Arguments.Current_Event = Sx.Events.Add_Atom then
               begin
                  Process.Max_Width := Natural'Value
                    (Sx.To_String (Arguments.Current_Atom));
               exception
                  when Constraint_Error =>
                     Process.Max_Width := 0;
               end;
            else
               Process.Max_Width := 0;
            end if;

         when Max_Height =>
            if Arguments.Current_Event = Sx.Events.Add_Atom then
               begin
                  Process.Max_Height := Natural'Value
                    (Sx.To_String (Arguments.Current_Atom));
               exception
                  when Constraint_Error =>
                     Process.Max_Height := 0;
               end;
            else
               Process.Max_Height := 0;
            end if;

         when Quality =>
            if Arguments.Current_Event = Sx.Events.Add_Atom then
               begin
                  Process.Quality := Natural'Value
                    (Sx.To_String (Arguments.Current_Atom));
               exception
                  when Constraint_Error =>
                     Process.Quality := 0;
               end;
            else
               Process.Quality := 0;
            end if;

         when Path =>
            if Arguments.Current_Event = Sx.Events.Add_Atom then
               Process.Path := Sx.Atom_Ref_Constructors.Create
                                  (Arguments.Current_Atom);
            else
               Process.Path.Reset;
            end if;

         when Prefix =>
            if Arguments.Current_Event = Sx.Events.Add_Atom then
               Process.Prefix := Sx.Atom_Ref_Constructors.Create
                                  (Arguments.Current_Atom);
            else
               Process.Prefix.Reset;
            end if;

         when Suffix =>
            if Arguments.Current_Event = Sx.Events.Add_Atom then
               Process.Suffix := Sx.Atom_Ref_Constructors.Create
                                  (Arguments.Current_Atom);
            else
               Process.Suffix.Reset;
            end if;
      end case;
   end Update_Description;


   procedure Write
     (File_Name : in String;
      Image : in Image_Data;
      Quality : in Natural)
   is
      use type C.int;
      Handle : constant TurboJPEG_Thin.Handle := TurboJPEG_Thin.Init_Compress;
      Buf : Buffer := (Ada.Finalization.Limited_Controlled
                       with Data => null, Size => 0);
      Result : C.int;
   begin
      Result := TurboJPEG_Thin.Compress_From_YUV_Planes
        (Handle,
         +Image.Planes,
         C.int (Image.Width),
         (others => 0),
         C.int (Image.Height),
         Image.Sub_Samp,
         Buf.Data,
         Buf.Size,
         C.int (Quality),
         TurboJPEG_Thin.Value (TurboJPEG_Thin.Progressive));
      pragma Assert (Result = 0);

      declare
         use type TurboJPEG_Thin.Bytes.Pointer;
         use type C.unsigned_long;
         File : Ada.Streams.Stream_IO.File_Type;
      begin
         Ada.Streams.Stream_IO.Create
           (File, Ada.Streams.Stream_IO.Append_File, File_Name, "");

         for I in 0 .. Buf.Size - 1 loop
            Ada.Streams.Stream_IO.Write
              (File, (1 => Ada.Streams.Stream_Element
                     (TurboJPEG_Thin.Bytes.Pointer'(Buf.Data
                                                    + C.ptrdiff_t (I)).all)));
         end loop;

         Ada.Streams.Stream_IO.Close (File);
      end;
   exception
      when others =>
         Result := TurboJPEG_Thin.Destroy (Handle);
         raise;
   end Write;


   ----------------------
   -- Public Interface --
   ----------------------

   function Create (Arguments : in out Sx.Lockable.Descriptor'Class)
     return Process_Description
   is
      Result : Process_Description;
   begin
      Read_Description (Arguments, Result, Natools.Meaningless_Value);
      return Result;
   end Create;


   function Create
     (File_Name : in String;
      Orientation : in Orientations.Enum)
     return Image
   is
      Data : Data_Refs.Data_Access;
      Ref : Data_Refs.Immutable_Reference;
   begin
      Load_Image :
      declare
         use type C.int;
         Handle : constant TurboJPEG_Thin.Handle
           := TurboJPEG_Thin.Init_Decompress;
         Jpeg : constant Buffer := Create (File_Name);
         W, H, SS, CS, Result : C.int;
      begin
         Result := TurboJPEG_Thin.Decompress_Header
           (Handle, Jpeg.Data, Jpeg.Size, W, H, SS, CS);
         pragma Assert (Result = 0);

         Data := new Image_Data'(Create (Positive (W), Positive (H), SS));
         Ref := Data_Refs.Create (Data);

         Result := TurboJPEG_Thin.Decompress_To_YUV_Planes
           (Handle, Jpeg.Data, Jpeg.Size, +Data.Planes,
            W, (others => 0), H, 0);
         pragma Assert (Result = 0);

         Result := TurboJPEG_Thin.Destroy (Handle);
         pragma Assert (Result = 0);
      end Load_Image;

      return Image'(Ref => Adjust_Orientation (Ref, Orientation));
   end Create;


   procedure Run
     (Object : in Image;
      Base_Name : in String;
      Description : in Process_Description)
   is
      Input : constant Data_Refs.Accessor := Object.Ref.Query;
      Resize_Num, Resize_Denom : Natural := 0;
      Output : Data_Refs.Immutable_Reference := Object.Ref;
   begin
      if Description.Max_Width > 0 then
         if Description.Max_Height > 0 then
            if Description.Max_Width * Input.Height
              > Description.Max_Height * Input.Width
            then
               --  Max_Width / Width > Max_Height / Height
               Resize_Num := Description.Max_Height;
               Resize_Denom := Input.Height;
            else
               Resize_Num := Description.Max_Width;
               Resize_Denom := Input.Width;
            end if;
         else
            Resize_Num := Description.Max_Width;
            Resize_Denom := Input.Width;
         end if;
      elsif Description.Max_Height > 0 then
         Resize_Num := Description.Max_Height;
         Resize_Denom := Input.Height;
      end if;

      if Resize_Num > 0 and Resize_Denom > 0 then
         declare
            New_Height : constant Positive
              := (Input.Height * Resize_Num + Resize_Denom / 2) / Resize_Denom;
            New_Width : constant Positive
              := (Input.Width * Resize_Num + Resize_Denom / 2) / Resize_Denom;
         begin
            Output := Resize_Image (Input, New_Width, New_Height);
         end;
      end if;

      Write
        (File_Name => To_String (Description.Path)
                      & To_String (Description.Prefix)
                      & Base_Name
                      & To_String (Description.Suffix, ".jpg"),
         Image => Output.Query,
         Quality => Description.Quality);
   end Run;


   function Create (Size : in Interfaces.C.int) return Buffer is
      use type TurboJPEG_Thin.Bytes.Pointer;
      Data : constant TurboJPEG_Thin.Bytes.Pointer
        := TurboJPEG_Thin.Alloc (Size);
   begin
      if Data = null then
         raise Storage_Error with "Unable to allocate" & Size'Img
           & " bytes of TurboJPEG buffer";
      end if;

      return Buffer'(Ada.Finalization.Limited_Controlled
                     with Data => Data, Size => C.unsigned_long (Size));
   end Create;


   function Create (File_Name : in String) return Buffer is
      Reader : constant Sx.File_Readers.Atom_Reader
        := Sx.File_Readers.Reader (File_Name);
   begin
      return Result : Buffer := Create (C.int (Reader.Length)) do
         declare
            Index : C.ptrdiff_t := 0;

            procedure Save_Block (Block : in Sx.Atom);

            procedure Save_Block (Block : in Sx.Atom) is
               use type C.ptrdiff_t;
               use type TurboJPEG_Thin.Bytes.Pointer;
               P : TurboJPEG_Thin.Bytes.Pointer;
            begin
               for B of Block loop
                  P := Result.Data + Index;
                  P.all := C.unsigned_char (B);
                  Index := Index + 1;
               end loop;
            end Save_Block;
         begin
            Reader.Block_Query (1024, Save_Block'Access);
         end;
      end return;
   end Create;


   overriding procedure Finalize (Self : in out Buffer) is
   begin
      TurboJPEG_Thin.Free (Self.Data);
      Self.Data := null;
   end Finalize;

end Lithium.Photo_Process;
