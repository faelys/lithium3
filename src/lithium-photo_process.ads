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

------------------------------------------------------------------------------
-- Lithium.Photo_Process provides a programmable processor for JPEG images. --
------------------------------------------------------------------------------

with Natools.S_Expressions.Lockable;

private with Ada.Finalization;
private with Interfaces.C;
private with Natools.References;
private with Natools.S_Expressions.Atom_Refs;
private with Natools.Storage_Pools;
private with TurboJPEG_Thin;

package Lithium.Photo_Process is

   package Sx renames Natools.S_Expressions;

   type Process_Description is private;

   function Create (Arguments : in out Sx.Lockable.Descriptor'Class)
     return Process_Description;

   package Orientations is
      type Enum is
        (Top_Left, Top_Right, Bottom_Right, Bottom_Left,
         Left_Top, Right_Top, Right_Bottom, Left_Bottom);
   end Orientations;

   type Image is tagged private;

   function Create
     (File_Name : in String;
      Orientation : in Orientations.Enum)
     return Image;

   procedure Run
     (Object : in Image;
      Base_Name : in String;
      Description : in Process_Description);

private

   type Process_Description is record
      Max_Width : Natural := 0;
      Max_Height : Natural := 0;
      Quality : Natural := 85;
      Path : Sx.Atom_Refs.Immutable_Reference;
      Prefix : Sx.Atom_Refs.Immutable_Reference;
      Suffix : Sx.Atom_Refs.Immutable_Reference;
   end record;

   type Buffer is new Ada.Finalization.Limited_Controlled with record
      Data : TurboJPEG_Thin.Bytes.Pointer := null;
      Size : Interfaces.C.unsigned_long := 0;
   end record;

   function Create (Size : in Interfaces.C.int) return Buffer;

   function Create (File_Name : in String) return Buffer;

   overriding procedure Finalize (Self : in out Buffer);

   function Plane_Size
     (Label : in TurboJPEG_Thin.Plane_Label;
      Width, Height : in Positive;
      Sub_Samp : in Interfaces.C.int)
     return Interfaces.C.unsigned_long
     is (TurboJPEG_Thin.Plane_Size_YUV
           (TurboJPEG_Thin.Plane_Label'Pos (Label),
            Interfaces.C.int (Width),
            0,  --  Stride
            Interfaces.C.int (Height),
            Sub_Samp));

   function Create
     (Label : in TurboJPEG_Thin.Plane_Label;
      Width, Height : in Positive;
      Sub_Samp : in Interfaces.C.int)
     return Buffer
     is (Create (Interfaces.C.int
          (Plane_Size (Label, Width, Height, Sub_Samp))));

   type Planes_Buffer is array (TurboJPEG_Thin.Plane_Label) of Buffer;

   function "+" (Buffer : Planes_Buffer) return TurboJPEG_Thin.Planes_Buffer
     is ((TurboJPEG_Thin.Y  => Buffer (TurboJPEG_Thin.Y).Data,
          TurboJPEG_Thin.Cb => Buffer (TurboJPEG_Thin.Cb).Data,
          TurboJPEG_Thin.Cr => Buffer (TurboJPEG_Thin.Cr).Data));

   type Image_Data is limited record
      Width : Positive;
      Height : Positive;
      Sub_Samp : Interfaces.C.int;
      Planes : Planes_Buffer;
   end record;

   function Create
     (Width, Height : in Positive;
      Sub_Samp : in Interfaces.C.int)
     return Image_Data
     is (Width => Width,
         Height => Height,
         Sub_Samp => Sub_Samp,
         Planes =>
           (TurboJPEG_Thin.Y
              => Create (TurboJPEG_Thin.Y, Width, Height, Sub_Samp),
            TurboJPEG_Thin.Cb
              => Create (TurboJPEG_Thin.Cb, Width, Height, Sub_Samp),
            TurboJPEG_Thin.Cr
              => Create (TurboJPEG_Thin.Cr, Width, Height, Sub_Samp)));

   package Data_Refs is new Natools.References
     (Image_Data,
      Natools.Storage_Pools.Access_In_Default_Pool'Storage_Pool,
      Natools.Storage_Pools.Access_In_Default_Pool'Storage_Pool);

   type Image is tagged record
      Ref : Data_Refs.Immutable_Reference;
   end record;

end Lithium.Photo_Process;
