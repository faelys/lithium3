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
-- TurboJPEG_Thin provides the thinnest binding for TurboJPEG API.          --
------------------------------------------------------------------------------

with Interfaces.C;
with Interfaces.C.Pointers;
with Interfaces.C.Strings;

package TurboJPEG_Thin with Preelaborate is

   package C renames Interfaces.C;
   use type C.int;

   type Sub_Sampling is
     (SS_444,
      SS_422,
      SS_420,
      SS_Gray,
      SS_440,
      SS_411);

   function MCU_Width (S : in Sub_Sampling) return Integer
     is (case S is
         when SS_444  => 8,
         when SS_422  => 16,
         when SS_420  => 16,
         when SS_Gray => 8,
         when SS_440  => 8,
         when SS_411  => 32);

   function MCU_Height (S : in Sub_Sampling) return Integer
     is (case S is
         when SS_444  => 8,
         when SS_422  => 8,
         when SS_420  => 16,
         when SS_Gray => 8,
         when SS_440  => 16,
         when SS_411  => 8);

   type Pixel_Format is
     (RGB,  BGR,
      RGBX, BGRX,
      XBGR, XRGB,
      Gray,
      RGBA, BGRA,
      ABGR, ARGB,
      CMYK,
      Unknown);

   function Red_Offset (P : in Pixel_Format) return C.ptrdiff_t
     is (case P is
         when RGB  => 0,
         when BGR  => 2,
         when RGBX => 0,
         when BGRX => 2,
         when XBGR => 3,
         when XRGB => 1,
         when RGBA => 0,
         when BGRA => 2,
         when ABGR => 3,
         when ARGB => 1,
         when Gray | CMYK | Unknown
            => raise Constraint_Error);

   function Green_Offset (P : in Pixel_Format) return C.ptrdiff_t
     is (case P is
         when RGB  => 1,
         when BGR  => 1,
         when RGBX => 1,
         when BGRX => 1,
         when XBGR => 2,
         when XRGB => 2,
         when RGBA => 1,
         when BGRA => 1,
         when ABGR => 2,
         when ARGB => 2,
         when Gray | CMYK | Unknown
            => raise Constraint_Error);

   function Blue_Offset (P : in Pixel_Format) return C.ptrdiff_t
     is (case P is
         when RGB  => 2,
         when BGR  => 0,
         when RGBX => 2,
         when BGRX => 0,
         when XBGR => 1,
         when XRGB => 3,
         when RGBA => 2,
         when BGRA => 0,
         when ABGR => 1,
         when ARGB => 3,
         when Gray | CMYK | Unknown
            => raise Constraint_Error);

   function Alpha_Offset (P : in Pixel_Format) return C.ptrdiff_t
     is (case P is
         when RGBA => 3,
         when BGRA => 3,
         when ABGR => 0,
         when ARGB => 0,
         when RGB | BGR | RGBX | BGRX | XBGR | XRGB
           | Gray | CMYK | Unknown
            => raise Constraint_Error);

   function Pixel_Size (P : in Pixel_Format) return C.ptrdiff_t
     is (case P is
         when RGB  => 3,
         when BGR  => 3,
         when RGBX => 4,
         when BGRX => 4,
         when XBGR => 4,
         when XRGB => 4,
         when Gray => 1,
         when RGBA => 4,
         when BGRA => 4,
         when ABGR => 4,
         when ARGB => 4,
         when CMYK => 4,
         when Unknown => raise Constraint_Error);

   type Color_Space is
     (RGB,
      YCbCr,
      Gray,
      CYMR,
      YCCK);

   type Flag is
     (Bottom_Up,
      Fast_Upsample,
      No_Realloc,
      Fast_DCT,
      Accurate_DCT,
      Stop_On_Warning,
      Progressive);

   type Flags is array (Flag) of Boolean;

   function Value (F : in Flag) return C.int
     is (case F is
         when Bottom_Up       =>     2,
         when Fast_Upsample   =>   256,
         when No_Realloc      =>  1024,
         when Fast_DCT        =>  2048,
         when Accurate_DCT    =>  4096,
         when Stop_On_Warning =>  8192,
         when Progressive     => 16384);

   function Value (F : in Flags) return C.int
     is ((if F (Bottom_Up)       then Value (Bottom_Up)       else 0)
       + (if F (Fast_Upsample)   then Value (Fast_Upsample)   else 0)
       + (if F (No_Realloc)      then Value (No_Realloc)      else 0)
       + (if F (Fast_DCT)        then Value (Fast_DCT)        else 0)
       + (if F (Accurate_DCT)    then Value (Accurate_DCT)    else 0)
       + (if F (Stop_On_Warning) then Value (Stop_On_Warning) else 0)
       + (if F (Progressive)     then Value (Progressive)     else 0));

   type Err is (Warning, Fatal);

   type Operation is
     (None,
      H_Flip,
      V_Flip,
      Transpose,
      Transverse,
      Rot_90,
      Rot_180,
      Rot_270);

   type Operation_Flag is
     (Perfect,
      Trim,
      Crop,
      Gray,
      No_Output,
      Progressive,
      Copy_None);

   type Operation_Flags is array (Operation_Flag) of Boolean;

   function Op_Value (F : in Operation_Flag) return C.int
     is (case F is
         when Perfect     =>  1,
         when Trim        =>  2,
         when Crop        =>  4,
         when Gray        =>  8,
         when No_Output   => 16,
         when Progressive => 32,
         when Copy_None   => 64);

   function Op_Value (F : in Operation_Flags) return C.int
     is ((if F (Perfect)      then Op_Value (Perfect)      else 0)
       + (if F (Trim)         then Op_Value (Trim)         else 0)
       + (if F (Crop)         then Op_Value (Crop)         else 0)
       + (if F (Gray)         then Op_Value (Gray)         else 0)
       + (if F (No_Output)    then Op_Value (No_Output)    else 0)
       + (if F (Progressive)  then Op_Value (Progressive)  else 0)
       + (if F (Copy_None)    then Op_Value (Copy_None)    else 0));

   type Scaling_Factor is record
      Num, Denom : C.int;
   end record with Convention => C;

   type Scaling_Factor_Array
     is array (C.unsigned range <>) of aliased Scaling_Factor
     with Convention => C;

   package Scaling_Factors is new Interfaces.C.Pointers
     (Index => C.unsigned,
      Element => Scaling_Factor,
      Element_Array => Scaling_Factor_Array,
      Default_Terminator => (Num => 0, Denom => 0));

   type Region is record
      X, Y, W, H : C.int;
   end record with Convention => C;

   --  TODO: tjtransform

   type Void is null record with Convention => C;
   type Handle is access all Void
     with Convention => C,
          Storage_Size => 0;

   type Byte_Array
     is array (C.unsigned_long range <>) of aliased C.unsigned_char
     with Convention => C;

   package Bytes is new Interfaces.C.Pointers
     (Index => C.unsigned_long,
      Element => C.unsigned_char,
      Element_Array => Byte_Array,
      Default_Terminator => 0);

   type Plane_Label is (Y, Cb, Cr);
   type Planes_Buffer is array (Plane_Label) of Bytes.Pointer
     with Convention => C;
   type Stride_Array is array (Plane_Label) of C.int
     with Convention => C;

   --  TODO; TJPAD

   Success : constant := 0;

   function Scaled
     (Dimension : in C.int;
      Factor : in Scaling_Factor)
     return C.int
     is ((Dimension * Factor.Num + Factor.Denom - 1) / Factor.Denom);

   function Init_Compress return Handle
     with Import, Convention => C, External_Name => "tjInitCompress";

   function Compress
     (H : in Handle;
      Src_Buf : in Bytes.Pointer;
      Width : in C.int;
      Pitch : in C.int;
      Height : in C.int;
      Pixel_Format : in C.int;
      Jpeg_Buf : in out Bytes.Pointer;
      Jpeg_Size : in out C.unsigned_long;
      Jpeg_Subsamp : in C.int;
      Jpeg_Qual : in C.int;
      Flags : in C.int)
     return C.int
     with Import, Convention => C, External_Name => "tjCompress2";

   function Compress_From_YUV
     (H : in Handle;
      Src_Buf : in Bytes.Pointer;
      Width : in C.int;
      Pad : in C.int;
      Height : in C.int;
      Sub_Samp : in C.int;
      Jpeg_Buf : in out Bytes.Pointer;
      Jpeg_Size : in out C.unsigned_long;
      Jpeg_Qual : in C.int;
      Flags : in C.int)
     return C.int
     with Import, Convention => C, External_Name => "tjCompressFromYUV";

   function Compress_From_YUV_Planes
     (H : in Handle;
      Src_Buf : in Planes_Buffer;
      Width : in C.int;
      Strides : in Stride_Array;
      Height : in C.int;
      Sub_Samp : in C.int;
      Jpeg_Buf : in out Bytes.Pointer;
      Jpeg_Size : in out C.unsigned_long;
      Jpeg_Qual : in C.int;
      Flags : in C.int)
     return C.int
     with Import, Convention => C, External_Name => "tjCompressFromYUVPlanes";

   function Buf_Size
     (Width : in C.int;
      Height : in C.int;
      Sub_Samp : in C.int)
     return C.unsigned_long
     with Import, Convention => C, External_Name => "tjBufSize";

   function Buf_Size_YUV
     (Width : in C.int;
      Pad : in C.int;
      Height : in C.int;
      Sub_Samp : in C.int)
     return C.unsigned_long
     with Import, Convention => C, External_Name => "tjBufSizeYUV2";

   function Plane_Size_YUV
     (Component_Id : in C.int;
      Width : in C.int;
      Stride : in C.int;
      Height : in C.int;
      Sub_Samp : in C.int)
     return C.unsigned_long
     with Import, Convention => C, External_Name => "tjPlaneSizeYUV";

   function Plane_Width
     (Component_Id : in C.int;
      Width : in C.int;
      Sub_Samp : in C.int)
     return C.int
     with Import, Convention => C, External_Name => "tjPlaneWidth";

   function Plane_Height
     (Component_Id : in C.int;
      Height : in C.int;
      Sub_Samp : in C.int)
     return C.int
     with Import, Convention => C, External_Name => "tjPlaneHeight";

   function Encode_YUV
     (H : in Handle;
      Src_Buf : in Bytes.Pointer;
      Width : in C.int;
      Pitch : in C.int;
      Height : in C.int;
      Pixel_Format : in C.int;
      Dest_Buf : in Bytes.Pointer;
      Pad : in C.int;
      Sub_Samp : in C.int;
      Flags : in C.int)
     return C.int
     with Import, Convention => C, External_Name => "tjEncodeYUV3";

   function Encode_YUV_Planes
     (H : in Handle;
      Src_Buf : in Bytes.Pointer;
      Width : in C.int;
      Pitch : in C.int;
      Height : in C.int;
      Pixel_Format : in C.int;
      Dest_Buf : in Planes_Buffer;
      Strides : in Stride_Array;
      Sub_Samp : in C.int;
      Flags : in C.int)
     return C.int
     with Import, Convention => C, External_Name => "tjEncodeYUVPlanes";

   function Init_Decompress return Handle
     with Import, Convention => C, External_Name => "tjInitDecompress";

   function Decompress_Header
     (H : in Handle;
      Jpeg_Buf : in Bytes.Pointer;
      Jpeg_Size : in C.unsigned_long;
      Width : out C.int;
      Height : out C.int;
      Sub_Samp : out C.int;
      Color_Space : out C.int)
     return C.int
     with Import, Convention => C, External_Name => "tjDecompressHeader3";

   function Get_Scaling_Factors
     (Count : out C.int) return Scaling_Factors.Pointer
     with Import, Convention => C, External_Name => "tjGetScalingFactors";

   function Decompress
     (H : in Handle;
      Jpeg_Buf : in Bytes.Pointer;
      Jpeg_Size : in C.unsigned_long;
      Dest_Buf : in Bytes.Pointer;
      Width : in C.int;
      Pitch : in C.int;
      Height : in C.int;
      Pixel_Format : in C.int;
      Flags : in C.int)
     return C.int
     with Import, Convention => C, External_Name => "tjDecompress2";

   function Decompress_To_YUV
     (H : in Handle;
      Jpeg_Buf : in Bytes.Pointer;
      Jpeg_Size : in C.unsigned_long;
      Dest_Buf : in Bytes.Pointer;
      Width : in C.int;
      Pad : in C.int;
      Height : in C.int;
      Flags : in C.int)
     return C.int
     with Import, Convention => C, External_Name => "tjDecompressToYUV2";

   function Decompress_To_YUV_Planes
     (H : in Handle;
      Jpeg_Buf : in Bytes.Pointer;
      Jpeg_Size : in C.unsigned_long;
      Dest_Buf : in Planes_Buffer;
      Width : in C.int;
      Strides : in Stride_Array;
      Height : in C.int;
      Flags : in C.int)
     return C.int
     with Import, Convention => C, External_Name => "tjDecompressToYUVPlanes";

   function Decode_YUV
     (H : in Handle;
      Src_Buf : in Bytes.Pointer;
      Pad : in C.int;
      Sub_Samp : in C.int;
      Dest_Buf : in Bytes.Pointer;
      Width : in C.int;
      Pitch : in C.int;
      Height : in C.int;
      Pixel_Format : in C.int;
      Flags : in C.int)
     return C.int
     with Import, Convention => C, External_Name => "tjDecodeYUV";

   function Decode_YUV_Planes
     (H : in Handle;
      Src_Buf : in Planes_Buffer;
      Strides : in Stride_Array;
      Sub_Samp : in C.int;
      Dest_Buf : in Bytes.Pointer;
      Width : in C.int;
      Pitch : in C.int;
      Height : in C.int;
      Pixel_Format : in C.int;
      Flags : in C.int)
     return C.int
     with Import, Convention => C, External_Name => "tjDecodeYUVPlanes";

   function Init_Transform return Handle
     with Import, Convention => C, External_Name => "tjInitTransform";

   --  TODO: tjTransform

   function Destroy (H : in Handle) return C.int
     with Import, Convention => C, External_Name => "tjDestroy";

   function Alloc (Count : in C.int) return Bytes.Pointer
     with Import, Convention => C, External_Name => "tjAlloc";

   function Load_Image
     (Filename : in C.char_array;
      Width : out C.int;
      Align : in C.int;
      Height : out C.int;
      Pixel_Format : out C.int;
      Flags : in C.int)
     return Bytes.Pointer
     with Import, Convention => C, External_Name => "tjLoadImage";

   function Save_Image
     (Filename : in C.char_array;
      Buffer : in Bytes.Pointer;
      Width : in C.int;
      Pitch : in C.int;
      Height : in C.int;
      Pixel_Format : in C.int;
      Flags : in C.int)
     return Bytes.Pointer
     with Import, Convention => C, External_Name => "tjSaveImage";

   procedure Free (Buffer : in Bytes.Pointer)
     with Import, Convention => C, External_Name => "tjFree";

   function Get_Error_Str (H : in Handle) return C.Strings.chars_ptr
     with Import, Convention => C, External_Name => "tjGetErrorStr2";

   function Get_Error_Code (H : in Handle) return C.int
     with Import, Convention => C, External_Name => "tjGetErrorCode";

end TurboJPEG_Thin;
