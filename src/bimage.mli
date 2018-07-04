(*---------------------------------------------------------------------------
   Copyright (c) 2018 Zach Shipko. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Image processing library

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1 Bimage} *)
open Bigarray

exception Unsupported
(** Raised when attempting to use Char, Int8_signed, Int16_signed Bigarray types *)

type u8 = int8_unsigned_elt
type u16 = int16_unsigned_elt
type i32 = int32_elt
type i64 = int64_elt
type f32 = float32_elt
type f64 = float64_elt
type c32 = complex32_elt
type c64 = complex64_elt

val u8: (int, u8) kind
val u16: (int, u16) kind
val i32: (int32, i32) kind
val i64: (int64, i64) kind
val f32: (float, f32) kind
val f64: (float, f64) kind
val c32: (Complex.t, c32) kind
val c64: (Complex.t, c64) kind

type 'a color =
  | Gray: [`Gray] color
  | Rgb: [`Rgb] color
  | Rgba: [`Rgba] color
  | Channels: int -> 'a color

type gray = [`Gray]
type rgb = [`Rgb]
type rgba = [`Rgba]

val gray: gray color
val rgb: rgb color
val rgba: rgba color

val channels_of_color: 'a color -> int
(** Returns the number of channels for a given color *)

val kind_max: ('a, 'b) kind -> 'a
(** [kind_max k] returns the maximum normalized value for [k] *)

val kind_min: ('a, 'b) kind -> 'a
(** [kind_min k] returns the minimum normalized value for [k] *)

val to_float: ('a, 'b) kind -> 'a -> float
(** [to_float k x] converts a value of kind [k] to float *)

val of_float: ('a, 'b) kind -> float -> 'a
(** [of_float k x] converts a float to a value of kind [k] *)


(** The Data module defines several operations on one dimensional image data *)
module Data: sig
  type ('a, 'b) t = ('a, 'b, c_layout) Array1.t
  (** Data type *)

  val kind: ('a, 'b) t -> ('a, 'b) kind
  (** Get the [Bigarray.kind] *)

  val of_array: ('a, 'b) kind -> 'a array -> ('a, 'b) t
  (** Converts an array to a [Data.t] of the given kind *)

  val to_array: ('a, 'b) t -> 'a array
  (** Converts a [Data.t] to an array *)

  val create: ('a, 'b) kind -> ?mmap:string -> int -> ('a, 'b) t
  (** Create a new [Data.t] with the given length. If [mmap] is provided then the file
      will be memory mapped to the path specified *)

  val length: ('a, 'b) t -> int
  (** Returns the number of elements in a [Data.t] *)

  val fold2: ('a -> 'd -> 'c -> 'c) -> ('a, 'b) t -> ('d, 'e) t -> 'c -> 'c
  (** Reduce over two [Data.t] *)

  val fold: ('a -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
  (** Reduce over a single [Data.t] *)

  val fill: ('a, 'b) t -> 'a -> unit
  (** [fill d x] sets each value of [d] to [x] *)

  val map_inplace: ('a -> 'a) -> ('a, 'b) t -> unit
  (** [map_inplace f data] runs [f] over each value of [data] *)

  val map2_inplace: ('a -> 'c -> 'a) -> ('a, 'b) t -> ('c, 'd) t -> unit
  (** [map2_inplace f data1 data2] runs [f] over each value of [data1] and [data2] *)

  val slice: offs:int -> length:int -> ('a, 'b) t -> ('a, 'b) t
  (** [slice ~offs ~length data] extracts a section of [data] of [length]
      values starting at index [offs] *)

  val copy_to:  dest:('a, 'b) t -> ('a, 'b) t -> unit
  (** [copy_to ~dest src] copies each value from [src] to [dest] *)

  val copy: ('a, 'b) t -> ('a, 'b) t
  (** Create a new copy of [Data.t] *)

  val convert: ('c, 'd) kind -> ('a -> 'c) -> ('a, 'b) t -> ('c, 'd) t
  (** Convert between [Data.t] types *)

  val convert_to: ('c, 'd) kind -> ('a -> 'c) -> dest:('c, 'd) t -> ('a, 'b) t -> unit
  (** Convert between [Data.t] types with an existing destination image *)

  val of_float: ?dest:('a, 'b) t -> ('a, 'b) kind -> (float, f32) t -> ('a, 'b) t
  (** [of_float ~dest k data] converts a [Data.t] from float to [k], storing the results in
      [dest] if provided. *)

  val to_float: ?dest:(float, f32) t -> ('a, 'b) t -> (float, f32) t
  (** [to_float ~dest data] converts a [Data.t] to float values, storing the results in
      [dest] if provided. *)

  val hash: ('a, 'b) t -> int
  (** Default hash function *)

  val compare: ('a, 'b) t -> ('a, 'b) t -> int
  (** Default comparison function *)

  val equal: ('a, 'b) t -> ('a, 'b) t -> bool
  (** Default equality function *)
end

(** Kernels are used for filtering images using convolution *)
module Kernel: sig
  type t

  val create: int -> int -> t
  (** [create rows cols] makes a new Kernel with the given dimensions *)

  val get: t -> int -> int -> float
  (** [get kernel y x] gets the value at (x, y) *)

  val set: t -> int -> int -> float -> unit
  (** [set kernel y x v] sets the value at (x, y) *)

  val sum: t -> float
  (** Get the sum of each value of a kernel *)

  val normalize: t -> t
  (** [normalize kernel] returns a kernel where each element has been divided by the sum of all elements *)
end

(** The Image module defines a simple interface for manipulating image data *)
module Image: sig
  type ('a, 'b, 'c) t = {
    width: int;
    height: int;
    color: 'c color;
    data: ('a, 'b) Data.t;
  }

  val create: ?mmap:string -> ('a, 'b) kind -> 'c color -> int -> int -> ('a, 'b, 'c) t
  (** [create kind color width height] makes a new image with the given [kind], [color] and dimensions *)

  val like: ('a, 'b) kind -> 'c color -> ('d, 'e, 'f) t -> ('a, 'b, 'c) t
  (** [like kind color img] creates a new image with the same dimensions as [img] with the given [kind] and [color] *)

  val channels: ('a, 'b, 'c) t -> int
  (** Returns the number of channels in an image *)

  val kind: ('a, 'b, 'c) t -> ('a, 'b) kind
  (** Returns the image kind *)

  val shape: ('a, 'b, 'c) t -> int * int * int
  (** Returns the width, height and channels *)

  val convert_to: ?scale:float -> ('d, 'e) kind -> dest:('d, 'e, 'c) t -> ('a, 'b, 'c) t -> unit
  (** Convert an image to an existing image of another kind, optionally using a scale factor *)

  val convert: ?scale:float -> ('d, 'e) kind -> ('a, 'b, 'c) t -> ('d, 'e, 'c) t
  (** Convert an image to a new image of another kind, optionally using a scale factor *)

  val at: ('a, 'b, 'c) t -> int -> int -> ('a, 'b) Data.t
  (** [at image x y] returns a [Data.t] with data from (x, y). The data contains one value
      for each channel of the image *)

  val get: ('a, 'b, 'c) t -> int -> int -> int -> float
  (** [get image x y c] returns a float representation of the value at (x, y, c). This is
      equivalent to  [to_float (kind image) (at image x y).{c}] *)

  val set: ('a, 'b, 'c) t -> int -> int -> int -> float -> unit
  (** Set a single channel of the given image at (x, y) *)

  val each_pixel: (int -> int -> ('a, 'b) Data.t -> unit) -> ('a, 'b, 'c) t -> unit
  (** Iterate over each pixel in an image. The data segment used in the callback is mutable and
      will write directly to the underlying image data. *)
end

(** Op is used to define pixel-level operations *)
module Op: sig
  type ('a, 'b, 'c) t = int -> int -> int -> ('a, 'b, 'c) Image.t array -> float

  val blend: ('a, 'b, 'c) t
  (** Blend two images: [a + b / 2] *)

  val min: ('a, 'b, 'c) t
  (** Minimum pixel value of two images *)

  val max: ('a, 'b, 'c) t
  (** Maximum pixel value of two images *)

  val grayscale: ('a, 'b, [< `Rgb | `Rgba]) t
  (** Convert a color image to grayscale *)

  val color: ('a, 'b, [`Gray]) t
  (** Convert a grayscale image to color *)

  val eval: ('a, 'b, 'c) t -> ('d, 'e, 'f) Image.t -> ('a, 'b, 'c) Image.t array -> unit
  (** Evaluate an operation *)

  val join: (float -> float -> float) -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t
  (** [join f a b] builds a new operation from [f(a, b)] *)

  val map: ('a, 'b, 'c) t -> (float -> float) -> ('a, 'b, 'c) t
  (** [map a f] builds a new operation from [f(a)] *)

  val scalar: float -> ('a, 'b, 'c) t
  (** Builds an operation returning a single value *)

  val ( &+ ): ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t
  (** Infix operator for [join] using addition *)

  val ( &- ): ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t
  (** Infix operator for [join] using subtraction *)

  val ( &* ): ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t
  (** Infix operator for [join] using multiplication *)

  val ( &/ ): ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t
  (** Infix operator for [join] using division *)

  val ( $ ): ('a, 'b, 'c) t -> (float -> float) -> ('a, 'b, 'c) t
  (** Infix operator for [map] *)
end

(** Magick defines image I/O operations using ImageMagick/GraphicsMagick on the
    command-line *)
module Magick: sig
  val read: string -> ('a, 'b) kind -> 'c color -> ('a, 'b, 'c) Image.t option
  (** [read filename kind color] loads an image from [filename] on disk using the given [kind] and [color] *)

  val write: string -> ('a, 'b, 'c) Image.t -> unit
  (** [write filename image] saves an image to [filename] *)

  val command: string ref
  (** [command] contains the command used to call out to ImageMagick/GraphicsMagick. For example,
      if you'd like to use GraphicsMagick then set this to "gm convert" *)
end

(*---------------------------------------------------------------------------
   Copyright (c) 2018 Zach Shipko

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
