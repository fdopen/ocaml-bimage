open Type

type layout =
  | Planar
  | Interleaved

type ('a, 'b, 'c) t = {
  width: int;
  height: int;
  color: 'c Color.t;
  step: int;
  layout: layout;
  data: ('a, 'b) Data.t;
}

let create ?(layout = Interleaved) ?mmap kind color width height =
  let channels = channels_of_color color in
  let data = Data.create ?mmap kind (width * height * channels) in
  let step = width * channels in
  {width; height; color; step; layout; data}

let of_data color width height layout data =
  let channels = channels_of_color color in
  let step = width * channels in
  if step * height <> Data.length data then
    Error.exc `Invalid_shape
  else
  {width; height; color; step; layout; data}

let like image =
  create ~layout:image.layout (Data.kind image.data) image.color image.width image.height

let like_with_kind kind image =
  create ~layout:image.layout kind image.color image.width image.height

let like_with_color color image =
  create ~layout:image.layout (Data.kind image.data) color image.width image.height

let like_with_layout layout image =
  create ~layout (Data.kind image.data) image.color image.width image.height

let copy image =
  let data = Data.copy image.data in
  of_data image.color image.width image.height image.layout data

let random ?(layout = Interleaved) kind color width height =
  let channels = channels_of_color color in
  let data = Data.random kind (width * height * channels) in
  let step = width * channels in
  {width; height; color; step; layout; data}

let channels {color; _} = channels_of_color color
let kind {data; _} = Data.kind data [@@inline]
let color {color; _} = color
let layout {layout; _} = layout
let shape {width; height; color; _} = width, height, channels_of_color color
let length {step; height; _} = step * height [@@inline]
let empty_pixel image = Pixel.empty (channels image)
let empty_data image = Data.create (kind image) (channels image)

let convert_to ~dest img =
  let dest_k = kind dest in
  let src_k = kind img in
  for i = 0 to length dest - 1 do
    dest.data.{i} <- Kind.convert ~from:src_k dest_k img.data.{i}
  done

let convert k img =
  let dest = create ~layout:img.layout k img.color img.width img.height in
  convert_to ~dest img;
  dest

let of_any_color im color: (('a, 'b, 'c) t, Error.t) result =
  if Color.channels color = Color.channels im.color then
    Ok (of_data color im.width im.height im.layout im.data)
  else
    Error `Invalid_color

let index image x y c =
  match image.layout with
  | Planar -> image.width * image.height * c + y * image.width + x
  | Interleaved -> y * image.step + image.color.Color.channels * x
[@@inline]

let index_at image offs =
  Data.slice image.data ~offs ~length:image.color.Color.channels

let get image x y c =
  let index = index image x y c in
  if index < 0 || index >= length image then Kind.min (kind image)
  else image.data.{index}
[@@inline]

let set image x y c v =
  let index = index image x y c in
  image.data.{index} <- v
[@@inline]

let get_f image x y c =
  let kind = kind image in
  get image x y c
  |> Kind.to_float kind

let set_f image x y c v =
  let kind = kind image in
  let v = Kind.of_float kind v in
  set image x y c v

let get_n image x y c =
  let kind = kind image in
  get image x y c
  |> Kind.to_float kind
  |> Kind.normalize kind

let set_n image x y c v =
  let kind = kind image in
  let v = Kind.denormalize kind v
    |> Kind.of_float kind
  in
  set image x y c v

let get_pixel image ?dest x y =
  let c = channels image in
  let Pixel.Pixel px = match dest with Some px -> px | None -> Pixel.empty c in
  for i = 0 to c - 1 do
    px.{i} <- get_f image x y i
  done;
  Pixel.Pixel px

let set_pixel image x y (Pixel.Pixel px) =
  let c = channels image in
  for i = 0 to c - 1 do
    set_f image x y i px.{i}
  done

let get_data image ?dest x y =
  let c = channels image in
  let px = match dest with Some px -> px | None -> Data.create (kind image) c in
  for i = 0 to c - 1 do
    px.{i} <- get image x y i
  done;
  px

let set_data image x y px =
  let c = channels image in
  for i = 0 to c - 1 do
    set image x y i px.{i}
  done

let each_pixel f ?(x = 0) ?(y = 0) ?width ?height img =
  let width =
    match width with
    | Some w -> min (img.width - x) w
    | None -> img.width - x
  in
  let height =
    match height with
    | Some h -> min (img.height - y) h
    | None -> img.height - y
  in
  let px = empty_data img in
  for j = y to y + height - 1 do
    for i = x to x + width - 1 do
      let px = get_data img ~dest:px i j in
      f i j px
    done
  done
[@@inline]

let avg ?(x = 0) ?(y = 0)  ?width ?height img =
  let width = match width with None -> img.width - x | Some w -> min w (img.width - x) in
  let height = match height with None -> img.height - y | Some h -> min h (img.width - y) in
  let avg = Data.create f32 (channels img) in
  let channels = channels img in
  let size = float_of_int (width * height) in
  let kind = kind img in
  each_pixel (fun _x _y px ->
    for i = 0 to channels - 1 do
      avg.{i} <- avg.{i} +. Kind.to_float kind px.{i}
    done
  ) ~x ~y ~width ~height img;
  for i = 0 to channels - 1 do
    avg.{i} <- avg.{i} /. size
  done;
  avg

let convert_layout layout im =
  let width, height, _ = shape im in
  let dest = create ~layout (kind im) (color im) width height in
  each_pixel (fun x y px ->
    for i = 0 to Data.length px - 1 do
      dest.data.{index dest x y i} <- px.{i}
    done
  ) im;
  dest

