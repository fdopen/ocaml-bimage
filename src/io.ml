open Type

module Magick = struct
  let pixel_type: type a. a color -> string = function
    | Gray -> "gray"
    | Rgb -> "rgb"
    | Rgba -> "rgba"
    | _ -> failwith "unsupported"

  let command = ref "convert"

  let read filename t color =
    try
      let read_image_data t filename img =
        let f = pixel_type img.Image.color in
        let channels = Image.channels img in
        let cmd = Printf.sprintf "%s '%s' -depth 8 %s:-" !command filename f in
        let input = Unix.open_process_in cmd in
        let kind = Image.kind img in
        let fmax = to_float kind (kind_max kind) in
        let fmin = to_float kind (kind_min kind) in
        for i = 0 to (Image.(img.width *  img.height)  * channels) - 1 do
          let x = to_float u8 (input_byte input) in
          let y = x *. ((fmax -. fmin) /. 255.) in
          img.Image.data.{i} <- of_float kind y
        done;
        close_in input
      in
      (* Read image size *)
      let identify = Unix.open_process_in ("identify " ^ filename) in
      let s = input_line identify in
      let () = close_in identify in
      let shape =
        String.split_on_char ' ' s
        |> fun x -> List.nth x 2
        |> String.split_on_char 'x'
      in
      match List.map int_of_string shape with
      | x::y::[] ->
          let img = Image.create t color x y in
          let () = read_image_data t filename img in
          Some img
      | _ -> None (* Invalid image/shape *)
    with End_of_file -> None | Failure _ -> None

  let write filename img =
    let width, height, channels = Image.shape img in
    let f = pixel_type img.Image.color in
    let cmd = Printf.sprintf "%s -depth 8 -size %dx%d %s:- '%s'" !command width height f filename in
    let output = Unix.open_process_out cmd in
    let kind = Image.kind img in
    let fmax = to_float kind (kind_max kind) in
    let fmin = to_float kind (kind_min kind) in
    for i = 0 to Image.(img.width *  img.height)  * channels - 1 do
      let x = to_float kind img.Image.data.{i} in
      let y = x *. (255. /. (fmax -. fmin)) in
      output_byte output (of_float u8 y)
    done;
    close_out output
end
