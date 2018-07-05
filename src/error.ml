type t = [
  | `Invalid_shape
  | `Msg of string
]

let to_string = function
  | `Invalid_shape -> "invalid shape"
  | `Msg m -> m

exception Exc of t

let exc x = raise (Exc x)

let unwrap = function
  | Ok x -> x
  | Error e -> exc e
