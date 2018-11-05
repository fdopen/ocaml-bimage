type feature_map = (Hash.t, int * int) Hashtbl.t

let gen ?(size = 8) image =
  let ht = Hashtbl.create 8 in
  let s = ref Hash.Set.empty in
  Image.for_each (fun x y _px ->
      try
        let im = Image.crop image ~x ~y ~width:size ~height:size in
        let hash = Hash.phash im in
        s := Hash.Set.add hash !s;
        Hashtbl.add ht hash (x, y)
      with Invalid_argument _ -> ()) image;
  ht, !s

let has_overlap a b =
  let hs, s = gen a in
  let ht, t = gen b in
  hs, ht, Hash.Set.inter s t
