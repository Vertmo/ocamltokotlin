let f1 _ y z = y

let f5params (x : int) (y : int) (z : int) (t : int) (u : int) = (); z

let ignore _ = ()

let _ =
  ignore (f5params 1 2 3)

let g f = f ()

let _ = g f1 () ()
