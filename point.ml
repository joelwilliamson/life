type t = (int * int)
let create (x,y) = (x,y)

let x (x,y) = x

let y (x,y) = y

let add (x1,y1) (x2,y2) = (x1+x2),(y1+y2)

let sub (x1,y1) (x2,y2) = (x1-x2),(y1-y2)

let addsub (x1,y1) (x2,y2) = (x1+x2),(y1-y2)

let subadd (x1,y1) (x2,y2) = (x1-x2),(y1+y2)

let to_string (x,y) = "("^(string_of_int x)^","^(string_of_int y)^")"
