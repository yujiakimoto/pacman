let arr_to_list arr = 
  Array.map Array.to_list arr |> Array.to_list

let list_to_arr lst = 
  List.map Array.of_list lst |> Array.of_list

let position_to_string pos = 
  "(" ^ (fst pos |> string_of_int) ^ ", " ^ (snd pos |> string_of_int) ^ ")"

let remove lst pos = 
  let rec remove' lst acc = 
    match lst with 
    | [] -> acc
    | h :: t -> if h = pos then remove' t acc else remove' t (h :: acc)
  in remove' lst []

let modulo n d = 
  let r = n mod d in if r >= 0 then r else r + d 

let decisecond () = 
  let t = int_of_float (Unix.gettimeofday () *. 10.0) in modulo t 10

let centisecond () =
  let t = int_of_float (Unix.gettimeofday () *. 100.0) in modulo t 100

let even n = 
  if modulo n 2 = 0 then true else false

let distance (x1, y1) (x2, y2) = 
  ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1)) |> float_of_int |> sqrt

let random_sample lst = 
  let i = Random.int (List.length lst) in List.nth lst i