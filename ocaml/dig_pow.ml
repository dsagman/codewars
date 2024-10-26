
let list_of_int n =
  string_of_int n 
  |> String.to_seq 
  |> List.of_seq
  |> List.map (fun c -> int_of_string (String.make 1 c));;

let rec pow x n =
  if n = 0 then 1 else x * pow x (n - 1);;

let sum_list l = List.fold_left (fun acc x -> acc + x) 0 l;;

let dig_pow n p =
  let digits = list_of_int n in
  let sum = sum_list ((List.mapi (fun i x -> pow x (p + i))) digits)
  in if sum mod n = 0 then sum / n else -1;; 

