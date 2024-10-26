(* find primes between two numbers *)

let rec reverse (xs: string) : string =
  if xs = "" then ""
  else (reverse (String.sub xs 1 ((String.length xs) - 1))) ^ (String.sub xs 0 1);;

let reverse_num (n: int) : int =
  int_of_string (reverse (string_of_int n));;

let rec is_prime n d =
  if d * d > n then true
  else if n mod d = 0 then false
  else is_prime n (d + 1);;

let range m n =
  let rec aux m n acc =
    if m > n then acc
    else aux m (n - 1) (n :: acc)
  in aux m n [];;

let backwards_prime m n : int list =
  List.filter (fun x -> is_prime x 2 
                && is_prime (reverse_num x) 2 
                && x != (reverse_num x)) (range m n);; 

let p_list = backwards_prime 7000 7100;;
List.iter (fun x -> print_int x; print_string " ") p_list;;


print_endline " " ;;
print_endline (reverse_num 1234 |> string_of_int);;
print_endline (string_of_bool (is_prime 7001 2));;

