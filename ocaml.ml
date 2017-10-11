(* 99problems in ocaml.. a WIP *)

(* 1. Write a function last : 'a list -> 'a option that returns the last element of a list.  *)
let rec last = function
  | [] -> None
  | [x] -> Some x
  | _ :: t -> last t
;;

(* 2. Find the last but one (last and penultimate) elements of a list. (easy) *)
let rec last_two = function
  | [] -> None
  | x :: y :: [] -> Some (x, y)
  | _ :: t -> last_two t
;;

(* 3. Find the k'th element of a list. *)
let rec at pos = function
  | [] -> None
  | x :: t -> if pos = 1 then Some x else at (pos - 1) t
;;

(* 4. Find the number of elements of a list. *)
let length lst =
  let rec aux i = function
    | [] -> i
    | x :: t -> aux (i + 1) t
  in aux 0 lst
;;

(* 5. Reverse a list. *)
let rev lst =
  let rec aux acc = function
    | [] -> acc
    | x :: t -> aux (x :: acc) t
  in aux [] lst
;;

(* 6. Find out whether a list is a palindrome. *)
let is_palindrome a =
  a = rev a
;;

(* 7. Flatten a nested list structure. *)
type 'a node =
  | One of 'a
  | Many of 'a node list
;;

let flatten o =
  let rec aux acc = function
    | [] -> acc
    | One x :: t -> aux (x :: acc) t
    | Many l :: t -> aux (aux acc l) t
  in rev (aux [] o)
;;

(* 8. Eliminate consecutive duplicates of list elements. *)
let compress2 lst =
  let rec aux acc a = function
    | [] -> a :: acc
    | x :: xs ->
      if x = a then
        aux acc a xs
      else
        aux (a :: acc) x xs
  in rev (aux [] (List.hd lst) (List.tl lst))
;;

let rec compress = function
  | a :: (b :: _ as t) -> if a = b then compress t else a :: compress t
  | a -> a  (* anything else *)
;;

(* 9. Pack consecutive duplicates of list elements into sublists. *)
let pack lst =
  let rec aux acc curr = function
  | [] -> []
  | [x] -> (x :: curr) :: acc
  | a :: (b :: _ as t) -> if a = b then aux acc (a :: curr) t else aux ((a :: curr) :: acc) [] t
  in rev (aux [] [] lst)
;;

(* 10. Run-length encoding of a list. *)
let encode lst =
  let rec aux acc curr = function
  | [] -> []
  | [x] -> (curr, x) :: acc
  | a :: (b :: _ as t) -> if a = b then aux acc (curr + 1) t else aux ((curr, a) :: acc) 1 t
  in rev (aux [] 1 lst)
;;

(* 11. Modified run-length encoding. *)
type 'a rle =
  | One of 'a
  | Many of int * 'a;;

let encode2 lst =
  let make_result = function
  | a, b when a = 1 -> One(b)
  | a, b -> Many(a, b)
  in
  let rec aux acc curr = function
  | [] -> []
  | [x] -> make_result (curr, x) :: acc
  | a :: (b :: _ as t) ->
    if a = b then
      aux acc (curr + 1) t
    else
      aux (make_result (curr, a) :: acc) 1 t
  in rev (aux [] 1 lst)
;;

(* 12. Decode a run-length encoded list. *)
let decode lst =
  let rec many acc n x =
    if n = 0 then
      acc
    else
      many (x :: acc) (n - 1) x
  in
  let rec aux acc = function
    | [] -> acc
    | One x :: xs -> aux (x :: acc) xs
    | Many (n, x) :: xs -> aux (many acc n x) xs
  in rev (aux [] lst)
;;

(* 13. Run-length encoding of a list (direct solution). *)
(* ??? *)

(* 14. Duplicate the elements of a list. *)
let rec duplicate = function
  | [] -> []
  | x :: xs -> x :: x :: duplicate xs
;;

(* 15. Replicate the elements of a list a given number of times. *)
let replicate lst count =
  let rec many acc n x =
    if n = 0 then
      acc
    else
      many (x :: acc) (n - 1) x
  in
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux (many acc count x) xs
  in rev (aux [] lst)
;;

(* 16. Drop every N'th element from a list. *)
let drop lst count =
  let rec aux n = function
    | [] -> []
    | x :: xs -> if n = count then aux 1 xs else x :: aux (n + 1) xs
  in aux 1 lst
;;

(* 17. Split a list into two parts; the length of the first part is given. *)
let split lst count =
  let rec aux acc n = function
    | [] -> (rev (acc), [])
    | x :: xs as t -> if n = count then (rev acc, t) else aux (x :: acc) (n + 1) xs
  in aux [] 0 lst
;;

(* 18. Extract a slice from a list. *)
let slice lst l r =
  let rec aux acc i = function
    | [] -> acc
    | x :: xs when i >= l && i <= r -> aux (x :: acc) (i + 1) xs
    | x :: xs -> aux acc (i + 1) xs
  in rev (aux [] 0 lst)
;;

(* this version is actually WORSE because it is NOT tail recursive! *)
let slice lst l r =
  let rec aux i = function
    | [] -> []
    | x :: xs when i >= l && i <= r -> x :: aux (i + 1) xs
    | x :: xs -> aux (i + 1) xs
  in aux 0 lst
;;

(* 19. Rotate a list N places to the left. *)
let rotate lst n =
  let rec aux acc i = function
    | [] -> acc
    | x :: xs as t ->
      if i > n then
        t @ acc
      else
        aux (acc @ x :: []) (i + 1) xs
  in aux [] 1 lst
;;

(***** skip some stuff ******)

(* 23. Extract a given number of randomly selected elements from a list. *)
let rand_select lst count =
  let len = List.length lst in
  let rec aux acc i =
    if i >= count then
      acc
    else
      aux (at (Random.int len + 1) lst :: acc) (i + 1)
  in aux [] 0
;;

(* 26. Generate the combinations of K distinct objects chosen from the N elements of a list. *)
(* NOT CORRECT *)
let extract n lst =
  let len = List.length lst in
  let rec take from = function
    | [] -> []
    | x :: xs -> if from = 1 then xs else take (from - 1) xs
  in
  let rec sweep acc i =
    if i < (len - 1) then
      let c = List.nth lst i in
      sweep ((List.map (fun x -> c :: x :: []) (take (i + 1) lst)) :: acc) (i + 1)
    else
      acc
  in rev (sweep [] 0)
;;

(* 31. Determine whether a given integer number is prime. *)
let is_prime n =
  let rec aux i =
    if i = 1 then
      true
    else if n mod i = 0 then
      false
    else
      aux (i - 1)
  in
  if n <= 2 then false
  else aux (n - 1)
;;

(* 32. Determine the greatest common divisor of two positive integer numbers. *)
let rec gcd a b =
  if b = 0 then a
  else gcd b (a mod b)
;;

(* 33. Determine whether two positive integer numbers are coprime. *)
let coprime a b = (gcd a b) = 1 ;;

(* 34. Calculate Euler's totient function *)
let phi m =
  let rec aux acc i =
    if i = m then acc
    else if coprime i m then aux (acc + 1) (i + 1)
    else aux acc (i + 1)
  in aux 0 1
;;

(* 35. Determine the prime factors of a given positive integer. *)
let factors n =
  let rec aux d n =
    if n = 1 then [] else
      if n mod d = 0 then d :: aux d (n / d) else aux (d+1) n
  in
  aux 2 n;;

(* 36. Determine the prime factors of a given positive integer (2). *)
let factors2 n = encode (factors n) ;;

(* 37. Calculate Euler's totient function φ(m) (improved). *)
let phi_improved n =
  let int_pow x y = int_of_float (float_of_int x ** float_of_int y) in
  let rec aux acc = function
    | [] -> acc
    | (m, p) :: xs -> aux (acc * (p - 1) * (int_pow p (m - 1))) xs
  in aux 1 (factors2 n)
;;

(* 38. Compare the two methods of calculating Euler's totient function. *)
let timeit f a =
  let t0 = Unix.gettimeofday() in
  ignore(f a);
  let t1 = Unix.gettimeofday() in
  t1 -. t0;;

(* 39. A list of prime numbers. *)
let all_primes l u =
  let rec aux acc i =
    if i > u then acc
    else if is_prime i then aux (i :: acc) (i + 1)
    else aux acc (i + 1)
  in aux [] l
;;

(* 46 & 47. Truth tables for logical expressions (2 variables). *)

type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr
;;

exception Invalid_variable of string;;

let eval_bool_expr2 a_label a_value b_label b_value expr =
  let rec aux = function
    | Var x ->
      if x = a_label then a_value
      else if x = b_label then b_value
      else raise (Invalid_variable x)
    | Not x -> not (aux x)
    | And (x, y) -> (aux x) && (aux y)
    | Or (x, y) -> (aux x) || (aux y)
  in aux expr
;;

(* disjointed combinations.. ? not right for this task.. *)
let combinations lst =
  let rec aux acc i =
    let a, tail = split lst i in
    let head = List.hd (List.rev a) in
    if i > List.length lst then
      acc
    else
      aux ((List.map (fun x -> [head; x]) tail) @ acc) (i + 1)
  in List.rev (aux [] 1)
;;

let all_combinations lst n =
  let rec aux acc i =
    let a, tail = split lst i in
    let head = List.hd (List.rev a) in
    if i > List.length lst then
      acc
    else
      aux ((List.map (fun x -> [head; x]) tail) @ acc) (i + 1)
  in List.rev (aux [] 1)
;;

(* emulate a binary counter *)
let mod_2_counter lst =
  let rec aux acc carry = function
    | [] ->
      if carry then
        true :: acc
      else
        acc
    | x :: xs ->
      match (x, carry) with
      | true, true -> aux (false :: acc) true xs
      | true, false -> aux (true :: acc) carry xs
      | false, _ -> aux (carry :: acc) false xs
  in aux [] true (List.rev lst)
;;

let mod_2_combinations lst =
  let int_pow x y = int_of_float (float_of_int x ** float_of_int y) in
  let n = int_pow 2 (List.length lst) in
  let rec aux acc i curr =
    if i = n then
      acc
    else
      let next = mod_2_counter curr in
      aux (next :: acc) (i + 1) next
  in aux [lst] 1 lst
;;

let table2 a b expr =
  let combinations = (mod_2_combinations [false; false]) in
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> match x with
      | a_val :: b_val :: [] -> aux ((a_val, b_val, (eval_bool_expr2 a a_val b b_val expr)) :: acc) xs
      | _ -> raise (Invalid_variable "foo")
  in List.rev (aux [] combinations)
;;

(* 48. Truth tables for logical expressions. *)
let rec repeated_sequence el n =
  if n = 0 then []
  else el :: repeated_sequence el (n - 1)
;;

let rec get_var k = function
  | (a, b) :: xs when a = k -> b
  | _ :: xs -> get_var k xs
  | [] -> raise (Invalid_variable k)
;;

let eval_bool_expr variable_combinations expr =
  let rec aux = function
    | Var x -> get_var x variable_combinations
    | Not x -> not (aux x)
    | And (x, y) -> (aux x) && (aux y)
    | Or (x, y) -> (aux x) || (aux y)
  in aux expr
;;

let table (variables: string list) expr =
  let combinations = (mod_2_combinations (repeated_sequence false (List.length variables))) in
  let variable_combinations = List.map (fun x -> List.combine variables x) combinations in
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux ((x, (eval_bool_expr x expr)) :: acc) xs
  in List.rev (aux [] variable_combinations)
;;

(* 49. Gray code. *)
let gray n =
  let rec aux acc i =
    if i = n then
      acc
    else
      let next = List.map (fun x -> "0" ^ x) acc @ List.map (fun x -> "1" ^ x) (List.rev acc) in
      aux next (i + 1)
  in aux ["0"; "1"] 1
;;


(* 50. Huffman code *)

type 'a binary_tree =
  | Leaf of (int * 'a)
  | Tree of (int * ('a binary_tree * 'a binary_tree))
;;

(* first a lame "priority queue" *)
module Pq = struct
  type 'a weighted_item = (int * 'a binary_tree)
  type 'a t = 'a weighted_item list

  let empty = []

  let insert pq a =
    let sort_func a b =
      let w1 = match a with
        | Tree (w, _) | Leaf (w, _) -> w in
      let w2 = match b with
        | Tree (w, _) | Leaf (w, _) -> w in
      if w1 < w2 then -1 else if w1 == w2 then 0 else 1 in
    let lst = a :: pq in
    List.stable_sort sort_func lst

let remove_min = function
    | hd :: tl -> (Some hd, tl)
    | [] -> (None, [])
end

let make_pq lst =
  let rec aux pq = function
    | [] -> pq
    | (v, w) :: xs -> aux (Pq.insert pq (Leaf (w, v))) xs
  in aux Pq.empty lst
;;

(* build the actual huffman tree *)
let make_huffman_tree lst =
  let pq = make_pq lst in
  let rec build_tree pq =
    let left, pq1 = Pq.remove_min pq in
    let right, pq2 = Pq.remove_min pq1 in
    match left, right with
      | Some x, Some y ->
        let w1 = match x with
          | Tree (w, _) | Leaf (w, _) -> w in
        let w2 = match y with
          | Tree (w, _) | Leaf (w, _) -> w in
        let new_tree = Tree (w1 + w2, (x, y)) in
        let new_pq = Pq.insert pq2 new_tree in
            build_tree new_pq      
      | _, _ ->
        pq
        (* failwith "shouldn't happen?" *)
  in List.hd (build_tree pq)
;;

(* dfs to report codes *)
let huffman lst =
  let tree = make_huffman_tree lst in
  let rec dfs acc curr = function
    | Tree (_, (x, y)) -> 
    (dfs acc (curr ^ "0") x) @ (dfs acc (curr ^ "1") y)
    | Leaf (_, x) -> (x, curr) :: acc
  in dfs [] "" tree
;;


(* 55. Construct completely balanced binary trees. *)

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;
