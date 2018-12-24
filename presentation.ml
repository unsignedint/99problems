(* basics! *)

3 * 4 ;;

let x = 6 ;;

let y = x + 1 ;;

2 * 0.5 ;;

2.0 * 0.5 ;;

2.0 *. 0.5 ;;

"Hello" ^ "world" ;;

let greet name =
  "hey " ^ name ;;

greet "Jim" ;;

greet 2 ;;

let yell sentence =
  sentence ^ "!!" ;;

yell greet "Jim" ;;

yell (greet "Jim") ;;

(* unwind double application, g(f(x)) *)
greet "Bob" |> yell ;;

"Bob" |> greet |> yell ;;

"Bob" |> greet |> yell |> String.uppercase_ascii ;;

(* higher-order functions and partial application *)
let square x = x * x ;;

let quad x =
  square (square x)
;;

let twice f x =
  f (f x)
;;

let half x = x / 2 ;;

let quarter = twice half ;;

let float_quarter = twice (fun x -> x /. 2.0) ;;

(* pattern matching *)
let is_banana word =
  match word with
  | "banana" -> true
  | _ -> false
;;

let fruit_or_not word =
  match word with
  | "banana" | "apple" -> true
  | _ -> false
;;

let fizzbuzzer x =
  match x with
  | x when x mod 15 = 0 -> "fizzbuzz"
  | x when x mod 3 = 0 -> "fizz"
  | x when x mod 5 = 0 -> "buzz"
  | _ -> ""
;;

(* lists *)
[1;2;3;4;5;6;7;8;9;10] |> List.map fizzbuzzer ;;

3 :: [1;2;3] ;;

let rec sum = function
  | [] -> 0
  | hd :: tl -> hd + sum tl
;;

(*
= 1 + sum [2;3]
= 1 + (2 + sum [3])
= 1 + (2 + (3 + sum []))
= 1 + (2 + (3 + 0))
= 1 + (2 + 3)
= 1 + 5
= 6
*)

(* tail recursion *)
let sum' lst =
  let rec aux acc = function
    | [] -> acc
    | hd :: tl -> aux (acc + hd) tl
  in
  aux 0 lst
;;

let make_range n =
  let rec aux acc i =
    if i > n then acc
    else aux (i :: acc) (i + 1)
  in
  aux [] 0
;;

List.map fizzbuzzer (make_range 20) ;;

(* optional type *)
let rec last = function
  | [] -> None
  | [x] -> Some x
  | _ :: t -> last t
;;

let add_one_to_last_element lst =
  let el = last lst in
  match el with
  | None -> failwith "empty list man"
  | Some x -> x + 1
;;

add_one_to_last_element [1;2;3;4] ;;

add_one_to_last_element [] ;;

(* make our own types *)
type operation =
  | Inc
  | Dec
;;

let calculate op x =
  match op with
  | Inc -> x + 1
  | Dec -> x - 1
;;

calculate Inc 5 ;;

type operation' =
  | Inc
  | Dec
  | Mul of int
;;

let calculate' op x =
  match op with
  | Inc -> x + 1
  | Dec -> x - 1
  | Mul y -> x * y
;;

calculate' (Mul 5) 5 ;;


(* SYMBOLIC MANIPULATION *)

type expr =
  | Add of expr * expr   (* Sum of two expressions *)
  | Mul of expr * expr   (* Product of two expressions *)
  | Int of int           (* Integer constant *)
  | Var of string        (* Named variable, like "x" *)
  | Sin of expr          (* Sine *)
  | Cos of expr          (* Cosine *)
;;

let x = Var("x") in
  Add(Sin(Add(Mul(Int 3, x), Int 1)), Mul(Int 2, x))
;;

(* overload operator +: *)
let rec ( +: ) f g =
  match f, g with
  | Int n, Int m        -> Int (n + m)
  | Int 0, f | f, Int 0 -> f
  | f, Add(g, h)        -> f +: g +: h
  | f, g when f > g     -> g +: f
  | f, g                -> Add(f, g)
;;

Int 5 +: Int 10 ;;

(* Note that these functions rotate the abstract syntax tree to ensure that
   sequences of additions and multiplications are left-associative,
   i.e. a*b*c is represented as (a*b)*c and not as a*(b*c). This can then be
   used as an assumption to simplify other functions, such as a pretty printer. *)

let rec ( *: ) f g =
  match f, g with
  | Int n, Int m        -> Int (n * m)
  | Int 0, _ | _, Int 0 -> Int 0
  | Int 1, f | f, Int 1 -> f
  | f, Mul(g, h)        -> f *: g *: h
  | f, g when f > g     -> g *: f
  | f, g                -> Mul(f, g)
;;

let x = Var("x") in
  Sin(Int 3 *: x +: Int 1) +: Int 2 *: x
;;


(* now the differential operator *)
let rec d f x =
  match f with
  | Var y when x=y  -> Int 1
  | Var _ | Int _   -> Int 0
  | Add(f, g)       -> d f x +: d g x
  | Mul(f, g)       -> f *: d g x +: g *: d f x
  | Sin(f)          -> Cos(f) *: d f x
  | Cos(f)          -> Int (-1) *: Sin(f) *: d f x
;;

(* pretty printer setup *)
open Format;;
let rec print_expr ff = function
  | Int n -> fprintf ff "%d" n
  | Var v -> fprintf ff "%s" v
  | Sin(f) -> fprintf ff "sin(%a)" print_expr f
  | Cos(f) -> fprintf ff "cos(%a)" print_expr f
  | Add(f, g) -> fprintf ff "%a +@;<1 2>%a" print_expr f print_expr g
  | Mul(Add _ as f, g) -> fprintf ff "(@[%a@])@;<1 2>%a" print_expr f print_expr g
  | Mul(f, g) -> fprintf ff "%a@;<1 2>%a" print_expr f print_expr g;;
#install_printer print_expr;;

let a = Var "a" and b = Var "b" and c = Var "c" and x = Var "x" ;;
(* ax^2 + bx + xsin(2x) *)
let expr = a*:x*:x +: b*:x +: x*:Sin(Int 2 *: x) ;;

(* Dx(ax^2 + bx + xsin(2x)) = 2ax + b + 2xcos(2x) + sin2x *)
d expr "x" ;;
