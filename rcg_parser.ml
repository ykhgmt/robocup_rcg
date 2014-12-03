(* open Sexp;; *)
open Sexplib.Sexp;;
#require "sexplib";;

(*
module Sexp : sig
  type t =
  | Atom of string
  | List of t list
end
*)

let s_parse1 =
"((l 1) 0 0x9 -49 0 0 0 2.327 0 (v h 180) (s 8000 1 1 130600) (c 0 0 906 0 1 907 1 0 0 0 0))";;

let s_parse1 =
"(((l 1) 0 0x9 -49 0 0 0 2.327 0 (v h 180) (s 8000 1 1 130600) (c 0 0 906 0 1 907 1 0 0 0 0)))";;

let s_parse2 =
  "(
((l 1) 0 0x9 -49 0 0 0 2.327 0 (v h 180) (s 8000 1 1 130600) (c 0 0 906 0 1 907 1 0 0 0 0))
((l 2) 3 0x1 -25 -5 0 0 78.518 0 (v h 180) (s 8000 0.901147 1 130600) (c 0 0 887 0 1 888 1 0 0 0 0))
)";;

type agent =
  Label of int
| Pos_x of int
| Pos_y of int
| V_x of int
| V_y of int
;;

type agent_inf =
  Record of  (agent * agent * agent * agent * agent)
| Records of  (agent * agent * agent * agent * agent) list
;;

(* string -> t*)
let lexing s = scan_sexp (Lexing.from_string s);;

let matching a =
  match a with
    List ( List ( Atom "l" :: rest) :: (Atom a) ::  [List(Atom "d" :: rest2)])  -> a
  | _ -> failwith "NO"
;;

let rec get_records s =
  match s with
  | Record (a1 , a2 , a3 , a4 , a5 )-> (a1,a2,a3,a4,a5)
  | _ -> failwith "fail get_records"
;;

let rec parse s =
  match s with
  | (List
       (List( Atom a :: [Atom num])
        :: b1 :: b2
        :: (Atom x) :: (Atom y) :: (Atom vx) :: (Atom vy)
        :: b3 :: b4
        :: List (Atom "v" :: rest1)
        :: List (Atom "s" :: rest2)
        :: List (Atom "c" :: rest3)
        :: []
       ))
    ->
    Record (
      Label (int_of_string num) ,
      Pos_x (int_of_string x) ,
      Pos_y (int_of_string y) ,
      V_x (int_of_string vx) ,
      V_y (int_of_string vy)
    )
  | List(rs) ->
    Records(List.map (fun r ->
      get_records (parse r) ) rs)
  | _ -> failwith "fail"
;;

let parse_lex s =
  let lex = Lexing.from_string s
  in parse (scan_sexp lex)
;;

(* test *)
(*matching (lexing parsep);;*)
matching (lexing "((l a) a (d e))");;
parse_lex s_parse1;;
parse_lex s_parse2;;
