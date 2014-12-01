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

let s_parse2 =
  "(((l 1) 0 0x9 -49 0 0 0 2.327 0 (v h 180) (s 8000 1 1 130600) (c 0 0 906 0 1 907 1 0 0 0 0))
((l 2) 3 0x1 -25 -5 0 0 78.518 0 (v h 180) (s 8000 0.901147 1 130600) (c 0 0 887 0 1 888 1 0 0 0 0)))";;

type agent =
 Label of int
| Pos_x of int
| Pos_y of int
| V_x of int
| V_y of int
;;

type agent_inf =
  Record of  (agent * agent * agent * agent * agent)
;;

(* string -> t*)
let lexing s = scan_sexp (Lexing.from_string s);;

let matching a =
  match a with
    List ( List ( Atom "l" :: rest) :: (Atom a) ::  [List(Atom "d" :: rest2)])  -> a
  | _ -> failwith "NO"
;;

(* rest4 == []  *)
let rec parse s =
  match s with
  List
      (List
         (List( Atom a :: [Atom num])
          :: b1 :: b2
          :: (Atom x) :: (Atom y) :: (Atom vx) :: (Atom vy)
          :: b3 :: b4
          :: List (Atom "v" :: rest1)
          :: List (Atom "s" :: rest2)
          :: List (Atom "c" :: rest3)
          :: rest4
         ) :: rest5)
    -> (*rest5*)
      Record (Label (int_of_string num) ,
              Pos_x (int_of_string x) ,
              Pos_y (int_of_string y) ,
              V_x (int_of_string vx) ,
              V_y (int_of_string vy))
      ::
  | _ -> failwith "NO"
;;

let parse_lex s =
  let lex = Lexing.from_string s
  in parse (scan_sexp lex)
;;

(* test *)
(*matching (lexing parsep);;*)
matching (lexing "((l a) a  (d e))");;
parse_lex s_parse2;;

(* string -> rcg *)
(*
let parse s =
  let rec parse s =
    match s with
      Atom a -> Atom (a)
    | List [] -> List []
    | List [Atom a] -> List [Atom a]
  (*  | List (Atom "l" :: :: rest) -> List (Atom a :: List.map parse rest) *)
    | List (Atom a :: rest) -> List (Atom a :: List.map parse rest)
    | List (List a :: rest)
      -> List (List (List .map parse a) :: List.map parse rest)
  in
  let lex = Lexing.from_string s in
  parse (scan_sexp lex)
;;
*)
(*
let parse s =
  let parse s =
    match s with
      List (List (Atom a :: [Atom b]) :: rest)
        -> (* Record(Label 1 , int_of_string b, int_of_string b) *)
          ()
    | _ -> failwith "Parse fail"
  in
  let lex = Lexing.from_string s in
  parse (scan_sexp lex)
;;
*)

(* string -> rcg *)
(*
let parse s =
  let rec parse s =
    match s with
    | List (Atom "l"::conds) ->
      Conds (List.map (fun x -> cond_parse x) conds)
    | List (Atom "action"::Atom act::Atom pr::[Atom v]) ->
      let price = to_price pr v in
      let act = to_action act price in
      Action act
    | List (conds::[action]) ->
      let conds = get_conds (parse conds) in
      let action = get_action (parse action) in
      Record (conds, action)
    | List (rs) ->
      Records (List.map (fun r -> get_record (parse r)) rs)
    | _ -> failwith "invalid syntax"
  in
  let lex = Lexing.from_string s in
  parse (scan_sexp lex);;
*)

(*
let parse s =
  let rec parse s =
    match s with
    | List (Atom "l"::conds) ->
      Conds (List.map (fun x -> cond_parse x) conds)
    | List (Atom "action"::Atom act::Atom pr::[Atom v]) ->
      let price = to_price pr v in
      let act = to_action act price in
      Action act
    | List (conds::[action]) ->
      let conds = get_conds (parse conds) in
      let action = get_action (parse action) in
      Record (conds, action)
    | List (rs) ->
      Records (List.map (fun r -> get_record (parse r)) rs)
    | _ -> failwith "invalid syntax"
  in
  let lex = Lexing.from_string s in
  parse (scan_sexp lex);;
*)

(*
let parsed = parse sample;;
*)
