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

let parsep =
"((l 1) 0 0x9 -49 0 0 0 2.327 0 (v h 180) (s 8000 1 1 130600) (c 0 0 906 0 1 907 1 0 0 0 0))";;

type pos_x_y = X of int | Y of int;;

type agent_l = Label of int;;

type rcg =
  Pos of (pos_x_y * pos_x_y)
| Label of int
| Etc of string
;;

(* string -> t*)
let lexing s = scan_sexp (Lexing.from_string s);;

let parse_atom a =
  function

(* string -> rcg *)
let parse s =
  let rec parse s =
    match s with
      Atom a -> Atom (a)
    | List [] -> List []
    | List [Atom a] -> List [Atom a]
    | List (Atom "l" :: :: rest) -> List (Atom a :: List.map parse rest)
    | List (Atom a :: rest) -> List (Atom a :: List.map parse rest)
    | List (List a :: rest)
      -> List (List (List .map parse a) :: List.map parse rest)
  in
  let lex = Lexing.from_string s in
  parse (scan_sexp lex)
;;

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
