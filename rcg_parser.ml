(* open Sexp;; *)
(*open Core;;
open Core.Std;;
*)
open Sexplib.Sexp;;
#require "sexplib";;

(*
module Sexp : sig
  type t =
  | Atom of string
  | List of t list
end
*)

let str = In_channel.input_all(In_channel.create("helios-test_prot3.txt"));;

(*
let str = "(show 1 ((b) 0 0 0 0) ((l 1) 0 0x9 -49 0 0 0 2.327 0 (v h 180) (s 8000 1 1 130600) (c 0 0 906 0 1 907 1 0 0 0 0)))"
*)
let show = "(show 1 ((b) 0 0 0 0))";;

type agent =
  Team of string
| Label of float
| Pos_x of float
| Pos_y of float
| V_x of float
| V_y of float
;;

type ball =
| B_Pos_x of float
| B_Pos_y of float
| B_V_x of float
| B_V_y of float
;;

type agent_ball =
| Ball of  (ball * ball * ball * ball)
| Agent of (agent * agent * agent * agent * agent * agent)
;;

type ab_elm =
| Record_ab of agent_ball
| Records_ab of agent_ball list
;;

type sec =
| Cycle of int;;

type rcg =
| Rcg of (sec * agent_ball list)
;;

type elm =
| Record of rcg
| Records of rcg list;;

(* string -> t *)
let lexing s = scan_sexp (Lexing.from_string s);;

let rec get_records s =
  match s with
  | Record (a) -> a
  | _ -> failwith "fail get_records"
;;

let rec parse_agent r =
  match r with
  | List (List ([Atom "b"]) :: Atom x :: Atom y :: Atom vx :: Atom vy ::[] )
    -> Ball(B_Pos_x (Float.of_string x) ,
            B_Pos_y (Float.of_string y) ,
            B_V_x (Float.of_string vx) ,
            B_V_y (Float.of_string vy))
  | (List
       (List( Atom team :: [Atom num])
        :: b1 :: b2
        :: (Atom x) :: (Atom y) :: (Atom vx) :: (Atom vy)
        :: b3 :: b4
        :: List (Atom "v" :: rest1)
        :: List (Atom "s" :: rest2)
        :: List (Atom "c" :: rest3)
        :: []
       ))
    ->
    Agent(
      Team (team) ,
      Label (Float.of_string num) ,
      Pos_x (Float.of_string x) ,
      Pos_y (Float.of_string y) ,
      V_x (Float.of_string vx) ,
      V_y (Float.of_string vy)
    )
  | (List
       (List( Atom team :: [Atom num])
        :: b1 :: b2
        :: (Atom x) :: (Atom y) :: (Atom vx) :: (Atom vy)
        :: b3 :: b4
        :: List (Atom "v" :: rest1)
        :: List (Atom "s" :: rest2)
        :: rest4
        :: List (Atom "c" :: rest3)
        :: []
       ))
    ->
    (Agent(
      Team (team) ,
      Label (Float.of_string num) ,
      Pos_x (Float.of_string x) ,
      Pos_y (Float.of_string y) ,
      V_x (Float.of_string vx) ,
      V_y (Float.of_string vy)
     ))
;;

let rec parse s =
  match s with
  | List(Atom "show" :: Atom sec :: rest)
    -> Record(Rcg(Cycle (int_of_string sec) , List.map ~f:parse_agent rest))
  | List(rs) ->
    Records(List.map ~f:(fun r -> get_records (parse r) ) rs)
;;

let rec parse_t_list s =
  match s with
  | List(rs) -> rs
  | _ -> failwith "fail parse_agent"
;;


let parse_lex s =
  let lex = Lexing.from_string s
  in parse (scan_sexp lex)
;;

(* test *)
(*matching (lexing parsep);;*)
(*
#trace get_records;;
#trace parse_agent;;
*)
(*
parse_lex s_parse1;;
parse_lex s_parse2;;
*)

parse_lex str;;

(*
lexing str;;
*)
(* parse_lex str;; *)
