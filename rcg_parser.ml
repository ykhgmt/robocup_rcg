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

let str = In_channel.input_all(In_channel.create("helios-test_prot4.txt"));;

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
(*
type ball_inf =
  Record_b of  (ball * ball * ball * ball)
;;

type agent_inf =
  Record of  (agent * agent * agent * agent * agent * agent)
| Records of  (agent * agent * agent * agent * agent * agent) list
;;

type sec =
  Show of int
;;
*)
type rcg =
| None
| Sec of int
| Ball of  (ball * ball * ball * ball)
| Agent of (agent * agent * agent * agent * agent * agent)
(*
  | Cycle of (sec * ball_inf * agent_inf )
  | Cycles of (sec * ball_inf * agent_inf ) list
*)
;;

type elm =
| Record of rcg
| Records of rcg list;;

(* string -> t *)
let lexing s = scan_sexp (Lexing.from_string s);;


(*let matching a =
  match a with
    List ( List ( Atom "l" :: rest) :: (Atom a) ::  [List(Atom "d" :: rest2)])  -> a
  | _ -> failwith "NO"
;;

matching (lexing "((l a) a (d e))");;
*)

let rec get_records s =
  match s with
  | Record (a) -> a
  (*| Record (Sec a1) -> (Sec a1)
  | Record (Ball(a1,a2,a3,a4)) -> (a1,a2,a3,a4)
  | Record (Agent(a1,a2,a3,a4,a5,a6)) -> (a1,a2,a3,a4,a5,a6)
  *)
  | _ -> failwith "fail get_records"
;;

let rec parse_agent s =
  match s with
  | Atom "show" -> Record (None)
  | Atom a -> Record (Sec (int_of_string a))
  | List(List[Atom "b"] :: Atom x :: Atom y ::Atom vx :: Atom vy ::[])
    -> Record(Ball(B_Pos_x(Float.of_string x) ,
                   B_Pos_y(Float.of_string y) ,
                   B_V_x(Float.of_string vx) ,
                   B_V_y(Float.of_string vy)
    ))
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
    Record(Agent(
      Team (team) ,
      Label (Float.of_string num) ,
      Pos_x (Float.of_string x) ,
      Pos_y (Float.of_string y) ,
      V_x (Float.of_string vx) ,
      V_y (Float.of_string vy)
    ))
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
    Record(Agent(
      Team (team) ,
      Label (Float.of_string num) ,
      Pos_x (Float.of_string x) ,
      Pos_y (Float.of_string y) ,
      V_x (Float.of_string vx) ,
      V_y (Float.of_string vy)
    ))
  | List(rs) ->
    Records(List.map ~f:(fun r -> get_records (parse_agent r) ) rs)
;;

let rec parse_t_list s =
  match s with
  | List(rs) -> rs
  | _ -> failwith "fail parse_agent"
;;


let parse_lex s =
  let lex = Lexing.from_string s
  in parse_agent (scan_sexp lex)
;;

(* test *)
(*matching (lexing parsep);;*)

#trace get_records;;
#trace parse_agent;;

(*
parse_lex s_parse1;;
parse_lex s_parse2;;
*)

parse_lex str;;

(*
lexing str;;
*)
(* parse_lex str;; *)
