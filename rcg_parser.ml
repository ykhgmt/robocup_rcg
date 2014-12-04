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

let str = In_channel.input_all(In_channel.create("helios-test_prot.txt"));;

let show = "(show 1 ((b) 0 0 0 0))";;

type agent =
  Team of string
| Label of int
| Pos_x of int
| Pos_y of int
| V_x of int
| V_y of int
;;

type ball =
| B_Pos_x of int
| B_Pos_y of int
| B_V_x of int
| B_V_y of int
;;

type ball_inf =
  Record_b of  (ball * ball * ball * ball)
| Records_b of  (ball * ball * ball * ball) list
;;

type agent_inf =
  Record of  (agent * agent * agent * agent * agent * agent)
| Records of  (agent * agent * agent * agent * agent * agent) list
;;

(*
type cycle =
  Show of int
| Agent of agent_inf
| Ball of ball;;
*)

type rcg =
  Ball_inf of ball_inf
| Record_b of  (ball * ball * ball * ball * ball * ball)
| Records_b of  (ball * ball * ball * ball * ball * ball) list
| Record of  (agent * agent * agent * agent * agent * agent)
| Records of  (agent * agent * agent * agent * agent * agent) list
| Cycle of (int * ball_inf * agent_inf)
| Cycles of (int * ball_inf * agent_inf) list
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
  | Record (a1,a2,a3,a4,a5,a6) -> (a1,a2,a3,a4,a5,a6)
  | _ -> failwith "fail get_records"
;;

let rec set_records_b s =
  match s with
  | (a1,a2,a3,a4) -> (B_Pos_x a1, B_Pos_y a2, B_V_x a3, B_V_y a4)
  | _ -> failwith "fail get_records"
;;

let rec parse2 s =
  match s with
  | List(Atom "show" :: Atom cycle
         :: List(List[Atom "b"]
                 :: Atom x :: Atom y ::Atom vx :: Atom vy ::[])
         :: [])
    -> cycle
  (*| List(rs) ->
    Records(List.map ~f:(fun r ->
    get_records (parse2 r) ) rs)
  *)
  | _ -> failwith "fail"
;;

parse2 (lexing show);;

let rec parse s =
  match s with
(*  | List(Atom "show" :: Atom cycle
         :: List(List[Atom "b"]
                 :: Atom x :: Atom y ::Atom vx :: Atom vy ::[])
         :: [])
    -> cycle
*)
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
    Record (
      Team (team) ,
      Label (int_of_string num) ,
      Pos_x (int_of_string x) ,
      Pos_y (int_of_string y) ,
      V_x (int_of_string vx) ,
      V_y (int_of_string vy)
    )
  | List(rs) ->
(*    Cycle(1,
          Record_b (set_records_b(
            parse rs
(*1,2,3,4*))
          ),
*)
          Records(List.map ~f:(fun r -> get_records (parse r) ) rs)
(*    ) *)
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
lexing str;;
(*parse_lex str;;*)
