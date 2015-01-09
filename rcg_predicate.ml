(* %mode
:- modeh(1,good(+ac))?
:- modeb( *,has_action(+ac,-pass))?     名前で判断
:- modeb(1,long(+pass))?                compe_pass_pass_d
:- modeb(1,zone(+pass,#int,#int))?      pass_zone_list
:- modeb(1,enemy_p(+pass))?             pass_enemy_near
:- modeb(1,enemy_a(+pass))?             pass_enemy_near
:- modeb(1,order(+pass,+pass))?         cycle数で判断

%positive example

good(a1).
%good(a2).
%good(a3).
%good(a4).

%%type

ac(a1).
pass(p1). pass [pass(p_A_Cycle)]
pass(p2).

%background knowledge

has_action(a1,p1).
has_action(a1,p2).
enemy_p(p1).
long(p2).
order(p1,p2).
%zone(p2,1,2).

pass -> p_A_Cycle,p_1_1...
pN,... -> has_action(a1,pN),...
pN,... -> long(pN),enemy_p(pN)...

posデータだけに選別
*)

let rec pos_selec pass pos =
  match pos with
  | [] -> []
  | (b,a) as sec :: rest ->
    let rec pos_selec2 ba pass =
      match pass with
      | [] -> []
      | Record_dp(P(rs)) as pa :: rest
          when rs.sec_p >= b && rs.sec_p <= a ->
        pa :: pos_selec2 sec rest
      | Record_dp(P(rs)) :: rest
        -> pos_selec2 sec rest
      | _ -> failwith "none drrible"
    in pos_selec2 sec pass :: pos_selec pass rest
;;

let pos_selection_pass = pos_selec pass positive_length;;

let type_pass_r p =
  let rec type_pass p c =
  match p with
  | [] -> []
  | reco  :: rest ->
    let rec type_pass2 reco c =
      match reco with
      | [] -> []
      | Record_dp(P(rs)) :: rest ->
        ("p_" ^ (string_of_int c) ^ "_" ^ (string_of_int(rs.sec_p)))
        :: type_pass2 rest c
      | _ -> failwith "fail Drrible none"
    in (c,type_pass2 reco c) :: type_pass rest (c+1)
  in type_pass p 0
;;

let good_ac_pass_hasac  = type_pass_r pos_selection_pass;;

let rec good_pre gaph =
  match gaph with
  | [] -> []
  | (s,r) :: rest ->
    ("good(a" ^ (string_of_int s) ^ ").") :: good_pre rest
;;

let good_predicate = good_pre good_ac_pass_hasac;;

let rec ac_pre gaph =
  match gaph with
  | [] -> []
  | (s,r) :: rest ->
    ("ac(a" ^ (string_of_int s) ^ ").") :: ac_pre rest
;;

let ac_predicate = ac_pre good_ac_pass_hasac;;

let rec pass_pre gaph =
  match gaph with
  | [] -> []
  | (s,r) :: rest ->
    let rec pass_pre2 r =
      match r with
      | [] -> []
      | p :: rest ->
        ("pass(" ^ p ^ ").") :: pass_pre2 rest
    in (pass_pre2 r) @ pass_pre rest
;;

let pass_predicate = pass_pre good_ac_pass_hasac;;

let rec has_a_pre gaph =
  match gaph with
  | [] -> []
  | (s,r) :: rest ->
    let rec has_a_pre2 s r =
      match r with
      | [] -> []
      | p :: rest ->
        ("has_action(a" ^ (string_of_int s) ^ "," ^ p ^ ").")
        :: has_a_pre2 s rest
    in (has_a_pre2 s r) @ has_a_pre rest
;;

let has_a_pre = has_a_pre good_ac_pass_hasac;;

let rec long_pre gaph lp=
  match lp with
  | [] -> []
  | (s,d) :: rest_lp ->
    let rec long_pre2 gaph s =
      match gaph with
      | [] -> []
      | (sec,reco) :: rest_gaph ->
        let rec long_pre3 reco s =
          match reco with
          | [] -> []
          | h :: t when (List.nth(String.split h '_') 2)
              = (Some (string_of_int s))
                ->  ("long(" ^ h ^ ").") :: long_pre3 t s
          | h :: t -> long_pre3 t s
        in long_pre3 reco s @ long_pre2 rest_gaph s
    in long_pre2 gaph s @ long_pre gaph rest_lp
;;

pos_selection_pass;;
long_pass;;

let long_predicate = long_pre good_ac_pass_hasac long_pass;;

(*
let enemy_pre gaph=
;;
let enemy_predicate = ;;
*)
