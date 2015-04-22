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

let pos_selection_pass_p = pos_selec pass positive_length;;

let rec pos_list_length r =
  match r with
  | [] -> []
  | h :: r when ((List.length h) > rcg_bhv_num)
    -> h :: pos_list_length r
  | h :: r -> pos_list_length r
;;

(*let pos_selection_pass = pos_list_length pos_selection_pass_p;;*)
let pos_selection_pass = pos_list_length pos_selection_pass_p;;


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

let has_a_predicate = has_a_pre good_ac_pass_hasac;;

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

let long_predicate = long_pre good_ac_pass_hasac long_pass;;

let p_or_a pa h =
    if pa = "p" then ("enemy_p(" ^ h ^ ").")
    else if  pa = "a" then ("enemy_a(" ^ h ^ ").")
    else failwith "fail p_or_a"
;;

let rec enemy_pre_gen pa s gaph =
  match gaph with
  | [] -> []
  | (asec,t) :: rest ->
    let rec enemy_pre_gen2 pa s t =
      match t with
      | [] -> []
      | h :: r when (List.nth(String.split h '_') 2)
          = (Some (string_of_int s))
            -> (p_or_a pa h) :: enemy_pre_gen2 pa s r
      | h :: r
        -> enemy_pre_gen2 pa s r
    in enemy_pre_gen2 pa s t @ enemy_pre_gen pa s rest
;;


let rec enemy_pre gaph pen =
  match pen with
  | [] -> []
  | (s,(e1,e2)) :: rest when e1 != [] && e2 != [] ->
    enemy_pre_gen "p" s gaph @ enemy_pre_gen "a" s gaph
    @  enemy_pre gaph rest
  | (s,(e1,e2)) :: rest when e1 != [] ->
    enemy_pre_gen "p" s gaph @ enemy_pre gaph rest
  | (s,(e1,e2)) :: rest when e2 != [] ->
    enemy_pre_gen "a" s gaph @ enemy_pre gaph rest
  | (s,(e1,e2)) :: rest ->
    enemy_pre gaph rest
;;

let enemy_predicate = enemy_pre good_ac_pass_hasac pass_enemy_near;;

let rec order_pre gaph =
  match gaph with
  | [] -> []
  | (s,pr) :: rest ->
    let rec order_pre2 pr =
      match pr with
      | [] -> []
      | h :: h2 :: r ->
        ("order(" ^ h ^ "," ^ h2 ^ ").") :: order_pre2 (h2::r)
      | h :: r -> []
    in order_pre2 pr @ order_pre rest
;;

let order_predicate = order_pre good_ac_pass_hasac;;

let rec zone_pre zone gaph =
  match zone with
  | [] -> []
  | (s,(z1,z2)) :: rest ->
    let rec zone_pre2 s gaph =
      match gaph with
      | [] -> []
      | (sec,pre) :: rest ->
        let rec zone_pre3 s pre =
          match pre with
          | [] -> []
          | h :: r when (List.nth(String.split h '_') 2)
              = (Some (string_of_int s)) ->
            ("zone(" ^ h ^ "," ^ (string_of_int z1) ^ "," ^ (string_of_int z2) ^ ").")
            :: zone_pre3 s r
          | h :: r -> zone_pre3 s r
        in zone_pre3 s pre @ zone_pre2 s rest
    in zone_pre2 s gaph @ zone_pre rest gaph
;;

let zone_predicate = zone_pre pass_zone_list good_ac_pass_hasac;;

let create_number_file filename strnum_tup =
  let outc = Out_channel.create filename in
  List.iter strnum_tup ~f:(fun p -> fprintf outc "%s \n" p);
  Out_channel.close outc
;;

  (*let filenw = ["(*"^filen^"*)"];;*)

create_number_file ("progol_data_" ^ filen ^ ".pl")
  (filenw @ good_predicate @ ac_predicate @ pass_predicate
   @ has_a_predicate @ zone_predicate @ order_predicate
  @ enemy_predicate @ long_predicate);;
