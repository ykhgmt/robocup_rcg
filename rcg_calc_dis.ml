type dis =
| Dis of float
;;

type agent_dis =
| A_teaminf of string
| Distance of dis
;;

type agent_dist =
| Agent_d of ( agent_dis * agent_dis)

type agent_distance =
| Record_dis of (sec * agent_dist list)
| Records_dis of (sec * agent_dist list) list
;;

let rcg_matc op =
  match op with
  | Rcg_m(a) -> []
  | Rcg_p(a) -> []
  | Rcg_ab(a) -> [a]
;;

let rec rcg_matcrec =
  function
  | [] ->[]
  | h :: t -> rcg_matc h @ rcg_matcrec t
;;

(* elm -> agent_distance *)

let elm_agent e =
  match e with
  | Records(a) ->  (rcg_matcrec a)
  | _ -> failwith "fail calc_dis"
;;

let test_data = elm_agent data;;

let ballx , bally =
  let get_ball_x_y c =
    match c with
    | [] -> failwith "fail"
    | (Cycle num , rest) :: rest2 ->
      match rest with
      | [] -> failwith "fail"
      | Ball(B_Pos_x x,B_Pos_y y,B_V_x vx, B_V_y vy) :: t ->
        (x,y)
      | _ -> failwith "fail"
  in get_ball_x_y test_data
;;

let test2 =
  let agent_inf_dd c =
    match c with
    | [] -> []
    | (Cycle num , rest) :: rest2 ->
      match rest with
      | [] -> failwith "fail"
      | h :: t -> t
  in agent_inf_dd test_data
;;

let test =
  function
  | Agent (Team str , Label l, Pos_x x, Pos_y y, V_x vx, V_y vy)
    -> sqrt(((x -. ballx) ** 2.0) +. (y -. bally) ** 2.0)
;;

List.map ~f:test test2;;


(*
let ttt c =
  match c with
  | [] -> failwith "fail"
  | (Cycle num , rest) :: rest2 ->
    match rest with
    | [] -> failwith "fail"
    | h :: Agent(t) -> Agent(t)
in
let get_ballx_y a =
  match a with
  | [] -> failwith "fail"
  | Agent(Team a1,Label a2,Pos_x a3,Pos_y a4,a5,a6) :: rest2
    -> a1
in get_ballx_y ttt
;;

get_ballx_y (elm_agent data);;
*)

(*
let calc_dis4 a =

;;

let calc_dis3 =
  function
  | Ball(B_Pos_x a , B_Pos_y b , c , d) ->
  | Agent(Team str , Label a , Pos_x a2 , Pos_y a3 ,
          V_x  a4 , V_y a5)
    ->
;;
*)
