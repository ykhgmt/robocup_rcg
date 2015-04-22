open Pervasives;;

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

let rec cycle_rm sa =
  match sa with
  | [] -> []
  | (Cycle num,agenb) as a:: rest ->
    match rest with
    | [] -> []
    | (Cycle num2,agenb2) :: rest2 when num = num2 ->
      [] @ cycle_rm rest
    | (Cycle num2,agenb2)  :: rest2 ->
      a :: cycle_rm rest
;;

let test_data = cycle_rm(elm_agent rcg_data);;

let i = ref 0;;

let rec get_ball_x_y c =
  match c with
  | [] -> []
  | (Cycle num , rest) :: rest2 ->
    match rest with
    | [] -> failwith "fail"
    | Ball(B_Pos_x x,B_Pos_y y,B_V_x vx, B_V_y vy) :: t ->
      (num,x,y) :: get_ball_x_y rest2
    | _ -> failwith "fail"
;;

let ball_xy = get_ball_x_y test_data;;


let rec get_agent_list c =
  match c with
  | [] -> []
  | (Cycle num , rest) :: rest2 ->
    match rest with
    | [] -> failwith "fail"
    | h :: t ->
      t :: get_agent_list rest2
;;

let agent_xy = get_agent_list test_data;;

let rec calc_distance agent ball_d =
  match agent with
  | [] -> []
  | Agent (Team str , Label l, Pos_x x, Pos_y y, V_x vx, V_y vy) :: rest
    ->
    begin
      match ball_d with
      | (num,h1,h2) as t ->
        ( num , str ^ " " ^ (string_of_float l) ,
          (sqrt((((x -. h1) *. (x -. h1))) +. ((y -. h2) *. (y -. h2)))))
        :: calc_distance rest t
    end
  | _ -> failwith "fail calc_distance"
;;

let rec calc_distance2 agent ball_d =
  match agent with
  | [] -> []
  | head :: rest ->
    begin
      match ball_d with
      | [] -> []
      | head_b :: rest_b ->
        calc_distance head head_b  :: calc_distance2 rest rest_b
    end
;;

let ball_agent_distance = calc_distance2 agent_xy ball_xy;;

let rec minimum l m =
  match l with
  | [] ->
     begin
       match m with
       | (num,a,b) as t -> if b < ball_holder_distance
			   then t else (-1,"None",-1.0)
     end
  | (num,s,f) as a :: r ->
     begin
       match m with (* 同じsec内の一番近いエージェントを選択 *)
       | (num2,s2,f2) ->
          if f < f2
          then minimum r a
	  else minimum r m
     end
;;

let rec ball_holder ba =
  match ba with
  | [] -> []
  | h :: t -> minimum h (0, "Start",10000.0) :: ball_holder t
;;

let bf = ball_holder ball_agent_distance;;

let rec none_rm a =
  match a with
  | [] -> []
  | f :: r ->
    match f with
    | (-1 , "None" , -1.0) -> [] @ none_rm r
    | (num , label , distance) -> [(num , label , distance)] @ none_rm r
;;

let ballh = none_rm bf;;

  (* (*test*)
  let create_number_file filename strnum_tup =
  let outc = Out_channel.create filename in
  List.iter strnum_tup ~f:(fun (n,x,y) ->
			   fprintf outc "%d , %s , %f\n" n x y);
  Out_channel.close outc;;

  create_number_file "ballh.csv" ballh;;*)
