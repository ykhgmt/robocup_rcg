let rec drib_rm dp =
  match dp with
  | [] -> []
  | Record_dp(D(rs)) :: rest -> [] @ drib_rm rest
  | (Record_dp(P(rs))) as a :: rest -> [a] @ drib_rm rest
  | _ -> failwith "fail drib_rm"
;;

let pass = drib_rm drib_pass;;

let rec pos_pre_trans dp ab =
  match dp with
  | [] -> []
  | Record_dp(D(rs)) :: rest -> failwith "None drrible"
  | Record_dp(P(rs)) :: rest ->
    match ab with
    | [] -> []
    | h :: r ->
      let rec pos_pre_trans2 ab2 =
        match ab2 with
        | [] -> failwith "None2"
        | (Cycle num , agenb) :: rest2 when num = rs.sec_p ->
          let rec pos_pre_trans3 agenb ll =
            match agenb with
            | [] -> failwith "None3"
            | Ball(rs_b) :: rest3 -> pos_pre_trans3 rest3 ll
            | Agent(Team t,Label l,Pos_x px,Pos_y py,vx,vy) :: rest3
                when (t ^ " " ^ (string_of_float l)) = ll
                -> px,py
            | Agent(t,Label l,Pos_x px,Pos_y py,vx,vy) :: rest3
              -> pos_pre_trans3 rest3 ll
          in ((fun ((x,y),(x2,y2)) ->
            sqrt(((x -. x2) ** 2.0) +. ((y -. y2) ** 2.0)))
                 (pos_pre_trans3 agenb rs.age1,
                  pos_pre_trans3 agenb rs.age2))
        | (Cycle num , agenb) :: rest2 -> pos_pre_trans2 rest2
      in pos_pre_trans2 ab :: pos_pre_trans rest r
;;

let pass_d = pos_pre_trans pass test_data;;

let rec compe a b =
  match a with
  | [] -> []
  | Record_dp(P(hs)) :: r ->
    begin
      match b with
      | [] -> []
      | h2 :: r2 ->
        (hs.sec_p,h2) :: compe r r2
    end
  | _ -> failwith "fail compe"
;;

let compe_pass_pass_d = compe pass pass_d;;


let rec pos_pass_enemy agenb (x,y) =
    match agenb with
    | [] -> []
    | Ball(b) :: rest -> pos_pass_enemy rest (x,y)
    | Agent(Team t,Label l,Pos_x px,Pos_y py,vx,vy) :: rest
        when t = "r" && (px > x
              && (((sqrt((x -. px) ** 2.0)
                    +. ((y -. py) ** 2.0))) < 10.0))
          -> (t ^ (string_of_float l)) :: pos_pass_enemy rest (x,y)
    | Agent(t,Label l,Pos_x px,Pos_y py,vx,vy) :: rest
      -> pos_pass_enemy rest (x,y)
;;

let rec pd_pos dp ab =
  match dp with
  | [] -> []
  | Record_dp(D(rs)) :: rest -> failwith "None drrible"
  | Record_dp(P(rs)) :: rest ->
    match ab with
    | [] -> []
    | h :: r ->
      let rec pos_pre_trans2 ab2 =
        match ab2 with
        | [] -> failwith "None2"
        | (Cycle num , agenb) :: rest2 when num = rs.sec_p ->
          let rec pos_pre_trans3 agenb ll =
            match agenb with
            | [] -> []
            | Ball(rs_b) :: rest3 -> pos_pre_trans3 rest3 ll
            | Agent(Team t,Label l,Pos_x px,Pos_y py,vx,vy) :: rest3
                when (t ^ " " ^ (string_of_float l)) = ll  ->
              pos_pass_enemy agenb (px,py)
            | Agent(t,Label l,Pos_x px,Pos_y py,vx,vy) :: rest3
              -> pos_pre_trans3 rest3 ll
          in (pos_pre_trans3 agenb rs.age1,pos_pre_trans3 agenb rs.age2)
        | (Cycle num , agenb) :: rest2 -> pos_pre_trans2 rest2
      in pos_pre_trans2 ab :: pd_pos rest r
;;

compe pass (pd_pos pass test_data);;


let rec pass_zone y =
  if y < -13.0
  then 1
  else if (y >= -13.0) && (y <= 13.0)
  then 2
  else 3
;;

let rec pd_zone dp ab =
  match dp with
  | [] -> []
  | Record_dp(D(rs)) :: rest -> failwith "None drrible"
  | Record_dp(P(rs)) :: rest ->
    match ab with
    | [] -> []
    | h :: r ->
      let rec pd_zone2 ab2 =
        match ab2 with
        | [] -> failwith "fail pd_pos2"
        | (Cycle num , agenb) :: rest2 when num = rs.sec_p ->
          let rec pd_zone3 agenb ll =
            match agenb with
            | [] -> failwith "fail pd_pos3"
            | Ball(rs_b) :: rest3 -> pd_zone3 rest3 ll
            | Agent(Team t,Label l,Pos_x px,Pos_y py,vx,vy) :: rest3
                when (t ^ " " ^ (string_of_float l)) = ll  ->
              pass_zone py
            | Agent(t,Label l,Pos_x px,Pos_y py,vx,vy) :: rest3
              -> pd_zone3 rest3 ll
          in (pd_zone3 agenb rs.age1,pd_zone3 agenb rs.age2)
        | (Cycle num , agenb) :: rest2 -> pd_zone2 rest2
      in pd_zone2 ab :: pd_zone rest r
;;

compe pass (pd_zone pass test_data);;

(*
  test_data;;
  drib_pass;;
*)
(*
let rec positive_data s_ab =
  match s_ab with
  | [] -> []
  | (c,ab) :: rest
      ->
*)
