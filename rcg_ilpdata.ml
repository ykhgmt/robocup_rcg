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

let rec pos_data salist =
  match salist with
  | [] -> []
  | h :: r ->
    match h with
    | (Cycle s , ablist) ->
      match ablist with
      | [] -> []
      | Ball(B_Pos_x x,B_Pos_y y,B_V_x vx,B_V_y vy) :: rest ->
        (s,x) :: pos_data r
      | Agent(a) :: rest -> [] @ pos_data r
;;

let pos = pos_data test_data

let rec pos_data_r =
  function
  | [] -> []
  | (s,b) :: rest ->
    let rec pos_data2 s b rest =
      match rest with
      | [] -> []
      | q::w::e::r::t::(s2,b2) :: rest2 ->
        if b > b2
        then (s,s2) :: pos_data_r rest2
        else pos_data2 s b2 rest2
    in pos_data2 s b rest
;;

pos_data_r pos;;

let rec pos_data_sele d =
  match d with
  | [] -> []
  | (b1,b2) :: rest when b1+6 = b2 -> [] @ pos_data_sele rest
  | (b1,b2) as a :: rest -> a :: pos_data_sele rest
;;

let pos_sec = pos_data_sele (pos_data_r pos);;

drib_pass;;
pos_sec;;

let rec pos_dpass dp pos =
  match pos with
  | [] -> []
  | (s1,s2) :: rest ->
    let rec pos_dpass2 s1 s2 dp =
      match dp with
      | [] -> []
      | (Record_dp(D(a))) as rs :: rest2
          when (a.sec_d >= s1) && a.sec_d <= s2
            -> [rs] @ pos_dpass2 s1 s2 rest2
      | (Record_dp(P(a))) as rs :: rest2
          when a.sec_p >= s1 && a.sec_p <= s2
            -> [rs] @ pos_dpass2 s1 s2 rest2
      | (Record_dp(D(a))) :: rest2
            -> pos_dpass2 s1 s2 rest2
      | (Record_dp(P(a))) :: rest2
            -> pos_dpass2 s1 s2 rest2
    in pos_dpass2 s1 s2 dp :: pos_dpass dp rest
;;

pos_dpass drib_pass pos_sec;;

let rec pos_leng p leng =
  match p with
  | [] -> []
  | (a,b) as c :: rest when ((b - a) > leng) -> c :: aa rest
  | (a,b) :: rest -> aa rest
;;

pos_leng pos_sec 80;;
