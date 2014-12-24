drib_pass;;
test_data;;

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
    | h :: r as hr ->
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
      in pos_pre_trans2 hr :: pos_pre_trans rest r
;;

let test = pos_pre_trans pass test_data;;

List.length test;;
