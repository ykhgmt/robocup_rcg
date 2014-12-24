type drrible = {
  sec_d : int;
  age : string;
};;

type pass = {
  sec_p : int;
  age1 : string;
  age2 : string;
};;

type dp =
| D of drrible
| P of pass;;

type record_dp =
  Record_dp of (dp);;

let rec d_check b =
  match b with
  | [] -> []
  | (i,s,d) :: rest ->
    match rest with
    | [] -> []
    | (i2,s2,d2) :: rest2 ->
      if s = s2 then
        d_check rest
      else
        Record_dp ( D ({sec_d = i; age = s}))
        :: Record_dp (P ({sec_p = i2; age1 = s; age2 = s2}))
        :: d_check rest
;;

let dplist = d_check ballh;;


let rec dp_match_l b =
  match b with
  | [] -> []
  | Record_dp(a) :: rest->
    match a with
    | D(rs) ->
      if (String.get rs.age 0) = 'l'
      then [Record_dp(D(rs))] @ dp_match rest
      else [] @ dp_match rest
    | P(rs) ->
      if (((String.get rs.age1 0) = 'l') && ((String.get rs.age2 0) = 'l'))
      then [Record_dp(P(rs))] @ dp_match rest
      else [] @ dp_match rest
;;

let drib_pass = dp_match dplist;;
