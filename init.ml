#use "topfind";;
#thread;;
#camlp4o;;
#require "core.top";;
#require "core.syntax";;

open Core;;
open Core.Std;;

#use "rcg_parser.ml";;
#use "rcg_calc_dis.ml";;
#use "rcg_bhv.ml";;
#use "rcg_ilpdata.ml";;
#use "rcg_predicate.ml";;

let ball_holder_distance = 2.0;;
let long_pass_distance = 10.0;;

let pass_zone_y_1 = -13.0;;
let pass_zone_y_2 =  13.0;;
(*let positive_sec = 40;;*)
let positive_sec = 80;;
(*let rcg_bhv_num = 3;;*)
let rcg_bhv_num = 5;;
