# robocup_rcg
parsing robocup rcg and creating proogl data.

version
OCaml  4.01.0
Core 111.28.01

・使用方法
1.分析対象ファイルをrcg_parser.mlのfilenに記述する．
2.rcgファイルの((playmode 1 kick_off_l)より上を削除
  末尾のmagを削除
  全体を()で覆う
3.utopで #use "init.ml"を実行

・しきい値
ボールホルダーの決定距離(選手とボールの距離による判断)
let ball_holder_distance = 2.0;;
ロングパスの距離
let long_pass_distance = 10.0;;
zoneのしきい値の決定
let pass_zone_y_2 =  13.0;;
let pass_zone_y_1 = -13.0;;
(*let positive_sec = 40;;*)
正例とする行動がどれだけ長く続いているか
let positive_sec = 80;;
(*let rcg_bhv_num = 3;;*)
正例とする行動群の数の下限の設定
let rcg_bhv_num = 5;;

・分析方法
S式で記述されているログデータからサイクルごとのボールと選手のxy座標を抽出する．
データは主に以下の様な形で記述されている．
(team 1 HELIOS_base test 0 0)
//サイクル1　左チーム　右チーム　0 - 0
(show 1 ((b)             0 0 0 0) 
//サイクル１のボールの   X座標 Y VX VY(speed)
((l 1)      0 0x9 -49    0      0  0 2.327 0 (v h 180) (s 8000 1 1 130600) (c 0 0 906 0 1 907 1 0 0 0 0))
leftチーム1       x座標  Y座標  VX VY

選手とボールの位置を抽出することができたら
ボールに一番近い選手をボールホルダーを定義する．
ボールホルダーが移り変わることをパスと定義する．
(スローインなどの特殊動作もパスに含まれてしまう)

パスを抽出することができたらそのパスのされた座標からそのパスの属性を決定する．
設定する属性は
・longpass
・enemypass (敵の位置(パスをする前に敵がいるか(p:previus)，
された後にいるか(after)))
・zone (パスの縦座標を大まかにわけたもの)(ex:zone1,zone2)
・order (パスの順番)
以上になる．
この情報の定義は上記の閾値で設定されている．

Progolのデータとして出力されたデータの説明を加える
モード宣言



(*
ボールと選手の相対距離からボールホルダーの抽出
ボールホルダーの遷移からパスの抽出
パスの行われた位置から
　敵の相対距離からの抽出によるボールの性質の決定
　パスがどこからどこへ行われたか(zone)13,-13
　パスの順番(オーダー)
*)