open Core
open Parser

type attr =
  | Time of int
  | X of float
  | Y of float
  | Day of day
and day = Mon | Tue | Wed | Thu
        | Fri | Sat | Sun | Unk

type tree =
  | Leaf of leaf
  | Node of node
and node = {t:tree list; a:attr}
and leaf = {c:cat; mutable i:int}

let catList =
  [ARSON;
  ASSAULT;
  BADCHECKS;
  BRIBERY;
  BURGLARY;
  DISORDERLY;
  DRIVING;
  DRUG;
  DRUNK;
  EMBEZZLE;
  EXTORTION;
  FAMILY;
  FORGERY;
  FRAUD;
  GAMBLING;
  KIDNAPPING;
  LARCENY;
  LIQUOR;
  LOITER;
  MISSING;
  NONCRIMINAL;
  OTHER;
  PORN;
  PROSTITUTION;
  RECOVERED;
  ROBBERY;
  RUNAWAY;
  SECONDARY;
  SEXOFFENSESF;
  SEXOFFENSESNF;
  STOLEN;
  SUICIDE;
  SUSPICIOUS;
  TREA;
  TRESPASS;
  WARRANTS;
  WEAPON;
  UNRECOGNIZED;
  UNDETERMINED]

let coordList =
  [0.;
  0.1;
  0.2;
  0.3;
  0.4;
  0.5;
  0.6;
  0.7;
  0.8;
  0.9]

let timeList =
  [0; 1; 2; 3; 4; 5;
  6; 7; 8; 9; 10; 11;
  12; 13; 14; 15; 16; 17;
  18; 19; 20; 21; 22; 23]

let dayList =
  [Mon; Tue; Wed; Thu;
  Fri; Sat; Sun; Unk]

let rec makeLeaves (l : cat list) : tree list =
  match l with
  |[] -> []
  |h::t -> (Leaf {c=h;i=0})::makeLeaves t;;

let rec makeNodes1 (fl2 : float list)
(cl : cat list) : tree list =
  match fl2 with
  |[] -> []
  |h::t ->
  (Node {t=(makeLeaves cl);a=Y h})::makeNodes1 t cl;;

let rec makeNodes2 (fl1 : float list)
(fl2 : float list) (cl : cat list) : tree list =
  match fl1 with
  |[] -> []
  |h::t ->
  (Node {t=(makeNodes1 fl2 cl);a=X h})::
  makeNodes2 t fl2 cl;;

let rec makeNodes3 (il : int list)
(fl1 : float list) (fl2 : float list)
(cl : cat list) : tree list =
  match il with
  |[] -> []
  |h::t ->
  (Node {t=(makeNodes2 fl1 fl2 cl);a=Time h})::
  makeNodes3 t fl1 fl2 cl;;

let rec makeNodes (dl : day list)
(il : int list) (fl1 : float list)
(fl2 : float list)
(cl : cat list) : tree list =
  match dl with
  |[] -> []
  |h::t ->
  (Node {t=(makeNodes3 il fl1 fl2 cl);a=Day h})::
  makeNodes t il fl1 fl2 cl;;

let hollowForest (unit) : tree list =
  makeNodes dayList timeList
  coordList coordList catList;;

let editLeaf (l : leaf) = l.i <- (l.i + 1);;

let leafBool (categ : cat) = function
|Leaf l -> (l.c = categ)
|Node _ -> false;;

let nodeBool (att : attr) = function
|Leaf _ -> false
|Node n -> (n.a = att);;

let randomForest (datl : data list) : tree list ->
  [];;
