open Core
open Parser

type attr =
  | Time of int
  | X of float
  | Y of float
  | Date of date
and date = MON | TUE | WED | THU
         | FRI | SAT | SUN | UNK

type tree =
  | Leaf of leaf
  | Node of node
and node = {t : tree list; a : attr}
and leaf = {c : cat; mutable i : int}
and cat =
  | ARSON | ASSAULT | BADCHECKS | BRIBERY
  | BURGLARY | DISORDERLY | DRIVING | DRUG
  | DRUNK | EMBEZZLE | EXTORTION | FAMILY
  | FORGERY | FRAUD | GAMBLING | KIDNAPPING
  | LARCENY | LIQUOR | LOITER | MISSING
  | NONCRIMINAL | OTHER | PORN | PROSTITUTION
  | RECOVERED | ROBBERY | RUNAWAY | SECONDARY
  | SEXOFFENSESF | SEXOFFENSESNF | STOLEN
  | SUICIDE | SUSPICIOUS | TREA | TRESPASS
  | VANDALISM | VEHICLE | WARRANTS | WEAPON
  | UNDETERMINED | UNRECOGNIZED

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

let dateList =
  [MON; TUE; WED; THU;
  FRI; SAT; SUN; UNK]

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

let rec makeNodes (dl : date list)
(il : int list) (fl1 : float list)
(fl2 : float list)
(cl : cat list) : tree list =
  match dl with
  |[] -> []
  |h::t ->
  (Node {t=(makeNodes3 il fl1 fl2 cl);a=Date h})::
  makeNodes t il fl1 fl2 cl;;

let hollowForest (unit) : tree list =
  makeNodes dateList timeList
  coordList coordList catList;;

let leafBool (categ : cat) = function
  |Leaf l -> (l.c = categ)
  |Node _ -> false;;

let nodeBool (att : attr) = function
  |Leaf _ -> false
  |Node n -> (n.a = att);;

let nodeListGet = function
  |Leaf _ -> []
  |Node n -> n.t;;

let editLeaf = function
  |Leaf l -> (l.i <- (l.i+1))
  |Node _ -> ();;

let rec editTree1 (categ : cat) (tl : tree list)
: unit = match tl with
  |[] -> ()
  |h::t -> if (leafBool categ h)
  then (editLeaf h)
  else (editTree1 categ t);;

let rec editTree2 (aY : attr) (categ : cat)
(tl : tree list) : unit = match tl with
  |[] -> ()
  |h::t -> if (nodeBool aY h)
  then (editTree1 categ (nodeListGet h))
  else (editTree2 aY categ t);;

let rec editTree3 (aX : attr) (aY : attr)
(categ : cat) (tl : tree list)
: unit = match tl with
  |[] -> ()
  |h::t -> if (nodeBool aX h)
  then (editTree2 aY categ (nodeListGet h))
  else (editTree3 aX aY categ t);;

let rec editTree4 (aTime : attr) (aX : attr)
(aY : attr) (categ : cat) (tl : tree list)
: unit = match tl with
  |[] -> ()
  |h::t -> if (nodeBool aTime h)
  then (editTree3 aX aY categ (nodeListGet h))
  else (editTree4 aTime aX aY categ t);;

let rec editTree (aDay : attr) (aTime : attr)
(aX : attr) (aY : attr) (categ : cat)
(tl : tree list) : unit = match tl with
  |[] -> ()
  |h::t -> if (nodeBool aDay h)
  then (editTree4 aTime aX aY categ (nodeListGet h))
  else (editTree aDay aTime aX aY categ t);;

let dayToDate (d : Parser.day) : attr =
  match d with
  |Mon -> Date MON
  |Tue -> Date TUE
  |Wed -> Date WED
  |Thur -> Date THU
  |Fri -> Date FRI
  |Sat -> Date SAT
  |Sun -> Date SUN
  |_ -> Date UNK;;

let ofDayToTime (ti : Core.Time.Ofday.t) : attr =
  Time (Pervasives.int_of_float ti);;

let updateTree (dat : data) (tl : tree list)
: unit =
  editTree (dayToDate (dat.dayOfWeek))
  (ofDayToTime (dat.ofDay))
  (X (dat.x))
  (Y (dat.y))
  (dat.category)
  (tl);;

let randomForest (datl : data list)
(tl : tree list) : tree list =
  match datl with
  |[] -> tl
  |h::t -> updateTree h tl; randomForest t tl;;
