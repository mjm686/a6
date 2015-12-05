open Core
open Parser

type attr =
  | Time of int
  | X of float
  | Y of float
  | Day of day

type tree =
  | Leaf of leaf
  | Node of node
and node = {t : tree list; a : attr}
and leaf = {c : cat; mutable i : int}

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
  [Mon; Tue; Wed; Thur;
  Fri; Sat; Sun]

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

let ofDayToTime (ti : Time.Ofday.t) : attr =
  Time (Pervasives.int_of_float
    ((Time.Ofday.to_float ti) /. 3600.));;

let yConvert (fY : float) : attr =
  let newY = (fY -. 37.69) /. 0.15 in
  if newY < 0. then Y 0. else
  if newY >= 1. then Y 0.9 else
  Y ((Pervasives.floor (10. *. newY)) /. 10.);;

let xConvert (fX : float ) : attr =
  let newX = (fX +. 122.53) /. 0.2 in
  if newX < 0. then X 0. else
  if newX >= 1. then X 0.9 else
  X ((Pervasives.floor (10. *. newX)) /. 10.);;

let updateTree (dat : data) (tl : tree list)
: unit =
  editTree (Day dat.dayOfWeek)
  (ofDayToTime (dat.ofDay))
  (xConvert dat.x)
  (yConvert dat.y)
  (dat.category)
  (tl);;

let rec randomForest (datl : data list)
(tl : tree list) : tree list =
  match datl with
  |[] -> tl
  |h::t -> updateTree h tl; randomForest t tl;;
