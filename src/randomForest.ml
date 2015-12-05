open Parser
open RForest
open Core
open Printf

let rec getLeafs1 (aY : attr)
(tl : tree list) : tree list = match tl with
  |[] -> []
  |h::t -> if (nodeBool aY h)
  then (nodeListGet h)
  else (getLeafs1 aY t);;

let rec getLeafs2 (aX : attr) (aY : attr)
(tl : tree list) : tree list = match tl with
  |[] -> []
  |h::t -> if (nodeBool aX h)
  then (getLeafs1 aY (nodeListGet h))
  else (getLeafs2 aX aY t);;

let rec getLeafs3 (aTime : attr) (aX : attr)
(aY : attr) (tl : tree list) : tree list
= match tl with
  |[] -> []
  |h::t -> if (nodeBool aTime h)
  then (getLeafs2 aX aY (nodeListGet h))
  else (getLeafs3 aTime aX aY t);;

let rec getLeafs4 (aDay : attr) (aTime : attr)
(aX : attr) (aY : attr) (tl : tree list)
: tree list = match tl with
  |[] -> []
  |h::t -> if (nodeBool aDay h)
  then (getLeafs3 aTime aX aY (nodeListGet h))
  else (getLeafs4 aDay aTime aX aY t);;

let rec getLeafs (dat : data) (tl : tree list)
: tree list =
  getLeafs4 (Day dat.dayOfWeek)
  (ofDayToTime (dat.ofDay))
  (xConvert dat.x)
  (yConvert dat.y)
  (tl);;

let getI = function
  |Leaf l -> l.i
  |Node _ -> 0;;

let getC = function
  |Leaf l -> l.c
  |Node _ -> UNDETERMINED;;

let rec total (tl : tree list) : int =
  match tl with
  |[] -> 0
  |h::t -> (getI h) + (total t);;

let rec mostPrevalent (tl : tree list)
(i : int) (categ : cat) : cat =
  match tl with
  |[] -> categ
  |h::t -> if (getI h > i)
  then mostPrevalent t (getI h) (getC h)
  else mostPrevalent t i categ;;

let rec predict1 (tl : tree list)
(aY : attr) : cat =
  match tl with
  |[] -> UNDETERMINED
  |h::t -> if (nodeBool aY h)
  then mostPrevalent (nodeListGet h) 0
  UNDETERMINED
  else predict1 t aY;;

let rec predict2 (tl : tree list)
(aX : attr) (aY : attr) : cat =
  match tl with
  |[] -> UNDETERMINED
  |h::t -> if (nodeBool aX h)
  then predict1 (nodeListGet h) aY
  else predict2 t aX aY;;

let rec predict3 (tl : tree list)
(aTime : attr) (aX : attr) (aY : attr)
: cat =
  match tl with
  |[] -> UNDETERMINED
  |h::t -> if (nodeBool aTime h)
  then predict2 (nodeListGet h) aX aY
  else predict3 t aTime aX aY;;

let rec predict4 (tl : tree list)
(aDay : attr) (aTime : attr) (aX : attr)
(aY : attr) : cat =
  match tl with
  |[] -> UNDETERMINED
  |h::t -> if (nodeBool aDay h)
  then predict3 (nodeListGet h) aTime aX aY
  else predict4 t aDay aTime aX aY;;

let predictCat (dat : data) (tl : tree list)
: cat =
  predict4 tl (Day dat.dayOfWeek)
  (ofDayToTime (dat.ofDay))
  (xConvert dat.x)
  (yConvert dat.y);;

let predict (dat : data) (tl : tree list)
: int * (cat * cat) =
  ((dat.id), (dat.category,
    (predictCat dat tl)))

let counter = ref 0 
let rec predictions (datlist : data list)
(tl : tree list)
: (int * (cat * cat)) list =
  let _ = 
    if !counter mod 1000 = 0 
    then printf "Random forest has classified %d data points...\n" !counter
    else () in
  match datlist with
  |[] -> []
  |h::t ->
      incr counter;
    (predict h tl)::(predictions t tl);;

let finale (train : data list)
(test : data list)
: (int * (cat * cat)) list =
  predictions test
  (randomForest train (hollowForest ()));;
