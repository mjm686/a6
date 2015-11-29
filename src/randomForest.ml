open Parser
open rForest

let rec getLeafs1 (aY : attr)
(tl : tree list) : tree list = match tl with
  |[] -> []
  |h::t -> if (nodeBool aY h)
  then (nodeListGet h)
  else (editTree2 aY t);;

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
  getLeafs4 (dayToDate (dat.dayOfWeek))
  (ofDayToTime (dat.ofDay))
  (X (dat.x))
  (Y (dat.y))
  (tl);;

let getI = function
  |Leaf l -> l.i
  |Node _ -> 0;;

let rec total (tl : tree list) : int =
  match tl with
  |[] -> 0
  |h::t -> (getI h) + (total t);;

let divideIntoFloat (i1 : int) (i2 : int)
: float =
  (Pervasives.float_of_int i1) /.
  (Pervasives.float_of_int i2);;

let singlePair (tl : tree list) = function
  |Node _ -> (UNDETERMINED, 0.)
  |Leaf l -> if (total tl = 0)
    then (UNDETERMINED, 0.)
    else (l.c,
      divideIntoFloat l.i (total tl));;

let rec listPair (tl : tree list)
(tlr : tree list) : (cat * float) list =
  match tl with
  |[] -> []
  |h::t -> (singlePair tlr h)::
  (listPair t tlr);;

let predict (dat : data) (tl : tree list)
: (cat * float) list =
  listPair (getLeafs dat tl) (getLeafs dat tl);;

let predictions (datl : data list)
(tl : tree list) : (cat * float) list list =
  match datl with
  |[] -> []
  |h::t -> (predict h tl)::(predictions t tl);;

let finale (datl1 : data list) (datl2 : data list)
: (cat * float) list list =
  predictions datl2
  (randomForest datl1 (hollowForest()));;
