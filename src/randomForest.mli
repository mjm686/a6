open Parser
open RForest
open Core

(**
 * [getLeafs1 aY tl] returns a new tree list based on
 * attr aY and tree list tl.
 *
 * This tree list will be the list of Leafs
 * corresponding to the Node containing aY.
 *)
val getLeafs1 : attr -> tree list -> tree list

(**
 * [getLeafs2 aX aY tl] returns a new tree list
 * based on attr aY and aX and tree list tl.
 *
 * This tree list will be the list of Leafs
 * corresponding to the Node containing aY, whose
 * parent node contains aX.
 *)
val getLeafs2 : attr -> attr -> tree list
-> tree list

(**
 * [getLeafs3 aTime aX aY tl] returns a new
 * tree list based on attr aTime, aY, and aX and
 * tree list tl.
 *
 * This tree list will be the list of Leafs
 * corresponding to the Node containing aY, whose
 * parent node contains aX, whose parent node
 * contains aTime.
 *)
val getLeafs3 : attr -> attr -> attr -> tree list
-> tree list

(**
 * [getLeafs4 aDate aTime aX aY tl] returns a new
 * tree list based on attr aDate, aTime, aY,
 * and aX and tree list tl.
 *
 * This tree list will be the list of Leafs
 * corresponding to the Node containing aY, whose
 * parent node contains aX, whose parent node
 * contains aTime, whose parent node contains
 * aDate.
 *)
val getLeafs4 : attr -> attr -> attr -> attr
-> tree list -> tree list

(**
 * [getLeafs dat tl] returns a new tree list based on
 * data dat and tree list tl.
 *
 * This tree list will be the list of Leafs
 * corresponding to the location and time given in
 * dat.
 *
 * [getLeafs] uses the other getLeafs functions
 * as helpers, and essentially runs [getLeafs4]
 * using information from dat as arguments.
 *)
val getLeafs : data -> tree list -> tree list

(**
 * [getI lf] returns the value lf.i of Leaf i.
 *)
val getI : tree -> int

(**
 * [getC lf] returns the value lf.c of Leaf i.
 *)
val getC : tree -> cat

(**
 * [total tl] returns a the total number of crimes
 * recorded for a given list of Leafs tl.
 *
 * Tree list tl should be a list acquired by using
 * the above function getLeafs.
 *)
val total : tree list -> int

(**
 * [mostPrevalent tl i categ] returns a category.
 *
 * The category corresponds to the Leaf in tl
 * with the greatest i value; otherwise it
 * returns categ.
 *)
val mostPrevalent : tree list -> int -> cat
-> cat

(**
 * [predict1 tl aY] returns a category.
 *
 * It runs [mostPrevalent] on the Leaf list
 * corresponding to the node containing aY
 * in the list tl.
 *)
val predict1 : tree list -> attr -> cat

(**
 * [predict2 tl aX aY] returns a category.
 *
 * It runs [predict1] on the Node list
 * corresponding to the node containing aX
 * in the list tl.
 *)
val predict2 : tree list -> attr -> attr -> cat

(**
 * [predict3 tl aTime aX aY] returns a category.
 *
 * It runs [predict2] on the Node list
 * corresponding to the node containing aTime
 * in the list tl.
 *)
val predict3 : tree list -> attr -> attr
-> attr -> cat

(**
 * [predict4 tl aDay aTime aX aY] returns a category.
 *
 * It runs [predict3] on the Node list
 * corresponding to the node containing aDay
 * in the list tl.
 *)
val predict4 : tree list -> attr -> attr
-> attr -> attr -> cat

(**
 * [predictCat dat tl] returns a category.
 *
 * It runs [predict4] on the list tl and
 * returns the category predicted based on
 * time and location information in dat.
 *)
val predictCat : data -> tree list -> cat

(**
 * [predict dat tl] returns a value of the form
 * (int, (cat, cat)).
 *
 * The list predicts a crime type based on time
 * and location data in dat and returns
 * (dat's id, (category of crime recorded in dat,
 * predicted category of crime)).
 *)
val predict : data -> tree list
-> int * (cat * cat)

(**
 * [predictions dl tl] returns a list values of
 * the form (int, (cat, cat)).
 *
 * predictions runs [predict] on each element
 * of dl and returns the resulting values in
 * a list.
 *)
val predictions : data list -> tree list ->
(int * (cat * cat)) list

(**
 * [finale train test] returns a list values of
 * the form (int, (cat, cat)).
 *
 * finale uses data list train as "training data"
 * to construct a "random forest" of decision
 * trees. This forest is then used to run
 * [predictions] on the data list test (the
 * "testing data").
 *)
val finale : data list -> data list ->
(int * (cat * cat)) list
