open Parser
open rForest

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
 * [total tl] returns a the total number of crimes
 * recorded for a given list of Leafs tl.
 *
 * Tree list tl should be a list acquired by using
 * the above function getLeafs.
 *)
val total : tree list -> int

(**
 * [divideIntoFloat i1 i2] converts integers i1
 * and i2 into floats and divides them.
 *)
val divideIntoFloat : int -> int -> float

(**
 * [singlePair tl lf] returns a pair containing a
 * category and a float.
 *
 * The category is lf.c and the float is
 * lf.i divided by the total number of crimes in
 * list tl, assuming lf is a Leaf. If Leaf lf is
 * in list tl, the float will represent the
 * percentage of crimes of type lf.c in the
 * list tl.
 *
 * If lf is a Node, the pair returned will instead
 * contain category UNDETERMINED and float 0.
 *)
val singlePair : tree list -> tree -> cat * float

(**
 * [listPair tl tlr] returns a list of pairs
 * containing a category and a float.
 *
 * listPair takes in a list of trees (both tl
 * and tlr should be the same tree list) and
 * outputs a list of pairs; each pair is
 * the result of [singlePair] being applied
 * to an element of tl.
 *)
val listPair : tree list -> tree list
-> (cat * float) list

(**
 * [predict dat tl] returns a list of pairs of categories
 * and floats.
 *
 * The floats are probabilities, summing to 1, of the
 * predicted likelyhood of the corresponding crime
 * type, predicted based on a location and time given in
 * dat. These predictions are based on decision trees
 * created with training data and stored in tl.
 *)
val predict : data -> tree list -> (cat*float) list

(**
 * [predict dl tl] returns a list of lists of pairs
 * of categories and floats.
 *
 * predictions runs [predict] on each element of dl
 * and returns the resulting lists of pairs in a list.
 *)
val predictions : data list -> tree list ->
(cat*float) list list

(**
 * [finale dl1 dl2] returns a list of lists of pairs
 * of categories and floats.
 *
 * finale uses data list dl1 as "training data" to
 * construct a "random forest" of decision trees.
 * This forest is then used to run [predictions]
 * on the data list dl2 (the "testing data").
 *)
val finale : data list -> data list ->
(cat*float) list list
