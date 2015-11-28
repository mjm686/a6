open Parser
open Dtree

(**
 * [listCats dat tl] returns a new tree list based on
 * data dat and tree list tl.
 *
 * This tree list will be the list of crime categories
 * corresponding to the location and time given in
 * dat (a list of Leafs).
 *)
val listCats : data -> tree list -> tree list

(**
 * [total tl] returns a the total number of crimes
 * recorded for a certain location and time.
 *
 * Tree list tl should be a list acquired by using
 * the above function listCats.
 *)
val total : tree list -> int

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
 * [predictions dl tl] returns a list of lists of pairs
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
