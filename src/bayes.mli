open Parser
open Point

(**
 * [bayes_train l] takes in training data and outputs the collection of points that
 * result.
 *)
val bayes_train  : data list -> points

(**
 * [bayes_predict_all dl ps] tbd
 *)
val bayes_predict_all : data list -> points -> (int * (cat * cat)) list