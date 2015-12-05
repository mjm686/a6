open Parser
open Point

(**
 * [kNN_train l] takes in training data and outputs the collection of points that
 * result.
 *)
val kNN_train  : data list -> points

(**
 * [kNN_predict_all dl ps] predicts the classifications of the given data list
 * dl under the training points ps, and then returns a list of tuples of the
 * correct categories followed by the predicted categories.
 *)
val kNN_predict_all : data list -> points -> (int * (cat * cat)) list