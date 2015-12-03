open Parser
open Point

(**
 * [kNN_train l] takes in training data and outputs the collection of points that
 * result.
 *)
val kNN_train  : data list -> points

(**
 * [classify p ps] classifies a point according to training points ps.
 *
 * It returns the predicted category.
 *)
val classify  : point -> points -> cat

(**
 * [predict d ps] predicts the classification of the given d under the training
 * points ps, and then returns a tuple of the correct category with the
 * predicted category.
 *
 * Note the prediction relies heavily on the choice of distance used in the
 * algorithm, as well as the method of classifying categorical variable
 * distances.
 *)
val predict  : data -> points -> (cat * cat)

(**
 * [kNN_predict_all dl ps] tbd
 *)
val kNN_predict_all : data list -> points -> (int * (cat * cat)) list