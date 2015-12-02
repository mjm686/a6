open Parser
open Point

(**
 * [train l] takes in training data and outputs the collection of points that
 * result.
 *)
val train  : data list -> points

(**
 * [classify p ps] classifies a point according to training points ps.
 *
 * It returns a new point of the predicted category.
 *)
val classify  : point -> points -> point

(**
 * [predict d] predicts the classification of the given d under the training
 * points, and then returns a tuple of the correct category with the
 * predicted category.
 *
 * Note the prediction relies heavily on the choice of distance used in the
 * algorithm, as well as the method of classifying categorical variable
 * distances.
 *)
val predict  : data -> (cat * cat)

(**
 * [predict_all dl] tbd
 *)
val predict_all : data list -> (int * (cat * cat))