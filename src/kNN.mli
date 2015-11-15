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
 * [predict d ps] predicts the classification of the given d under the training
 * points ps, and then returns the predicted category.
 *
 * Note the prediction relies heavily on the choice of distance used in the
 * algorithm, as well as the method of classifying categorical variable
 * distances.
 *)
val predict  : data -> points -> cat