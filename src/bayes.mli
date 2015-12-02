open Parser
open Point

(**
 * [train l] takes in training data and outputs the collection of points that
 * result.
 *)
val train  : data list -> points

(**
 * [prior_probability cat] returns the prior probability of any point being
 * a certain category cat. That is, it returns the number of points of that
 * category divided by the total number of points.
 *
 * The proabilitiy is a float from 0.0 to 1.0 inclusive.
 *)
val prior_probability  : cat -> float

(**
 * [likelihood p cat] returns the likelihood of having point p given category
 * cat.
 *
 * The proabilitiy is a float from 0.0 to 1.0 inclusive.
 *)
val likelihood  : point -> cat -> float

(**
 * [posterior_probability p cat] returns the posterior probability of
 * point p being category cat. That is, it returns the probability of
 * category cat given point p.
 *
 * The proabilitiy is a float from 0.0 to 1.0 inclusive.
 *)
val posterior_probability  : point -> cat -> float

(**
 * [classify p ps] classifies a point according to training points ps.
 *
 * It returns a new point of the predicted category.
 *)
val classify  : point -> points -> point

(**
 * [predict d] predicts the classification of the given d under the training
 * points, and then returns the predicted category.
 *
 *)
val predict  : data -> points -> cat