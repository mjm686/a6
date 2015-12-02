open Parser
open Point

(**
 * [bayes_train l] takes in training data and outputs the collection of points that
 * result.
 *)
val bayes_train  : data list -> points

(**
 * [prior_probability ps cat] returns the prior probability of any point in ps
 * being a certain category cat . That is, it returns the number of points of
 * that category divided by the total number of points.
 *
 * The proabilitiy is a float from 0.0 to 1.0 inclusive.
 *)
val prior_probability  : points -> cat -> float

(**
 * [likelihood p ps cat] returns the likelihood of having point p in ps given
 * category cat.
 *
 * The proabilitiy is a float from 0.0 to 1.0 inclusive.
 *)
val likelihood  : point -> points -> cat -> float

(**
 * [posterior_probability p ps cat] returns the posterior probability of
 * point p in ps being category cat. That is, it returns the probability of
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
 * [predict d ps] predicts the classification of the given d under the training
 * points ps, and then returns the predicted category.
 *
 *)
val predict  : data -> points -> (cat * cat)

(**
 * [bayes_predict_all dl ps] tbd
 *)
val bayes_predict_all : data list -> points -> (int * (cat * cat))