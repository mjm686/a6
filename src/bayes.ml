open Parser
open Point

let points_data = ref points
let points_cat_tally = ref points
let num_points = ref int


(**
 * [train l] takes in training data and outputs the collection of points that
 * result.
 *)
let train  =
  let x = Point.create_points l in
  let _ = points_data := x in
  (points_cat_tally := Point.tally_cats x);
  (num_points := List.length l);
  x

(**
 * [prior_probability cat] returns the prior probability of any point being
 * a certain category cat. That is, it returns the number of points of that
 * category divided by the total number of points.
 *
 * The proabilitiy is a float from 0.0 to 1.0 inclusive.
 *)
let prior_probability cat =
  let cat_num = Point.get_number in
  let n = float_of_int (List.assoc cat_num !points_cat_tally) in
  let d = float_of_int !num_points in
  n /. d

(**
 * [likelihood p cat] returns the likelihood of having point p given category
 * cat.
 *
 * The proabilitiy is a float from 0.0 to 1.0 inclusive.
 *)
let likelihood p cat =
  (** let date = p.date in
  let t = Point.points_within 5. p !points_data in
  let t_num = float_of_int (List.length t) in
  let sub = Point.num_of_class t cat in*)
  failwith("tbd")


(**
 * [posterior_probability p cat] returns the posterior probability of
 * point p being category cat. That is, it returns the probability of
 * category cat given point p.
 *
 * The proabilitiy is a float from 0.0 to 1.0 inclusive.
 *)
let posterior_probability p cat =
  (prior_probability cat) *. (likelihood p cat)

(**
 * [classify p ps] classifies a point according to training points ps.
 *
 * It returns a new point of the predicted category.
 *)
let classify =

(**
 * [predict d] predicts the classification of the given d under the training
 * points, and then returns the predicted category.
 *
 *)
let predict d =
  let p = Point.create_point d
  let ps = !points_data in
  classify p ps