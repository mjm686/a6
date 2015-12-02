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
  let t_num = List.assoc cat (!points_cat_tally) in

  let date = p.date in
  let s = Point.points_within_feat 5. p DATE !points_data in
  let s_num = List.length s in
  let date_p = s_num /. t_num in

  let ofDay =
  let s = Point.points_within_feat 30. p OFDAY !points_data in
  let s_num = List.length s in
  let ofDay_p = s_num /. t_num in

  let dayOfWeek =
  let s = Point.points_within_feat 0.5 p DAYOFWEEK !points_data in
  let s_num = List.length s in
  let dayOfWeek_p = s_num /. t_num in

  let x =
  let s = Point.points_within_feat 0.005 p X !points_data in
  let s_num = List.length s in
  let x_p = s_num /. t_num in

  let y =
  let s = Point.points_within_feat 0.005 p Y !points_data in
  let s_num = List.length s in
  let y_p = s_num /. t_num in

  date_p *. ofDay_p *. dayOfWeek_p *. x_p *. y_p

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