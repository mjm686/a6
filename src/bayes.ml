open Parser
open Point

let points_cat_tally = ref (tally_cats ([]))
let num_points = ref 0

(**
 * [bayes_train l] takes in training data and outputs the collection of points that
 * result.
 *)
let bayes_train l =
  create_points l

(**
 * [prior_probability ps cat] returns the prior probability of any point in ps
 * being a certain category cat . That is, it returns the number of points of
 * that category divided by the total number of points.
 *
 * The proabilitiy is a float from 0.0 to 1.0 inclusive.
 *)
let prior_probability ps cat =
  let n = float_of_int (List.assoc cat !points_cat_tally) in
  let d = float_of_int !num_points in
  n /. d

(**
 * [likelihood p ps cat] returns the likelihood of having point p in ps given
 * category cat.
 *
 * The proabilitiy is a float from 0.0 to 1.0 inclusive.
 *)
let likelihood p ps cat =
  let t_num = float_of_int (List.assoc cat (!points_cat_tally)) in

  let s = points_within_feat 5. p DATE ps in
  let s_num = float_of_int (List.length s) in
  let date_p = s_num /. t_num in

  let s = points_within_feat 30. p OFDAY ps in
  let s_num = float_of_int (List.length s) in
  let ofDay_p = s_num /. t_num in

  let s = points_within_feat 0.5 p DAYOFWEEK ps in
  let s_num = float_of_int (List.length s) in
  let dayOfWeek_p = s_num /. t_num in

  let s = points_within_feat 0.005 p X ps in
  let s_num = float_of_int (List.length s) in
  let x_p = s_num /. t_num in

  let s = points_within_feat 0.005 p Y ps in
  let s_num = float_of_int (List.length s) in
  let y_p = s_num /. t_num in

  date_p *. ofDay_p *. dayOfWeek_p *. x_p *. y_p

(**
 * [posterior_probability p ps cat] returns the posterior probability of
 * point p in ps being category cat. That is, it returns the probability of
 * category cat given point p.
 *
 * The proabilitiy is a float from 0.0 to 1.0 inclusive.
 *)
let posterior_probability p ps cat =
  (prior_probability ps cat) *. (likelihood p ps cat)

(**
 * [classify p ps] classifies a point according to training points ps.
 *
 * It returns the predicted category.
 *)
let classify p ps =
  let c = ref UNDETERMINED in
  let n = ref 0. in
  let _ =
  (for i = 1 to 38 do
    (let x = get_category i in
    let p = posterior_probability p ps x in
    if p >= !n then ((c := x);(n := p)) else ())
  done) in
  !c


(**
 * [predict d ps] predicts the classification of the given d under the training
 * points ps, and then returns the predicted category.
 *
 *)
let predict d ps =
  let p = create_point d in
  let pred_c = classify p ps in
  ((classification p), pred_c)

(**
 * [bayes_predict_all dl ps] tbd
 *)
let bayes_predict_all dl ps =
  let _ = (points_cat_tally := tally_cats ps) in
  let _ = (num_points := List.length ps) in
  let rec loop dl out =
    match dl with
      | h::t -> let id = h.id in
                loop t ((id, (predict h ps))::out)
      | [] -> [] in
  loop dl []