open Parser
open Point

let points_data = ref points

(**
 * [train l] takes in training data and outputs the collection of points that
 * result.
 *)
let train l =
  let x = Point.create_points l in
  let _ = points_data := x in
  x


(**
 * [classify p ps] classifies a point according to training points ps.
 *
 * It returns a new point of the predicted category.
 *)
let classify p ps =

  let ps_sub = Point.points_within 1. p ps in
  let l = Point.tally_cats ps_sub in
  let l = List.sort (fun x y -> if snd x > snd y then (-1) else 1) l in
  let x = List.hd l in
  let out = fst x in
  Point.get_category out


(**
 * [predict d] predicts the classification of the given d under the training
 * points, and then returns a tuple of the correct category with the
 * predicted category.
 *
 * Note the prediction relies heavily on the choice of distance used in the
 * algorithm, as well as the method of classifying categorical variable
 * distances.
 *)
let predict d =
  let p = Point.create_point d in
  let ps = !points_data in
  let p2 = classify p ps in
  ((Point.classification p), (Point.classification p2))

(**
 * [predict_all dl] tbd
 *)
let predict_all dl =
  let rec loop dl out =
    match dl with
      | h::t -> let id = h.id in
                loop t (id, (predict h))::out
      | [] -> [] in
  loop dl []