open Parser
open Point

(**
 * [kNN_train l] takes in training data and outputs the collection of points that
 * result.
 *)
let kNN_train l =
  create_points l true


let i = ref 1

(**
 * [classify p ps] classifies a point according to training points ps.
 *
 * It returns the predicted category.
 *)
let classify p ps =
  
  let _ = 
    if (!i mod 1000) = 0 then print_endline ("kNN"^(string_of_int(!i)))
    else () in
  let _ = incr i in
  let ps_sub = points_within 8. p ps in
  let l = tally_cats ps_sub in
  let l = List.sort (fun x y -> if snd x > snd y then (-1) else 1) l in
  if l = [] then UNDETERMINED else
  let x = List.hd l in
  let out = fst x in
  out


(**
 * [predict d ps] predicts the classification of the given d under the training
 * points ps, and then returns a tuple of the correct category with the
 * predicted category.
 *
 * Note the prediction relies heavily on the choice of distance used in the
 * algorithm, as well as the method of classifying categorical variable
 * distances.
 *)
let predict d ps =
  let p = create_point d false in
  let c_pred = classify p ps in
  ((classification p), c_pred)

(**
 * [kNN_predict_all dl ps] tbd
 *)
let kNN_predict_all dl ps =
  let l =  List.map (fun x -> (x.id, (predict x ps))) dl in
  let _ = Printf.printf "kNN takes in %d records\n" (List.length dl) in
  l


