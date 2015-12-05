open Parser
open Core

(**
 * Represents a 6-dimensional data point from data fields of a given crime.
 *)
type point = {
  idp : int;
  datep : float;
  ofDayp : float;
  categoryp : cat;
  dayOfWeekp : day;
  pdDistrictp : string;
  xp : float;
  yp : float;
  training : bool
}

(**
 * Represents an entire collection of data points, forming the whole graph.
 *)
type points = point list

(**
 * Represents the features used to predict a point's classification.
 *)
type features = DATE | OFDAY | DAYOFWEEK | X | Y

(**
 * [create_point d] creates an individual many-dimensional point from the given
 *  data,with flag t indicating whether the point should be considered a
 * training point.
 *)
let create_point d t =
  let dt = d.date in
  let day = Date0.day dt in
  let month = Month.to_int (Date0.month dt) in
  let dt = float_of_int(month * 30 + day) in

  let tm = Span.to_min (Time.Ofday.to_span_since_start_of_day d.ofDay) in

  {
   idp = d.id;
   datep = dt;
   ofDayp = tm;
   categoryp = d.category;
   dayOfWeekp = d.dayOfWeek;
   pdDistrictp = d.pdDistrict;
   xp = d.x;
   yp = d.y;
   training = t
  }

(**
 * [create_points d t] creates a collection of points from the given data list
 * d, with flag t indicating whether the points should be considered training
 * points.
 *)
let create_points d t =
  List.map (fun x -> create_point x t) d

let date_distance p1 p2 =
  abs_float(p2.datep -. p1.datep)

let ofDay_distance p1 p2 =
  abs_float(p2.ofDayp -. p1.ofDayp)

let dayOfWeek_distance p1 p2 =
  if p1.dayOfWeekp = p2.dayOfWeekp then 0. else 1.

let x_distance p1 p2 =
  abs_float(p2.xp -. p1.xp)

let y_distance p1 p2 =
  abs_float(p2.yp -. p1.yp)

(**
 * [distance p1 p2] calculates the Euclidean distance between two points p1
 * and p2.
 *)
let distance p1 p2 =
  let d = sqrt (
            (((date_distance p1 p2) /. 5.)       ** 2.) +.
            (((ofDay_distance p1 p2) /. 30.)     ** 2.) +.
            (((dayOfWeek_distance p1 p2))        ** 2.) +.
            (((x_distance p1 p2) /. 0.0005)       ** 2.) +.
            (((y_distance p1 p2)) /. 0.0005)      ** 2.
          ) in
  d

(**
 * [points_within k p ps] returns a list of points within the given distance.
 *
 * For point p within a field of points ps, returns a list of points that fall
 * within the given distance.
 *)
let points_within k p ps =
  List.filter (fun x -> ((distance p x) <= k) && x.training) ps

(**
 * [points_within k p feat ps] returns a list of points within the given
 * distance, measured only the dimension of feature feat.
 *
 * For point p within a field of points ps, returns a list of points that fall
 * within the given distance.
 *)
let points_within_feat k p feat ps =
  match feat with
  | DATE -> List.filter (fun x -> ((date_distance p x) <= k) && x.training) ps
  | OFDAY -> List.filter (fun x -> ((ofDay_distance p x) <= k) && x.training) ps
  | DAYOFWEEK -> List.filter (fun x -> ((dayOfWeek_distance p x) <= k) && x.training) ps
  | X -> List.filter (fun x -> ((x_distance p x) <= k) && x.training) ps
  | Y -> List.filter (fun x -> ((y_distance p x) <= k) && x.training) ps

(**
 * [classification p] returns the category classification of the given point.
 *)
let classification p = p.categoryp

(**
 * [get_castegory i] returns the category classification assigned to the given
 * number.
 *)
let get_category = function
  | 0 -> ARSON
  | 1 -> ASSAULT
  | 2 -> BADCHECKS
  | 3 -> BRIBERY
  | 4 -> BURGLARY
  | 5 -> DISORDERLY
  | 6 -> DRIVING
  | 7 -> DRUG
  | 8 -> DRUNK
  | 9 -> EMBEZZLE
  | 10 -> EXTORTION
  | 11 -> FAMILY
  | 12 -> FORGERY
  | 13 -> FRAUD
  | 14 -> GAMBLING
  | 15 -> KIDNAPPING
  | 16 -> LARCENY
  | 17 -> LIQUOR
  | 18 -> LOITER
  | 19 -> MISSING
  | 20 -> NONCRIMINAL
  | 21 -> OTHER
  | 22 -> PORN
  | 23 -> PROSTITUTION
  | 24 -> RECOVERED
  | 25 -> ROBBERY
  | 26 -> RUNAWAY
  | 27 -> SECONDARY
  | 28 -> SEXOFFENSESF
  | 29 -> SEXOFFENSESNF
  | 30 -> STOLEN
  | 31 -> SUICIDE
  | 32 -> SUSPICIOUS
  | 33 -> TREA
  | 34 -> TRESPASS
  | 35 -> VANDALISM
  | 36 -> VEHICLE
  | 37 -> WARRANTS
  | 38 -> WEAPON
  | _ -> failwith("Error")

(**
 * [get_number cat] returns the number assigned to the given category
 * classification
 *)
let get_number cat =
    let l = [(ARSON, 0); (ASSAULT, 1); (BADCHECKS, 2);
            (BRIBERY, 3); (BURGLARY, 4); (DISORDERLY, 5);
            (DRIVING, 6); (DRUG, 7); (DRUNK, 8); (EMBEZZLE, 9);
            (EXTORTION, 10); (FAMILY, 11); (FORGERY, 12);
            (FRAUD, 13); (GAMBLING, 14); (KIDNAPPING, 15);
            (LARCENY, 16); (LIQUOR, 17); (LOITER, 18);
            (MISSING, 19); (NONCRIMINAL, 20); (OTHER, 21);
            (PORN, 22); (PROSTITUTION, 23); (RECOVERED, 24);
            (ROBBERY, 25); (RUNAWAY, 26); (SECONDARY, 27);
            (SEXOFFENSESF, 28); (SEXOFFENSESNF, 29); (STOLEN, 30);
            (SUICIDE, 31); (SUSPICIOUS, 32); (TREA, 33);
            (TRESPASS, 34); (VANDALISM, 35); (VEHICLE, 36);
            (WARRANTS, 37); (WEAPON, 38)] in
    List.assoc cat l

(**
 * [tally_cats ps] returns a list of tuples, one for each category, with
 * the first component of each tuple being the category, and the second
 * component being the number of training points of that cateogry in the points
 * ps.
 *)
let tally_cats ps =
  let ps = List.filter (fun x -> x.training) ps in
  let c_list = List.map (fun x -> classification x) ps in
  let n_list = [(0, ref 0); (1, ref 0); (2, ref 0); (3, ref 0);
                (4, ref 0); (5, ref 0); (6, ref 0); (7, ref 0);
                (8, ref 0); (9, ref 0); (10, ref 0); (11, ref 0);
                (12, ref 0); (13, ref 0); (14, ref 0); (15, ref 0);
                (16, ref 0); (17, ref 0); (18, ref 0); (19, ref 0);
                (20, ref 0); (21, ref 0); (22, ref 0); (23, ref 0);
                (24, ref 0); (25, ref 0); (26, ref 0); (27, ref 0);
                (28, ref 0); (29, ref 0); (30, ref 0); (31, ref 0);
                (32, ref 0); (33, ref 0); (34, ref 0); (35, ref 0);
                (36, ref 0); (37, ref 0); (38, ref 0)] in
  let rec loop c_list n_list =
    match c_list with
      | h::t -> (incr (List.assoc (get_number h) n_list));
                loop t n_list
      | [] -> n_list in
  let out = loop c_list n_list in
  List.map (fun x -> (get_category(fst x), !(snd x))) out

(**
 * [num_of_class ps cat] returns the number of points of the given category
 * cat within the provided points ps.
 *)
let num_of_class ps cat =
  let rec loop ps cat i =
    match ps with
    | h::t -> if (h.categoryp = cat) then (incr i); loop t cat i
    | [] -> !i in
  let i : (int ref) = ref 0 in
  loop ps cat i
