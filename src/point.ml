Open Parser

(**
 * Represents a 6-dimensional data point from data fields of a given crime.
 *)
type point = {
  id: int;
  date: float;
  ofDay: float;
  category: cat;
  dayOfWeek: day;
  pdDistrict: string;
  x: float;
  y: float
}

(**
 * Represents an entire collection of data points, forming the whole graph.
 *)
type points = point list

(**
 * [create_point d] creates an individual 6-dimensional point from the given
 *  data.
 *)
let create_point d =
  let dt = d.date in
  let day = Date0.day dt in
  let month = Month.to_int (Date0.month dt)
  let dt = float_of_int(month * 30 + day)

  let tm = Span.minute (Time.Ofday.to_span_since_start_of_day d.ofDay)

  {
   id = d.id;
   date = dt
   ofDay = tm
   category = d.category;
   dayOfWeek = d.dayOfWeek;
   pdDistrict = d.pdDistrict;
   x = d.x;
   y = d.y;
  }


(**
 * [create_points d] creates a collection of points from the given data list d.
 *)
let create_points d =
  List.map (fun x -> create_point x) d

(**
 * [distance p1 p2] calculates the Euclidean distance between two points p1
 * and p2.
 *)
let distance p1 p2 =
  let d1 = if p1.dayOfWeek = p2.dayOfWeek then 0 else 1
  let d = sqrt (((p2.date - p1.date)**2.) +. ((p2.ofDay - p1.ofDay)**2.) +.
          ((p2.x - p1.x)**2.) +. ((p2.y - p1.y))**2.
           +. (d1)**2. )

(**
 * [points_within k p ps] returns a list of points within the given distance.
 *
 * For point p within a field of points ps, returns a list of points that fall
 * within the given distance.
 *)
let points_within  k p ps =
  List.filter (fun x -> (distance p x) <= k) ps


(**
 * [classification p] returns the category classification of the given point.
 *)
let classification p = p.category

let tally_cats ps =
  let c_list = List.map (fun x -> classification x) ps
  let n_list = [(0, ref 0); (1, ref 0); (2, ref 0); (3, ref 0);
                (4, ref 0); (5, ref 0); (6, ref 0); (7, ref 0);
                (8, ref 0); (9, ref 0); (10, ref 0); (11, ref 0);
                (12, ref 0); (13, ref 0); (14, ref 0); (15, ref 0);
                (16, ref 0); (17, ref 0); (18, ref 0); (19, ref 0);
                (20, ref 0); (21, ref 0); (22, ref 0); (23, ref 0);
                (24, ref 0); (25, ref 0); (26, ref 0); (27, ref 0);
                (28, ref 0); (29, ref 0); (30, ref 0); (31, ref 0);
                (32, ref 0); (33, ref 0); (34, ref 0); (35, ref 0);
                (36, ref 0); (37, ref 0); (38, ref 0)]
  let rec loop c_list n_list =
    match c_list with
      | h::t -> (match h with
                  | ARSON -> incr (List.assoc 0 n_list)
                  | ASSAULT -> incr (List.assoc 1 n_list)
                  | BADCHECKS -> incr (List.assoc 2 n_list)
                  | BRIBERY -> incr (List.assoc 3 n_list)
                  | BURGLARY -> incr (List.assoc 4 n_list)
                  | DISORDERLY -> incr (List.assoc 5 n_list)
                  | DRIVING -> incr (List.assoc 6 n_list)
                  | DRUG -> incr (List.assoc 7 n_list)
                  | DRUNK -> incr (List.assoc 8 n_list)
                  | EMBEZZLE -> incr (List.assoc 9 n_list)
                  | EXTORTION -> incr (List.assoc 10 n_list)
                  | FAMILY -> incr (List.assoc 11 n_list)
                  | FORGERY -> incr (List.assoc 12 n_list)
                  | FRAUD -> incr (List.assoc 13 n_list)
                  | GAMBLING -> incr (List.assoc 14 n_list)
                  | KIDNAPPING -> incr (List.assoc 15 n_list)
                  | LARCENY -> incr (List.assoc 16 n_list)
                  | LIQUOR -> incr (List.assoc 17 n_list)
                  | LOITER -> incr (List.assoc 18 n_list)
                  | MISSING -> incr (List.assoc 19 n_list)
                  | NONCRIMINAL -> incr (List.assoc 20 n_list)
                  | OTHER -> incr (List.assoc 21 n_list)
                  | PORN -> incr (List.assoc 22 n_list)
                  | PROSTITUTION -> incr (List.assoc 23 n_list)
                  | RECOVERED -> incr (List.assoc 24 n_list)
                  | ROBBERY -> incr (List.assoc 25 n_list)
                  | RUNAWAY -> incr (List.assoc 26 n_list)
                  | SECONDARY -> incr (List.assoc 27 n_list)
                  | SEXOFFENSESF -> incr (List.assoc 28 n_list)
                  | SEXOFFENSESNF -> incr (List.assoc 29 n_list)
                  | STOLEN -> incr (List.assoc 30 n_list)
                  | SUICIDE -> incr (List.assoc 31 n_list)
                  | SUSPICIOUS -> incr (List.assoc 32 n_list)
                  | TREA -> incr (List.assoc 33 n_list)
                  | TRESPASS -> incr (List.assoc 34 n_list)
                  | VANDALISM -> incr (List.assoc 35 n_list)
                  | VEHICLE -> incr (List.assoc 36 n_list)
                  | WARRANTS -> incr (List.assoc 37 n_list)
                  | WEAPON -> incr (List.assoc 38 n_list)
                  | _ -> failwith("Error") );
                  loop t n_list
      | [] -> ()
  let out = loop c_list n_list in
  List.map (fun x -> (fst x, !(snd x))) out

let get_category i =
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
  | _ -> failwith("Error") );


(**
 * [num_of_class ps cat] returns the number of points of the given category
 * cat within the provided points ps.
 *)
let num_of_class ps cat =
  let rec loop ps cat i =
    match ps with
    | h::t -> if (h = cat) then (incr i); loop t cat i
    | [] -> i
  let i : (int ref) = ref 0 in
  loop ps cat i
