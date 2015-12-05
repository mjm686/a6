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
val create_point  : data -> bool -> point

(**
 * [create_points d t] creates a collection of points from the given data list
 * d, with flag t indicating whether the points should be considered training
 * points.
 *)
val create_points  : data list -> bool -> points

(**
 * [distance p1 p2] calculates the Euclidean distance between two points p1
 * and p2.
 *)
val distance  : point -> point -> float

(**
 * [points_within k p ps ot] returns a list of points within the given distance.
 *
 * For point p within a field of points ps, returns the points that fall
 * within the given distance.
 *)
val points_within  : float -> point -> points -> points

(**
 * [points_within k p feat ps] returns a list of points within the given
 * distance, measured only the dimension of feature feat.
 *
 * For point p within a field of points ps, returns a list of points that fall
 * within the given distance.
 *)
val points_within_feat  : float -> point -> features -> points -> points

(**
 * [classification p] returns the category classification of the given point.
 *)
val classification : point -> cat

(**
 * [get_castegory i] returns the category classification assigned to the given
 * number.
 *)
val get_category : int -> cat

(**
 * [get_number cat] returns the number assigned to the given category
 * classification
 *)
val get_number : cat -> int

(**
 * [tally_cats ps] returns a list of tuples, one for each category, with
 * the first component of each tuple being the category, and the second
 * component being the number of training points of that cateogry in the points
 * ps.
 *)
val tally_cats : points -> (cat * int) list

(**
 * [num_of_class ps cat] returns the number of points of the given category
 * cat within the provided points ps.
 *)
val num_of_class : points -> cat -> int