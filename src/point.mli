open Parser
open Core

(**
 * Represents a 6-dimensional data point from data fields of a given crime.
 *)
type point = {
  ident : int;
  date : float;
  ofDay : float;
  category : cat;
  dayOfWeek : day;
  pdDistrict : string;
  x : float;
  y : float
}

(**
 * Represents an entire collection of data points, forming the whole graph.
 *)
type points = point list

(**
 * tbd
 *)
type features = DATE | OFDAY | DAYOFWEEK | X | Y

(**
 * [create_empty_points()] creates a collection of 0 points
 *)
val create_empty_points  : unit -> points

(**
 * [create_point d] creates an individual 6-dimensional point from the given
 *  data.
 *)
val create_point  : data -> point

(**
 * [create_points d] creates a collection of points from the given data list d.
 *)
val create_points  : data list -> points

(**
 * [distance p1 p2] calculates the Euclidean distance between two points p1
 * and p2.
 *)
val distance  : point -> point -> float

(**
 * [points_within k p ps] returns a list of points within the given distance.
 *
 * For point p within a field of points ps, returns the points that fall
 * within the given distance.
 *)
val points_within  : float -> point -> points -> points

(**
 * tbd
 *)
val points_within_feat  : float -> point -> features -> points -> points

(**
 * [classification p] returns the category classification of the given point.
 *)
val classification : point -> cat

(**
 * tbd
 *)
val get_category : int -> cat

(**
 * tbd
 *)
val get_number : cat -> int

(**
 * tbd
 *)
val tally_cats : points -> (cat * int) list

(**
 * [num_of_class ps cat] returns the number of points of the given category
 * cat within the provided points ps.
 *)
val num_of_class : points -> cat -> int