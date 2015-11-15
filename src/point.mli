Open Parser

(**
 * Represents a 6-dimensional data point from data fields of a given crime.
 *)
type point

(**
 * Represents an entire collection of data points, forming the whole graph.
 *)
type points

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
 * For point p within a field of points ps, returns a list of points that fall
 * within the given distance.
 *)
val points_within  : int -> point -> points -> point list

(**
 * [classification p] returns the category classification of the given point.
 *)
val classification : point -> cat