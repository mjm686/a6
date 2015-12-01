type fname

(** [main] is the main method of the project. It loads training data from
 * [first fname] and testing data from [second fname] and writes the results
 * to [third fname] in csv format.
 *)
val main: fname -> fname -> fname -> unit
