type fname
(** [main] is the main method of the project. It loads training data from
 *  [first fname] and testing data from [second fname] and training data for
 *  evaluation from [third fname] and testing data for evaluation from [fourth 
 *  fname] and writes the results to [fifth fname] in csv format. [n] is the
 *  maximum number of data points parsed into the system from each file.
 *    *)
val main: int -> fname -> fname -> fname -> fname -> fname -> unit
