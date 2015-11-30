open Parser

(* a set represents a new partition of the training data for 
 * cross validation*)
type set

(** [shuffle] shuffles the training data into k different sets of training
 * and testing data for cross validation with the test set being a different
 * partition for each set
 *
 * [shuffle d k] writes out k pairs of training and testing datasets.
 * *)
val shuffle: data list -> int -> unit
