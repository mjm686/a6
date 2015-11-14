(* a set represents a new partition of the training data for 
 * cross validation*)
type set = {
  train: data list;
  test: data list
}

(** [shuffle] shuffles the training data into four different sets of training
 * and testing data for cross validation with the test set being a different
 * partition for each set
 *
 * [shuffle d] returns a list of size 4 containing 4 sets
 * *)
val shuffle: data list -> set list
