open Parser

(* a prediction is consisted of the [algo] that it's produced by, 
 * the class predicted, and the correct class *)
type prediction
(* record type to keep track of the weights of algorithms
 * all weights sum up to 1 always *)
type weights

(** [init] initiates a set of equal weights that sum to 1 for all algorithms
 *
 *  [init ()] returns a weights record
 * *)
val init : unit -> weights

val default : unit -> weights

(** [adjust] tweeks the [weights] of algorithms based on the [prediction] given
 *  [param] is just a constant that can be modified in the EG algoirthm that
 *  we might need when we are dealing with the entire system's performance
 *
 *  [adjust p w param] returns a new set of weights
 * *)
val adjust : prediction -> weights -> float -> unit

val exp_eval: eval_out -> eval_out -> eval_out -> weights

val print_weights: weights -> unit

val combine_results: eval_out -> eval_out -> eval_out -> weights -> output list
