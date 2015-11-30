(* a prediction is consisted of the [algo] that it's produced by, 
 * the class predicted, and the correct class *)
type prediction
(* record type to keep track of the weights of algorithms
 * all weights sum up to 1 always *)
type weights
type param
type loss
(** [init] initiates a set of equal weights that sum to 1 for all algorithms
 *
 *  [init ()] returns a weights record
 * *)
val init : unit -> weights

(** [adjust] tweeks the [weights] of algorithms based on the [prediction] given
 *  [param] is just a constant that can be modified in the EG algoirthm that
 *  we might need when we are dealing with the entire system's performance
 *
 *  [adjust p w param] returns a new set of weights
 * *)
val adjust : prediction -> weights -> param -> weights
