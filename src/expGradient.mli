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

(** [adjust] tweeks the [weights] of algorithms based on the [prediction] given
 *  [param] is just a constant that can be modified in the EG algoirthm that
 *  we might need when we are dealing with the entire system's performance
 * *)
val adjust : prediction -> weights -> float -> unit

(** [exp_eval] does the same thing as ClassifierEval.eval, it's only created 
 * to avoid exposing the implementation of type prediction
 * *)
val exp_eval: eval_out -> eval_out -> eval_out -> weights

(** [print_weights] prints all three algorithm's weights out.
 * *)
val print_weights: weights -> unit

(** [combine_results] takes in all three algorithms' outputs and returns a list
 * that contains a list of (cat, prob) for each data point that will later be 
 * used to write to a properly formatted csv file.
 * *)
val combine_results: eval_out -> eval_out -> eval_out -> weights -> output list
