open Parser
open ExpGradient

(** [eval] runs the EG algorithm to give weights
 *  to all three algorithms based on their performances
 *
 *  [eval e1 e2 e3] takes in the outputs from all three algorithms on the 
 *  evaluation set of data and returns a set of weights
 *  *)
val eval : eval_out -> eval_out -> eval_out -> weights
