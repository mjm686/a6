open Parser
open ExpGradient

(** [eval] runs 4-fold cross validation with the EG algorithm to give weights
 *  to all three algorithms based on their performances, and then each 
 *  algorithm would receive the average weight of the four weights
 *
 *  [eval d] takes the parsed traning data and returns a set of weights for
 *  outputting the final predictions for the testing data
 * *)
val eval : data list -> weights
