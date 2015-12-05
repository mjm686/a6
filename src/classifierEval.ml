open CrossValid
open Parser
open ExpGradient

exception End

(* Only need to ever run once to shuffle the data and write to 
 * different files *)
let cross_valid_file () = 
  let ic = load_file "../data/train.csv" in
  let rec helper ic = 
    let d = try parse_train ic 100000 with 
    | EOF d -> shuffle d 4; raise End in
    let _ = Printf.printf "total %d\n" (List.length d) in
    shuffle d 4;
    try helper ic with 
    | End -> () in
  helper ic 

let eval (rf: eval_out) (knn: eval_out) (bayes: eval_out) : weights =
 exp_eval rf knn bayes 

