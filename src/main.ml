open KNN
open Parser
open Bayes
open ClassifierEval
open RandomForest

type fname = string

let main train test output =
  let ic = load_file train in
  let ic_test = load_file test in
  let d = parse_train ic in
  let test = parse_test ic_test in 
  let weights = eval () in
  (* let knn_result = knn_classify test (knn_train d) in
   * let bayes_result = bayes_classify test (bayes_train d) in
   * let rf_result = rf_classify test (rf_train d) in
   * write_output output (results) weights;)    
   * *)
  failwith "TODO"
