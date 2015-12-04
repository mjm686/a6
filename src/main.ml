open KNN
open Parser
open Bayes
open ClassifierEval
(*open RandomForest*)

type fname = string

let main train test output =
  let ic = load_file train in
  (*let ic_test = load_file test in*)
  let d = try parse_train ic with
  | EOF d -> d in
  (*let test = try parse_train ic_test with
  | EOF d -> d in*)
  (*let weights = eval () in*)

  let point_training = kNN_train d in
  let _ = kNN_predict_all d (point_training) in
  let _ = bayes_predict_all d (point_training) in

  (*let _ = finale d test in*)
  (*
   * let rf_result = rf_classify test (rf_train d) in
   * write_output output (results) weights;)
   * *)
  ()

let () = main "train_fold1.csv" "test_fold1.csv" ""
