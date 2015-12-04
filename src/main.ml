open KNN
open Parser
open Bayes
open ClassifierEval
open RandomForest
open Printf

type fname = string

let rec print_outputs ls = 
  match ls with
  | [] -> ()
  | h::t ->
      let cats = snd h in
      let p = printf "%d correct:%s | predicted:%s\n" in
      p (fst h) (cat_to_string (fst cats)) (cat_to_string (snd cats));
      print_outputs t

let main train test output =
  let ic = load_file train in
  let ic_test = load_file test in
  let d = try parse_train ic with
  | EOF d -> d in
  let _ = printf "Finished parsing traning\n" in
  let test = try parse_train ic_test with 
  | EOF d -> d in
  let _ = printf "Finished parsing testing\n" in
  (*let weights = eval () in*)

  let _ = printf "Random Forest Done...\n" in
  let point_training = kNN_train d in
  let _ = printf "KNN Traning Done...Starting classifying...\n" in
  let kNN_results = kNN_predict_all test point_training in
  let _ = printf "%d kNN results\n" (List.length kNN_results) in
  let _ = print_outputs kNN_results in

  let _ = finale d test in
  (*let _ = bayes_predict_all test (point_training) in*)

    (*
   * let rf_result = rf_classify test (rf_train d) in
   * write_output output (results) weights;)
   * *)
  ()

let () = main "train_fold1.csv" "test_fold2.csv" ""
