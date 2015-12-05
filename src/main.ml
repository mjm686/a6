open KNN
open Parser
open Bayes
open ClassifierEval
open RandomForest
open Printf
open ExpGradient
open Core

type fname = string

let rec print_outputs ls =
  match ls with
  | [] -> printf "++++++++++++++++++++++++++++++++++++++++++++++++++\n"
  | h::t ->
      let cats = snd h in
      let p = printf "%d correct:%s | predicted:%s%!\n" in
      p (fst h) (cat_to_string (fst cats)) (cat_to_string (snd cats));
      print_outputs t

let accuracy outputs =
  let correct = List.filter (fun i ->
    let cats = snd i in
    fst cats = (snd cats)) outputs in
  ((float_of_int (List.length correct)) /. (float_of_int (List.length outputs))) *. 100.0

let main train test output =
  let _ = printf "##############################################\n" in
  let start = Time.now () in
  let ic = load_file train in
  let dl = try parse_train ic with
  | EOF d -> d in
  let _ = printf "Finished parsing traning\n" in

  let ic_test = load_file test in
  let test = try parse_train ic_test with
  | EOF d -> d in
  let _ = printf "Finished parsing testing\n" in

  let point_training = kNN_train dl in
  let _ = printf "KNN Traning Done...Starting classifying...\n" in
  let kNN_results = kNN_predict_all test point_training in
  let _ = printf "%d kNN results\n" (List.length kNN_results) in

  let rf = finale dl test in
  let _ = printf "Ranfom forest done on %d records with %d training\n" (List.length test) (List.length dl) in
  let _ = printf "Random forest accuracy: %f%% \n" (accuracy rf) in
  let _ = printf "kNN accuracy: %f%% \n" (accuracy kNN_results) in
  let bayes = bayes_predict_all test (point_training) in
  let _ = printf "***Bayes done***\n" in
  let _ = printf "Bayes accuracy: %f%% \n" (accuracy bayes) in
  let weights = eval rf kNN_results bayes in
  let _ = print_weights weights in
  let endTime = Time.now () in
  let _ = printf "####################################################\n" in
  let _ = print_endline (Time.Span.to_string (Time.diff endTime start)) in
  
  (*
   * let rf_result = rf_classify test (rf_train d) in
   * write_output output (results) weights;)
   * *)
  ()

let () = main "../data/train_1.csv" "../data/test_1.csv" ""
