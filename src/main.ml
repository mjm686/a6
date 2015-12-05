open KNN
open Parser
open Bayes
open ClassifierEval
open RandomForest
open Printf
open ExpGradient
open Core

type fname = string
type results = {
  knn: eval_out;
  bayes: eval_out;
  rf: eval_out
}

(* Prints out the classification results. Helper function for debugging. *)
let rec print_outputs ls =
  match ls with
  | [] -> printf "++++++++++++++++++++++++++++++++++++++++++++++++++\n"
  | h::t ->
      let cats = snd h in
      let p = printf "%d correct:%s | predicted:%s%!\n" in
      p (fst h) (cat_to_string (fst cats)) (cat_to_string (snd cats));
      print_outputs t

(* Gives the accuracy(%) of an algorithm with training data and testing data
 * for evaluation, meaning the testing data has correct classification and
 * is taken from the original training data. *)
let accuracy outputs =
  let correct = List.filter (fun i ->
    let cats = snd i in
    fst cats = (snd cats)) outputs in
  let num_correct = float_of_int (List.length correct) in
  let total = float_of_int (List.length outputs) in
  ( num_correct  /. total ) *. 100.0

(* Classifies [test] with all three algorithms after training with [train] *)
let classify train test = 
  let point_training = kNN_train train in
  let _ = printf "Starting kNN classifier...\n" in
  let kNN = kNN_predict_all test point_training in
  let _ = printf "kNN done classifying...Starting random forest...\n" in
  let rf = finale train test in
  let _ = printf "Ranfom forest done classifying...Starting bayes...\n" in
  let bayes = bayes_predict_all test (point_training) in
  let _ = printf "***All three algorithms done classifying***\n" in
  {knn=kNN;rf=rf;bayes=bayes}

(* Prints out accuracies for all algorithms obtained after classifying the
 * evaluation training data and testing data *)
let print_accuracies results =
  let rf = accuracy results.rf in
  let bayes = accuracy results.bayes in
  let knn = accuracy results.knn in 
  let _ = printf "-------------ACCURACIES-------------\n" in
  let _ = printf "| Random forest accuracy: %f%%|\n" (rf) in
  let _ = printf "| kNN accuracy: %f%%          |\n" (knn) in
  let _ = printf "| Bayes accuracy: %f%%        |\n" (bayes) in
  printf "-------------ACCURACIES-------------\n"

(* Main function that runs the entire system and then writes the final outputs
 * to a .csv file.
 * 
 * [n] maximal number of data points we intend to have from each file, if
 * a file has less than [n] records, m, the parser just goes through the file
 * and returns all the data points in there.
 * [train] the filename of the training set, should be "../data/train.csv".
 * [test] the filename of the testing set, should be "../data/test.csv".
 * [eval_train] the filename of the shuffled training set obtained from 
 * the 4-fold cross validation, could be "../data/train_1.csv".
 * [eval_test] the filename of the shuffled testing set obtained from the
 * 4-fold cross validation, could be "../data/test_1.csv. 
 * [output] the filename of the output file.
 *
 * Note: the suffix number of [eval_test] has to be the same as the file used 
 * for [eval_train]. Otherwise there will be duplicates of data points in 
 * [eval_test]".
 *
 * *)
let main n ftrain ftest eval_train eval_test output =
  (* Starts timer *)
  let _ = printf "-----------------------------------------------------\n" in
  let start = Time.now () in

  (* Loads [train] into stdin and parses the training data *)
  let ic = load_file ftrain in
  let dl = try parse_train ic n with
  | EOF d -> d in
  let _ = printf "Parsed %d traning data...\n" (List.length dl) in

  (* Loads [test] into stdin and parses the testing data *)
  let ic_test = load_file ftest in
  let test = try parse_test ic_test n with
  | EOF d -> d in
  let _ = printf "Parsed %d testing data...\n" (List.length test) in

  (* Loads [eval_train] into stdin and parses it *)
  let ic_eval_train = load_file eval_train in
  let eval_train = try parse_train ic_eval_train n with
  | EOF d -> d in
  let p = printf "Parsed %d training set for evaluation...\n" in
  let  _ = p (List.length eval_train) in
  
  (* Loads [eval_test] into stdin and parses it *)
  let ic_eval_test = load_file eval_test in
  let eval_test = try parse_train ic_eval_test n with
  | EOF d -> d in
  let p = printf "Parsed %d testing set for evaluation...\n" in
  let  _ = p (List.length eval_test) in
  
  (* Evaluate the algorithms and obtain weights for the final output based on
   * the evaluation results *)
  let eval_results = classify eval_train eval_test in
  let weights = eval eval_results.rf eval_results.knn eval_results.bayes in
  let _ = print_weights weights in
  let _ = print_accuracies eval_results in
  
  (* Train with the training data and classify the testing data and assign 
   * probabilities to each algorithm's output. Finally, write to [output]*)
  let _ = printf "Finished classifier evaluation!\n" in
  let _ = printf "****************************************************\n" in
  let _ = printf "NOW CLASSIFYING %s WITH %s AS TRAINING SET..." ftest ftrain in
  let results = classify dl test in
  let partial = combine_results (List.rev results.rf) (List.rev results.knn) in
  let results = partial results.bayes weights in

  let endTime = Time.now () in
  let _ = printf "----------------------------------------------------\n" in
  (* Print out total time elapsed *)
  let _ = print_endline (Time.Span.to_string (Time.diff endTime start)) in
  write_to output results

(* The function call that runs the project *)
let () = main 3000 "../data/train.csv" "../data/test.csv" "../data/train_1.csv" "../data/test_1.csv" "test_output.csv"
