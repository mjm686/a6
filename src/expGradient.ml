open Parser
open Printf

type algo = | KNN | RF | Bayes

type prediction = (algo * (cat * cat))

type weights = {
  mutable knn: float;
  mutable rf: float;
  mutable bayes: float
}

type loss = Correct | Incorrect

let init () = 
  let weight = 1.0 /. 3.0 in
  {knn=weight;rf=weight;bayes=weight}

(* Helper function to normalize all weights so they all sum to 1 *)
let normalize w = 
  let total = w.knn +. w.rf +. w.bayes in
  w.knn <- (w.knn /. total);
  w.rf <- (w.rf /. total);
  w.bayes <- (w.bayes /. total)

(* Helper function to implement the EG algorithm's formula *)
let expGrad param l w = 
  match l with
  | Correct -> w
  | Incorrect -> w *. (exp (-. param *. 1.0))

(* Implementations of functions in .mli *)
let adjust p w param = 
  match p with
  | (KNN, (correct, guess)) ->
      let new_w = 
        if correct = guess then expGrad param Correct w.knn
        else expGrad param Incorrect w.knn in
      w.knn <- new_w;
      normalize w
  | (RF, (correct, guess)) ->
      let new_w =
        if correct = guess then expGrad param Correct w.rf
        else expGrad param Incorrect w.rf in
      w.rf <- new_w;
      normalize w
  | (Bayes, (correct, guess)) ->
      let new_w = 
        if correct = guess then expGrad param Correct w.bayes
        else expGrad param Incorrect w.bayes in
      w.bayes <- new_w;
      normalize w

let exp_eval (rf:eval_out) (knn:eval_out) (bayes:eval_out) : weights = 
  let weights = init () in
    let _ = List.iter (fun i ->
      let cats = snd i in
      adjust (RF,cats) weights 0.0011) rf in
    let _ = List.iter (fun i ->
      let cats = snd i in
      adjust (KNN,cats) weights 0.0011) knn in
    let _ = List.iter (fun i ->
      let cats = snd i in
      adjust (Bayes,cats) weights 0.0011) bayes in
    weights

let print_weights (w: weights) : unit = 
  printf "*** Weights: knn:%f | rf:%f | bayes:%f | ***\n" w.knn w.rf w.bayes

let rec combine_results rf knn bayes w = 
  match rf, knn, bayes with
  | h1::t1, h2::t2, h3::t3 ->
      let class_prob = 
        [(snd (snd h1), w.rf);(snd (snd h2), w.knn);(snd (snd h3), w.bayes)] in
      ((fst h1), class_prob)::(combine_results t1 t2 t3 w)
  | [], [], [] -> []
  (* Should never happen since all algorithms should output results of the same
   * length *)
  | _ -> failwith "Bad data output"

