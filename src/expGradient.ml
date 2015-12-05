open Parser

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

let normalize w = 
  let total = w.knn +. w.rf +. w.bayes in
  w.knn <- (w.knn /. total);
  w.rf <- (w.rf /. total);
  w.bayes <- (w.bayes /. total)

let expGrad param l w = 
  match l with
  | Correct -> w
  | Incorrect -> w *. (exp (-. param *. 1.0))

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
      adjust (RF,cats) weights 0.001) rf in
    let _ = List.iter (fun i ->
      let cats = snd i in
      adjust (KNN,cats) weights 0.001) knn in
    let _ = List.iter (fun i ->
      let cats = snd i in
      adjust (Bayes,cats) weights 0.001) bayes in
    weights

let print_weights (w: weights) : unit = 
  Printf.printf "*** Weights: knn:%f | rf:%f | bayes:%f | ***\n" w.knn w.rf w.bayes


