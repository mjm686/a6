open Parser

type algo = | KNN | RF | Bayes

type prediction = (algo * cat * cat)

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

let adjust (p: prediction) w param = 
  match p with
  | (KNN, correct, guess) ->
      let new_w = 
        if correct = guess then expGrad param Correct w.knn
        else expGrad param Incorrect w.knn in
      w.knn <- new_w;
      normalize w
  | (RF, correct, guess) ->
      let new_w =
        if correct = guess then expGrad param Correct w.rf
        else expGrad param Incorrect w.rf in
      w.rf <- new_w;
      normalize w
  | (Bayes, correct, guess) ->
      let new_w = 
        if correct = guess then expGrad param Correct w.bayes
        else expGrad param Incorrect w.bayes in
      w.bayes <- new_w;
      normalize w

