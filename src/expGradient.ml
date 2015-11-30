open Parser

type prediction = (algo * cat * cat)
and algo = | KNN | RF | Bayes

type weights = {
  knn: float;
  rf: float;
  bayes: float
}

type param = int

type loss = Correct | Incorrect

let init () = failwith "TODO"

let adjust p w param = failwith "TODO"
