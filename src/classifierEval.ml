open CrossValid
open Parser

let ic = load_file "../data/train.csv" in
let d = try parse_train ic with 
| EOF d -> d in
let _ = Printf.printf "total %d\n" (List.length d) in
shuffle d 1

let eval dl = failwith "TODO"
