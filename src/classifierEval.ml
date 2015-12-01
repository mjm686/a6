open CrossValid
open Parser

exception End
exception EndWithData

let cross_valid () = 
  let ic = load_file "../data/train.csv" in
  let rec helper ic = 
    let d = try parse_train ic with 
    | EOF d -> shuffle d 4; raise End in
    let _ = Printf.printf "total %d\n" (List.length d) in
    shuffle d 4;
    try helper ic with 
    | End -> () in
  helper ic 

let ic = load_file "test_fold4.csv" in
let dl = parse_train ic in
Printf.printf "total %d\n" (List.length dl)

let eval dl = failwith "TODO"
