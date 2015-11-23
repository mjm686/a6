open Parser

type set = {
  train: data list;
  test: data list
}

let shuffle_data dl = 
  let nd = List.map (fun c -> (Random.bits (), c)) dl in
  let sond = List.sort compare nd in
  List.map snd sond

let rec sublist b e l = 
  match l with
  | [] -> failwith "sublist"
  | h::t ->
      let tail = 
        if e = 0 then [] else sublist (b-1) (e-1) t in
      if b > 0 then tail else h::tail

let shuffle dl k = 
  let k = float_of_int k in
  let len = float_of_int (List.length dl) in
  let test_size = floor (len /. k) in
  let dl = shuffle_data dl in
  failwith "TODO"
