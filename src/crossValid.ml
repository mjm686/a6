open Parser


type set = {
  train: data list;
  test: data list
}

let randomize dl = 
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

let write_to set k =
  let k = string_of_int k in
  let test_fname = "test_fold"^k in
  let train_fname = "train_fold"^k in
  let test = set.test in
  let train = set.train in
  let train = List.map (fun i -> Parser.data_to_string i) train in
  let test = List.map (fun i -> Parser.data_to_string i) test in
  let oc_test = Csv.to_channel (open_out test_fname) in
  let oc_train = Csv.to_channel (open_out train_fname) in
  Csv.output_all oc_test test;
  Csv.output_all oc_train train

let write_all_sets dl size = 
  let counter = ref 1 in
  let rec helper start = 
    if ((start + size) > (List.length dl)) then ()
    else begin
      let start_index = start + size in
      let test = sublist start start_index dl in
      let latter = (sublist (start_index+1) (List.length dl) dl) in 
      let train = (sublist 0 start dl)@ latter in 
      let set = {test = test; train = train} in
      write_to set !counter;
      counter := !counter + 1;
      helper (start_index + 1)
    end in
  helper 0

let shuffle dl k = 
  let k = float_of_int k in
  let len = float_of_int (List.length dl) in
  let test_size = floor (len /. k) in
  let dl = randomize dl in
  write_all_sets dl (int_of_float test_size);
  ()


  
