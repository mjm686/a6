open Parser
open Printf

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
  | [] -> l
  | h::t ->
      let tail = 
        if e = 0 then [] else sublist (b-1) (e-1) t in
      if b > 0 then tail else h::tail

let write_to set k =
  let k = string_of_int k in
  let test_fname = "test_fold"^k^".csv" in
  let train_fname = "train_fold"^k^".csv" in
  let test = set.test in
  let train = set.train in
  let train = List.map (fun i -> Parser.data_to_string i) train in
  let test = List.map (fun i -> Parser.data_to_string i) test in
  let open_out = open_out_gen [Open_rdonly;Open_wronly;Open_creat;Open_append] 0o666 in
  let oc_test = Csv.to_channel (open_out test_fname) in
  let oc_train = Csv.to_channel (open_out train_fname) in
  List.iter (fun i -> Csv.output_record oc_test i) test;
  List.iter (fun i -> Csv.output_record oc_train i) train

let write_all_sets dl size = 
  let counter = ref 1 in
  let rec helper start = 
    if ((start + size) > (List.length dl)) then ()
    else begin
      let start_index = start + size in
      let _ = printf "%d\n" start_index in
      let test = sublist start start_index dl in
      let latter = (sublist (start_index+1) (List.length dl) dl) in 
      let train = (sublist 0 start dl)@ latter in 
      let set = {test = test; train = train} in
      write_to set !counter;
      counter := !counter + 1;
      helper (start_index)
    end in
  helper 0

let shuffle dl k = 
  let k = float_of_int k in
  let len = float_of_int (List.length dl) in
  let test_size = int_of_float (floor (len /. k)) in
  let dl = randomize dl in
  write_all_sets dl test_size


  
