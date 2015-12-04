open Async
open Csv
open Printf
open Core

type cat = 
   | ARSON | ASSAULT | BADCHECKS | BRIBERY | BURGLARY | DISORDERLY 
   | DRIVING | DRUG | DRUNK | EMBEZZLE
 | EXTORTION | FAMILY | FORGERY | FRAUD | GAMBLING 
 | KIDNAPPING | LARCENY | LIQUOR | LOITER | MISSING 
 | NONCRIMINAL | OTHER | PORN | PROSTITUTION 
 | RECOVERED | ROBBERY | RUNAWAY | SECONDARY
 | SEXOFFENSESF | SEXOFFENSESNF | STOLEN
 | SUICIDE | SUSPICIOUS | TREA | TRESPASS | VANDALISM | VEHICLE 
 | WARRANTS | WEAPON | UNDETERMINED | UNRECOGNIZED

type data = {
  id: int;
  date: Date0.t;
  ofDay: Time.Ofday.t;
  category: cat;
  dayOfWeek: day;
  pdDistrict: string;
  x: float;
  y: float 
} and day = | Mon | Tue | Wed | Thur | Fri | Sat | Sun | Unknown

type output = int * (cat*float) list

exception BadData
exception EOF of data list
exception InvalidInput of output 

let classes = 
  [ARSON;ASSAULT;BADCHECKS;BRIBERY;BURGLARY;DISORDERLY 
  ;DRIVING;DRUG;DRUNK;EMBEZZLE
;EXTORTION;FAMILY;FORGERY;FRAUD;GAMBLING 
;KIDNAPPING;LARCENY;LIQUOR;LOITER;MISSING 
;NONCRIMINAL;OTHER;PORN;PROSTITUTION 
;RECOVERED;ROBBERY;RUNAWAY;SECONDARY
;SEXOFFENSESF;SEXOFFENSESNF;STOLEN
;SUICIDE;SUSPICIOUS;TREA;TRESPASS;VANDALISM;VEHICLE 
;WARRANTS;WEAPON]

let load_file fname = 
  let ic = Csv.of_channel (open_in fname) in
  (* To skip over the first line, which is the field names *)
  ic

let parse_cat s = 
  match s with 
  | "ARSON" -> ARSON
  | "ASSAULT" -> ASSAULT
  | "BAD CHECKS" -> BADCHECKS
  | "BRIBERY" -> BRIBERY
  | "BURGLARY" -> BURGLARY
  | "DISORDERLY CONDUCT" -> DISORDERLY
  | "DRIVING UNDER THE INFLUENCE" -> DRIVING
  | "DRUG/NARCOTIC" -> DRUG
  | "DRUNKENNESS" -> DRUNK
  | "EMBEZZLEMENT" -> EMBEZZLE
  | "EXTORTION" -> EXTORTION
  | "FAMILY OFFENSES" -> FAMILY
  | "FORGERY/COUNTERFEITING" -> FORGERY
  | "FRAUD" -> FRAUD
  | "GAMBLING" -> GAMBLING
  | "KIDNAPPING" -> KIDNAPPING
  | "LARCENY/THEFT" -> LARCENY
  | "LIQUOR LAWS" -> LIQUOR
  | "LOITERING" -> LOITER 
  | "MISSING PERSON" -> MISSING
  | "NON-CRIMINAL" -> NONCRIMINAL
  | "OTHER OFFENSES" -> OTHER
  | "PORNOGRAPHY/OBSCENE MAT" -> PORN
  | "PROSTITUTION" -> PROSTITUTION
  | "RECOVERED VEHICLE" -> RECOVERED
  | "ROBBERY" -> ROBBERY
  | "RUNAWAY" -> RUNAWAY
  | "SECONDARY CODES" -> SECONDARY
  | "SEX OFFENSES FORCIBLE" -> SEXOFFENSESF
  | "SEX OFFENSES NON FORCIBLE" -> SEXOFFENSESNF
  | "STOLEN PROPERTY" -> STOLEN
  | "SUICIDE" -> SUICIDE
  | "SUSPICIOUS OCC" -> SUSPICIOUS
  | "TREA" -> TREA
  | "TRESPASS" -> TRESPASS
  | "VANDALISM" -> VANDALISM
  | "VEHICLE THEFT" -> VEHICLE
  | "WARRANTS" -> WARRANTS
  | "WEAPON LAWS" -> WEAPON
  | _ -> UNRECOGNIZED

let parse_day s = 
  match s with
  | "Monday" -> Mon
  | "Tuesday" -> Tue
  | "Wednesday" -> Wed
  | "Thursday" -> Thur
  | "Friday" -> Fri
  | "Saturday" -> Sat
  | "Sunday" -> Sun
  | _ -> Unknown

let cat_to_string c = 
  match c with 
  | ARSON -> "ARSON"
  | ASSAULT -> "ASSAULT"
  | BADCHECKS -> "BAD CHECKS"
  | BRIBERY -> "BRIBERY"
  | BURGLARY -> "BURGLARY"
  | DISORDERLY -> "DISORDERLY CONDUCT"
  | DRIVING -> "DRIVING UNDER THE INFLUENCE"
  | DRUG -> "DRUG/NARCOTIC"
  | DRUNK -> "DRUNKENNESS"
  | EMBEZZLE -> "EMBEZZLEMENT"
  | EXTORTION -> "EXTORTION"
  | FAMILY -> "FAMILY OFFENSES"
  | FORGERY -> "FORGERY/COUNTERFEITING"
  | FRAUD -> "FRAUD"
  | GAMBLING -> "GAMBLING"
  | KIDNAPPING -> "KIDNAPPING"
  | LARCENY -> "LARCENY/THEFT"
  | LIQUOR -> "LIQUOR LAWS"
  | LOITER -> "LOITERING"
  | MISSING -> "MISSING PERSON"
  | NONCRIMINAL -> "NON-CRIMINAL"
  | OTHER -> "OTHER OFFENSES"
  | PORN -> "PORNOGRAPHY/OBSCENE MAT"
  | PROSTITUTION -> "PROSTITUTION"
  | RECOVERED -> "RECOVERED VEHICLE"
  | ROBBERY -> "ROBBERY"
  | RUNAWAY -> "RUNAWAY"
  | SECONDARY -> "SECONDARY CODES"
  | SEXOFFENSESF -> "SEX OFFENSES FORCIBLE"
  | SEXOFFENSESNF -> "SEX OFFENSES NON FORCIBLE"
  | STOLEN -> "STOLEN PROPERTY"
  | SUICIDE -> "SUICIDE"
  | SUSPICIOUS -> "SUSPICIOUS OCC"
  | TREA -> "TREA"
  | TRESPASS -> "TRESPASS"
  | VANDALISM -> "VANDALISM"
  | VEHICLE -> "VEHICLE THEFT"
  | WARRANTS -> "WARRANTS"
  | WEAPON -> "WEAPON LAWS"
  | UNRECOGNIZED -> "UNRECOGNIZED"
  | UNDETERMINED -> "UNDETERMINED"

let day_to_string d = 
  match d with
  | Mon -> "Monday"
  | Tue -> "Tuesday"
  | Wed -> "Wednesday"
  | Thur -> "Thursday"
  | Fri -> "Friday"
  | Sat -> "Saturday"
  | Sun -> "Sunday"
  | Unknown -> "Unknown"

let print_single d = 
  let open Format in
  let format_helper f d =
    fprintf f "%a %a %s %s %s %f %f\n" Date0.pp d.date Time.Ofday.pp d.ofDay (cat_to_string d.category) (day_to_string d.dayOfWeek) d.pdDistrict d.x d.y in
  printf "Id %d %a" d.id format_helper d

let float_helper s = 
  if String.length s < 12 then String.length s
  else 12

let parse_single ?test:(test=false) id ls =
  let time = Time.of_string (List.hd ls) in
  let (date, of_day) = Time.to_date_ofday time (Time.Zone.local) in
  let cat = 
    if test then UNDETERMINED 
    else parse_cat (List.nth ls 1) in
  let day_of_week = parse_day (List.nth ls 3) in
  let district = List.nth ls 4 in 
  let x_s = List.nth ls 7 in
  let y_s = List.nth ls 8 in
  let x = try float_of_string (String.sub x_s 0 (float_helper x_s)) with
     | Failure _  -> 0.0
   in
  let y = try float_of_string (String.sub y_s 0 (float_helper y_s)) with
    | Failure _ -> 0.0
   in
  let d = {
    id = id;
    date = date;
    ofDay = of_day;
    category = cat;
    dayOfWeek = day_of_week;
    pdDistrict = district;
    x = x;
    y = y 
  } in
  d

let parse_test_single ls =
  let id = int_of_string (List.hd ls) in
  let time = Time.of_string (List.nth ls 1) in
  let (date, of_day) = Time.to_date_ofday time (Time.Zone.local) in
  let day_of_week = parse_day (List.nth ls 2) in
  let district = List.nth ls 3 in 
  let x_s = List.nth ls 5 in
  let y_s = List.nth ls 6 in
  let x = try float_of_string (String.sub x_s 0 (float_helper x_s)) with
     | Failure _  -> 0.0
   in
  let y = try float_of_string (String.sub y_s 0 (float_helper y_s)) with
    | Failure _ -> 0.0
   in
  let d = {
    id = id;
    date = date;
    ofDay = of_day;
    category = UNDETERMINED;
    dayOfWeek = day_of_week;
    pdDistrict = district;
    x = x;
    y = y 
  } in
  let _ = printf "%d\n" d.id in
  d

let parse_fold_single ls =
  let id = int_of_string (List.hd ls) in
  let date = Date0.of_string_iso8601_basic (List.nth ls 1) 0 in
  let of_day = Time.Ofday.of_string (List.nth ls 2) in
  let cat = parse_cat (List.nth ls 3) in
  let day_of_week = parse_day (List.nth ls 4) in
  let district = List.nth ls 5 in 
  let x = try float_of_string (List.nth ls 6) with
    | Failure _ -> raise BadData in
  let y = try float_of_string (List.nth ls 7) with
    | Failure _ -> raise BadData in
  let d = {
    id = id;
    date = date;
    ofDay = of_day;
    category = cat;
    dayOfWeek = day_of_week;
    pdDistrict = district;
    x = x;
    y = y 
  } in
  Some d

let compare_data d1 d2 =
  let d1 = cat_to_string d1.category in
  let d2 = cat_to_string d2.category in
  String.compare d1 d2

let counter1 = ref 0
(*let id = ref 0*)
let parse ic = 
  let rec helper (acc: data list) : data list =
    if !counter1 >= 100000 then 
      let _ = counter1 := 0 in
      acc
      (*(List.sort compare_data acc) *)
    else begin
      let next_ic = try (Csv.next ic) with
        | End_of_file -> 
            Csv.close_in ic; 
            (*let acc = List.sort compare_data acc in*)
            raise (EOF acc);
        | Csv.Failure (n1,n2,s) ->
           (* if any record fails to be in csv format, skip *) 
            printf "failed at field %d line %d because %s\n" n1 n2 s;
            Csv.next ic in
      (*id := !id + 1;*)
      counter1 := !counter1 + 1;
      let d = next_ic in
      helper ((parse_test_single d)::acc)
    end
  in let data = helper [] in
  (*List.sort compare_data data*)
  data

let get_next ic acc = 
  try (Csv.next ic) with
    | End_of_file -> 
        Csv.close_in ic; 
        let acc = List.sort compare_data acc in
        raise (EOF acc);
    | Csv.Failure (n1,n2,s) ->
       (* if any record fails to be in csv format, skip *) 
        printf "failed at field %d line %d because %s\n" n1 n2 s;
        Csv.next ic

let counter2 = ref 0
let parse_fold ic = 
  let rec helper (acc: data list) : data list =
    if !counter2 >= 10000 then 
      let _ = counter2 := 0 in
      (List.sort compare_data acc) 
    else
      let _ = counter2 := !counter2 + 1 in
      let d = try get_next ic acc with
        | EOF acc -> raise (EOF acc) in
      let d = try parse_fold_single d with
        | BadData -> None in
      match d with 
        | None -> helper acc
        | Some d -> helper (d::acc) in
  let data = helper [] in
  List.sort compare_data data

let print_all data = 
  List.iter print_single data

(* ls is an associative list of class and prob (float) *)
let rec format_output_helper classes ls = 
  match classes with
  | [] -> []
  | h::t ->
      let p = try List.assoc h ls with
      | Not_found -> 0.0 in
      let p = 
        if p = 0.0 then string_of_int (int_of_float p) 
        else string_of_float p in
      p::(format_output_helper t ls)  

(* formats an output list into string list list *)
let rec format_output outputs = 
  match outputs with
  | [] -> []
  | h::t -> begin
    let id = string_of_int (fst h) in
    let tl = format_output_helper classes (snd h) in
    (id::tl)::(format_output t)
  end

(* Functions in mli implemented *)
let parse_test ic = parse ic

let parse_train ic = parse_fold ic

let write_to fname csv = 
  let csv = format_output csv in
  let oc = Csv.to_channel (open_out fname) in
  List.iter (fun i -> Csv.output_record oc i) csv

let data_to_string (d: data) : string list = 
  let id = string_of_int d.id in
  let date = Date0.to_string_iso8601_basic d.date in
  let ofDay = Time.Ofday.to_string d.ofDay in
  let category = cat_to_string d.category in
  let day = day_to_string d.dayOfWeek in
  let x = string_of_float d.x in
  let y = string_of_float d.y in
  [id;date;ofDay;category;day;d.pdDistrict;x;y]

(*let example = [(1,[(ARSON, 0.1);(ASSAULT, 0.57123);(BRIBERY, 0.836324)]);(3, [(TREA, 0.8);(WEAPON, 0.3844514);(OTHER, 0.13)])] in
write_to "test.csv" example*)

(*let ic = load_file "../data/test.csv" in
let rec example_parse_test acc =
  let dl = try parse_test ic with
  | EOF dl -> 
      let acc = dl@acc in 
      let _ = raise (EOF acc) in
      acc in
  try example_parse_test dl with
  | EOF dl -> dl in
example_parse_test []*)

(*let ic = load_file "train_fold1.csv" in
parse_train ic*)

let ic = load_file "train_fold2.csv" in
let rec example_parse_train acc =
  let dl = try parse_fold ic with
  | EOF dl -> 
      let acc = dl@acc in 
      let _ = raise (EOF acc) in
      acc in
  try example_parse_train dl with
  | EOF dl -> dl in
example_parse_train []

