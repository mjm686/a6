open Async
open Csv
open Printf
open Core

type data = {
  id: int;
  date: Date0.t;
  ofDay: Time.Ofday.t;
  category: cat;
  dayOfWeek: day;
  pdDistrict: string;
  x: float;
  y: float 
} and cat = 
   | ARSON | ASSAULT | BADCHECKS | BRIBERY | BURGLARY | DISORDERLY 
   | DRIVING | DRUG | DRUNK | EMBEZZLE
 | EXTORTION | FAMILY | FORGERY | FRAUD | GAMBLING 
 | KIDNAPPING | LARCENY | LIQUOR | LOITER | MISSING 
 | NONCRIMINAL | OTHER | PORN | PROSTITUTION 
 | RECOVERED | ROBBERY | RUNAWAY | SECONDARY
 | SEXOFFENSESF | SEXOFFENSESNF | STOLEN
 | SUICIDE | SUSPICIOUS | TREA | TRESPASS | VANDALISM | VEHICLE 
 | WARRANTS | WEAPON | UNDETERMINED | UNRECOGNIZED
and day = | Mon | Tue | Wed | Thur | Fri | Sat | Sun | Unknown

type output = float list

exception EOF of data list
exception InvalidInput of output 

let load_file fname = 
  let ic = open_in fname in
  let ic = Csv.of_channel ic in
  (* To skip over the first line, which is the field names *)
  let _ = Csv.next ic in
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
  printf "Row %d %a" d.id format_helper d

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

let compare_data d1 d2 =
  let d1 = cat_to_string d1.category in
  let d2 = cat_to_string d2.category in
  String.compare d1 d2

let parse ic test = 
  let id = ref 0 in
  let rec helper (acc: data list) : data list =
    let next ic = try (Csv.next ic) with
      | End_of_file -> 
          Csv.close_in ic; 
          let acc = List.sort compare_data acc in
          raise (EOF acc);
      | Csv.Failure (n1,n2,s) ->
         (* if any record fails to be in csv format, skip *) 
          printf "failed at field %d line %d because %s\n" n1 n2 s;
          Csv.next ic in
    id := !id + 1;
    let d = next ic in
    helper ((parse_single !id d ~test:test)::acc)
  in let data = helper [] in
  List.sort compare_data data

let print_all data = 
  List.iter print_single data

let rec stringify csv = 
  match csv with
  | [] -> []
  | h::t -> begin
    let s_h = 
      match h with
      | [] -> raise (InvalidInput h)
      | h1::t1 -> (string_of_int (int_of_float h1))::
        (List.map (fun f -> string_of_float f) t1) in
    s_h::(stringify t)
  end

(* Functions in mli implemented *)
let parse_test ic = parse ic true

let parse_train ic = parse ic false

let write_to fname csv = 
  let csv = stringify csv in
  let oc = open_out fname in
  let oc = Csv.to_channel oc in
  Csv.output_all oc csv

let data_to_string (d: data) : string list = 
  let date = Date0.to_string_american d.date in
  let ofDay = Time.Ofday.to_string d.ofDay in
  let category = cat_to_string d.category in
  let day = day_to_string d.dayOfWeek in
  let x = string_of_float d.x in
  let y = string_of_float d.y in
  [date;ofDay;category;day;d.pdDistrict;x;y]

let ic = load_file "../data/train.csv" in
let d = try parse_train ic with 
| EOF d -> d in
printf "%d\n" (List.length d);
print_all d;
