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

let load_file fname = 
  Csv.load fname

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

let parse_single ?test:(test=false) id ls =
  (*let id = int_of_string (List.hd ls) in*)
  let time = Time.of_string (List.hd ls) in
  let (date, of_day) = Time.to_date_ofday time (Time.Zone.local) in
  let cat = 
    if test then UNDETERMINED 
    else parse_cat (List.nth ls 1) in
  let day_of_week = parse_day (List.nth ls 3) in
  let district = List.nth ls 4 in 
  let x = float_of_string (String.sub (List.nth ls 7) 0 12) in
  let y = float_of_string (String.sub (List.nth ls 8) 0 12) in
  let d =
  {
    id = id;
    date = date;
    ofDay = of_day;
    category = cat;
    dayOfWeek = day_of_week;
    pdDistrict = district;
    x = x;
    y = y 
  } in
  let () = print_single d in
  d

let parse_train fname = 
  let csv = load_file fname in
  let counter = ref 0 in
  List.map (fun i -> counter:= !counter + 1; parse_single !counter i) (List.tl csv)

let parse_test fname = 
  let csv = load_file fname in
  let counter = ref 0 in
  List.map (fun i -> counter:= !counter + 1; parse_single !counter i ~test:true) (List.tl csv)

let print_all data = 
  List.iter print_single data

let _ = parse_train "../data/train.csv"


