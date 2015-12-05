open Core
open Csv

(* Types are exposed in .mli for other modules' implementations *)
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
type algo = | KNN | RF | Bayes
type day = | Mon | Tue | Wed | Thur | Fri | Sat | Sun | Unknown
type data = {
  id: int;
  date: Date0.t;
  ofDay: Time.Ofday.t;
  category: cat;
  dayOfWeek: day;
  pdDistrict: string;
  x: float;
  y: float 
} 

type output = int * (cat * float) list
type eval_out = (int * (cat * cat)) list

exception EOF of data list

(** [load_file f] opens the file f in stdin and returns an Csv.in_channel
 *  that will later be used in parse_train and parse_test.
 *)
val load_file : string -> Csv.in_channel

(** [parse_train] parses the csv training data into data records
 *
 *  [parse_train ic n] return [data list] that's at most [n]-element long 
 *  where ic is the in_channel returned by load_file on the intended file to 
 *  read in
 * *)
val parse_train : Csv.in_channel -> int -> data list

(** [parse_test] parses the csv testing data into data records of which all
 *  data instances' category is Undetermined
 *
 *  [parse_test ic n] return [data list] that's at most [n]-element long
 *  where ic is the in_channel returned by load_file on the intended file to 
 *  read in
 * *)
val parse_test : Csv.in_channel -> int ->  data list

(** [write_to] writes properly formatted csv records into a file.
 *
 * [write_to fname csv] writes [csv] records into file [fname]
 *)
val write_to: string -> output list -> unit

(* [data_to_string] returns a string list representation of a data record *)
val data_to_string : data -> string list

(* [cat_to_string] returns a string representation of a category *)
val cat_to_string: cat -> string
