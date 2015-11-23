open Core
open Csv

type data 
type output

(** [load_file f] opens the file f in stdin and returns an Csv.in_channel
 *  that will later be used in parse_train and parse_test.
 *)
val load_file : string -> Csv.in_channel

(** [parse_train] parses the csv training data into data records
 *
 *  [parse_train ic] return [data list] where ic is the in_channel 
 *  returned by load_file on the intended file to read in
 * *)
val parse_train : Csv.in_channel -> data list

(** [parse_test] parses the csv testing data into data records of which all
 *  data instances' category is Undetermined
 *
 *  [parse_test ic] return [data list] where ic is the in_channel returned
 *  by load_file on the intended file to read in
 * *)
val parse_test : Csv.in_channel ->  data list

(** [write_to] writes properly formatted csv records into a file.
 *
 * [write_to fname csv] writes [csv] records into file [fname]
 *)
val write_to: string -> output list -> unit
