open Core
open Csv

type data 

(** [parse_train] parses the csv training data into data records
 *
 *  [parse_train s] return [data list] where s is the file name to read from 
 * *)
val parse_train : string -> data list

(** [parse_test] parses the csv testing data into data records of which all
 *  data instances' category is Undetermined
 *
 *  [parse_test s] return [data list] where s is the file name to read from 
 * *)
val parse_test : string -> data list
