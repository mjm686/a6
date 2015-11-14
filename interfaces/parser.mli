open Core
open Csv

type data = {
  date: Time.t;
  category: cat;
  dayOfWeek: day;
  pdDistrict: string;
  x: float;
  y: float 
}
= cat = 
   | ARSON | ASSAULT | BAD CHECKS | BRIBERY | BURGLARY | DISORDERLY CONDUCT 
   | DRIVING UNDER THE INFLUENCE | DRUGNARCOTIC | DRUNKENNESS | EMBEZZLEMENT
 | EXTORTION | FAMILY OFFENSES | FORGERYCOUNTERFEITING | FRAUD | GAMBLING 
 | KIDNAPPING | LARCENYTHEFT | LIQUOR LAWS | LOITERING | MISSING PERSON 
 | NONCRIMINAL | OTHER OFFENSES | PORNOGRAPHYOBSCENE MAT | PROSTITUTION 
 | RECOVERED VEHICLE | ROBBERY | RUNAWAY | SECONDARY CODES 
 | SEX OFFENSES FORCIBLE | SEX OFFENSES NON FORCIBLE | STOLEN PROPERTY 
 | SUICIDE | SUSPICIOUS OCC | TREA | TRESPASS | VANDALISM | VEHICLE THEFT 
 | WARRANTS | WEAPON LAWS | Undetermined
= day = | Mon | Tue | Wed | Thur | Fri | Sat | Sun

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
