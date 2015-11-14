(* a prediction is consisted of the [algo] that it's produced by, 
 * the class predicted, and the correct class *)
type prediction = (algo * cat * cat)
= algo = KNN | DT | Bayes
= cat = | ARSON | ASSAULT | BAD CHECKS | BRIBERY | BURGLARY | DISORDERLY CONDUCT 
   | DRIVING UNDER THE INFLUENCE | DRUGNARCOTIC | DRUNKENNESS | EMBEZZLEMENT
 | EXTORTION | FAMILY OFFENSES | FORGERYCOUNTERFEITING | FRAUD | GAMBLING 
 | KIDNAPPING | LARCENYTHEFT | LIQUOR LAWS | LOITERING | MISSING PERSON 
 | NONCRIMINAL | OTHER OFFENSES | PORNOGRAPHYOBSCENE MAT | PROSTITUTION 
 | RECOVERED VEHICLE | ROBBERY | RUNAWAY | SECONDARY CODES 
 | SEX OFFENSES FORCIBLE | SEX OFFENSES NON FORCIBLE | STOLEN PROPERTY 
 | SUICIDE | SUSPICIOUS OCC | TREA | TRESPASS | VANDALISM | VEHICLE THEFT 
 | WARRANTS | WEAPON LAWS | Undetermined

(* record type to keep track of the weights of algorithms
 * all weights sum up to 1 always *)
type weights = {
  knn: float;
  dt: float;
  bayes: float
}

type param = int

type loss = Correct of 0 | Incorrect of 1

(** [init] initiates a set of equal weights that sum to 1 for all algorithms
 *
 *  [init ()] returns a weights record
 * *)
val init : unit -> weights

(** [adjust] tweeks the [weights] of algorithms based on the [prediction] given
 *  [param] is just a constant that can be modified in the EG algoirthm that
 *  we might need when we are dealing with the entire system's performance
 *
 *  [adjust p w param] returns a new set of weights
 * *)
val adjust : prediction -> weights -> param -> weights
