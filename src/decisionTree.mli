open Parser
open Dtree

(**
 * [timeFirstTree str] returns a new decision tree with the
 * nodes representing the times incidents occurred at minimum
 * depth.
 *)
val timeFirstTree : bytes -> tree

(**
 * [locFirstTree str] returns a new decision tree with the
 * nodes representing the locations incidents occurred at
 * minimum depth.
 *)
val locFirstTree : bytes -> tree

(**
 * [complexity str] returns a float representing the entropy
 * of a certain tree.
 *
 * The entropy is a measure of the disorder of a tree, or the
 * amount of information which must be stored. Entropy will be
 * calculated via the technique described on slide 14 of this
 * webpage:
 * http://www.ke.tu-darmstadt.de/lehre/archiv/ws0809/mldm/dt.pdf.
 *)
val complexity : bytes -> float

(**
 * [trainingTree str] returns a new decision tree created with
 * the training data.
 *
 * The tree will either use a location-first or time-first
 * method, first meaning minimum depth, based on which option
 * has the lowest entropy as calulated in [complexity str].
 * This tree can then be used to predict the category of crimes
 * given time and location from the testing data.
 *)
val trainingTree : bytes -> tree

(**
 * [predictions str tr] returns a string containing the
 * predictions of crime types based on location and time.
 *
 * The tree created in [trainingTree str] and the testing
 * data as parsed by Parser will be used to make the
 * predictions and output a list of predicted crime types.
 *)
val predictions : bytes -> tree -> bytes
