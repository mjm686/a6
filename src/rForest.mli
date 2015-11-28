open Core
open Parser

type attr =
	| Time of int
  | X of float
  | Y of float
  | Day of day
and day = Mon | Tue | Wed | Thu | Fri | Sat | Sun | Unk

type tree =
	| Leaf of leaf
	| Node of node
and node = {t:tree list; a:attr}
and leaf = {c:cat; mutable i:int}

(**
 * [makeLeaves l] returns a list of Leafs.
 *
 * The Leafs all begin with value i=0 and each Leaf
 * contains one category from l.
 *)
val makeLeaves : cat list -> tree list

(**
 * [makeNodes1 fl cl] returns a list of Nodes.
 *
 * Every Node in the list has every leaf from
 * [makeLeaves cl] listed in Node.t and one value of
 * fl is stored in each Node.
 *)
val makeNodes1 : float list -> cat list -> tree list

(**
 * [makeNodes2 fl1 fl2 cl] returns a list of Nodes.
 *
 * Every Node in the list has every Node from
 * [makeNodes1 fl2 cl] listed in Node.t and one value of
 * fl1 is stored in each Node.
 *)
val makeNodes2 : float list -> float list ->
 cat list -> tree list

(**
 * [makeNodes3 il fl1 fl2 cl] returns a list of Nodes.
 *
 * Every Node in the list has every Node from
 * [makeNodes2 fl1 fl2 cl] listed in Node.t and one
 * value of il is stored in each Node.
 *)
val makeNodes3 : int list -> float list ->
float list -> cat list -> tree list

(**
 * [makeNodes dl il fl1 fl2 cl] returns a list of
 * Nodes.
 *
 * Every Node in the list has every Node from
 * [makeNodes3 il fl1 fl2 cl] listed in Node.t and
 * one value of dl is stored in each Node.
 *)
val makeNodes : day list -> int list ->
float list -> float list -> cat list -> tree list

(**
 * [hollowForest ()] returns a list of trees.
 *
 * The trees are made with the structure Day Node ->
 * Time Node -> X Node -> Y Node -> cat Leaf and
 * the trees are made using the lists defined
 * in dTree.ml which contain all values for these
 * attributes and categories of crimes. Each leaf
 * has i set to 0 initially.
 *
 * The list is essentially a "blank" random forest.
 *)
val hollowForest : unit -> tree list

(**
 * [randomForest datl] creates a list of trees from data
 * list datl.
 *
 * Each leaf node is initially set up such that each
 * i = 0; the data in datl is then used to update each
 * i based on how many times a certain crime type
 * occurred at a specific location and time.
 *)
val randomForest : data list -> tree list
