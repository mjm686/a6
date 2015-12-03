open Core
open Parser

type attr =
	| Time of int
  | X of float
  | Y of float
  | Day of day

type tree =
	| Leaf of leaf
	| Node of node
and node = {t : tree list; a : attr}
and leaf = {c : cat; mutable i : int}

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
val makeNodes1 : float list -> cat list
-> tree list

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
 *
 * The previous makeNodes functions are meant
 * only to be helper functions for [makeNodes].
 *)
val makeNodes : data list -> int list ->
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
 * [leafBool categ t] returns a boolean.
 *
 * leafBool returns true if categ is equivalent to the
 * category stored in Leaf t; if t is a Node, leafBool
 * returns false.
 *)
val leafBool : cat -> tree -> bool

(**
 * [nodeBool att t] returns a boolean.
 *
 * nodeBool returns true if att is equivalent to the
 * attribute stored in Node t; if t is a Leaf,
 * nodeBool returns false.
 *)
val nodeBool : attr -> tree -> bool

(**
 * [nodeListGet n] returns a tree list.
 *
 * nodeListGet returns the tree list stored in
 * Node n (n.t); if n is a Leaf,
 * nodeListGet returns an empty list.
 *)
val nodeListGet : tree -> tree list

(**
 * [editLeaf lf] increments the i value of a
 * Leaf lf by 1; if lf is a Node, editLeaf
 * does nothing.
 *)
val editLeaf : tree -> unit

(**
 * [editTree1 categ tl] increments the i value of
 * a Leaf in a tree list by one.
 *
 * The i value changed is the value stored in the
 * Leaf corresponding to category categ.
 *)
val editTree1 : cat -> tree list -> unit

(**
 * [editTree2 aY categ tl] increments the i value of
 * a Leaf in a tree list by one.
 *
 * The i value changed is the value stored in the
 * Leaf corresponding to category categ, whose
 * parent node contains attribute aY; this Node
 * containing aY must be in list tl.
 *)
val editTree2 : attr -> cat -> tree list -> unit

(**
 * [editTree3 aX aY categ tl] increments the i
 * value of a Leaf in a tree list by one.
 *
 * The i value changed is the value stored in the
 * Leaf corresponding to category categ, whose
 * parent node contains attribute aY, whose
 * parent node contains attribute aX; this Node
 * containing aX must be in list tl.
 *)
val editTree3 : attr -> attr -> cat -> tree list
-> unit

(**
 * [editTree4 aTime aX aY categ tl] increments the
 * i value of a Leaf in a tree list by one.
 *
 * The i value changed is the value stored in the
 * Leaf corresponding to category categ, whose
 * parent node contains attribute aY, whose
 * parent node contains attribute aX, whose
 * parent node contains attribute aTime; this Node
 * containing aTime must be in list tl.
 *)
val editTree4 : attr -> attr -> attr -> cat
-> tree list -> unit

(**
 * [editTree aDay aTime aX aY categ tl] increments
 * the i value of a Leaf in a random forest by one.
 *
 * The i value changed is the value stored in the
 * Leaf corresponding to category categ whose
 * parent node contains attribute aY, whose
 * parent node contains attribute aX, whose
 * parent node conatins attribute aTime, whose
 * parent node contains attribute aDay; this Node
 * containing aDay must be in list tl.
 *
 * This function is the basis for editing
 * trees in the random forest based on training
 * data. The other editTree functions are helpers
 * for [editTree].
 *)
val editTree : attr -> attr -> attr -> attr
-> cat -> tree list -> unit

(**
 * [ofDayToTime ti] creates a Time attribute from a
 * Core.Time.Ofday.t data type.
 *)
val ofDayToTime : Time.Ofday.t -> attr

(**
 * [xConvert fl] creates an X attribute from a
 * float.
 *)
val xConvert : float -> attr

(**
 * [yConvert fl] creates a Y attribute from a
 * float.
 *)
val yConvert : float -> attr

(**
 * [updateTree dat tl] edits a Leaf, as in [editTree],
 * based on the information stored in data.
 *)
val updateTree : Parser.data -> tree list -> unit

(**
 * [randomForest datl tl] fills a random forest tl
 * with data from datl.
 *
 * tl should be a blank random forest (created via
 *[hollowForest ()]); this forest will then be updated
 * to account for the data in datl.
 *)
val randomForest : data list -> tree list 
-> tree list
