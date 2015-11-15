open Core
open Parser 

type attr = 
	| Date | Day | District | X | Y

type val = 
	| Float of float
	| Day of day
	| Date of Time.t
	| String of string
	= day = Mon | Tue | Wed | Thur | Fri | Sat | Sun

type tree = 
	| Leaf of cat
	| Node of node
= node = (tree list * attr * val * data)

(**
 * [newTree] creates a new tree.
 *
 * The new tree initially has no nodes.
 *)
val newTree : unit -> tree

(**
 * [newNode dat] creates a new node.
 *
 * The new node contains the data dat.
 *)
val newNode : 'a -> node

(**
 * [add nd tr] adds a node nd to tree tr (returns a new tree).
 *
 * add should place the node in the proper level of the tree; e.g.,
 * if the format is time nodes are parents to location nodes which
 * are parents to crime type nodes, a crime type node should be
 * added at a depth of 3.
 *)
val add  : node -> tree -> tree

(**
 * [check nd tr] determines whether or not a node nd is part of
 * a tree tr.
 *
 * Will be used to determine if a node should be added; if it
 * is already present, its weight will instead increase by one.
 *)
val check : node -> tree -> boolean

(**
 * [weightUp nd] adds one to the weight of a leaf node nd.
 *
 * In order to predict crime type, the tree will keep track of
 * how often crime types occurred in each location; it will
 * predict the crime types in the testing data by referring to
 * the crime type nodes with the greatest weight corresponding
 * to the time and location of the incident.
 *)
val weightUp : node -> unit

(**
 * [heaviest nd1 nd2] returns the heaviest node along the branch
 * corresponding to nodes nd1 and nd2.
 *
 * That is, it returns a crime type node which, based on the
 * training data, has the greatest weight and is thus the most
 * likely to have occurred given a node nd1 which specifies
 * location and a node nd2 which specifies time of the incident.
 *)
val heaviest : node -> node -> node

(**
 * [nodeValue nd] returns the data stored in node nd.
 *
 * Depending on how the nodes are implemented, this may not
 * strictly be necessary.
 *)
val nodeValue : node -> 'a

(**
 * [treeString tr] returns a string representing the tree tr.
 *
 * The string should indicate what depth each node is at,
 * what data is in each node, and the weights of the
 * crime type nodes.
 *)
val treeString : tree -> bytes

(**
 * [nodeString nd] returns a string representing the node nd.
 *
 * The string should indicate the weight and value stored
 * within the node, as well as the number of leaf nodes (if any).
 *)
val nodeString : node -> bytes

(**
 * [numLeaf nd] returns the number of leaf nodes of nd, if any.
 *)
val numLeaf : node -> int

(**
 * [weight nd] returns the weight of node nd.
 *)
val weight : node -> int

(**
 * I'm still not sure what the best way to represent
 * trees and nodes is; I was thinking perhaps a list
 * or array method, but I'm not sure that they would be
 * the best methods to do so. Also, if there are any
 * suggestions you have for this so far, let me know,
 * I would appreciate the help.
 *)