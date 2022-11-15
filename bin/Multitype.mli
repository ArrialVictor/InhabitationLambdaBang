		(*** Multitypes ***)

type typ =
  	| TVar of int
  	| TArr of typ * typ
  	| TMlt of typ list
  	| THole


		(*** Comparaison and Matching ***)

val typComp 	: typ -> typ -> int
	(* Given two types 't1' and 't2', this function returns -1 if
	't1' is greater than 't2', 0 in case of equality and 1 if
	't1' is smaller than 't2'. *)

val size 		: typ -> int
	(* Given a type 't', this function return the size of the type. *)


		(*** Constructor constants and functions ***)

val tHole 		: typ
	(* This function returns a type hole. *)

val tVar 		: int -> typ
	(* Given an integer 'i', this function returns a type variable
	with identifier 'i'. *)

val tArrow 		: typ -> typ -> typ
	(* Given two types 't1' and 't2', this function returns an arrow
	type from 't1' to 't2'. *)

val tEmpty 		: typ
	(* This function returns an empty intersection type. *)

val tSingleton 	: typ -> typ
	(* Given a type 't', this function returns and singleton (intersection
	of a single type) of the given type 't'. *)

val tMultiset 	: typ list -> typ
	(* Given a list of types 'ls', this function returns the intersection
	type whose types are the elements of the given list 'ls'. *)

val add_opt 	: typ -> typ -> typ option
	(* Given a multitype 'M' and a type 't', this function returns an
	option value of an intersection type containing the same types as
	'M' and the additional type 't'.
	If the type 'M' is not an intersection, then the function returns None. *)

val ( ++ ) 		: typ -> typ -> typ
	(* This notation is the unsafe variant of add_opt *)

val merge_opt 	: typ -> typ -> typ option
	(* Given two intersections 'M1' and 'M2', this function returns an
	option value of an intersection type containing the types of 'M1'
	and 'M2'.
	If either 'M1' of 'M2' are not intersections then the function returns
	None. *)

val ( |+| ) 	: typ -> typ -> typ
	(* This notation is the unsafe variant of merge_opt. *)



	
		(*** Splittings ***)

val pickDirectSubtype 	: typ -> (typ * typ) list
	(* Given a type 't', this function returns a list of all pairs of
	a direct subtypes of 't' and a multitype of the remaining others. *)

val pickHeadType_opt	: typ -> (typ * typ) list option
	(* Given an intersection 't', this function returns an option value
	with the content of a call to pickDirectSubtype when 't' (i.e. a list
	of all pairs of a typed picked out of 't' and a multitype of the
	the remaining ones).
	It 't' is not an intersection, the function return None. *)

val pick2SplitType_opt 	: typ -> typ list list option
	(* Given a an intersection 't', this function return option value
	of a list of all possible splits of 't' as two intersections.
	If 't' is not an intersection, the function returns None. *)

val pickNSplitType_opt 	: int -> typ -> typ list list option
	(* Given an integer 'i' and an intersection 't', this function returns
	an option value of a list of all possible splits of 't' as N intersections.
	If 't' is not an intersection, the function returns None. *)


	(* Printings *)
val typ_to_str 			: 		typ 		-> string
val typSplits_to_str 	: 	typ list list 	-> string


	(* Multitype Sets *)
module OrderedMultitype : sig
  	type t = typ
  	val compare : typ -> typ -> int
end

module MultitypeSet :
	sig
		type elt = OrderedMultitype.t
		type t = Set.Make(OrderedMultitype).t
		val empty : t
		val mem : elt -> t -> bool
		val singleton : elt -> t
		val union : t -> t -> t
		val elements : t -> elt list
		val choose : t -> elt
		val choose_opt : t -> elt option
		val iter: (elt -> unit) -> t -> unit
	end