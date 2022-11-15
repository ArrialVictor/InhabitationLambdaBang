open Multitype

	(* To allow Set.Make *)
module OrderedMultitype :
	sig
  		type t = typ
  		val compare : t -> t -> int
	end

	
	(* Subtype Algorithm *)
val subtypeSearch : typ -> typ -> MultitypeSet.t
val checkSubtype 	: typ -> typ -> typ -> bool
