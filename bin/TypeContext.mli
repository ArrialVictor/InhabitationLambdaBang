open Multitype
open CBPVBotTerms

module MultitypeMap :
sig
  type key = Variable.t
  type 'a t = 'a Map.Make(Variable).t
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val map : ('a -> 'b) -> 'a t -> 'b t
end



(*** Semantical Head ***)
module Head :
sig
  type head = Head of Variable.t * typ

  (* Constructor function *)
  val semHead_opt : Variable.t -> typ -> head option
  val semHead     : Variable.t -> typ -> head 

  (* Projections *)
  val getTyp      : head -> typ
  val getStripTyp : head -> typ
  val getVar      : head -> Variable.t

  (* Printing *)
  val to_str        : head -> string
  val to_str_pretty : head -> string
end


(*** Type Context ***)
type typCtxt = typ MultitypeMap.t

(* Constructor functions *)
val emptyCtxt   : typCtxt
val ctxtAdd_opt : typCtxt -> Variable.t -> typ -> typCtxt option
val ctxtAdd     : typCtxt -> Variable.t -> typ -> typCtxt
val ctxtRemove  : typCtxt -> Variable.t -> typCtxt
val ctxtMerge   : typCtxt -> typCtxt -> typCtxt

(* Others *)
val domain    : typCtxt -> VariableSet.t
val newVar    : VariableSet.t -> Variable.t
val pickHead  : typCtxt -> (Head.head * typCtxt) list

(* Splitting *)
val pickNSplitCtxt : int -> typCtxt -> typCtxt list list
val pickTwoSplitCtxt :        typCtxt -> typCtxt list list

(* Printing *)
val ctxt_to_str         : typCtxt           -> string
val ctxt_to_str_pretty  : typCtxt           -> string
val ctxtSplits_to_str   : typCtxt list list -> string




