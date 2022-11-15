open Multitype
open CBPVBotTerms
open TypeContext
open Grammar

module Call :
	sig
  		type call =
    			| CallN 	of 				typCtxt * typ	
    			| CallH 	of Head.head  * typCtxt * typ

  		val getTypCtxt 	: call -> typCtxt
  		val getTyp 		: call -> typ
  		val getHead_opt : call -> Head.head option
  		val getHead 	: call -> Head.head
  		val call_to_str : call -> string
	end

val pickInt : int -> int -> int list

val botTermSet_to_str : BotTermSet.t -> string

val inhAlgo : ?verbose:int -> grammar -> typCtxt -> typ -> BotTermSet.t

val printInhResults : BotTermSet.t -> unit