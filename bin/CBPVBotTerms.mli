
    (*** Variables ***)
module Variable = Int

    (*** Bottom term ***)
type botTerms =
    | Var   of Variable.t
    | App   of  botTerms  * botTerms
    | Abs   of Variable.t * botTerms
    | Es    of  botTerms  * Variable.t * botTerms
    | Der   of  botTerms
    | Bag   of  botTerms
    | Unt


    (*** Variable Set ***)
module VariableSet :
    sig
        type elt = Variable.t
        type t = Set.Make(Variable).t
        val empty : t
        val add : elt -> t -> t
        val singleton : elt -> t 
        val remove : elt -> t -> t
        val union : t -> t -> t
        val inter : t -> t -> t
        val max_elt : t -> elt
        val max_elt_opt : t -> elt option
    end

val newIndepVar : VariableSet.t -> Variable.t


    (*** Bottom-Term Map ***)
module OrderedBotTerms : sig
    type t = botTerms
    val compare : t -> t -> int
end

module BotTermSet :
    sig
        type elt = OrderedBotTerms.t
        type t = Set.Make(OrderedBotTerms).t
        val empty : t
        val is_empty : t -> bool
        val add : elt -> t -> t
        val singleton : elt -> t
        val remove : elt -> t -> t
        val union : t -> t -> t
        val inter : t -> t -> t
        val iter : (elt -> unit) -> t -> unit
        val map : (elt -> elt) -> t -> t
        val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
        val elements : t -> elt list
        val of_list : elt list -> t
    end


    (*** Hygiene ***)
val freeVar     : botTerms -> VariableSet.t
val boundVar    : botTerms -> VariableSet.t
val isHygienic  : botTerms -> bool


    (*** Structure Check ***)
val isBang        : botTerms -> bool
val isAbs         : botTerms -> bool
val fullClashFree : botTerms -> bool
val isCanonical   : botTerms -> bool


    (*** Hygienic Clash-Free Constructors ***)
exception HygieneError  of string
exception ClashError    of string
val var   : Variable.t ->  botTerms
val app   :  botTerms  ->  botTerms  -> botTerms
val abs   : Variable.t ->  botTerms  -> botTerms
val es    :  botTerms  -> Variable.t -> botTerms -> botTerms
val der   :  botTerms  ->  botTerms
val bag   :  botTerms  ->  botTerms
val unt   :  botTerms


    (*** Supremum ***)
val areCompatible     : botTerms -> botTerms -> bool
val areCompatibleLs   : botTerms list       -> bool
val areCompatibleSet  : BotTermSet.t        -> bool
val anfJoin_opt       : botTerms -> botTerms -> botTerms option
val anfJoin           : botTerms -> botTerms -> botTerms


    (*** Printing ***)
val var_to_str_pretty : Variable.t -> string
val term_to_str         : botTerms -> string
val term_to_str_pretty  : botTerms -> string


    (*** Embedding Erasure ***)
val cbnErasing : botTerms -> BotTermSet.t
val cbnErasingMap : BotTermSet.t -> BotTermSet.t

val cbvErasing : botTerms -> BotTermSet.t
val cbvErasingMap : BotTermSet.t -> BotTermSet.t
