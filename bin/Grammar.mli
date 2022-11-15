


	(* Constants (C) *)
module GrammarCst = String
module GrammarCstSet :
	sig
		type elt = GrammarCst.t
		type t
	end

	(* Non Terminals (N) *)
module GrammarNTerm = String
module GrammarNTermSet :
	sig
		type elt = GrammarNTerm.t
		type t
	end

	(* Productions rules (R) *)
module GrammarProdRules :
	sig
		type key = GrammarNTerm.t
		type +!'a t
	end


	(* Algebraic Terms (T_{Σ_C}(N)) *)
type grammarTerm =
	| VAR
	| NTERM	of GrammarNTerm.t				(* Non-terminal symbol *)
	| CST	of GrammarCst.t					(* Constant symbol *)
	| APP	of grammarTerm * grammarTerm
	| ABS	of grammarTerm
	| ES	of grammarTerm * grammarTerm
	| DER	of grammarTerm
	| BAG	of grammarTerm


	(* C-grammar (G = (Σ_C, N, R, s)) *)
type grammar


	(* Projections *)

val getGramStart : grammar -> GrammarNTerm.t
  (* Given a grammar 'gram', this function returns the start symbol of
  the given grammar 'gram'. *)

val getGramProdTerms : grammar -> GrammarNTerm.t -> grammarTerm list 
  (* Given a grammar 'gram' and a non-terminal symbol 'nTerm',
  	this function returns a list [t_1; ...; t_n] of algebraic term T_{Σ_C}(N)
	such that for each t_i there exists a production of the form
	"nTerm |-> t_i" in the given grammar 'gram'. *)


	(* Constructor Functions *)

val emptyGram : GrammarNTerm.t -> grammar
	(* Given a non-terminal symbol 'start', this function returns a C-grammar
	G = (∅, {start}, ∅, start) which has no constant symbol, no production
	rule and start as start symbol and unique non-terminal symbol. *)

val addCst : grammar -> GrammarCst.t -> grammar
	(* Given a grammar 'gram' and a constant symbol 'cst', this function
	returns the given grammar whose constant symbol set has been extended
	to include 'cst'. If it already was a constant of 'gram' then the
	grammar is left untouched. *)

val addNonTerm : grammar -> GrammarNTerm.t -> grammar
	(* Given a grammar 'gram' and a non-terminal symbol 'nTerm', this function
	returns the given grammar whose non-terminal symbol set have been extended
	to include the non-terminal symbol ’nTerm’. If it already was a non-terminal
	symbol of ’gram’, then all productions rules attached to it are removed. *)

val appendGramProd : grammar -> GrammarNTerm.t -> grammarTerm -> grammar
	(* Given a grammar 'gram', a non-terminal symbol 'nTerm' and a grammar
	term 'term', the function returns the given grammar whose production
	rules have been extended to include the rule "nTerm ⇝ term". If 'nTerm'
	or if any non-terminal of 'term' is not a non-terminal symbol of 'gram'
	then the grammar is left untouched. And similarly if any constant
	of 'term' is not a constant of 'gram'. *)

val removeGram : grammar -> GrammarNTerm.t -> grammar
	(* Given a grammar 'gram' and a non-terminal symbol 'nTerm', the function
	returns the given grammar whose non-terminal 'nTerm' has been removed.
	If 'nTerm' was not a non-terminal symbol of 'gram' then the grammar
	is left untouched. Moreover, if 'nTerm' is the start symbol of 'gram'
	or if it is contained in any production rule associated to another
	non-terminal symbol, then the removal is considered unsafe and the
	grammar is left untouched. *)


	(* Printing *)

val grammarTerm_to_str  : grammarTerm -> String.t
val grammar_to_str           : grammar -> unit







