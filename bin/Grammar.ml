
open Option
open List


	(* Constants (C) *)
module GrammarCst 		= String
module GrammarCstSet 	= Set.Make(GrammarCst)

	(* Non Terminals (N) *)
module GrammarNTerm 	= String
module GrammarNTermSet 	= Set.Make(GrammarNTerm)

	(* Productions rules (R) *)
module GrammarProdRules = Map.Make(GrammarNTerm)

	(* Algebraic Terms (T_{Σ_C}(N)) *)
type grammarTerm =
	| VAR
	| NTERM	of GrammarNTerm.t				(* Non-terminal symbols *)
	| CST	of GrammarCst.t					(* Constants *)
	| APP	of grammarTerm * grammarTerm
	| ABS	of grammarTerm
	| ES	of grammarTerm * grammarTerm
	| DER	of grammarTerm
	| BAG	of grammarTerm


	(* C-grammar (G = (Σ_C, N, R, s)) *)
type grammar = Grammar of
	GrammarCstSet.t							(* Constants (C) *)
	* GrammarNTermSet.t						(* Non-terminals (N) *)
	* grammarTerm list GrammarProdRules.t	(* Production rules (R) *)
	* GrammarNTerm.t 						(* Start symbol (s) *)



	(* Projections *)

let getGramCst = function
	| Grammar(setC, _, _, _) -> setC

let getGramNonTerm = function
	| Grammar(_, setN, _, _) -> setN

let getGramProd = function
	| Grammar(_, _, prodR, _) -> prodR

let getGramStart = function
	| Grammar(_, _, _, s) -> s



let applyToGramMap f = function
	| Grammar(setC, setN, prodR, s) -> Grammar(setC, setN, f prodR, s)

let getGramProdTerms gram nTerm =
	GrammarProdRules.find nTerm (getGramProd gram)

let rec computeNonTerm = function
	| VAR 		-> GrammarNTermSet.empty
	| NTERM(s)	-> GrammarNTermSet.singleton s
	| CST(_)	-> GrammarNTermSet.empty
	| APP(t, u)	-> GrammarNTermSet.union (computeNonTerm t) (computeNonTerm u)
	| ABS(t)	-> computeNonTerm t
	| ES(t, u)	-> GrammarNTermSet.union (computeNonTerm t) (computeNonTerm u)
	| DER(t)	-> computeNonTerm t
	| BAG(t)	-> computeNonTerm t

let rec computeCst = function
	| VAR 		-> GrammarCstSet.empty
	| NTERM(_)	-> GrammarCstSet.empty
	| CST(s)	-> GrammarCstSet.singleton s
	| APP(t, u)	-> GrammarCstSet.union (computeCst t) (computeCst u)
	| ABS(t)	-> computeCst t
	| ES(t, u)	-> GrammarCstSet.union (computeCst t) (computeCst u)
	| DER(t)	-> computeCst t
	| BAG(t) 	-> computeCst t





	(* Constructor Functions *)

let emptyGram start =
	Grammar(GrammarCstSet.empty, GrammarNTermSet.singleton start, GrammarProdRules.add start [] (GrammarProdRules.empty), start)

let addCst gram cst =
	match gram with
		| Grammar(setC, setN, prodR, s) -> Grammar(GrammarCstSet.add cst setC, setN, prodR, s)

let addNonTerm gram nTerm =
	match gram with
		| Grammar(setC, setN, prodR, s) -> Grammar(setC, GrammarNTermSet.add nTerm setN, GrammarProdRules.add nTerm [] prodR, s)

let appendGramProd gram nTerm term =
	if GrammarNTermSet.subset (computeNonTerm term) (getGramNonTerm gram)
		then 
			if GrammarCstSet.subset (computeCst term) (getGramCst gram)
				then applyToGramMap (GrammarProdRules.update nTerm (fun x -> Option.bind x (fun y -> Some(term :: y)))) gram
				else let () = print_endline "This production cannot be added since it is uses constants which do not exist in this grammar."
					in gram
		else let () = print_endline "This production cannot be added since it is uses non-terminals which do not exist in this grammar."
			in gram

let removeGram gram nTerm =
	if nTerm = getGramStart gram
		then let () = print_endline "The grammar start symbol cannot be deleted."
			in gram
		else
			let lsOthers = (GrammarNTermSet.elements (GrammarNTermSet.remove nTerm (getGramNonTerm gram))) in
			let prodInOthers = List.flatten (List.map (fun var -> getGramProdTerms gram var) lsOthers) in
			let setNonTermInOthers = List.fold_left (fun s1 s2 -> GrammarNTermSet.union s1 s2) GrammarNTermSet.empty (List.map (fun prod -> computeNonTerm prod) prodInOthers)
			in if GrammarNTermSet.mem nTerm setNonTermInOthers
				then let () = print_endline "This non-terminal cannot be deleted since it is used in other production rules."
					in gram
				else applyToGramMap (GrammarProdRules.remove nTerm) gram


	(* Printing *)

let rec grammarTerm_to_str = function
	| VAR 			-> "x"
	| CST(s)		-> s
	| NTERM(g) 		-> g
	| APP(g1, g2) 	-> (grammarTerm_to_str g1) ^ " " ^ (grammarTerm_to_str g2)
	| ABS(g) 		-> "λx." ^ (grammarTerm_to_str g)
	| ES(g1, g2) 	-> (grammarTerm_to_str g1) ^ " [x := " ^ (grammarTerm_to_str g2) ^ "]"
	| DER(g) 		-> "der(" ^ (grammarTerm_to_str g) ^ ")"
	| BAG(g) 		-> "!" ^ (grammarTerm_to_str g)
	

let grammar_to_str gram =
	let aux ls =
		let str = List.fold_left (fun str ruleStruct -> (grammarTerm_to_str ruleStruct) ^ " | " ^ str) "" ls
			in if (String.length str >= 2) then String.sub str 0 (String.length str - 2) else str
	in GrammarProdRules.iter (fun name ls -> print_endline (name ^ " ::= " ^ (aux ls))) (getGramProd gram)

