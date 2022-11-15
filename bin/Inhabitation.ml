include Multitype
open CBPVBotTerms
include SubtypeSearch
include TypeContext
include Grammar

open Option
open List

module Call = struct

	type call =
		| CallN of 				typCtxt * typ
		| CallH of Head.head  * typCtxt * typ

	let getTypCtxt call = match call with
		| CallH(_, ctxt, _) 	-> ctxt
		| CallN(ctxt, _) 		-> ctxt

	let getTyp call = match call with 
		| CallH(_, _, typ) 	-> typ
		| CallN(_, typ) 	-> typ

	let getHead_opt call = match call with
		| CallH(head, _, _) -> Some(head)
		| _ -> None

	let getHead call = get (getHead_opt call)

	let call_to_str call = match call with
		| CallH(head, ctxt, typ) 	-> "H(" ^ Head.to_str head ^ ";  " ^ ctxt_to_str ctxt ^ ";  " ^ typ_to_str typ ^ ")"
		| CallN(ctxt, typ)			-> "N(" ^ ctxt_to_str ctxt ^ ";  " ^ typ_to_str typ ^ ")"

end

let pickInt start length = init length (fun i -> i + start)


	(*** Subtype Search Algorithm ***)

let (#||) set1 set2 = BotTermSet.union set1 set2
let (#&&) set1 set2 = BotTermSet.inter set1 set2
let (#+)  set   e 	= BotTermSet.add 	e   set
let (#-)  set   e 	= BotTermSet.remove e   set
let single x = BotTermSet.singleton x

let botTermSet_to_str s =
	if BotTermSet.is_empty s then "∅"
		else let output = BotTermSet.fold (fun term str -> (str ^ (term_to_str_pretty term) ^ ",  ")) s ""
			in (String.sub (output) 0 (String.length output - 3))

let ( ||> ) ls f = List.map f ls
let ( |/> ) ls f = filter_map f ls
let ( ||>= ) x f = flatten (map f x)
let ( |$>= ) ls f = fold_left (fun s acc -> BotTermSet.union s acc) BotTermSet.empty (map f ls)
let ( $$> ) set f = BotTermSet.map f set
let ( >|$> ) ls f  = BotTermSet.of_list (ls ||> f) 

let inhAlgo ?(verbose = 0) gram ctxt typ =

	let bnd = ref (VariableSet.singleton 1) in

	let rec main_call ?(tab = "   ") currGram call =
		let lsRules = getGramProdTerms gram currGram in
			fold_left (fun sRes rule -> sRes #|| (applyRule rule call ~tab)) BotTermSet.empty lsRules

	and headedCall f call =
		match call with 
			| Call.CallH(head, typCtxt, typ) 	-> f head typCtxt typ
			| _ 								-> BotTermSet.empty

	and beheadedCall f call =
		match call with 
			| Call.CallN(typCtxt, typ) 	-> f typCtxt typ
			| _ 						-> BotTermSet.empty

	and applyRule ?(tab = "") gram call = match gram with
		| VAR 						-> rules_var 	~tab call
		| NTERM(g)					-> rules_eps 	~tab call g
		| APP(NTERM(g1), NTERM(g2)) -> rules_app	~tab call g1 g2
		| ABS(NTERM(g)) 			-> rules_abs	~tab call g
		| ES(NTERM(g1), NTERM(g2))	-> rules_es 	~tab call g1 g2
		| DER(NTERM(g)) 			-> rules_der	~tab call g
		| BAG(NTERM(g)) 			-> rules_bag	~tab call g
		| BAG(CST "⊥")				-> rules_bagUnt ~tab call
		| _ 						-> no_rule

	and no_rule = BotTermSet.empty


		(* Variables *)

	and rules_var ~tab call =
		let strCall = Call.call_to_str call in
			headedCall (rule_var ~tab ~strCall) call

	and rule_var ~tab ~strCall head ctxt typ =
		let res =
			if (Head.getTyp head = tSingleton typ && ctxt = emptyCtxt)
				then single (var (Head.getVar head))
				else BotTermSet.empty

		in let () = verbosePrint tab res ("Var - " ^ strCall)
			in res


		(* Applications *)

	and rules_app ~tab call subGram1 subGram2 =
		let strCall = Call.call_to_str call in
			headedCall (rule_app subGram1 subGram2 ~tab ~strCall:strCall) call
	
	and rule_app ~tab ~strCall subGram1 subGram2 head ctxt typ =

		let splitCtxt (head, ctxt, typ) =
			pickTwoSplitCtxt ctxt ||> (fun ls -> (head, hd ls, hd (tl ls), typ))
		and findSubtype (head, ctxt1, ctxt2, typ) =
			let subtypes = MultitypeSet.elements (subtypeSearch (Head.getStripTyp head) (tArrow (tHole) typ))
			and extractArgTyp = function
				| TArr(t, _) -> Some(t)
				| _ -> None
			in subtypes
				|/> extractArgTyp
				||> (fun argTyp -> ((head, ctxt1, TArr(argTyp, typ)), (ctxt2, argTyp)))
		and callCons ((head, ctxt1, typ1), (ctxt2, typ2)) =
			(Call.CallH (head, ctxt1, typ1), Call.CallN(ctxt2, typ2))
		and combineApp (set1, set2) =
			BotTermSet.elements set1 |$>= (fun t1 -> set2 $$> app t1)

		in let res = [(head, ctxt, typ)]
			||>= splitCtxt
			||>= findSubtype
			||> callCons
			||> (fun (call1, call2) -> let () = verboseSep tab in 
					(main_call ~tab:(tab ^ " │ ░") subGram1 call1, main_call ~tab:(tab ^ " │ ▓") subGram2 call2))
			|$>= combineApp

		in let () = verbosePrint tab res ("App - " ^ strCall)
			in res


		(* Abstraction *)

	and rules_abs ~tab call subGram =
		let strCall = Call.call_to_str call in
			beheadedCall (rule_abs subGram ~tab ~strCall) call

	and rule_abs ~tab ~strCall subGram ctxt typ =
		let generateVar (ctxt, typ) =
			let var = max (newVar (domain ctxt)) (VariableSet.max_elt !bnd + 1)
				in (bnd := VariableSet.add var !bnd);
					(var, ctxt, typ)
		and extractArgTyp (var, ctxt, typ) = match typ with
			| TArr(argTyp, typ') ->
				(match argTyp with
					| TMlt(_) 	-> Some((var, ctxtAdd ctxt var argTyp, typ'))
					| _ 		-> None)
			| _ -> None
		and callCons (var, ctxt, typ) =
			(var, Call.CallN(ctxt, typ))
		and addLambda (var, set) =
			set $$> abs var

		in let res = [(ctxt, typ)]
			||> generateVar
			|/> extractArgTyp
			||> callCons
			||> (fun (var, call) -> (var, main_call ~tab:(tab ^ " │ ") subGram call))
			|$>= addLambda

		in let () = verbosePrint tab res ("Abs - " ^ strCall)
			in res


		(* Explicit Substitutions *)

	and rules_es ~tab call subGram1 subGram2 =
		let strCall = Call.call_to_str call in
			(headedCall (rule_esh subGram1 subGram2 ~tab ~strCall) call)
		#|| (headedCall (rule_eshc subGram1 subGram2 ~tab ~strCall) call)
		#|| (beheadedCall (rule_esn subGram1 subGram2 ~tab ~strCall) call)


	and rule_esh ~tab ~strCall subGram1 subGram2 head ctxt typ = 

		let chooseNewHead (hd1, ctxt, typ) =
			pickHead ctxt ||> (fun (hd2, ctxt') -> ((hd1, ctxt', typ), hd2))
		and chooseCtxtSplit ((hd1, ctxt, typ), hd2) =
			pickTwoSplitCtxt ctxt ||> (fun ls -> ((hd1, hd ls, typ), (hd2, hd (tl ls))))
		and addNewVar ((hd1, ctxt1, typ), (hd2, ctxt2)) =
			let gam =
				VariableSet.union
					(VariableSet.union
						(VariableSet.union (domain ctxt1) (domain ctxt2))
					(VariableSet.singleton (Head.getVar hd2)))
				(VariableSet.singleton (Head.getVar hd1)) in
				let var = max (newVar gam) (VariableSet.max_elt !bnd + 1) 
				in (bnd := VariableSet.add var !bnd);
					(var, (hd1, ctxt1, typ), (hd2, ctxt2))
		and chooseTypPatt (var, (hd1, ctxt1, typ), (hd2, ctxt2)) =
			pickInt 0 (size (Head.getStripTyp hd2))
				||> (fun i -> init i (fun _ -> tHole) |> tMultiset)
				||> (fun patt -> (var, (hd1, ctxt1, typ), (hd2, ctxt2, patt)))
		and chooseSubtype (var, (hd1, ctxt1, typ), (hd2, ctxt2, patt)) =
			MultitypeSet.elements (subtypeSearch (Head.getStripTyp hd2) patt)
				||> (fun typ' -> (var, (hd1, ctxtAdd ctxt1 var typ', typ), (hd2, ctxt2, typ')))
		and callCons (var, (hd1, ctxt1, typ1), (hd2, ctxt2, typ2)) =
			(var, Call.CallH(hd1, ctxt1, typ1), Call.CallH(hd2, ctxt2, typ2))
		and computeRecCall (var, call1, call2) =
			let () = verboseSep tab and tab1 = (tab ^ " │ ░") and tab2 = (tab ^ " │ ▓")
				in (var, main_call ~tab:tab1 subGram1 call1, main_call ~tab:tab2 subGram2 call2)
		and combineEs (var, set1, set2) =
			BotTermSet.elements set1 |$>= (fun t1 -> set2 $$> es t1 var)

		in let res =
			[(head, ctxt, typ)]
			||>= chooseNewHead
			||>= chooseCtxtSplit
			||> addNewVar
			||>= chooseTypPatt
			||>= chooseSubtype
			||>	callCons
			||>	computeRecCall
			|$>= combineEs

		in let () = verbosePrint tab res ("EsH - " ^ strCall)
			in res

	and rule_eshc ~tab ~strCall subGram1 subGram2 head ctxt typ =

		let chooseCtxtSplit (head, ctxt, typ) =
			pickTwoSplitCtxt ctxt ||> (fun ls -> ((hd ls, typ), (head, hd (tl ls))))
		and addNewVar ((ctxt1, typ), (hd, ctxt2)) =
			let gam = VariableSet.union (VariableSet.union (domain ctxt1) (domain ctxt2)) (VariableSet.singleton (Head.getVar hd)) in
			let var = max (newVar gam) (VariableSet.max_elt !bnd + 1) 
				in (bnd := VariableSet.add var !bnd);
					(var, (ctxt1, typ), (hd, ctxt2))
		and chooseTypPatt (var, (ctxt1, typ), (hd, ctxt2)) =
			pickInt 1 (size (Head.getStripTyp hd))
				||> (fun i -> init i (fun _ -> tHole) |> tMultiset)
				||> (fun patt -> (var, (ctxt1, typ), (hd, ctxt2, patt)))
		and chooseSubtype (var, (ctxt1, typ), (hd, ctxt2, patt)) =
			MultitypeSet.elements (subtypeSearch (Head.getStripTyp hd) patt)
				||> (fun typ' -> (var, (ctxt1, typ), (hd, ctxt2, typ')))
		and chooseHead (var, (ctxt1, typ1), (hd2, ctxt2, typ2)) =
			pickDirectSubtype typ2
				||> (fun (hd1, typ2') ->
					let hd1' = Head.Head(var, tSingleton hd1)
					and ctxt1' = ctxtAdd ctxt1 var typ2'
						in (var, (hd1', ctxt1', typ1), (hd2, ctxt2, typ2)))
		and isSubtype (var, (hd1, ctxt1, typ1), (hd2, ctxt2, typ2)) =
			if checkSubtype typ1 (Head.getStripTyp hd1) tHole
				then Some((var, (hd1, ctxt1, typ1), (hd2, ctxt2, typ2))) else None
		and callCons (var, (hd1, ctxt1, typ1), (hd2, ctxt2, typ2)) =
			(var, Call.CallH(hd1, ctxt1, typ1), Call.CallH(hd2, ctxt2, typ2))
		and computeRecCall (var, call1, call2) =
			let () = verboseSep tab and tab1 = (tab ^ " │ ░") and tab2 = (tab ^ " │ ▓")
				in (var, main_call ~tab:tab1 subGram1 call1, main_call ~tab:tab2 subGram2 call2)
		and combineEs (var, set1, set2) =
			BotTermSet.elements set1 |$>= (fun t1 -> set2 $$> es t1 var)

		in let res = [(head, ctxt, typ)]
			||>= chooseCtxtSplit
			||> addNewVar
			||>= chooseTypPatt
			||>= chooseSubtype
			||>= chooseHead
			|/> isSubtype
			||>	callCons
			||>	computeRecCall
			|$>= combineEs

		in let () = verbosePrint tab res ("EsHc - " ^ strCall)
			in res

	and rule_esn ~tab ~strCall subGram1 subGram2 ctxt typ =

		let chooseNewHead (ctxt, typ) =
			pickHead ctxt ||> (fun (hd2, ctxt') -> ((ctxt', typ), hd2))
		and chooseCtxtSplit ((ctxt, typ), hd2) =
			pickTwoSplitCtxt ctxt ||> (fun ls -> ((hd ls, typ), (hd2, hd (tl ls))))
		and addNewVar ((ctxt1, typ), (hd2, ctxt2)) =
			let gam =
				VariableSet.union 
					(VariableSet.union (domain ctxt1) (domain ctxt2))
					(VariableSet.singleton (Head.getVar hd2)) in
				let var = max (newVar gam) (VariableSet.max_elt !bnd + 1)
				in (bnd := VariableSet.add var !bnd);
					(var, (ctxt1, typ), (hd2, ctxt2))
		and chooseTypPatt (var, (ctxt1, typ1), (hd2, ctxt2)) =
			pickInt 0 (size (Head.getStripTyp hd2))
				||> (fun i -> init i (fun _ -> tHole) |> tMultiset)
				||> (fun patt -> (var, (ctxt1, typ1), (hd2, ctxt2, patt)))
		and chooseSubtype (var, (ctxt1, typ1), (hd2, ctxt2, patt)) =
			MultitypeSet.elements (subtypeSearch (Head.getStripTyp hd2) patt)
				||> (fun typ2 -> (var, (ctxtAdd ctxt1 var typ2, typ1), (hd2, ctxt2, typ2)))
		and callCons (var, (ctxt1, typ1), (hd2, ctxt2, typ2)) =
			(var, Call.CallN(ctxt1, typ1), Call.CallH(hd2, ctxt2, typ2))
		and computeRecCall (var, call1, call2) =
			let () = verboseSep tab and tab1 = (tab ^ " │ ░") and tab2 = (tab ^ " │ ▓")
				in (var, main_call ~tab:tab1 subGram1 call1, main_call ~tab:tab2 subGram2 call2)
		and combineEs (var, set1, set2) =
			BotTermSet.elements set1 |$>= (fun t1 -> set2 $$> es t1 var)

		in let res = [(ctxt, typ)]
			||>= chooseNewHead
			||>= chooseCtxtSplit
			||> addNewVar
			||>= chooseTypPatt
			||>= chooseSubtype
			||>	callCons
			||>	computeRecCall
			|$>= combineEs

		in let () = verbosePrint tab res ("EsN - " ^ strCall)
			in res


		(* Dereliction *)

	and rules_der ~tab call subGram =
		let strCall = Call.call_to_str call in
			headedCall (rule_der subGram ~tab ~strCall) call

	and rule_der ~tab ~strCall subGram head ctxt typ =
		let addMlt (head, ctxt, typ) =
			(head, ctxt, tSingleton typ) 
		and isSubtype (head, ctxt, typ) =
			if checkSubtype typ (Head.getStripTyp head) tHole
				then Some((head, ctxt, typ)) else None
		and callCons (head, ctxt, typ) =
			Call.CallH (head, ctxt, typ)
		and addDer t = der t

		in let res = [(head, ctxt, typ)]
			||> addMlt
			|/> isSubtype
			||> callCons
			|$>= (main_call ~tab:(tab ^ " │ ") subGram)
			$$> addDer

		in let () = verbosePrint tab res ("Der - " ^ strCall)
			in res


		(* Bang *)

	and rules_bag ~tab call subGram =
		let strCall = Call.call_to_str call in
			beheadedCall (rule_bag subGram ~tab ~strCall) call


	and rule_bag ~tab ~strCall subGram ctxt typ =
		let isNoneEmptyMultiset (ctxt, typ) =
			match typ with
				| TMlt(ls) ->
					if List.length ls > 0
						then Some((ctxt, ls))
						else None
				| _ -> None
		and chooseCtxtSplitting (ctxt, ls) =
			pickNSplitCtxt (length ls) ctxt
				||> (map2 (fun styp ctxt -> (ctxt, styp)) ls)
		and callCons (ctxt, typ) =
			Call.CallN (ctxt, typ)
		and recCall ls =
			let tabs = [(tab ^ " │ ░");  (tab ^ " │ ▓")] in
			let recAux i call = main_call ~tab:(nth tabs (i mod 2)) subGram call
				in mapi recAux ls
		and areAllSucessful setLs =
			if exists (BotTermSet.is_empty) setLs then None else Some(setLs)
		and combine setLs =
			fold_left (fun tl set -> (BotTermSet.fold (fun t acc -> ((List.map (fun s -> BotTermSet.add t s) tl) @ acc)) set [])) [BotTermSet.empty] setLs
		and areCompatible set =
			if (areCompatibleSet set) then Some(set) else None
		and joinAnf set =
			BotTermSet.fold (fun anf1 anf2 -> anfJoin anf1 anf2) set Unt
		and addBang t = bag t

		in let res = [(ctxt, typ)]
			|/> isNoneEmptyMultiset
			||>= chooseCtxtSplitting
			||> (fun ls -> ls ||> callCons)
			||> recCall
			|/> areAllSucessful
			||>= combine
			|/> areCompatible
			>|$> joinAnf
			$$> addBang

		in let () = verbosePrint tab res ("Bg - " ^ strCall)
			in res


		(* Bang Untyped *)
	
	and rules_bagUnt ~tab call =
		let strCall = Call.call_to_str call in
			beheadedCall (rule_bagUnt ~tab ~strCall) call

	and rule_bagUnt ~tab ~strCall ctxt typ =
		let res =
			if (tEmpty =  typ && ctxt = emptyCtxt)
				then single (bag unt)
				else BotTermSet.empty

		in let () = verbosePrint tab res ("Bg⊥ - " ^ strCall)
			in res


		(* Silent Transitions *)

	and rules_eps ~tab call subGram =
		let strCall = Call.call_to_str call in
			(headedCall (rule_HH subGram ~tab ~strCall) call)
		#|| (beheadedCall (rule_NH subGram ~tab ~strCall) call)
		#|| (beheadedCall (rule_NN subGram ~tab ~strCall) call)

	and rule_NH ~tab ~strCall subGram ctxt typ =
		let isSubtype (h, c) =
			if checkSubtype typ (Head.getStripTyp h) tHole
				then Some((h, c)) else None
		and callCons (h, c) = Call.CallH (h, c, typ)

			in let res =
				pickHead ctxt
				|/> isSubtype
				||> callCons
				|$>= (fun c -> let () = verboseSep tab in main_call ~tab:(tab ^ " │ ") subGram c)

		in let () = verbosePrint tab res ("NH - " ^ strCall)
			in res

	and rule_NN ~tab ~strCall subGram ctxt typ =
		let res = main_call subGram (Call.CallN(ctxt, typ)) ~tab:(tab ^ " │ ")

		in let () = verbosePrint tab res ("NN - " ^ strCall)
			in res

	and rule_HH ~tab ~strCall subGram head ctxt typ =
		let res = main_call subGram (Call.CallH(head, ctxt, typ)) ~tab:(tab ^ " │ ") 

		in let () = verbosePrint tab res ("HH - " ^ strCall)
			in res
		




	and sepPos = 100

	and verbosePrint tab res str = match verbose with
		| 3 -> if (BotTermSet.is_empty res)
			then 
				let fullCallStr = tab ^ " ✘ " ^ str in
				let e = String.length (String.escaped fullCallStr) and l = String.length fullCallStr in
				let completed = fullCallStr ^ (String.make (sepPos - (l - (2 * ((e - l) / 9)))) ' ' ^ "║") in
				print_endline completed
			else
				let fullCallStr = tab ^ " ✔ " ^ str in
				let e = String.length (String.escaped fullCallStr) and l = String.length fullCallStr in
				let completed = fullCallStr ^ (String.make (sepPos - (l - (2 * ((e - l) / 9)))) ' ' ^ "╠> ") in
					(print_string completed;
					BotTermSet.iter (fun t -> print_string (term_to_str t ^ "  ")) res;
					print_endline "")
		| 2 -> if (BotTermSet.is_empty res)
			then print_endline (tab ^ " ✘ " ^ str)
			else print_endline (tab ^ " ✔ " ^ str)
		| 1 -> if (BotTermSet.is_empty res)
			then ()
			else print_endline (tab ^ " ✔ " ^ str)
		| _ -> ()

	and verboseSep tab = match verbose with
		| 3 -> 
			let fullCallStr = tab ^ " ├───────────────" in
			let e = String.length (String.escaped fullCallStr) and l = String.length fullCallStr in
			let completed = fullCallStr ^ (String.make (sepPos - (l - (2 * ((e - l) / 9)))) ' ' ^ "║") in
			print_endline completed
		| 1 | 2 ->
			print_endline (tab ^ " ├───────────────")
		| _ -> ()


in let call = Call.CallN (ctxt, typ)
	in let res = main_call (getGramStart gram) call
	in let () = print_endline ("Inh " ^ Call.call_to_str call)
		in res


let printInhResults res = 
	print_endline ("Sol>  " ^ botTermSet_to_str res)


