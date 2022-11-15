open List

let lsTermVarSymb = ["x"; "y"; "z"; "m"; "n"; "o"; "p"; "q"; "i"; "j"; "k"]

module Variable = Int



	(*** Bottom terms ***)

type botTerms =
	| Var 	of Variable.t
	| App 	of  botTerms  * botTerms
	| Abs 	of Variable.t * botTerms
	| Es 	of  botTerms  * Variable.t * botTerms
	| Der 	of  botTerms
	| Bag 	of  botTerms
	| Unt


	(*** Variable Set ***)

module VariableSet = Set.Make(Variable)
	let (#||) set1 set2 = VariableSet.union set1 set2
	let (#&&) set1 set2	= VariableSet.inter set1 set2
	let (#+) set e		= VariableSet.add e set
	let (#-) set e		= VariableSet.remove e set


	(*** Variable Map ***)

module VariableMap = Map.Make(Variable)

module OrderedBotTerms = struct
	type t = botTerms

	let compare t1 t2 =
		let rec aux varMap t1 t2 = match t1 with
			| Unt -> (match t2 with
				| Unt -> 0
				| _ -> -1)
			| Var i1 -> (match t2 with
				| Unt -> 1
				| Var i2 ->
					(match VariableMap.find_opt i1 !varMap with
						| Some i' 	-> i' - i2
						| None 		-> (varMap := (VariableMap.add i1 i2 !varMap)); 0)
				| _ -> -1)
			| App(t11, t12) -> (match t2 with
				| Unt | Var _ -> 1
				| App(t21, t22) -> let cmpL = aux varMap t11 t21 in
					if cmpL = 0 then aux varMap t12 t22 else cmpL
				| _ -> -1)
			| Abs(i1, t1') -> (match t2 with
				| Unt | Var _ | App _ -> 1
				| Abs(i2, t2') 	->
					(match VariableMap.find_opt i1 !varMap with
						| Some i1' -> if i1' != i2 then i1' - i2 else aux varMap t1' t2'
						| None 		-> (varMap := (VariableMap.add i1 i2 !varMap)); aux varMap t1' t2')
				| _ -> -1)
			| Es(t11, i1, t12) -> (match t2 with
				| Unt | Var _ | App _ | Abs _ -> 1
				| Es(t21, i2, t22) 	->
					let cmpL = aux varMap t11 t21 in
					if cmpL = 0
						then (match VariableMap.find_opt i1 !varMap with
							| Some i1' -> if i1' != i2 then i1' - i2 else aux varMap t12 t22
							| None 		-> (varMap := (VariableMap.add i1 i2 !varMap)); aux varMap t12 t22)
						else cmpL
				| _ -> -1)
			| Der t1' -> (match t2 with
				| Unt | Var _ | App _ | Abs _ | Es _ -> 1
				| Der t2' 		-> aux varMap t1' t2'
				| _ 			-> -1)
			| Bag t1' -> match t2 with
				| Unt | Var _ | App _ | Abs _ | Es _ | Der _ -> 1
				| Bag t2' 		-> aux varMap t1' t2'
		in aux (ref VariableMap.empty) t1 t2
end


	(*** Bottom-Term Map ***)

module BotTermSet = Set.Make(OrderedBotTerms)



	(*** Hygiene ***)

let newIndepVar varSet = VariableSet.max_elt varSet + 1

let rec freeVar = function
	| Var(i)		-> VariableSet.singleton i
	| App(t1, t2)	-> (freeVar t1) #|| (freeVar t2)
	| Abs(i, t)		-> (freeVar t) #- i
	| Es(t1, i, t2) -> (freeVar t1) #- i #|| (freeVar t2)
	| Der(t)		-> freeVar t
	| Bag(t)		-> freeVar t
	| Unt			-> VariableSet.empty

let rec boundVar = function
	| Var(_)		-> VariableSet.empty
	| App(t1, t2)	-> (boundVar t1) #|| (boundVar t2)
	| Abs(i, t)		-> (boundVar t) #+ i
	| Es(t1, i, t2) -> ((boundVar t1) #+ i) #|| (boundVar t2)
	| Der(t)		-> boundVar t
	| Bag(t)		-> boundVar t
	| Unt 			-> VariableSet.empty

let rec isHygienic = function
	| Var(_)		-> true
	| App(t1, t2)	-> (isHygienic t1) && (isHygienic t2)
						&& (VariableSet.is_empty ((boundVar t1) #&& (boundVar t2)))
						&& (VariableSet.is_empty ((freeVar t1) #&& (boundVar t2)))
						&& (VariableSet.is_empty ((freeVar t2) #&& (boundVar t1)))
	| Abs(_, t)		-> isHygienic t
	| Es(t1, i, t2) -> (isHygienic t1) && (isHygienic t2)
						&& (VariableSet.is_empty ((boundVar t1) #&& ((boundVar t2) #+ i)))
						&& (VariableSet.is_empty ((freeVar t1) #&& ((boundVar t2) #+ i)))
						&& (VariableSet.is_empty (((freeVar t2) #- i) #&& (boundVar t1)))
	| Der(t)		-> isHygienic t
	| Bag(t)		-> isHygienic t
	| Unt			-> true


exception HygieneError of string

let varHygienic _ = ()

let appHygienic t1 t2 = 
	if not ((boundVar t1) #&& (boundVar t2) = VariableSet.empty)
		then raise (HygieneError "Subterms must not share bound variables")
	else if not ((freeVar t1) #&& (boundVar t2) = VariableSet.empty)
		then raise (HygieneError "Left free variables must not be shared with right bound variables")
	else if not ((boundVar t1) #&& (freeVar t2) = VariableSet.empty)
		then raise (HygieneError "Left bound variables must not be shared with right free variables")
	else ()

let absHygienic _ _ = ()

let esHygienic t1 i t2 =
	if not ((boundVar t1) #&& ((boundVar t2) #+ i) = VariableSet.empty)
		then raise (HygieneError "Subterms must not share bound variables")
	else if not (((freeVar t1) #- i) #&& (boundVar t2) = VariableSet.empty)
		then raise (HygieneError "Left free variables must not be shared with right bound variables")
	else if not ((boundVar t1) #&& ((freeVar t2) #- i) = VariableSet.empty)
		then raise (HygieneError "Left bound variables must not be shared with right free variables")
	else ()

let derHygienic _ = ()

let bagHygienic _ = ()

let untHygienic _ = ()



	(*** Structure Check ***)

let rec isBang = function
	| Es(t, _, _) 	-> isBang t
	| Bag(_) 		-> true
	| _ 			-> false

let rec isAbs = function
	| Es(t, _, _) 	-> isAbs t
	| Abs(_, _)		-> true
	| _ 			-> false

let rec fullClashFree = function
	| Var(_) 		-> true
	| App(t1, t2) 	-> not (isBang t1) && not (isAbs t2)
						&& fullClashFree t1 && fullClashFree t2
	| Abs(_, t)		-> fullClashFree t
	| Es(t1, _, t2)	-> not (isAbs t2) && fullClashFree t1 && fullClashFree t2
	| Bag(t)		-> fullClashFree t
	| Der(t) 		-> not (isAbs t) && fullClashFree t
	| Unt 			-> true


exception ClashError of string

let varClashFree _ = ()

let appClashFree t1 t2 =
	if (isBang t1)
		then raise (ClashError "A bang to the left of an application forms a clash")
	else if (isAbs t2)
		then raise (ClashError "An abstraction to the right of an application forms a clash")
	else ()

let absClashFree _ _ = ()

let esClashFree _ _ t2 =
	if (isAbs t2)
		then raise (ClashError "An abstraction to the right of an explicit substitution forms a clash")
	else ()

let derClashFree t =
	if (isAbs t)
		then raise (ClashError "An abstraction under a dereliction forms a clash")
	else ()

let bagClashFree _ = ()

let untClashFree _ = ()



	(*** Hygienic Clash-Free Constructors ***)

let var i =
	let _ = varHygienic i in
	let _ = varClashFree i
		in Var i

let app t1 t2 =
	let _ = appHygienic t1 t2 in
	let _ = appClashFree t1 t2
		in App(t1, t2)

let abs i t = 
	let _ = absHygienic i t in
	let _ = absClashFree i t
		in Abs(i, t)

let es t1 i t2 = 
	let _ = esHygienic t1 i t2 in
	let _ = esClashFree t1 i t2
		in Es(t1, i, t2)

let der t =
	let _ = derHygienic t in
	let _ = derClashFree t
		in Der(t)

let bag t = 
	let _ = bagHygienic t in
	let _ = bagClashFree t
		in Bag(t)

let unt = 
	let _ = untHygienic in
	let _ = untClashFree
		in Unt



	(*** Canonicals and Approximants ***)

let isCanonical t =
	let rec isCNO t = (isCNA t) || (isCNB t)
	and isCNA t = (isCNE t) || match t with
		| Bag(Unt) 		-> true
		| Bag(t') 		-> isCNO t'
		| Es(t1, _, t2) ->  (isCNA t1) && (isCNE t2)
		| _ 			-> false
	and isCNB t = (isCNE t) || match t with
		| Abs(_, t') 	-> isCNO t'
		| Es(t1, _, t2) ->  (isCNB t1) && (isCNE t2)
		| _ 			-> false
	and isCNE t = match t with
		| Var _ 		-> true
		| App(t1, t2) 	-> (isCNE t1) && (isCNA t2)
		| Der(t')		-> isCNE t'
		| Es(t1, _, t2)	-> (isCNE t1) && (isCNE t2)
		| _ 			-> false
	in (isCNO t)


let isApproximant t =
	match t with
		| Unt -> true
		| _ -> isCanonical t



	(*** Supremum ***)

let areCompatible anf1 anf2 =
	let rec aux varMap anf1 anf2 = match anf1, anf2 with
		| Unt	, _ 	-> true
		| _		, Unt 	-> true
		| Var i	, Var j ->
			(match VariableMap.find_opt i !varMap with
				| Some j' 	-> j = j'
				| None 		-> (varMap := (VariableMap.add i j !varMap)); true)
		| App(a11, a12), 	App(a21, a22)	-> (aux varMap a11 a21) && (aux varMap a12 a22)
		| Es(a11, i, a12), Es(a21, j, a22)	->
			let varEq = (match VariableMap.find_opt i !varMap with
				| Some j' 	-> j = j'
				| None 		-> (varMap := (VariableMap.add i j !varMap)); true)
			in (aux varMap a11 a21) && (aux varMap a12 a22) && varEq
		| Der a1, Der a2 	-> aux varMap a1 a2
		| Bag a1, Bag a2 	-> aux varMap a1 a2
		| _ -> false	
	in if (isApproximant anf1 && isApproximant anf2)
		then aux (ref VariableMap.empty) anf1 anf2
		else false

let areCompatibleLs lsAnf = 
	snd (List.fold_left (fun (anf1, b) anf2 -> (anf2, b && (areCompatible anf1 anf2))) (Unt, true) lsAnf)

let areCompatibleSet setAnf = 
	snd (BotTermSet.fold (fun anf2 (anf1, b) -> (anf2, b && (areCompatible anf2 anf1))) setAnf (Unt, true))

let anfJoin_opt anf1 anf2 =
	let rec joinAux anf1 anf2 = match anf1, anf2 with
		| Unt	, _ 	-> anf2
		| _		, Unt 	-> anf1
		| Var _	, Var _ -> anf1
		| App(a11, a12), 	App(a21, a22) ->
			let joinL = joinAux a11 a12 and joinR = joinAux a21 a22
				in app joinL joinR
		| Es(a11, v1, a12), Es(a21, _, a22) -> 
			let joinL = joinAux a11 a12 and joinR = joinAux a21 a22
				in es joinL v1 joinR
		| Der a1, Der a2 	-> der (joinAux a1 a2)
		| Bag a1, Bag a2 	-> bag (joinAux a1 a2)
		| _ -> failwith ("ERROR : Compatibility is supposed to ensure join existence") (* Never reached *)
	in if areCompatible anf1 anf2
		then Some (joinAux anf1 anf2)
		else None

let anfJoin anf1 anf2 =
	Option.get (anfJoin_opt anf1 anf2)



	(*** Printing ***)

let rec term_to_str = function
	| Var i 		-> Int.to_string i
	| App(t1, t2) 	->
		let left = (match t1 with
		| Abs _ | Bag _ | Var _ -> "(" ^ (term_to_str t1) ^ ")"
		| _ 				 	-> term_to_str t1)
		and right = (match t2 with
			| App _ | Es _ | Der _ 	-> "(" ^ (term_to_str t2) ^ ")"
			| _ 					-> term_to_str t2)
			in left ^ right
	| Abs(i, t) 	-> "λ" ^ (Int.to_string i) ^ "." ^ (term_to_str t)
	| Es(t1, i, t2) ->
		let left = (match t1 with
			| App _ | Abs _ | Bag _ -> "(" ^ (term_to_str t1) ^ ")"
			| _ 					-> term_to_str t1)
		and right = "[" ^ (Int.to_string i) ^ ":=" ^ (term_to_str t2) ^ "]"
		in left ^ right
	| Der t 		-> "der(" ^ (term_to_str t) ^ ")"
	| Bag t 		-> "!" ^ (term_to_str t)
	| Unt 			-> "⊥"

let var_to_str_pretty (i : Variable.t) =
	if i = -1 then "(⊥v)"
	else if i < (List.length lsTermVarSymb)
		then List.nth lsTermVarSymb (i-1)
		else Int.to_string i

let term_to_str_pretty t =
	let vMap = ref VariableMap.empty and iMax = ref 0 in

	let rec var_to_str_aux i =
		if i = -1 then "_" else
		match VariableMap.find_opt i !vMap with
			| Some j 	-> var_to_str_pretty j
			| None 		->
				(iMax := !iMax + 1);
				(vMap := VariableMap.add i !iMax !vMap);
				var_to_str_pretty !iMax

	and aux = function
		| Var i 		-> var_to_str_aux i
		| App(t1, t2) 	->
			let left = (match t1 with
			| Abs _ | Bag _ -> "(" ^ (aux t1) ^ ")"
			| _ 			-> aux t1)
			and right = (match t2 with
				| App _ | Es _ | Der _ | Abs _ -> "(" ^ (aux t2) ^ ")"
				| _ 					-> aux t2)
				in left ^ right
		| Abs(i, t) 	-> "λ" ^ (var_to_str_aux i) ^ "." ^ aux t
		| Es(t1, i, t2) ->
			let left = (match t1 with
				| App _ | Abs _ | Bag _ -> "(" ^ (aux t1) ^ ")"
				| _ 					-> aux t1)
			and right = "[" ^ (var_to_str_aux i) ^ ":=" ^ aux t2 ^ "]"
			in left ^ right
		| Der t -> "der(" ^ (aux t) ^ ")"
		| Bag t -> "!" ^ aux t
		| Unt 	-> "⊥"
	in aux t


	(*** Embedding Erasure ***)

let ( |$>= ) ls f = fold_left (fun s acc -> BotTermSet.union s acc) BotTermSet.empty (map f ls)
let ( $$> ) set f = BotTermSet.map f set

let rec cbnErasing t = match t with
	| Var i -> BotTermSet.singleton (Var i)
	| App (u, v) -> let uInv = cbnErasing u and vInv = cbnErasing v
		in (BotTermSet.elements uInv) |$>= (fun a -> (BotTermSet.map (fun b -> App (a, b)) vInv))
	| Abs (i, u) -> (cbnErasing u) $$> (fun a -> Abs (i, a))
	| Es (u, i, v) -> let uInv = cbnErasing u and vInv = cbnErasing v
		in (BotTermSet.elements uInv) |$>= (fun a -> (BotTermSet.map (fun b -> Es (a, i, b)) vInv)) 
	| Unt -> (BotTermSet.singleton Unt)
	| Bag u -> cbnErasing u
	| _ -> BotTermSet.empty

let cbnErasingMap s = (BotTermSet.elements s) |$>= cbnErasing

let rec cbvErasing t = match t with
	| Var i -> BotTermSet.singleton (Var i)
	| App (u, v) -> let uInv = cbvErasing u and vInv = cbvErasing v
		in (BotTermSet.elements uInv) |$>= (fun a -> (BotTermSet.map (fun b -> App (a, b)) vInv))
	| Abs (i, u) -> (cbvErasing u) $$> (fun a -> Abs (i, a))
	| Es (u, i, v) -> let uInv = cbvErasing u and vInv = cbvErasing v
		in (BotTermSet.elements uInv) |$>= (fun a -> (BotTermSet.map (fun b -> Es (a, i, b)) vInv)) 
	| Unt -> (BotTermSet.union (BotTermSet.singleton (Var (-1))) (BotTermSet.singleton (Abs (-1, Unt))))
	| Bag u -> cbvErasing u
	| _ -> BotTermSet.empty

let cbvErasingMap s = (BotTermSet.elements s) |$>= cbvErasing