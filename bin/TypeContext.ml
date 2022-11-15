include Multitype
include CBPVBotTerms

open Option
open List
let ( >>= ) x f = flatten (map f x)

module MultitypeMap = Map.Make(Variable)




	(*** Head Type ***)

module Head = struct

	type head = Head of Variable.t * typ

	let semHead_opt var mTyp = match mTyp with
		| TMlt([typ]) 	-> Some(Head(var, tSingleton typ))
		| _ 			-> None

	let semHead var mTyp = get (semHead_opt var mTyp)


		(* Projections *)

	let getTyp = function
		| Head(_, mTyp) -> mTyp

	let getStripTyp_opt = function
		| Head(_, TMlt([typ])) -> Some(typ)
		| _ -> None

	let getStripTyp head = get (getStripTyp_opt head)

	let getVar = function
		| Head(var, _) -> var


		(* Printing *)

	let to_str head =
		term_to_str (var (getVar head)) ^ " : " ^ typ_to_str (getTyp head)

	let to_str_pretty head =
		term_to_str_pretty (var (getVar head)) ^ " : " ^ typ_to_str (getTyp head)

end




	(*** Type Context ***)

type typCtxt = typ MultitypeMap.t


(* Constructor Functions *)

let emptyCtxt = MultitypeMap.empty

let ctxtAdd_opt ctxt var mTyp =
	match mTyp with
		| TMlt([]) -> Some(ctxt)
		| TMlt(_) ->
			let newTyp = match MultitypeMap.find_opt var ctxt with
				| Some(mTyp') 	-> mTyp |+| mTyp' (* supposes a well-formed context *)
				| None 			-> mTyp
			in Some(MultitypeMap.add var newTyp ctxt)
		| _ ->
			let () = print_endline "All context types must be multisets"
			in None

let ctxtAdd ctxt var mTyp = get (ctxtAdd_opt ctxt var mTyp)

let ctxtRemove ctxt var = MultitypeMap.remove var ctxt

let ctxtMerge ctxt1 ctxt2 = 
	let f _ typOpt1 typOpt2 =
		match typOpt1, typOpt2 with
			| Some(typ1), Some(typ2) 	-> Some(typ1 |+| typ2) (* supposes a well-formed context *)
			| Some(typ1), None 			-> Some(typ1)
			| None, Some(typ2) 			-> Some(typ2)
			| None, None 				-> None
	in MultitypeMap.merge f ctxt1 ctxt2

let isEmpty ctxt = MultitypeMap.is_empty ctxt




(* Others *)

let domain ctxt =
	MultitypeMap.fold (fun var _ set -> VariableSet.add var set) ctxt VariableSet.empty

let newVar set = match VariableSet.max_elt_opt set with 
	| Some(i) 	-> i + 1
	| None 		-> 1


let pickHead ctxt =
	let pickOnVar (var, typ) =
		let varPickLs = value (pickHeadType_opt typ) ~default:[]
		in List.map (fun (t1, t2) -> (Head.Head(var, tSingleton t1), ctxtAdd (ctxtRemove ctxt var) var t2)) varPickLs
	in MultitypeMap.bindings ctxt >>= pickOnVar



(* Context Splits *)

let pickNSplitCtxt n ctxt =
	if (n = 0 && not(isEmpty ctxt)) then [] 		(* no 0 splits on non-empty ctxt *)
	else
		let nEmpty = [init n (fun _ -> emptyCtxt)]
		and pickOnVar var mTyp nSplSubCtxt =
			let varPickLs = value (pickNSplitType_opt n mTyp) ~default:[]
			and nAdd nSplC nSplV  = map2 (fun c t -> ctxtAdd c var t) nSplC nSplV
				in nSplSubCtxt >>= (fun nSpl -> (map (nAdd nSpl) varPickLs))
		in MultitypeMap.fold pickOnVar ctxt nEmpty

let pickTwoSplitCtxt = pickNSplitCtxt 2




(* Printing *)

let ctxt_to_str ctxt =
	let aux i typ str = str ^ (term_to_str (var i)) ^ ":" ^ (typ_to_str typ) ^ ", "
		in let str = MultitypeMap.fold aux ctxt ""
		in if isEmpty ctxt then "∅" else String.sub str 0 (String.length str - 2)

let ctxt_to_str_pretty ctxt =
	if isEmpty ctxt then "∅" else "{" ^ ctxt_to_str ctxt ^ "}"


let ctxtSplits_to_str splits =
	let split_to_str ls = 
		let inStr = fold_left (fun str ctxt -> str ^ (ctxt_to_str_pretty ctxt) ^ ",\n") "" ls
			in "[" ^ (String.sub inStr 0 (String.length inStr - 2)) ^ "]\n\n"
	in List.fold_left (fun a b -> a ^ b) "Splits :\n" (List.map split_to_str splits)


