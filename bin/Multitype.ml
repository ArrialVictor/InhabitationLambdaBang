
open Option
open List
include Util

let ( |> ) x f = map f x
let ( |?> ) x f = Option.map f x

let lsTypeVarSymb = ["α"; "β"; "γ" ; "δ"; "ε"; "σ"; "τ"; "ρ"; "µ"; "υ"; "ϕ"; "ζ"; "η"; "θ"; "ι"; "κ"; "ν"; "χ"; "ψ"]


	(*** Types ***)
	
type typ =
	| TVar of int
	| TArr of typ * typ
	| TMlt of typ list
	| THole




	(*** Comparaison and Matching ***)

let typComp t1 t2 = 
	let rec firstNonZero = function
		| i :: ls' 	-> if (i = 0) then firstNonZero ls' else i
		| _ 		-> 0
	in let rec compAux t1 t2 = match t1, t2 with
		| TMlt(lsTyp1), TMlt(lsTyp2) -> 
			(let len1 = length lsTyp1 and len2 = length lsTyp2 in
				if len1 = len2
					then firstNonZero (map2 compAux lsTyp1 lsTyp2)
					else Stdlib.compare lsTyp1 lsTyp2)
		| _ -> Stdlib.compare t1 t2
	in compAux t1 t2



let mltApply_opt f typ =
	match typ with
	| TMlt ls	-> Some (f ls)
	| _			-> None

let rec size typ = match typ with
	| TVar _			-> 1
	| TArr(typ1, typ2) 	-> 1 + (size typ1) + (size typ2)
	| TMlt lsTyp		-> 1 + (List.length lsTyp)
	| THole 			-> 0






	(*** Constructor Functions ***)

let tHole = THole

let tVar i = TVar i

let tArrow mType1 mType2 = TArr (mType1, mType2)

let tEmpty = TMlt []

let tSingleton typ = TMlt ([typ])

let add_opt mTyp typ =
	let rec addAux t ls = match ls with
		| []			-> [t]
		| t' :: ls'		-> if (typComp t t' <= 0)
			then t  :: ls
			else t' :: (addAux t ls')
	in mltApply_opt (fun ls -> TMlt(addAux typ ls)) mTyp 

let ( ++ ) mTyp typ =
	get (add_opt mTyp typ)

let merge_opt mTyp1 mTyp2 =
	mltApply_opt (fold_left (++) mTyp1) mTyp2

let ( |+| ) mTyp1 mTyp2 =
	get (merge_opt mTyp1 mTyp2)

let tMultiset lsTyp =
	fold_left (++) tEmpty lsTyp






	(*** Splitting and Picking ***)

let pickDirectSubtype typ =
	let rec pickMult = function 
		| typ :: tl ->
			let pickNow 	= (typ, tMultiset tl)
			and pickLatex 	= map (fun (h, t) -> (h, t ++ typ)) (pickMult tl)
				in pickNow :: pickLatex
		| [] -> []
	in match typ with
		| TMlt lsTyp	-> pickMult lsTyp
		| TArr(t1, t2)	-> [(t1, tSingleton t2); (t2, tSingleton t1)]
		| _				-> []

let pickHeadType_opt mTyp =
	mltApply_opt (fun _ -> pickDirectSubtype mTyp) mTyp

let pickNSplitType_opt n mTyp =
	mltApply_opt (allNSplits n) mTyp
		|?> (fun splits -> splits |> (fun split -> split |> tMultiset))

let pick2SplitType_opt mTyp =
	pickNSplitType_opt 2 mTyp




	(*** Printings ***)

let rec typ_to_str = function
	| THole 				-> "□"
	| TVar i 			-> if (i > 0 && i < length lsTypeVarSymb) then nth lsTypeVarSymb (i-1) else Int.to_string i
	| TArr(typ1, typ2) 	->
		let str1 = typ_to_str typ1
		and str2 = typ_to_str typ2
			in (match typ1 with
				| TArr(_, _) -> "(" ^ str1 ^ ")" ^ "->" ^ str2
				| _ 		-> 		str1 		^ "->" ^ str2)
	| TMlt lsTyp 		->
		let rec to_str_Aux = function
			| [] 			-> ""
			| typ::lsTyp' 	-> ", " ^ typ_to_str typ ^ to_str_Aux lsTyp'
		in match lsTyp with
			| [] 			-> "[]"
			| typ::lsTyp' 	-> "[" ^ typ_to_str typ ^ to_str_Aux lsTyp' ^ "]"


let typSplits_to_str splits =
	let split_to_str ls = 
		let inStr = fold_left (fun str mTyp -> str ^ (typ_to_str mTyp) ^ ", ") "" ls
			in "\t[" ^ (String.sub inStr 0 (String.length inStr - 2)) ^ "]\n"
	in fold_left (fun a b -> a ^ b) "Splits :\n" (map split_to_str splits)


module OrderedMultitype = struct
  	type t = typ
  	let compare t1 t2 = typComp t1 t2
end

module MultitypeSet = Set.Make(OrderedMultitype)