include Multitype
open List

let (#||) set1 set2 = MultitypeSet.union set1 set2


	(*** Multitype Matching ***)

let distrHoles ls =
	let rec extract = function
		| [] 			-> (0, [])
		| THole :: ls 	-> let (i, ls') = extract ls in (1 + i, ls')
		| 	e 	:: ls 	-> let (i, ls') = extract ls in (i, e :: ls')
	and distrAux (nbH, ls) =
		if nbH = 0 then [ls] else match ls with
			| [] 		-> [init nbH (fun _ -> THole)]
			| e :: ls' 	-> 
				let subHere 	= distrAux (nbH - 1, ls) in 
				let subNotHere 	= distrAux (nbH, ls')
					in (map (cons THole) subHere) @ (map (cons e) subNotHere)
	in extract ls |> distrAux

let rec tMatch typ patt =
	match (typ, patt) with
		| (_, THole) -> true
		| (TVar i, TVar j) -> i = j
		| (TArr(t11, t12), TArr(t21, t22)) -> (tMatch t11 t21) && (tMatch t12 t22)
		| (TMlt ls1, TMlt ls2) -> 
			if (length ls1) != (length ls2)
				then false
				else let ls2' = distrHoles ls2 in
					exists (fun pos -> exists2 tMatch ls1 pos) ls2'
		| _ -> false


	(*** Subtype Search Algorithm ***)

let subtypeSearch typ patt =
  	let rec searchAux typ =
    			(arrowRule 	typ)
    		#|| (multRule 	typ)
    		#|| (reflRule 	typ)

  	and arrowRule = function
    	| TArr(typ1, typ2) ->
      		(searchAux typ1) #|| (searchAux typ2)
    	| _ -> MultitypeSet.empty

  	and multRule = function
    	| TMlt(lsTyp) ->
      		fold_left (#||) MultitypeSet.empty (map searchAux lsTyp)
    	| _ -> MultitypeSet.empty

  	and reflRule typ =
    	if tMatch typ patt
    		then MultitypeSet.singleton typ
    		else MultitypeSet.empty 
  	in searchAux typ


let checkSubtype typ1 typ2 patt = 
  	let subtypes = subtypeSearch typ2 patt
  		in MultitypeSet.mem typ1 subtypes


