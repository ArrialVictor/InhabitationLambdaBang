open List

let ( >>= ) x f = flatten (map f x)


let allTwoSplits ls =
	let pickSide t split =
		let (left, right) = (nth split 0, nth split 1) 
			in [[t :: left; right]; [left; t :: right]]
	in let rec twoSplitsAux = function
		| [] 		-> [[[]; []]]
		| t :: ls' 	-> twoSplitsAux ls' >>= (pickSide t)
	in twoSplitsAux ls


let allNSplits n ls =
	let rec nSplitsAux n ls =
		if (n = 1) then [[ls]] else
		let resplitFirst ls =
			map (fun ls' -> ls' @ (tl ls)) (allTwoSplits (hd ls))
		in nSplitsAux (n - 1) ls >>= resplitFirst
	in if n <= 0 then [[]] else nSplitsAux n ls


let printNSplits toStr nSplits = 
    List.iter (fun s -> (print_string "["; List.iter (fun l -> (print_string " ["; List.iter (fun e -> print_string (" " ^ (toStr e))) l; print_string " ]")) s; print_string " ];\t")) nSplits
