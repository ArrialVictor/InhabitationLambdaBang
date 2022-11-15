include CBPVBotTerms
include Multitype
include TypeContext
include Grammar

let () = print_endline ("\n*************** Examples of Grammar ***************\n")


(* Call-by-Push-Value : B *)

    (* Introducing non terminals *)
let canCBPV = emptyGram "cno"
let canCBPV = addNonTerm canCBPV "cna"
let canCBPV = addNonTerm canCBPV "cnb"
let canCBPV = addNonTerm canCBPV "cne"
let canCBPV = addCst canCBPV "⊥"

    (* Adding production rules *)
let canCBPV = appendGramProd canCBPV "cne" (VAR)
let canCBPV = appendGramProd canCBPV "cne" (APP((NTERM "cne"), (NTERM "cna")))
let canCBPV = appendGramProd canCBPV "cne" (DER (NTERM "cne"))
let canCBPV = appendGramProd canCBPV "cne" (ES((NTERM "cne"), (NTERM "cne")))

let canCBPV = appendGramProd canCBPV "cnb" (NTERM "cne")
let canCBPV = appendGramProd canCBPV "cnb" (ABS (NTERM "cno"))
let canCBPV = appendGramProd canCBPV "cnb" (ES((NTERM "cnb"), (NTERM "cne")))

let canCBPV = appendGramProd canCBPV "cna" (NTERM "cne")
let canCBPV = appendGramProd canCBPV "cna" (BAG (NTERM "cno"))
let canCBPV = appendGramProd canCBPV "cna" (BAG (CST "⊥"))
let canCBPV = appendGramProd canCBPV "cna" (ES((NTERM "cna"), (NTERM "cne")))

let canCBPV = appendGramProd canCBPV "cno" (NTERM "cna")
let canCBPV = appendGramProd canCBPV "cno" (NTERM "cnb")

    (* Printing grammar *)
let () = grammar_to_str canCBPV
let () = print_newline ()


(* Call-by-Value : BV *)
let canCBV = emptyGram "vna"
let canCBV = addNonTerm canCBV "vnb"
let canCBV = addNonTerm canCBV "vne_v"
let canCBV = addNonTerm canCBV "vne_a"
let canCBV = addNonTerm canCBV "vne_d"
let canCBV = addNonTerm canCBV "vne_f"
let canCBV = addCst canCBV "⊥"

let canCBV = appendGramProd canCBV "vna" (NTERM "vne_a")
let canCBV = appendGramProd canCBV "vna" (BAG (NTERM "vnb"))
let canCBV = appendGramProd canCBV "vna" (BAG (CST "⊥"))
let canCBV = appendGramProd canCBV "vna" (ES ((NTERM "vna"), (NTERM "vne_a")))

let canCBV = appendGramProd canCBV "vnb" (NTERM "vne_v")
let canCBV = appendGramProd canCBV "vnb" (ABS (NTERM "vna"))

let canCBV = appendGramProd canCBV "vne_v" (VAR)

let canCBV = appendGramProd canCBV "vne_a" (APP ((NTERM "vne_f"), (NTERM "vna")))
let canCBV = appendGramProd canCBV "vne_a" (APP ((NTERM "vne_d"), (NTERM "vna")))
let canCBV = appendGramProd canCBV "vne_a" (ES ((NTERM "vne_a"), (NTERM "vne_a")))

let canCBV = appendGramProd canCBV "vne_d" (DER (NTERM "vne_a"))

let canCBV = appendGramProd canCBV "vne_f" (VAR)
let canCBV = appendGramProd canCBV "vne_f" (ES ((NTERM "vne_f"), (NTERM "vne_a")))

let () = grammar_to_str canCBV
let () = print_newline ()


(* Call-by-Name : BN *)
let canCBN = emptyGram "nno"
let canCBN = addNonTerm canCBN "nne"
let canCBN = addNonTerm canCBN "nna"
let canCBN = addCst canCBN "⊥"

let canCBN = appendGramProd canCBN "nno" (ABS (NTERM "nno"))
let canCBN = appendGramProd canCBN "nno" (NTERM "nne")    

let canCBN = appendGramProd canCBN "nne" (VAR)
let canCBN = appendGramProd canCBN "nne" (APP ((NTERM "nne"), (NTERM "nna")))

let canCBN = appendGramProd canCBN "nna" (BAG (NTERM "nno"))
let canCBN = appendGramProd canCBN "nna" (BAG (CST "⊥"))

let () = grammar_to_str canCBN
let () = print_newline ()




include Inhabitation

let () = print_endline ("\n*************** Examples of the Algorithm ***************\n")


(*** Type Contexts and Types of Examples ***)

(* Example 1 *)
let ctxtEx1 = emptyCtxt
let ctxtEx1 = ctxtAdd ctxtEx1 1 (tEmpty ++ (tSingleton (tSingleton (tVar 1))))
let typEx1 = (tVar 1)
(* Example 2 *)
let ctxtEx2 = emptyCtxt
let typEx2 = (tArrow (tEmpty ++ (tArrow (tSingleton (tVar 1)) (tVar 1))) (tArrow (tSingleton (tVar 1)) (tVar 1)))
(* Example 3 *)
let ctxtEx3 = emptyCtxt
let typEx3 = (tSingleton (tArrow (tSingleton (tArrow (tSingleton (tVar 1)) (tSingleton (tVar 1)))) (tSingleton (tArrow (tSingleton (tVar 1)) (tSingleton (tVar 1))))))
(* Example 4 *)
let ctxtEx4 = emptyCtxt
let typEx4 = (tArrow (tArrow tEmpty tEmpty) (tEmpty))
(* Example 5 *)
let ctxtEx5 = emptyCtxt
let ctxtEx5 = ctxtAdd ctxtEx5 1 (tSingleton (tArrow (tEmpty) (tVar 1)))
let typEx5 = tVar 1
(* Example 6 *)
let ctxtEx6 = emptyCtxt
let typEx6 = (tSingleton (tArrow (tSingleton (tVar 1)) (tSingleton (tVar 1))))

(*** Examples Call-by-Push-Value ***)

let () = print_endline ("\n\t***** Call-by-Push-Value *****\n")

let () = print_endline "  --- Example 1 ---"
let () = printInhResults (inhAlgo ~verbose:0 canCBPV ctxtEx1 typEx1)
let () = print_newline ()

let () = print_endline "  --- Example 2 ---"
let () = printInhResults (inhAlgo ~verbose:0 canCBPV ctxtEx2 typEx2)
let () = print_newline ()

let () = print_endline "  --- Example 3 ---"
let () = printInhResults (inhAlgo ~verbose:0 canCBPV ctxtEx3 typEx3)
let () = print_newline ()

let () = print_endline "  --- Example 4 ---"
let () = printInhResults (inhAlgo ~verbose:0 canCBPV ctxtEx4 typEx4)
let () = print_newline ()

let () = print_endline "  --- Example 5 ---"
let () = printInhResults (inhAlgo ~verbose:0 canCBPV ctxtEx5 typEx5)
let () = print_newline ()

let () = print_endline "  --- Example 6 ---"
let () = printInhResults (inhAlgo ~verbose:0 canCBPV ctxtEx6 typEx6)
let () = print_newline ()

(*** Examples Call-by-Name ***)

let () = print_endline ("\n\t***** Call-by-Name *****\n")

let () = print_endline "  --- Example 1 ---"
let () = printInhResults (cbnErasingMap (inhAlgo ~verbose:0 canCBN ctxtEx1 typEx1))
let () = print_newline ()

let () = print_endline "  --- Example 2 ---"
let () = printInhResults (cbnErasingMap (inhAlgo ~verbose:0 canCBN ctxtEx2 typEx2))
let () = print_newline ()

let () = print_endline "  --- Example 3 ---"
let () = printInhResults (cbnErasingMap (inhAlgo ~verbose:0 canCBN ctxtEx3 typEx3))
let () = print_newline ()

let () = print_endline "  --- Example 4 ---"
let () = printInhResults (cbnErasingMap (inhAlgo ~verbose:0 canCBN ctxtEx4 typEx4))
let () = print_newline ()

let () = print_endline "  --- Example 5 ---"
let () = printInhResults (cbnErasingMap (inhAlgo ~verbose:0 canCBN ctxtEx5 typEx5))
let () = print_newline ()

let () = print_endline "  --- Example 6 ---"
let () = printInhResults (cbnErasingMap (inhAlgo ~verbose:0 canCBN ctxtEx5 typEx6))
let () = print_newline ()

(*** Examples Call-by-Value ***)

let () = print_endline ("\n\t***** Call-by-Value *****\n")

let () = print_endline "  --- Example 1 ---"
let () = printInhResults (cbvErasingMap (inhAlgo ~verbose:0 canCBV ctxtEx1 typEx1))
let () = print_newline ()

let () = print_endline "  --- Example 2 ---"
let () = printInhResults (cbvErasingMap (inhAlgo ~verbose:0 canCBV ctxtEx2 typEx2))
let () = print_newline ()

let () = print_endline "  --- Example 3 ---"
let () = printInhResults (cbvErasingMap (inhAlgo ~verbose:0 canCBV ctxtEx3 typEx3))
let () = print_newline ()

let () = print_endline "  --- Example 4 ---"
let () = printInhResults (cbvErasingMap (inhAlgo ~verbose:0 canCBV ctxtEx4 typEx4))
let () = print_newline ()

let () = print_endline "  --- Example 5 ---"
let () = printInhResults (cbvErasingMap (inhAlgo ~verbose:0 canCBV ctxtEx5 typEx5))
let () = print_newline ()

let () = print_endline "  --- Example 6 ---"
let () = printInhResults (cbvErasingMap (inhAlgo ~verbose:0 canCBV ctxtEx6 typEx6))
let () = print_newline ()
