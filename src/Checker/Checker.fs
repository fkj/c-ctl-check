namespace CCtlCheck.Checker

module Checker =
    open CCtlCheck.TransitionSystems.TSTypes
    open CCtlCheck.CCtl.CCtlTypes
    open CCtlCheck.ConstraintSemirings.CSRTypes
    open CCtlCheck.ConstraintSemirings.CSR

    open System.Collections.Generic

    let rec transpose = function
    | (_::_)::_ as m -> List.map List.head m :: transpose (List.map List.tail m)
    | _ -> []

    let rec checkCTL<'D when 'D : comparison>
        (M: TransitionSystem<'D>) (formula: Formula<'D>) (csr: CSR<'D>) : 'D list =
        match formula with
        | Zero -> List.replicate M.states.Length csr.bottom
        | One -> List.replicate M.states.Length csr.top
        | Proposition(p) -> Map.find p M.propositions
        | Function(fname,formulas) -> let func : 'D list -> 'D = Map.find fname csr.functions
                                      let valuations : 'D list list = List.map (fun f -> checkCTL<'D> M f csr) formulas
                                      let valuationColumns = transpose valuations
                                      in List.map func valuationColumns
        | Choose(f1,f2) -> let valuation1 : 'D list = checkCTL<'D> M f1 csr
                           let valuation2 : 'D list = checkCTL<'D> M f2 csr
                           in List.map2 (csr.choose2) valuation1 valuation2
        | Combine(f1,f2) -> let valuation1 : 'D list = checkCTL<'D> M f1 csr
                            let valuation2 : 'D list = checkCTL<'D> M f2 csr
                            in List.map2 (csr.combine) valuation1 valuation2
        | Temporal(q,f1,op,f2) -> let v1 : 'D list = checkCTL<'D> M f1 csr
                                  let v2 : 'D list = checkCTL<'D> M f2 csr
                                  let start : 'D = match op with
                                                   | Until -> csr.bottom
                                                   | Release -> csr.top
                                  let mutable v : 'D list = List.replicate M.states.Length start
                                  let mutable v' : 'D list = v
                                  while List.forall2 (=) v' v do
                                      v' <- v
                                      let formula : Formula<'D> =
                                          match op with
                                          | Until -> Choose(Valuation v2, Combine(Valuation v1, Next(q, Valuation v')))
                                          | Release -> Combine(Valuation v2, Choose(Valuation v1, Next(q, Valuation v')))
                                      v <- checkCTL<'D> M formula csr
                                  v
        | Next(q,f) -> let v' : 'D list = checkCTL M f csr
                       let quantifier : 'D -> 'D -> 'D = match q with
                                                         | Sum -> csr.choose2
                                                         | Product -> csr.combine
                                                         | Glb -> csr.combine
                       let start : 'D = match q with
                                        | Sum -> csr.bottom
                                        | Product -> csr.top
                                        | Glb -> csr.top
                       let v : State list = M.states
                       let computeState (s : State) : 'D =
                           let next : State list = M.nextStates s
                           List.fold (fun (result : 'D) (state : State) -> quantifier (v'.Item(state)) result) start next
                       in List.map (computeState) v
        | Valuation(v) -> v

module Examples =
    open Checker
    open CCtlCheck.ConstraintSemirings.Examples
    open CCtlCheck.CCtl.CCtlTypes
    open CCtlCheck.TransitionSystems.Examples
    
    let ex1 = checkCTL<float> power1 (Temporal(Sum, Zero, Until, Proposition("v"))) power
