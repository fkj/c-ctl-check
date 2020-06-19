namespace CCtlCheck.App

module Main =
    open Argu
    open System
    open CCtlCheck.App.Cli
    open CCtlCheck.Checker.Examples
    open CCtlCheck.CCtlParser.Parser
    open CCtlCheck.CCtl.CCtlTypes
    open CCtlCheck.ConstraintSemirings.Examples


    /// The idea here is to circumvent the type system by making the important parts non-generic
    let printResult (arguments: ParseResults<CLIArguments>) csr =
        match csr with
        | Bool -> let cCtlFormula = translateF<bool> (parseCCtl (arguments.GetResult CCtl))
                  in printfn "%s, %A" (cCtlFormula.ToString()) tmr1
        | Opt -> let cCtlFormula = translateF<float> (parseCCtl (arguments.GetResult CCtl))
                 in printfn "%s" (cCtlFormula.ToString())
        | MaxMin -> let cCtlFormula = translateF<float> (parseCCtl (arguments.GetResult CCtl))
                    in printfn "%s" (cCtlFormula.ToString())
        | Prob -> let cCtlFormula = translateF<float> (parseCCtl (arguments.GetResult CCtl))
                  in printfn "%s" (cCtlFormula.ToString())
        | Fuzzy -> let cCtlFormula = translateF<float> (parseCCtl (arguments.GetResult CCtl))
                   in printfn "%s" (cCtlFormula.ToString())
        | Power -> let cCtlFormula = translateF<float> (parseCCtl (arguments.GetResult CCtl))
                   in printfn "%s" (cCtlFormula.ToString())
        | Access -> let cCtlFormula = translateF<Set<AccessRights>> (parseCCtl (arguments.GetResult CCtl))
                    in printfn "%s" (cCtlFormula.ToString())

    [<EntryPoint>]
    let main argv =
        try
            let arguments = Cli.parser.Parse argv
            printResult arguments (arguments.GetResult CSR)
            0
        with e ->
            printfn "%s" e.Message
            2
