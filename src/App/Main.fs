namespace CCtlCheck.App

module Main =
    open Argu
    open CCtlCheck
    open CCtlCheck.App.Cli
    open CCtlCheck.CCtlParser.Parser
    open CCtlCheck.ConstraintSemirings.Examples
    open CCtlCheck.Checker.Checker

    let computeBool (arguments: ParseResults<CLIArguments>) csr =
        let cCtlFormula = translateF<bool> (parseCCtl (arguments.GetResult CCtl))
        let ts = TransitionSystems.Convert.fromBoolFile (TSParser.Parser.parse (arguments.GetResult TS))
        let result = checkCTL<bool> ts cCtlFormula csr
        (ts, result)

    let computeFloat (arguments: ParseResults<CLIArguments>) csr =
        let cCtlFormula = translateF<float> (parseCCtl (arguments.GetResult CCtl))
        let ts = TransitionSystems.Convert.fromFloatFile (TSParser.Parser.parse (arguments.GetResult TS))
        let result = checkCTL<float> ts cCtlFormula csr
        (ts, result)

    let printFinal result =
        printfn "\nFinal result: %A" (snd result)

    /// The idea here is to circumvent the type system by making the important parts non-generic
    /// This is necessary because the CSRs have different types which are determined at runtime
    let printResult (arguments: ParseResults<CLIArguments>) csr =
        match csr with
        | Bool -> computeBool arguments boolean |> printFinal
        | Opt -> computeFloat arguments optimization |> printFinal
        | MaxMin -> computeFloat arguments maxMin |> printFinal
        | Prob -> computeFloat arguments probabilistic |> printFinal
        | Fuzzy -> computeFloat arguments fuzzy |> printFinal
        | Power -> computeFloat arguments power |> printFinal
        | Access -> printfn "Not implemented yet..."

    [<EntryPoint>]
    let main argv =
        try
            let arguments = Cli.parser.Parse argv
            printResult arguments (arguments.GetResult CSR)
            0
        with e ->
            printfn "%s" e.Message
            2
