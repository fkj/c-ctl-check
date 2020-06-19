namespace CCtlCheck.App

module Main =
    open Argu
    open System
    open CCtlCheck.App.Cli
    open CCtlCheck.Checker.Examples

    [<EntryPoint>]
    let main argv =
        try
            let arguments = Cli.parser.Parse argv
            printfn "Iteration:"
            printfn "Final result: %A" tmr1
            0
        with e ->
            printfn "%s" e.Message
            2
