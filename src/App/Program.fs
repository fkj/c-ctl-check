module CCtlCheck.App

open System
open CCtlCheck.Checker.Examples

[<EntryPoint>]
let main argv =
    printfn "Iteration:"
    printfn "Final result: %A" tmr1
    0 // return an integer exit code
