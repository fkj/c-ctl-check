namespace CCtlCheck.App

module Cli =
    open Argu

    type CSR =
        | Bool
        | Opt
        | MaxMin
        | Prob
        | Fuzzy
        | Power
        | Access

    type CLIArguments =
        | [<ExactlyOnce>] TS of path : string
        | [<MainCommand; ExactlyOnce>] CCtl of cctl : string
        | [<ExactlyOnce>] CSR of CSR
    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | TS _ -> "the path of the transition system to use."
                | CCtl _ -> "the c-CTL property to evaluate."
                | CSR _ -> "the constraint semiring to use for the evaluation."

    let parser = ArgumentParser.Create<CLIArguments>(programName = "cctlcheck.exe")
