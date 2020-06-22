namespace CCtlCheck.TSParser

module Parser =
    open System.IO
    open System

    let splitSystem (data: string list list) =
        let rec loop = function
        | (_,[],x,y) -> (x, y)
        | (false,h::t,x,y) -> if List.isEmpty h
                              then loop(true, t, x, y)
                              else loop(false, t, h::x, y)
        | (true,h::t,x,y) -> loop(true, t, x, h::y)
        let reverseFirst = (fun (x,y) -> (List.rev x, y))
        in reverseFirst (loop(false, data, [], []))

    let parse (path: string) =
        let lines = Seq.toList (File.ReadAllLines(path))
        let words = List.map (fun (line: string) -> Seq.toList (line.Split((null: char[]), StringSplitOptions.RemoveEmptyEntries))) lines
        splitSystem words
