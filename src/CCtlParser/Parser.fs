namespace CCtlCheck.CCtlParser

module Parser =
    open FSharp.Text.Lexing
    open CCtlCheck.CCtlParser
    open CCtlCheck.CCtl.CCtlTypes
    
    let parseCCtl text =
        let lexbuf = LexBuffer<char>.FromString text
        CCtlParser.start CCtlLexer.tokenize lexbuf

    let translateQ (pre: PreQuantifier) : Quantifier =
        match pre with
        | PGlb -> Glb
        | PSum -> Sum
        | PProduct -> Product

    let translateO (pre: PreOperator) : Operator =
        match pre with
        | PUntil -> Until
        | PRelease -> Release

    let rec translateF<'D> (pre: PreFormula) : Formula<'D> =
        match pre with
        | PZero -> Zero
        | POne -> One
        | PProposition(s) -> Proposition(s)
        | PFunction(s,fs) -> Function(s, List.map translateF<'D> fs)
        | PChoose(f1,f2) -> Choose(translateF<'D> f1, translateF<'D> f2)
        | PCombine(f1,f2) -> Combine(translateF<'D> f1, translateF<'D> f2)
        | PTemporal(q,f1,op,f2) -> Temporal(translateQ q, translateF<'D> f1, translateO op, translateF<'D> f2)
        | PNext(q,f) -> Next(translateQ q, translateF<'D> f)
