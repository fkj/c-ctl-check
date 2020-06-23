namespace CCtlCheck.CCtl

module CCtlTypes =
    open CCtlCheck.TransitionSystems.TSTypes
    open CCtlCheck.ConstraintSemirings.CSRTypes
    
    type Valuation<'D> = State -> 'D

    /// Parsed c-CTL formulas with no domain, which are later translated into propert Formulas
    type PreFormula =
        | PZero
        | POne
        | PProposition of string
        | PFunction of string * PreFormula list
        | PChoose of PreFormula * PreFormula
        | PCombine of PreFormula * PreFormula
        | PTemporal of PreQuantifier * PreFormula * PreOperator * PreFormula
        | PNext of PreQuantifier * PreFormula
    and PreQuantifier =
        | PGlb
        | PSum
        | PProduct
    and PreOperator =
        | PUntil
        | PRelease

    /// A formula in c-CTL over domain 'D is recursively defined over the following cases:
    type Formula<'D> =
        | Zero
        | One
        | Proposition of string
        | Function of string * Formula<'D> list
        | Choose of Formula<'D> * Formula<'D>
        | Combine of Formula<'D> * Formula<'D>
        | Temporal of Quantifier * Formula<'D> * Operator * Formula<'D>
        | Next of Quantifier * Formula<'D>
        | Valuation of 'D list
    with override this.ToString() =
           let commaList xs =
             let commaSep x s =
                match (x,s) with
                | (x,"") -> x
                | (x,p) -> x + ", " + p
             in List.foldBack (commaSep) xs ""
           in match this with
              | Zero -> "\U0001D7CE"
              | One -> "\U0001D7CF"
              | Proposition(s) -> s
              | Function(s,fs) -> s + "(" + commaList (List.map (fun x -> x.ToString()) fs) + ")"
              | Choose(f1,f2) -> f1.ToString() + " + " + f2.ToString()
              | Combine(f1,f2) -> f1.ToString() + " \U000000D7 " + f2.ToString()
              | Temporal(q,f1,op,f2) -> q.ToString() + "(" + f1.ToString() + " " + op.ToString() + " " + f2.ToString() + ")"
              | Next(q,f) -> q.ToString() + "(" + f.ToString() + ")"
              | Valuation(ds) -> "[" + commaList (List.map (fun x -> x.ToString()) ds) + "]"
    and Quantifier =
        | Glb
        | Sum
        | Product
    with override this.ToString() =
          match this with
          | Glb -> "\U00002293"
          | Sum -> "\U00002211"
          | Product -> "\U0000220F"
    and Operator =
        | Until
        | Release
    with override this.ToString() =
          match this with
          | Until -> "U"
          | Release -> "R"
