namespace CCtlCheck.CCtl

module CCtlTypes =
    open CCtlCheck.TransitionSystems.TSTypes
    open CCtlCheck.ConstraintSemirings.CSRTypes
    
    type Valuation<'D> = State -> 'D

    /// A formula in c-CTL over domain 'D is recursively defined over the following cases:
    type Formula<'D> =
        | Zero
        | One
        | Proposition of string
        | Function of string * Formula<'D> list
        | Choose of Formula<'D> * Formula<'D>
        | Combine of Formula<'D> * Formula<'D>
        | Temporal of Quantifier * Operator<'D>
        | Next of Quantifier * Formula<'D>
    and Quantifier =
        | Glb
        | Sum
        | Product
    and Operator<'D> =
        | Until of Formula<'D> * Formula<'D>
        | Release of Formula<'D> * Formula<'D>
