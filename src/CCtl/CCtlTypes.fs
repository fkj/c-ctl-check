namespace CCtlCheck.CCtl

module CCtlTypes =

    type State = string
    type Valuation<'D> = State -> 'D

    /// A formula in c-CTL over domain 'D is recursively defined over the following cases:
    type Formula<'D> =
        | Zero
        | One
        | Variable of string
        | Proposition of string
        | Function of string * Formula<'D> list
        | Choose of Formula<'D> * Formula<'D>
        | Combine of Formula<'D> * Formula<'D>
        | Quantifier of Quantifier * Temporal<'D>
        | Next of Quantifier * Formula<'D>
    and Quantifier =
        | Glb
        | Sum
        | Product
    and Temporal<'D> =
        | Temporal of Formula<'D> * Op * Op * Formula<'D>
    and Op =
        | Choose
        | Combine
