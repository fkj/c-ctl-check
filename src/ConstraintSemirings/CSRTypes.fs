namespace CCtlCheck.ConstraintSemirings

[<AutoOpen>]
module CSRTypes =
    /// A constraint semiring is defined over a domain, and contains the following elements:
    ///   - An additive projection operator, project/+
    ///   - A multiplicative combination operator, combine/*
    ///   - A least element bottom/0
    ///   - A largest element top/1
    ///
    /// The following properties must hold of the multiplicative operator:
    ///   - Binary associative
    ///   - Commutative
    ///   - Distributes over the additive operator
    ///   - Has top as its unit element
    ///   - Has bottom as its absorbing element
    ///
    /// If the multiplicative operator is also idempotent, the CSR is a distributive CSR.
    /// This is required for the CTL model checking problem to be decidable.
    type CSR<'D> when 'D : comparison =
        {
            project: Set<'D> -> 'D;
            combine: Set<'D> -> 'D;
            bottom: 'D;
            top: 'D
        }
