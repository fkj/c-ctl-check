namespace CCtlCheck.ConstraintSemirings

module CSR =
    /// The Cartesian product of two c-semirings is also a c-semiring
    let product (c1: CSR<'D1>) (c2: CSR<'D2>) : CSR<'D1 * 'D2> =
        {
            project = fun s -> (c1.project (Set.map fst s), c2.project (Set.map snd s));
            combine = fun s -> (c1.combine (Set.map fst s), c2.combine (Set.map snd s));
            bottom = (c1.bottom, c2.bottom);
            top = (c1.top, c2.top);
        }
