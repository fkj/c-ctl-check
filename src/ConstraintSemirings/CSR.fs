namespace CCtlCheck.ConstraintSemirings

module CSR =
    /// The partial order induced by the choice operator
    let below (c: CSR<'D>) (a, b: 'D) : bool = c.choose (set [a; b]) = b
    
    /// The Cartesian product of two c-semirings is also a c-semiring
    let product (c1: CSR<'D1>) (c2: CSR<'D2>) : CSR<'D1 * 'D2> =
        {
            choose = fun s -> (c1.choose (Set.map fst s), c2.choose (Set.map snd s));
            combine = fun x y -> (c1.combine (fst x) (fst y), c2.combine (snd x) (snd y));
            bottom = (c1.bottom, c2.bottom);
            top = (c1.top, c2.top);
            functions = Map.empty; // TODO: fix this so additional functions are not removed
        }

    type CSR<'D when 'D : comparison> with
        member this.choose2 (x : 'D) (y : 'D) =
            this.choose (set [x; y])

module Examples =
    
    /// The Boolean c-semiring implements usual model checking
    /// Distributive
    let boolean : CSR<bool> =
        {
            choose = fun a -> Set.fold (fun x y -> x || y) false a;
            combine = fun x y -> x && y;
            bottom = false;
            top = true;
            functions = Map [("not", fun xs -> match xs with [x] -> not x | _ -> failwith "WRONG APPLICATION OF AUX FUNCTION NOT")];
        }

    /// The optimization c-semiring can be used for e.g. pricing or delays
    /// Non-distributive
    let optimization : CSR<float> =
        {
            choose = fun a -> Set.fold min infinity a;
            combine = fun x y -> x + y;
            bottom = infinity;
            top = 0.0;
            functions = Map.empty;
        }

    /// The max/min c-semiring can be used for e.g. bandwidth
    /// Distributive
    let maxMin : CSR<float> =
        {
            choose = fun a -> Set.fold max 0.0 a;
            combine = min;
            bottom = 0.0;
            top = infinity;
            functions = Map.empty;
        }

    /// The probabilistic c-semiring can be used for e.g. performance or rates
    /// Non-distributive
    let probabilistic : CSR<float> =
        {
            choose = fun a -> Set.fold max 0.0 a;
            combine = fun x y -> x * y;
            bottom = 0.0;
            top = 1.0;
            functions = Map.empty;
        }

    /// The fuzzy c-semiring can be used for e.g. performance or rates
    /// Distributive
    let fuzzy : CSR<float> =
        {
            choose = fun a -> Set.fold max 0.0 a;
            combine = min;
            bottom = 0.0;
            top = 1.0;
            functions = Map.empty;
        }

    /// The power consumption c-semiring can be used to determine power used
    /// Distributive
    let power : CSR<float> =
        {
            choose = fun a -> Set.fold min infinity a;
            combine = max;
            bottom = infinity;
            top = 0.0;
            functions = Map.empty;
        }

    /// The set-based c-semiring can be instantiated with e.g. capabilities or access rights
    /// Distributive
    let setCSR<'S when 'S : comparison> (s : Set<'S>) : CSR<Set<'S>> =
        {
            choose = fun a -> Set.fold (Set.union) Set.empty a;
            combine = Set.intersect;
            bottom = Set.empty;
            top = s;
            functions = Map.empty;
        }

    /// Here is a very basic access rights construction
    type AccessRights =
        | Confidential
        | Public

    let allRights = set [Confidential; Public]

    let accessRights : CSR<Set<AccessRights>> = setCSR allRights
