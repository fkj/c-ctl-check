namespace CCtlCheck.TransitionSystems

module Examples =
    open TSTypes

    let power1 : TransitionSystem<float> =
        {
            states = [0; 1; 2;]
            nextStates = fun s -> match s with
                                  | 0 -> [1; 2]
                                  | 1 -> [2]
                                  | 2 -> [2]
                                  | _ -> failwith "WRONG NEXT STATE";
            propositions = Map [ ("v", [1.0; 2.0; 0.0] ) ];
        }
