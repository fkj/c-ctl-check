namespace CCtlCheck.TransitionSystems

module Examples =
    open TSTypes

    /// This is the pen-and-paper example from the presentations
    let power1 : TransitionSystem<float> =
        {
            states = [0; 1; 2];
            nextStates = fun s -> match s with
                                  | 0 -> [1; 2]
                                  | 1 -> [2]
                                  | 2 -> [2]
                                  | _ -> failwith "WRONG NEXT STATE";
            propositions = Map [ ("v", [1.0; 2.0; 0.0] ) ];
        }

    /// This is the TMR in figure 6.3 in Baier and Katoen
    let tmr : TransitionSystem<bool> =
        {
            states = [0; 1; 2; 3; 4];
            nextStates = fun s -> match s with
                                  | 0 -> [0; 1; 4]
                                  | 1 -> [0; 1; 2; 4]
                                  | 2 -> [1; 2; 3; 4]
                                  | 3 -> [2; 3; 4]
                                  | 4 -> [0]
                                  | _ -> failwith "WRONG NEXT STATE";
            propositions = Map [ ("up3", [true;  false; false; false; false]);
                                 ("up2", [false; true;  false; false; false]);
                                 ("up1", [false; false; true;  false; false]);
                                 ("up0", [false; false; false; true;  false]);
                                 ("down",[false; false; false; false; true ])
                               ];
        }
