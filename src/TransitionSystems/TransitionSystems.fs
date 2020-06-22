namespace CCtlCheck.TransitionSystems

module Convert =
    open TSTypes

    let splitLast list =
        let last = List.last list
        let rest = list.[..list.Length-2]
        in (last, rest)

    let collectSystem (matrix, propositions) toMap =
        {
            states = [0 .. List.length (List.item 0 matrix) - 1];
            nextStates = fun s -> List.map System.Int32.Parse (List.item s matrix);
            propositions = toMap propositions;
        }

    let toMap<'D> parser (props: string list list) : Map<string, 'D list> =
      let convertList (prop: string list) : (string * 'D list) =
        let (name, valuation) = splitLast prop
        in (name, List.map parser valuation)
      in Map.ofList (List.map convertList props)

    let fromFloatFile data : TransitionSystem<float> =
      collectSystem data (toMap System.Double.Parse)

    let fromBoolFile data : TransitionSystem<bool> =
      collectSystem data (toMap System.Boolean.Parse)

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
