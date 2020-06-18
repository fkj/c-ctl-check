namespace CCtlCheck.TransitionSystems

module TSTypes =
    type State = int
    
    type TransitionSystem<'D> =
        {
            states : State list;
            nextStates : State -> State list;
            propositions : Map<string, 'D list>;
        }
