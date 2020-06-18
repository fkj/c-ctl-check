namespace CCtlCheck.TransitionSystems

module TS =
    open TSTypes
    
    let nextStates (s: State) : State list = []
    let numberOfStates (M: TransitionSystem) : int = 1
    let states (M: TransitionSystem) : State list = []
    let getProposition (M: TransitionSystem) : Map<string, 'D list> = Map.empty
