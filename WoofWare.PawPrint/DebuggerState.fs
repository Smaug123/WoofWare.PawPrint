namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
type DebuggerState = | Detached

[<RequireQualifiedAccess>]
module DebuggerState =
    let isAttached (state : DebuggerState) : bool =
        match state with
        | DebuggerState.Detached -> false

    /// Whether an attached debugger has asked to receive `Debugger.Log` messages.
    /// CoreCLR answers false whenever no debugger is attached.
    let isLogging (state : DebuggerState) : bool =
        match state with
        | DebuggerState.Detached -> false
