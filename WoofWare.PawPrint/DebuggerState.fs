namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
type DebuggerState = | Detached

[<RequireQualifiedAccess>]
module DebuggerState =
    let isAttached (state : DebuggerState) : bool =
        match state with
        | DebuggerState.Detached -> false
