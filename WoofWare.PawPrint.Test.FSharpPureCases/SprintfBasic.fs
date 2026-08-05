module SprintfBasic

/// Issue #690: the F# `sprintf` machinery (`Microsoft.FSharp.Core.Printf`) reflects over its own
/// format-handling methods with `RuntimeMethodHandle.IsGenericMethodDefinition` while building the
/// captured-argument closures for a format string. This exercises that path end to end.
let main (_argv : string array) : int =
    let s = sprintf "%d-%s-%b" 42 "hi" true

    if s <> "42-hi-true" then 1 else 0
