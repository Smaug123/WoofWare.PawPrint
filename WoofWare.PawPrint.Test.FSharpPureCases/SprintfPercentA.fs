module SprintfPercentA

type Shape =
    | Point
    | Circle of radius : int
    | Rectangle of width : int * height : int

/// Probe: does `%A` work on a discriminated union? `%A` goes through
/// `Microsoft.FSharp.Text.StructuredPrintfImpl`, which reflects over the union's cases.
let main (_argv : string array) : int =
    let nullary = sprintf "%A" Point

    if nullary <> "Point" then
        1
    else

    let single = sprintf "%A" (Circle 3)

    if single <> "Circle 3" then
        2
    else

    let multi = sprintf "%A" (Rectangle (3, 4))

    if multi <> "Rectangle (3, 4)" then 3 else 0
