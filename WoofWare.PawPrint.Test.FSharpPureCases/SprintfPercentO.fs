module SprintfPercentO

type Shape =
    | Point
    | Circle of radius : int
    | Rectangle of width : int * height : int

/// Probe: does `%O` work on a discriminated union that has no custom `ToString`, i.e. one
/// relying on the compiler-generated reflective `ToString` FSharp.Core supplies?
let main (_argv : string array) : int =
    let nullary = sprintf "%O" Point

    if nullary <> "Point" then
        1
    else

    let single = sprintf "%O" (Circle 3)

    if single <> "Circle 3" then
        2
    else

    let multi = sprintf "%O" (Rectangle (3, 4))

    if multi <> "Rectangle (3, 4)" then 3 else 0
