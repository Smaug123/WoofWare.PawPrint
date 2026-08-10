module UnionReflection

open Microsoft.FSharp.Reflection

type Shape =
    | Point
    | Circle of radius : int
    | Rectangle of width : int * height : int

/// `Microsoft.FSharp.Reflection.FSharpType`/`FSharpValue` drive the whole F# union story at runtime:
/// structural equality/comparison fallbacks, `%A` formatting, and every serializer that understands
/// unions go through `GetUnionCases`/`GetUnionFields`/`MakeUnion`. Each of those walks the
/// `CompilationMappingAttribute` applications on the union's nested case types and then builds
/// reflected accessors for the case fields, so this exercises custom-attribute decoding, nested-type
/// enumeration, and `System.Reflection` member lookup together.
///
/// Each failure gets its own exit code so a partial implementation is distinguishable from a
/// total one.
let main (_argv : string array) : int =
    if not (FSharpType.IsUnion typeof<Shape>) then
        1
    else

    let cases = FSharpType.GetUnionCases typeof<Shape>

    if cases.Length <> 3 then
        2
    else

    let names = cases |> Array.map (fun case -> case.Name)

    if names <> [| "Point" ; "Circle" ; "Rectangle" |] then
        3
    else

    let tags = cases |> Array.map (fun case -> case.Tag)

    if tags <> [| 0 ; 1 ; 2 |] then
        4
    else

    let rectangleFields = cases.[2].GetFields () |> Array.map (fun field -> field.Name)

    if rectangleFields <> [| "width" ; "height" |] then
        5
    else

    // Round-trip a value through the reflected constructor and back out through the reader.
    let built = FSharpValue.MakeUnion (cases.[2], [| box 3 ; box 4 |]) |> unbox<Shape>

    if built <> Rectangle (3, 4) then
        6
    else

    let readCase, readFields = FSharpValue.GetUnionFields (built, typeof<Shape>)

    if readCase.Name <> "Rectangle" then
        7
    elif readFields <> [| box 3 ; box 4 |] then
        8
    else

    // A nullary case is represented differently (a singleton static property rather than a
    // constructor call), so check it separately.
    let point = FSharpValue.MakeUnion (cases.[0], [||]) |> unbox<Shape>

    if point <> Point then
        9
    else

    let pointCase, pointFields = FSharpValue.GetUnionFields (point, typeof<Shape>)

    if pointCase.Tag <> 0 then 10
    elif pointFields.Length <> 0 then 11
    else 0
