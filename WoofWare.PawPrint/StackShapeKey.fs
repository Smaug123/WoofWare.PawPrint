namespace WoofWare.PawPrint

/// Which method a stack-shape analysis describes. A body read from a PE image is analysed once
/// per definition: how many values each of its instructions pops and pushes is the same at
/// every instantiation. A minted dynamic method is its own.
[<RequireQualifiedAccess>]
type StackShapeKey =
    | Metadata of assemblyFullName : string * methodRow : int
    | Dynamic of DynamicMethodHandle
