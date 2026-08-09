module Main

[<EntryPoint>]
let main (argv : string array) : int =
    match argv.[0] with
    | "Placeholder" -> Placeholder.main argv.[1..]
    | "CeqBranch" -> CeqBranch.main argv.[1..]
    | "TailCall" -> TailCall.main argv.[1..]
    | "AbstractDispatch" -> AbstractDispatch.main argv.[1..]
    | "ByrefDispatch" -> ByrefDispatch.main argv.[1..]
    | "SprintfBasic" -> SprintfBasic.main argv.[1..]
    | "UnionReflection" -> UnionReflection.main argv.[1..]
    | name -> failwith $"Unknown test case: {name}"
