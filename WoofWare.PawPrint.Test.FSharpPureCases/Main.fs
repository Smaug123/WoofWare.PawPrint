module Main

[<EntryPoint>]
let main (argv : string array) : int =
    match argv.[0] with
    | "Placeholder" -> Placeholder.main argv.[1..]
    | "BeqBranch" -> BeqBranch.main argv.[1..]
    | name -> failwith $"Unknown test case: {name}"
