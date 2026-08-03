module TailCall

/// Mutual recursion: FSC cannot rewrite these into a loop, so with `--tailcalls+`
/// (the Release default) it emits the `tail.` prefix on the cross-calls.
let rec private isEven (n : int) : bool = if n = 0 then true else isOdd (n - 1)

and private isOdd (n : int) : bool = if n = 0 then false else isEven (n - 1)

/// A tail call through a first-class function value: FSC emits `tail. callvirt`
/// on `FSharpFunc<_,_>::Invoke`. `NoInlining` keeps the optimiser from inlining
/// the body into the caller, where the call would no longer be in tail position
/// and the prefix would vanish.
[<System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)>]
let private applyTail (f : int -> int) (x : int) : int = f x

let private double (x : int) : int = x * 2

let private triple (x : int) : int = x * 3

let main (argv : string array) : int =
    // Choose the function at runtime so FSC can't inline `applyTail` down to a
    // constant and delete the `tail. callvirt` we're trying to exercise.
    let chosen : int -> int = if argv.Length > 0 then triple else double

    if not (isEven 10) then 1
    elif isEven 11 then 2
    elif not (isOdd 7) then 3
    elif isOdd 8 then 4
    elif applyTail chosen 21 <> 42 then 5
    else 0
