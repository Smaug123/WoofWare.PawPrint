module ByrefDispatch

/// Regression test for GitHub issue #692 ("getTypeOfObj on a ManagedPointer receiver").
///
/// #692 was reported against FSharp.Core's `MapEnumerator`1::DoMoveNext(byref<T>)` — an
/// abstract method with a *byref* parameter, dispatched virtually. The hypothesis (see the
/// issue thread) is that #692 was never a distinct bug: it was a symptom of #693 (virtual
/// dispatch using `Parameters.Length`, which can undercount an abstract declaration's true
/// arity, to find `this` on the eval stack). When the resolver peeks too few slots, it grabs
/// an *argument* instead of the receiver. Virtual dispatch refuses a receiver that is not an
/// object reference, so a mis-peek fails loudly whatever the argument's shape.
/// `AbstractDispatch.fs` (the #693 regression test) uses a plain `int` parameter; this file pins
/// the byref-parameter shape that #692 was reported against, which `AbstractDispatch.fs` does not
/// cover.
[<AbstractClass>]
type Base () =
    abstract member Bump : byref<int> -> int

type Derived (seed : int) =
    inherit Base ()
    override _.Bump (x : byref<int>) = seed + x

let main (_argv : string array) : int =
    let b : Base = Derived 40
    let mutable v = 2
    b.Bump &v
