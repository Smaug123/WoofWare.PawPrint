module ByrefDispatch

/// Regression test for GitHub issue #692 ("getTypeOfObj on a ManagedPointer receiver").
///
/// #692 was reported against FSharp.Core's `MapEnumerator`1::DoMoveNext(byref<T>)` — an
/// abstract method with a *byref* parameter, dispatched virtually. The hypothesis (see the
/// issue thread) is that #692 was never a distinct bug: it was a symptom of #693 (virtual
/// dispatch using `Parameters.Length`, which can undercount an abstract declaration's true
/// arity, to find `this` on the eval stack). When the resolver peeks too few slots, it grabs
/// an *argument* instead of the receiver. `AbstractDispatch.fs` (the #693 regression test)
/// uses a plain `int` parameter, so a mis-peek there just silently dispatches on the wrong
/// operand -- `Int32` still satisfies `getTypeOfObj`, so the bug is invisible without manual
/// inspection. A *byref* argument is different: `getTypeOfObj` has no case for
/// `EvalStackValue.ManagedPointer`, so mis-peeking a byref argument as `this` fails loudly
/// with exactly #692's error rather than quietly dispatching wrong. This file exists to pin
/// that byref-specific shape, which `AbstractDispatch.fs` does not cover.
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
