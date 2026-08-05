module AbstractDispatch

/// Reproduces GitHub issue #693: an abstract member *declaration* gets zero Param-table
/// rows from FSC (it has no name/default-value/marshalling metadata to record), even
/// though its signature has one declared parameter. Virtual dispatch that used
/// `Parameters.Length` to find `this` on the eval stack would peek the wrong slot here
/// and dispatch on the argument instead of the receiver.
[<AbstractClass>]
type Base () =
    abstract member Combine : int -> int

type Derived (seed : int) =
    inherit Base ()
    override _.Combine x = seed + x

let main (_argv : string array) : int =
    let b : Base = Derived 40
    b.Combine 2
