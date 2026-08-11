namespace WoofWare.PawPrint

/// The element type of an array.
[<RequireQualifiedAccess>]
module ArrayElementType =
    /// The element type of an array with this shape.
    ///
    /// Total over the array-shaped handles and only those: an `ArrayShape` describes an
    /// array, so a `ConcreteType`/`Byref`/`Pointer`/`FunctionPointer` in that position is an
    /// interpreter bug rather than an input this can be asked about.
    let ofShape (shape : ArrayShape) : ConcreteTypeHandle =
        match shape.ConcreteType with
        | ConcreteTypeHandle.OneDimArrayZero element -> element
        | ConcreteTypeHandle.Array (element, _) -> element
        | ConcreteTypeHandle.Concrete _
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ ->
            failwith $"array object has non-array concrete type: %O{shape.ConcreteType}"
