namespace WoofWare.PawPrint

/// The inverse of the shapes `box` writes: what a boxed object logically holds, and whether a
/// byref to it addresses the value or a wrapper around it.
///
/// This lives apart from the `box`/`unbox` opcodes because the reflection primitives need the
/// same answer — `RuntimeFieldHandle_SetValue` unboxes the `object?` it is handed before storing
/// it in a field — and a second derivation of "what did `box` put in there" is exactly the kind
/// of copy that drifts.
[<RequireQualifiedAccess>]
module BoxedValue =
    /// `Some zero` exactly when `box` stored a *bare* primitive inside a synthetic single-field
    /// struct, `zero` being the zero of that primitive (whose size is the field's extent). `None`
    /// when the boxed storage is the value type's own fields — either because it is
    /// primitive-like (IntPtr, RuntimeTypeHandle, an enum, ...) and stays wrapped, or because it
    /// is a genuine value type.
    ///
    /// This distinction is what separates "a byref to the box addresses the value directly" from
    /// "it addresses a wrapper around the value", so both `contents` and the `unbox` opcode hang
    /// off it.
    let barePrimitiveShape
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (handle : ConcreteTypeHandle)
        (contents : CliValueType)
        (state : IlMachineState)
        : CliType option * IlMachineState
        =
        if contents.PrimitiveLikeKind.IsSome then
            None, state
        else
            let zero, state = IlMachineState.cliTypeZeroOfHandle state baseClassTypes handle

            match zero with
            | CliType.ValueType _ -> None, state
            | bare -> Some bare, state

    /// The CLI value logically held by a boxed object whose runtime type is `handle`. Callers must
    /// already have established that `contents.Declared = handle` — both `box` paths guarantee it,
    /// by constructing the heap object's contents with `CliValueType.OfFields ... handle`.
    ///
    /// Three shapes come back out, matching the three `box` writes:
    ///   - primitive-like (IntPtr, RuntimeTypeHandle, an enum, ...): keep it wrapped, since the
    ///     push path flattens it via the `PrimitiveLikeKind` invariant;
    ///   - a genuine multi-field value type: keep it wrapped;
    ///   - a bare primitive (Int32, Float64, ...), which `box` stored in a synthetic single-field
    ///     struct: read field 0 back by offset and size. `box` guarantees that shape, so this is a
    ///     nominal dereference rather than a structural guess.
    let contents
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (handle : ConcreteTypeHandle)
        (contents : CliValueType)
        (state : IlMachineState)
        : CliType * IlMachineState
        =
        let shape, state = barePrimitiveShape baseClassTypes handle contents state

        match shape with
        | None -> CliType.ValueType contents, state
        | Some zero ->
            let size = (CliType.SizeOf zero).Size
            CliValueType.DereferenceFieldAt 0 size contents, state
