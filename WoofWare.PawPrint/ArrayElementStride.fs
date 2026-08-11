namespace WoofWare.PawPrint

/// The byte distance between consecutive cells of an array.
[<RequireQualifiedAccess>]
module ArrayElementStride =
    /// The element type of an array with this shape.
    let elementHandle (shape : ArrayShape) : ConcreteTypeHandle =
        match shape.ConcreteType with
        | ConcreteTypeHandle.OneDimArrayZero element -> element
        | ConcreteTypeHandle.Array (element, _) -> element
        | ConcreteTypeHandle.Concrete _
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ ->
            failwith $"array object has non-array concrete type: %O{shape.ConcreteType}"

    /// The byte distance between consecutive cells of an array with this shape.
    ///
    /// Derived from the element type alone, never by measuring a stored cell. The stride
    /// is a property of the array's *type*: CoreCLR fixes it when the array type is laid
    /// out, and no store into a cell can change it. PawPrint happens to keep a `CliType`
    /// per cell, so it could instead measure cell 0 — but that is a read of guest memory
    /// to answer a question about a type, which would show up as a spurious access to
    /// anything watching the heap, and it has no answer at all for an empty array. That
    /// is why the call sites this replaces all had to carry a second branch for
    /// `Length = 0`.
    ///
    /// Uses the non-loading walk: the array exists, so whatever allocated it already ran
    /// a state-threading `zeroOf` over this very element handle to produce its zero
    /// element. See `IAssemblyLoad.alreadyLoadedOnly`.
    let ofShape (baseClassTypes : BaseClassTypes<DumpedAssembly>) (state : IlMachineState) (shape : ArrayShape) : int =
        let zero, _, _ =
            CliType.zeroOf
                IAssemblyLoad.alreadyLoadedOnly
                state.ConcreteTypes
                state._LoadedAssemblies
                baseClassTypes
                (elementHandle shape)

        CliType.sizeOf zero

    // Four call sites still measure cell 0 instead of calling this, because they sit in
    // functions that do not carry `BaseClassTypes` and cannot get it without threading it
    // through `readArrayBytesAs`, `writeArrayBytes` and
    // `NullaryIlOp.tryManagedPointerAddressBits` — the last of which currently needs only
    // `(state, ptr)`. A fifth, `tryWriteArrayElementPrecise`, holds it only as an option,
    // following `IlMachineManagedByref`'s existing convention that metadata-light entry
    // points pass `None`. Migrating those is a separate change with a real blast radius;
    // they are marked `STRIDE-FROM-CELL` so they can be found.

    /// The byte distance between consecutive cells of the array at `addr`. Fails if
    /// `addr` is not a live array.
    let ofAddress
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (addr : ManagedHeapAddress)
        : int
        =
        ofShape baseClassTypes state (ManagedHeap.getArrayShape addr state.ManagedHeap)
