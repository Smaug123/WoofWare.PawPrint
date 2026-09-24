namespace WoofWare.PawPrint

open Microsoft.Extensions.Logging

/// The outcome of the type test that ECMA-335 III.4.32 (`unbox`) and the value-type arm of
/// III.4.33 (`unbox.any`) share; CoreCLR routes both through `CastHelpers.Unbox_Helper`.
[<RequireQualifiedAccess>]
type UnboxTypeTest =
    /// The operand is a boxed value whose type the token accepts. Materialise from
    /// `boxed.ConcreteType` rather than from the token's handle: under the enum/underlying
    /// relaxation in `unboxPermitted` the two differ, and `Contents` was built with the former.
    | Accepted of addr : ManagedHeapAddress * boxed : AllocatedNonArrayObject
    /// The operand is null. `unbox` and the non-Nullable arm of `unbox.any` both raise
    /// NullReferenceException; only the `Nullable<T>` arm of `unbox.any` accepts null, and it
    /// never reaches this test.
    | NullOperand
    /// InvalidCastException: the operand is not a boxed value type the token accepts.
    | WrongType

/// What ECMA-335 III.4.32's `unbox` produces from its operand.
[<RequireQualifiedAccess>]
type UnboxAddress =
    /// The managed pointer `unbox` pushes. It aliases the box: writes through it are visible
    /// through every reference to the box.
    | Address of ManagedPointerSource
    /// The instruction faults instead, and pushes nothing.
    | Faulted of OpcodeFault

/// The inverse of the shapes `box` writes: what a boxed object logically holds, and whether a
/// byref to it addresses the value or a wrapper around it.
///
/// This lives apart from the `box`/`unbox` opcodes because the reflection primitives need the
/// same answer — `RuntimeFieldHandle_SetValue` unboxes the `object?` it is handed before storing
/// it in a field, and `Unsafe.Unbox<T>` is the `unbox` instruction under another name — and a
/// second derivation of "what did `box` put in there" is exactly the kind of copy that drifts.
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

    /// Shared by `unbox` and the value-type arm of `unbox.any`, so the two cannot drift apart on
    /// which operands they accept. `opName` appears only in diagnostics for shapes we do not model.
    let unboxTypeTest
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (opName : string)
        (targetConcreteTypeHandle : ConcreteTypeHandle)
        (actualObj : EvalStackValue)
        (state : IlMachineState)
        : IlMachineState * UnboxTypeTest
        =
        match actualObj with
        | EvalStackValue.NullObjectRef -> state, UnboxTypeTest.NullOperand
        | EvalStackValue.ObjectRef addr ->
            let boxedOpt =
                match ManagedHeap.tryGet addr state.ManagedHeap with
                | Some v -> Some v
                | None ->
                    // An array is never a boxed value type, so per the CLR this is an ordinary
                    // type mismatch rather than an interpreter abort.
                    if ManagedHeap.isArray addr state.ManagedHeap then
                        None
                    else
                        failwith $"%s{opName}: could not find managed object with address {addr}"

            match boxedOpt with
            | None -> state, UnboxTypeTest.WrongType
            | Some boxed ->
                // Handle identity, or same-primitive-element-type per CoreCLR
                // `CastHelpers.Unbox_Helper` — the clause that lets a boxed enum unbox to its
                // underlying integer and back. Not assignability, and narrower than ECMA-335's
                // verification types: see `unboxPermitted`.
                let state, permitted =
                    IlMachineState.unboxPermitted
                        loggerFactory
                        baseClassTypes
                        state
                        boxed.ConcreteType
                        targetConcreteTypeHandle

                if permitted then
                    state, UnboxTypeTest.Accepted (addr, boxed)
                else
                    state, UnboxTypeTest.WrongType
        | other -> failwith $"%s{opName}: unexpected eval stack value {other}"

    /// ECMA-335 III.4.32 (`unbox`) with type token `targetConcreteTypeHandle`, applied to
    /// `operand`: the address of the boxed value, or the fault the instruction raises instead.
    ///
    /// This is the whole of the instruction bar its effect on the evaluation stack, so that
    /// `Unsafe.Unbox<T>` — whose body the runtime replaces with `ldarg.0; unbox !!T; ret` — can
    /// share it rather than restate it. `opName` appears only in diagnostics.
    ///
    /// A `Nullable<T>` token, and a token that is not a nominal value type, are refused with a
    /// host failure rather than answered.
    let unboxAddress
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (opName : string)
        (targetConcreteTypeHandle : ConcreteTypeHandle)
        (operand : EvalStackValue)
        (state : IlMachineState)
        : IlMachineState * UnboxAddress
        =
        // Unlike `unbox.any`, whose token may denote any boxable type, III.4.32 requires a value
        // type, and no structural handle shape is one: arrays are reference types and byrefs are
        // not boxable at all. A *pointer* token is unverifiable rather than invalid — real .NET
        // reaches the instruction for `unbox int*` (measured) — but supporting it needs the boxed
        // pointer `box` does not implement, so all of these are refused together. Dispatch on the
        // shape before touching metadata, since these handles have no row in `AllConcreteTypes`.
        match targetConcreteTypeHandle with
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ ->
            failwith
                $"TODO: %s{opName} of %O{targetConcreteTypeHandle} is not implemented. Arrays and byrefs are invalid IL here per ECMA-335 III.4.32; a pointer token is legal on real .NET (measured) and needs the boxed pointer `box` does not implement."
        | ConcreteTypeHandle.Concrete _ ->

        let targetConcreteType =
            AllConcreteTypes.lookup targetConcreteTypeHandle state.ConcreteTypes
            |> Option.get

        let targetDefn =
            (state._LoadedAssemblies.ByDefinitionName targetConcreteType.AssemblyFullName)
                .TypeDefs.[targetConcreteType.Definition.Get]

        // `Nullable<T>` is a value type, so test for it before the general value-type check.
        if InternalTypeKind.kind baseClassTypes targetConcreteType = InternalTypeKind.Nullable then
            // `box` of a `Nullable<T>` yields null or a boxed `T`, so there is no `Nullable<T>` in
            // the heap for a pointer to point *into*. CoreCLR resolves that by materialising a
            // fresh `Nullable<T>` into a JIT temp and pushing the temp's address
            // (jit/importer.cpp, `CEE_UNBOX` with `CORINFO_HELP_UNBOX_NULLABLE`), which the JIT
            // itself flags as non-compliant with ECMA-335: the result aliases a copy, so writes
            // through it are lost. Modelling that needs a storage location for the temp, which
            // this interpreter has no notion of at this point; rather than guess at one, refuse
            // loudly. Roslyn never emits this shape — it compiles `(T?) o` to
            // `unbox.any; stloc; ldloca` — and `Unsafe.Unbox<T>`'s `T : struct` constraint
            // excludes `Nullable<T>`, so reaching this is a signal that some other IL producer
            // needs the temp modelled properly.
            failwith
                $"TODO: %s{opName} with a System.Nullable`1 type token (%O{targetConcreteTypeHandle}) is unimplemented; CoreCLR would push the address of a materialised copy rather than a pointer into the box"

        if not (DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies targetDefn) then
            failwith
                $"%s{opName}: type token denotes reference type %O{targetConcreteTypeHandle}, but ECMA-335 III.4.32 requires a value type; this is invalid IL"

        let state, typeTest =
            unboxTypeTest loggerFactory baseClassTypes opName targetConcreteTypeHandle operand state

        match typeTest with
        | UnboxTypeTest.NullOperand -> state, UnboxAddress.Faulted OpcodeFault.NullReference
        | UnboxTypeTest.WrongType -> state, UnboxAddress.Faulted OpcodeFault.InvalidCast
        | UnboxTypeTest.Accepted (addr, boxed) ->
            let barePrimitive, state =
                barePrimitiveShape baseClassTypes boxed.ConcreteType boxed.Contents state

            match barePrimitive with
            | None when boxed.ConcreteType = targetConcreteTypeHandle ->
                // The box's storage is the target type's own fields, and `HeapValue` denotes the
                // whole boxed value (see `CellAwareMemOps`), so every consumer — `ldind`, `ldobj`,
                // `ldfld`, `ldflda`, `stobj`, `stfld` — finds exactly the value it expects there.
                state, UnboxAddress.Address (ManagedPointerSource.Byref (ByrefRoot.HeapValue addr, []))
            | Some _
            | None ->
                // Either the box holds a bare primitive, which `box` wrapped in a single-field
                // struct, or the type test accepted a different type under the relaxation, which
                // admits only primitives and enums reporting the same primitive element type. In
                // both cases the box's storage is one field holding the value itself — the
                // primitive's own backing field (`System.Int32::m_value`) or an enum's `value__` —
                // and that cell holds a value of `T`'s primitive element type, which is what the two
                // types agree on. Where the cell's own type is not `T` (a boxed int unboxed as an
                // enum, or one enum's box as another), the byref still views it as `T`, so `T`'s
                // own members — an enum's `value__` — resolve through it.
                match CliValueType.TryAllFields boxed.Contents with
                | [ field ] ->
                    let field = CliConcreteField.ToCliField field

                    let cell =
                        ManagedPointerSource.Byref (ByrefRoot.HeapValue addr, [ ByrefProjection.Field field.Id ])

                    let address =
                        if field.Type = targetConcreteTypeHandle then
                            cell
                        else
                            ManagedPointerSource.reinterpretAs targetConcreteType cell

                    state, UnboxAddress.Address address
                | fields ->
                    failwith
                        $"%s{opName}: box of %O{boxed.ConcreteType} accepted as %O{targetConcreteTypeHandle} must hold exactly one field, the value itself, but holds %d{fields.Length}"
