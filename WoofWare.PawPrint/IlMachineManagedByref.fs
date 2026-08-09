namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open System.Reflection.Metadata

[<RequireQualifiedAccess>]
module IlMachineManagedByref =
    /// `true` when a `ReinterpretAs ty` projection against a value of the given
    /// shape can be treated as a no-op. Matches same-width primitive reinterprets
    /// within the integer family (including signed<->unsigned and char<->ushort
    /// pairs, which share bit patterns and round-trip through the Int32 stack
    /// slot with modular narrowing) and within the float family (same width
    /// only). Rejects float<->int bit reinterprets, overlay structs, enum
    /// underlying coercions, and any size change; those still need a proper
    /// bytewise implementation.
    let private classifyValueForReinterpret (value : CliType) : (string * int) voption =
        match value with
        | CliType.Bool _ -> ValueSome ("int", 1)
        | CliType.Char _ -> ValueSome ("int", 2)
        | CliType.Numeric (CliNumericType.Int8 _) -> ValueSome ("int", 1)
        | CliType.Numeric (CliNumericType.UInt8 _) -> ValueSome ("int", 1)
        | CliType.Numeric (CliNumericType.Int16 _) -> ValueSome ("int", 2)
        | CliType.Numeric (CliNumericType.UInt16 _) -> ValueSome ("int", 2)
        | CliType.Numeric (CliNumericType.Int32 _) -> ValueSome ("int", 4)
        | CliType.Numeric (CliNumericType.Int64 _) -> ValueSome ("int", 8)
        | CliType.Numeric (CliNumericType.Float32 _) -> ValueSome ("float", 4)
        | CliType.Numeric (CliNumericType.Float64 _) -> ValueSome ("float", 8)
        | _ -> ValueNone

    let private classifyTypeForReinterpret (ty : ConcreteType<ConcreteTypeHandle>) : (string * int) voption =
        if ty.Namespace <> "System" then
            ValueNone
        else
            match ty.Name with
            | "Boolean"
            | "SByte"
            | "Byte" -> ValueSome ("int", 1)
            | "Int16"
            | "UInt16"
            | "Char" -> ValueSome ("int", 2)
            | "Int32"
            | "UInt32" -> ValueSome ("int", 4)
            | "Int64"
            | "UInt64" -> ValueSome ("int", 8)
            | "Single" -> ValueSome ("float", 4)
            | "Double" -> ValueSome ("float", 8)
            | _ -> ValueNone

    let private isSafeReinterpretPassthrough (value : CliType) (ty : ConcreteType<ConcreteTypeHandle>) : bool =
        match classifyValueForReinterpret value, classifyTypeForReinterpret ty with
        | ValueSome v, ValueSome t -> v = t
        | _ -> false

    let private bytesEqual (left : byte[]) (right : byte[]) : bool =
        if left.Length <> right.Length then
            false
        else
            let mutable equal = true
            let mutable i = 0

            while equal && i < left.Length do
                equal <- left.[i] = right.[i]
                i <- i + 1

            equal

    /// Constructor-level shape comparison: true iff `a` and `b` share the same
    /// top-level `CliType` constructor (and, for `Numeric`, the same
    /// `CliNumericType` constructor). Used as the shared primitive by both the
    /// strict and the widened comparators below; not called directly by the
    /// fast-path sites. Two value-type cells of the same size could still be
    /// wholly unrelated structures, so this returns `false` for the
    /// `ValueType, ValueType` pair and lets the byte-walk path reconstruct
    /// through the requested template.
    let private sameCliConstructor (a : CliType) (b : CliType) : bool =
        match a, b with
        | CliType.Bool _, CliType.Bool _
        | CliType.Char _, CliType.Char _
        | CliType.ObjectRef _, CliType.ObjectRef _
        | CliType.RuntimePointer _, CliType.RuntimePointer _ -> true
        | CliType.Numeric a, CliType.Numeric b ->
            match a, b with
            | CliNumericType.Int8 _, CliNumericType.Int8 _
            | CliNumericType.UInt8 _, CliNumericType.UInt8 _
            | CliNumericType.Int16 _, CliNumericType.Int16 _
            | CliNumericType.UInt16 _, CliNumericType.UInt16 _
            | CliNumericType.Int32 _, CliNumericType.Int32 _
            | CliNumericType.Int64 _, CliNumericType.Int64 _
            | CliNumericType.NativeInt _, CliNumericType.NativeInt _
            | CliNumericType.NativeFloat _, CliNumericType.NativeFloat _
            | CliNumericType.Float32 _, CliNumericType.Float32 _
            | CliNumericType.Float64 _, CliNumericType.Float64 _ -> true
            | _ -> false
        | CliType.ValueType _, CliType.ValueType _ -> false
        | _ -> false

    /// True iff the two values have the same primitive shape *after* peeling
    /// primitive-like single-field wrappers (`IntPtr`, `RuntimeTypeHandle`,
    /// `EnumLike`, ...) from both sides. This is the *read-side* fast-path
    /// comparator: it lets a wrapped template recover a stored bare cell (and
    /// vice versa) without losing tagged `NativeIntSource` provenance through
    /// the byte-walk fallback, since the wrapper and its bare-primitive
    /// contents share byte representation and `EvalStackValue.ofCliType`
    /// flattening behaviour.
    ///
    /// **Asymmetric output shape, gated by byte-addressability.** Because
    /// this predicate is permissive about wrapper depth, the sites that
    /// `match` on it and would otherwise return the stored cell
    /// (`readStackMemoryBytesAs`, `tryReadHeapValueFieldPrecise`) further
    /// gate the fast-path return on `CliType.ByteAddressability`: only
    /// non-byte-addressable cells (tagged `NativeIntSource`, object refs,
    /// runtime pointers) skip the byte-walk and propagate storage shape,
    /// since those carry provenance the byte path cannot reconstruct.
    /// Byte-addressable cells fall through to the byte-walk so the result
    /// has the *template*'s exact CLI shape. Callers that pass a wrapped
    /// template against a non-byte-addressable storage cell still see the
    /// storage shape and must reconcile via `unwrapPrimitiveLikeDeep` or
    /// `EvalStackValue.ofCliType`.
    ///
    /// **Do NOT use on the write side.** Installing a wrapper-vs-bare-shape
    /// `newValue` into a heap field whose recorded `Contents` has a different
    /// shape would overwrite the field's CLI shape via `WithFieldSetById`,
    /// silently coercing e.g. a boxed-IntPtr `_value` field from bare
    /// `NativeInt` to wrapped `IntPtr`. Write-side sites use
    /// `sameCliConstructor` (strict, no unwrap) so a shape mismatch falls
    /// through to byte-scatter; in the case where byte-scatter can't service
    /// the write (non-byte-addressable storage), the failure is loud rather
    /// than a silent corruption of the field's CLI shape.
    let private haveSameCliShape (a : CliType) (b : CliType) : bool =
        let a = CliType.unwrapPrimitiveLikeDeep a
        let b = CliType.unwrapPrimitiveLikeDeep b
        sameCliConstructor a b

    /// `true` iff a storage cell holding `cell` may stand in for a value of CliType `target`
    /// without any bytewise reinterpret step — i.e. reading the cell *is* reading the target, and
    /// writing the target into it *is* writing the cell.
    ///
    /// This is deliberately type identity rather than layout compatibility. A cell's declared type
    /// is not decoration: `CliValueType.Declared` decides primitive-like flattening at the eval
    /// stack boundary, so storing a value of some other equally-shaped type into a cell would leave
    /// the cell lying about what it holds. Anything this refuses falls through to the bytewise
    /// path, which is correct wherever it is defined and produces a useful diagnostic where it is
    /// not, so refusing costs nothing but a slower route.
    ///
    /// Note that no test distinguishes this from the laxer "any two value types" rule, and that is
    /// not an oversight: reaching the difference needs a reinterpret between two *distinct* value
    /// types over reference-containing storage, which the real runtime performs happily and
    /// PawPrint deliberately refuses. That divergence cannot be asserted differentially, so the
    /// strictness here is a deliberate choice of the safe direction rather than a pinned-down
    /// behaviour. Mutating it does not fail the suite; treat it as load-bearing anyway.
    let private isCellIdentityCompatible (cell : CliType) (target : CliType) : bool =
        match cell, target with
        | CliType.ObjectRef _, CliType.ObjectRef _ -> true
        | CliType.ValueType a, CliType.ValueType b -> a.Declared = b.Declared
        | CliType.Numeric a, CliType.Numeric b -> CliNumericType.SameKind a b
        | CliType.Bool _, CliType.Bool _ -> true
        | CliType.Char _, CliType.Char _ -> true
        | _ -> false

    /// The storage cell a reinterpreting byref addresses, when the bytewise path cannot serve the
    /// access at all. `[InlineArray(N)] struct { T _item; }` produces exactly this shape — indexing
    /// element `k` is `Unsafe.Add(ref Unsafe.As<TBuffer, T>(ref buffer), k)`, i.e.
    /// `[ReinterpretAs T; ByteOffset k * sizeof(T)]`.
    ///
    /// Byte-addressable storage deliberately gets `None`, so this never changes which path serves
    /// an access that already works: bytes remain the general mechanism and naming a cell is the
    /// fallback for storage that has no byte image. That is not a new rule — it is the one the
    /// object-reference-only predicate already encoded implicitly, since `ObjectRef` storage is
    /// exactly the non-byte-addressable case. Stating it here keeps
    /// `isCellIdentityCompatible` a predicate about types alone, rather than one that also
    /// silently means "and bytes would not have worked".
    ///
    /// The gate is conservatism, not correctness: where both routes are defined they agree, which
    /// is what `TestCliTypeCellPaths`'s "naming a cell agrees with the byte view" property pins.
    /// Removing the gate accordingly fails no test. It earns its place by making that agreement
    /// something this code does not have to rely on holding in every corner — provenance-carrying
    /// numerics and pointer-shaped values have their own byte-rendering rules — rather than by
    /// changing an answer.
    ///
    /// `CliType.CellPathsExactlyCovering` supplies the structural half: the range is exactly some
    /// cell's extent, unaliased by any sibling, so nothing outside it can be disturbed. It reports
    /// nested cells outermost first, so taking the first type-compatible answer names the
    /// shallowest cell that will do.
    let private tryNameCellForByrefAccess
        (byteOffset : int)
        (storage : CliType)
        (targetTemplate : CliType)
        : FieldId list option
        =
        match CliType.ByteAddressability storage with
        | CliByteAddressability.ByteAddressable -> None
        | CliByteAddressability.Rejected _ ->

        let targetSize = CliType.sizeOf targetTemplate

        // The range can name the storage itself and not merely a field of it: an
        // `Unsafe.As<Elem, Wrapper>` over reference-containing storage reinterprets the whole
        // cell. `CellPathsExactlyCovering` reports *fields*, so the empty path is this function's
        // own base case rather than something it can return — and deliberately so, since folding
        // it into that recursion would give every exactly-covering field a second, laxer route to
        // the same path, bypassing the field-consistency gate there.
        if
            byteOffset = 0
            && targetSize = CliType.sizeOf storage
            && isCellIdentityCompatible storage targetTemplate
        then
            Some []
        else

        CliType.CellPathsExactlyCovering byteOffset targetSize storage
        |> List.tryFind (fun (_, contents) -> isCellIdentityCompatible contents targetTemplate)
        |> Option.map fst

    /// Byte image of a CLI value for noop-detection purposes. If a value is
    /// classified as byte-addressable, `CliType.ToBytes` must be able to render
    /// it; otherwise the classifier is wrong and should fail here.
    let private tryToBytesForNoopCheck (value : CliType) : byte[] voption =
        match CliType.ByteAddressability value with
        | CliByteAddressability.Rejected _ -> ValueNone
        | CliByteAddressability.ByteAddressable -> ValueSome (CliType.ToBytes value)

    /// `true` only when replacing `current` with `updated` is provably unobservable, so the write
    /// can be skipped.
    ///
    /// Structural equality is not such a proof. `-0.0f` and `+0.0f` compare equal and are told
    /// apart by `1.0f / x`, so a value differing from its replacement only in the sign of a zero
    /// would have its write dropped. That bites hardest where it is least visible: storage holding
    /// a reference has no byte image, so it is compared structurally or not at all.
    ///
    /// Both proofs below are exact — a byte image settles every bit, and two `ObjectRef`s are the
    /// heap addresses themselves. Everything else is written. Failing to spot a no-op costs a
    /// redundant store; a wrong "no-op" costs the write.
    let private isProvableNoOpWrite (current : CliType) (updated : CliType) : bool =
        match tryToBytesForNoopCheck current, tryToBytesForNoopCheck updated with
        | ValueSome currentBytes, ValueSome updatedBytes -> currentBytes = updatedBytes
        | _ ->

        match current, updated with
        | CliType.ObjectRef a, CliType.ObjectRef b -> a = b
        | _ -> false

    let setStatic
        (owner : StaticOwner)
        (ty : ConcreteTypeHandle)
        (field : ComparableFieldDefinitionHandle)
        (value : CliType)
        (this : IlMachineState)
        : IlMachineState
        =
        let ownerStatics =
            match this._Statics.TryGetValue owner with
            | false, _ -> ImmutableDictionary.Empty
            | true, v -> v

        let ownerStatics =
            match ownerStatics.TryGetValue ty with
            | false, _ -> ownerStatics.Add (ty, Map.ofList [ field, value ])
            | true, v -> ownerStatics.SetItem (ty, Map.add field value v)

        { this with
            _Statics = this._Statics.SetItem (owner, ownerStatics)
        }

    let getStatic
        (owner : StaticOwner)
        (ty : ConcreteTypeHandle)
        (field : ComparableFieldDefinitionHandle)
        (this : IlMachineState)
        : CliType option
        =
        match this._Statics.TryGetValue owner with
        | false, _ -> None
        | true, ownerStatics ->

        match ownerStatics.TryGetValue ty with
        | false, _ -> None
        | true, v -> Map.tryFind field v

    let private tryReadInitializedStackMemoryBytes
        (state : IlMachineState)
        (thread : ThreadId)
        (frame : FrameId)
        (block : StackMemoryBlockId)
        (byteOffset : int)
        (byteCount : int)
        : byte[] voption
        =
        let pool = IlMachineThreadState.getStackMemoryPool thread frame state
        StackMemoryPool.tryReadBytes block byteOffset byteCount pool

    let private tryReadInitializedNativeMemoryBytes
        (state : IlMachineState)
        (block : NativeMemoryBlockId)
        (byteOffset : int)
        (byteCount : int)
        : byte[] voption
        =
        NativeMemoryPool.tryReadBytes block byteOffset byteCount state.Kernel.NativeMemoryPool

    let private readRootValue (state : IlMachineState) (root : ByrefRoot) : CliType =
        match root with
        | ByrefRoot.LocalVariable (t, f, v) -> (IlMachineThreadState.getFrame t f state).LocalVariables.[int<uint16> v]
        | ByrefRoot.Argument (t, f, v) -> (IlMachineThreadState.getFrame t f state).Arguments.[int<uint16> v]
        | ByrefRoot.StackMemoryByte (t, f, block, byteOffset) ->
            // A bare StackMemoryByte byref points at a typed cell starting at
            // `byteOffset`. If a cell starts there, return it as-is; we don't
            // synthesise a typed value from raw bytes here because we have no
            // target template — typed reads through a `ReinterpretAs` go via
            // `readManagedByrefBytesAs` instead.
            let pool = IlMachineThreadState.getStackMemoryPool t f state

            match StackMemoryPool.tryFindCellCovering block byteOffset pool with
            | Some (cellOffset, cell) when cellOffset = byteOffset -> cell
            | Some (cellOffset, cell) ->
                failwith
                    $"TODO: typed read of local memory %O{block} at byte offset %d{byteOffset} lands inside cell starting at %d{cellOffset} (size %d{CliType.sizeOf cell}); needs a byte-view byref shape"
            | None ->
                failwith
                    $"TODO: typed read of local memory %O{block} at byte offset %d{byteOffset} has no typed cell here; needs a byte-view byref shape"
        | ByrefRoot.NativeMemoryByte (block, byteOffset) ->
            // Mirror of the StackMemoryByte case. The native-heap pool is global on
            // IlMachineState; use-after-free is caught inside NativeMemoryPool.
            match NativeMemoryPool.tryFindCellCovering block byteOffset state.Kernel.NativeMemoryPool with
            | Some (cellOffset, cell) when cellOffset = byteOffset -> cell
            | Some (cellOffset, cell) ->
                failwith
                    $"TODO: typed read of native-heap memory %O{block} at byte offset %d{byteOffset} lands inside cell starting at %d{cellOffset} (size %d{CliType.sizeOf cell}); needs a byte-view byref shape"
            | None ->
                failwith
                    $"TODO: typed read of native-heap memory %O{block} at byte offset %d{byteOffset} has no typed cell here; needs a byte-view byref shape"
        | ByrefRoot.HeapValue addr -> CliType.ValueType (ManagedHeap.get addr state.ManagedHeap).Contents
        | ByrefRoot.HeapObjectField (addr, field) ->
            ManagedHeap.get addr state.ManagedHeap
            |> AllocatedNonArrayObject.DereferenceFieldById field
        | ByrefRoot.ArrayElement (arr, index) -> IlMachineThreadState.getArrayValue arr index state
        | ByrefRoot.PeByteRange peByteRange ->
            failwith
                $"TODO: reading PE byte-range root %O{peByteRange} requires a primitive byte-view projection; plain typed PE byte-range root reads are not modelled"
        | ByrefRoot.StaticField (ty, field, owner) ->
            match getStatic owner ty field state with
            | Some value -> value
            | None ->
                failwith
                    $"Static field byref %O{field.Get} on concrete type %O{ty} in %O{owner} was read before the static slot was initialised"
        | ByrefRoot.StringCharAt (str, charIndex) ->
            ManagedHeap.getStringChar str charIndex state.ManagedHeap |> CliType.ofChar
        | ByrefRoot.ExposedClassObject target ->
            // Pre-allocation at byref construction (see
            // MethodTableProjection.tryProjectAuxiliaryDataFieldAddress) guarantees
            // the RuntimeType is registered before any read; a missing entry here
            // means the byref was constructed by an unintended path.
            match TypeHandleRegistry.tryFindHandle target state.TypeHandles with
            | Some addr -> CliType.ObjectRef (Some addr)
            | None ->
                failwith
                    $"interpreter bug: cached-RuntimeType byref for type %O{target} reached read without prior RuntimeType registry allocation"

    let private writeRootValue (state : IlMachineState) (root : ByrefRoot) (updated : CliType) : IlMachineState =
        // The ReferenceEquals checks in this function are allocation shortcuts for direct root
        // writes where the caller is storing the exact object already present. Semantic no-op
        // detection for byte/projection writes is represented explicitly by `option` results
        // before this function is called.
        match root with
        | ByrefRoot.LocalVariable (t, f, v) ->
            let existing = IlMachineThreadState.getLocalVariable t f v state

            if System.Object.ReferenceEquals (existing, updated) then
                state
            else
                state |> IlMachineThreadState.setLocalVariable t f v updated
        | ByrefRoot.Argument (t, f, v) ->
            let existing = (IlMachineThreadState.getFrame t f state).Arguments.[int<uint16> v]

            if System.Object.ReferenceEquals (existing, updated) then
                state
            else
                state |> IlMachineThreadState.setArgument t f v updated
        | ByrefRoot.StackMemoryByte (t, f, block, byteOffset) ->
            // A bare StackMemoryByte byref points at a typed cell. The caller
            // has already chosen the typed value to install; preserve any
            // provenance carried by the value (e.g. tagged native-int sources)
            // by storing it as a typed cell rather than flattening to bytes.
            // We short-circuit byte-identical writes over an existing typed
            // cell when keeping that cell is shape-preserving, or when
            // restamping a differently-sized value would collapse a wider
            // existing cell. Fresh local memory still needs the typed cell to
            // be installed, even when its zero-filled byte view already matches
            // the write.
            let pool = IlMachineThreadState.getStackMemoryPool t f state

            // Refuse a typed write that lands inside (but does not start at)
            // an existing cell: silently evicting the covering cell would lose
            // its provenance. Symmetric to the read-side check in
            // `readRootValue` for `ByrefRoot.StackMemoryByte`.
            match StackMemoryPool.tryFindCellCovering block byteOffset pool with
            | Some (cellOffset, cell) when cellOffset <> byteOffset ->
                failwith
                    $"TODO: typed write of %O{updated} to local memory %O{block} at byte offset %d{byteOffset} lands inside cell starting at %d{cellOffset} (size %d{CliType.sizeOf cell}); needs a byte-view byref shape"
            | _ ->
                match StackMemoryPool.tryReadCell block byteOffset pool with
                | Some existing when System.Object.ReferenceEquals (existing, updated) -> state
                | Some existing ->
                    let existingSize = CliType.sizeOf existing
                    let updatedSize = CliType.sizeOf updated

                    let isNoop =
                        match tryToBytesForNoopCheck updated with
                        | ValueNone -> false
                        | ValueSome updatedBytes ->
                            match StackMemoryPool.tryReadBytes block byteOffset updatedBytes.Length pool with
                            | ValueSome existingBytes -> bytesEqual existingBytes updatedBytes
                            | ValueNone -> false

                    let preservesExistingShape =
                        // Strict constructor check (no primitive-like
                        // unwrap): a typed store whose `updated` shape
                        // differs from the cell's `existing` shape, even by
                        // only a wrapper layer, must restamp the cell rather
                        // than short-circuit. Otherwise a `stind.i` writing
                        // bare `NativeInt` followed by a byte-identical
                        // `stobj IntPtr` would leave the cell bare, and the
                        // next read through the wrapped template would
                        // observe a shape it can't service. The strict
                        // comparator also rejects unrelated structs of the
                        // same size, so byte-identical writes between two
                        // non-primitive-like value types of the same size
                        // still restamp through the else branch below.
                        existingSize <> updatedSize || sameCliConstructor existing updated

                    if isNoop && preservesExistingShape then
                        state
                    else
                        if existingSize <> updatedSize then
                            failwith
                                $"TODO: typed write of %O{updated} to local memory %O{block} at byte offset %d{byteOffset} would replace an existing cell of size %d{existingSize} with size %d{updatedSize}; use a byte-view byref shape"

                        let pool = StackMemoryPool.writeCell block byteOffset updated pool
                        IlMachineThreadState.setStackMemoryPool t f pool state
                | None ->
                    let pool = StackMemoryPool.writeCell block byteOffset updated pool
                    IlMachineThreadState.setStackMemoryPool t f pool state
        | ByrefRoot.NativeMemoryByte (block, byteOffset) ->
            // Mirror of the StackMemoryByte write case but routed through the
            // global NativeMemoryPool. Use-after-free is caught inside the pool
            // when the block has been removed.
            let pool = state.Kernel.NativeMemoryPool

            match NativeMemoryPool.tryFindCellCovering block byteOffset pool with
            | Some (cellOffset, cell) when cellOffset <> byteOffset ->
                failwith
                    $"TODO: typed write of %O{updated} to native-heap memory %O{block} at byte offset %d{byteOffset} lands inside cell starting at %d{cellOffset} (size %d{CliType.sizeOf cell}); needs a byte-view byref shape"
            | _ ->
                match NativeMemoryPool.tryReadCell block byteOffset pool with
                | Some existing when System.Object.ReferenceEquals (existing, updated) -> state
                | Some existing ->
                    let existingSize = CliType.sizeOf existing
                    let updatedSize = CliType.sizeOf updated

                    let isNoop =
                        match tryToBytesForNoopCheck updated with
                        | ValueNone -> false
                        | ValueSome updatedBytes ->
                            match NativeMemoryPool.tryReadBytes block byteOffset updatedBytes.Length pool with
                            | ValueSome existingBytes -> bytesEqual existingBytes updatedBytes
                            | ValueNone -> false

                    let preservesExistingShape =
                        existingSize <> updatedSize || sameCliConstructor existing updated

                    if isNoop && preservesExistingShape then
                        state
                    else
                        if existingSize <> updatedSize then
                            failwith
                                $"TODO: typed write of %O{updated} to native-heap memory %O{block} at byte offset %d{byteOffset} would replace an existing cell of size %d{existingSize} with size %d{updatedSize}; use a byte-view byref shape"

                        let pool = NativeMemoryPool.writeCell block byteOffset updated pool
                        IlMachineThreadState.setNativeMemoryPool pool state
                | None ->
                    let pool = NativeMemoryPool.writeCell block byteOffset updated pool
                    IlMachineThreadState.setNativeMemoryPool pool state
        | ByrefRoot.HeapValue addr ->
            let contents =
                match updated with
                | CliType.ValueType contents -> contents
                | other -> failwith $"cannot write non-value-type {other} through heap value byref"

            let existing = ManagedHeap.get addr state.ManagedHeap

            if System.Object.ReferenceEquals (contents, existing.Contents) then
                state
            else
                { state with
                    ManagedHeap =
                        ManagedHeap.set
                            addr
                            { existing with
                                Contents = contents
                            }
                            state.ManagedHeap
                }
        | ByrefRoot.HeapObjectField (addr, field) ->
            let existing = ManagedHeap.get addr state.ManagedHeap
            let existingField = AllocatedNonArrayObject.DereferenceFieldById field existing

            if System.Object.ReferenceEquals (existingField, updated) then
                state
            else
                let withUpdatedField =
                    existing |> AllocatedNonArrayObject.SetFieldById field updated

                { state with
                    ManagedHeap = ManagedHeap.set addr withUpdatedField state.ManagedHeap
                }
        | ByrefRoot.ArrayElement (arr, index) ->
            let existing = IlMachineThreadState.getArrayValue arr index state

            if System.Object.ReferenceEquals (existing, updated) then
                state
            else
                state |> IlMachineThreadState.setArrayValue arr updated index
        | ByrefRoot.PeByteRange peByteRange ->
            failwith $"PE byte range is read-only; refusing to write %O{updated} through %O{peByteRange}"
        | ByrefRoot.StaticField (ty, field, owner) ->
            match getStatic owner ty field state with
            | Some existing when System.Object.ReferenceEquals (existing, updated) -> state
            | _ -> state |> setStatic owner ty field updated
        | ByrefRoot.StringCharAt (str, charIndex) ->
            let updated =
                match updated with
                | CliType.Char (high, low) -> char (int high * 256 + int low)
                | other ->
                    // Direct same-width primitive writes, for example Stind.I2
                    // storing a UInt16 through a ref char byref, preserve the
                    // raw UTF-16 bits while normalising the stored cell to char.
                    let charTemplate = CliType.ofChar (char 0)
                    let charSize = CliType.sizeOf charTemplate

                    if CliType.sizeOf other <> charSize then
                        failwith
                            $"string character write expected a 2-byte char-compatible value, got %d{CliType.sizeOf other} bytes from %O{other}"

                    let updatedCell =
                        let updatedBytes = CliType.BytesAt 0 charSize other
                        CliType.WithBytesAt 0 updatedBytes charTemplate

                    match updatedCell with
                    | CliType.Char (high, low) -> char (int high * 256 + int low)
                    | reconstructed -> failwith $"string character write reconstructed non-char value %O{reconstructed}"

            if ManagedHeap.getStringChar str charIndex state.ManagedHeap = updated then
                state
            else
                { state with
                    ManagedHeap = ManagedHeap.setStringChar str charIndex updated state.ManagedHeap
                }
        | ByrefRoot.ExposedClassObject target ->
            // Managed CoreLib only writes this cache via the native
            // QCall path inside GetRuntimeTypeFromHandleSlow, which is not
            // implemented in WoofWare. Our fast read always returns a non-null
            // canonical RuntimeType, so the `?? GetRuntimeTypeFromHandleSlow(...)`
            // branch in the managed accessor never fires; reaching this write
            // means a code path is bypassing that contract.
            failwith $"writes to the cached-RuntimeType cell for type %O{target} are not modelled (got %O{updated})"

    let private readProjectedValue (rootValue : CliType) (projs : ByrefProjection list) : CliType =
        projs
        |> List.fold
            (fun value proj ->
                match proj with
                | ByrefProjection.Field field ->
                    match value with
                    | CliType.ValueType vt -> CliValueType.DereferenceFieldById field vt
                    | v -> failwith $"could not find field {field.Name} on non-ValueType {v}"
                | ByrefProjection.ReinterpretAs ty ->
                    if isSafeReinterpretPassthrough value ty then
                        value
                    else
                        failwith
                            $"TODO: read through `ReinterpretAs` from value %O{value} as type %s{ty.Namespace}.%s{ty.Name}; needs a bytewise implementation"
                | ByrefProjection.ByteOffset n ->
                    failwith
                        $"TODO: readManagedByref via ByteOffset %d{n} requires a trailing byte-view byref shape; generic Ldind at a non-normalised byte offset is not modelled (value: %O{value})"
            )
            rootValue

    let private validateByteAddressableCell (context : string) (value : CliType) : unit =
        // Keep this caller-side check even though CliType byte helpers validate too: this layer
        // can report which byref shape requested the byte view, while CliType protects direct
        // callers of the byte helpers.
        match CliType.ByteAddressability value with
        | CliByteAddressability.ByteAddressable -> ()
        | CliByteAddressability.Rejected rejection ->
            failwith
                $"refusing byte view over %s{rejection.Description} in %s{context}. Value layout:\n%s{CliType.DescribeByteLayout None value}"

    let private byteAddressableCellSize (context : string) (value : CliType) : int =
        validateByteAddressableCell context value
        CliType.sizeOf value

    let private byteAddressableCellBytesAt (context : string) (offset : int) (count : int) (value : CliType) : byte[] =
        validateByteAddressableCell context value
        CliType.BytesAt offset count value

    let private withByteAddressableCellBytesAtIfChanged
        (context : string)
        (offset : int)
        (bytes : byte[])
        (value : CliType)
        : CliType option
        =
        validateByteAddressableCell context value
        CliType.WithBytesAtIfChanged offset bytes value

    let private splitTrailingByteView (src : ManagedPointerSource) : (ByrefRoot * ByrefProjection list * int) voption =
        match src with
        | ManagedPointerSource.Null -> ValueNone
        | ManagedPointerSource.NativeIntPlaceholder _ -> ValueNone
        | ManagedPointerSource.Byref (root, projs) ->
            match List.rev projs with
            | ByrefProjection.ByteOffset n :: ByrefProjection.ReinterpretAs _ :: revPrefix ->
                ValueSome (root, List.rev revPrefix, n)
            | ByrefProjection.ByteOffset n :: _ ->
                failwith
                    $"ByteOffset %d{n} without a preceding ReinterpretAs in projection chain: %O{src} (this is an interpreter bug)"
            | ByrefProjection.ReinterpretAs _ :: revPrefix -> ValueSome (root, List.rev revPrefix, 0)
            | _ -> ValueNone

    let private floorDivRem (value : int) (divisor : int) : int * int =
        if divisor <= 0 then
            failwith $"floorDivRem requires a positive divisor, got %d{divisor}"

        let q = value / divisor
        let r = value - q * divisor

        if r < 0 then q - 1, r + divisor else q, r

    let private readArrayBytesAs
        (state : IlMachineState)
        (arr : ManagedHeapAddress)
        (index : int)
        (byteOffset : int)
        (targetTemplate : CliType)
        : CliType
        =
        let targetSize = CliType.sizeOf targetTemplate
        let arrObj = state.ManagedHeap.Arrays.[arr]

        if arrObj.Length = 0 then
            failwith $"TODO: byte-view read from empty array %O{arr} at index %d{index} offset %d{byteOffset}"

        // Compute cell size with `CliType.sizeOf` (not `byteAddressableCellSize`)
        // so we can recognise whole-cell-aligned reads of cells that may
        // carry non-byte-renderable provenance — e.g. an `IntPtr[]` slot
        // that now holds a `TypeHandlePtr` after a typed store through a
        // fixed-array pointer. For non-byte-addressable cells that match
        // the target shape exactly, we short-circuit and return the typed
        // cell directly, preserving provenance; for everything else we
        // fall through to the byte-scatter loop, which validates byte
        // addressability per cell as it gathers.
        let firstCellSize = CliType.sizeOf arrObj.Elements.[0]
        let cellAdvance, inCellStart = floorDivRem byteOffset firstCellSize

        let shortCircuitCell =
            if inCellStart = 0 && targetSize = firstCellSize then
                let targetCell = index + cellAdvance

                if targetCell < 0 || targetCell >= arrObj.Length then
                    failwith
                        $"TODO: byte-view read past array bounds at cell %d{targetCell} of length %d{arrObj.Length}"

                let cellValue = arrObj.Elements.[targetCell]

                // Mirror `readStackMemoryBytesAs` / `tryReadHeapValueFieldPrecise`:
                // propagate the stored cell only when it (a) is non-byte-addressable
                // (so the byte-scatter path can't service it without losing
                // provenance) AND (b) shares the requested template's CLI shape.
                // A same-size shape mismatch — e.g. reading an `IntPtr[]` cell
                // through `Unsafe.ReadUnaligned<long>` — falls through to the
                // byte-walk, which will surface a clear error for the
                // non-byte-renderable cell rather than silently returning a
                // wrongly-shaped value.
                match CliType.ByteAddressability cellValue with
                | CliByteAddressability.Rejected _ when haveSameCliShape cellValue targetTemplate -> ValueSome cellValue
                | _ -> ValueNone
            else
                ValueNone

        // The short-circuit above only recognises a *whole* element. An element that is a value
        // type containing object references has no byte image at all, so a read that lands inside
        // one cannot be served by the byte-scatter loop below either — the only thing to return is
        // the cell the byte range names. `tryNameCellForByrefAccess` yields `None` for
        // byte-addressable elements, so nothing that reaches the byte walk today is diverted.
        // A range spilling past the element yields `None` too, and falls through to the walk,
        // which reports the unrenderable cell.
        let namedInnerCell =
            match shortCircuitCell with
            | ValueSome _ -> ValueNone
            | ValueNone ->

            let targetCell = index + cellAdvance

            if targetCell < 0 || targetCell >= arrObj.Length then
                ValueNone
            else

            let cellValue = arrObj.Elements.[targetCell]

            tryNameCellForByrefAccess inCellStart cellValue targetTemplate
            |> Option.map (fun path -> CliType.getCellAtPath path cellValue)
            |> ValueOption.ofOption

        match shortCircuitCell, namedInnerCell with
        | ValueSome cellValue, _
        | _, ValueSome cellValue -> cellValue
        | ValueNone, ValueNone ->
            let buf = Array.zeroCreate<byte> targetSize
            let mutable filled = 0
            let mutable cell = index + cellAdvance
            let mutable inCellOffset = inCellStart

            while filled < targetSize do
                if cell < 0 || cell >= arrObj.Length then
                    failwith
                        $"TODO: byte-view read past array bounds at cell %d{cell} of length %d{arrObj.Length} while gathering %d{targetSize} bytes"

                let cellSize =
                    byteAddressableCellSize $"array %O{arr} element %d{cell}" arrObj.Elements.[cell]

                let canTake = cellSize - inCellOffset
                let take = min canTake (targetSize - filled)

                let bytes =
                    byteAddressableCellBytesAt
                        $"array %O{arr} element %d{cell}"
                        inCellOffset
                        take
                        arrObj.Elements.[cell]

                Array.blit bytes 0 buf filled take
                filled <- filled + take
                cell <- cell + 1
                inCellOffset <- 0

            CliType.ofBytesLike targetTemplate buf

    /// Read `byteOffset ..` out of a PE byte range and rebuild a value of `targetTemplate`'s
    /// shape from those bytes. The read is bounds-checked against the range's own declared
    /// size, so it can never wander into whatever metadata happens to follow the field in the
    /// section.
    let readPeByteRangeBytesAs
        (state : IlMachineState)
        (peByteRange : PeByteRangePointer)
        (byteOffset : int)
        (targetTemplate : CliType)
        : CliType
        =
        let targetSize = CliType.sizeOf targetTemplate

        if byteOffset < 0 || targetSize > peByteRange.Size - byteOffset then
            failwith
                $"PE byte-view read at offset %d{byteOffset} for %d{targetSize} bytes is outside byte range size %d{peByteRange.Size}: %O{peByteRange}"

        let assembly =
            state.LoadedAssembly' peByteRange.AssemblyFullName
            |> Option.defaultWith (fun () ->
                failwith $"PE byte-view read needs loaded assembly %s{peByteRange.AssemblyFullName}"
            )

        let bytes =
            match peByteRange.Source with
            | PeByteRangePointerSource.FieldRva _
            | PeByteRangePointerSource.ManagedResource _ ->
                let sectionData =
                    assembly.PeReader.GetSectionData peByteRange.RelativeVirtualAddress

                let mutable reader = sectionData.GetReader ()
                reader.Offset <- byteOffset
                reader.ReadBytes targetSize
            | PeByteRangePointerSource.FieldSignatureBlob field ->
                let mdReader = assembly.PeReader.GetMetadataReader ()
                let fieldDef = mdReader.GetFieldDefinition field.Get
                let mutable blobReader = mdReader.GetBlobReader fieldDef.Signature
                blobReader.Offset <- byteOffset
                blobReader.ReadBytes targetSize
            | PeByteRangePointerSource.MethodSignatureBlob method ->
                let mdReader = assembly.PeReader.GetMetadataReader ()
                let methodDef = mdReader.GetMethodDefinition method.Get
                let mutable blobReader = mdReader.GetBlobReader methodDef.Signature
                blobReader.Offset <- byteOffset
                blobReader.ReadBytes targetSize

        CliType.ofBytesLike targetTemplate bytes

    let private readStringBytesAs
        (state : IlMachineState)
        (str : ManagedHeapAddress)
        (charIndex : int)
        (byteOffset : int)
        (targetTemplate : CliType)
        : CliType
        =
        let targetSize = CliType.sizeOf targetTemplate
        let cellAdvance, inCellStart = floorDivRem byteOffset 2
        let buf = Array.zeroCreate<byte> targetSize
        let mutable filled = 0
        let mutable cell = charIndex + cellAdvance
        let mutable inCellOffset = inCellStart
        let cellSize = CliType.sizeOf (CliType.ofChar (char 0))

        while filled < targetSize do
            let canTake = cellSize - inCellOffset
            let take = min canTake (targetSize - filled)

            let charBytes =
                ManagedHeap.getStringChar str cell state.ManagedHeap
                |> CliType.ofChar
                |> CliType.BytesAt inCellOffset take

            Array.blit charBytes 0 buf filled take
            filled <- filled + take
            cell <- cell + 1
            inCellOffset <- 0

        CliType.ofBytesLike targetTemplate buf

    /// Render a `ConcreteTypeHandle` as `Namespace.Name [AssemblyShortName]` for
    /// diagnostic messages. Falls back gracefully when the lookup chain breaks,
    /// since this is called from failure paths that should not throw a second time.
    let private describeConcreteType (state : IlMachineState) (handle : ConcreteTypeHandle) : string =
        match AllConcreteTypes.lookup handle state.ConcreteTypes with
        | None -> $"<unregistered concrete type %O{handle}>"
        | Some concrete ->
            match state.LoadedAssembly concrete.Assembly with
            | None -> $"<unloaded assembly %O{concrete.Assembly} for concrete type %O{handle}>"
            | Some assembly ->
                match assembly.TypeDefs.TryGetValue concrete.Definition.Get with
                | true, typeDef ->
                    $"%s{typeDef.Namespace}.%s{typeDef.Name} [%s{assembly.Name.Name}] (concrete %O{handle})"
                | false, _ ->
                    $"<missing TypeDef %O{concrete.Definition.Get} in %s{assembly.Name.Name}> (concrete %O{handle})"

    let private heapValueForByteView
        (operation : string)
        (state : IlMachineState)
        (addr : ManagedHeapAddress)
        : AllocatedNonArrayObject
        =
        let obj = ManagedHeap.get addr state.ManagedHeap

        match CliValueType.ByteAddressability obj.Contents with
        | CliByteAddressability.ByteAddressable -> obj
        | CliByteAddressability.Rejected rejection ->
            let typeDescription = describeConcreteType state obj.ConcreteType

            failwith
                $"%s{operation}: refusing byte view over boxed %s{rejection.Description} of %s{typeDescription} at %O{addr}. Boxed value layout:\n%s{CliValueType.DescribeByteLayout (Some state.ConcreteTypes) obj.Contents}"

    let private heapValueByteSize
        (operation : string)
        (state : IlMachineState)
        (addr : ManagedHeapAddress)
        : AllocatedNonArrayObject * int
        =
        let obj = heapValueForByteView operation state addr
        obj, CliValueType.SizeOf(obj.Contents).Size

    /// Field-precise byte-view read: when a *unique* typed instance field starts at
    /// exactly `byteOffset`, matches the requested size, has the same CLI shape as the
    /// requested template, AND is itself non-byte-addressable (object-reference or
    /// runtime-pointer), return its `Contents` directly. The non-byte-addressable gate
    /// is what makes this a strict extension of the byte-walk path: byte-addressable
    /// fields are left to `CliValueType.BytesAt`, which resolves explicit-layout
    /// overlaps via `EditedAtTime` ordering. The uniqueness gate (single matching
    /// candidate) refuses to guess between aliased fields. Returns `None` when no
    /// such field exists, so the caller falls through to the byte-walk.
    let private tryReadHeapValueFieldPrecise
        (state : IlMachineState)
        (addr : ManagedHeapAddress)
        (byteOffset : int)
        (targetTemplate : CliType)
        : CliType option
        =
        let obj = ManagedHeap.get addr state.ManagedHeap
        let targetSize = CliType.sizeOf targetTemplate

        let candidates =
            CliValueType.TryFieldsAt byteOffset obj.Contents
            |> List.filter (fun f -> f.Size = targetSize && haveSameCliShape f.Contents targetTemplate)

        match candidates with
        | [ f ] ->
            match CliType.ByteAddressability f.Contents with
            | CliByteAddressability.Rejected _ -> Some f.Contents
            | CliByteAddressability.ByteAddressable -> None
        | _ -> None

    let private readHeapValueBytesAs
        (state : IlMachineState)
        (addr : ManagedHeapAddress)
        (byteOffset : int)
        (targetTemplate : CliType)
        : CliType
        =
        match tryReadHeapValueFieldPrecise state addr byteOffset targetTemplate with
        | Some cell -> cell
        | None ->

        // `tryReadHeapValueFieldPrecise` preserves provenance for a top-level field when a byte
        // route also exists; this serves the case where none does. A boxed value type containing
        // object references has no byte image, so naming the cell the range picks out is the only
        // way to answer, and unlike the precise reader it descends to any depth. Byte-addressable
        // payloads get `None` here, so the byte rendering below stays the route for everything
        // that already works.
        let namedCell =
            let boxed = CliType.ValueType (ManagedHeap.get addr state.ManagedHeap).Contents

            tryNameCellForByrefAccess byteOffset boxed targetTemplate
            |> Option.map (fun path -> CliType.getCellAtPath path boxed)

        match namedCell with
        | Some cell -> cell
        | None ->

        let existing, payloadSize =
            heapValueByteSize "boxed value byte-view read" state addr

        let targetSize = CliType.sizeOf targetTemplate

        if byteOffset < 0 || targetSize > payloadSize - byteOffset then
            failwith
                $"boxed value byte-view read at offset %d{byteOffset} for %d{targetSize} bytes is outside %d{payloadSize}-byte boxed payload at %O{addr}"

        CliValueType.BytesAt byteOffset targetSize existing.Contents
        |> CliType.ofBytesLike targetTemplate

    let private readStackMemoryBytesAs
        (state : IlMachineState)
        (thread : ThreadId)
        (frame : FrameId)
        (block : StackMemoryBlockId)
        (byteOffset : int)
        (targetTemplate : CliType)
        : CliType
        =
        let targetSize = CliType.sizeOf targetTemplate

        if byteOffset < 0 then
            failwith
                $"local memory byte-view read at offset %d{byteOffset} in %O{block} is outside the block (negative offset)"

        let pool = IlMachineThreadState.getStackMemoryPool thread frame state
        let blockData = StackMemoryPool.getBlock block pool

        if int64 byteOffset + int64 targetSize > int64 blockData.Size then
            failwith
                $"local memory byte-view read at offset %d{byteOffset} for %d{targetSize} bytes is outside %O{block} of size %d{blockData.Size}"

        // Fast path that preserves provenance: when a typed cell starts at
        // exactly `byteOffset`, matches the requested size, AND has the same
        // CLI shape as the requested template, return it directly. This keeps
        // `NativeIntSource.FieldHandlePtr` and other tagged-pointer cells
        // intact across `ldind`-style typed reads, where the byte-walk
        // fallback would refuse via `byteAddressableCellBytesAt`. The shape
        // gate matters because, e.g., an `Int32` cell and a `Float32` template
        // have the same size but distinct meanings — falling through here
        // forces a proper bit-reinterpret via the byte path.
        //
        // The byte-addressability gate (symmetric with
        // `tryReadHeapValueFieldPrecise`) is what justifies the widened
        // `haveSameCliShape` here: when the stored cell is byte-addressable
        // we still defer to the byte-walk, which reconstructs the value in
        // the *template*'s exact CLI shape via `CliType.ofBytesLike`. So
        // callers reading with a bare `UInt8` template against a byte-sized
        // wrapped cell (or vice versa) get the requested shape back, while
        // non-byte-addressable storage (tagged `NativeIntSource` etc.) keeps
        // its storage shape so provenance survives.
        let fastPath =
            match StackMemoryPool.tryReadCell block byteOffset pool with
            | Some cell when CliType.sizeOf cell = targetSize && haveSameCliShape cell targetTemplate ->
                match CliType.ByteAddressability cell with
                | CliByteAddressability.Rejected _ -> Some cell
                | CliByteAddressability.ByteAddressable -> None
            | _ -> None

        match fastPath with
        | Some cell -> cell
        | None ->

        let buf = StackMemoryPool.readBytes block byteOffset targetSize pool
        CliType.ofBytesLike targetTemplate buf

    /// Mirror of `readStackMemoryBytesAs` for native-heap blocks. Use-after-free is
    /// reported by `NativeMemoryPool.getBlock` if the block was freed.
    let private readNativeMemoryBytesAs
        (state : IlMachineState)
        (block : NativeMemoryBlockId)
        (byteOffset : int)
        (targetTemplate : CliType)
        : CliType
        =
        let targetSize = CliType.sizeOf targetTemplate

        if byteOffset < 0 then
            failwith
                $"native-heap byte-view read at offset %d{byteOffset} in %O{block} is outside the block (negative offset)"

        let pool = state.Kernel.NativeMemoryPool
        let blockData = NativeMemoryPool.getBlock block pool

        if int64 byteOffset + int64 targetSize > int64 blockData.Size then
            failwith
                $"native-heap byte-view read at offset %d{byteOffset} for %d{targetSize} bytes is outside %O{block} of size %d{blockData.Size}"

        let fastPath =
            match NativeMemoryPool.tryReadCell block byteOffset pool with
            | Some cell when CliType.sizeOf cell = targetSize && haveSameCliShape cell targetTemplate ->
                match CliType.ByteAddressability cell with
                | CliByteAddressability.Rejected _ -> Some cell
                | CliByteAddressability.ByteAddressable -> None
            | _ -> None

        match fastPath with
        | Some cell -> cell
        | None ->

        let buf = NativeMemoryPool.readBytes block byteOffset targetSize pool
        CliType.ofBytesLike targetTemplate buf

    /// Read the byte range at `src` and rebuild a value of CLI shape compatible
    /// with `targetTemplate`. **Output shape is path-dependent for primitive-
    /// like wrapper templates against non-byte-addressable storage.** When a
    /// typed storage cell starts at the byref, `haveSameCliShape` accepts it,
    /// and the cell is non-byte-addressable, the cell is returned as-is — so
    /// a wrapped `IntPtr` template against a bare tagged `Numeric (NativeInt
    /// FieldHandlePtr ...)` cell returns the bare cell with its provenance
    /// intact (the byte-walk fallback cannot serialise tagged sources, so
    /// this fast path is load-bearing). When the cell is byte-addressable,
    /// the fast path defers to the byte-walk, which produces a value with
    /// the *template*'s exact CLI shape. Callers reading with a primitive
    /// template that may land on tagged-pointer storage must reconcile the
    /// returned shape via `EvalStackValue.ofCliType` (which flattens
    /// primitive-like wrappers) or an explicit
    /// `CliType.unwrapPrimitiveLikeDeep`.
    let internal zeroForConcreteType
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ty : ConcreteType<ConcreteTypeHandle>)
        : CliType
        =
        let handle =
            AllConcreteTypes.findExistingConcreteType state.ConcreteTypes ty.Identity ty.Generics
            |> Option.defaultWith (fun () ->
                failwith $"ReinterpretAs target %O{ty} is not present in the concrete-type registry"
            )

        CliType.zeroOf state.ConcreteTypes state._LoadedAssemblies baseClassTypes handle
        |> fst

    /// Forward walk through a `ByrefProjection` chain, accumulating a byte
    /// offset. `Field` projections consult `templateThunk` for the current
    /// type cursor (lazy, so chains containing no `Field` never resolve a
    /// template); `ReinterpretAs` re-anchors the cursor via `templateFor`;
    /// `ByteOffset` adds raw bytes and leaves the cursor alone.
    ///
    /// A `Field` *after* a `ByteOffset` resolves against the carried cursor,
    /// which is what `ldflda` on a `ref T` sitting `n` bytes along means in
    /// the real runtime: `base + n + offsetof(field, T)`, with no check on
    /// `n`. The cursor is still `T`-typed because
    /// `ManagedPointerSource.appendProjection` only appends a `ByteOffset` to
    /// a chain already ending in a `ReinterpretAs` (and the two sites that
    /// build a chain directly, `RuntimeFieldProjection`'s `RawData::Data`
    /// projection and `normaliseTrailingByteOffset`, both emit the
    /// `ReinterpretAs` immediately before it). So a byte cursor always
    /// qualifies a reinterpret, and that reinterpret's target is the anchor.
    /// `peelTrailingByteView` strips the leading `ReinterpretAs` before
    /// calling in, which is why the suffix may *begin* with a `ByteOffset`:
    /// the anchor is then the `rootTemplate` the caller supplies.
    ///
    /// The genuine violation is the mirror image — a `ByteOffset` hung off a
    /// `Field` navigation, with no reinterpret to say what type the raw bytes
    /// are being viewed as. `appendProjection` refuses to construct that, so
    /// reaching here with it means a chain was built directly and got it
    /// wrong; this raises a descriptive `failwith` so the caller can decide
    /// whether to propagate the bug or degrade.
    let internal walkProjectionByteOffset
        (templateFor : ConcreteType<ConcreteTypeHandle> -> CliType)
        (rootTemplate : unit -> CliType)
        (projs : ByrefProjection list)
        : int
        =
        // Structural precondition, checked once up front rather than woven
        // through the fold: it is a property of the chain alone, and keeping
        // it separate leaves the walk a plain accumulation.
        let rec checkAnchored (remaining : ByrefProjection list) : unit =
            match remaining with
            | ByrefProjection.Field _ :: (ByrefProjection.ByteOffset n :: _) ->
                failwith
                    $"Field navigation followed by ByteOffset %d{n} without an intervening ReinterpretAs in projection chain: %A{projs} (this is an interpreter bug)"
            | _ :: rest -> checkAnchored rest
            | [] -> ()

        checkAnchored projs

        let rec walk (templateThunk : unit -> CliType) (offset : int) (remaining : ByrefProjection list) : int =
            match remaining with
            | [] -> offset
            | ByrefProjection.Field field :: rest ->
                let template = templateThunk ()
                let fieldOffset, _ = CliType.getFieldLayoutById field template
                let fieldTemplate = CliType.getFieldById field template
                walk (fun () -> fieldTemplate) (offset + fieldOffset) rest
            | ByrefProjection.ReinterpretAs newReinTy :: rest -> walk (fun () -> templateFor newReinTy) offset rest
            | ByrefProjection.ByteOffset n :: rest -> walk templateThunk (offset + n) rest

        walk rootTemplate 0 projs

    /// Split a projection chain at the first `ReinterpretAs` and collapse
    /// everything beyond that point into an accumulated byte offset. Once a
    /// `ReinterpretAs` appears the underlying storage is being treated as raw
    /// bytes, so subsequent `Field` (resolved against the most recent
    /// `ReinterpretAs` target), `ByteOffset`, and chained `ReinterpretAs`
    /// projections are all bytewise. Walking forward through them accumulates
    /// the byte offset that the byte-view ultimately addresses, leaving the
    /// structural prefix (everything before the first `ReinterpretAs`) for
    /// the dispatcher.
    ///
    /// The forward walk is strictly more general than a right-to-left
    /// per-pair peel: it handles `[ReinterpretAs Outer; Field I; Field Y]`
    /// (e.g. `Volatile.Write(ref view.I.Y, _)` on
    /// `Unsafe.As<int, Outer>(ref arr[0])`), where the second `Field` would
    /// be unreachable from the right because its layout depends on the type
    /// chosen by the preceding `Field`.
    ///
    /// Returns `ValueSome (structuralPrefix, offset)` when the chain contains
    /// at least one `ReinterpretAs`, else `ValueNone`. The `structuralPrefix`
    /// never contains a `ReinterpretAs` by construction. A `ByteOffset` hung
    /// off a `Field` navigation, with no reinterpret to anchor it, is a
    /// construction-site invariant violation and is raised by the walk; see
    /// `walkProjectionByteOffset`.
    ///
    /// `baseClassTypes` is required only when the byte-view suffix navigates
    /// through a `Field` projection (Field layout is resolved against the
    /// current type template, which requires metadata). Metadata-light
    /// callers (the BCT-less `writeManagedByref` entry point used by
    /// primitive/external boundaries that do not currently carry type
    /// metadata) may pass `None`; their canonical chain shapes are
    /// `[..., ReinterpretAs T]` and `[..., ReinterpretAs T; ByteOffset n]`,
    /// whose suffixes contain no `Field` and therefore need no template. A
    /// BCT-less call with a `Field` in the byte-view suffix is an interpreter
    /// bug (the construction site that emitted such a chain ought to carry
    /// BCT) and is raised here with a descriptive message.
    let private peelTrailingByteView
        (baseClassTypes : BaseClassTypes<DumpedAssembly> option)
        (state : IlMachineState)
        (projs : ByrefProjection list)
        : (ByrefProjection list * int) voption
        =
        let rec findFirstReinterpret
            (revPrefix : ByrefProjection list)
            (remaining : ByrefProjection list)
            : (ByrefProjection list * ConcreteType<ConcreteTypeHandle> * ByrefProjection list) option
            =
            match remaining with
            | [] -> None
            | ByrefProjection.ReinterpretAs reinTy :: rest -> Some (List.rev revPrefix, reinTy, rest)
            | proj :: rest -> findFirstReinterpret (proj :: revPrefix) rest

        match findFirstReinterpret [] projs with
        | None -> ValueNone
        | Some (structuralPrefix, firstReinTy, afterReinterpret) ->
            // Walk forward through the byte-view suffix, accumulating byte
            // offset. Lazy template resolution means BCT is only consulted
            // when a `Field` actually appears in the byte-view suffix, so
            // the metadata-light call shapes (`[ReinterpretAs T]`,
            // `[ReinterpretAs T; ByteOffset n]`) work with
            // `baseClassTypes = None`.
            let templateFor (ty : ConcreteType<ConcreteTypeHandle>) : CliType =
                match baseClassTypes with
                | Some bct -> zeroForConcreteType bct state ty
                | None ->
                    failwith
                        $"peelTrailingByteView: BaseClassTypes required to navigate `Field` projection after `ReinterpretAs` %s{ty.Namespace}.%s{ty.Name} in projection chain: %A{projs} (metadata-light entry points cannot resolve Field layout; pass BaseClassTypes via writeManagedByrefWithBase)"

            let totalOffset =
                walkProjectionByteOffset templateFor (fun () -> templateFor firstReinTy) afterReinterpret

            ValueSome (structuralPrefix, totalOffset)


    let readManagedByrefBytesAs
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (src : ManagedPointerSource)
        (targetTemplate : CliType)
        : CliType
        =
        match src with
        | ManagedPointerSource.Null -> failwith "TODO: throw NullReferenceException"
        | ManagedPointerSource.NativeIntPlaceholder bits ->
            failwith
                $"readManagedByrefBytesAs: cannot dereference fake non-null byref @ 0x%x{bits}; the placeholder must never be read"
        | ManagedPointerSource.Byref (ByrefRoot.HeapValue addr, []) -> readHeapValueBytesAs state addr 0 targetTemplate
        | ManagedPointerSource.Byref (ByrefRoot.StackMemoryByte (thread, frame, block, byteOffset), []) ->
            readStackMemoryBytesAs state thread frame block byteOffset targetTemplate
        | ManagedPointerSource.Byref (ByrefRoot.NativeMemoryByte (block, byteOffset), []) ->
            readNativeMemoryBytesAs state block byteOffset targetTemplate
        | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, index), []) ->
            readArrayBytesAs state arr index 0 targetTemplate
        | ManagedPointerSource.Byref (ByrefRoot.PeByteRange peByteRange, []) ->
            readPeByteRangeBytesAs state peByteRange 0 targetTemplate
        | ManagedPointerSource.Byref (ByrefRoot.StringCharAt (str, charIndex), []) ->
            readStringBytesAs state str charIndex 0 targetTemplate
        | ManagedPointerSource.Byref (outerRoot, outerProjs) ->
            // Collapse any trailing byte-view segment of the projection
            // chain into an accumulated byte offset. Once a `ReinterpretAs`
            // appears the underlying storage is being treated as raw bytes,
            // so subsequent `ByteOffset`, `Field` (resolved against the
            // most recent `ReinterpretAs` target), and chained `ReinterpretAs`
            // projections are all bytewise. Peeling them exposes the
            // residual structural prefix to the existing dispatchers so
            // the bytewise read can route through the appropriate root
            // reader (or, when the prefix navigates into a host-shaped
            // cell, through `resolveCell`). The BCL's cast-cache walk
            // (`Unsafe.As<byte, CastCacheEntry>(...)` then `entry._version`
            // then `Volatile.Read` wrapping the result in `VolatileUInt32`)
            // is the load-bearing example.
            let byteViewShape : (ByrefProjection list * int) voption =
                peelTrailingByteView (Some baseClassTypes) state outerProjs

            match byteViewShape with
            | ValueSome (prefixProjs, byteOffset) ->
                match outerRoot, prefixProjs with
                | ByrefRoot.StackMemoryByte (thread, frame, block, rootByteOffset), [] ->
                    readStackMemoryBytesAs state thread frame block (rootByteOffset + byteOffset) targetTemplate
                | ByrefRoot.NativeMemoryByte (block, rootByteOffset), [] ->
                    readNativeMemoryBytesAs state block (rootByteOffset + byteOffset) targetTemplate
                | ByrefRoot.ArrayElement (arr, index), [] -> readArrayBytesAs state arr index byteOffset targetTemplate
                | ByrefRoot.PeByteRange peByteRange, [] ->
                    readPeByteRangeBytesAs state peByteRange byteOffset targetTemplate
                | ByrefRoot.PeByteRange peByteRange, prefixProjs ->
                    failwith
                        $"TODO: PE byte-view read with non-empty prefix projections %O{prefixProjs}: %O{peByteRange}"
                | ByrefRoot.StringCharAt (str, charIndex), [] ->
                    readStringBytesAs state str charIndex byteOffset targetTemplate
                | ByrefRoot.HeapValue addr, [] -> readHeapValueBytesAs state addr byteOffset targetTemplate
                | _, prefixProjs ->
                    let rootValue = readRootValue state outerRoot
                    let targetSize = CliType.sizeOf targetTemplate

                    // CLR pointer arithmetic on a managed pointer to a struct
                    // field is allowed to cross into sibling fields of the parent
                    // (e.g. `Unsafe.Add(ref guid._a, 1)` reaches `_b`/`_c`). When
                    // the byte read overflows the immediate cell, lift back
                    // through trailing `Field` projections, accumulating each
                    // field's offset within its parent until the read fits.
                    let rec resolveCell (projs : ByrefProjection list) (offset : int) : CliType * int =
                        let cell = readProjectedValue rootValue projs
                        let cellSize = byteAddressableCellSize $"single-cell byref %O{src}" cell

                        if offset >= 0 && targetSize <= cellSize - offset then
                            cell, offset
                        else
                            match List.tryLast projs with
                            | Some (ByrefProjection.Field field) ->
                                let parentProjs = projs |> List.take (List.length projs - 1)
                                let parentValue = readProjectedValue rootValue parentProjs
                                let fieldOffset, _ = CliType.getFieldLayoutById field parentValue
                                resolveCell parentProjs (offset + fieldOffset)
                            | _ ->
                                failwith
                                    $"TODO: byte-view read at offset %d{offset} for %d{targetSize} bytes does not fit in single primitive cell of size %d{cellSize}: %O{src}"

                    // Storage with no byte image — a value type holding object references — cannot
                    // be indexed by `resolveCell`, which lifts scope outward until a *byte* read
                    // fits. Descend instead: if the range is exactly some cell's extent, that cell
                    // is the read. `resolveCell` widens, this narrows; between them the walk is
                    // total for the shapes the byte path cannot represent.
                    let named =
                        let cellHere = readProjectedValue rootValue prefixProjs

                        tryNameCellForByrefAccess byteOffset cellHere targetTemplate
                        |> Option.map (fun path -> CliType.getCellAtPath path cellHere)

                    match named with
                    | Some cell -> cell
                    | None ->

                    let cell, finalOffset = resolveCell prefixProjs byteOffset

                    let bytes =
                        byteAddressableCellBytesAt $"single-cell byref %O{src}" finalOffset targetSize cell

                    CliType.ofBytesLike targetTemplate bytes
            | ValueNone ->
                let raw = readProjectedValue (readRootValue state outerRoot) outerProjs
                let rawSize = byteAddressableCellSize $"plain byref %O{src}" raw
                let targetSize = CliType.sizeOf targetTemplate

                if targetSize > rawSize then
                    failwith
                        $"TODO: byte-view read of %d{targetSize} bytes does not fit in plain primitive cell of size %d{rawSize}: %O{src}"

                byteAddressableCellBytesAt $"plain byref %O{src}" 0 targetSize raw
                |> CliType.ofBytesLike targetTemplate

    let readManagedByref
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (src : ManagedPointerSource)
        : CliType
        =
        match src with
        | ManagedPointerSource.Null -> failwith "TODO: throw NullReferenceException"
        | ManagedPointerSource.NativeIntPlaceholder bits ->
            failwith
                $"readManagedByref: cannot dereference fake non-null byref @ 0x%x{bits}; the placeholder must never be read"
        | ManagedPointerSource.Byref (root, projs) ->
            match List.rev projs with
            | ByrefProjection.ByteOffset _ :: ByrefProjection.ReinterpretAs ty :: _
            | ByrefProjection.ReinterpretAs ty :: _ ->
                let targetTemplate = zeroForConcreteType baseClassTypes state ty

                // CoreLib hosts the captured ExecutionContext inside Task.m_stateObject
                // (typed object?) and exposes it via `ref Unsafe.As<object?, ExecutionContext?>(ref m_stateObject)`.
                // The resulting byref shape is `[..., ReinterpretAs RefType]` over storage
                // whose immediate cell is an ObjectRef; a subsequent Ldind.ref then needs
                // the stored reference unchanged. Object references are not byte-addressable
                // in our value model, so routing this through the bytewise path would
                // refuse. Mirror the write side's structural-write escape (see
                // `useStructuralWriter` in `writeManagedByrefCore`) by short-circuiting
                // here when the peeled byte offset is exactly zero and both the storage
                // cell and the reinterpret target are reference-typed: return the cell
                // unchanged. Non-zero offsets still fail loudly in the bytewise path —
                // a mid-cell view of an ObjectRef has no defined meaning — and any other
                // storage/target shape continues to route through the bytewise dispatcher.
                //
                // The elision is gated on the root having a typed cell. Byte-only roots
                // (`StackMemoryByte`, `NativeMemoryByte`, `PeByteRange`) have no
                // structural value to read — `readRootValue` would throw — and a
                // `ReinterpretAs RefType` over them really does mean "read these bytes
                // as a reference", which only the bytewise path can answer.
                let isTypedCellRoot =
                    match root with
                    | ByrefRoot.StackMemoryByte _
                    | ByrefRoot.NativeMemoryByte _
                    | ByrefRoot.PeByteRange _ -> false
                    | _ -> true

                let elideAsObjectRefCell () : CliType option =
                    if not isTypedCellRoot then
                        None
                    else
                        match peelTrailingByteView (Some baseClassTypes) state projs with
                        | ValueSome (structuralPrefix, byteOffset) ->
                            let cell = readProjectedValue (readRootValue state root) structuralPrefix

                            match cell, targetTemplate with
                            | CliType.ObjectRef _, CliType.ObjectRef _ ->
                                // A whole-cell view of a reference is the reference; a *mid-cell*
                                // one has no defined meaning, so only offset 0 elides here.
                                if byteOffset = 0 then Some cell else None
                            | CliType.ValueType _, CliType.ObjectRef _ ->
                                // The storage is a value type but the target is a bare
                                // reference: `Unsafe.As<TBuffer, T>(ref buffer)`, optionally
                                // walked forward by `Unsafe.Add`, where the byte range picked
                                // out is exactly one reference-typed cell of `TBuffer` — as
                                // `[InlineArray(N)]` over a reference element generates for
                                // every slot. The byref addresses precisely that cell, so we
                                // can hand it back. Storage where the range merely *straddles*
                                // a reference, or is aliased by a sibling, names no cell and
                                // still routes bytewise, where it fails loudly rather than
                                // silently dropping the rest of the struct.
                                tryNameCellForByrefAccess byteOffset cell targetTemplate
                                |> Option.map (fun path -> CliType.getCellAtPath path cell)
                            | _ -> None
                        | _ -> None

                match elideAsObjectRefCell () with
                | Some cell -> cell
                | None -> readManagedByrefBytesAs baseClassTypes state src targetTemplate
            | ByrefProjection.ByteOffset n :: _ ->
                failwith
                    $"ByteOffset %d{n} without a preceding ReinterpretAs in projection chain: %O{src} (this is an interpreter bug)"
            | _ -> readProjectedValue (readRootValue state root) projs

    /// Outcome of classifying the projection
    /// `[..., ReinterpretAs reinterpretTy, Field field]` over storage of some
    /// `CliType` value.
    ///
    /// `ElideAsField` (Phase A) signals that the reinterpret target is a
    /// transparent offset-0 single-field wrapper whose only field is
    /// layout-compatible with the storage value itself, so reads return the
    /// storage cell and writes overwrite the storage cell directly.
    ///
    /// `ElideAsStorageInnerField` (Phase B) signals that *both* the reinterpret
    /// target and the storage are transparent offset-0 single-field wrappers
    /// of a layout-compatible primitive (the canonical example is the BCL
    /// `Unsafe.As<TaskAwaiter<T>, TaskAwaiter>` motif, where both are
    /// single-`object`-field structs). Reads return the storage's inner field;
    /// writes replace only that inner field, preserving the outer wrapper.
    /// The payload is the storage-side `FieldId` to access via
    /// `CliType.getFieldById` / `withFieldSetById`.
    ///
    /// `NotTransparent` means the access must go through the bytewise
    /// reinterpret path; callers whose storage cannot be byte-addressed (a
    /// bare `ObjectRef`, or a value type holding one) must produce their own
    /// diagnostic in that branch.
    type private TransparentWrapperOutcome =
        | ElideAsField of FieldId
        | ElideAsStorageInnerField of FieldId list
        | NotTransparent

    /// The storage cell a `ReinterpretAs`-then-`Field` byref names, if any. The field sits
    /// `fieldOffset` bytes into the reinterpret target, which itself sits `byteOffset` bytes into
    /// the storage, so the byref addresses whatever cell occupies that sum.
    ///
    /// Shared by the write-side dispatcher, which asks whether such a cell exists in order to
    /// route the write, and by the writer that then installs into it. Asking and answering with
    /// one function is what keeps them from disagreeing about which accesses are serviceable.
    let private tryNameCellThroughReinterpretField
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (byteOffset : int)
        (storageValue : CliType)
        (reinterpretTy : ConcreteType<ConcreteTypeHandle>)
        (field : FieldId)
        : FieldId list option
        =
        let reinterpretZero = zeroForConcreteType baseClassTypes state reinterpretTy
        let fieldOffset, _ = CliType.getFieldLayoutById field reinterpretZero
        let fieldTemplate = CliType.getFieldById field reinterpretZero

        tryNameCellForByrefAccess (byteOffset + fieldOffset) storageValue fieldTemplate

    /// Classifier shared by the read- and write-side `ReinterpretAs+Field`
    /// dispatchers. See `TransparentWrapperOutcome` for the cases.
    let private classifyTransparentWrapper
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (storageValue : CliType)
        (reinterpretTy : ConcreteType<ConcreteTypeHandle>)
        (field : FieldId)
        : TransparentWrapperOutcome
        =
        let targetTemplate = zeroForConcreteType baseClassTypes state reinterpretTy

        match targetTemplate with
        | CliType.ValueType cvt ->
            // `FieldsAt 0` lists every field that *starts* at offset 0; an
            // explicit-layout overlap there yields more than one, in which case
            // eliding through one field would silently leave an overlapping
            // sibling stale on write. Combined with `targetSize = f.Size`, no
            // field can start at any offset in `(0, targetSize)` without
            // running past the wrapper, so the single offset-0 entry is the
            // wrapper's full extent. Raw-bytes storage returns `[]` from
            // `TryFieldsAt` and so falls through to `NotTransparent`.
            match CliValueType.TryFieldsAt 0 cvt with
            | [ f ] when
                f.Id = field
                && f.Size = CliType.sizeOf f.Contents
                && CliType.sizeOf targetTemplate = f.Size
                ->
                // Phase A: storage IS layout-compatible with the field cell
                // (the bare ObjectRef case, e.g. `Unsafe.As<object,
                // ObjectWrapper>` on an `object` heap field).
                if isCellIdentityCompatible storageValue f.Contents then
                    TransparentWrapperOutcome.ElideAsField field
                else
                    // Phase B: storage is itself a transparent offset-0
                    // single-field wrapper of a layout-compatible primitive
                    // (the `Unsafe.As<TaskAwaiter<T>, TaskAwaiter>` motif).
                    // The same size/single-offset-0-field gates apply
                    // symmetrically on the storage side.
                    match tryNameCellForByrefAccess 0 storageValue f.Contents with
                    | Some innerPath -> TransparentWrapperOutcome.ElideAsStorageInnerField innerPath
                    | None -> TransparentWrapperOutcome.NotTransparent
            | _ -> TransparentWrapperOutcome.NotTransparent
        | _ -> TransparentWrapperOutcome.NotTransparent

    let private readReinterpretedByrefField
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (src : ManagedPointerSource)
        (reinterpretTy : ConcreteType<ConcreteTypeHandle>)
        (field : FieldId)
        : CliType
        =
        let targetTemplate = zeroForConcreteType baseClassTypes state reinterpretTy
        let fieldTemplate = CliType.getFieldById field targetTemplate
        let fieldOffset, _ = CliType.getFieldLayoutById field targetTemplate

        match fieldTemplate with
        | CliType.ObjectRef _ ->
            // Object-reference storage is not byte-addressable, so the bytewise
            // reinterpret path can never serve this access; the classifier
            // decides whether the projection is a transparent single-field
            // wrapper that we can pass through to the underlying ObjectRef
            // cell, and otherwise we surface a diagnostic in place of the
            // unreachable bytewise fallback.
            match splitTrailingByteView src with
            | ValueSome (root, prefixProjs, byteOffset) ->
                let storageValue = readProjectedValue (readRootValue state root) prefixProjs

                // The field sits at `fieldOffset` within the reinterpret target, which itself sits
                // at `byteOffset` within the storage, so the reference the byref names occupies
                // that sum. When some cell has exactly that extent, that cell *is* the reference
                // and no wrapper reasoning is needed — this is the general case, of which the
                // classifier below handles the shapes it cannot reach: storage that is itself a
                // bare `ObjectRef` has no cells to name.
                let named =
                    tryNameCellForByrefAccess (byteOffset + fieldOffset) storageValue fieldTemplate
                    |> Option.map (fun path -> CliType.getCellAtPath path storageValue)

                match named with
                | Some cell -> cell
                | None ->

                match classifyTransparentWrapper baseClassTypes state storageValue reinterpretTy field with
                | TransparentWrapperOutcome.ElideAsField _ when byteOffset = 0 -> storageValue
                | TransparentWrapperOutcome.ElideAsStorageInnerField innerPath when byteOffset = 0 ->
                    CliType.getCellAtPath innerPath storageValue
                | TransparentWrapperOutcome.ElideAsField _
                | TransparentWrapperOutcome.ElideAsStorageInnerField _ ->
                    failwith
                        $"TODO: transparent-wrapper read of object-reference field %O{field} through %O{reinterpretTy} at byte offset %d{byteOffset}; object-reference interior byte views are not modelled"
                | TransparentWrapperOutcome.NotTransparent ->
                    failwith
                        $"TODO: object-reference field %O{field} through %O{reinterpretTy} is not a transparent single-field wrapper of object-reference storage (storage cell %O{storageValue}); bytewise reinterpret over object-reference storage is not modelled"
            | ValueNone ->
                failwith
                    $"TODO: object-reference field %O{field} through %O{reinterpretTy} without a trailing ReinterpretAs byte-view shape: %O{src}"
        | CliType.RuntimePointer _ ->
            failwith
                $"TODO: runtime-pointer field %O{field} through %O{reinterpretTy}; pointer byte views are not modelled"
        | CliType.Numeric _
        | CliType.Bool _
        | CliType.Char _
        | CliType.ValueType _ ->
            let fieldPtr =
                if fieldOffset = 0 then
                    src
                else
                    ManagedPointerSource.appendProjection (ByrefProjection.ByteOffset fieldOffset) src

            readManagedByrefBytesAs baseClassTypes state fieldPtr fieldTemplate

    let readManagedByrefField
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (src : ManagedPointerSource)
        (field : FieldId)
        : CliType
        =
        match src with
        | ManagedPointerSource.Null -> failwith "TODO: throw NullReferenceException"
        | ManagedPointerSource.NativeIntPlaceholder bits ->
            failwith
                $"readManagedByrefField: cannot read field %O{field} through fake non-null byref @ 0x%x{bits}; the placeholder must never be dereferenced"
        | ManagedPointerSource.Byref (root, projs) ->
            match List.rev projs with
            | ByrefProjection.ByteOffset _ :: ByrefProjection.ReinterpretAs ty :: _
            | ByrefProjection.ReinterpretAs ty :: _ -> readReinterpretedByrefField baseClassTypes state src ty field
            | ByrefProjection.ByteOffset n :: _ ->
                failwith
                    $"ByteOffset %d{n} without a preceding ReinterpretAs in projection chain: %O{src} (this is an interpreter bug)"
            | _ ->
                readProjectedValue (readRootValue state root) projs
                |> CliType.getFieldById field

    let private applyProjectionsForWriteIfChanged
        (rootValue : CliType)
        (projs : ByrefProjection list)
        (newValue : CliType)
        : CliType option
        =
        let rec go (rootValue : CliType) (projs : ByrefProjection list) (newValue : CliType) : CliType option =
            match projs with
            | [] -> Some newValue
            | [ ByrefProjection.Field field ] -> Some (CliType.withFieldSetById field newValue rootValue)
            | ByrefProjection.Field field :: rest ->
                let fieldValue = CliType.getFieldById field rootValue

                match go fieldValue rest newValue with
                | None -> None
                | Some updatedField -> Some (CliType.withFieldSetById field updatedField rootValue)
            | ByrefProjection.ReinterpretAs ty :: _ ->
                failwith
                    $"TODO: write through `ReinterpretAs` as %s{ty.Namespace}.%s{ty.Name} followed by further projections; needs a bytewise implementation"
            | ByrefProjection.ByteOffset n :: _ ->
                // Symmetric to the readManagedByref ByteOffset case: byte-offset
                // writes go through Unsafe.WriteUnaligned (which scatters bytes
                // into the cell stream directly), not through the generic write
                // fold. Reaching here means a generic Stind at a non-zero byte
                // offset, which we don't yet model.
                failwith
                    $"TODO: writeManagedByref via ByteOffset %d{n} requires bytewise scatter; generic Stind at a non-zero byte offset is not modelled"

        go rootValue projs newValue

    let private writeArrayBytes
        (state : IlMachineState)
        (arr : ManagedHeapAddress)
        (index : int)
        (byteOffset : int)
        (bytes : byte[])
        : IlMachineState
        =
        let arrObj = state.ManagedHeap.Arrays.[arr]

        if arrObj.Length = 0 then
            failwith $"TODO: byte-view write to empty array %O{arr} at index %d{index} offset %d{byteOffset}"

        // Use `CliType.sizeOf` (not `byteAddressableCellSize`) for the stride.
        // Mirrors `readArrayBytesAs`: deriving the cell stride doesn't require
        // element 0 to be byte-renderable. Consider `fixed (IntPtr* p = arr)`
        // where `p[0] = typeof(int).TypeHandle.Value` populates element 0 with
        // non-byte-addressable provenance, then `p[1] = IntPtr.Zero` byte-
        // scatters into element 1: only the cells the loop actually touches
        // need to be byte-addressable, validated per iteration below.
        let firstCellSize = CliType.sizeOf arrObj.Elements.[0]

        let cellAdvance, inCellStart = floorDivRem byteOffset firstCellSize
        let mutable state = state
        let mutable filled = 0
        let mutable cell = index + cellAdvance
        let mutable inCellOffset = inCellStart

        while filled < bytes.Length do
            if cell < 0 || cell >= arrObj.Length then
                failwith $"TODO: byte-view write past array bounds at cell %d{cell} of length %d{arrObj.Length}"

            let existing = state.ManagedHeap.Arrays.[arr].Elements.[cell]

            // Deriving how much of this cell the write covers doesn't require the cell to be
            // byte-renderable, for the same reason the stride above doesn't; the byte-view
            // path below validates before it actually renders anything.
            let cellSize = CliType.sizeOf existing
            let canTake = cellSize - inCellOffset
            let take = min canTake (bytes.Length - filled)

            if take <= 0 then
                failwith
                    $"byte-view write to array %O{arr} element %d{cell} made no progress: cell size %d{cellSize}, in-cell offset %d{inCellOffset}, %d{bytes.Length - filled} byte(s) still to write"

            let cellBytes = bytes.[filled .. filled + take - 1]

            // A run of zero bytes is the one byte-level write that is meaningful against
            // storage with no byte rendering.
            let updated =
                if cellBytes |> Array.forall (fun b -> b = 0uy) then
                    // The slot count the BCL derives is `byteLength / sizeof(IntPtr)`, which
                    // covers the element exactly only because a GC-containing value type ends on
                    // a pointer boundary. `CliValueType.SizeOfFieldStorage` guarantees that, so
                    // this should be unreachable; if it ever fires, the element's size is the
                    // bug and the clear would otherwise truncate, leaving the tail of the
                    // element quietly set. Refuse rather than giving a wrong answer.
                    if CliType.ContainsObjectReferences existing && cellSize % NATIVE_INT_SIZE <> 0 then
                        failwith
                            $"array %O{arr} element %d{cell} contains object references but its %d{cellSize}-byte size is not a multiple of %d{NATIVE_INT_SIZE}; such a value type must be sized to a pointer multiple by CliValueType.SizeOfFieldStorage, so its layout is wrong rather than this clear."

                    CliType.WithZeroedRangeIfChanged inCellOffset take existing
                else
                    withByteAddressableCellBytesAtIfChanged
                        $"array %O{arr} element %d{cell}"
                        inCellOffset
                        cellBytes
                        existing

            match updated with
            | None -> ()
            | Some newCell -> state <- IlMachineThreadState.setArrayValue arr newCell cell state

            filled <- filled + take
            cell <- cell + 1
            inCellOffset <- 0

        state

    let private writeStackMemoryBytesAt
        (state : IlMachineState)
        (thread : ThreadId)
        (frame : FrameId)
        (block : StackMemoryBlockId)
        (byteOffset : int)
        (bytes : byte[])
        : IlMachineState
        =
        if bytes.Length = 0 then
            state
        else

        match tryReadInitializedStackMemoryBytes state thread frame block byteOffset bytes.Length with
        | ValueSome existing when bytesEqual existing bytes -> state
        | _ ->

        let pool = IlMachineThreadState.getStackMemoryPool thread frame state
        let pool = StackMemoryPool.writeBytes block byteOffset bytes pool
        IlMachineThreadState.setStackMemoryPool thread frame pool state

    let private writeNativeMemoryBytesAt
        (state : IlMachineState)
        (block : NativeMemoryBlockId)
        (byteOffset : int)
        (bytes : byte[])
        : IlMachineState
        =
        if bytes.Length = 0 then
            state
        else

        match tryReadInitializedNativeMemoryBytes state block byteOffset bytes.Length with
        | ValueSome existing when bytesEqual existing bytes -> state
        | _ ->

        let pool =
            NativeMemoryPool.writeBytes block byteOffset bytes state.Kernel.NativeMemoryPool

        IlMachineThreadState.setNativeMemoryPool pool state

    let private writeStringBytes
        (state : IlMachineState)
        (str : ManagedHeapAddress)
        (charIndex : int)
        (byteOffset : int)
        (bytes : byte[])
        : IlMachineState
        =
        let cellAdvance, inCellStart = floorDivRem byteOffset 2
        let mutable state = state
        let mutable filled = 0
        let mutable cell = charIndex + cellAdvance
        let mutable inCellOffset = inCellStart
        let cellSize = CliType.sizeOf (CliType.ofChar (char 0))

        while filled < bytes.Length do
            let existingChar = ManagedHeap.getStringChar str cell state.ManagedHeap
            let canTake = cellSize - inCellOffset
            let take = min canTake (bytes.Length - filled)
            let cellBytes = bytes.[filled .. filled + take - 1]
            let existingCell = CliType.ofChar existingChar

            match CliType.WithBytesAtIfChanged inCellOffset cellBytes existingCell with
            | None -> ()
            | Some (CliType.Char (high, low)) ->
                let newChar = char (int high * 256 + int low)

                state <-
                    { state with
                        ManagedHeap = ManagedHeap.setStringChar str cell newChar state.ManagedHeap
                    }
            | Some other -> failwith $"string byte-view write reconstructed non-char value %O{other}"

            filled <- filled + take
            cell <- cell + 1
            inCellOffset <- 0

        state

    let private writeHeapValueBytes
        (state : IlMachineState)
        (addr : ManagedHeapAddress)
        (byteOffset : int)
        (bytes : byte[])
        : IlMachineState
        =
        let existing, payloadSize =
            heapValueByteSize "boxed value byte-view write" state addr

        if byteOffset < 0 || bytes.Length > payloadSize - byteOffset then
            failwith
                $"boxed value byte-view write at offset %d{byteOffset} for %d{bytes.Length} bytes is outside %d{payloadSize}-byte boxed payload at %O{addr}"

        match CliValueType.WithBytesAtIfChanged byteOffset bytes existing.Contents with
        | None -> state
        | Some updatedContents ->
            let updated =
                { existing with
                    Contents = updatedContents
                }

            { state with
                ManagedHeap = ManagedHeap.set addr updated state.ManagedHeap
            }

    /// Whether `newValue` has the same storage shape as the existing `cell`,
    /// for the purposes of whole-cell replacement.
    ///
    /// Broadens the primitive-only `haveSameCliShape` (which intentionally
    /// rejects every `ValueType, ValueType` pair) to also accept user structs
    /// whose declared `ConcreteTypeHandle` matches the cell's.
    let private cellShapeMatches (cell : CliType) (newValue : CliType) : bool =
        if haveSameCliShape cell newValue then
            true
        else
            match cell, newValue with
            | CliType.ValueType cellVt, CliType.ValueType newVt -> cellVt.Declared = newVt.Declared
            | _ -> false

    /// Whether a whole-cell store of `newValue` may replace the existing
    /// `cell`, given that the two occupy exactly the same byte range.
    ///
    /// When both sides render as bytes, the replacement is equivalent to a
    /// byte scatter that the caller could have performed instead, so it is
    /// always allowed, even if the new value has a different shape.
    /// The byte rendering already preserves whatever information the caller
    /// would need to recover the shape.
    ///
    /// When either side has no byte rendering — a tagged pointer, an object
    /// reference, a value type containing either — byte scatter is not
    /// available, so whole-cell replacement is the *only* spelling of the
    /// store. It is still fine over a primitive-shaped cell: the guest is
    /// discarding a scalar and a same-width scalar takes its place, which is
    /// exactly the documented restamp. It is not fine over a `ValueType` cell
    /// unless the shape survives, because a struct cell's identity *is* its
    /// layout: a wrapper struct exactly covers an `IntPtr`-width store, but
    /// restamping it as a bare numeric cell leaves a later `ldobj` of the
    /// wrapper type unable to read what was written. Nothing can reconstruct
    /// that, so such a store must decline here and fail loud downstream rather
    /// than silently corrupt the slot.
    let private wholeCellReplacementPreservesShape (cell : CliType) (newValue : CliType) : bool =
        match CliType.ByteAddressability cell, CliType.ByteAddressability newValue with
        | CliByteAddressability.ByteAddressable, CliByteAddressability.ByteAddressable -> true
        | _ ->
            match cell with
            | CliType.ValueType _ -> cellShapeMatches cell newValue
            | _ -> true

    /// True when `covering` — the result of a `tryFindCellCovering` at the
    /// destination offset — is a single existing cell that `newValue` replaces
    /// exactly (same start, same size, shape-preserving per
    /// `wholeCellReplacementPreservesShape`) and which cannot be rendered as
    /// bytes.
    ///
    /// Such a cell has no byte pattern to scatter over, so byte scatter fails
    /// loud; but an exact-width whole-cell replacement is well-defined, because
    /// the write covers precisely the bytes the old cell occupied. This is what
    /// lets `stack[i] = handle` be followed by `stack[i] = IntPtr.Zero`: the
    /// second store is byte-addressable and would otherwise take the scatter
    /// path into a cell that cannot accept it.
    ///
    /// The width condition is necessary, because a narrower store cannot
    /// come up with new values for the bytes it doesn't touch.
    let private replacesWholeNonByteAddressableCell
        (covering : (int * CliType) option)
        (byteOffset : int)
        (newValue : CliType)
        : bool
        =
        match covering with
        | None -> false
        | Some (cellOffset, cell) ->
            cellOffset = byteOffset
            && CliType.sizeOf cell = CliType.sizeOf newValue
            && wholeCellReplacementPreservesShape cell newValue
            && (
                match CliType.ByteAddressability cell with
                | CliByteAddressability.Rejected _ -> true
                | CliByteAddressability.ByteAddressable -> false
            )

    /// Whether a typed whole-cell store of `newValue` at `byteOffset` is
    /// observably equivalent to scattering its bytes there: it must replace at
    /// most one existing cell, exactly and shape-preservingly, and no other
    /// cell may intersect the destination range.
    let private stackMemoryByteTypedWriteSafe
        (pool : StackMemoryPool)
        (block : StackMemoryBlockId)
        (byteOffset : int)
        (newValue : CliType)
        : bool
        =
        let destSize = CliType.sizeOf newValue

        match StackMemoryPool.tryFindCellCovering block byteOffset pool with
        | Some (cellOffset, cell) ->
            cellOffset = byteOffset
            && CliType.sizeOf cell = destSize
            && wholeCellReplacementPreservesShape cell newValue
        | None ->
            let mutable safe = true
            let mutable i = byteOffset + 1
            let endOffset = byteOffset + destSize

            while safe && i < endOffset do
                match StackMemoryPool.tryFindCellCovering block i pool with
                | Some _ -> safe <- false
                | None -> i <- i + 1

            safe

    /// Native-heap mirror of `stackMemoryByteTypedWriteSafe`; same contract.
    let private nativeMemoryByteTypedWriteSafe
        (pool : NativeMemoryPool)
        (block : NativeMemoryBlockId)
        (byteOffset : int)
        (newValue : CliType)
        : bool
        =
        let destSize = CliType.sizeOf newValue

        match NativeMemoryPool.tryFindCellCovering block byteOffset pool with
        | Some (cellOffset, cell) ->
            cellOffset = byteOffset
            && CliType.sizeOf cell = destSize
            && wholeCellReplacementPreservesShape cell newValue
        | None ->
            let mutable safe = true
            let mutable i = byteOffset + 1
            let endOffset = byteOffset + destSize

            while safe && i < endOffset do
                match NativeMemoryPool.tryFindCellCovering block i pool with
                | Some _ -> safe <- false
                | None -> i <- i + 1

            safe

    /// Field-precise byte-view write: when `newValue` matches a *unique* instance field at
    /// exactly `byteOffset` with the same size and CLI shape, AND that field is itself
    /// non-byte-addressable (object-reference or runtime-pointer), update it directly via
    /// `WithFieldSetById` and return the new state. Returns `None` when no such field
    /// exists, so the caller falls through to byte scatter (which itself rejects when
    /// the heap object's storage isn't byte-addressable). Symmetric in *shape* to
    /// `tryReadHeapValueFieldPrecise`, but deliberately stricter on the shape predicate:
    /// the comparator here is `sameCliConstructor` (no primitive-like unwrap), so a
    /// wrapper-vs-bare `newValue` mismatch does *not* fire the install. Widening to
    /// `haveSameCliShape` here would let `WithFieldSetById` overwrite the field's CLI
    /// shape — e.g. coercing a boxed `IntPtr._value` from bare `NativeInt` to wrapped
    /// `IntPtr` when an `Unsafe.WriteUnaligned<IntPtr>` arrives — which is a silent
    /// corruption of the heap object's structural shape, not the recoverable read-side
    /// asymmetry that `haveSameCliShape` is designed for. The non-byte-addressable gate
    /// keeps byte-addressable primitive writes on the byte-scatter path so explicit-
    /// layout overlap semantics (resolved by `WithBytesAtIfChanged` and `EditedAtTime`)
    /// are preserved.
    let private tryWriteHeapValueFieldPrecise
        (state : IlMachineState)
        (addr : ManagedHeapAddress)
        (byteOffset : int)
        (newValue : CliType)
        : IlMachineState option
        =
        let obj = ManagedHeap.get addr state.ManagedHeap
        let destSize = CliType.sizeOf newValue

        let candidates =
            CliValueType.TryFieldsAt byteOffset obj.Contents
            |> List.filter (fun f -> f.Size = destSize && sameCliConstructor f.Contents newValue)

        match candidates with
        | [ f ] ->
            match CliType.ByteAddressability f.Contents with
            | CliByteAddressability.ByteAddressable -> None
            | CliByteAddressability.Rejected _ ->
                let updatedContents = CliValueType.WithFieldSetById f.Id newValue obj.Contents

                let updated =
                    { obj with
                        Contents = updatedContents
                    }

                { state with
                    ManagedHeap = ManagedHeap.set addr updated state.ManagedHeap
                }
                |> Some
        | _ -> None

    /// Element-precise byte-view write for an array element: when the destination cell
    /// is non-byte-addressable (object reference, runtime pointer) and the new value is
    /// the same CLI constructor of the same size at exactly `byteOffset = 0`, update the
    /// element directly. This is the array-element analogue of `tryWriteHeapValueFieldPrecise`
    /// and exists for the same reason: the byte-scatter path
    /// (`writeArrayBytes` → `withByteAddressableCellBytesAtIfChanged`) refuses
    /// non-byte-addressable cells, but the typed-cell write preserves their identity.
    /// The motivating case is `Volatile.Write(ref keys[index], key)`, whose lowering
    /// builds a byref over the reference-typed array element with a trailing
    /// `[ReinterpretAs VolatileObject; Field Value]` view.
    let private tryWriteArrayElementPrecise
        (state : IlMachineState)
        (arr : ManagedHeapAddress)
        (index : int)
        (byteOffset : int)
        (newValue : CliType)
        : IlMachineState option
        =
        if byteOffset <> 0 then
            None
        else

        let arrObj = state.ManagedHeap.Arrays.[arr]

        if index < 0 || index >= arrObj.Length then
            None
        else

        let existing = arrObj.Elements.[index]

        if CliType.sizeOf existing <> CliType.sizeOf newValue then
            None
        else if not (sameCliConstructor existing newValue) then
            None
        else

        match CliType.ByteAddressability existing with
        | CliByteAddressability.ByteAddressable -> None
        | CliByteAddressability.Rejected _ -> IlMachineThreadState.setArrayValue arr newValue index state |> Some

    let private writeManagedByrefBytesOrTypedCellCore
        (baseClassTypes : BaseClassTypes<DumpedAssembly> option)
        (state : IlMachineState)
        (src : ManagedPointerSource)
        (newValue : CliType)
        : IlMachineState
        =
        // Fast path: a bare `StackMemoryByte` byref whose destination range
        // matches the layout of an existing cell (or covers no existing
        // cell) is semantically a typed-cell store, not a byte scatter.
        // Routing it through `writeRootValue` preserves the provenance of
        // `newValue` (e.g. `NativeIntSource.FieldHandlePtr` from a
        // stackalloc + stind through a NativeInt-wrapped pointer; see
        // NullaryIlOp.fs's `stind` dispatcher and `Localloc`, which pushes
        // `EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ...)`).
        // The same fast path also accepts a trailing byte view
        // (`[ReinterpretAs ty]` / `[ReinterpretAs ty; ByteOffset n]`,
        // or a chained reinterpret followed by `Field`/`ByteOffset`
        // segments that `peelTrailingByteView` collapses into a single
        // byte offset) when the value being written cannot be flattened
        // to bytes; otherwise byte-view writes must continue to land in
        // the `Bytes` overlay so that partial-cell semantics (`stind.i1`
        // updating one byte of a wider cell, byte-by-byte initialisation
        // of a stackalloc buffer) are preserved. The Span<IntPtr> pinning
        // path that feeds RuntimeTypeHandle.GetFields produces a byte-view
        // shape over localloc memory and writes `FieldHandlePtr`-tagged
        // native ints through it; those are not byte-addressable, so the
        // typed-cell path is the only one that can preserve them.
        // We restrict the fast path to writes that are observably equivalent
        // to byte scatter: the new value must replace at most one existing
        // cell that starts exactly at `byteOffset` and has the same size as
        // the new value, and no other cell may intersect the destination
        // range. Otherwise we fall through to byte scatter, which preserves
        // partial-cell semantics (`stind.i1` updating one byte of an
        // existing `Int32`) and correctly throws on unmodelled byte views
        // of non-byte-addressable cells (e.g. tagged-pointer cells).
        let stackMemoryByteTarget =
            match src with
            | ManagedPointerSource.Byref (ByrefRoot.StackMemoryByte (thread, frame, block, byteOffset), []) ->
                ValueSome (thread, frame, block, byteOffset)
            | ManagedPointerSource.Byref (ByrefRoot.StackMemoryByte (thread, frame, block, rootByteOffset), projs) ->
                // Iterative peel mirrors the read side: a chained
                // byte-view (e.g. `[ReinterpretAs S, Field f, ReinterpretAs
                // T]`) reduces to a single byte offset over the
                // StackMemoryByte root, so the typed-cell fast path can
                // preserve the provenance of `newValue` rather than
                // hitting byte scatter (which would reject the
                // non-byte-addressable payload).
                match peelTrailingByteView baseClassTypes state projs with
                | ValueSome ([], viewByteOffset) ->
                    let byteOffset = rootByteOffset + viewByteOffset

                    match CliType.ByteAddressability newValue with
                    | CliByteAddressability.Rejected _ -> ValueSome (thread, frame, block, byteOffset)
                    | CliByteAddressability.ByteAddressable ->
                        // Byte-addressable byte-view writes normally follow the
                        // byte-scatter path below, to preserve the `Bytes`
                        // overlay representation that gives `stind.i1` its
                        // partial-cell semantics. The exception is a
                        // destination that byte scatter cannot represent at
                        // all: an exact-width cell carrying provenance, which
                        // this write wholly replaces.
                        let pool = IlMachineThreadState.getStackMemoryPool thread frame state

                        if
                            replacesWholeNonByteAddressableCell
                                (StackMemoryPool.tryFindCellCovering block byteOffset pool)
                                byteOffset
                                newValue
                        then
                            ValueSome (thread, frame, block, byteOffset)
                        else
                            ValueNone
                | ValueSome _
                | ValueNone -> ValueNone
            | _ -> ValueNone

        match stackMemoryByteTarget with
        | ValueSome (thread, frame, block, byteOffset) ->
            let pool = IlMachineThreadState.getStackMemoryPool thread frame state
            let typedWriteSafe = stackMemoryByteTypedWriteSafe pool block byteOffset newValue

            if typedWriteSafe then
                writeRootValue state (ByrefRoot.StackMemoryByte (thread, frame, block, byteOffset)) newValue
            else
                let bytes = CliType.ToBytes newValue
                writeStackMemoryBytesAt state thread frame block byteOffset bytes
        | ValueNone ->

        // Same fast path for native-heap blocks. The pool is global on state, so we
        // only need (block, byteOffset) here. The iterative-peel mirror of the
        // StackMemoryByte case above: chained byte-view shapes
        // (`[ReinterpretAs S, Field f, ReinterpretAs T]` and similar) reduce to a
        // single byte offset over the NativeMemoryByte root, allowing the typed-cell
        // fast path to preserve `newValue`'s provenance.
        let nativeMemoryByteTarget =
            match src with
            | ManagedPointerSource.Byref (ByrefRoot.NativeMemoryByte (block, byteOffset), []) ->
                ValueSome (block, byteOffset)
            | ManagedPointerSource.Byref (ByrefRoot.NativeMemoryByte (block, rootByteOffset), projs) ->
                match peelTrailingByteView baseClassTypes state projs with
                | ValueSome ([], viewByteOffset) ->
                    let byteOffset = rootByteOffset + viewByteOffset

                    match CliType.ByteAddressability newValue with
                    | CliByteAddressability.Rejected _ -> ValueSome (block, byteOffset)
                    | CliByteAddressability.ByteAddressable ->
                        // Exactly the StackMemoryByte reasoning above: byte
                        // scatter is the norm, except into an exact-width
                        // provenance-bearing cell it cannot represent.
                        if
                            replacesWholeNonByteAddressableCell
                                (NativeMemoryPool.tryFindCellCovering block byteOffset state.Kernel.NativeMemoryPool)
                                byteOffset
                                newValue
                        then
                            ValueSome (block, byteOffset)
                        else
                            ValueNone
                | ValueSome _
                | ValueNone -> ValueNone
            | _ -> ValueNone

        match nativeMemoryByteTarget with
        | ValueSome (block, byteOffset) ->
            let pool = state.Kernel.NativeMemoryPool
            let typedWriteSafe = nativeMemoryByteTypedWriteSafe pool block byteOffset newValue

            if typedWriteSafe then
                writeRootValue state (ByrefRoot.NativeMemoryByte (block, byteOffset)) newValue
            else
                let bytes = CliType.ToBytes newValue
                writeNativeMemoryBytesAt state block byteOffset bytes
        | ValueNone ->

        // Field-precise byte-view write into a heap object: when the destination is a
        // typed instance field of matching size and shape, route the write through the
        // field cell rather than the byte-scatter path. This preserves identity for
        // object-reference and runtime-pointer fields, whose `CliType.ToBytes` is not
        // defined and which `writeHeapValueBytes` would refuse via byte addressability.
        // Mirrors `tryReadHeapValueFieldPrecise` on the read path; uses the iterative
        // peel for symmetry with the read-side classifier so a chained byte-view that
        // lands on a precise field (e.g. `Volatile.Write` lowering of an object-typed
        // field through an `Unsafe.As` view) still routes to the typed write.
        //
        // For non-byte-renderable values, `writeManagedByrefCore` routes chains whose
        // peel result has a non-empty structural prefix through the structural
        // projection writer (`writeProjectedValueIfChanged`), so this dispatcher
        // typically only needs to handle the empty-prefix case. Direct callers of
        // `writeManagedByrefBytesOrTypedCell` (e.g. property tests) may still pass a
        // non-empty prefix for byte-renderable values; in that case the typed-cell
        // fast path declines and the byte-scatter fallback below handles the chain.
        let heapFieldPreciseWrite =
            match src with
            | ManagedPointerSource.Byref (ByrefRoot.HeapValue addr, []) ->
                tryWriteHeapValueFieldPrecise state addr 0 newValue
            | ManagedPointerSource.Byref (ByrefRoot.HeapValue addr, projs) ->
                match peelTrailingByteView baseClassTypes state projs with
                | ValueSome ([], byteOffset) -> tryWriteHeapValueFieldPrecise state addr byteOffset newValue
                | ValueSome (_ :: _, _)
                | ValueNone -> None
            | ManagedPointerSource.Byref (ByrefRoot.HeapObjectField (addr, field), projs) ->
                match peelTrailingByteView baseClassTypes state projs with
                | ValueSome ([], byteOffset) ->
                    // The named field's offset is the starting point inside the heap
                    // object; the peel result lands at `byteOffset` beyond that. For an
                    // exact whole-field overlap (`byteOffset = 0` matches the typed-cell
                    // matcher's same-size predicate), the typed write preserves
                    // identity for object-reference and runtime-pointer cells.
                    let obj = ManagedHeap.get addr state.ManagedHeap
                    let fieldOffset, _ = CliValueType.GetFieldLayoutById field obj.Contents
                    tryWriteHeapValueFieldPrecise state addr (fieldOffset + byteOffset) newValue
                | ValueSome (_ :: _, _)
                | ValueNone -> None
            | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, index), []) ->
                tryWriteArrayElementPrecise state arr index 0 newValue
            | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, index), projs) ->
                match peelTrailingByteView baseClassTypes state projs with
                | ValueSome ([], byteOffset) ->
                    // `Volatile.Write(ref array[i], obj)` lowers to a byref over the
                    // array element with a trailing `[ReinterpretAs VolatileObject;
                    // Field Value]` view, which peels to `([], 0)`. Route to the
                    // element-precise typed-cell write so that the destination
                    // ObjectRef cell is preserved rather than scattered through the
                    // byte-write path (which refuses non-byte-addressable cells).
                    tryWriteArrayElementPrecise state arr index byteOffset newValue
                | ValueSome (_ :: _, _)
                | ValueNone -> None
            | _ -> None

        match heapFieldPreciseWrite with
        | Some updatedState -> updatedState
        | None ->

        // Typed-cell fast path for byte-view-anchored ArrayElement byrefs.
        // The Conv_U/Conv_I anchor wraps a plain `ArrayElement` byref in a
        // trailing `[ReinterpretAs T; ByteOffset 0]` so subsequent pointer
        // arithmetic uses byte stride; for whole-cell-aligned `stobj` of a
        // non-byte-renderable value (e.g. `*p = new HandleHolder { P = ... }`
        // where the struct holds a `TypeHandlePtr`), the byte-scatter path
        // below would fail at `CliType.ToBytes`. Route such writes through
        // `setArrayValue` so the cell preserves the new value's provenance.
        //
        // Shape acceptance uses the shared `cellShapeMatches` (see its
        // docstring for why declared-handle equality is the right rule for
        // user structs). Use `CliType.sizeOf` (not `byteAddressableCellSize`)
        // for stride derivation because element 0 itself may already carry
        // non-byte-renderable provenance from a prior typed store.
        let arrayElementTypedCellWrite =
            match src with
            | ManagedPointerSource.Byref (ByrefRoot.ArrayElement _, _) ->
                match splitTrailingByteView src with
                | ValueSome (ByrefRoot.ArrayElement (arr, index), [], byteOffset) ->
                    let arrObj = state.ManagedHeap.Arrays.[arr]

                    if arrObj.Length = 0 then
                        None
                    else
                        let cellSize = CliType.sizeOf arrObj.Elements.[0]
                        let cellAdvance, inCellStart = floorDivRem byteOffset cellSize
                        let newSize = CliType.sizeOf newValue

                        if
                            inCellStart = 0
                            && newSize = cellSize
                            && cellShapeMatches arrObj.Elements.[0] newValue
                        then
                            let targetCell = index + cellAdvance

                            if targetCell < 0 || targetCell >= arrObj.Length then
                                None
                            else
                                Some (IlMachineThreadState.setArrayValue arr newValue targetCell state)
                        else
                            None
                | _ -> None
            | _ -> None

        match arrayElementTypedCellWrite with
        | Some updatedState -> updatedState
        | None ->

        let bytes = CliType.ToBytes newValue

        match src with
        | ManagedPointerSource.Null -> failwith "TODO: throw NullReferenceException"
        | ManagedPointerSource.NativeIntPlaceholder bits ->
            failwith
                $"writeManagedByref: cannot write through fake non-null byref @ 0x%x{bits}; the placeholder must never be dereferenced"
        | ManagedPointerSource.Byref (ByrefRoot.HeapValue addr, []) -> writeHeapValueBytes state addr 0 bytes
        | ManagedPointerSource.Byref (ByrefRoot.StackMemoryByte _, []) ->
            // Already handled by the StackMemoryByte typed-cell fast path above.
            failwith "unreachable: bare StackMemoryByte byref dispatched in fast path"
        | ManagedPointerSource.Byref (ByrefRoot.NativeMemoryByte _, []) ->
            // Already handled by the NativeMemoryByte typed-cell fast path above.
            failwith "unreachable: bare NativeMemoryByte byref dispatched in fast path"
        | ManagedPointerSource.Byref (ByrefRoot.PeByteRange peByteRange, _) ->
            failwith
                $"PE byte range is read-only; refusing byte-view write of %d{bytes.Length} bytes through %O{peByteRange}"
        | ManagedPointerSource.Byref (ByrefRoot.StringCharAt (str, charIndex), []) ->
            writeStringBytes state str charIndex 0 bytes
        | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, index), []) ->
            writeArrayBytes state arr index 0 bytes
        | ManagedPointerSource.Byref (outerRoot, outerProjs) ->
            // Collapse any trailing byte-view segment of the projection
            // chain into an accumulated byte offset. Mirrors
            // `readManagedByrefBytesAs`: once a `ReinterpretAs` appears
            // the underlying storage is being treated as raw bytes, so
            // subsequent `ByteOffset`, `Field` (resolved against the most
            // recent `ReinterpretAs` target), and chained `ReinterpretAs`
            // projections are all bytewise. Peeling them exposes the
            // residual structural prefix to the existing dispatchers.
            let byteViewShape : (ByrefProjection list * int) voption =
                peelTrailingByteView baseClassTypes state outerProjs

            match byteViewShape with
            | ValueSome (prefixProjs, byteOffset) ->
                // Storage with no byte image cannot be written bytewise at all: not by the
                // specialised root writers below, and not by lifting outward in `resolveCell`,
                // which has no byte-addressable cell to lift to. Descend instead and name the cell
                // the range picks out, mirroring `readManagedByrefBytesAs`. This is the route for
                // `buffer[k].Tag = v` over an `[InlineArray]` whose element holds a reference: the
                // value written is byte-renderable, so the write arrives here rather than at the
                // structural writer, but the *storage* is not.
                //
                // Probed only for roots whose typed read is total. `readRootValue` throws for
                // `PeByteRange`, and for the raw byte pools when no typed cell starts at the
                // offset; those roots are byte storage by construction and can never hold a
                // reference anyway. `tryNameCellForByrefAccess` yields `None` for byte-addressable
                // storage, so nothing that reaches the writers below today is diverted.
                //
                // This does read the root value for `ArrayElement` and `HeapValue`, which
                // previously went straight to a specialised byte writer. Both reads are total:
                // those roots are only ever built by `ldelema` and by boxing, which validate the
                // element and the payload respectively.
                let namedWrite : IlMachineState voption =
                    match outerRoot with
                    | ByrefRoot.PeByteRange _
                    | ByrefRoot.StackMemoryByte _
                    | ByrefRoot.NativeMemoryByte _
                    | ByrefRoot.StringCharAt _ -> ValueNone
                    | _ ->

                    let rootValue = readRootValue state outerRoot
                    let cellHere = readProjectedValue rootValue prefixProjs

                    match tryNameCellForByrefAccess byteOffset cellHere newValue with
                    | None -> ValueNone
                    | Some path ->
                        if isProvableNoOpWrite (CliType.getCellAtPath path cellHere) newValue then
                            ValueSome state
                        else

                        let updatedCell = CliType.withCellAtPathSet path newValue cellHere

                        match applyProjectionsForWriteIfChanged rootValue prefixProjs updatedCell with
                        | None -> ValueSome state
                        | Some updatedRoot -> ValueSome (writeRootValue state outerRoot updatedRoot)

                match namedWrite with
                | ValueSome state -> state
                | ValueNone ->

                match outerRoot, prefixProjs with
                | ByrefRoot.StackMemoryByte (thread, frame, block, rootByteOffset), [] ->
                    // Byte-addressable byte-view writes through a localloc buffer
                    // intentionally fall through here (the typed-cell fast path
                    // above declines them so that the `Bytes` overlay
                    // representation is preserved for `stind.i1`-style partial
                    // updates).
                    writeStackMemoryBytesAt state thread frame block (rootByteOffset + byteOffset) bytes
                | ByrefRoot.NativeMemoryByte (block, rootByteOffset), [] ->
                    // Same reasoning as the StackMemoryByte case above, but
                    // routed through the global NativeMemoryPool.
                    writeNativeMemoryBytesAt state block (rootByteOffset + byteOffset) bytes
                | ByrefRoot.ArrayElement (arr, index), [] -> writeArrayBytes state arr index byteOffset bytes
                | ByrefRoot.StringCharAt (str, charIndex), [] -> writeStringBytes state str charIndex byteOffset bytes
                | ByrefRoot.HeapValue addr, [] -> writeHeapValueBytes state addr byteOffset bytes
                | _, prefixProjs ->
                    let rootValue = readRootValue state outerRoot

                    // Storage with no byte image cannot be written by lifting outward either:
                    // there is no byte-addressable cell to lift to. Descend instead and name the
                    // cell the range picks out, mirroring `readManagedByrefBytesAs`. This is the
                    // route for `buffer[k].Tag = v` over an `[InlineArray]` whose element holds a
                    // reference: the value written is byte-renderable, so the write arrives here
                    // rather than at the structural writer, but the *storage* is not.
                    // `tryNameCellForByrefAccess` yields `None` for byte-addressable storage, so
                    // nothing that reaches `resolveCell` today is diverted.
                    let namedWrite =
                        let cellHere = readProjectedValue rootValue prefixProjs

                        match tryNameCellForByrefAccess byteOffset cellHere newValue with
                        | None -> ValueNone
                        | Some path ->
                            let updatedCell = CliType.withCellAtPathSet path newValue cellHere

                            if updatedCell = cellHere then
                                ValueSome state
                            else
                                match applyProjectionsForWriteIfChanged rootValue prefixProjs updatedCell with
                                | None -> ValueSome state
                                | Some updatedRoot -> ValueSome (writeRootValue state outerRoot updatedRoot)

                    match namedWrite with
                    | ValueSome state -> state
                    | ValueNone ->

                    // Symmetric to the read path: when the byte write overflows
                    // the immediate cell, lift back through trailing `Field`
                    // projections so a write through e.g. `Unsafe.Add(ref s.A, 1)`
                    // updates the parent struct's sibling field.
                    let rec resolveCell
                        (projs : ByrefProjection list)
                        (offset : int)
                        : ByrefProjection list * int * CliType
                        =
                        let cell = readProjectedValue rootValue projs
                        let cellSize = byteAddressableCellSize $"single-cell byref %O{src}" cell

                        if offset >= 0 && bytes.Length <= cellSize - offset then
                            projs, offset, cell
                        else
                            match List.tryLast projs with
                            | Some (ByrefProjection.Field field) ->
                                let parentProjs = projs |> List.take (List.length projs - 1)
                                let parentValue = readProjectedValue rootValue parentProjs
                                let fieldOffset, _ = CliType.getFieldLayoutById field parentValue
                                resolveCell parentProjs (offset + fieldOffset)
                            | _ ->
                                failwith
                                    $"TODO: byte-view write at offset %d{offset} for %d{bytes.Length} bytes does not fit in single primitive cell of size %d{cellSize}: %O{src}"

                    let liftedProjs, finalOffset, cell = resolveCell prefixProjs byteOffset

                    match
                        withByteAddressableCellBytesAtIfChanged $"single-cell byref %O{src}" finalOffset bytes cell
                    with
                    | None -> state
                    | Some updatedCell ->
                        match applyProjectionsForWriteIfChanged rootValue liftedProjs updatedCell with
                        | None -> state
                        | Some updatedRoot -> writeRootValue state outerRoot updatedRoot
            | ValueNone ->
                let rootValue = readRootValue state outerRoot
                let cell = readProjectedValue rootValue outerProjs
                let cellSize = byteAddressableCellSize $"plain byref %O{src}" cell

                if bytes.Length > cellSize then
                    failwith
                        $"TODO: byte-view write of %d{bytes.Length} bytes does not fit in plain primitive cell of size %d{cellSize}: %O{src}"

                match withByteAddressableCellBytesAtIfChanged $"plain byref %O{src}" 0 bytes cell with
                | None -> state
                | Some updatedCell ->
                    match applyProjectionsForWriteIfChanged rootValue outerProjs updatedCell with
                    | None -> state
                    | Some updatedRoot -> writeRootValue state outerRoot updatedRoot

    /// Public BCT-aware entry point. Delegates to `writeManagedByrefBytesOrTypedCellCore`
    /// with `Some baseClassTypes`; the core body lazily consults the BCT only when a
    /// `Field` projection appears in the byte-view suffix, so the metadata-light shapes
    /// `[ReinterpretAs T]` and `[ReinterpretAs T; ByteOffset n]` flow through
    /// `writeManagedByrefCore` with `None` without forcing BCT lookups.
    let writeManagedByrefBytesOrTypedCell
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (src : ManagedPointerSource)
        (newValue : CliType)
        : IlMachineState
        =
        writeManagedByrefBytesOrTypedCellCore (Some baseClassTypes) state src newValue

    let private splitFirstReinterpret
        (projs : ByrefProjection list)
        : (ByrefProjection list * ConcreteType<ConcreteTypeHandle> * ByrefProjection list) option
        =
        let rec loop (revPrefix : ByrefProjection list) (remaining : ByrefProjection list) =
            match remaining with
            | [] -> None
            | ByrefProjection.ReinterpretAs ty :: rest -> Some (List.rev revPrefix, ty, rest)
            | proj :: rest -> loop (proj :: revPrefix) rest

        loop [] projs

    let private describeCliStorage (state : IlMachineState) (value : CliType) : string =
        CliType.DescribeByteLayout (Some state.ConcreteTypes) value

    let private reinterpretStorageBytes
        (state : IlMachineState)
        (operation : string)
        (storageValue : CliType)
        : byte[]
        =
        match CliType.ByteAddressability storageValue with
        | CliByteAddressability.ByteAddressable -> CliType.ToBytes storageValue
        | CliByteAddressability.Rejected rejection ->
            failwith
                $"TODO: %s{operation}: write through `ReinterpretAs` over byte-unaddressable storage (%s{rejection.Description}) is not modelled; storage layout:\n%s{describeCliStorage state storageValue}"

    let private ofBytesLikeForReinterpret
        (state : IlMachineState)
        (operation : string)
        (storageValue : CliType)
        (bytes : byte[])
        : CliType
        =
        try
            CliType.ofBytesLike storageValue bytes
        with ex ->
            failwith
                $"%s{operation}: failed to reconstruct storage from reinterpreted bytes. Reinterpret writes into unrepresented padding are not modelled. Storage layout:\n%s{describeCliStorage state storageValue}\nInner error: %s{ex.Message}"

    let private splitTrailingPrefixByteOffset (projs : ByrefProjection list) : ByrefProjection list * int =
        match List.rev projs with
        | ByrefProjection.ByteOffset n :: revPrefix -> List.rev revPrefix, n
        | _ -> projs, 0

    let rec private writeProjectedValueIfChanged
        (baseClassTypes : BaseClassTypes<DumpedAssembly> option)
        (state : IlMachineState)
        (rootValue : CliType)
        (projs : ByrefProjection list)
        (newValue : CliType)
        : CliType option
        =
        match baseClassTypes, splitFirstReinterpret projs with
        | Some baseClassTypes, Some (prefixProjs, reinterpretTy, reinterpretProjs) ->
            let storageProjs, byteOffset = splitTrailingPrefixByteOffset prefixProjs
            let storageValue = readProjectedValue rootValue storageProjs

            match
                writeReinterpretedStorageIfChanged
                    baseClassTypes
                    state
                    storageValue
                    byteOffset
                    reinterpretTy
                    reinterpretProjs
                    newValue
            with
            | None -> None
            | Some updatedStorage -> applyProjectionsForWriteIfChanged rootValue storageProjs updatedStorage
        | _ -> applyProjectionsForWriteIfChanged rootValue projs newValue

    and private writeReinterpretedStorageIfChanged
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (storageValue : CliType)
        (byteOffset : int)
        (reinterpretTy : ConcreteType<ConcreteTypeHandle>)
        (reinterpretProjs : ByrefProjection list)
        (newValue : CliType)
        : CliType option
        =
        // Reinterpret writes are byte updates to the original storage shape. This covers patterns
        // such as `Unsafe.As<bool, VolatileBoolean>(ref location).Value = value`, and recurses for
        // nested `Unsafe.As` chains before rebuilding the original cell.
        let operation =
            $"write through `ReinterpretAs` as %s{reinterpretTy.Namespace}.%s{reinterpretTy.Name}"

        // Transparent single-field wrapper write fast path. CoreLib lowers
        // `Volatile.Write<T>(ref T, T) where T : class?` to
        // `Unsafe.As<T, VolatileObject>(ref location).Value = value`, which
        // produces a byref over `CliType.ObjectRef` storage with a trailing
        // `ReinterpretAs VolatileObject` and a `Field "Value"` projection.
        // Bytewise reinterpret over an `ObjectRef` is meaningless because
        // ObjectRef storage is not byte-addressable; the classifier reuses the
        // same predicate as `readReinterpretedByrefField` to decide whether
        // this is a transparent wrapper access we can pass through. A bare
        // reference is not the only storage with no byte image — a value type
        // holding references has none either — so each leg below accepts
        // exactly what the classifier's `isCellIdentityCompatible` accepts,
        // rather than object references alone. Anything narrower would refuse
        // writes the classifier has already declared elidable.
        // Install `newValue` into the cell at `path`, the byref having been shown to name exactly
        // that cell. The cell was chosen because it is identity-compatible with what the byref
        // reinterprets it as, so the value being stored must be too, or the cell would end up
        // holding a different kind of thing than it claims to.
        let writeIntoNamedCell (path : FieldId list) (describeCell : string) : CliType option voption =
            let current = CliType.getCellAtPath path storageValue

            if isCellIdentityCompatible newValue current then
                if isProvableNoOpWrite current newValue then
                    ValueSome None
                else
                    ValueSome (Some (CliType.withCellAtPathSet path newValue storageValue))
            else
                failwith
                    $"%s{operation}: assigning %s{describeCliStorage state newValue}, which is not the same kind of value as the %s{describeCliStorage state current} held by %s{describeCell}"

        // The write mirror of the naming step in `readReinterpretedByrefField`: a trailing `Field`
        // lands `fieldOffset` bytes into the reinterpret target, which itself sits `offset`
        // bytes into the storage, so the byref names whatever cell occupies that sum.
        // `buffer[0].Payload = box` is exactly this shape, and the classifier cannot serve it —
        // `Elem` is not a transparent single-field wrapper of anything.
        let tryNameThroughFieldAt (offset : int) (field : FieldId) : CliType option voption =
            match tryNameCellThroughReinterpretField baseClassTypes state offset storageValue reinterpretTy field with
            | None -> ValueNone
            | Some path -> writeIntoNamedCell path $"the cell %O{path} named by field %O{field} of %O{reinterpretTy}"

        let transparentWrapperFastPath () : CliType option voption =
            match reinterpretProjs with
            // A trailing `Field` may be preceded by a `ByteOffset`: `buffer[k].Payload = box`
            // walks the reinterpreted view on by whole elements before selecting the field, giving
            // `[ByteOffset k*sizeof(Elem); Field Payload]`. The two cursors add, exactly as in the
            // no-field arm below — `byteOffset` is the prefix cursor into `storageValue` taken
            // before the reinterpret, and the trailing `ByteOffset` moves the reinterpreted view
            // on from there.
            //
            // Only a *non-byte-renderable* value arrives here at all: `writeManagedByrefCore`
            // routes on `CliType.ByteAddressability newValue`, so the sibling
            // `buffer[k].Tag = someByte` goes to the bytes-or-typed-cell writer instead and is
            // served by its own cell naming. Both shapes need the `ByteOffset`-then-`Field` chain
            // to fold to an offset in the first place, which is why they were blocked together.
            | [ ByrefProjection.Field field ]
            | [ ByrefProjection.ByteOffset _ ; ByrefProjection.Field field ] ->
                let trailingOffset =
                    match reinterpretProjs with
                    | [ ByrefProjection.ByteOffset n ; _ ] -> n
                    | _ -> 0

                let totalOffset = byteOffset + trailingOffset

                // The transparent-wrapper classifier asks whether the wrapper's single field spans
                // the storage exactly, which is only a meaningful question at offset zero. At any
                // other offset the byref necessarily names an interior cell, so only the naming
                // route applies.
                if totalOffset <> 0 then
                    tryNameThroughFieldAt totalOffset field
                else

                match classifyTransparentWrapper baseClassTypes state storageValue reinterpretTy field with
                | TransparentWrapperOutcome.ElideAsField _ ->
                    // The classifier chose this outcome because the storage is identity-compatible
                    // with the wrapper's only field, which spans the wrapper; writing that field
                    // therefore replaces the storage outright. Hold the value being stored to the
                    // same standard, so the storage cannot end up holding a different kind of
                    // thing than it claims to.
                    match newValue with
                    | _ when isCellIdentityCompatible newValue storageValue ->
                        if isProvableNoOpWrite storageValue newValue then
                            ValueSome None
                        else
                            ValueSome (Some newValue)
                    | other ->
                        failwith
                            $"%s{operation}: assigning %s{describeCliStorage state other}, which is not the same kind of value as the %s{describeCliStorage state storageValue} it would replace, to field %O{field} of a single-instance-field wrapper"
                | TransparentWrapperOutcome.ElideAsStorageInnerField innerPath ->
                    writeIntoNamedCell innerPath $"inner cell %O{innerPath} of a nested single-instance-field wrapper"
                | TransparentWrapperOutcome.NotTransparent ->
                    // Not a wrapper the classifier recognises, but the byref may still name a cell
                    // outright — the same naming route the non-zero-offset case above takes.
                    tryNameThroughFieldAt 0 field
            // No trailing `Field`: the byref reinterprets the storage directly as some other
            // type, possibly walked forward by `Unsafe.Add`. When the byte range it picks out is
            // exactly one reference-typed cell of the storage, the write lands squarely on that
            // cell — the `[InlineArray(N)]`-over-a-reference-element write, at slot 0 for the
            // bare shape and at slot `n / sizeof(T)` for the `ByteOffset` one — mirroring the
            // read-side elision in `readManagedByref`.
            //
            // The two cursors add: `byteOffset` is the prefix cursor into `storageValue` taken
            // before the reinterpret, and a trailing `ByteOffset n` moves the reinterpreted view
            // on by a further `n` bytes. Both index the same storage, which is exactly what
            // `peelTrailingByteView`'s forward walk computes for the byte path.
            //
            // This is *not* the array-element zero-fill that `writeArrayBytes` handles via
            // `CliType.WithZeroedRangeIfChanged`: the value written here is an arbitrary
            // reference rather than a zero, so there is nothing to decompose — the field is
            // simply replaced.
            | []
            | [ ByrefProjection.ByteOffset _ ] ->
                let trailingOffset =
                    match reinterpretProjs with
                    | [ ByrefProjection.ByteOffset n ] -> n
                    | _ -> 0

                let targetTemplate = zeroForConcreteType baseClassTypes state reinterpretTy

                match tryNameCellForByrefAccess (byteOffset + trailingOffset) storageValue targetTemplate with
                | Some innerPath -> writeIntoNamedCell innerPath $"the storage cell %O{innerPath} that the byref names"
                | None -> ValueNone
            | _ -> ValueNone

        match transparentWrapperFastPath () with
        | ValueSome result -> result
        | ValueNone ->

        let storageBytes = reinterpretStorageBytes state operation storageValue
        let reinterpretZero = zeroForConcreteType baseClassTypes state reinterpretTy
        let reinterpretSize = CliType.sizeOf reinterpretZero

        if byteOffset < 0 || reinterpretSize > storageBytes.Length - byteOffset then
            failwith
                $"TODO: %s{operation} requires %d{reinterpretSize} bytes at offset %d{byteOffset}, but storage has %d{storageBytes.Length} bytes. Storage layout:\n%s{describeCliStorage state storageValue}"

        let reinterpretBytes = storageBytes.[byteOffset .. byteOffset + reinterpretSize - 1]

        let reinterpretTemplate =
            ofBytesLikeForReinterpret state operation reinterpretZero reinterpretBytes

        match
            writeProjectedValueIfChanged (Some baseClassTypes) state reinterpretTemplate reinterpretProjs newValue
        with
        | None -> None
        | Some updatedReinterpret ->
            let updatedBytes = CliType.ToBytes updatedReinterpret

            if updatedBytes.Length <> reinterpretSize then
                failwith
                    $"TODO: %s{operation} produced %d{updatedBytes.Length} bytes for reinterpret type %O{reinterpretTy}, expected %d{reinterpretSize}. Storage layout:\n%s{describeCliStorage state storageValue}"

            if bytesEqual updatedBytes reinterpretBytes then
                None
            else
                CliType.WithBytesAtIfChanged byteOffset updatedBytes storageValue

    let private writeManagedByrefCore
        (baseClassTypes : BaseClassTypes<DumpedAssembly> option)
        (state : IlMachineState)
        (src : ManagedPointerSource)
        (newValue : CliType)
        : IlMachineState
        =
        match src with
        | ManagedPointerSource.Null -> failwith "TODO: throw NullReferenceException"
        | ManagedPointerSource.NativeIntPlaceholder bits ->
            failwith
                $"writeManagedByrefCore: cannot write through fake non-null byref @ 0x%x{bits}; the placeholder must never be dereferenced"
        | ManagedPointerSource.Byref (root, []) -> writeRootValue state root newValue
        | ManagedPointerSource.Byref (root, projs) ->
            // Mirror the read-side dispatch: when we have BaseClassTypes,
            // use the iterative peel so chained byte views like
            // `[ReinterpretAs S, Field f, ReinterpretAs T, Field g]`
            // (e.g. `Volatile.Write` on a field of an `Unsafe.As`-projected
            // struct view) route to the bytes-or-typed-cell writer. Without
            // BCT, fall back to the trailing-suffix-only classifier.
            //
            // Dispatch the byte-view byref through one of two writers:
            //
            //  - `writeManagedByrefBytesOrTypedCell` handles byte-scatter writes
            //    (`stind.i1` updating one byte of a wider cell, partial cell
            //    writes via `Unsafe.Add`) and a few precise typed-cell fast
            //    paths for storage that supports byte-level addressability
            //    (StackMemoryByte, NativeMemoryByte, HeapValue, HeapObjectField,
            //    ArrayElement, StringCharAt). When it cannot precise-write, it falls back
            //    through `CliType.ToBytes newValue`, which refuses
            //    non-byte-renderable values (object references, runtime
            //    pointers).
            //  - `writeProjectedValueIfChanged` handles structural writes via
            //    `writeReinterpretedStorageIfChanged`, including the
            //    transparent-wrapper fast path that lets ref↔ref writes
            //    through a `VolatileObject<T>`-style wrapper succeed for any
            //    root (locals, arguments, statics included).
            //
            // The dispatcher routes:
            //  * byte-renderable values: to the bytes-or-typed-cell writer.
            //    For empty prefix the writer's typed-cell fast path can
            //    preserve identity; for non-empty prefix the byte-scatter
            //    fallback's `resolveCell` lifts back through trailing `Field`
            //    projections to handle `Unsafe.Add`-style cross-cell writes.
            //  * non-byte-renderable values: to the structural projection
            //    writer, which is the only one that can preserve identity for
            //    object references and tagged pointers over arbitrary roots.
            //
            // Both writers now accept an optional `BaseClassTypes`: the peel
            // and the bytes-or-typed-cell writer only consult BCT when a
            // `Field` projection appears in the byte-view suffix, which
            // metadata-light callers do not produce.
            let peeled : (ByrefProjection list * int) voption =
                peelTrailingByteView baseClassTypes state projs

            let valueIsByteRenderable =
                match CliType.ByteAddressability newValue with
                | CliByteAddressability.ByteAddressable -> true
                | CliByteAddressability.Rejected _ -> false

            // The forward-walk peel guarantees the structural prefix never
            // contains a `ReinterpretAs`, so the byte-scatter writer's
            // `resolveCell` (which navigates the prefix via
            // `readProjectedValue`) can safely take the byte-renderable path
            // without re-checking the prefix shape.
            let useStructuralWriter () : IlMachineState =
                let rootValue = readRootValue state root

                match writeProjectedValueIfChanged baseClassTypes state rootValue projs newValue with
                | None -> state
                | Some updatedRoot -> writeRootValue state root updatedRoot

            // Transparent-wrapper writes (Phase A, where the storage *is* the
            // wrapper's only field; and Phase B, where the storage is itself a
            // transparent offset-0 single-field wrapper of the same primitive,
            // e.g. CoreLib's `Unsafe.As<TaskAwaiter<T>, TaskAwaiter>` motif)
            // reach the bytes-or-typed-cell writer via the byte-addressable
            // roots (HeapValue/HeapObjectField/ArrayElement). Its precise-write
            // helpers reject a cross-constructor write (`ValueType` storage,
            // `ObjectRef` payload) and the byte-scatter fallback then hits
            // `CliType.ToBytes` on a live reference — which refuses. Re-route
            // both elidable outcomes to the structural writer, whose
            // `transparentWrapperFastPath` serves them from the same
            // classifier. Where the byte path also works (Phase A over a bare
            // `ObjectRef`) the two agree, so routing on the classifier's answer
            // rather than on which storage shapes the byte writer happens to
            // cope with keeps this decision in one place.
            //
            // `NotTransparent` must stay on the precise-write path: for e.g.
            // `Unsafe.As<object, StructWithMultipleFields>(ref h.Field).Obj = x`
            // only that path succeeds, because the structural writer would fall
            // through to `reinterpretStorageBytes` on byte-unaddressable
            // storage.
            //
            // The classifier requires a typed `ValueType` reinterpret target, so
            // raw byte roots (`StackMemoryByte`, `NativeMemoryByte`) can never
            // reach it; `readRootValue` would also throw for them when no typed
            // cell covers the root offset, so we must not probe those roots
            // here.
            //
            // The same argument applies unchanged to the no-`Field` shapes
            // `[ReinterpretAs T]` and `[ReinterpretAs T; ByteOffset n]` over
            // storage whose byte range `n` is exactly one reference cell — the
            // `[InlineArray(N)]`-over-a-reference-element write, at slot 0 and
            // at every later slot respectively. There the
            // storage is a `ValueType` wrapper and the payload an `ObjectRef`,
            // so the precise-write helpers reject the cross-constructor write
            // and the byte-scatter fallback again reaches `CliType.ToBytes` on
            // a live reference. Without this the fix would cover only locals,
            // whose roots fall to `useStructuralWriter` anyway, and a wrapper
            // reached by `ldflda` on a class field or `ldelema` on an array
            // element would still fail.
            let isTransparentWrapperStructuralWrite () : bool =
                match baseClassTypes, root, projs with
                | Some bct,
                  (ByrefRoot.HeapValue _ | ByrefRoot.HeapObjectField _ | ByrefRoot.ArrayElement _),
                  [ ByrefProjection.ReinterpretAs reinterpretTy ; ByrefProjection.Field field ] ->
                    let storageValue = readRootValue state root

                    match classifyTransparentWrapper bct state storageValue reinterpretTy field with
                    | TransparentWrapperOutcome.ElideAsField _
                    | TransparentWrapperOutcome.ElideAsStorageInnerField _ -> true
                    | TransparentWrapperOutcome.NotTransparent ->
                        // Not a wrapper, but the byref may still name a cell outright —
                        // `buffer[0].Payload = box`, where `Elem` wraps nothing. Only the
                        // structural writer reaches the naming step, so route on whether that
                        // step would find a cell, using the very function that will look.
                        tryNameCellThroughReinterpretField bct state 0 storageValue reinterpretTy field
                        |> Option.isSome
                | Some bct,
                  (ByrefRoot.HeapValue _ | ByrefRoot.HeapObjectField _ | ByrefRoot.ArrayElement _),
                  [ ByrefProjection.ReinterpretAs reinterpretTy ] ->
                    let storageValue = readRootValue state root
                    let targetTemplate = zeroForConcreteType bct state reinterpretTy

                    tryNameCellForByrefAccess 0 storageValue targetTemplate |> Option.isSome
                | Some bct,
                  (ByrefRoot.HeapValue _ | ByrefRoot.HeapObjectField _ | ByrefRoot.ArrayElement _),
                  [ ByrefProjection.ReinterpretAs reinterpretTy ; ByrefProjection.ByteOffset byteOffset ] ->
                    let storageValue = readRootValue state root
                    let targetTemplate = zeroForConcreteType bct state reinterpretTy

                    tryNameCellForByrefAccess byteOffset storageValue targetTemplate
                    |> Option.isSome
                | _ -> false

            match peeled, valueIsByteRenderable with
            | ValueSome (_, _), true ->
                // Byte-renderable values flow through the bytes-or-typed-cell
                // writer regardless of BCT availability. The metadata-light
                // shapes `[ReinterpretAs T]` and `[ReinterpretAs T; ByteOffset n]`
                // (used by primitive/external boundaries that don't carry type
                // metadata) work here because the core never consults BCT
                // unless a `Field` appears in the byte-view suffix.
                writeManagedByrefBytesOrTypedCellCore baseClassTypes state src newValue
            | ValueSome ([], _), false when isTransparentWrapperStructuralWrite () -> useStructuralWriter ()
            | ValueSome ([], _), false ->
                // Non-byte-renderable empty-prefix peel: the bytes-or-typed-cell
                // writer can precise-write for the byte-addressable storage
                // roots (StackMemoryByte, NativeMemoryByte, HeapValue,
                // HeapObjectField, ArrayElement, StringCharAt). For other roots
                // its fallback hits `CliType.ToBytes`; route those through the
                // structural path instead.
                match root with
                | ByrefRoot.StackMemoryByte _
                | ByrefRoot.NativeMemoryByte _
                | ByrefRoot.HeapValue _
                | ByrefRoot.HeapObjectField _
                | ByrefRoot.ArrayElement _
                | ByrefRoot.StringCharAt _ -> writeManagedByrefBytesOrTypedCellCore baseClassTypes state src newValue
                | _ -> useStructuralWriter ()
            | _ -> useStructuralWriter ()

    let writeManagedByref (state : IlMachineState) (src : ManagedPointerSource) (newValue : CliType) : IlMachineState =
        // Call sites that can supply BaseClassTypes should use writeManagedByrefWithBase so
        // non-trailing ReinterpretAs projections can be applied bytewise. This metadata-light entry point
        // remains for primitive/external boundaries that do not currently carry type metadata.
        writeManagedByrefCore None state src newValue

    let writeManagedByrefWithBase
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (src : ManagedPointerSource)
        (newValue : CliType)
        : IlMachineState
        =
        writeManagedByrefCore (Some baseClassTypes) state src newValue

    let private isNumericProvenanceRejection (rejection : CliByteAddressabilityRejection) : bool =
        match rejection with
        | CliByteAddressabilityRejection.NativeIntSourceNotByteAddressable _
        | CliByteAddressabilityRejection.Int64SourceNotByteAddressable _ -> true
        | CliByteAddressabilityRejection.ObjectReference
        | CliByteAddressabilityRejection.RuntimePointer
        | CliByteAddressabilityRejection.ValueTypeContainsObjectReferences _
        | CliByteAddressabilityRejection.ValueTypeContainsRuntimePointers _
        | CliByteAddressabilityRejection.ValueTypeContainsNonByteAddressableField _ -> false

    /// Whether a same-width primitive store must be serviced by replacing the *whole
    /// destination cell*, because that cell has no byte image for a byte scatter to write
    /// into.
    ///
    /// This is deliberately broader than `isNumericProvenanceRejection`, which asks a
    /// different question about the *payload*: "does this value carry provenance we must not
    /// flatten". A pointer-typed slot (`void*`, `T*`, `delegate*<...>`) holds a
    /// `CliType.RuntimePointer` whose zero is `Managed Null`, and every `stind.i` into one —
    /// including a store of plain zero — reaches the byte writer and is refused there. The
    /// canonical guest shape is a `ref`/`out` parameter bound to a pointer-typed field, which
    /// is how CoreLib's `RuntimeTypeHandle.GetActivationInfo` shim assigns its results into
    /// `RuntimeType.ActivatorCache`'s fields.
    ///
    /// When the payload is the same width as the cell — which the caller checks — a byte
    /// scatter and a whole-cell replacement address exactly the same range, so replacing the
    /// cell is exact rather than a guess.
    ///
    /// The pointer-cell arm additionally consults the payload, which the two
    /// numeric-provenance arms have no need to. Whole-cell replacement restamps the cell with
    /// the payload's shape, and only a pointer-shaped payload leaves a pointer slot still
    /// holding a pointer. On a 64-bit runtime `stind.i8`/`stind.r8` through a `long*`/`double*`
    /// alias of the same slot are *also* exact-width stores, so width alone cannot tell them
    /// apart; restamping for those would turn the cell into `Numeric Int64`/`Float64` and the
    /// next read of the field would push the wrong evaluation-stack kind, failing somewhere
    /// downstream with a message that names neither the field nor the store. Those keep the
    /// pre-existing refusal at the store, where the cause is visible.
    /// `sourcesPure/PointerFieldAliasedWidthStore.cs` is the parked guest for that gap.
    ///
    /// Object references stay excluded: replacing an objref cell with a native int is a type
    /// change rather than a representation change, and `stind.ref` owns that path. Value-type
    /// cells stay excluded for the same reason — a same-width primitive store is not a
    /// whole-struct replacement.
    let private destinationNeedsWholeCellStore
        (newValue : CliType)
        (rejection : CliByteAddressabilityRejection)
        : bool
        =
        match rejection with
        | CliByteAddressabilityRejection.NativeIntSourceNotByteAddressable _
        | CliByteAddressabilityRejection.Int64SourceNotByteAddressable _ -> true
        | CliByteAddressabilityRejection.RuntimePointer ->
            match CliType.unwrapPrimitiveLike newValue with
            | CliType.RuntimePointer _
            | CliType.Numeric (CliNumericType.NativeInt _) -> true
            | _ -> false
        | CliByteAddressabilityRejection.ObjectReference
        | CliByteAddressabilityRejection.ValueTypeContainsObjectReferences _
        | CliByteAddressabilityRejection.ValueTypeContainsRuntimePointers _
        | CliByteAddressabilityRejection.ValueTypeContainsNonByteAddressableField _ -> false

    let private byteAddressabilityRejection (value : CliType) : CliByteAddressabilityRejection option =
        match CliType.ByteAddressability value with
        | CliByteAddressability.ByteAddressable -> None
        | CliByteAddressability.Rejected rejection -> Some rejection

    let private writeExactWidthPrimitiveTypedStore
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (src : ManagedPointerSource)
        (newValue : CliType)
        (reason : string)
        (knownExisting : CliType option)
        : IlMachineState
        =
        match src with
        | ManagedPointerSource.Null -> failwith "TODO: throw NullReferenceException"
        | ManagedPointerSource.NativeIntPlaceholder bits ->
            failwith
                $"writeExactWidthPrimitiveTypedStore: cannot write through fake non-null byref @ 0x%x{bits}; the placeholder must never be dereferenced"
        | ManagedPointerSource.Byref (ByrefRoot.StackMemoryByte _, _) ->
            failwith "unreachable: StackMemoryByte primitive stores are dispatched before exact-width typed store"
        | ManagedPointerSource.Byref (ByrefRoot.NativeMemoryByte _, _) ->
            failwith "unreachable: NativeMemoryByte primitive stores are dispatched before exact-width typed store"
        | ManagedPointerSource.Byref _ ->
            match splitTrailingByteView src with
            | ValueSome (ByrefRoot.ArrayElement (arr, index), [], byteOffset) ->
                // Byte-view-anchored array byref (set up by Conv_U/Conv_I on a
                // plain ArrayElement so that subsequent native-pointer
                // arithmetic uses byte stride). For a whole-cell-aligned
                // single-cell write of matching width *and shape*, the byte
                // view is a no-op label on top of a typed-cell store; route
                // through `setArrayValue` so the cell preserves the new
                // value's provenance (e.g. `TypeHandlePtr` from
                // `typeof(int).TypeHandle.Value` into `IntPtr[]`).
                //
                // The shape comparator is `haveSameCliShape` (wrapper-peeling)
                // rather than the stricter `sameCliConstructor`: array cells
                // for primitive-like wrapped types (such as `IntPtr`) are not
                // shape-contracted — both the bare `Numeric NativeInt` form
                // and the wrapped `ValueType { ... }` form are legitimate
                // storage shapes for the same logical element type, and
                // existing code on the read path uses `unwrapPrimitiveLikeDeep`
                // to reconcile them. This contrasts with heap fields, where
                // the recorded `Contents` shape is a contract; for arrays the
                // contract is the element type, not any specific cell
                // representation. A truly different shape (e.g. writing a
                // `NativeInt` payload into an `Int32` cell, which can have
                // matching width on 32-bit) is still rejected — `Numeric
                // Int32` and `Numeric NativeInt` differ at the unwrapped
                // constructor level.
                //
                // Use `CliType.sizeOf` rather than `byteAddressableCellSize`
                // because the cell itself may already carry non-byte-
                // renderable provenance.
                let arrObj = state.ManagedHeap.Arrays.[arr]

                if arrObj.Length = 0 then
                    failwith
                        $"TODO: byte-view typed store into empty array %O{arr} at index %d{index} offset %d{byteOffset}"

                let cellSize = CliType.sizeOf arrObj.Elements.[0]
                let cellAdvance, inCellStart = floorDivRem byteOffset cellSize
                let newSize = CliType.sizeOf newValue

                if
                    inCellStart = 0
                    && newSize = cellSize
                    && haveSameCliShape arrObj.Elements.[0] newValue
                then
                    let targetCell = index + cellAdvance

                    if targetCell < 0 || targetCell >= arrObj.Length then
                        failwith
                            $"TODO: byte-view typed store past array bounds at cell %d{targetCell} of length %d{arrObj.Length}"

                    IlMachineThreadState.setArrayValue arr newValue targetCell state
                else
                    failwith
                        $"TODO: primitive indirect store of %O{newValue} through byte-view byref %O{src} cannot preserve %s{reason}: write size %d{newSize}, cell size %d{cellSize}, in-cell offset %d{inCellStart}, cell shape %O{arrObj.Elements.[0]}"
            | ValueSome _ ->
                failwith
                    $"TODO: primitive indirect store of %O{newValue} through byte-view byref %O{src} cannot preserve %s{reason}"
            | ValueNone ->
                let existing =
                    knownExisting
                    |> Option.defaultWith (fun () -> readManagedByref baseClassTypes state src)

                let existingSize = CliType.sizeOf existing
                let newSize = CliType.sizeOf newValue

                if existingSize <> newSize then
                    failwith
                        $"TODO: primitive indirect store of %O{newValue} through %O{src} cannot preserve %s{reason}: destination is %d{existingSize} bytes but value is %d{newSize} bytes"

                writeManagedByrefWithBase baseClassTypes state src newValue

    /// Flatten a `stind` destination over a byte-addressable block root
    /// (`StackMemoryByte` / `NativeMemoryByte`) to a single byte offset
    /// relative to the block's origin.
    ///
    /// A bare root is already flat. A projected root collapses through
    /// `peelTrailingByteView`: the canonical shapes are `[ReinterpretAs T]`
    /// and `[ReinterpretAs T; ByteOffset n]`, which is what a `Span<T>`
    /// element indexer or `GetPinnableReference` over stackalloc/native
    /// memory produces, plus the chained forms with interior `Field` steps.
    /// This is deliberately the same reduction
    /// `writeManagedByrefBytesOrTypedCell` performs one layer down, so that
    /// the caller's typed-write-safety test is asked about exactly the byte
    /// range the eventual write will touch.
    ///
    /// `ValueNone` means the chain does not reduce to a single offset (no
    /// `ReinterpretAs` anchor, or a residual structural prefix left over).
    /// The caller must then fail loud rather than guess: a provenance-bearing
    /// payload has no byte pattern, so there is no safe fallback.
    let private tryFlatBlockByteOffset
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (rootByteOffset : int)
        (projs : ByrefProjection list)
        : int voption
        =
        match projs with
        | [] -> ValueSome rootByteOffset
        | _ ->
            match peelTrailingByteView (Some baseClassTypes) state projs with
            | ValueSome ([], viewByteOffset) -> ValueSome (rootByteOffset + viewByteOffset)
            | ValueSome _
            | ValueNone -> ValueNone

    /// Store the payload of a primitive `stind.*` instruction.
    ///
    /// Byte-addressable values take the byte-scatter path, so `stind.i1` over
    /// an existing `Int32` slot updates one byte rather than replacing the slot
    /// with an `Int8`. Numeric provenance-bearing values (for example
    /// `NativeIntSource.FieldHandlePtr`) deliberately cannot be flattened to
    /// bytes. For those, and for exact-width replacement of an existing
    /// provenance-bearing numeric cell, use a typed store when the destination
    /// width proves that byte scatter and whole-cell replacement have the same
    /// address range. This intentionally records the payload's primitive shape
    /// when it differs from the previous same-width primitive template: the
    /// tag is part of the value being stored. `StackMemoryByte` and
    /// `NativeMemoryByte` byrefs use the same whole-cell test as
    /// `writeManagedByrefBytesOrTypedCell`, asked about the flattened
    /// destination offset (`tryFlatBlockByteOffset`) so that a byte-view
    /// projection chain — the shape a `Span<T>` element indexer over
    /// stackalloc/native memory produces — is serviced identically to a bare
    /// root rather than rejected one layer above the code that can honour it.
    /// Same-width
    /// byte-renderable stores restamp the cell when the primitive shape differs,
    /// even if the bytes are identical; byte-identical differently-sized stores
    /// preserve the existing cell because restamping would discard bytes outside
    /// the payload range. Provenance-bearing payloads are not byte-renderable,
    /// so same-width local-memory stores still restamp the typed cell with the
    /// payload's tag and shape.
    /// Reference stores are not routed here; `stind.ref` remains on the
    /// typed/reference-aware path.
    let writeIndirectPrimitiveStore
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (src : ManagedPointerSource)
        (newValue : CliType)
        : IlMachineState
        =
        match src with
        | ManagedPointerSource.NativeIntPlaceholder bits ->
            failwith
                $"writeIndirectPrimitiveStore: cannot write through fake non-null byref @ 0x%x{bits}; the placeholder must never be dereferenced"
        | ManagedPointerSource.Byref (ByrefRoot.StackMemoryByte (thread, frame, block, rootByteOffset), projs) ->
            match byteAddressabilityRejection newValue with
            | Some rejection when isNumericProvenanceRejection rejection ->
                let typedWriteSafe =
                    match tryFlatBlockByteOffset baseClassTypes state rootByteOffset projs with
                    | ValueNone -> false
                    | ValueSome byteOffset ->
                        let pool = IlMachineThreadState.getStackMemoryPool thread frame state
                        stackMemoryByteTypedWriteSafe pool block byteOffset newValue

                if typedWriteSafe then
                    writeManagedByrefBytesOrTypedCell baseClassTypes state src newValue
                else
                    failwith
                        $"TODO: primitive indirect store of %O{newValue} through byte-view byref %O{src} cannot preserve new value's %s{rejection.Description}"
            | _ -> writeManagedByrefBytesOrTypedCell baseClassTypes state src newValue
        | ManagedPointerSource.Byref (ByrefRoot.NativeMemoryByte (block, rootByteOffset), projs) ->
            match byteAddressabilityRejection newValue with
            | Some rejection when isNumericProvenanceRejection rejection ->
                let typedWriteSafe =
                    match tryFlatBlockByteOffset baseClassTypes state rootByteOffset projs with
                    | ValueNone -> false
                    | ValueSome byteOffset ->
                        nativeMemoryByteTypedWriteSafe state.Kernel.NativeMemoryPool block byteOffset newValue

                if typedWriteSafe then
                    writeManagedByrefBytesOrTypedCell baseClassTypes state src newValue
                else
                    failwith
                        $"TODO: primitive indirect store of %O{newValue} through byte-view byref %O{src} cannot preserve new value's %s{rejection.Description}"
            | _ -> writeManagedByrefBytesOrTypedCell baseClassTypes state src newValue
        | ManagedPointerSource.Null -> failwith "TODO: throw NullReferenceException"
        | ManagedPointerSource.Byref _ ->
            let sourceRejection = byteAddressabilityRejection newValue

            match sourceRejection with
            | Some rejection when isNumericProvenanceRejection rejection ->
                writeExactWidthPrimitiveTypedStore
                    baseClassTypes
                    state
                    src
                    newValue
                    $"new value's %s{rejection.Description}"
                    None
            | _ ->
                match splitTrailingByteView src with
                | ValueSome (ByrefRoot.ArrayElement (arr, index), [], byteOffset) ->
                    // Symmetric to the `ValueNone` arm below for the
                    // non-byte-view case: a byte-renderable payload still
                    // needs the typed-store path when the *existing* array
                    // cell carries non-byte-renderable numeric provenance.
                    // Sequence like `*p = handle; *p = IntPtr.Zero;` over a
                    // `fixed (IntPtr* p = arr)` lands here on the second
                    // store — the new value is the byte-addressable zero
                    // but the cell still holds a `TypeHandlePtr`, which the
                    // byte-scatter path would refuse.
                    let arrObj = state.ManagedHeap.Arrays.[arr]

                    let typedCellOverride =
                        if arrObj.Length = 0 then
                            ValueNone
                        else
                            let cellSize = CliType.sizeOf arrObj.Elements.[0]
                            let cellAdvance, inCellStart = floorDivRem byteOffset cellSize

                            if inCellStart = 0 && CliType.sizeOf newValue = cellSize then
                                let targetCell = index + cellAdvance

                                if targetCell >= 0 && targetCell < arrObj.Length then
                                    // Same destination-side test as the non-array arm below: the
                                    // *routing* question is about the cell, so both sites must
                                    // ask it the same way. (What the two do once routed still
                                    // differs: the array path in
                                    // `writeExactWidthPrimitiveTypedStore` demands
                                    // `haveSameCliShape` and fails loudly otherwise, while the
                                    // non-array path checks width only and restamps the cell with
                                    // the payload's shape. That asymmetry predates this change.)
                                    // An array element cell really can be a
                                    // `CliType.RuntimePointer` — C# has no `int*[]` syntax, but
                                    // `Array.CreateInstance(typeof(int*), n)` succeeds and
                                    // `MemoryMarshal.GetArrayDataReference(Array)` hands out a
                                    // byte-view byref over one.
                                    //
                                    // This does not make that case *work*: measured, a pointer
                                    // cell that gets past the payload gate reaches
                                    // `writeExactWidthPrimitiveTypedStore` and stops at its
                                    // `haveSameCliShape` check, because a `stind.i` payload
                                    // arrives as `Numeric NativeInt` while the cell is a
                                    // `RuntimePointer`. That is a separate, pre-existing gap in
                                    // the byte-view typed store, and it fails loudly with a
                                    // message naming the shapes. Routing here is still the honest
                                    // classification, and it only ever diverts cases that
                                    // previously failed too.
                                    match byteAddressabilityRejection arrObj.Elements.[targetCell] with
                                    | Some rejection when destinationNeedsWholeCellStore newValue rejection ->
                                        ValueSome (arrObj.Elements.[targetCell], rejection)
                                    | _ -> ValueNone
                                else
                                    ValueNone
                            else
                                ValueNone

                    match typedCellOverride with
                    | ValueSome (existing, rejection) ->
                        writeExactWidthPrimitiveTypedStore
                            baseClassTypes
                            state
                            src
                            newValue
                            $"destination cell's existing %s{rejection.Description}"
                            (Some existing)
                    | ValueNone -> writeManagedByrefBytesOrTypedCell baseClassTypes state src newValue
                | ValueSome _ -> writeManagedByrefBytesOrTypedCell baseClassTypes state src newValue
                | ValueNone ->
                    // Even a byte-renderable payload may need a typed store when the
                    // destination cell has no byte image of its own — either because it carries
                    // non-byte-renderable numeric provenance, or because it is a pointer-typed
                    // slot (`void*`, `delegate*<...>`), which has no byte image at all. The
                    // pointer case additionally requires a pointer-shaped payload; see
                    // `destinationNeedsWholeCellStore`.
                    let existing = readManagedByref baseClassTypes state src

                    match byteAddressabilityRejection existing with
                    | Some rejection when destinationNeedsWholeCellStore newValue rejection ->
                        writeExactWidthPrimitiveTypedStore
                            baseClassTypes
                            state
                            src
                            newValue
                            $"destination's existing %s{rejection.Description}"
                            (Some existing)
                    | _ -> writeManagedByrefBytesOrTypedCell baseClassTypes state src newValue
