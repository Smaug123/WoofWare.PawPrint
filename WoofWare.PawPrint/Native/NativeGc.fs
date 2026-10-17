namespace WoofWare.PawPrint

open System.Collections.Immutable

/// A layout of `System.GCMemoryInfoData` that `GC.GetMemoryInfo`'s native filler is known to write.
/// CoreCLR's `GCInterface::GetMemoryInfo` (comutilnative.cpp) writes every instance field of the
/// object through a native mirror of its layout (comutilnative.h), so the managed declaration and
/// the VM change together.
[<RequireQualifiedAccess>]
type GcMemoryInfoDataLayout =
    /// .NET 10's: each generation's `GCGenerationInfo`, and each pause duration, is a field of its
    /// own (`_generationInfo0` to `_generationInfo4`, `_pauseDuration0` and `_pauseDuration1`).
    | SeparateFields
    /// .NET 11's: `InlineArray5<GCGenerationInfo> _generationInfo` and
    /// `InlineArray2<TimeSpan> _pauseDurations`.
    | InlineArrays

[<RequireQualifiedAccess>]
module GcMemoryInfoDataLayout =
    /// Every layout.
    let known : GcMemoryInfoDataLayout list =
        [ GcMemoryInfoDataLayout.SeparateFields ; GcMemoryInfoDataLayout.InlineArrays ]

    let private scalarFieldNames : string list =
        [
            "_highMemoryLoadThresholdBytes"
            "_totalAvailableMemoryBytes"
            "_memoryLoadBytes"
            "_heapSizeBytes"
            "_fragmentedBytes"
            "_totalCommittedBytes"
            "_promotedBytes"
            "_pinnedObjectsCount"
            "_finalizationPendingCount"
            "_index"
            "_generation"
            "_pauseTimePercentage"
            "_compacted"
            "_concurrent"
        ]

    /// The instance fields `GCMemoryInfoData` declares in `layout`, in declaration order.
    let fieldNames (layout : GcMemoryInfoDataLayout) : string list =
        match layout with
        | GcMemoryInfoDataLayout.SeparateFields ->
            scalarFieldNames
            @ [
                "_generationInfo0"
                "_generationInfo1"
                "_generationInfo2"
                "_generationInfo3"
                "_generationInfo4"
                "_pauseDuration0"
                "_pauseDuration1"
            ]
        | GcMemoryInfoDataLayout.InlineArrays -> scalarFieldNames @ [ "_generationInfo" ; "_pauseDurations" ]

    /// The layout whose instance fields are `declared`, in declaration order, if there is one.
    let classify (declared : string list) : GcMemoryInfoDataLayout option =
        known |> List.tryFind (fun layout -> fieldNames layout = declared)

    /// Each known layout's fields, for a refusal to name.
    let describeKnown : string =
        known
        |> List.map (fun layout ->
            let fields = String.concat ", " (fieldNames layout)
            $"%O{layout} (%s{fields})"
        )
        |> String.concat "; "

[<RequireQualifiedAccess>]
module NativeGc =
    /// Write the zero of its declared type into one of `addr`'s instance fields.
    let private zeroField
        (ctx : NativeCallContext)
        (state : IlMachineState)
        (addr : ManagedHeapAddress)
        (fieldName : string)
        : IlMachineState
        =
        let operation = "GC.GetMemoryInfo"
        let obj = ManagedHeap.get addr state.ManagedHeap

        let _, declaringTypeInfo =
            IlMachineState.tryGetConcreteTypeInfo state obj.ConcreteType
            |> Option.defaultWith (fun () ->
                failwith $"%s{operation}: GCMemoryInfoData's concrete type %O{obj.ConcreteType} has no TypeDef row"
            )

        let fieldInfo = FieldIdentity.requiredOwnInstanceField declaringTypeInfo fieldName

        // Look up the field's declared signature so the zero value gets the field's own
        // concrete shape (recursing into that struct's fields), rather than assuming a shape
        // here that could drift from the real declaration.
        let state, zero, _handle =
            IlMachineState.cliTypeZeroOf
                ctx.LoggerFactory
                ctx.BaseClassTypes
                ctx.BaseClassTypes.Corelib
                fieldInfo.Signature
                ImmutableArray.Empty
                ImmutableArray.Empty
                state

        IlMachineState.setOwnInstanceField addr fieldName zero state

    /// `GC_ALLOC_FLAGS` (`GC.CoreCLR.cs:94-99`, kept in sync upstream with `gcinterface.h`).
    /// Only these two ever reach `GCInterface_AllocateNewArray`: CoreCLR `_ASSERTE`s that no
    /// other bit is set (`comutilnative.cpp:953`), because `GC.cs` is the only caller.
    let private GC_ALLOC_ZEROING_OPTIONAL = 16
    let private GC_ALLOC_PINNED_OBJECT_HEAP = 64

    /// The SZ-array type `GCInterface_AllocateNewArray` was asked to allocate, and its element
    /// type. Both managed callers pass `typeof(T[]).TypeHandle`, so anything else here means a
    /// caller we have not read rather than a guest-reachable state; CoreCLR likewise only
    /// `_ASSERTE`s the shape (`comutilnative.cpp:950`, plus `AllocateSzArray`'s
    /// `ELEMENT_TYPE_SZARRAY` assert at `gchelpers.cpp:635`).
    let private szArrayTypeForAllocation
        (operation : string)
        (target : RuntimeTypeHandleTarget)
        : ConcreteTypeHandle * ConcreteTypeHandle
        =
        match target with
        | RuntimeTypeHandleTarget.DynamicMethodsClass scopeAssembly ->
            RuntimeTypeHandleTarget.refuseMetadataQuery operation scopeAssembly
        | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.OneDimArrayZero element as arrayType) -> arrayType, element
        | RuntimeTypeHandleTarget.Closed other ->
            failwith
                $"%s{operation}: expected the type handle of an SZ array, got %O{other}; both managed callers pass typeof(T[]).TypeHandle"
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
            failwith $"%s{operation}: expected a closed SZ array type handle, got open generic definition %O{identity}"
        | RuntimeTypeHandleTarget.OpenConstructed (definition, _) ->
            failwith
                $"%s{operation}: expected a closed SZ array type handle, got open constructed type %O{definition.TypeDefinition.Get}"
        | RuntimeTypeHandleTarget.GenericParameter (declaringType, position) ->
            failwith
                $"%s{operation}: expected a closed SZ array type handle, got generic parameter #%i{position} of %O{declaringType.TypeDefinition.Get}"
        | RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, declaringMethod, position) ->
            failwith
                $"%s{operation}: expected a closed SZ array type handle, got method generic parameter #%i{position} of method %O{declaringMethod.Get} on %O{declaringType.TypeDefinition.Get}"
        | RuntimeTypeHandleTarget.Composite _
        | RuntimeTypeHandleTarget.FunctionPointer _ ->
            failwith $"%s{operation}: expected a closed SZ array type handle, got %O{target}"

    /// `System.GC.GetMemoryInfo(GCMemoryInfoData data, int kind)`: the InternalCall behind
    /// the public `GC.GetGCMemoryInfo(GCKind)`. CoreCLR (comutilnative.cpp:585,
    /// GCInterface::GetMemoryInfo) takes the address of every field of `data` and hands them to
    /// `GCHeapUtilities::GetGCHeap()->GetMemoryInfo(...)` (gc.cpp:51875), which fills them in
    /// from `kind`'s corresponding `last_gc_info` record (an all-zero, statically-initialised
    /// struct until the first completed GC of that kind writes into it) plus two fields —
    /// `_highMemoryLoadThresholdBytes` and `_totalAvailableMemoryBytes` — that are always
    /// derived from the configured heap limit / physical memory, independent of GC history.
    ///
    /// PawPrint's interpreter never performs a garbage collection, of any kind, ever. So for
    /// every `kind` the honest answer to "what did the last GC of this kind record" is "there
    /// has never been one" — which is exactly the documented all-zero state that
    /// `GCMemoryInfo`'s doc comment describes (Index == 0, with every other field zero too, is
    /// the caller-facing way to detect "no GC of this kind has happened"). Unlike a real
    /// process asking this before its first GC, this is not a transient startup state that
    /// later becomes stale: it is permanently, unconditionally true here.
    ///
    /// `_highMemoryLoadThresholdBytes` / `_totalAvailableMemoryBytes` are also reported as zero,
    /// which is a real departure from upstream: no real machine ever has zero physical memory,
    /// so a real CLR never returns this combination. Unlike the GC-history fields above, these
    /// two are computed unconditionally from `total_physical_mem` upstream
    /// (`gc.cpp`, `GCHeap::GetMemoryInfo`) rather than from any `last_gc_info` record, so
    /// reporting them as zero is a genuine divergence rather than the documented never-collected
    /// state. There are two known consumers, and neither crashes:
    ///
    ///   1. `System.Buffers.Utilities.GetMemoryPressure` (behind `ArrayPool<T>`'s trim
    ///      heuristic) reads `0 >= 0` as "high pressure" where a real CLR reads "no data yet"
    ///      as low pressure, so `ArrayPool<T>` trims more eagerly here. Not a correctness
    ///      concern: pool contents were never part of that type's contract.
    ///   2. `System.Threading.PortableThreadPool.OnGen2GCCallback` reads
    ///      `HighMemoryLoadThresholdBytes` and `MemoryLoadBytes` directly, and the gate thread
    ///      calls it *eagerly at startup* rather than only on a real GC. Its result feeds the
    ///      thread pool's blocking-adjustment heuristic, which bails out at
    ///      `if (memoryLimitBytes <= 0) break;` given our zero. So the memory-pressure throttle
    ///      on worker-thread growth is skipped entirely: PawPrint's simulated pool never
    ///      throttles for memory pressure, and is silently more permissive than a real CLR
    ///      under genuine pressure. Harmless today (PawPrint models no memory limit to be under
    ///      pressure *from*), but this is the first place to look if simulated and real
    ///      thread-pool growth ever need to agree.
    ///
    /// PawPrint has no notion of a simulated physical-memory budget and no consumer needing a
    /// specific value yet (see `EmulatedKernel.fs` for where such host-visible-but-deterministic
    /// state belongs if a real need arises); inventing one here would be speculative generality.
    /// If a real need materialises, add that modelling there rather than growing an ad hoc
    /// constant in this handler.
    ///
    /// The VM writes every instance field of `data`, so this does too, each with the zero of its
    /// declared type, in whichever `GcMemoryInfoDataLayout` the CoreLib declares; it refuses any
    /// other layout, whose VM it has not read.
    let tryExecute (ctx : NativeCallContext) : NativeHandlerResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            ctx.TargetAssembly.Name.Name,
            ctx.TargetType.Namespace,
            ctx.TargetType.Name,
            instruction.ExecutingMethod.Name,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | "System.Private.CoreLib",
          "System",
          "GC",
          "GetMemoryInfo",
          [ CorelibType state.TypeSystem.ConcreteTypes ("System", "GCMemoryInfoData", dataGenerics)
            ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Void when dataGenerics.IsEmpty ->
            let operation = "GC.GetMemoryInfo"

            if instruction.Arguments.Length <> 2 then
                failwith $"%s{operation}: expected two arguments, got %d{instruction.Arguments.Length}"

            let dataAddr =
                match instruction.Arguments.[0] with
                | CliType.ObjectRef (Some addr) -> addr
                | CliType.ObjectRef None ->
                    failwith
                        $"%s{operation}: data was null; GC.GetGCMemoryInfo always allocates a fresh GCMemoryInfoData immediately before calling this InternalCall"
                | other -> failwith $"%s{operation}: expected GCMemoryInfoData object reference, got %O{other}"

            let kind = NativeCall.int32Argument operation instruction.Arguments.[1]

            // GC.GetGCMemoryInfo(GCKind) range-checks `kind` against [GCKind.Any, GCKind.Background]
            // = [0, 3] in managed code before it ever reaches this InternalCall, so an out-of-range
            // value here means some caller bypassed that check. Assert rather than silently treat it
            // as one of the four real kinds.
            if kind < 0 || kind > 3 then
                failwith
                    $"%s{operation}: kind %d{kind} is outside GCKind's [Any, Background] = [0, 3] range; the managed GetGCMemoryInfo wrapper validates this before reaching the InternalCall"

            let declared =
                let obj = ManagedHeap.get dataAddr state.ManagedHeap

                let _, declaringTypeInfo =
                    IlMachineState.tryGetConcreteTypeInfo state obj.ConcreteType
                    |> Option.defaultWith (fun () ->
                        failwith
                            $"%s{operation}: GCMemoryInfoData's concrete type %O{obj.ConcreteType} has no TypeDef row"
                    )

                declaringTypeInfo.Fields
                |> List.filter (fun field -> not field.IsStatic)
                |> List.map (fun field -> field.Name)

            let layout =
                GcMemoryInfoDataLayout.classify declared
                |> Option.defaultWith (fun () ->
                    failwith
                        $"%s{operation}: this CoreLib's GCMemoryInfoData declares the instance fields %A{declared}, which is no layout PawPrint knows the VM to write. Known: %s{GcMemoryInfoDataLayout.describeKnown}"
                )

            let state =
                GcMemoryInfoDataLayout.fieldNames layout
                |> List.fold (fun state fieldName -> zeroField ctx state dataAddr fieldName) state

            NativeHandlerResult.completed state |> Some
        | _ -> None

    /// `GCInterface_AllocateNewArray` (`comutilnative.cpp:940`): the QCall behind both
    /// `GC.AllocateArray<T>(int, bool)` and `GC.AllocateUninitializedArray<T>(int, bool)`, and
    /// their only shared primitive. Upstream it forwards straight to `AllocateSzArray`
    /// (`gchelpers.cpp:624`), whose entire guest-observable contract is: reject a negative
    /// length with `OverflowException`, reject a length past `Array.MaxLength` with an
    /// `OutOfMemoryException` carrying `IDS_EE_ARRAY_DIMENSIONS_EXCEEDED`, and otherwise return
    /// a fresh SZ array of that length.
    ///
    /// Neither `GC_ALLOC_FLAGS` bit that can reach here changes what PawPrint does:
    ///
    ///   * `GC_ALLOC_ZEROING_OPTIONAL` (set by `AllocateUninitializedArray`) *permits* the
    ///     runtime to skip zeroing; it does not require it, and the API's contract states the
    ///     contents are unspecified. PawPrint always zeroes.
    ///     The consequence is worth knowing when writing a
    ///     differential test: reading an element of an uninitialized array before writing it
    ///     yields 0 here and arbitrary bytes on CoreCLR. See `docs/divergences.md`.
    ///   * `GC_ALLOC_PINNED_OBJECT_HEAP` (set by `pinned: true`) asks for the pinned object heap.
    ///     PawPrint never collects and never moves an object, so every allocation is already
    ///     permanently pinned in the only sense this API can express, and there is no generation
    ///     or heap-segment modelling for the request to feed — `GC.GetGeneration`, the usual way
    ///     a guest would notice POH placement, is not implemented at all.
    ///
    /// Any *other* flag bit is a different matter: CoreCLR `_ASSERTE`s that none is set, on the
    /// grounds that `GC.cs` is the only caller, so one appearing here means a caller we have not
    /// read rather than a guest-reachable state. That fails loudly.
    let tryExecuteQCall (entryPoint : string) (ctx : NativeCallContext) : NativeHandlerResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            entryPoint,
            ctx.TargetAssembly.Name.Name,
            ctx.TargetType.Namespace,
            ctx.TargetType.Name,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | "GCInterface_AllocateNewArray",
          "System.Private.CoreLib",
          "System",
          "GC",
          [ ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.IntPtr
            ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Int32
            // `GC_ALLOC_FLAGS` is nested in `GC`, and a nested TypeDef carries an empty
            // namespace in ECMA metadata (II.22.37), so it is matched with "" rather than
            // "System" — as `NativeException` matches the nested `ExceptionMessageKind`.
            CorelibType state.TypeSystem.ConcreteTypes ("", "GC_ALLOC_FLAGS", flagsGenerics)
            CorelibType state.TypeSystem.ConcreteTypes ("System.Runtime.CompilerServices",
                                                        "ObjectHandleOnStack",
                                                        objectHandleGenerics) ],
          MethodReturnType.Void when flagsGenerics.IsEmpty && objectHandleGenerics.IsEmpty ->
            let operation = "GC.AllocateNewArray"

            if instruction.Arguments.Length <> 4 then
                failwith $"%s{operation}: expected four native arguments, got %d{instruction.Arguments.Length}"

            // Both callers reach this through `RuntimeTypeHandle.ToIntPtr(typeof(T[]).TypeHandle)`,
            // i.e. the raw `RuntimeType.m_handle` rather than a `QCallTypeHandle` wrapper.
            let typeHandle =
                NativeCall.runtimeTypeHandleTargetOfEvalStackValue
                    operation
                    (EvalStackValue.ofCliType instruction.Arguments.[0])

            let length = NativeCall.int32Argument operation instruction.Arguments.[1]
            let flags = NativeCall.int32Argument operation instruction.Arguments.[2]

            let ret =
                NativeCall.objectHandleOnStackTarget operation state "ret" instruction.Arguments.[3]

            let unrecognisedFlags =
                flags &&& ~~~(GC_ALLOC_ZEROING_OPTIONAL ||| GC_ALLOC_PINNED_OBJECT_HEAP)

            if unrecognisedFlags <> 0 then
                failwith
                    $"%s{operation}: GC_ALLOC_FLAGS 0x%x{flags} sets bits 0x%x{unrecognisedFlags} outside the GC_ALLOC_ZEROING_OPTIONAL|GC_ALLOC_PINNED_OBJECT_HEAP mask 0x%x{GC_ALLOC_ZEROING_OPTIONAL ||| GC_ALLOC_PINNED_OBJECT_HEAP}; CoreCLR asserts that GC.cs is the only caller and passes no others"

            // Destructure the type handle before the range checks, as CoreCLR does: a handle we
            // cannot read is a bug in the caller, and should be reported as such even when the
            // length would independently have thrown.
            let arrayType, elementType = szArrayTypeForAllocation operation typeHandle

            // The same two `AllocateSzArray` checks the `newarr` opcode makes, in the same
            // order, from the same classifier — `gchelpers.cpp:637-641`. Both are reached
            // before any allocation is attempted, so an absurd length costs nothing, and both
            // leave `ret` at whatever the caller initialised it to (null, in both callers).
            match SzArrayAllocation.checkLength length with
            | Some err ->
                let fault, message = SzArrayAllocation.faultFor err
                let exceptionType = OpcodeFault.resolve ctx.BaseClassTypes fault

                NativeHandlerResult.raiseExceptionWithMessage exceptionType message state
                |> Some
            | None ->

            // Zeroing happens even under GC_ALLOC_ZEROING_OPTIONAL because it is the only
            // content we can produce: `allocateArray` fills from a `CliType` template and the
            // heap model has no representation for unset storage.
            let zero, state =
                IlMachineState.cliTypeZeroOfHandle state ctx.BaseClassTypes elementType

            let arrayAddr, state =
                IlMachineState.allocateArray arrayType (fun () -> zero) length state

            IlMachineState.writeManagedByrefWithBase
                ctx.BaseClassTypes
                state
                (ManagedPointerSource.requireAddressed ret)
                (CliType.ObjectRef (Some arrayAddr))
            |> NativeHandlerResult.completed
            |> Some
        | _ -> None
