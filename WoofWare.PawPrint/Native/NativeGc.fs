namespace WoofWare.PawPrint

open System.Collections.Immutable

[<RequireQualifiedAccess>]
module NativeGc =
    /// Zero out a field whose declared type is itself a value type (`GCGenerationInfo`,
    /// `TimeSpan`) rather than a primitive: looks up the field's declared signature so the
    /// zero value gets the field's own concrete shape (recursing into that struct's fields),
    /// rather than assuming a shape here that could drift from the real declaration.
    let private zeroStructField
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

    let private zeroInt64 : CliType =
        CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L))

    let private zeroInt32 : CliType = CliType.Numeric (CliNumericType.Int32 0)

    let private zeroByte : CliType = CliType.Numeric (CliNumericType.UInt8 0uy)

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
    /// state. There are two known consumers, and neither crashes, but the second is more
    /// consequential than it first appears:
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
    /// Known limitation, tracked as issue #729. The scalar fields written here are all readable,
    /// but `GCMemoryInfo`'s two *span-valued* properties are not usable past index 0.
    /// `GenerationInfoAsSpan`/`PauseDurationsAsSpan` are built with
    /// `MemoryMarshal.CreateReadOnlySpan(ref _generationInfo0, 5)` — a byref to the first of a run
    /// of sibling fields, walked forward by `sizeof(element)` on the assumption that
    /// `[StructLayout(Sequential)]` made them contiguous. PawPrint stores each field as its own
    /// cell, so index 1 onwards walks off the end of the first cell and fails in
    /// `IlMachineManagedByref.resolveCell`. That is a general "sibling fields are not contiguous"
    /// gap rather than anything GC-specific — this handler is merely the first thing to make an
    /// instance of it reachable — so it is deliberately not worked around here; fixing it in the
    /// byref machinery would unblock every BCL type using the same idiom.
    /// `sourcesPure/GCMemoryInfoSpanProperties.cs` pins it in the `unimplemented` set.
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
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "GCMemoryInfoData", dataGenerics)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
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

            let state =
                [
                    "_highMemoryLoadThresholdBytes", zeroInt64
                    "_totalAvailableMemoryBytes", zeroInt64
                    "_memoryLoadBytes", zeroInt64
                    "_heapSizeBytes", zeroInt64
                    "_fragmentedBytes", zeroInt64
                    "_totalCommittedBytes", zeroInt64
                    "_promotedBytes", zeroInt64
                    "_pinnedObjectsCount", zeroInt64
                    "_finalizationPendingCount", zeroInt64
                    "_index", zeroInt64
                    "_generation", zeroInt32
                    "_pauseTimePercentage", zeroInt32
                    "_compacted", zeroByte
                    "_concurrent", zeroByte
                ]
                |> List.fold
                    (fun state (fieldName, value) -> IlMachineState.setOwnInstanceField dataAddr fieldName value state)
                    state

            let state =
                [
                    "_generationInfo0"
                    "_generationInfo1"
                    "_generationInfo2"
                    "_generationInfo3"
                    "_generationInfo4"
                    "_pauseDuration0"
                    "_pauseDuration1"
                ]
                |> List.fold (fun state fieldName -> zeroStructField ctx state dataAddr fieldName) state

            NativeHandlerResult.completed state |> Some
        | _ -> None
