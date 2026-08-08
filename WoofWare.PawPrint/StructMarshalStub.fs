namespace WoofWare.PawPrint

open System.Collections.Immutable
open Microsoft.Extensions.Logging

/// How one field's managed value becomes its unmanaged form.
///
/// CoreCLR decides this in `MarshalInfo::MarshalInfo` (mlinfo.cpp) and emits one IL marshaller
/// per field into the synthesised struct stub. PawPrint has no IL synthesis, so the decision is
/// data and `StructMarshalStub.executeStubCall` is its interpreter.
[<RequireQualifiedAccess>]
type StructMarshalFieldKind =
    /// The managed value's byte image *is* the native image: emit it unchanged. This is the
    /// per-field equivalent of the whole-struct blittable memmove path.
    | CopyBytes
    /// CoreCLR's `MARSHAL_TYPE_DATE` (mlinfo.cpp:1747): a `System.DateTime` field becomes an
    /// 8-byte little-endian IEEE-754 double holding `dt.ToOADate()`, *not* the managed
    /// `ulong _dateData` image. The conversion is `ILDateMarshaler::EmitConvertContentsCLRToNative`
    /// (ilmarshalers.cpp:1241), which calls managed `StubHelpers.DateMarshaler.ConvertToNative`.
    | OADate

/// One field's contribution to the unmanaged image: where it goes, and how it gets there.
type StructMarshalStep =
    {
        Placement : MarshalFieldPlacement
        Kind : StructMarshalFieldKind
    }

/// Everything needed to write a struct's unmanaged image, derived once from the struct's value.
type StructMarshalPlan =
    {
        NativeSize : SizeofResult
        Steps : StructMarshalStep list
    }

/// The runtime-synthesised struct-marshalling stub that CoreCLR's
/// `MarshalNative_TryGetStructMarshalStub` (marshalnative.cpp:118) returns for a type that has
/// layout but is not blittable.
///
/// CoreCLR emits IL and hands CoreLib the entry address; CoreLib invokes it with `calli` through
/// `delegate*&lt;ref byte, byte*, int, ref CleanupWorkListElement?, void&gt;`. PawPrint has neither IL
/// synthesis nor code addresses, so the pointer is a `NativeIntSource.StructMarshalStub` carrying
/// the type's identity and `calli` routes to `executeStubCall` here instead of pushing a frame.
///
/// The stub is *not* self-contained: `MARSHAL_TYPE_DATE` needs `DateTime.ToOADate`, whose
/// behaviour (a zero special case, a VB compatibility fixup, and a guest-visible
/// `OverflowException` below `OADateMinAsTicks`) belongs to the guest's CoreLib and must not be
/// re-derived in the host. So the stub calls the guest's own
/// `StubHelpers.DateMarshaler.ConvertToNative`, once per conversion field.
///
/// Making that work without a frame of our own is the interesting part. A native handler would
/// normally suspend via `NativeHandlerResult.PushedManagedCallee`, but that machinery is bound to
/// `MethodInfo`-backed frames — `AbstractMachine.executeOneStep` only reaches `dispatchNative` for
/// `MethodBody.InternalCall`/`PInvoke`, and re-entry re-locates the handler from the live frame's
/// `ExecutingMethod`. A stub has no `MethodInfo` by construction. Instead the whole `calli`
/// re-executes: each conversion pushes its callee and leaves the program counter unadvanced, the
/// callee's result lands on the eval stack above the stub pointer, and the next execution of the
/// same `calli` sees one more completed result. Nothing is written to memory until every
/// conversion has completed, so a retry never observes a half-written image.
[<RequireQualifiedAccess>]
module StructMarshalStub =

    /// Whether a field's managed byte image is also its native image, i.e. whether CoreCLR's
    /// `IsFieldBlittable` would accept it.
    ///
    /// Shared by the blittable arm of `MarshalNative_TryGetStructMarshalStub` (which needs the
    /// bare yes/no) and by `tryComputePlan` (which needs it per field). One recursion, so the two
    /// cannot drift: a field the QCall's fast path accepts is exactly a field the stub would
    /// copy verbatim.
    let rec isBlittableField
        (concreteTypes : AllConcreteTypes)
        (assemblies : LoadedAssemblies)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (t : CliType)
        : bool
        =
        match t with
        // `NativeInt` cells carry provenance under PawPrint (e.g. `TypeHandlePtr` from
        // `typeof(T).TypeHandle.Value`). CoreCLR memmoves the integer-width bits regardless, but
        // PawPrint's byte model rejects non-`Verbatim` provenance because `CliNumericType.ToBytes`
        // cannot serialise it. We accept `IntPtr`/`UIntPtr` here because the blittable arm returns
        // a null stub, instructing CoreLib to call `SpanHelpers.Memmove(ref byte, ref byte, nuint)`
        // — which PawPrint intercepts and routes through `CellAwareMemOps.copy`, preserving
        // whole-cell provenance when both endpoints anchor on cell-aware roots. The hazard that
        // remains is value-level: a struct holding a non-`Verbatim` `IntPtr` marshalled to
        // `AllocHGlobal`'d native memory (a byte-only endpoint) still falls back to the byte walk
        // and surfaces the `validateByteAddressableCell` failure there, not here.
        | CliType.Numeric (CliNumericType.NativeInt _) -> true
        | CliType.Numeric _ -> true
        | CliType.Bool _
        | CliType.Char _
        | CliType.ObjectRef _
        | CliType.RuntimePointer _ -> false
        | CliType.ValueType vt ->
            // DateTime is structurally a single `ulong _dateData` and would otherwise qualify as
            // strictly numeric, but CoreCLR's `MarshalInfo` (mlinfo.cpp:1747) special-cases
            // DateTime fields as `MARSHAL_TYPE_DATE`: 8 bytes of OADate, NOT the managed
            // `_dateData` byte image. The memmove fast path would silently emit the wrong bytes,
            // so it is not blittable — `tryComputePlan` picks it up as `OADate` instead.
            let isDateTime =
                CliValueType.IsHostKnownDateTime concreteTypes assemblies corelib vt

            // Decimal is structurally four `Int32` fields (`flags`, `hi`, `lo`, `mid`) and would
            // otherwise recurse to true, but CoreCLR's `MarshalInfo` routes Decimal fields through
            // marshal-stub synthesis (`NFT_DECIMAL` in fieldmarshaler.cpp): managed `Decimal` is
            // 16 bytes with 4-byte field alignment, native `DECIMAL` is 16 bytes with 8-byte
            // alignment (its `Lo64` union member is `ULONGLONG`). The outer struct's managed
            // layout therefore positions Decimal at a different offset than the native layout —
            // `{ int x; decimal d; }` is 20 bytes managed, 24 bytes native. Memmoving would write
            // into native padding.
            let isDecimal = CliValueType.IsHostKnownDecimal concreteTypes assemblies corelib vt

            if isDateTime || isDecimal then
                false
            else
                match vt._Storage with
                // RawBytes-backed value types are not the typical struct-with-fields shape;
                // conservatively reject so we don't quietly accept primitive wrappers whose
                // CoreCLR marshal size diverges from the byte image.
                | CliValueTypeStorage.RawBytes _ -> false
                | CliValueTypeStorage.Fields storage ->
                    storage.Fields
                    |> List.forall (fun field -> isBlittableField concreteTypes assemblies corelib field.Contents)

    /// Whether the whole struct is blittable, i.e. whether CoreCLR's `th.IsBlittable()` arm of
    /// `MarshalNative_TryGetStructMarshalStub` applies and the guest can memmove.
    ///
    /// Top-level entry walks the outer struct's fields via `isBlittableField`. Host-known
    /// field-only rejections (Decimal) do not apply to the outer type's *own* declared type,
    /// which is what we're classifying here; a top-level DateTime is filtered earlier by the
    /// AutoLayout gate, and if it ever reached us we'd want the same answer the field walker
    /// gives, so we intentionally don't short-circuit it.
    let isStructStrictlyNumericBlittable
        (concreteTypes : AllConcreteTypes)
        (assemblies : LoadedAssemblies)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (t : CliType)
        : bool
        =
        match t with
        | CliType.ValueType vt ->
            match vt._Storage with
            | CliValueTypeStorage.RawBytes _ -> false
            | CliValueTypeStorage.Fields storage ->
                storage.Fields
                |> List.forall (fun field -> isBlittableField concreteTypes assemblies corelib field.Contents)
        | _ ->
            // Top-level primitive (e.g. `Marshal.StructureToPtr<int>`): defer to the field walker.
            // Primitives are unconditionally blittable; Bool/Char/etc. are not — same semantics
            // either way.
            isBlittableField concreteTypes assemblies corelib t

    /// Derive the write plan for `value`'s unmanaged image, or say why we can't.
    ///
    /// The offsets come from `CliValueType.TryComputeMarshalLayout`, i.e. from the same walk that
    /// answers `Marshal.SizeOf`, so a field cannot land in one place for sizing and another for
    /// writing. Only the *classification* is added here.
    let tryComputePlan
        (concreteTypes : AllConcreteTypes)
        (assemblies : LoadedAssemblies)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (value : CliType)
        : Result<StructMarshalPlan, string>
        =
        match value with
        | CliType.ValueType vt ->
            match vt._Storage with
            | CliValueTypeStorage.RawBytes _ ->
                // `TryComputeMarshalLayout` reports no placements for raw-byte storage because
                // there are no declared fields — which is not the same as "nothing to write".
                Result.Error
                    "the type has raw-byte storage rather than declared fields, so there are no per-field placements to marshal"
            | CliValueTypeStorage.Fields _ ->

            match CliValueType.TryComputeMarshalLayout concreteTypes assemblies corelib vt with
            | Result.Error err -> Result.Error err.Reason
            | Result.Ok (nativeSize, placements) ->

            // A `CopyBytes` step writes the managed value itself at the native offset, so it is
            // only sound when the managed image of that value *is* its native image. For a
            // primitive that is definitional. For a composite it is a claim about the value's
            // interior, and the managed layout walk (`CliValueType.SizeOf`) is not the marshal
            // layout walk (`TryComputeMarshalLayout`) — the two agree for the shapes we have
            // looked at, but nothing enforces it, and this whole arm exists because CoreCLR
            // repositions some fields between the two forms. So accept composites only where the
            // interior is trivial: a primitive-like wrapper (an enum, `IntPtr`, …) is a single
            // field at offset 0, and its image is that field's image whichever walk you use.
            // Anything else needs a recursive plan, and should get one when a test motivates it.
            let isCopyableVerbatim (contents : CliType) : bool =
                isBlittableField concreteTypes assemblies corelib contents
                && (
                    match contents with
                    | CliType.ValueType vt -> vt.PrimitiveLikeKind.IsSome
                    | _ -> true
                )

            let steps =
                placements
                |> List.map (fun placement ->
                    let contents = placement.Field.Contents

                    if isCopyableVerbatim contents then
                        // Necessary condition on top of the shape restriction above: a field
                        // whose native width differs from its managed one cannot be written by
                        // copying the managed value, whatever its interior looks like.
                        let managedSize = CliType.SizeOf contents

                        if managedSize.Size <> placement.NativeSize.Size then
                            Result.Error
                                $"field %s{placement.Field.Name} occupies %d{managedSize.Size} managed byte(s) but %d{placement.NativeSize.Size} native one(s), so its managed image cannot be copied verbatim"
                        else
                            Result.Ok
                                {
                                    Placement = placement
                                    Kind = StructMarshalFieldKind.CopyBytes
                                }
                    else

                    match contents with
                    | CliType.ValueType fieldVt when
                        CliValueType.IsHostKnownDateTime concreteTypes assemblies corelib fieldVt
                        ->
                        Result.Ok
                            {
                                Placement = placement
                                Kind = StructMarshalFieldKind.OADate
                            }
                    | _ when isBlittableField concreteTypes assemblies corelib contents ->
                        Result.Error
                            $"TODO: field %s{placement.Field.Name} is a nested composite whose fields are individually blittable, but writing it verbatim would assume its managed and unmanaged interiors coincide; that needs a recursive marshal plan"
                    | _ ->
                        Result.Error
                            $"field %s{placement.Field.Name} is neither blittable nor a marshalling case PawPrint implements (contents %O{contents})"
                )

            let failures =
                steps
                |> List.choose (fun step ->
                    match step with
                    | Result.Error err -> Some err
                    | Result.Ok _ -> None
                )

            match failures with
            | err :: _ -> Result.Error err
            | [] ->
                Result.Ok
                    {
                        NativeSize = nativeSize
                        Steps =
                            steps
                            |> List.map (fun step ->
                                match step with
                                | Result.Ok step -> step
                                | Result.Error err -> failwith $"unreachable: %s{err}"
                            )
                    }
        | _ -> Result.Error $"only value types have a struct-marshal plan; got %O{value}"

    /// `MarshalOperation` (stubgen.h:26): the third argument CoreLib passes to the stub.
    [<RequireQualifiedAccess>]
    type private Operation =
        | Marshal
        | Unmarshal
        | Cleanup

    /// The eval-stack shape a struct-marshal `calli` presents, once recognised.
    type StubCall =
        {
            /// The type whose unmanaged image is to be written.
            TypeHandle : ConcreteTypeHandle
            /// Results of the conversion calls completed so far, in plan order. Empty on the
            /// first execution of the `calli`.
            Completed : float list
        }

    /// Recognise a `calli` whose target is a struct-marshal stub, including a re-execution part
    /// way through the conversion calls.
    ///
    /// The stub pointer stays on the stack for the whole sequence and each completed conversion
    /// leaves its `double` result above it, so the run of `Float`s above the pointer counts how
    /// far we got. The scan stops at the first value that is neither, which bounds it: an
    /// ordinary `calli` cannot be mistaken for one of these unless a stub pointer is sitting
    /// directly beneath a run of floats, and the only producer of a stub pointer is the QCall,
    /// whose value CoreLib loads immediately below the `calli` that consumes it.
    let tryRecognise (values : EvalStackValue list) : StubCall option =
        let rec go (acc : float list) (vs : EvalStackValue list) : StubCall option =
            match vs with
            | EvalStackValue.NativeInt (NativeIntSource.StructMarshalStub typeHandle) :: _ ->
                Some
                    {
                        TypeHandle = typeHandle
                        Completed = acc
                    }
            | EvalStackValue.Float f :: rest -> go (f :: acc) rest
            | _ -> None

        go [] values

    let private operationOf (operation : string) (value : EvalStackValue) : Operation =
        match value with
        | EvalStackValue.Int32 (Int32Source.Verbatim 0) -> Operation.Marshal
        | EvalStackValue.Int32 (Int32Source.Verbatim 1) -> Operation.Unmarshal
        | EvalStackValue.Int32 (Int32Source.Verbatim 2) -> Operation.Cleanup
        | other ->
            failwith
                $"%s{operation}: expected a MarshalOperation (0=Marshal, 1=Unmarshal, 2=Cleanup) as the third argument, got %O{other}"

    let private managedPointerOf (operation : string) (what : string) (value : EvalStackValue) : ManagedPointerSource =
        match value with
        | EvalStackValue.ManagedPointer src -> src
        | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer src) -> src
        | other -> failwith $"%s{operation}: expected %s{what} to be a managed pointer, got %O{other}"

    /// `System.Byte` as a concrete type, for forming byte-view byrefs into the destination. Same
    /// derivation as `CellAwareMemOps`, whose own copy is private.
    let private byteType
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        : ConcreteType<ConcreteTypeHandle>
        =
        let handle =
            AllConcreteTypes.findExistingNonGenericConcreteType state.ConcreteTypes baseClassTypes.Byte.Identity
            |> Option.defaultWith (fun () -> failwith $"%s{operation}: System.Byte is not concretized")

        AllConcreteTypes.lookup handle state.ConcreteTypes
        |> Option.defaultWith (fun () -> failwith $"%s{operation}: concrete System.Byte handle %O{handle} not found")

    /// The guest's `StubHelpers.DateMarshaler.ConvertToNative(DateTime) -> double`, which is what
    /// CoreCLR's own date marshaller calls (ilmarshalers.cpp:1241).
    let private dateConvertToNative
        (operation : string)
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        : IlMachineState * MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        let declaringType =
            baseClassTypes.Corelib.TypeDefs
            |> Seq.tryPick (fun (KeyValue (_, v)) ->
                if v.Namespace = "System.StubHelpers" && v.Name = "DateMarshaler" then
                    Some v
                else
                    None
            )
            |> Option.defaultWith (fun () ->
                failwith $"%s{operation}: System.StubHelpers.DateMarshaler not found in corelib"
            )

        let method =
            declaringType.Methods
            |> List.tryFind (fun m -> m.Name = "ConvertToNative" && m.IsStatic && MethodInfo.arity m = 1)
            |> Option.defaultWith (fun () ->
                failwith $"%s{operation}: System.StubHelpers.DateMarshaler.ConvertToNative(DateTime) not found"
            )

        let state, concretized, _declaringTypeHandle =
            ExecutionConcretization.concretizeMethodWithAllGenerics
                loggerFactory
                baseClassTypes
                ImmutableArray.Empty
                method
                ImmutableArray.Empty
                state

        state, concretized

    /// Execute (or continue executing) a `calli` whose target is a struct-marshal stub. The
    /// caller has already established, via `tryRecognise`, that the stub pointer is on the eval
    /// stack under `call.Completed.Length` conversion results.
    ///
    /// Returns with the program counter *unadvanced* whenever a conversion callee was pushed, so
    /// the same `calli` runs again once that callee returns.
    let executeStubCall
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (call : StubCall)
        (thread : ThreadId)
        (state : IlMachineState)
        : IlMachineState * WhatWeDid
        =
        let operation = "struct-marshal stub"
        let completedCount = List.length call.Completed

        let frameId = state.ThreadState.[thread].ActiveMethodState
        let values = (IlMachineState.getFrame thread frameId state).EvaluationStack.Values

        // [results...; stub; cleanupList; operation; destination; source; ...caller's own values]
        let argBase = completedCount + 1

        if List.length values < argBase + 4 then
            failwith
                $"%s{operation}: evaluation stack holds %d{List.length values} value(s), too few for the stub pointer, its %d{completedCount} completed conversion(s) and four arguments"

        let op = operationOf operation values.[argBase + 1]

        let destination =
            managedPointerOf operation "the destination pointer" values.[argBase + 2]

        let source = managedPointerOf operation "the source reference" values.[argBase + 3]

        /// Drop the stub pointer, its completed conversion results, and the four arguments, then
        /// step past the `calli`.
        let finish (state : IlMachineState) : IlMachineState * WhatWeDid =
            let mutable state = state

            for _ in 1 .. argBase + 4 do
                let _, next = IlMachineState.popEvalStack thread state
                state <- next

            IlMachineState.advanceProgramCounter thread state, WhatWeDid.Executed

        match op with
        | Operation.Cleanup ->
            // CoreLib calls the stub with `Cleanup` before `Marshal` when `fDeleteOld` is set, to
            // free whatever the previous contents owned. Every field kind we support owns nothing
            // — a copied byte image and an OADate double both live entirely inside the
            // destination buffer — so there is nothing to release. A field kind that *did* own
            // native memory (a `ByValTStr`, an allocated array) would have to be released here,
            // and `tryComputePlan` refuses every such kind today.
            finish state
        | Operation.Unmarshal ->
            failwith
                $"TODO %s{operation}: native-to-managed direction (MarshalOperation.Unmarshal, reached via Marshal.PtrToStructure) is not implemented for type %O{call.TypeHandle}"
        | Operation.Marshal ->

        let template, state =
            IlMachineState.cliTypeZeroOfHandle state baseClassTypes call.TypeHandle

        let sourceValue =
            IlMachineState.readManagedByrefBytesAs baseClassTypes state source template

        let plan =
            match tryComputePlan state.ConcreteTypes state._LoadedAssemblies baseClassTypes sourceValue with
            | Result.Ok plan -> plan
            | Result.Error reason ->
                // Unreachable in practice: the QCall only mints a stub pointer for a type whose
                // plan it has already computed. Reaching it means the plan is not a function of
                // the type alone, which would be a bug worth seeing.
                failwith
                    $"%s{operation}: type %O{call.TypeHandle} has no marshal plan at stub-execution time, though the QCall minted a stub for it: %s{reason}"

        let conversions =
            plan.Steps
            |> List.filter (fun step ->
                match step.Kind with
                | StructMarshalFieldKind.OADate -> true
                | StructMarshalFieldKind.CopyBytes -> false
            )

        if completedCount > List.length conversions then
            failwith
                $"%s{operation}: %d{completedCount} conversion result(s) are on the evaluation stack but type %O{call.TypeHandle} has only %d{List.length conversions} conversion field(s)"

        if completedCount < List.length conversions then
            // Convert the next field by calling the guest's own marshaller, and leave the program
            // counter where it is so this whole `calli` re-executes with one more result in hand.
            let next = conversions.[completedCount]

            let state, convertToNative =
                dateConvertToNative operation loggerFactory baseClassTypes state

            let depthBeforeArgument =
                (IlMachineState.getFrame thread frameId state).EvaluationStack.Values.Length

            let state =
                IlMachineState.pushToEvalStack next.Placement.Field.Contents thread state

            let threadState = state.ThreadState.[thread]

            let state, commitment =
                IlMachineStateExecution.callMethodWithCommitment
                    loggerFactory
                    baseClassTypes
                    None
                    ConstructionState.NotConstructing
                    false
                    false
                    false // leave our program counter alone: this `calli` must run again
                    convertToNative.Generics
                    convertToNative
                    thread
                    threadState
                    None
                    ConstructedObjectDisposition.PushToCaller
                    false // wrapExceptionInTargetInvocation
                    state

            match commitment with
            | IlMachineStateExecution.CallCommitment.Committed -> state, WhatWeDid.Executed
            | IlMachineStateExecution.CallCommitment.Raised ->
                // The callee raised instead of running; exception dispatch unwinds through this
                // frame, so the stub pointer and arguments below us are the dispatcher's problem,
                // not ours, and this `calli` will not re-execute.
                state, WhatWeDid.Executed
            | IlMachineStateExecution.CallCommitment.SuspendedForClassInit ->
                // A class initialiser is now the active frame and our argument was left on our
                // own frame for the retry to re-pop. But the retry is a fresh execution of this
                // `calli`, which re-derives the argument from the source struct and would find a
                // stale one on top of the stub pointer — where `tryRecognise` expects only
                // `Float` results. Take it back off.
                let frame = IlMachineState.getFrame thread frameId state
                let depthNow = frame.EvaluationStack.Values.Length

                if depthNow <> depthBeforeArgument + 1 then
                    failwith
                        $"%s{operation}: conversion callee suspended for class init having changed the evaluation stack depth by %d{depthNow - depthBeforeArgument} rather than leaving the pushed argument in place; cannot restore the frame for the retry"

                let _, restored = MethodState.popFromStack frame
                IlMachineState.setFrame thread frameId restored state, WhatWeDid.SuspendedForClassInit
        else

        // Every conversion has completed. Only now do we touch the destination, so a retry can
        // never have observed a partially-written image.
        let byteView = byteType operation baseClassTypes state

        let mutable state = state
        let mutable remainingConversions = call.Completed

        for step in plan.Steps do
            let toWrite =
                match step.Kind with
                | StructMarshalFieldKind.CopyBytes -> step.Placement.Field.Contents
                | StructMarshalFieldKind.OADate ->
                    match remainingConversions with
                    | [] ->
                        failwith
                            $"%s{operation}: ran out of conversion results while writing field %s{step.Placement.Field.Name}"
                    | head :: rest ->
                        remainingConversions <- rest
                        CliType.Numeric (CliNumericType.Float64 head)

            // The destination is a raw buffer the guest sized from `Marshal.SizeOf`, so the plan's
            // own total is the only bound we can check against. A step that ran past it would be
            // writing outside what the guest allocated.
            if step.Placement.NativeOffset + step.Placement.NativeSize.Size > plan.NativeSize.Size then
                failwith
                    $"%s{operation}: field %s{step.Placement.Field.Name} would be written at offset %d{step.Placement.NativeOffset} for %d{step.Placement.NativeSize.Size} byte(s), past the %d{plan.NativeSize.Size}-byte unmanaged image of %O{call.TypeHandle}"

            let target =
                ManagedPointerByteView.addByteOffset
                    baseClassTypes
                    state
                    byteView
                    step.Placement.NativeOffset
                    destination

            state <- IlMachineState.writeManagedByrefWithBase baseClassTypes state target toWrite

        if not (List.isEmpty remainingConversions) then
            failwith
                $"%s{operation}: %d{List.length remainingConversions} conversion result(s) were left unconsumed after writing every field"

        finish state
