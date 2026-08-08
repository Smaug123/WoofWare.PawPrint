namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection.Metadata
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

/// One field's contribution to the unmanaged image: where it goes, how it gets there, and the
/// managed value it starts from.
type StructMarshalStep =
    {
        Placement : MarshalFieldPlacement
        Kind : StructMarshalFieldKind
        /// The field's *effective* managed value, resolved through the containing value type
        /// rather than read off `Placement.Field.Contents`.
        ///
        /// The two differ under explicit layout. Assigning one of a set of overlapping fields
        /// deliberately leaves its siblings' stored `Contents` stale — `WithFieldSetById` says so
        /// — and it is `DereferenceFieldById` that resolves which write actually covers a given
        /// range. Marshalling the stored contents would emit the stale sibling.
        Value : CliType
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
/// `delegate*&lt;ref byte, byte*, int, ref CleanupWorkListElement?, void&gt;`. PawPrint has no IL
/// synthesis, but it does not need any: the stub is a `MethodInfo.Synthesised` carrying
/// `RuntimeBehaviour.StructMarshalStub`, so an ordinary `NativeIntSource.FunctionPointer` holds
/// it, `calli` needs no special case, and `AbstractMachine` dispatches it beside the delegate
/// constructor and `Invoke`.
///
/// That the stub has a real frame is what makes the rest of this module straightforward.
/// `MARSHAL_TYPE_DATE` needs `DateTime.ToOADate`, whose behaviour (a zero special case, a VB
/// compatibility fixup, and a guest-visible `OverflowException` below `OADateMinAsTicks`) belongs
/// to the guest's CoreLib and must not be re-derived in the host — so the stub calls the guest's
/// own `StubHelpers.DateMarshaler.ConvertToNative`, once per conversion field, by pushing it as a
/// callee and not returning its own frame. The result lands on the stub's *own* evaluation stack,
/// which starts empty and which nothing else writes to, so counting the results is reading our own
/// state rather than inferring anything about the caller's. Nothing is written to the destination
/// until every conversion has completed, so a resumption never observes a half-written image.
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
        // `NativeInt` cells carry provenance under PawPrint (e.g. a pointer from
        // `Marshal.AllocHGlobal`, or `TypeHandlePtr` from `typeof(T).TypeHandle.Value`). CoreCLR
        // memmoves the integer-width bits regardless; PawPrint cannot, because
        // `CliNumericType.ToBytes` refuses to serialise provenance. We accept `IntPtr`/`UIntPtr`
        // anyway because neither caller flattens such a cell to bytes: the blittable arm returns a
        // null stub, so CoreLib's `SpanHelpers.Memmove` is intercepted and routed through
        // `CellAwareMemOps.copy`; and the stub path reads the source struct structurally
        // (`readSource`) and writes each field as a typed value, so a pointer cell survives into
        // the destination intact. What a guest cannot then do is *read the destination back*
        // through a byte view — `Marshal.ReadIntPtr` over such a cell is refused by
        // `executeLdind` (#801) — but that is a gap in reading native memory, not in classifying
        // fields.
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
        : Result<StructMarshalPlan, MarshalSizeError>
        =
        match value with
        | CliType.ValueType vt ->
            match vt._Storage with
            | CliValueTypeStorage.RawBytes _ ->
                // `TryComputeMarshalLayout` reports no placements for raw-byte storage because
                // there are no declared fields — which is not the same as "nothing to write".
                MarshalSizeError.NotImplemented
                    "the type has raw-byte storage rather than declared fields, so there are no per-field placements to marshal"
                |> Result.Error
            | CliValueTypeStorage.Fields _ ->

            match CliValueType.TryComputeMarshalLayout concreteTypes assemblies corelib vt with
            | Result.Error err -> Result.Error err
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
                    // Known limitation (#802): when two or more fields overlap this range,
                    // `DereferenceFieldById` answers by byte-rendering the storage, which a
                    // pointer cell has no rendering for. So an overlapped `IntPtr` field throws
                    // from here rather than marshalling. The single-covering-field case — every
                    // sequential layout, and most explicit ones — returns the cell directly and
                    // is unaffected.
                    let contents = CliValueType.DereferenceFieldById placement.Field.Id vt

                    // A `[MarshalAs]` descriptor selects the field's native type, and CoreCLR
                    // rejects most pairings outright: `Int32` admits only `I4`/`U4`
                    // (`IDS_EE_BADMARSHAL_*`), and a `DateTime` field admits only DEFAULT and
                    // STRUCT (mlinfo.cpp:1747-1754, `IDS_EE_BADMARSHAL_DATETIME`). Width alone
                    // does not separate the legal pairings from the illegal ones —
                    // `[MarshalAs(UnmanagedType.R4)] int` is four bytes either way, and
                    // `[MarshalAs(UnmanagedType.I8)] DateTime` is eight — so classifying on
                    // width would silently marshal types the real runtime refuses to load.
                    // Refuse the lot until a motivating test makes it worth modelling which
                    // pairings CoreCLR accepts and what each one writes.
                    if placement.Field.MarshallingDescriptor.IsSome then
                        MarshalSizeError.NotImplemented
                            $"field %s{placement.Field.Name} carries a [MarshalAs] descriptor, and PawPrint does not model which native types a field's declared type may legally pair with, nor what each pairing writes"
                        |> Result.Error

                    // Necessary condition on top of the shape restriction above: a field
                    // whose native width differs from its managed one cannot be written by
                    // copying the managed value, whatever its interior looks like.
                    else if isCopyableVerbatim contents then
                        let managedSize = CliType.SizeOf contents

                        if managedSize.Size <> placement.NativeSize.Size then
                            MarshalSizeError.NotImplemented
                                $"field %s{placement.Field.Name} occupies %d{managedSize.Size} managed byte(s) but %d{placement.NativeSize.Size} native one(s), so its managed image cannot be copied verbatim"
                            |> Result.Error
                        else
                            Result.Ok
                                {
                                    Placement = placement
                                    Kind = StructMarshalFieldKind.CopyBytes
                                    // Unwrap primitive-like wrappers. The native image has no
                                    // notion of `System.IntPtr`-the-struct — it holds a
                                    // pointer-sized value — and writing the wrapper installs a
                                    // value-type cell in native memory whose `_value` field
                                    // carries the provenance. `Marshal.ReadIntPtr` then takes a
                                    // byte view over that cell and is refused, where the same
                                    // buffer written by `Marshal.WriteIntPtr` reads back fine.
                                    // Composites reach this arm only when they are primitive-like
                                    // (see `isCopyableVerbatim`), so this unwraps exactly the
                                    // cases that need it.
                                    Value = CliType.unwrapPrimitiveLikeDeep contents
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
                                Value = contents
                            }
                    | _ when isBlittableField concreteTypes assemblies corelib contents ->
                        MarshalSizeError.NotImplemented
                            $"field %s{placement.Field.Name} is a nested composite whose fields are individually blittable, but writing it verbatim would assume its managed and unmanaged interiors coincide; that needs a recursive marshal plan"
                        |> Result.Error
                    | _ ->
                        MarshalSizeError.NotImplemented
                            $"field %s{placement.Field.Name} is neither blittable nor a marshalling case PawPrint implements (contents %O{contents})"
                        |> Result.Error
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
                                | Result.Error err -> failwith $"unreachable: %s{err.Reason}"
                            )
                    }
        | _ ->
            MarshalSizeError.NotImplemented $"only value types have a struct-marshal plan; got %O{value}"
            |> Result.Error

    /// The synthesised method that *is* a type's struct-marshal stub.
    ///
    /// CoreCLR builds a real `MethodDesc` over synthesised IL here (`CreateStructMarshalILStub`,
    /// dllimport.cpp:5289) and hands CoreLib its entry address. This is PawPrint's equivalent: a
    /// `MethodInfo.Synthesised` carrying `RuntimeBehaviour.StructMarshalStub`, which
    /// `NativeIntSource.FunctionPointer` can hold like any other function pointer, so `calli`
    /// needs no special case and the stub gets an ordinary frame — its own evaluation stack, its
    /// own locals, and the existing re-entry machinery.
    ///
    /// The declaring type is the type being marshalled. That gives the stub a real
    /// `ConcreteTypeHandle` (so `loadClass` and every diagnostic have something true to say) and
    /// makes its identity per-marshalled-type, which is exactly the per-MethodTable identity
    /// CoreCLR's stub cache has: `MethodInfo.NominallyEqual` compares declaring type plus
    /// synthesised kind, so two stubs for one type are one method.
    let synthesise
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (typeHandle : ConcreteTypeHandle)
        : MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        let declaringType =
            AllConcreteTypes.lookup typeHandle state.ConcreteTypes
            |> Option.defaultWith (fun () ->
                failwith $"%s{operation}: type %O{typeHandle} is not registered in AllConcreteTypes"
            )

        let handleOf (ty : TypeInfo<GenericParamFromMetadata, TypeDefn>) : ConcreteTypeHandle =
            AllConcreteTypes.findExistingNonGenericConcreteType state.ConcreteTypes ty.Identity
            |> Option.defaultWith (fun () -> failwith $"%s{operation}: %s{ty.Name} is not concretized")

        // CoreLib invokes the stub through
        // `delegate*<ref byte, byte*, int, ref CleanupWorkListElement?, void>`. These types are
        // load-bearing rather than decorative: `callMethod` coerces each popped argument to the
        // zero of its declared parameter type, so declaring the byrefs as `IntPtr` would deliver
        // them wrapped in the `System.IntPtr` struct and every consumer would have to unwrap.
        // `Byref`/`Pointer` handles need no registration in `AllConcreteTypes` — they are
        // structural — so there is no reason to approximate.
        let byteHandle = handleOf baseClassTypes.Byte
        let refByte = ConcreteTypeHandle.Byref byteHandle
        let bytePtr = ConcreteTypeHandle.Pointer byteHandle
        let int32Handle = handleOf baseClassTypes.Int32

        // The cleanup work list is a reference type the stub only ever passes along, and PawPrint
        // supports no field kind that registers cleanup work, so its element type is never
        // dereferenced. Declare it as a byref to `System.Object`: right shape, and honest that we
        // do not model `CleanupWorkListElement` itself.
        let refCleanup = ConcreteTypeHandle.Byref (handleOf baseClassTypes.Object)

        MethodInfo.Synthesised (
            {
                DeclaringType = declaringType
                Name = "<StructMarshalStub>"
                Body = MethodBody.RuntimeProvided RuntimeBehaviour.StructMarshalStub
                Generics = ImmutableArray.Empty
                Signature =
                    {
                        Header =
                            ComparableSignatureHeader.Make (
                                SignatureHeader (
                                    SignatureKind.Method,
                                    SignatureCallingConvention.Default,
                                    SignatureAttributes.None
                                )
                            )
                        ParameterTypes = [ refByte ; bytePtr ; int32Handle ; refCleanup ]
                        GenericParameterCount = 0
                        RequiredParameterCount = 4
                        ReturnType = MethodReturnType.Void
                    }
                IsStatic = true
            },
            SynthesisedMethod.StructMarshalStub
        )

    /// `MarshalOperation` (stubgen.h:26): the third argument CoreLib passes to the stub.
    [<RequireQualifiedAccess>]
    type private Operation =
        | Marshal
        | Unmarshal
        | Cleanup

    /// The conversion results a part-way-through stub invocation has accumulated.
    ///
    /// Each conversion the stub needs pushes a managed callee and leaves the program counter put,
    /// so the stub is re-entered when that callee returns and its result is sitting on the stub's
    /// *own* evaluation stack. Counting them is therefore reading our own frame, not inferring
    /// anything about somebody else's: the stub frame starts empty and nothing but this code ever
    /// pushes to it. That is the same marker idiom `NativeRuntimeTypeQCall` uses, and it is sound
    /// here for the same reason — an owned, initially empty stack.
    /// A conversion helper that returns *void* is not handled by this counting discipline: it
    /// pushes nothing, the count never advances, and the stub would re-enter forever. CoreLib has
    /// several such marshallers (`CSTRMarshaler.ConvertFixedToNative`, `FixedWSTRMarshaler`), so
    /// this will need addressing before any of them is implemented. What the owned frame buys is
    /// that the fix is *available* — push a sentinel of our own alongside or instead of a result,
    /// since writing bookkeeping to our own stack corrupts nobody — where a design counting values
    /// on the caller's stack could not have done so at all. Admitting the fix and implementing it
    /// are different things, and only the first is done here.
    ///
    /// The theorem this rests on, stated once because it is the load-bearing half of the argument:
    /// result *i* is attributed to conversion *i* of a plan that is **recomputed on every pass**,
    /// so the attribution is only sound while the plan's step order is a deterministic function of
    /// inputs that cannot change between passes. It is — the order comes from the type's field
    /// layout, which further assembly loads and concretizations can only extend and never reorder,
    /// and from the source box, which nothing but the guest's own code can reach and which no
    /// conversion helper is given a reference to. A future step order derived from anything a
    /// callee could perturb would break this without breaking any test that runs today.
    let private completedConversions (frame : MethodState) : float list =
        frame.EvaluationStack.Values
        |> List.map (fun v ->
            match v with
            | EvalStackValue.Float f -> f
            | other ->
                failwith
                    $"struct-marshal stub: expected only conversion results on the stub's own evaluation stack, found %O{other}"
        )
        |> List.rev

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
        : IlMachineState * MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> * ConcreteTypeHandle
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

        let state, concretized, declaringTypeHandle =
            ExecutionConcretization.concretizeMethodWithAllGenerics
                loggerFactory
                baseClassTypes
                ImmutableArray.Empty
                method
                ImmutableArray.Empty
                state

        state, concretized, declaringTypeHandle

    /// Read the struct the stub is to marshal, given the byref CoreLib passed as the stub's first
    /// argument.
    ///
    /// That byref is always `RuntimeHelpers.GetRawData(box)` — `Marshal.StructureToPtr` boxes its
    /// argument and hands the stub a `ref byte` onto the box's payload (Marshal.CoreCLR.cs:264,
    /// :275) — so the value wanted is the boxed payload itself, read *structurally*.
    ///
    /// The distinction is load-bearing rather than stylistic. Reading through the `ref byte` view
    /// flattens every cell to bytes, and a struct may legally hold a value that has no byte
    /// rendering: an `IntPtr` field assigned from `Marshal.AllocHGlobal` is a managed pointer with
    /// provenance, which `CliNumericType.ToBytes` refuses. Such a struct marshals fine — the
    /// destination write preserves the pointer cell — but only if the read does not destroy it
    /// first. Only the *destination* is bytes; the source is a value.
    ///
    /// Any other byref shape falls back to the byte-image read. Nothing produces one today; the
    /// fallback exists so an unforeseen shape gets a typed read rather than a match failure.
    let private readSource
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (typeHandle : ConcreteTypeHandle)
        (source : ManagedPointerSource)
        (state : IlMachineState)
        : CliType
        =
        let addressesWholeValue (projections : ByrefProjection list) : bool =
            // A whole-value view is the empty chain, or a type view over the whole payload —
            // optionally with an explicit zero byte offset, which is the same address.
            match List.rev projections with
            | []
            | [ ByrefProjection.ReinterpretAs _ ]
            | [ ByrefProjection.ByteOffset 0 ; ByrefProjection.ReinterpretAs _ ] -> true
            | _ -> false

        match source with
        | ManagedPointerSource.Byref (ByrefRoot.HeapValue addr, projections) when addressesWholeValue projections ->
            CliType.ValueType (ManagedHeap.get addr state.ManagedHeap).Contents
        | _ ->
            let template, _ = IlMachineState.cliTypeZeroOfHandle state baseClassTypes typeHandle

            IlMachineState.readManagedByrefBytesAs baseClassTypes state source template

    /// Execute (or continue executing) a `calli` whose target is a struct-marshal stub. The
    /// caller has already established, via `tryRecognise`, that the stub pointer is on the eval
    /// stack under `call.Completed.Length` conversion results.
    ///
    /// Returns with the program counter *unadvanced* whenever a conversion callee was pushed, so
    /// the same `calli` runs again once that callee returns.
    /// Run (or resume) a struct-marshal stub frame.
    ///
    /// This is dispatched from `AbstractMachine.executeOneStep` exactly as the delegate
    /// constructor and `Invoke` are, so by the time it runs the stub has an ordinary frame: its
    /// four arguments are in `Arguments`, its evaluation stack is its own, and returning is
    /// `returnStackFrame`.
    ///
    /// A conversion field is handled by pushing the guest's own marshaller as a callee and
    /// *not* returning the frame. The dispatch loop runs the callee, whose result lands on this
    /// frame's evaluation stack, and re-enters here on a later step with one more result in hand.
    /// Nothing is written to the destination until every conversion has completed, so a resumption
    /// never observes a half-written image.
    let executeStubCall
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (instruction : MethodState)
        (state : IlMachineState)
        : ExecutionResult
        =
        let operation = "struct-marshal stub"

        let typeHandle =
            AllConcreteTypes.findExistingConcreteType
                state.ConcreteTypes
                instruction.ExecutingMethod.DeclaringType.Identity
                instruction.ExecutingMethod.DeclaringType.Generics
            |> Option.defaultWith (fun () ->
                failwith
                    $"%s{operation}: declaring type %O{instruction.ExecutingMethod.DeclaringType} is not registered in AllConcreteTypes"
            )

        let frameId = state.ThreadState.[thread].ActiveMethodState
        let completed = completedConversions (IlMachineState.getFrame thread frameId state)
        let completedCount = List.length completed

        if instruction.Arguments.Length <> 4 then
            failwith
                $"%s{operation}: expected the four arguments of `delegate*<ref byte, byte*, int, ref CleanupWorkListElement?, void>`, got %d{instruction.Arguments.Length}"

        let op = operationOf operation (EvalStackValue.ofCliType instruction.Arguments.[2])

        let destination =
            managedPointerOf operation "the destination pointer" (EvalStackValue.ofCliType instruction.Arguments.[1])

        let source =
            managedPointerOf operation "the source reference" (EvalStackValue.ofCliType instruction.Arguments.[0])

        /// The stub's work is done: drop the conversion results it accumulated, pop its frame, and
        /// hand control back to CoreLib.
        ///
        /// The results have to go first. They are this frame's scratch state, and the stub returns
        /// void, so leaving them would trip `returnStackFrame`'s check that a void method returns
        /// an empty stack — which is the check doing its job, since anything left there would
        /// otherwise land on CoreLib's stack.
        let finish (state : IlMachineState) : ExecutionResult =
            let mutable state = state

            for _ in 1..completedCount do
                let _, next = IlMachineState.popEvalStack thread state
                state <- next

            match IlMachineState.returnStackFrame loggerFactory baseClassTypes thread state with
            | ReturnFrameResult.NormalReturn state -> ExecutionResult.stepped (state, WhatWeDid.Executed)
            | result -> failwith $"%s{operation}: unexpected ReturnFrameResult returning from stub frame: %A{result}"

        /// The plan for the stub's type, derived from `value`. `nativeSizeOnly` callers pass the
        /// type's zero because they need only the total.
        let planFor (value : CliType) : StructMarshalPlan =
            match tryComputePlan state.ConcreteTypes state._LoadedAssemblies baseClassTypes value with
            | Result.Ok plan -> plan
            | Result.Error reason ->
                // Unreachable in practice: the QCall only mints a stub pointer for a type whose
                // plan it has already computed, from that type's zero value. Reaching it means
                // the plan is not a function of the type alone, which would be a bug worth
                // seeing.
                failwith
                    $"%s{operation}: type %O{typeHandle} has no marshal plan at stub-execution time, though the QCall minted a stub for it: %s{reason.Reason}"

        let byteView = byteType operation baseClassTypes state

        /// The address of byte `nativeOffset` of the destination, as a byte-view byref — the same
        /// shape the guest's own pointer arithmetic produces before a `stind`.
        let addressOf (nativeOffset : int) (state : IlMachineState) : ManagedPointerSource =
            ManagedPointerByteView.addByteOffset baseClassTypes state byteView nativeOffset destination

        let writeAt (nativeOffset : int) (value : CliType) (state : IlMachineState) : IlMachineState =
            IlMachineState.writeManagedByrefWithBase baseClassTypes state (addressOf nativeOffset state) value

        /// Write the unmanaged image: each step contributes its native value at its placement, and
        /// every byte not covered by a step is zeroed.
        ///
        /// The gaps have to be zeroed because CoreCLR's stub does the equivalent with `initblk`
        /// over the whole image — at the top of its Marshal stream (dllimport.cpp:1290, "so we can
        /// do a partial cleanup if marshalling fails") and at the end of its Cleanup stream
        /// (:1319) — and a guest reading the buffer can see the difference.
        ///
        /// It is the *field* ranges that cannot simply be byte-zeroed alongside them.
        /// `CellAwareMemOps.clear` byte-walks a `NativeMemoryByte` root by design — for byte
        /// storage the byte walk is the modelled access shape — so it cannot overwrite a cell that
        /// has no byte rendering, and a destination reused after a previous marshal holds exactly
        /// such a cell whenever the struct has a pointer field. Writing each field slot as a typed
        /// value replaces the cell wholesale, which both clears it and is what the marshal has to
        /// do anyway.
        ///
        /// Known limitation (#801): that reasoning holds only while the gaps are gaps in *both*
        /// the old and the new contents of the buffer. Reuse one buffer for two different struct
        /// layouts — `{ IntPtr; DateTime }` then `{ int; DateTime }` — and the second layout's
        /// padding at bytes 4..7 lands inside the first's pointer cell, where the gap clear
        /// byte-walks into it and throws. CoreCLR's `initblk` has no notion of cells and so has no
        /// such trouble; closing it needs a "drop any cells overlapping this range" operation on
        /// the native memory pool, which does not exist yet.
        let writeImage
            (plan : StructMarshalPlan)
            (valueFor : StructMarshalStep -> CliType)
            (state : IlMachineState)
            : IlMachineState
            =
            let mutable state = state
            let mutable cursor = 0

            // Placements ascend for sequential layout but not for explicit layout, and explicit
            // fields may overlap; ordering by offset makes the gap walk correct for both, and
            // `max` keeps an overlapped range from being re-zeroed as if it were a gap.
            for step in plan.Steps |> List.sortBy (fun s -> s.Placement.NativeOffset) do
                let offset = step.Placement.NativeOffset

                if offset > cursor then
                    state <-
                        CellAwareMemOps.clear baseClassTypes operation state (addressOf cursor state) (offset - cursor)

                cursor <- max cursor (offset + step.Placement.NativeSize.Size)

            if cursor < plan.NativeSize.Size then
                state <-
                    CellAwareMemOps.clear
                        baseClassTypes
                        operation
                        state
                        (addressOf cursor state)
                        (plan.NativeSize.Size - cursor)

            // Fields last, and in declaration order: under explicit layout two fields may cover
            // the same bytes, and CoreCLR marshals them in declaration order, so the later one
            // wins.
            for step in plan.Steps do
                state <- writeAt step.Placement.NativeOffset (valueFor step) state

            state

        /// The zero of a step's *native* form. Not the zero of its managed form: an `OADate`
        /// step's native cell is a `double`, and installing a zeroed `DateTime` there would leave
        /// the destination holding a managed-shaped cell.
        let nativeZero (step : StructMarshalStep) : CliType =
            match step.Kind with
            | StructMarshalFieldKind.OADate -> CliType.Numeric (CliNumericType.Float64 0.0)
            | StructMarshalFieldKind.CopyBytes -> CliType.ZeroLike step.Value

        let clearImage (plan : StructMarshalPlan) (state : IlMachineState) : IlMachineState =
            writeImage plan nativeZero state

        match op with
        | Operation.Cleanup ->
            // CoreLib calls the stub with `Cleanup` before `Marshal` when `fDeleteOld` is set, to
            // release whatever the previous contents owned. Every field kind we support owns
            // nothing — a copied value and an OADate double both live entirely inside the
            // destination buffer — so there is nothing to release, and a field kind that *did*
            // own native memory (a `ByValTStr`, an allocated array) would have to release it
            // here; `tryComputePlan` refuses every such kind today.
            //
            // The zeroing, though, is not optional: CoreCLR clears the native image after the
            // per-field release "so we don't leave anything dangling", and a guest calling
            // `DestroyStructure` and then reading the buffer sees that. The type's zero value is
            // enough here, because only the plan's total size is wanted.
            let zero, state = IlMachineState.cliTypeZeroOfHandle state baseClassTypes typeHandle

            finish (clearImage (planFor zero) state)
        | Operation.Unmarshal ->
            failwith
                $"TODO %s{operation}: native-to-managed direction (MarshalOperation.Unmarshal, reached via Marshal.PtrToStructure) is not implemented for type %O{typeHandle}"
        | Operation.Marshal ->

        let sourceValue = readSource operation baseClassTypes typeHandle source state

        match sourceValue with
        | CliType.ValueType vt when vt.Declared = typeHandle -> ()
        | other ->
            failwith
                $"%s{operation}: expected the source reference to address a value of type %O{typeHandle}, which is the type the stub was minted for, but read %O{other}"

        let plan = planFor sourceValue

        // CoreCLR opens its Marshal stream by zeroing the whole native image (dllimport.cpp:1290,
        // "so we can do a partial cleanup if marshalling fails"), and only then writes fields. We
        // must too, or padding — bytes 12..15 of `{DateTime; int; DateTime}`, say — keeps whatever
        // the guest's buffer held before, which a guest reading the image can see.
        //
        // This runs on *every* pass of the re-executing `calli`, not just the first. Zeroing is
        // idempotent and nothing else can write to the buffer in between, so the repetition costs
        // only time — and it is what makes a throwing conversion agree with CoreCLR. There, a
        // conversion that throws part way lands in the catch trampoline, which branches to the
        // Cleanup stream and re-zeroes (dllimport.cpp:1319); here, the buffer was zeroed on the
        // pass that pushed the conversion and we never write on the pass that throws. Both leave
        // a zeroed image, including when the guest handed us a dirty buffer.
        let state = clearImage plan state

        let conversions =
            plan.Steps
            |> List.filter (fun step ->
                match step.Kind with
                | StructMarshalFieldKind.OADate -> true
                | StructMarshalFieldKind.CopyBytes -> false
            )

        if completedCount > List.length conversions then
            failwith
                $"%s{operation}: %d{completedCount} conversion result(s) are on the evaluation stack but type %O{typeHandle} has only %d{List.length conversions} conversion field(s)"

        if completedCount < List.length conversions then
            // Convert the next field by calling the guest's own marshaller. We do not return this
            // frame, so the dispatch loop runs the callee and then re-enters us with its result on
            // our evaluation stack.
            let next = conversions.[completedCount]

            let state, convertToNative, convertToNativeDeclaringType =
                dateConvertToNative operation loggerFactory baseClassTypes state

            // Run the helper's class initialiser first, exactly as every other call site does.
            // `callMethodWithCommitment` cannot do it for us: its `SuspendedForClassInit` outcome
            // comes only from the `Activator.CreateInstance<T>()` intrinsic, so a plain static
            // CoreLib method would otherwise be entered with its statics uninitialised, and the
            // cross-thread `Blocked` protocol would be bypassed. `DateMarshaler` has no `.cctor`
            // today, which is why this is latent; a future conversion helper's might.
            //
            // Nothing has been pushed yet, and we do not return the frame on any of these
            // outcomes, so we are simply re-entered and re-derive everything.
            match
                IlMachineStateExecution.loadClass loggerFactory baseClassTypes convertToNativeDeclaringType thread state
            with
            | FirstLoadThis state -> ExecutionResult.stepped (state, WhatWeDid.SuspendedForClassInit)
            | ThrowingTypeInitializationException state ->
                ExecutionResult.stepped (state, WhatWeDid.ThrowingTypeInitializationException)
            | Blocked (state, blockedBy) -> ExecutionResult.stepped (state, WhatWeDid.BlockedOnClassInit blockedBy)
            | NothingToDo state ->

            let state = IlMachineState.pushToEvalStack next.Value thread state
            let threadState = state.ThreadState.[thread]

            let state, commitment =
                IlMachineStateExecution.callMethodWithCommitment
                    loggerFactory
                    baseClassTypes
                    None
                    ConstructionState.NotConstructing
                    false
                    false
                    false // this frame has no program counter to advance
                    convertToNative.Generics
                    convertToNative
                    thread
                    threadState
                    None
                    ConstructedObjectDisposition.PushToCaller
                    false // wrapExceptionInTargetInvocation
                    state

            match commitment with
            | IlMachineStateExecution.CallCommitment.Committed
            | IlMachineStateExecution.CallCommitment.Raised ->
                // Either a callee frame or an exception constructor is now on top of us; in both
                // cases our frame stays put and the dispatch loop takes it from here.
                ExecutionResult.stepped (state, WhatWeDid.SuspendedForManagedCall)
            | IlMachineStateExecution.CallCommitment.SuspendedForClassInit ->
                // Unreachable: `loadClass` above has already run the callee's initialiser, and the
                // only other producer of this outcome is the `Activator.CreateInstance<T>()`
                // intrinsic, which no conversion helper is. If it ever fires, the argument we just
                // pushed is stranded on our evaluation stack where `completedConversions` expects
                // only results, so say so rather than silently resuming into a corrupt frame.
                failwith
                    $"%s{operation}: conversion callee %O{convertToNative} suspended for class initialisation after loadClass reported nothing to do"
        else

        // Every conversion has completed. Only now do we touch the destination with real values,
        // so a retry can never have observed a partially-written image.
        for step in plan.Steps do
            // The destination is a raw buffer the guest sized from `Marshal.SizeOf`, so the plan's
            // own total is the only bound we can check against. A step that ran past it would be
            // writing outside what the guest allocated.
            if step.Placement.NativeOffset + step.Placement.NativeSize.Size > plan.NativeSize.Size then
                failwith
                    $"%s{operation}: field %s{step.Placement.Field.Name} would be written at offset %d{step.Placement.NativeOffset} for %d{step.Placement.NativeSize.Size} byte(s), past the %d{plan.NativeSize.Size}-byte unmanaged image of %O{typeHandle}"

        // Conversion results are consumed in plan order, which is the order they were requested.
        let mutable remainingConversions = completed

        let valueFor (step : StructMarshalStep) : CliType =
            match step.Kind with
            | StructMarshalFieldKind.CopyBytes -> step.Value
            | StructMarshalFieldKind.OADate ->
                match remainingConversions with
                | [] ->
                    failwith
                        $"%s{operation}: ran out of conversion results while writing field %s{step.Placement.Field.Name}"
                | head :: rest ->
                    remainingConversions <- rest
                    CliType.Numeric (CliNumericType.Float64 head)

        let state = writeImage plan valueFor state

        if not (List.isEmpty remainingConversions) then
            failwith
                $"%s{operation}: %d{List.length remainingConversions} conversion result(s) were left unconsumed after writing every field"

        finish state
