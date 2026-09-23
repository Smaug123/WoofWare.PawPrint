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
    /// (ilmarshalers.cpp:1241), which calls managed `StubHelpers.DateMarshaler.ConvertToNative`;
    /// its inverse (ilmarshalers.cpp:1251) calls `DateMarshaler.ConvertToManaged` and constructs the
    /// field with `DateTime(long ticks)`, so a round trip keeps only whole milliseconds and always
    /// comes back with `DateTimeKind.Unspecified`.
    | OADate
    /// CoreCLR's `MARSHAL_TYPE_WINBOOL`: a `bool` field becomes a four-byte `BOOL`, 1 for true and
    /// 0 for false whatever non-zero byte the managed `bool` holds, and any non-zero `BOOL` comes
    /// back as a `bool` holding 1. `ILBoolMarshaler` (ilmarshalers.cpp:187) does both with `ceq`.
    | WinBool
    /// CoreCLR's `MARSHAL_TYPE_CBOOL`: as `WinBool`, but the native form is a single byte.
    | CBool
    /// CoreCLR's `MARSHAL_TYPE_ANSICHAR`: a `char` field becomes one byte, by
    /// `StubHelpers.AnsiCharMarshaler.ConvertToNative(char, bestFit, throwOnUnmappableChar)` on
    /// the way out and `ConvertToManaged(byte)` on the way back (ilmarshalers.cpp:1448). Both are
    /// the guest CoreLib's own code; the flags are the struct's `BestFitMappingAttribute`, as
    /// `StructMarshalStub.bestFitFlags` reads it.
    | AnsiChar of bestFit : bool * throwOnUnmappableChar : bool

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
/// `RuntimeBehaviour.StructMarshalStub`, so an ordinary
/// `NativeIntSource.FunctionPointer (FunctionPointerTarget.Managed …)` holds it, `calli` needs no
/// special case, and `AbstractMachine` dispatches it beside the delegate constructor and `Invoke`.
///
/// `MARSHAL_TYPE_DATE` needs `DateTime.ToOADate` one way and `DateTime.DoubleDateToTicks` and
/// `DateTime(long)` the other, whose behaviour (a zero special case, a VB compatibility fixup, and
/// guest-visible exceptions for dates outside the OLE Automation range) belongs to the guest's
/// CoreLib — so the stub calls the guest's own `StubHelpers.DateMarshaler` and `DateTime`
/// constructor, once per conversion field, by pushing each as a callee and not returning its own
/// frame; the result lands on the stub's own evaluation stack. `MARSHAL_TYPE_ANSICHAR` is the same:
/// what a `char` becomes as an ANSI byte, and back, is the guest CoreLib's
/// `StubHelpers.AnsiCharMarshaler`, not something the stub decides.
[<RequireQualifiedAccess>]
module StructMarshalStub =

    /// Whether a field's managed byte image is also its native image, i.e. whether CoreCLR's
    /// `IsFieldBlittable` would accept it. `charSet` is the declaring type's and `descriptor` the
    /// field's own `[MarshalAs]`, which between them decide whether a `char` field is a UTF-16
    /// code unit (blittable) or an ANSI byte (not). The descriptor is consulted for `bool` and
    /// `char` fields only: a `[MarshalAs]` that CoreCLR would refuse on a numeric or struct field
    /// is not caught here.
    ///
    /// Shared by the blittable arm of `MarshalNative_TryGetStructMarshalStub` (which needs the
    /// bare yes/no) and by `tryComputePlan` (which needs it per field). One recursion, so the two
    /// cannot drift: a field the QCall's fast path accepts is exactly a field the stub would
    /// copy verbatim.
    let rec isBlittableField
        (concreteTypes : AllConcreteTypes)
        (assemblies : LoadedAssemblies)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (charSet : System.Runtime.InteropServices.CharSet)
        (descriptor : FieldMarshalDescriptor option)
        (t : CliType)
        : bool
        =
        // `GetNestedFieldFlags` judges an enum field by its underlying element type
        // (classlayoutinfo.cpp:445), under the *containing* type's `CharSet`: an enum over `char`
        // is a `char` field, not a struct with a `CharSet` of its own.
        match CliValueType.TryEnumUnderlying concreteTypes assemblies corelib t with
        | Some (_, underlying) -> isBlittableField concreteTypes assemblies corelib charSet descriptor underlying
        | None ->

        match t with
        | CliType.Bool _
        | CliType.Char _ ->
            CliValueType.TryBoolCharFieldMarshal charSet descriptor t = Some (Result.Ok BoolCharMarshal.Utf16Char)
        // `NativeInt` cells carry provenance under PawPrint (e.g. a pointer from
        // `Marshal.AllocHGlobal`, or `TypeHandlePtr` from `typeof(T).TypeHandle.Value`). CoreCLR
        // memmoves the integer-width bits regardless; PawPrint cannot, because
        // `CliNumericType.ToBytes` refuses to serialise provenance. `IntPtr`/`UIntPtr` are
        // accepted because neither caller flattens such a cell to bytes: the blittable arm
        // returns a null stub, so CoreLib's `SpanHelpers.Memmove` is intercepted and routed
        // through `CellAwareMemOps.copy`; and the stub path reads the source struct structurally
        // (`readSource`) and writes each field as a typed value, so a pointer cell survives into
        // the destination intact. Reading the destination back through a byte view
        // (`Marshal.ReadIntPtr`) is refused by `executeLdind` (#801).
        | CliType.Numeric (CliNumericType.NativeInt _) -> true
        | CliType.Numeric _ -> true
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

            // Decimal is structurally `{ int; uint; ulong }` and would otherwise recurse to true,
            // but CoreCLR's `IsFieldBlittable` rejects a Decimal field unconditionally
            // (fieldmarshaler.cpp:266): managed `System.Decimal`'s alignment need not match
            // native `DECIMAL`'s, so the enclosing struct's managed layout cannot stand in for its
            // native one. `tryComputePlan` copies the Decimal itself verbatim instead, at the
            // offset the native layout walk gives it.
            let isDecimal = CliValueType.IsHostKnownDecimal concreteTypes assemblies corelib vt

            if isDateTime || isDecimal then
                false
            else
                match vt._Storage with
                // RawBytes-backed value types are not the typical struct-with-fields shape;
                // conservatively reject so we don't quietly accept primitive wrappers whose
                // CoreCLR marshal size diverges from the byte image.
                | CliValueTypeStorage.RawBytes _ -> false
                | CliValueTypeStorage.Fields storage -> areFieldsBlittable concreteTypes assemblies corelib vt storage

    /// Whether every field of `vt` is blittable, each judged under `vt`'s own `CharSet`: CoreCLR
    /// computes a nested struct's blittability once, for its own `MethodTable`.
    and private areFieldsBlittable
        (concreteTypes : AllConcreteTypes)
        (assemblies : LoadedAssemblies)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (vt : CliValueType)
        (storage : CliFieldBackedStorage)
        : bool
        =
        storage.Fields
        |> List.forall (fun field ->
            isBlittableField concreteTypes assemblies corelib vt.CharSet field.MarshallingDescriptor field.Contents
        )

    /// Whether the whole struct is blittable, i.e. whether CoreCLR's `th.IsBlittable()` arm of
    /// `MarshalNative_TryGetStructMarshalStub` applies and the guest can memmove.
    ///
    /// Walks the outer struct's fields via `isBlittableField`. The host-known field-only
    /// rejections (Decimal) do not apply to the outer type's own declared type; a top-level
    /// DateTime is filtered earlier by the AutoLayout gate.
    let isBlittableStruct
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
            | CliValueTypeStorage.Fields storage -> areFieldsBlittable concreteTypes assemblies corelib vt storage
        // A top-level primitive (e.g. `Marshal.StructureToPtr<int>`). A number's image is its own
        // native image. A top-level `bool` or `char` has no containing type to take a `CharSet`
        // from, and is not modelled: calling it non-blittable sends it to `tryComputePlan`, which
        // refuses anything but a value type.
        | CliType.Numeric _ -> true
        | CliType.Bool _
        | CliType.Char _
        | CliType.ObjectRef _
        | CliType.RuntimePointer _ -> false

    /// The `bestFit` and `throwOnUnmappableChar` flags CoreCLR builds `typeHandle`'s struct stub
    /// with (`CreateStructMarshalILStub`, dllimport.cpp:5312): `ReadBestFitCustomAttribute`
    /// (interoputil.cpp:817), where a `BestFitMappingAttribute` on the type overrides one on its
    /// assembly, and with neither best-fit mapping is on and throwing is off.
    let bestFitFlags
        (concreteTypes : AllConcreteTypes)
        (assemblies : LoadedAssemblies)
        (typeHandle : ConcreteTypeHandle)
        : bool * bool
        =
        let concreteType =
            AllConcreteTypes.lookup typeHandle concreteTypes
            |> Option.defaultWith (fun () ->
                failwith $"StructMarshalStub.bestFitFlags: %O{typeHandle} is not a registered concrete type"
            )

        let assembly = assemblies.ByDefinitionName concreteType.AssemblyFullName

        let mr =
            System.Reflection.Metadata.PEReaderExtensions.GetMetadataReader assembly.PeReader

        let describe () = $"type %O{typeHandle}"

        let readFrom
            (attributes : System.Reflection.Metadata.CustomAttributeHandleCollection)
            (flags : bool * bool)
            : bool * bool
            =
            let blob =
                attributes
                |> Seq.map mr.GetCustomAttribute
                |> Seq.tryFind (fun attr ->
                    CustomAttribute.constructorParentName mr describe attr.Constructor = Some (
                        "System.Runtime.InteropServices",
                        "BestFitMappingAttribute"
                    )
                )
                |> Option.map (fun attr -> mr.GetBlobBytes attr.Value)

            // CoreCLR reads the blob at fixed offsets rather than parsing it: the constructor's
            // `bool` follows the two-byte prolog, and a blob exactly long enough to carry the
            // `ThrowOnUnmappableChar` named argument ends with that argument's value.
            match blob with
            | Some bytes when bytes.Length > 4 && bytes.[0] = 1uy && bytes.[1] = 0uy ->
                let throwOnUnmappableChar =
                    if bytes.Length = 30 then bytes.[29] <> 0uy else snd flags

                bytes.[2] <> 0uy, throwOnUnmappableChar
            | _ -> flags

        (true, false)
        |> readFrom (mr.GetAssemblyDefinition().GetCustomAttributes ())
        |> readFrom (mr.GetTypeDefinition(concreteType.Definition.Get).GetCustomAttributes ())

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

            // A `CopyBytes` step writes the managed value itself at the native offset, which is
            // sound only when the managed image of that value *is* its native image. Definitional
            // for a primitive. For a composite it is a claim about the interior, and the managed
            // layout walk (`CliValueType.SizeOf`) is not the marshal layout walk
            // (`TryComputeMarshalLayout`) — CoreCLR repositions some fields between the two
            // forms. So accept composites only where the interior is trivial: a primitive-like
            // wrapper (an enum, `IntPtr`, …) is a single field at offset 0, whose image is that
            // field's image under either walk. Anything else needs a recursive plan.
            //
            // `System.Decimal` is the one composite whose interior is trivial by definition rather
            // than by shape. CoreCLR marshals a Decimal field with `ILDecimalMarshaler`
            // (`ILCopyMarshalerKnownStruct<CLASS__DECIMAL, DECIMAL>`, ilmarshalers.h:1788), a copy
            // marshaler whose native type *is* `System.Decimal`: the stub `ldobj`s the managed
            // value and `stobj`s it at the field's native offset. `isBlittableField` still says no,
            // because the *outer* struct is not blittable when it holds one — a Decimal's native
            // placement is decided by the native layout walk, not the managed one.
            //
            // Only a field with no `[MarshalAs]` descriptor reaches this (see below), so the
            // descriptor it is judged under is `None`.
            let isBlittableUndescribed (contents : CliType) : bool =
                isBlittableField concreteTypes assemblies corelib vt.CharSet None contents

            let isCopyableVerbatim (contents : CliType) : bool =
                match contents with
                | CliType.ValueType vt when CliValueType.IsHostKnownDecimal concreteTypes assemblies corelib vt -> true
                | CliType.ValueType vt -> vt.PrimitiveLikeKind.IsSome && isBlittableUndescribed contents
                | _ -> isBlittableUndescribed contents

            let bestFit = lazy (bestFitFlags concreteTypes assemblies vt._Declared)

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

                    match
                        CliValueType.TryBoolCharFieldMarshal vt.CharSet placement.Field.MarshallingDescriptor contents
                    with
                    | Some (Result.Error err) -> Result.Error (MarshalSizeError.prefixField placement.Field.Name err)
                    | Some (Result.Ok marshal) ->
                        let kind =
                            match marshal with
                            | BoolCharMarshal.WinBool -> StructMarshalFieldKind.WinBool
                            | BoolCharMarshal.CBool -> StructMarshalFieldKind.CBool
                            | BoolCharMarshal.AnsiChar -> StructMarshalFieldKind.AnsiChar bestFit.Value
                            // A UTF-16 code unit's managed image is its native image.
                            | BoolCharMarshal.Utf16Char -> StructMarshalFieldKind.CopyBytes

                        Result.Ok
                            {
                                Placement = placement
                                Kind = kind
                                Value = contents
                            }
                    | None ->

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
                    | _ when isBlittableUndescribed contents ->
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
    /// `FunctionPointerTarget.Managed` can hold like any other managed method, so `calli`
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
        // `delegate*<ref byte, byte*, int, ref CleanupWorkListElement?, void>`. `callMethod`
        // coerces each popped argument to the zero of its declared parameter type, so declaring
        // the byrefs as `IntPtr` would deliver them wrapped in the `System.IntPtr` struct.
        // `Byref`/`Pointer` handles are structural and need no registration in
        // `AllConcreteTypes`.
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
                Owner = MethodOwner.DeclaredOn declaringType
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

    /// The conversion results a part-way-through `Marshal` invocation of the stub has accumulated.
    ///
    /// Each conversion the stub needs pushes a managed callee and leaves the program counter put,
    /// so the stub is re-entered when that callee returns with its result on the stub's *own*
    /// evaluation stack, which starts empty and which nothing but this code pushes to. Same
    /// marker idiom as `NativeRuntimeTypeQCall`.
    ///
    /// A conversion helper that returns *void* pushes nothing, so the count never advances and
    /// the stub would re-enter forever. CoreLib has several such marshallers
    /// (`CSTRMarshaler.ConvertFixedToNative`, `FixedWSTRMarshaler`); implementing one requires
    /// pushing a sentinel of our own first, as the `Unmarshal` direction does for the void
    /// `DateTime` constructor.
    ///
    /// Result *i* is attributed to conversion *i* of a plan that is recomputed on every pass, so
    /// the attribution is sound only while the plan's step order is a deterministic function of
    /// inputs that cannot change between passes. It is: the order comes from the type's field
    /// layout (assembly loads and concretizations only extend, never reorder) and from the source
    /// box, which no conversion helper is given a reference to.
    ///
    /// Each result is checked against the conversion it answers when it is consumed, not here.
    let private completedConversions (frame : MethodState) : EvalStackValue list =
        frame.EvaluationStack.Values |> List.rev

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

    /// `System.Byte` as a concrete type, for forming byte-view byrefs into the native image. Same
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

    /// One of the guest's static `System.StubHelpers` conversions, by class, name and arity:
    ///
    /// - `DateMarshaler.ConvertToNative(DateTime) -> double`, which CoreCLR's date marshaller calls
    ///   on the way out (ilmarshalers.cpp:1247), and `DateMarshaler.ConvertToManaged(double) -> long`,
    ///   which it calls on the way back (ilmarshalers.cpp:1260);
    /// - `AnsiCharMarshaler.ConvertToNative(char, bool, bool) -> byte` and
    ///   `AnsiCharMarshaler.ConvertToManaged(byte) -> char`, likewise for its ANSI `char`
    ///   marshaller (ilmarshalers.cpp:1448, :1459).
    let private stubHelperMethod
        (operation : string)
        (className : string)
        (name : string)
        (arity : int)
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        : IlMachineState * MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        let declaringType =
            baseClassTypes.Corelib.TypeDefs
            |> Seq.tryPick (fun (KeyValue (_, v)) ->
                if v.Namespace = "System.StubHelpers" && v.Name = className then
                    Some v
                else
                    None
            )
            |> Option.defaultWith (fun () ->
                failwith $"%s{operation}: System.StubHelpers.%s{className} not found in corelib"
            )

        let method =
            declaringType.Methods
            |> List.tryFind (fun m -> m.Name = name && m.IsStatic && MethodInfo.arity m = arity)
            |> Option.defaultWith (fun () ->
                failwith
                    $"%s{operation}: static System.StubHelpers.%s{className}.%s{name} of %d{arity} argument(s) not found"
            )

        let state, concretized, _ =
            ExecutionConcretization.concretizeMethodWithAllGenerics
                loggerFactory
                baseClassTypes
                ImmutableArray.Empty
                method
                ImmutableArray.Empty
                state

        state, concretized

    /// The guest's `System.DateTime::.ctor(long ticks)`, which CoreCLR's date marshaller calls on
    /// the field's managed home with the tick count `DateMarshaler.ConvertToManaged` produced
    /// (`METHOD__DATE_TIME__LONG_CTOR`, ilmarshalers.cpp:1262).
    let private dateTimeTicksCtor
        (operation : string)
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        : IlMachineState * MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        let method =
            baseClassTypes.DateTime.Methods
            |> List.tryFind (fun m ->
                m.Name = ".ctor"
                && not m.IsStatic
                && (MethodInfo.requireRawSignature operation m).ParameterTypes = [
                    TypeDefn.PrimitiveType PrimitiveType.Int64
                ]
            )
            |> Option.defaultWith (fun () -> failwith $"%s{operation}: System.DateTime::.ctor(long) not found")

        let state, concretized, _ =
            ExecutionConcretization.concretizeMethodWithAllGenerics
                loggerFactory
                baseClassTypes
                ImmutableArray.Empty
                method
                ImmutableArray.Empty
                state

        state, concretized

    /// Whether a byref's projection chain addresses the whole of its root's value: the empty
    /// chain, or a type view over the whole payload — optionally with an explicit zero byte
    /// offset, which is the same address.
    let private addressesWholeValue (projections : ByrefProjection list) : bool =
        match List.rev projections with
        | []
        | [ ByrefProjection.ReinterpretAs _ ]
        | [ ByrefProjection.ByteOffset 0 ; ByrefProjection.ReinterpretAs _ ] -> true
        | _ -> false

    /// Read the struct the stub is to marshal, given the byref CoreLib passed as the stub's first
    /// argument.
    ///
    /// That byref is always `RuntimeHelpers.GetRawData(box)` — `Marshal.StructureToPtr` boxes its
    /// argument and hands the stub a `ref byte` onto the box's payload (Marshal.CoreCLR.cs:264,
    /// :275) — so the value wanted is the boxed payload itself, read *structurally*.
    ///
    /// Reading through the `ref byte` view flattens every cell to bytes, and a struct may legally
    /// hold a value that has no byte rendering: an `IntPtr` field assigned from
    /// `Marshal.AllocHGlobal` is a managed pointer with provenance, which `CliNumericType.ToBytes`
    /// refuses. Such a struct marshals fine — the destination write preserves the pointer cell —
    /// but only if the read does not destroy it first. Only the *destination* is bytes; the source
    /// is a value.
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
        match source with
        | ManagedPointerSource.Byref (ByrefRoot.HeapValue addr, projections) when addressesWholeValue projections ->
            CliType.ValueType (ManagedHeap.get addr state.ManagedHeap).Contents
        | _ ->
            let template, _ = IlMachineState.cliTypeZeroOfHandle state baseClassTypes typeHandle

            IlMachineState.readManagedByrefBytesAs baseClassTypes state source template

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
    /// `Marshal` writes nothing to the native image until every conversion has completed, so a
    /// resumption never observes a half-written image. `Unmarshal` stores each field into the box
    /// as it goes, in declaration order, which is what CoreCLR's stub does too; the box is the
    /// fresh one `Marshal.PtrToStructure` allocated, and it is dropped unseen if a conversion
    /// throws.
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
                instruction.ExecutingMethod.RequiredDeclaringType.Identity
                instruction.ExecutingMethod.DeclaringTypeGenerics
            |> Option.defaultWith (fun () ->
                failwith
                    $"%s{operation}: declaring type %s{MethodOwner.describe instruction.ExecutingMethod.Owner} is not registered in AllConcreteTypes"
            )

        let frameId = state.ThreadState.[thread].ActiveMethodState

        if instruction.Arguments.Length <> 4 then
            failwith
                $"%s{operation}: expected the four arguments of `delegate*<ref byte, byte*, int, ref CleanupWorkListElement?, void>`, got %d{instruction.Arguments.Length}"

        let op = operationOf operation (EvalStackValue.ofCliType instruction.Arguments.[2])

        // The managed struct (a byref onto a box's payload) and its native image. Which is read
        // and which written depends on the operation.
        let managed =
            managedPointerOf
                operation
                "the managed struct reference"
                (EvalStackValue.ofCliType instruction.Arguments.[0])

        let nativeImage =
            managedPointerOf operation "the native image pointer" (EvalStackValue.ofCliType instruction.Arguments.[1])

        /// The stub's work is done: pop its frame and hand control back to CoreLib. The stub returns
        /// void, so its evaluation stack must be empty by now, which `returnStackFrame` checks.
        let finish (state : IlMachineState) : ExecutionResult =
            match IlMachineState.returnStackFrame loggerFactory baseClassTypes thread state with
            | ReturnFrameResult.NormalReturn state -> ExecutionResult.stepped (state, WhatWeDid.Executed)
            | result -> failwith $"%s{operation}: unexpected ReturnFrameResult returning from stub frame: %A{result}"

        /// Call a guest method whose arguments are already on this frame's evaluation stack, without
        /// returning this frame. The dispatch loop runs the callee, whose result (if any) lands on
        /// this frame's evaluation stack, and then re-enters the stub.
        ///
        /// `callMethodWithCommitment` arms the callee's class initialiser on the callee's frame; the
        /// dispatch loop runs it as that frame's prologue.
        let callGuest
            (callee : MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
            (state : IlMachineState)
            : ExecutionResult
            =
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
                    IlMachineStateExecution.CallSiteTransition.StaysCooperative
                    IlMachineStateExecution.CallRoute.NamedByInstruction
                    callee.Generics
                    callee
                    thread
                    threadState
                    None
                    ReturnValueDisposition.PushToCaller
                    false // wrapExceptionInTargetInvocation
                    state

            match commitment with
            | IlMachineStateExecution.CallCommitment.Aborted fatal ->
                ExecutionResult.stepped (state, WhatWeDid.Aborted fatal)
            | IlMachineStateExecution.CallCommitment.Committed
            | IlMachineStateExecution.CallCommitment.Raised ->
                // Either a callee frame or an exception constructor is now on top of us; in both
                // cases our frame stays put and the dispatch loop takes it from here.
                ExecutionResult.stepped (state, WhatWeDid.SuspendedForManagedCall)

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

        /// The address of byte `nativeOffset` of the native image, as a byte-view byref — the same
        /// shape the guest's own pointer arithmetic produces before a `stind` or `ldind`.
        let addressOf (nativeOffset : int) (state : IlMachineState) : ManagedPointerSource =
            ManagedPointerByteView.addByteOffset state byteView nativeOffset nativeImage

        /// The native image is a raw buffer the guest sized from `Marshal.SizeOf`, so the plan's
        /// own total is the only bound we can check a field against. A step that ran past it would
        /// touch memory outside what the guest allocated.
        let checkInsideImage (plan : StructMarshalPlan) : unit =
            for step in plan.Steps do
                if step.Placement.NativeOffset + step.Placement.NativeSize.Size > plan.NativeSize.Size then
                    failwith
                        $"%s{operation}: field %s{step.Placement.Field.Name} occupies offset %d{step.Placement.NativeOffset} for %d{step.Placement.NativeSize.Size} byte(s), past the %d{plan.NativeSize.Size}-byte unmanaged image of %O{typeHandle}"

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
            | StructMarshalFieldKind.WinBool -> CliType.Numeric (CliNumericType.Int32 0)
            | StructMarshalFieldKind.CBool
            | StructMarshalFieldKind.AnsiChar _ -> CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim 0uy))
            | StructMarshalFieldKind.CopyBytes -> CliType.ZeroLike step.Value

        let clearImage (plan : StructMarshalPlan) (state : IlMachineState) : IlMachineState =
            writeImage plan nativeZero state

        /// Read a step's native value, which for every kind but `CopyBytes` is a single scalar of
        /// the width and type its `nativeZero` has.
        let readNativeScalar
            (step : StructMarshalStep)
            (native : ManagedPointerSource)
            (state : IlMachineState)
            : CliType
            =
            IlMachineState.readManagedByrefAs baseClassTypes state (nativeZero step) native

        match op with
        | Operation.Cleanup ->
            // CoreLib calls the stub with `Cleanup` before `Marshal` when `fDeleteOld` is set, to
            // release whatever the previous contents owned. Every field kind we support owns
            // nothing — a copied value, an OADate double, a BOOL and an ANSI byte all live
            // entirely inside the destination buffer — so there is nothing to release, and a field kind that *did*
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
            // CoreCLR's Unmarshal stream converts each field from its native form and stores it
            // into the field's managed home, in declaration order, so that under explicit layout
            // a later-declared field overwrites an earlier one it overlaps. The managed homes are
            // the fields of the box `PtrToStructureHelper` handed us (Marshal.CoreCLR.cs:291).
            //
            // A `CopyBytes` field is a load of its native bytes and a store to its home, and a
            // `WinBool` or `CBool` field the same with a comparison against zero between. An
            // `OADate` field is `ldflda home; ldind.r8 native; call DateMarshaler.ConvertToManaged;
            // call DateTime::.ctor(long)` (ilmarshalers.cpp:1251), so each takes two guest calls,
            // and an `AnsiChar` field is `ldind.u1 native; call AnsiCharMarshaler.ConvertToManaged`
            // and a store (ilmarshalers.cpp:1459), so one; the stub is re-entered after each call.
            // Progress is kept on this frame's evaluation stack, which nothing but this code pushes
            // to. Top first, it holds one of:
            //
            //   (empty)                       no field has been unmarshalled yet;
            //   k                             fields 0..k-1 have been unmarshalled;
            //   ticks; &home; k               field k-1 is a DateTime whose tick count is in hand,
            //                                 and whose constructor is next;
            //   c; k                          field k-1 is an ANSI char converted to `c`, which is
            //                                 yet to be stored.
            //
            // The counter is sound only while the plan's step order is a deterministic function
            // of the type, which `planFor` below makes it: it is computed from the type's zero,
            // never from the box's current contents.
            let box =
                match managed with
                | ManagedPointerSource.Byref (ByrefRoot.HeapValue addr, projections) when
                    addressesWholeValue projections
                    ->
                    addr
                | other ->
                    failwith
                        $"%s{operation}: expected the managed struct reference to address a whole box, which is what `RuntimeHelpers.GetRawData` produces, but got %O{other}"

            match (ManagedHeap.get box state.ManagedHeap).Contents with
            | boxed when boxed.Declared = typeHandle -> ()
            | boxed ->
                failwith
                    $"%s{operation}: expected the managed struct reference to address a box of %O{typeHandle}, which is the type the stub was minted for, but it holds a %O{boxed.Declared}"

            let zero, state = IlMachineState.cliTypeZeroOfHandle state baseClassTypes typeHandle
            let plan = planFor zero
            checkInsideImage plan
            let steps = Array.ofList plan.Steps

            let homeOf (step : StructMarshalStep) : ManagedPointerSource =
                ManagedPointerSource.Byref (ByrefRoot.HeapValue box, [ ByrefProjection.Field step.Placement.Field.Id ])

            let rec unmarshalFrom (index : int) (state : IlMachineState) : ExecutionResult =
                if index = steps.Length then
                    finish state
                else

                let step = steps.[index]
                let native = addressOf step.Placement.NativeOffset state

                match step.Kind with
                | StructMarshalFieldKind.CopyBytes ->
                    // CoreCLR's copy marshaler is an `ldobj` and `stobj` of the field's own type,
                    // so read as that type rather than as the plan's value, which the Marshal
                    // direction unwraps (an `IntPtr` field's step holds a bare native int), and
                    // coerce to it as `stobj` does. The coercion is not redundant: over a pointer
                    // cell with provenance, `readManagedByrefAs` hands back the bare cell whatever
                    // the template, and storing that would leave the field of type `IntPtr`
                    // holding something that is not one.
                    let template = CliType.ZeroLike step.Placement.Field.Contents

                    let value =
                        IlMachineState.readManagedByrefAs baseClassTypes state template native
                        |> EvalStackValue.ofCliType
                        |> EvalStackValue.toCliTypeCoerced template

                    let state =
                        IlMachineState.writeManagedByrefWithBase baseClassTypes state (homeOf step) value

                    unmarshalFrom (index + 1) state
                | StructMarshalFieldKind.OADate ->
                    let oaDate =
                        IlMachineState.readManagedByrefAs
                            baseClassTypes
                            state
                            (CliType.Numeric (CliNumericType.Float64 0.0))
                            native

                    let state, convertToManaged =
                        stubHelperMethod
                            operation
                            "DateMarshaler"
                            "ConvertToManaged"
                            1
                            loggerFactory
                            baseClassTypes
                            state

                    state
                    |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 (index + 1))) thread
                    |> IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer (homeOf step)) thread
                    |> IlMachineState.pushToEvalStack oaDate thread
                    |> callGuest convertToManaged
                | StructMarshalFieldKind.WinBool
                | StructMarshalFieldKind.CBool ->
                    let isTrue =
                        match step.Kind, readNativeScalar step native state with
                        | StructMarshalFieldKind.WinBool, CliType.Numeric (CliNumericType.Int32 v) -> v <> 0
                        | StructMarshalFieldKind.CBool, CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim v)) ->
                            v <> 0uy
                        | _, other ->
                            failwith
                                $"%s{operation}: unmarshalling field %s{step.Placement.Field.Name} of %O{typeHandle}, the native bool read back as %O{other}"

                    IlMachineState.writeManagedByrefWithBase baseClassTypes state (homeOf step) (CliType.ofBool isTrue)
                    |> unmarshalFrom (index + 1)
                | StructMarshalFieldKind.AnsiChar _ ->
                    let nativeByte = readNativeScalar step native state

                    let state, convertToManaged =
                        stubHelperMethod
                            operation
                            "AnsiCharMarshaler"
                            "ConvertToManaged"
                            1
                            loggerFactory
                            baseClassTypes
                            state

                    state
                    |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 (index + 1))) thread
                    |> IlMachineState.pushToEvalStack nativeByte thread
                    |> callGuest convertToManaged

            let frame = IlMachineState.getFrame thread frameId state

            match frame.EvaluationStack.Values with
            | [ EvalStackValue.Int64 _
                EvalStackValue.ManagedPointer home
                EvalStackValue.Int32 (Int32Source.Verbatim k) ] ->
                let inProgress =
                    if 0 < k && k <= steps.Length then
                        Some steps.[k - 1]
                    else
                        None

                match inProgress with
                | Some step when step.Kind = StructMarshalFieldKind.OADate && home = homeOf step -> ()
                | _ ->
                    failwith
                        $"%s{operation}: unmarshalling %O{typeHandle}, found a tick count for field %d{k - 1} addressed at %O{home}, which is not the home of a DateTime field of that index"

                let state, ctor = dateTimeTicksCtor operation loggerFactory baseClassTypes state
                callGuest ctor state
            | [ EvalStackValue.Int32 _ as converted ; EvalStackValue.Int32 (Int32Source.Verbatim k) ] ->
                let step =
                    if 0 < k && k <= steps.Length then
                        Some steps.[k - 1]
                    else
                        None

                match step with
                | Some ({
                            Kind = StructMarshalFieldKind.AnsiChar _
                        } as step) ->
                    let _, state = IlMachineState.popEvalStack thread state
                    let _, state = IlMachineState.popEvalStack thread state
                    let value = EvalStackValue.toCliTypeCoerced (CliType.Char (0uy, 0uy)) converted

                    IlMachineState.writeManagedByrefWithBase baseClassTypes state (homeOf step) value
                    |> unmarshalFrom k
                | _ ->
                    failwith
                        $"%s{operation}: unmarshalling %O{typeHandle}, found a converted value %O{converted} for field %d{k - 1}, which is not an ANSI char field"
            | stack ->

            let resumeAt, state =
                match stack with
                | [] -> 0, state
                | [ EvalStackValue.Int32 (Int32Source.Verbatim k) ] when 0 < k && k <= steps.Length ->
                    let _, state = IlMachineState.popEvalStack thread state
                    k, state
                | other ->
                    failwith
                        $"%s{operation}: unmarshalling %O{typeHandle}, expected this frame's evaluation stack to hold the stub's own progress record, but it holds %O{other}"

            unmarshalFrom resumeAt state
        | Operation.Marshal ->

        let completed = completedConversions (IlMachineState.getFrame thread frameId state)
        let completedCount = List.length completed
        let sourceValue = readSource operation baseClassTypes typeHandle managed state

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
                | StructMarshalFieldKind.OADate
                | StructMarshalFieldKind.AnsiChar _ -> true
                | StructMarshalFieldKind.CopyBytes
                | StructMarshalFieldKind.WinBool
                | StructMarshalFieldKind.CBool -> false
            )

        if completedCount > List.length conversions then
            failwith
                $"%s{operation}: %d{completedCount} conversion result(s) are on the evaluation stack but type %O{typeHandle} has only %d{List.length conversions} conversion field(s)"

        if completedCount < List.length conversions then
            // Convert the next field by calling the guest's own marshaller. We do not return this
            // frame, so the dispatch loop runs the callee and then re-enters us with its result on
            // our evaluation stack.
            let next = conversions.[completedCount]

            match next.Kind with
            | StructMarshalFieldKind.OADate ->
                let state, convertToNative =
                    stubHelperMethod operation "DateMarshaler" "ConvertToNative" 1 loggerFactory baseClassTypes state

                state
                |> IlMachineState.pushToEvalStack next.Value thread
                |> callGuest convertToNative
            | StructMarshalFieldKind.AnsiChar (bestFit, throwOnUnmappableChar) ->
                let state, convertToNative =
                    stubHelperMethod
                        operation
                        "AnsiCharMarshaler"
                        "ConvertToNative"
                        3
                        loggerFactory
                        baseClassTypes
                        state

                state
                |> IlMachineState.pushToEvalStack next.Value thread
                |> IlMachineState.pushToEvalStack (CliType.ofBool bestFit) thread
                |> IlMachineState.pushToEvalStack (CliType.ofBool throwOnUnmappableChar) thread
                |> callGuest convertToNative
            | StructMarshalFieldKind.CopyBytes
            | StructMarshalFieldKind.WinBool
            | StructMarshalFieldKind.CBool ->
                failwith $"unreachable: %O{next.Kind} is not a conversion kind, but was filtered in as one"
        else

        // Every conversion has completed. Only now do we touch the destination with real values,
        // so a retry can never have observed a partially-written image.
        checkInsideImage plan

        // Conversion results are consumed in plan order, which is the order they were requested.
        let mutable remainingConversions = completed

        let nextConversion (step : StructMarshalStep) : EvalStackValue =
            match remainingConversions with
            | [] ->
                failwith
                    $"%s{operation}: ran out of conversion results while writing field %s{step.Placement.Field.Name}"
            | head :: rest ->
                remainingConversions <- rest
                head

        let managedBool (step : StructMarshalStep) : bool =
            match step.Value with
            | CliType.Bool b -> b <> 0uy
            | other ->
                failwith $"%s{operation}: field %s{step.Placement.Field.Name} marshals as a bool, but holds %O{other}"

        let valueFor (step : StructMarshalStep) : CliType =
            match step.Kind with
            | StructMarshalFieldKind.CopyBytes -> step.Value
            | StructMarshalFieldKind.WinBool ->
                CliType.Numeric (CliNumericType.Int32 (if managedBool step then 1 else 0))
            | StructMarshalFieldKind.CBool ->
                CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim (if managedBool step then 1uy else 0uy)))
            | StructMarshalFieldKind.OADate ->
                match nextConversion step with
                | EvalStackValue.Float (EvalStackFloat.Double f) -> CliType.Numeric (CliNumericType.Float64 f)
                | other ->
                    failwith
                        $"%s{operation}: expected DateMarshaler.ConvertToNative's double for field %s{step.Placement.Field.Name}, found %O{other}"
            | StructMarshalFieldKind.AnsiChar _ ->
                match nextConversion step with
                | EvalStackValue.Int32 (Int32Source.Verbatim b) when 0 <= b && b <= 255 ->
                    CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim (byte b)))
                | other ->
                    failwith
                        $"%s{operation}: expected AnsiCharMarshaler.ConvertToNative's byte for field %s{step.Placement.Field.Name}, found %O{other}"

        let state = writeImage plan valueFor state

        if not (List.isEmpty remainingConversions) then
            failwith
                $"%s{operation}: %d{List.length remainingConversions} conversion result(s) were left unconsumed after writing every field"

        // The conversion results are this frame's scratch state and must go before it returns.
        let mutable state = state

        for _ in 1..completedCount do
            let _, next = IlMachineState.popEvalStack thread state
            state <- next

        finish state
