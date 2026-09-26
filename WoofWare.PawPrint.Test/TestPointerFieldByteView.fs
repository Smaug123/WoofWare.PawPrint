namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.Runtime.InteropServices
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Reading a pointer-typed field of a struct through a byte view of the struct, which is what
/// `Unsafe.ReadUnaligned<IntPtr>(ref Unsafe.As<S, byte>(ref s))` does.
///
/// A pointer field has no byte image, and nor does a struct holding one, so such a view reaches the
/// field only by naming it. The only reads it can serve are the ones that cover exactly one pointer
/// field at the width of a native int: those hand back the pointer the field holds, provenance and
/// all. `sourcesPure/PointerFieldThroughByteView.cs` checks the guest sees the right pointee from
/// every root a struct can live in. This file checks the two things a guest cannot: that the value
/// read is *the same value* a plain read of the field would push, and that the reads which would
/// need the field's bytes are still refused rather than answered with invented ones. Real .NET
/// answers those, so a differential guest could only ever be parked.
///
/// `TestPointerArrayCellByteView` is the same set of properties for a pointer that is an array
/// element rather than a field.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestPointerFieldByteView =

    // Factory intentionally undisposed: corelib.Logger outlives this scope.
    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private bct : BaseClassTypes<DumpedAssembly> = Corelib.getBaseTypes corelib

    let private loaded : LoadedAssemblies = LoadedAssemblies.ofAssemblies [ corelib ]

    let private loggerFactory = snd (LoggerFactory.makeTest ())

    let private preparedState : IlMachineState =
        { IlMachineState.initial loggerFactory ImmutableArray.Empty corelib with
            ConcreteTypes = Corelib.concretizeAll loaded bct AllConcreteTypes.Empty
        }

    let private int32Handle : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle preparedState.ConcreteTypes bct.Int32

    let private byteHandle : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle preparedState.ConcreteTypes bct.Byte

    let private intPtrHandle : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle preparedState.ConcreteTypes bct.IntPtr

    let private objectHandle : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle preparedState.ConcreteTypes bct.Object

    let private storageDeclared : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle preparedState.ConcreteTypes bct.TypedReference

    let private typeOf (handle : ConcreteTypeHandle) : ConcreteType<ConcreteTypeHandle> =
        AllConcreteTypes.lookup handle preparedState.ConcreteTypes
        |> Option.defaultWith (fun () -> failwith $"%O{handle} has no registry entry")

    let private pointerSize : int = 8

    let private nativeIntTemplate : CliType =
        CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L))

    /// `System.IntPtr` as `Unsafe.ReadUnaligned<IntPtr>` asks for it: the wrapper, not the bare
    /// native int it flattens to.
    let private intPtrTemplate : CliType =
        fst (IlMachineState.cliTypeZeroOfHandle preparedState bct intPtrHandle)

    let private pointerGen : Gen<CliRuntimePointer> =
        let local =
            gen {
                let! frame = Gen.choose (0, 20)
                let! slot = Gen.choose (0, 5)

                return
                    ManagedPointerSource.Byref
                        {
                            Root =
                                ByrefRoot.LocalVariable (ThreadId.ThreadId 0, FrameId.FrameId frame, uint16<int> slot)
                            Projections = []
                        }
                    |> CliRuntimePointer.Managed
            }

        Gen.oneof
            [
                ArbMap.defaults |> ArbMap.generate<int64> |> Gen.map CliRuntimePointer.Verbatim
                ArbMap.defaults
                |> ArbMap.generate<int64>
                |> Gen.map CliRuntimePointer.FieldRegistryHandle
                Gen.constant (CliRuntimePointer.Managed ManagedPointerSource.Null)
                local
                Gen.constant (CliRuntimePointer.TypeHandlePtr (RuntimeTypeHandleTarget.Closed int32Handle))
            ]

    let private leadId : FieldId = FieldId.named "Lead"
    let private refId : FieldId = FieldId.named "Ref"
    let private innerId : FieldId = FieldId.named "Inner"
    let private pointerId (i : int) : FieldId = FieldId.named $"P%d{i}"

    let private field (id : FieldId) (handle : ConcreteTypeHandle) (contents : CliType) : CliField =
        {
            Id = id
            Name = id.Name
            Contents = contents
            Offset = None
            Type = handle
            MarshallingDescriptor = None
        }

    let private synthesise (fields : CliField list) : CliValueType =
        SynthesisedLayoutKind.ofFields
            bct
            preparedState.ConcreteTypes
            storageDeclared
            Layout.Default
            CharSet.Ansi
            fields

    /// `{ byte Lead; int* P0; ...; object Ref }`, or the same without `Lead` or `Ref`. `Lead` puts
    /// every pointer at a non-zero offset; `Ref` gives the storage a reason other than its pointers
    /// to have no byte image.
    let private storageOf (withLead : bool) (withReference : bool) (pointers : CliRuntimePointer list) : CliValueType =
        [
            if withLead then
                yield field leadId byteHandle (CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim 0xA5uy)))
            for i, pointer in List.indexed pointers do
                yield field (pointerId i) (ConcreteTypeHandle.Pointer int32Handle) (CliType.RuntimePointer pointer)
            if withReference then
                yield field refId objectHandle (CliType.ObjectRef (Some (ManagedHeapAddress.ManagedHeapAddress 7)))
        ]
        |> synthesise

    type private Root =
        /// The storage is a boxed value: `ByrefRoot.HeapValue`.
        | Boxed
        /// The storage is element 1 of a two-element array: `ByrefRoot.ArrayElement`.
        | ArrayElement

    /// A byte cursor `byteOffset` bytes into the storage, rooted as `root` says: the shape
    /// `Unsafe.Add(ref Unsafe.As<S, byte>(ref s), byteOffset)` produces. A `nested` storage sits in
    /// the root as the only field of an enclosing struct, so the byref steps into it with a `Field`
    /// before the view, as it does for `ref outer.Inner`.
    let private cursorAt
        (root : Root)
        (nested : bool)
        (storage : CliValueType)
        (byteOffset : int)
        : IlMachineState * ManagedPointerSource
        =
        let storage, prefix =
            if nested then
                synthesise [ field innerId storageDeclared (CliType.ValueType storage) ],
                [ ByrefProjection.Field innerId ]
            else
                storage, []

        let byrefRoot, state =
            match root with
            | Root.Boxed ->
                let addr, state =
                    IlMachineState.allocateManagedObject storageDeclared storage preparedState

                ByrefRoot.HeapValue addr, state
            | Root.ArrayElement ->
                let arr, state =
                    IlMachineState.allocateArray
                        (ConcreteTypeHandle.OneDimArrayZero storageDeclared)
                        (fun () -> CliType.ValueType storage)
                        2
                        preparedState

                ByrefRoot.ArrayElement (arr, 1), state

        let projections =
            prefix
            @ [
                ByrefProjection.ReinterpretAs (typeOf byteHandle)
                ByrefProjection.ByteOffset byteOffset
            ]

        let src =
            (ManagedPointerSource.Byref
                {
                    Root = byrefRoot
                    Projections = []
                },
             projections)
            ||> List.fold (fun src proj -> ManagedPointerSource.appendProjection proj src)

        state, src

    type private Case =
        {
            Pointers : CliRuntimePointer list
            Target : int
            WithLead : bool
            WithReference : bool
            Root : Root
            Nested : bool
        }

        member this.Storage : CliValueType =
            storageOf this.WithLead this.WithReference this.Pointers

        member this.TargetOffset : int =
            fst (CliValueType.GetFieldLayoutById (pointerId this.Target) this.Storage)

        member this.CursorAt (byteOffset : int) : IlMachineState * ManagedPointerSource =
            cursorAt this.Root this.Nested this.Storage byteOffset

    let private caseGen : Gen<Case> =
        gen {
            let! count = Gen.choose (1, 3)
            let! pointers = Gen.listOfLength count pointerGen
            let! target = Gen.choose (0, count - 1)
            let! withLead = Gen.elements [ true ; false ]
            let! withReference = Gen.elements [ true ; false ]
            let! root = Gen.elements [ Root.Boxed ; Root.ArrayElement ]
            let! nested = Gen.elements [ true ; false ]

            return
                {
                    Pointers = pointers
                    Target = target
                    WithLead = withLead
                    WithReference = withReference
                    Root = root
                    Nested = nested
                }
        }

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 200

    let private messageOf (f : unit -> 'a) : string =
        let outcome =
            try
                f () |> ignore
                None
            with e ->
                Some e.Message

        match outcome with
        | None -> failwith "expected the access to be refused, but it succeeded"
        | Some message -> message

    /// The read agrees with a plain read of the target field, which pushes `ofCliType field` — so
    /// the pointer's provenance survives — and it comes back in the *template's* shape, so a caller
    /// asking for an `IntPtr` gets an `IntPtr` rather than the field's `RuntimePointer`. Both ways a
    /// byte view is read are held to it: with the opcode's own template (`ldind.i`,
    /// `Unsafe.ReadUnaligned<IntPtr>`) and with a template the pointer's view does not dictate
    /// (`ldobj`).
    [<TestCase true>]
    [<TestCase false>]
    let ``a field-aligned native-int read through a byte view hands over the pointer`` (wrapped : bool) : unit =
        let template = if wrapped then intPtrTemplate else nativeIntTemplate

        let property (case : Case) : unit =
            let state, cursor = case.CursorAt case.TargetOffset

            let expected =
                CliType.RuntimePointer case.Pointers.[case.Target]
                |> EvalStackValue.ofCliType
                |> EvalStackValue.toCliTypeCoerced template

            IlMachineState.readManagedByrefBytesAs bct state (ManagedPointerSource.requireAddressed cursor) template
            |> shouldEqual expected

            IlMachineState.readManagedByrefAs bct state template (ManagedPointerSource.requireAddressed cursor)
            |> shouldEqual expected

        Check.One (config, Prop.forAll (Arb.fromGen caseGen) property)

    /// A native-int-wide read that starts partway into a pointer field would need that field's
    /// bytes, and there are none to give.
    [<Test>]
    let ``a misaligned native-int read of a pointer field is refused`` () : unit =
        let gen =
            gen {
                let! case = caseGen
                let! residue = Gen.choose (1, pointerSize - 1)
                return case, residue
            }

        let property (case : Case, residue : int) : unit =
            let state, cursor = case.CursorAt (case.TargetOffset + residue)

            messageOf (fun () ->
                IlMachineState.readManagedByrefBytesAs
                    bct
                    state
                    (ManagedPointerSource.requireAddressed cursor)
                    nativeIntTemplate
            )
            |> shouldContainText "refusing byte view"

        Check.One (config, Prop.forAll (Arb.fromGen gen) property)

    /// A read narrower than a pointer, even one starting on the field's boundary, is a read of some
    /// of the pointer's bytes.
    [<Test>]
    let ``a field-aligned read narrower than a pointer is refused`` () : unit =
        let gen =
            gen {
                let! case = caseGen

                let! template =
                    Gen.elements
                        [
                            CliType.Numeric (CliNumericType.Int32 0)
                            CliType.Numeric (CliNumericType.UInt16 0us)
                            CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim 0uy))
                        ]

                return case, template
            }

        let property (case : Case, template : CliType) : unit =
            let state, cursor = case.CursorAt case.TargetOffset

            messageOf (fun () ->
                IlMachineState.readManagedByrefBytesAs
                    bct
                    state
                    (ManagedPointerSource.requireAddressed cursor)
                    template
            )
            |> shouldContainText "refusing byte view"

        Check.One (config, Prop.forAll (Arb.fromGen gen) property)

    /// A pointer-wide read of a pointer field as a *number* of the same width is a read of the
    /// pointer's bytes too, however the eval stack would later widen it.
    [<Test>]
    let ``a field-aligned int64 read of a pointer field is refused`` () : unit =
        let property (case : Case) : unit =
            let state, cursor = case.CursorAt case.TargetOffset

            messageOf (fun () ->
                IlMachineState.readManagedByrefBytesAs
                    bct
                    state
                    (ManagedPointerSource.requireAddressed cursor)
                    (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L)))
            )
            |> shouldContainText "refusing byte view"

        Check.One (config, Prop.forAll (Arb.fromGen caseGen) property)

    /// A `delegate*` field holds a native int naming its method rather than a `RuntimePointer`, and
    /// an `nint` field can hold one naming a handle. Such a field has named bytes but no number, so
    /// a byte view can hand it over only as a whole native int — the same value a plain read of the
    /// field pushes — and it does so whether the storage's other fields have a byte image, hold a
    /// reference, or hold a pointer. Read as an `int64` it is refused.
    ///
    /// Compared as the evaluation stack sees it: a top-level field of a box is served by a reader
    /// that hands back the field in its own shape rather than the template's, which flattens to the
    /// same stack value.
    [<TestCase true>]
    [<TestCase false>]
    let ``a native int naming a handle is handed over through a byte view whatever its siblings``
        (wrapped : bool)
        : unit
        =
        let template = if wrapped then intPtrTemplate else nativeIntTemplate

        let siblings : CliField list list =
            [
                []
                [
                    field refId objectHandle (CliType.ObjectRef (Some (ManagedHeapAddress.ManagedHeapAddress 7)))
                ]
                [
                    field
                        refId
                        (ConcreteTypeHandle.Pointer int32Handle)
                        (CliType.RuntimePointer (CliRuntimePointer.Verbatim 0x1000L))
                ]
            ]

        let gen =
            gen {
                let! case = caseGen
                let! handle = ArbMap.defaults |> ArbMap.generate<int64>

                let! source =
                    Gen.elements
                        [
                            NativeIntSource.FieldHandlePtr handle
                            NativeIntSource.MethodHandlePtr handle
                            NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.Closed int32Handle)
                        ]

                let! sibling = Gen.elements siblings
                return case, source, sibling
            }

        let property (case : Case, source : NativeIntSource, sibling : CliField list) : unit =
            let handleCell = CliType.Numeric (CliNumericType.NativeInt source)

            let storage =
                [
                    if case.WithLead then
                        yield
                            field leadId byteHandle (CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim 0uy)))
                    yield field (pointerId 0) intPtrHandle handleCell
                    yield! sibling
                ]
                |> synthesise

            let offset = fst (CliValueType.GetFieldLayoutById (pointerId 0) storage)
            let state, cursor = cursorAt case.Root case.Nested storage offset

            IlMachineState.readManagedByrefBytesAs bct state (ManagedPointerSource.requireAddressed cursor) template
            |> EvalStackValue.ofCliType
            |> shouldEqual (EvalStackValue.ofCliType handleCell)

            messageOf (fun () ->
                IlMachineState.readManagedByrefBytesAs
                    bct
                    state
                    (ManagedPointerSource.requireAddressed cursor)
                    (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L)))
            )
            |> ignore<string>

        Check.One (config, Prop.forAll (Arb.fromGen gen) property)
