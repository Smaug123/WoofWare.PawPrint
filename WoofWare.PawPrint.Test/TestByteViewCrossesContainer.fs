namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// A byte access through a byref may leave the cell its root names, because the root may be a
/// *view* into something larger. `sourcesPure/AreSameProjectionCrossesArrayElement.cs` and
/// `sourcesPure/GCMemoryInfoSpanProperties.cs` cover that it now works; this file covers the
/// two ways it must still refuse.
///
/// Neither refusal can be a `sourcesPure` guest. Reading past the end of an array is undefined
/// behaviour in real .NET rather than a fault, and reading across a managed reference is
/// something real .NET does happily — so in both cases the differential harness would see the
/// real runtime succeed, and a PawPrint that refuses would simply be parked, where nothing
/// ever runs the assertion.
///
/// Each test therefore asserts *which* guard fired. Stepping out into a container widens what
/// an access can reach, and the failure mode of getting that wrong is not an exception but a
/// wrong answer: reading a neighbouring allocation, or rendering an object reference as bytes.
/// A test satisfied by any exception would be satisfied by the refusal the step-out was
/// supposed to remove.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestByteViewCrossesContainer =

    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private baseClassTypes : BaseClassTypes<DumpedAssembly> =
        Corelib.getBaseTypes corelib

    let private loadedAssemblies : LoadedAssemblies =
        LoadedAssemblies.ofAssemblies [ corelib ]

    let private concreteTypes : AllConcreteTypes =
        Corelib.concretizeAll loadedAssemblies baseClassTypes AllConcreteTypes.Empty

    let private int32Handle : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle concreteTypes baseClassTypes.Int32

    let private objectHandle : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle concreteTypes baseClassTypes.Object

    let private state () : IlMachineState =
        let _, loggerFactory = LoggerFactory.makeTest ()

        { IlMachineState.initial loggerFactory ImmutableArray.Empty corelib with
            ConcreteTypes = concreteTypes
        }

    let private concreteTypeFor (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>) =
        ConcreteType.makeFromIdentity
            typeInfo.Identity
            typeInfo.Namespace
            typeInfo.Name
            ImmutableArray<ConcreteTypeHandle>.Empty

    let private byteType : ConcreteType<ConcreteTypeHandle> =
        concreteTypeFor baseClassTypes.Byte

    let private int32Template : CliType = CliType.Numeric (CliNumericType.Int32 0)

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

    // -----------------------------------------------------------------------
    // Containment: a container is a boundary, not a starting point.
    // -----------------------------------------------------------------------

    /// A two-`int` struct under default layout, so `X` sits at offset 0 and `Y` at offset 4.
    let private pairValueType (state : IlMachineState) (x : int) (y : int) : CliValueType =
        let field (name : string) (value : int) : CliField =
            {
                Id = FieldId.named name
                Name = name
                Contents = CliType.Numeric (CliNumericType.Int32 value)
                Offset = None
                Type = int32Handle
                MarshallingDescriptor = None
            }

        [ field "X" x ; field "Y" y ]
        |> SynthesisedLayoutKind.ofFields
            baseClassTypes
            state.ConcreteTypes
            int32Handle
            Layout.Default
            System.Runtime.InteropServices.CharSet.Ansi

    /// A `Pair[3]`, and a byref into element `index`'s `Y` field displaced four bytes — which
    /// is element `index + 1`'s `X`. The `Field Y` prefix is what forces the walk through the
    /// cell lift and out into the array: with a bare element root the byte cursor folds into
    /// the element index long before this, and `readArrayBytesAs` serves it directly.
    ///
    /// `index` is varied rather than fixed at 0 because the container offset is the *sum* of
    /// where the root sits in the array and where the access sits in the root. At index 0 the
    /// first term is zero, so a walk that forgot it entirely would still be right — and every
    /// guest that reaches this code happens to be rooted at offset zero.
    let private pairArrayByref (index : int) : IlMachineState * ManagedHeapAddress * ManagedPointerSource =
        let state = state ()
        let element = CliType.ValueType (pairValueType state 0 0)

        let arr, state =
            IlMachineState.allocateArray (ConcreteTypeHandle.OneDimArrayZero int32Handle) (fun () -> element) 3 state

        let ptr =
            ManagedPointerSource.Byref (
                ByrefRoot.ArrayElement (arr, index),
                [
                    ByrefProjection.Field (FieldId.named "Y")
                    ByrefProjection.ReinterpretAs byteType
                    ByrefProjection.ByteOffset 4
                ]
            )

        state, arr, ptr

    /// Element `index + 1`'s `X`, as an `int`.
    let private readNextX (arr : ManagedHeapAddress) (index : int) (state : IlMachineState) : CliType =
        ManagedHeap.getArrayValue arr (index + 1) state.ManagedHeap
        |> CliType.getFieldById (FieldId.named "X")

    [<TestCase 0>]
    [<TestCase 1>]
    let ``a byte cursor out of an element reads the next one`` (index : int) : unit =
        let state, arr, ptr = pairArrayByref index

        let state =
            IlMachineThreadState.setArrayValue arr (CliType.ValueType (pairValueType state 5 0)) (index + 1) state

        // 5 is element `index + 1`'s `X`. An implementation that dropped the root's own
        // position in the array would read element 1 for both cases and see 5 only when
        // `index` is 0, which is why both are run.
        IlMachineManagedByref.readManagedByrefBytesAs baseClassTypes state ptr int32Template
        |> shouldEqual (CliType.Numeric (CliNumericType.Int32 5))

    [<TestCase 0>]
    [<TestCase 1>]
    let ``a byte cursor out of an element writes the next one`` (index : int) : unit =
        let state, arr, ptr = pairArrayByref index

        let state =
            IlMachineManagedByref.writeManagedByrefBytesOrTypedCell
                baseClassTypes
                state
                ptr
                (CliType.Numeric (CliNumericType.Int32 7))

        readNextX arr index state
        |> shouldEqual (CliType.Numeric (CliNumericType.Int32 7))

        // And did not land back in the field it started from.
        ManagedHeap.getArrayValue arr index state.ManagedHeap
        |> CliType.getFieldById (FieldId.named "Y")
        |> shouldEqual (CliType.Numeric (CliNumericType.Int32 0))

    [<Test>]
    let ``a byte cursor past the last element is refused, not read from the next allocation`` () : unit =
        let state, _, ptr = pairArrayByref 2

        // Element 2 is the last of three, so `Y` displaced by four bytes is byte 24 of a
        // 24-byte array: one past the end. Nothing in PawPrint's heap says what is stored
        // there, and a real runtime would read whatever the allocator put next. The two cases
        // above are the controls that make this about the boundary rather than about the shape.
        messageOf (fun () -> IlMachineManagedByref.readManagedByrefBytesAs baseClassTypes state ptr int32Template)
        |> shouldContainText "past array bounds"

    [<Test>]
    let ``a byte cursor past the last element is refused on the write path too`` () : unit =
        let state, _, ptr = pairArrayByref 2

        messageOf (fun () ->
            IlMachineManagedByref.writeManagedByrefBytesOrTypedCell
                baseClassTypes
                state
                ptr
                (CliType.Numeric (CliNumericType.Int32 7))
        )
        |> shouldContainText "past array bounds"

    // -----------------------------------------------------------------------
    // Byte-imageless storage: a container may hold cells with no byte rendering.
    // -----------------------------------------------------------------------

    /// `{ int A; object O }`: the first field is byte-addressable, the second is not.
    let private mixedValueType (state : IlMachineState) : CliValueType =
        [
            {
                Id = FieldId.named "A"
                Name = "A"
                Contents = CliType.Numeric (CliNumericType.Int32 0)
                Offset = None
                Type = int32Handle
                MarshallingDescriptor = None
            }
            {
                Id = FieldId.named "O"
                Name = "O"
                Contents = CliType.ObjectRef None
                Offset = None
                Type = objectHandle
                MarshallingDescriptor = None
            }
        ]
        |> SynthesisedLayoutKind.ofFields
            baseClassTypes
            state.ConcreteTypes
            int32Handle
            Layout.Default
            System.Runtime.InteropServices.CharSet.Ansi

    /// A byref rooted at the byte-addressable field of a mixed object, displaced far enough
    /// that the access crosses into the reference field.
    let private mixedObjectByref (byteOffset : int) : IlMachineState * ManagedPointerSource =
        let state = state ()
        let contents = mixedValueType state
        let addr, state = IlMachineState.allocateManagedObject int32Handle contents state

        let ptr =
            ManagedPointerSource.Byref (
                ByrefRoot.HeapObjectField (addr, FieldId.named "A"),
                [
                    ByrefProjection.ReinterpretAs byteType
                    ByrefProjection.ByteOffset byteOffset
                ]
            )

        state, ptr

    /// The refusal that matters, and the one worth naming precisely.
    ///
    /// Stepping out of `A` reaches the whole object, which is the point — but the object holds
    /// a managed reference, and a reference has no byte image by design: rendering one would
    /// have to invent an address, which is exactly what a deterministic replay cannot do. Note
    /// what the message says: the refusal is `readHeapValueBytesAs`'s, covering the payload as
    /// a whole rather than this particular four-byte window (which happens to be padding). That
    /// is the container reader's own pre-existing rule, and the point here is that the step-out
    /// hands the access to it rather than around it.
    ///
    /// Asserting on the specific text is what makes this test load-bearing. Any exception would
    /// satisfy a looser check — including the "there is nothing larger to read from" refusal
    /// that this whole change exists to remove — so a regression that stopped stepping out at
    /// all would pass a test that only demanded failure.
    let private crossesIntoReference (byteOffset : int) : string =
        let state, ptr = mixedObjectByref byteOffset

        messageOf (fun () -> IlMachineManagedByref.readManagedByrefBytesAs baseClassTypes state ptr int32Template)

    [<Test>]
    let ``an access crossing into a reference cell is refused, not rendered as bytes`` () : unit =
        let message = crossesIntoReference 4

        message
        |> shouldContainText "refusing byte view over boxed value type containing object references"

        message |> shouldNotContainText "nothing larger"

    [<Test>]
    let ``the write path refuses to render a reference too`` () : unit =
        let state, ptr = mixedObjectByref 4

        let message =
            messageOf (fun () ->
                IlMachineManagedByref.writeManagedByrefBytesOrTypedCell
                    baseClassTypes
                    state
                    ptr
                    (CliType.Numeric (CliNumericType.Int32 7))
            )

        message
        |> shouldContainText "refusing byte view over boxed value type containing object references"

        message |> shouldNotContainText "nothing larger"
