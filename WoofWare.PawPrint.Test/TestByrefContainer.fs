namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `ByrefContainer.tryOfRoot` answers "which byte-addressable container holds this root, and
/// where in it does the root begin".
///
/// Two things are worth pinning here that no guest program pins. The first is the *mapping*:
/// a root that answered `StackArgument` where it meant `StackLocal` would still give a
/// self-consistent coordinate system, so every comparison between two byrefs of the same kind
/// would keep working and only a cross-kind pair — which guests essentially never form —
/// would go wrong. The second is that the whole-container arms perform no heap lookup, which
/// is asserted by handing them an *empty* heap: a lookup would throw.
///
/// The view arms (array element, class field) do consult the heap, and their offsets are
/// checked against layouts known from outside PawPrint: `int` and `long` element strides, and
/// a two-`int` struct under default layout whose second field is at offset 4.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestByrefContainer =

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

    let private int64Handle : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle concreteTypes baseClassTypes.Int64

    let private state () : IlMachineState =
        let _, loggerFactory = LoggerFactory.makeTest ()

        { IlMachineState.initial loggerFactory ImmutableArray.Empty corelib with
            ConcreteTypes = concreteTypes
        }

    /// Stands in for a real field row. The whole-container arms never resolve it.
    let private syntheticFieldHandle : ComparableFieldDefinitionHandle =
        ComparableFieldDefinitionHandle.Make Unchecked.defaultof<System.Reflection.Metadata.FieldDefinitionHandle>

    let private peByteRange : PeByteRangePointer =
        {
            AssemblyFullName = "Example"
            Source = PeByteRangePointerSource.FieldRva syntheticFieldHandle
            RelativeVirtualAddress = 4096
            Size = 8
        }

    // -----------------------------------------------------------------------
    // Roots that are a whole container of their own.
    // -----------------------------------------------------------------------

    /// Each root paired with the container it *is*. Run against `ManagedHeap.empty`, so an
    /// arm that reached for the heap would throw rather than quietly pass.
    let private wholeContainerRoots : (string * ByrefRoot * ByteStorageIdentity) list =
        [
            "local variable",
            ByrefRoot.LocalVariable (ThreadId.ThreadId 0, FrameId.FrameId 3, 1us),
            ByteStorageIdentity.StackLocal (ThreadId.ThreadId 0, FrameId.FrameId 3, 1us)

            "argument",
            ByrefRoot.Argument (ThreadId.ThreadId 0, FrameId.FrameId 3, 2us),
            ByteStorageIdentity.StackArgument (ThreadId.ThreadId 0, FrameId.FrameId 3, 2us)

            "static field",
            ByrefRoot.StaticField (int32Handle, syntheticFieldHandle, StaticOwner.Shared),
            ByteStorageIdentity.StaticField (int32Handle, syntheticFieldHandle, StaticOwner.Shared)

            "PE byte range", ByrefRoot.PeByteRange peByteRange, ByteStorageIdentity.PeByteRange peByteRange

            "boxed value",
            ByrefRoot.HeapValue (ManagedHeapAddress.ManagedHeapAddress 12),
            ByteStorageIdentity.HeapObject (ManagedHeapAddress.ManagedHeapAddress 12)
        ]

    [<Test>]
    let ``a root that is its own container starts at offset zero`` () : unit =
        for name, root, expected in wholeContainerRoots do
            match ByrefContainer.tryOfRoot ManagedHeap.empty root with
            | None -> failwith $"%s{name} should have a container"
            | Some (container, offset) ->
                container |> shouldEqual expected
                offset |> shouldEqual 0L

    /// A local and an argument of the same frame with the same slot number are *different*
    /// storage. The pair above already distinguishes them, but only because the slot numbers
    /// differ; this fixes the numbers so that only the container constructor can tell them
    /// apart.
    [<Test>]
    let ``a local and an argument in the same slot are different containers`` () : unit =
        let thread = ThreadId.ThreadId 0
        let frame = FrameId.FrameId 3

        let containerOf (root : ByrefRoot) : ByteStorageIdentity =
            ByrefContainer.tryOfRoot ManagedHeap.empty root |> Option.get |> fst

        let local = containerOf (ByrefRoot.LocalVariable (thread, frame, 1us))
        let argument = containerOf (ByrefRoot.Argument (thread, frame, 1us))

        local |> shouldNotEqual argument

    /// The `RuntimeType` cache cell is a single object reference living outside any
    /// byte-addressable allocation, so it has no container at all. `StorageLocation` relies on
    /// this to decline a precise coordinate for it rather than inventing one.
    [<Test>]
    let ``the exposed class object cell has no container`` () : unit =
        ByrefRoot.ExposedClassObject (RuntimeTypeHandleTarget.Closed int32Handle)
        |> ByrefContainer.tryOfRoot ManagedHeap.empty
        |> shouldEqual None

    // -----------------------------------------------------------------------
    // Byte-addressed roots: their own offset is their offset in the container.
    // -----------------------------------------------------------------------

    [<Test>]
    let ``a byte-addressed root carries its own offset into the block`` () : unit =
        let thread = ThreadId.ThreadId 0
        let frame = FrameId.FrameId 3
        let stackBlock = StackMemoryBlockId.StackMemoryBlockId 5
        let nativeBlock = NativeMemoryBlockId.NativeMemoryBlockId 6

        ByrefRoot.StackMemoryByte (thread, frame, stackBlock, 24)
        |> ByrefContainer.tryOfRoot ManagedHeap.empty
        |> shouldEqual (Some (ByteStorageIdentity.StackMemory (thread, frame, stackBlock), 24L))

        ByrefRoot.NativeMemoryByte (nativeBlock, 24)
        |> ByrefContainer.tryOfRoot ManagedHeap.empty
        |> shouldEqual (Some (ByteStorageIdentity.NativeMemory nativeBlock, 24L))

    /// A string's character data is UTF-16, so character `n` begins at byte `2n`. Counting it
    /// in characters would put every byref into a string at half its true coordinate, and
    /// agree with itself while doing so — nothing else in the system would notice.
    [<Test>]
    let ``a string character root is measured in bytes, not characters`` () : unit =
        let str = ManagedHeapAddress.ManagedHeapAddress 7

        ByrefRoot.StringCharAt (str, 5)
        |> ByrefContainer.tryOfRoot ManagedHeap.empty
        |> shouldEqual (Some (ByteStorageIdentity.String str, 10L))

    // -----------------------------------------------------------------------
    // View roots: array element and class field.
    // -----------------------------------------------------------------------

    /// Allocate a `len`-element array of `elementHandle` and report where element `index`
    /// begins in it.
    let private arrayElementLocation
        (elementHandle : ConcreteTypeHandle)
        (len : int)
        (index : int)
        : ByteStorageIdentity * int64
        =
        let state = state ()

        let zero, state =
            IlMachineState.cliTypeZeroOfHandle state baseClassTypes elementHandle

        let arr, state =
            IlMachineState.allocateArray (ConcreteTypeHandle.OneDimArrayZero elementHandle) (fun () -> zero) len state

        match ByrefContainer.tryOfRoot state.ManagedHeap (ByrefRoot.ArrayElement (arr, index)) with
        | None -> failwith "an array element must have a container"
        | Some (container, offset) ->
            container |> shouldEqual (ByteStorageIdentity.Array arr)
            container, offset

    /// The element stride is the element type's size — `sizeof<int>` and `sizeof<int64>` are
    /// the oracle, taken from outside PawPrint rather than from the array being measured.
    /// Two element types are used because a single one cannot distinguish "multiplied by the
    /// stride" from "multiplied by 4".
    [<Test>]
    let ``an array element is its index times the element stride`` () : unit =
        for index in 0..3 do
            arrayElementLocation int32Handle 4 index
            |> snd
            |> shouldEqual (int64 index * 4L)

            arrayElementLocation int64Handle 4 index
            |> snd
            |> shouldEqual (int64 index * 8L)

    /// An empty array has no cell to measure, and the stride is a property of the element type
    /// rather than of any stored value, so an out-of-range index still has a well-defined
    /// coordinate. Nothing dereferences it here; a byref past the end is address arithmetic,
    /// and refusing it belongs to the access path, not to this question.
    [<Test>]
    let ``an element index past the end of an empty array still has a coordinate`` () : unit =
        arrayElementLocation int32Handle 0 2 |> snd |> shouldEqual 8L

    /// A two-`int` struct under default layout: `A` at offset 0, `B` at offset 4.
    let private pairValueType (state : IlMachineState) : CliValueType =
        let field (name : string) (value : int) : CliField =
            {
                Id = FieldId.named name
                Name = name
                Contents = CliType.Numeric (CliNumericType.Int32 value)
                Offset = None
                Type = int32Handle
                MarshallingDescriptor = None
            }

        [ field "A" 111 ; field "B" 222 ]
        |> SynthesisedLayoutKind.ofFields
            baseClassTypes
            state.ConcreteTypes
            int32Handle
            Layout.Default
            System.Runtime.InteropServices.CharSet.Ansi

    let private stateWithPairObject () : IlMachineState * ManagedHeapAddress =
        let state = state ()
        let contents = pairValueType state
        let addr, state = IlMachineState.allocateManagedObject int32Handle contents state
        state, addr

    /// A field is a *view* into its object at the field's layout offset, not a container of
    /// its own. Both fields therefore report the same container, and are told apart only by
    /// their offsets — which is what lets two overlapping fields of an explicit-layout class
    /// be recognised as the same storage (#987).
    [<Test>]
    let ``a class field is its object at the field's layout offset`` () : unit =
        let state, addr = stateWithPairObject ()

        let locationOf (name : string) : ByteStorageIdentity * int64 =
            ByrefContainer.tryOfRoot state.ManagedHeap (ByrefRoot.HeapObjectField (addr, FieldId.named name))
            |> Option.get

        locationOf "A" |> shouldEqual (ByteStorageIdentity.HeapObject addr, 0L)
        locationOf "B" |> shouldEqual (ByteStorageIdentity.HeapObject addr, 4L)

    /// `StorageLocation.byteLocation` resolves a class-field root by adding the field's
    /// offset up front; the rewrite to the whole object with a leading `Field` projection is
    /// also constructible, so both shapes can be resolved and compared. Only the precise
    /// coordinates are compared: the two shapes carry *different* coarse keys, since
    /// `SharedStorageKey` separates a boxed value from a class field.
    [<Test>]
    let ``a field root and the object-plus-field rewrite give one coordinate`` () : unit =
        let state, addr = stateWithPairObject ()

        let resolve (root : ByrefRoot) (projs : ByrefProjection list) : ByteStorageIdentity * int64 =
            match StorageLocation.resolve baseClassTypes state (ManagedPointerSource.Byref (root, projs)) with
            | StorageLocation.LocationResolution.Located (_, Some precise) -> precise
            | other -> failwith $"expected a precise coordinate, got %O{other}"

        let viaFieldRoot = resolve (ByrefRoot.HeapObjectField (addr, FieldId.named "B")) []

        let viaRewrite =
            resolve (ByrefRoot.HeapValue addr) [ ByrefProjection.Field (FieldId.named "B") ]

        viaFieldRoot |> shouldEqual viaRewrite

        // Non-vacuity: `B` is not at offset 0, so an implementation that dropped the field's
        // offset would agree with the rewrite on `A` while differing here.
        viaFieldRoot |> shouldEqual (ByteStorageIdentity.HeapObject addr, 4L)

    /// The whole boxed value and its field at offset 0 name the same container, and only the
    /// offsets separate them. A per-field container would make these two incomparable.
    [<Test>]
    let ``a boxed value and its first field share a container`` () : unit =
        let state, addr = stateWithPairObject ()

        let containerOf (root : ByrefRoot) : ByteStorageIdentity =
            ByrefContainer.tryOfRoot state.ManagedHeap root |> Option.get |> fst

        containerOf (ByrefRoot.HeapValue addr)
        |> shouldEqual (containerOf (ByrefRoot.HeapObjectField (addr, FieldId.named "A")))
