namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.Runtime.InteropServices
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Loads and stores through a byte view over a struct holding a pointer, which has no byte image,
/// landing anywhere in the storage that holds the struct: on a field of the struct the view was
/// taken over, on a field of an enclosing struct, or outside the struct altogether, in a neighbour
/// of another type in the same localloc or native-heap block, or in another element of the same
/// array.
///
/// Such storage cannot be read or written bytewise, so an access to it is served by naming the
/// cell it lands on: the innermost cell on the byref's own field chain that contains the access
/// or, when the access leaves that chain, the cell of the container it lands in. The oracle is a
/// byte image of the container built from the layout as stated here rather than as PawPrint
/// computes it, together with the rule that decides whether an access has an answer at all:
///
/// * the innermost enclosing cell, looking first along the byref's field chain and then in its
///   container, holds no pointer: served, with the image's bytes;
/// * that cell holds a pointer, and the access is exactly one of its `int` fields viewed as an
///   `int`: served, with that field's bytes;
/// * anything else touching a cell that holds a pointer: refused.
///
/// A store has one more way to be served. An all-zero store through an array element, with no
/// field chain before its view, is written by the array's own byte writer, which clears the bytes
/// it covers rather than naming a cell; that is how `Array.Clear` empties an array of structs
/// holding references. Such a store is served, nulling any pointer it covers whole, unless it
/// covers only part of a pointer, which is refused.
///
/// So a served access always returns the image's bytes, a store changes exactly its bytes and
/// leaves every pointer it does not clear as it was, and no access reaching a pointer's bytes is
/// ever served except a store clearing the whole pointer. The refusals of accesses that miss the
/// pointer (a byte of an `int` field, say, or a clear that has a field chain) are where naming and
/// clearing stop rather than a claim about the real runtime, which serves them.
///
/// Each byref is built two ways. `Spelling.Unnormalised` spells it relative to the struct the view
/// was taken over, which is what the readers and writers must answer for whoever builds it.
/// `Spelling.AsUnsafeAddSpellsIt` builds it as a guest's `Unsafe.Add` does, through the byte-offset
/// normalisation, which keeps a localloc or native-heap root at its struct while a field chain
/// follows it, so the rule above applies unchanged.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestPointerStructByteViewCells =

    // Factory intentionally undisposed: corelib.Logger outlives this scope.
    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private bct : BaseClassTypes<DumpedAssembly> = Corelib.getBaseTypes corelib

    let private loadedAssemblies : LoadedAssemblies =
        LoadedAssemblies.ofAssemblies [ corelib ]

    let private concreteTypes : AllConcreteTypes =
        Corelib.concretizeAll loadedAssemblies bct AllConcreteTypes.Empty

    let private handleOf (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>) : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle concreteTypes typeInfo

    let private int32Handle : ConcreteTypeHandle = handleOf bct.Int32

    /// Stand-ins for the declared types of `Inner` and `Outer`; all that matters is that they
    /// differ from each other and from every view.
    let private innerDeclared : ConcreteTypeHandle = handleOf bct.RuntimeFieldHandle
    let private outerDeclared : ConcreteTypeHandle = handleOf bct.TypedReference

    let private typeOf (handle : ConcreteTypeHandle) : ConcreteType<ConcreteTypeHandle> =
        AllConcreteTypes.lookup handle concreteTypes
        |> Option.defaultWith (fun () -> failwith $"%O{handle} has no registry entry")

    [<RequireQualifiedAccess>]
    type private View =
        | Byte
        | Int32
        | Int64

    let private viewSize (view : View) : int =
        match view with
        | View.Byte -> 1
        | View.Int32 -> 4
        | View.Int64 -> 8

    let private viewType (view : View) : ConcreteType<ConcreteTypeHandle> =
        match view with
        | View.Byte -> typeOf (handleOf bct.Byte)
        | View.Int32 -> typeOf int32Handle
        | View.Int64 -> typeOf (handleOf bct.Int64)

    let private viewTemplate (view : View) : CliType =
        match view with
        | View.Byte -> CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim 0uy))
        | View.Int32 -> CliType.Numeric (CliNumericType.Int32 0)
        | View.Int64 -> CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L))

    let private viewValue (view : View) (bytes : byte[]) : CliType =
        match view with
        | View.Byte -> CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim bytes.[0]))
        | View.Int32 -> CliType.Numeric (CliNumericType.Int32 (BitConverter.ToInt32 (bytes, 0)))
        | View.Int64 -> CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim (BitConverter.ToInt64 (bytes, 0))))

    // `struct Inner { int* P; int A; int B; }` and
    // `struct Outer { int Lead; int Pad; Inner I; int Tail; int Last; }`, sequential with no
    // padding: `P` occupies bytes 8..16 of `Outer`.
    let private outerSize : int = 32

    /// The `int` fields of `Outer`, by path, with their offsets in `Outer`, in the order `Outer`
    /// holds them.
    let private intFields : (string list * int) list =
        [
            [ "Lead" ], 0
            [ "Pad" ], 4
            [ "I" ; "A" ], 16
            [ "I" ; "B" ], 20
            [ "Tail" ], 24
            [ "Last" ], 28
        ]

    /// The field chains a byref into `Outer` can take before its view, each with the extent of the
    /// cell it reaches and whether that cell holds the pointer.
    let private paths : (string list * int * int * bool) list =
        [
            [], 0, outerSize, true
            [ "Lead" ], 0, 4, false
            [ "Pad" ], 4, 4, false
            [ "I" ], 8, 16, true
            [ "I" ; "P" ], 8, 8, true
            [ "I" ; "A" ], 16, 4, false
            [ "I" ; "B" ], 20, 4, false
            [ "Tail" ], 24, 4, false
            [ "Last" ], 28, 4, false
        ]

    let private pathExtent (path : string list) : int * int * bool =
        paths
        |> List.pick (fun (p, offset, size, holdsPointer) ->
            if p = path then Some (offset, size, holdsPointer) else None
        )

    /// The offset and size of the pointer `I.P` in `Outer`.
    let private pointerExtent : int * int =
        let offset, size, _ = pathExtent [ "I" ; "P" ]
        offset, size

    let private field (name : string) (handle : ConcreteTypeHandle) (contents : CliType) : CliField =
        {
            Id = FieldId.named name
            Name = name
            Contents = contents
            Offset = None
            Type = handle
            MarshallingDescriptor = None
        }

    let private synthesise (declared : ConcreteTypeHandle) (fields : CliField list) : CliValueType =
        SynthesisedLayoutKind.ofFields bct concreteTypes declared Layout.Default CharSet.Ansi fields

    let private int32Cell (value : int) : CliType =
        CliType.Numeric (CliNumericType.Int32 value)

    /// An `Outer` holding `ints` in `intFields` order and `pointer` in `I.P`.
    let private outer (ints : int[]) (pointer : CliRuntimePointer) : CliType =
        let inner =
            synthesise
                innerDeclared
                [
                    field "P" (ConcreteTypeHandle.Pointer int32Handle) (CliType.RuntimePointer pointer)
                    field "A" int32Handle (int32Cell ints.[2])
                    field "B" int32Handle (int32Cell ints.[3])
                ]

        synthesise
            outerDeclared
            [
                field "Lead" int32Handle (int32Cell ints.[0])
                field "Pad" int32Handle (int32Cell ints.[1])
                field "I" innerDeclared (CliType.ValueType inner)
                field "Tail" int32Handle (int32Cell ints.[4])
                field "Last" int32Handle (int32Cell ints.[5])
            ]
        |> CliType.ValueType

    /// One cell of the storage holding the `Outer` the byref points into.
    [<RequireQualifiedAccess>]
    type private ModelCell =
        | Int32 of int
        | Int64 of int64
        /// Bytes no cell covers, which a zero-initialised block starts out reading as zero.
        | Gap of bytes : byte[]
        | Outer of ints : int[] * pointer : CliRuntimePointer

    let private cellSize (cell : ModelCell) : int =
        match cell with
        | ModelCell.Int32 _ -> 4
        | ModelCell.Int64 _ -> 8
        | ModelCell.Gap bytes -> bytes.Length
        | ModelCell.Outer _ -> outerSize

    [<RequireQualifiedAccess>]
    type private RootKind =
        /// A localloc block of mixed cells, the byref rooted at the `Outer` in it.
        | Stack
        /// A native-heap block laid out as `Stack`'s is.
        | Native
        /// An argument slot holding the `Outer`: a typed root that is its own container.
        | Argument
        /// Element 1 of an `Outer[3]`.
        | ArrayElement
        /// A boxed `Outer`.
        | Boxed

    type private Case =
        {
            RootKind : RootKind
            /// The storage, as cells in address order.
            Cells : ModelCell list
            /// The index in `Cells` of the `Outer` the byref points into.
            RootCell : int
            Path : string list
            View : View
            /// The byte offset of the access from the start of `Path`'s cell.
            Displacement : int
            Written : byte[]
        }

    let private cellOffsets (cells : ModelCell list) : int list =
        cells
        |> List.scan (fun offset cell -> offset + cellSize cell) 0
        |> List.take cells.Length

    let private containerSize (cells : ModelCell list) : int = cells |> List.sumBy cellSize

    let private rootOffset (case : Case) : int =
        (cellOffsets case.Cells).[case.RootCell]

    /// Where the access lands, in bytes from the start of the container.
    let private accessAddress (case : Case) : int =
        let pathOffset, _, _ = pathExtent case.Path
        rootOffset case + pathOffset + case.Displacement

    /// The container's bytes, with `None` for a byte of a pointer.
    let private image (cells : ModelCell list) : byte option[] =
        [|
            for cell in cells do
                match cell with
                | ModelCell.Int32 v -> yield! BitConverter.GetBytes v |> Array.map Some
                | ModelCell.Int64 v -> yield! BitConverter.GetBytes v |> Array.map Some
                | ModelCell.Gap bytes -> yield! bytes |> Array.map Some
                | ModelCell.Outer (ints, _) ->
                    let bytes = Array.create<byte option> outerSize None

                    for (_, offset), value in List.zip intFields (List.ofArray ints) do
                        BitConverter.GetBytes (value)
                        |> Array.iteri (fun i b -> bytes.[offset + i] <- Some b)

                    yield! bytes
        |]

    /// Whether naming gives the access an answer; see the fixture's docstring.
    let private isServed (case : Case) : bool =
        let address = accessAddress case
        let size = viewSize case.View

        let contains (start : int) (extent : int) =
            start <= address && address + size <= start + extent

        let namesAnIntField (outerStart : int) =
            case.View = View.Int32
            && intFields |> List.exists (fun (_, offset) -> outerStart + offset = address)

        let root = rootOffset case

        // The field chain, innermost first: `Path`'s cell, each cell enclosing it, then `Outer`.
        let chain =
            [ for n in case.Path.Length .. -1 .. 0 -> pathExtent (List.take n case.Path) ]

        match
            chain
            |> List.tryFind (fun (offset, extent, _) -> contains (root + offset) extent)
        with
        | Some (_, _, false) -> true
        | Some (_, _, true) -> namesAnIntField root
        | None ->

        match case.RootKind with
        | RootKind.Argument
        | RootKind.Boxed -> false
        | RootKind.Stack
        | RootKind.Native
        | RootKind.ArrayElement ->

        let touched =
            List.zip (cellOffsets case.Cells) case.Cells
            |> List.filter (fun (offset, cell) -> offset < address + size && address < offset + cellSize cell)

        match touched with
        | [ offset, ModelCell.Outer _ ] -> contains offset outerSize && namesAnIntField offset
        | touched ->
            touched
            |> List.forall (fun (_, cell) ->
                match cell with
                | ModelCell.Outer _ -> false
                | _ -> true
            )

    /// What an access gets.
    [<RequireQualifiedAccess>]
    type private Outcome =
        | Served
        /// Refused, with an exception whose message contains `fragment`.
        | Refused of fragment : string

    /// A byte view is refused for naming no cell, by the reader or writer that found a cell with no
    /// byte image where it needed bytes.
    let private refusedByteView : Outcome = Outcome.Refused "refusing byte view"

    let private loadOutcome (case : Case) : Outcome =
        if isServed case then Outcome.Served else refusedByteView

    /// Whether a store is a clear; see the fixture's docstring. A field chain before the view sends
    /// the store to the writer that lifts through the chain, which does not clear.
    let private isClear (case : Case) : bool =
        case.RootKind = RootKind.ArrayElement
        && List.isEmpty case.Path
        && case.Written |> Array.forall (fun b -> b = 0uy)

    let private storeOutcome (case : Case) : Outcome =
        if isClear case then
            let address = accessAddress case
            let pointerOffset, pointerSize = pointerExtent
            let pointerStart = rootOffset case + pointerOffset
            let pointerEnd = pointerStart + pointerSize
            let accessEnd = address + viewSize case.View
            let overlapsPointer = address < pointerEnd && pointerStart < accessEnd
            let coversPointer = address <= pointerStart && pointerEnd <= accessEnd

            if overlapsPointer && not coversPointer then
                Outcome.Refused "refusing to zero the partial range"
            else
                Outcome.Served
        else
            loadOutcome case

    let private pointerGen : Gen<CliRuntimePointer> =
        Gen.oneof
            [
                Gen.choose (1, 1000000) |> Gen.map (int64<int> >> CliRuntimePointer.Verbatim)
                Gen.choose (0, 5)
                |> Gen.map (fun slot ->
                    ManagedPointerSource.Byref
                        {
                            Root = ByrefRoot.LocalVariable (ThreadId.ThreadId 0, FrameId.FrameId 0, uint16<int> slot)
                            Projections = []
                        }
                    |> CliRuntimePointer.Managed
                )
            ]

    let private outerGen : Gen<ModelCell> =
        gen {
            let! ints = Gen.arrayOfLength 6 (Gen.choose (-100000, 100000))
            let! pointer = pointerGen
            return ModelCell.Outer (ints, pointer)
        }

    let private neighbourGen : Gen<ModelCell> =
        Gen.oneof
            [
                Gen.choose (-100000, 100000) |> Gen.map ModelCell.Int32
                Gen.choose (-100000, 100000)
                |> Gen.map (fun v -> ModelCell.Int64 (int64<int> v * 65537L))
                Gen.elements [ 4 ; 8 ]
                |> Gen.map (fun size -> ModelCell.Gap (Array.zeroCreate size))
                outerGen
            ]

    [<RequireQualifiedAccess>]
    type private Spelling =
        /// `Field` steps, a `ReinterpretAs` and a `ByteOffset`, appended as they are.
        | Unnormalised
        /// What `Unsafe.Add` builds: the byte offset added through `ManagedPointerByteView`, which
        /// normalises it. An array root is not built this way here, because normalising moves it
        /// by whole elements and so changes which field chain the fixture's rule looks along.
        | AsUnsafeAddSpellsIt

    /// `forWrite` confines an access through an array to the element the byref is rooted at: a
    /// store that leaves the element is written through `writeArrayBytes`, which names no cell.
    let private caseGen (spelling : Spelling) (forWrite : bool) : Gen<Case> =
        gen {
            let! rootKind =
                Gen.elements
                    [
                        RootKind.Stack
                        RootKind.Native
                        RootKind.Argument
                        match spelling with
                        | Spelling.Unnormalised -> RootKind.ArrayElement
                        | Spelling.AsUnsafeAddSpellsIt -> ()
                        RootKind.Boxed
                    ]

            let! root = outerGen

            let! cells, rootCell =
                match rootKind with
                | RootKind.Stack
                | RootKind.Native ->
                    gen {
                        let! before = Gen.listOf neighbourGen |> Gen.resize 3
                        let! after = Gen.listOf neighbourGen |> Gen.resize 4
                        return before @ [ root ] @ after, before.Length
                    }
                | RootKind.ArrayElement ->
                    gen {
                        let! first = outerGen
                        let! last = outerGen
                        return [ first ; root ; last ], 1
                    }
                | RootKind.Argument
                | RootKind.Boxed -> Gen.constant ([ root ], 0)

            let origin = List.sum (List.map cellSize (List.take rootCell cells))

            let lowest, highest =
                match rootKind with
                | RootKind.Stack
                | RootKind.Native -> 0, containerSize cells
                | RootKind.ArrayElement when not forWrite -> 0, containerSize cells
                | RootKind.ArrayElement -> outerSize, 2 * outerSize
                | RootKind.Argument
                | RootKind.Boxed -> 0, outerSize

            // Chosen first, so that the byte view's many offsets do not crowd out the others.
            let! view = Gen.elements [ View.Byte ; View.Int32 ; View.Int64 ]
            let size = viewSize view

            // Every field chain and `Unsafe.Add` step count, in units of the view, whose access
            // lies in `[lowest, highest)`.
            let accesses =
                [
                    for path, pathOffset, _, _ in paths do
                        for steps in -containerSize cells .. containerSize cells do
                            let address = origin + pathOffset + steps * size

                            if lowest <= address && address + size <= highest then
                                yield path, steps
                ]

            let! path, steps = Gen.elements accesses

            // All zeros often enough for every clear to be reached: uniform bytes would make a clear
            // one store in 256 for a byte view, and never for a wider one.
            let! written =
                Gen.frequency
                    [
                        1, Gen.constant (Array.zeroCreate<byte> size)
                        3, Gen.arrayOfLength size (Gen.choose (0, 255) |> Gen.map byte<int>)
                    ]

            return
                {
                    RootKind = rootKind
                    Cells = cells
                    RootCell = rootCell
                    Path = path
                    View = view
                    Displacement = steps * size
                    Written = written
                }
        }

    let private methodFrame (arguments : ImmutableArray<CliType>) (state : IlMachineState) : IlMachineState * ThreadId =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let objectToString =
            bct.Object.Methods
            |> List.find (fun method -> method.Name = "ToString" && (MethodInfo.arity method = 0))

        let state, signature =
            IlMachineState.concretizeMethodSignature
                loggerFactory
                bct
                state
                corelib.DefinitionFullName
                ImmutableArray.Empty
                ImmutableArray.Empty
                objectToString.Signature

        let method =
            objectToString
            |> MethodInfo.mapTypeGenerics (fun _ -> failwith "System.Object::ToString is not type-generic")
            |> MethodInfo.mapMethodGenerics (fun _ _ -> failwith "System.Object::ToString is not method-generic")
            |> MethodInfo.setMethodVars (MethodBody.Il (MethodInstructions.onlyRet ())) signature

        let methodState =
            match
                MethodState.Empty
                    state.ConcreteTypes
                    bct
                    state._LoadedAssemblies
                    corelib
                    method
                    ImmutableArray.Empty
                    arguments
                    None
            with
            | Ok methodState -> methodState
            | Error missing -> failwith $"Unexpected missing assembly references creating a frame: %O{missing}"

        let thread = ThreadId.ThreadId 0

        { state with
            ThreadState = Map.empty |> Map.add thread (ThreadState.New methodState)
        },
        thread

    let private toCliType (cell : ModelCell) : CliType =
        match cell with
        | ModelCell.Int32 v -> int32Cell v
        | ModelCell.Int64 v -> CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim v))
        | ModelCell.Outer (ints, pointer) -> outer ints pointer
        | ModelCell.Gap bytes -> failwith $"a gap of %d{bytes.Length} bytes has no cell"

    /// The state holding the storage, and a byref to the start of the `Outer` the case is rooted
    /// at. A block's cells are stored the way a guest stores them: a typed cell per `*p = ...`.
    let private allocate (case : Case) : IlMachineState * ManagedPointerSource =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let state =
            { IlMachineState.initial loggerFactory ImmutableArray.Empty corelib with
                ConcreteTypes = concreteTypes
            }

        let populate (atByte : int -> ManagedPointerSource) (state : IlMachineState) : IlMachineState =
            List.zip (cellOffsets case.Cells) case.Cells
            |> List.fold
                (fun state (offset, cell) ->
                    match cell with
                    | ModelCell.Gap _ -> state
                    | cell ->
                        IlMachineState.writeManagedByrefBytesOrTypedCell
                            bct
                            state
                            (ManagedPointerSource.requireAddressed (atByte offset))
                            (toCliType cell)
                )
                state

        let rootValue = toCliType case.Cells.[case.RootCell]

        match case.RootKind with
        | RootKind.Native ->
            let ptr, state =
                IlMachineState.allocateNativeMemory
                    MemoryBlockInitialization.ZeroInitialized
                    (containerSize case.Cells)
                    state

            let block =
                match ptr with
                | ManagedPointerSource.Byref {
                                                 Root = ByrefRoot.NativeMemoryByte (block, 0)
                                                 Projections = []
                                             } -> block
                | other -> failwith $"expected a byref to byte 0 of a fresh block, got %O{other}"

            let atByte (offset : int) =
                ManagedPointerSource.Byref
                    {
                        Root = ByrefRoot.NativeMemoryByte (block, offset)
                        Projections = []
                    }

            populate atByte state, atByte (rootOffset case)
        | RootKind.Stack ->
            let state, thread =
                methodFrame (ImmutableArray.Create (CliType.ObjectRef None)) state

            let ptr, state =
                IlMachineState.allocateStackMemory
                    thread
                    MemoryBlockInitialization.ZeroInitialized
                    (containerSize case.Cells)
                    state

            let atByte =
                match ptr with
                | ManagedPointerSource.Byref {
                                                 Root = ByrefRoot.StackMemoryByte (thread, frame, block, 0)
                                                 Projections = []
                                             } ->
                    fun (offset : int) ->
                        ManagedPointerSource.Byref
                            {
                                Root = ByrefRoot.StackMemoryByte (thread, frame, block, offset)
                                Projections = []
                            }
                | other -> failwith $"expected a byref to byte 0 of a fresh block, got %O{other}"

            populate atByte state, atByte (rootOffset case)
        | RootKind.Argument ->
            let state, thread = methodFrame (ImmutableArray.Create rootValue) state
            let frame = state.ThreadState.[thread].ActiveMethodState

            state,
            ManagedPointerSource.Byref
                {
                    Root = ByrefRoot.Argument (thread, frame, 0us)
                    Projections = []
                }
        | RootKind.ArrayElement ->
            let values = case.Cells |> List.map toCliType

            let arr, state =
                IlMachineState.allocateArray
                    (ConcreteTypeHandle.OneDimArrayZero outerDeclared)
                    (fun () -> List.head values)
                    values.Length
                    state

            let state =
                (state, List.indexed values)
                ||> List.fold (fun state (index, value) -> IlMachineState.setArrayValue arr value index state)

            state,
            ManagedPointerSource.Byref
                {
                    Root = ByrefRoot.ArrayElement (arr, case.RootCell)
                    Projections = []
                }
        | RootKind.Boxed ->
            let contents =
                match rootValue with
                | CliType.ValueType vt -> vt
                | other -> failwith $"an Outer is a value type, got %O{other}"

            let addr, state = IlMachineState.allocateManagedObject outerDeclared contents state

            state,
            ManagedPointerSource.Byref
                {
                    Root = ByrefRoot.HeapValue addr
                    Projections = []
                }

    /// `Unsafe.Add(ref Unsafe.As<_, View>(ref root.Path), steps)`, spelt as `spelling` says.
    let private build
        (spelling : Spelling)
        (case : Case)
        (state : IlMachineState)
        (atRoot : ManagedPointerSource)
        : ManagedPointerSource
        =
        let atPath =
            (atRoot, case.Path)
            ||> List.fold (fun ptr name ->
                ManagedPointerSource.appendProjection (ByrefProjection.Field (FieldId.named name)) ptr
            )

        match spelling with
        | Spelling.Unnormalised ->
            atPath
            |> ManagedPointerSource.appendProjection (ByrefProjection.ReinterpretAs (viewType case.View))
            |> ManagedPointerSource.appendProjection (ByrefProjection.ByteOffset case.Displacement)
        | Spelling.AsUnsafeAddSpellsIt ->
            ManagedPointerByteView.addByteOffset state (viewType case.View) case.Displacement atPath

    /// What a cell of the storage holds, as far as the guest can tell: an `Outer`'s `int` fields
    /// and its pointer, rather than the `CliType`, which also records when each field was written.
    [<RequireQualifiedAccess>]
    type private Observed =
        | Int32 of int
        | Int64 of int64
        | Bytes of byte[]
        | Outer of ints : int list * pointer : CliType

    let private observe (value : CliType) : Observed =
        match value with
        | CliType.Numeric (CliNumericType.Int32 v) -> Observed.Int32 v
        | CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim v)) -> Observed.Int64 v
        | CliType.ValueType _ ->
            let at (path : string list) =
                (value, path)
                ||> List.fold (fun v name -> CliType.getFieldById (FieldId.named name) v)

            let ints =
                intFields
                |> List.map (fun (path, _) ->
                    match at path with
                    | CliType.Numeric (CliNumericType.Int32 v) -> v
                    | other -> failwith $"field %A{path} read back as %O{other}"
                )

            Observed.Outer (ints, at [ "I" ; "P" ])
        | other -> failwith $"unexpected cell %O{other}"

    /// Every cell of the storage as it now stands, read through byrefs that name no view.
    let private readBack (case : Case) (atRoot : ManagedPointerSource) (state : IlMachineState) : Observed list =
        let root =
            match atRoot with
            | ManagedPointerSource.Byref {
                                             Root = root
                                             Projections = []
                                         } -> root
            | other -> failwith $"expected a bare root, got %O{other}"

        let cellRoot (index : int) (offset : int) : ManagedPointerSource =
            let delta = offset - rootOffset case

            let root =
                match root with
                | ByrefRoot.StackMemoryByte (thread, frame, block, rootAt) ->
                    ByrefRoot.StackMemoryByte (thread, frame, block, rootAt + delta)
                | ByrefRoot.NativeMemoryByte (block, rootAt) -> ByrefRoot.NativeMemoryByte (block, rootAt + delta)
                | ByrefRoot.ArrayElement (arr, _) -> ByrefRoot.ArrayElement (arr, index)
                | other when delta = 0 -> other
                | other -> failwith $"%O{other} holds one cell, so nothing is at %d{offset}"

            ManagedPointerSource.Byref
                {
                    Root = root
                    Projections = []
                }

        List.zip (cellOffsets case.Cells) case.Cells
        |> List.indexed
        |> List.map (fun (index, (offset, cell)) ->
            match cell with
            | ModelCell.Gap bytes ->
                Array.init
                    bytes.Length
                    (fun i ->
                        match
                            IlMachineState.readManagedByrefBytesAs
                                bct
                                state
                                (ManagedPointerSource.requireAddressed (cellRoot index (offset + i)))
                                (viewTemplate View.Byte)
                        with
                        | CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim b)) -> b
                        | other -> failwith $"byte %d{offset + i} read back as %O{other}"
                    )
                |> Observed.Bytes
            | _ ->
                IlMachineState.readManagedByref
                    bct
                    state
                    (ManagedPointerSource.requireAddressed (cellRoot index offset))
                |> observe
        )

    let private expectedObserved (cell : ModelCell) : Observed =
        match cell with
        | ModelCell.Int32 v -> Observed.Int32 v
        | ModelCell.Int64 v -> Observed.Int64 v
        | ModelCell.Gap bytes -> Observed.Bytes bytes
        | ModelCell.Outer (ints, pointer) -> Observed.Outer (List.ofArray ints, CliType.RuntimePointer pointer)

    /// `cells` with `bytes` stored at `address`. The store must lie in cells with a byte image, be
    /// exactly an `int` field of an `Outer`, or be a clear covering any pointer it touches whole,
    /// which is what `storeOutcome` serves.
    let private store (cells : ModelCell list) (address : int) (bytes : byte[]) : ModelCell list =
        // `current`'s bytes, which start at `start`, with the stored bytes laid over them.
        let overlay (start : int) (current : byte[]) : byte[] =
            let updated = Array.copy current

            for i in 0 .. bytes.Length - 1 do
                let at = address + i - start

                if at >= 0 && at < updated.Length then
                    updated.[at] <- bytes.[i]

            updated

        List.zip (cellOffsets cells) cells
        |> List.map (fun (offset, cell) ->
            match cell with
            | ModelCell.Int32 v -> ModelCell.Int32 (BitConverter.ToInt32 (overlay offset (BitConverter.GetBytes v), 0))
            | ModelCell.Int64 v -> ModelCell.Int64 (BitConverter.ToInt64 (overlay offset (BitConverter.GetBytes v), 0))
            | ModelCell.Gap bytes -> ModelCell.Gap (overlay offset bytes)
            | ModelCell.Outer (ints, pointer) ->
                let ints =
                    List.zip intFields (List.ofArray ints)
                    |> List.map (fun ((_, fieldOffset), value) ->
                        BitConverter.ToInt32 (overlay (offset + fieldOffset) (BitConverter.GetBytes value), 0)
                    )
                    |> Array.ofList

                let pointerOffset, pointerSize = pointerExtent
                let pointerStart = offset + pointerOffset
                let pointerEnd = pointerStart + pointerSize
                let storeEnd = address + bytes.Length

                let pointer =
                    if storeEnd <= pointerStart || pointerEnd <= address then
                        pointer
                    elif
                        address <= pointerStart
                        && pointerEnd <= storeEnd
                        && bytes |> Array.forall (fun b -> b = 0uy)
                    then
                        CliRuntimePointer.Managed ManagedPointerSource.Null
                    else
                        failwith
                            $"a store of %A{bytes} at %d{address} reaches the pointer at %d{pointerStart} without clearing it whole"

                ModelCell.Outer (ints, pointer)
        )

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 1000

    let private assertRefused (fragment : string) (action : unit -> 'a) : unit =
        let ex = Assert.Throws<Exception> (fun () -> action () |> ignore)
        ex.Message |> shouldContainText fragment

    let private loadProperty (spelling : Spelling) : unit =
        let property (case : Case) : unit =
            let state, atRoot = allocate case
            let ptr = build spelling case state atRoot
            let address = accessAddress case

            let read () =
                IlMachineState.readManagedByrefBytesAs
                    bct
                    state
                    (ManagedPointerSource.requireAddressed ptr)
                    (viewTemplate case.View)

            match loadOutcome case with
            | Outcome.Served ->
                let bytes =
                    (image case.Cells).[address .. address + viewSize case.View - 1]
                    |> Array.map (Option.defaultWith (fun () -> failwith "a served access reached a pointer byte"))

                read () |> shouldEqual (viewValue case.View bytes)
            | Outcome.Refused fragment -> assertRefused fragment read

        Check.One (config, Prop.forAll (Arb.fromGen (caseGen spelling false)) property)

    let private checkStore (spelling : Spelling) (case : Case) : unit =
        let state, atRoot = allocate case
        let ptr = build spelling case state atRoot

        let write () =
            IlMachineState.writeManagedByrefBytesOrTypedCell
                bct
                state
                (ManagedPointerSource.requireAddressed ptr)
                (viewValue case.View case.Written)

        match storeOutcome case with
        | Outcome.Served ->
            let state = write ()

            readBack case atRoot state
            |> shouldEqual (store case.Cells (accessAddress case) case.Written |> List.map expectedObserved)
        | Outcome.Refused fragment -> assertRefused fragment write

    let private storeProperty (spelling : Spelling) : unit =
        Check.One (config, Prop.forAll (Arb.fromGen (caseGen spelling true)) (checkStore spelling))

    [<Test>]
    let ``a load through a byte view over a pointer-holding struct reads the image or is refused`` () : unit =
        loadProperty Spelling.Unnormalised

    [<Test>]
    let ``a store through a byte view over a pointer-holding struct changes exactly its bytes or is refused``
        ()
        : unit
        =
        storeProperty Spelling.Unnormalised

    [<Test>]
    let ``a load through a byte view spelt as Unsafe.Add spells it reads the image or is refused`` () : unit =
        loadProperty Spelling.AsUnsafeAddSpellsIt

    [<Test>]
    let ``a store through a byte view spelt as Unsafe.Add spells it changes exactly its bytes or is refused``
        ()
        : unit
        =
        storeProperty Spelling.AsUnsafeAddSpellsIt

    /// A store of zeros through element 1 of an `Outer[3]`.
    let private arrayClear (path : string list) (view : View) (displacement : int) : Case =
        let outerCell (slot : int) =
            ModelCell.Outer (
                [| 67673 ; 57698 ; -91588 ; -68716 ; -42320 ; -94121 |],
                ManagedPointerSource.Byref
                    {
                        Root = ByrefRoot.LocalVariable (ThreadId.ThreadId 0, FrameId.FrameId 0, uint16<int> slot)
                        Projections = []
                    }
                |> CliRuntimePointer.Managed
            )

        {
            RootKind = RootKind.ArrayElement
            Cells = [ outerCell 0 ; outerCell 5 ; outerCell 2 ]
            RootCell = 1
            Path = path
            View = view
            Displacement = displacement
            Written = Array.zeroCreate (viewSize view)
        }

    [<Test>]
    let ``a zero byte stored into an int field through an array element clears it`` () : unit =
        let case = arrayClear [] View.Byte 4
        storeOutcome case |> shouldEqual Outcome.Served
        checkStore Spelling.Unnormalised case

    [<Test>]
    let ``zeros stored over a whole pointer through an array element null it`` () : unit =
        let case = arrayClear [] View.Int64 8
        storeOutcome case |> shouldEqual Outcome.Served
        checkStore Spelling.Unnormalised case

    [<Test>]
    let ``a zero byte stored into a pointer through an array element is refused`` () : unit =
        let case = arrayClear [] View.Byte 8

        storeOutcome case
        |> shouldEqual (Outcome.Refused "refusing to zero the partial range")

        checkStore Spelling.Unnormalised case

    [<Test>]
    let ``a zero byte stored through an array element and a field chain is refused`` () : unit =
        let case = arrayClear [ "Lead" ] View.Byte 4
        storeOutcome case |> shouldEqual refusedByteView
        checkStore Spelling.Unnormalised case
