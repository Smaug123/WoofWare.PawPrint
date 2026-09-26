namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// A byref into a localloc or native-heap block whose chain begins with `Field` steps — `p->O.I`
/// for a struct pointer `p` — viewed as `int` or `byte` and advanced by `Unsafe.Add`, for loads
/// and stores alike.
///
/// The struct is stored in the block as one typed cell, and a `Field` step is resolved against
/// that cell, so the byref's root must keep addressing the cell's start however far the view is
/// advanced: moving it would name storage that holds no struct. The trailing cursor then stays
/// relative to the field, and the access may land on a later field, an earlier one, a byte in the
/// middle of one, or outside the struct altogether.
///
/// The oracle is a flat byte image of the block, built from the struct's layout as stated here
/// rather than as PawPrint computes it.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestRawRootFieldPrefixByteView =

    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private bct : BaseClassTypes<DumpedAssembly> = Corelib.getBaseTypes corelib

    let private loadedAssemblies : LoadedAssemblies =
        LoadedAssemblies.ofAssemblies [ corelib ]

    let private concreteTypes : AllConcreteTypes =
        Corelib.concretizeAll loadedAssemblies bct AllConcreteTypes.Empty

    let private int32Handle : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle concreteTypes bct.Int32

    let private concreteTypeFor (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>) =
        ConcreteType.makeFromIdentity
            typeInfo.Identity
            typeInfo.Namespace
            typeInfo.Name
            ImmutableArray<ConcreteTypeHandle>.Empty

    /// The two views a guest steps through: `Unsafe.As<_, int>` and `Unsafe.As<_, byte>`.
    [<RequireQualifiedAccess>]
    type private View =
        | Int32
        | Byte

    let private viewSize (view : View) : int =
        match view with
        | View.Int32 -> 4
        | View.Byte -> 1

    let private viewType (view : View) : ConcreteType<ConcreteTypeHandle> =
        match view with
        | View.Int32 -> concreteTypeFor bct.Int32
        | View.Byte -> concreteTypeFor bct.Byte

    let private viewTemplate (view : View) : CliType =
        match view with
        | View.Int32 -> CliType.Numeric (CliNumericType.Int32 0)
        | View.Byte -> CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim 0uy))

    /// `struct Inner { int A; int B; int C; }`, `struct Outer { int Lead; Inner I; int Tail; }`,
    /// `struct Nest { int H0; int H1; Outer O; }`, all sequential with no padding. Each entry is
    /// a field path from `Nest` and the byte offset of the field it names.
    let private paths : (string list * int) list =
        [
            [], 0
            [ "H0" ], 0
            [ "H1" ], 4
            [ "O" ], 8
            [ "O" ; "Lead" ], 8
            [ "O" ; "I" ], 12
            [ "O" ; "I" ; "A" ], 12
            [ "O" ; "I" ; "B" ], 16
            [ "O" ; "I" ; "C" ], 20
            [ "O" ; "Tail" ], 24
        ]

    let private nestSize : int = 28

    let private int32Field (name : string) (value : int) : CliField =
        {
            Id = FieldId.named name
            Name = name
            Contents = CliType.Numeric (CliNumericType.Int32 value)
            Offset = None
            Type = int32Handle
            MarshallingDescriptor = None
        }

    let private structField (name : string) (value : CliValueType) : CliField =
        {
            Id = FieldId.named name
            Name = name
            Contents = CliType.ValueType value
            Offset = None
            Type = int32Handle
            MarshallingDescriptor = None
        }

    let private ofFields (fields : CliField list) : CliValueType =
        SynthesisedLayoutKind.ofFields
            bct
            concreteTypes
            int32Handle
            Layout.Default
            System.Runtime.InteropServices.CharSet.Ansi
            fields

    /// `Nest` holding `values`, in field order.
    let private nest (values : int[]) : CliType =
        let inner =
            ofFields
                [
                    int32Field "A" values.[3]
                    int32Field "B" values.[4]
                    int32Field "C" values.[5]
                ]

        let outer =
            ofFields
                [
                    int32Field "Lead" values.[2]
                    structField "I" inner
                    int32Field "Tail" values.[6]
                ]

        ofFields
            [
                int32Field "H0" values.[0]
                int32Field "H1" values.[1]
                structField "O" outer
            ]
        |> CliType.ValueType

    [<RequireQualifiedAccess>]
    type private RootKind =
        | Stack
        | Native

    type private Case =
        {
            RootKind : RootKind
            /// Bytes of plain `int` cells before the stored `Nest`, so that the root's own offset
            /// is not always zero.
            NestAt : int
            NestValues : int[]
            Path : string list
            PathOffset : int
            View : View
            /// The `Unsafe.Add` count, in units of the view.
            Step : int
            Written : int
        }

    /// The block holds `NestAt / 4` leading `int` cells, the `Nest`, and two trailing `int` cells.
    let private blockSize (case : Case) : int = case.NestAt + nestSize + 8

    let private initialImage (case : Case) : byte[] =
        let image = Array.zeroCreate<byte> (blockSize case)

        for slot in 0 .. blockSize case / 4 - 1 do
            let value =
                let offset = slot * 4

                if offset >= case.NestAt && offset < case.NestAt + nestSize then
                    case.NestValues.[(offset - case.NestAt) / 4]
                else
                    1000 + slot

            BitConverter.GetBytes(value).CopyTo (image, slot * 4)

        image

    let private accessAddress (case : Case) : int =
        case.NestAt + case.PathOffset + case.Step * viewSize case.View

    let private caseGen : Gen<Case> =
        gen {
            let! rootKind = Gen.elements [ RootKind.Stack ; RootKind.Native ]
            let! nestAt = Gen.elements [ 0 ; 4 ; 8 ]
            let! nestValues = Gen.arrayOfLength 7 (Gen.choose (-100000, 100000))
            let! path, pathOffset = Gen.elements paths
            let! view = Gen.elements [ View.Int32 ; View.Byte ]
            let size = viewSize view
            let total = nestAt + nestSize + 8
            let origin = nestAt + pathOffset
            // Every step whose access stays inside the block; an `int` view stays `int`-aligned,
            // as every origin here is.
            let! step = Gen.choose (-(origin / size), (total - size - origin) / size)
            let! written = Gen.choose (-100000, 100000)

            return
                {
                    RootKind = rootKind
                    NestAt = nestAt
                    NestValues = nestValues
                    Path = path
                    PathOffset = pathOffset
                    View = view
                    Step = step
                    Written = written
                }
        }

    let private methodFrame (state : IlMachineState) : IlMachineState * ThreadId =
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
                    (ImmutableArray.Create (CliType.ObjectRef None))
                    None
            with
            | Ok methodState -> methodState
            | Error missing -> failwith $"Unexpected missing assembly references creating a frame: %O{missing}"

        let thread = ThreadId.ThreadId 0

        { state with
            ThreadState = Map.empty |> Map.add thread (ThreadState.New methodState)
        },
        thread

    /// The state holding the block, and a byref to the block's byte 0.
    let private allocate (case : Case) : IlMachineState * ManagedPointerSource =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let state =
            { IlMachineState.initial loggerFactory ImmutableArray.Empty corelib with
                ConcreteTypes = concreteTypes
            }

        match case.RootKind with
        | RootKind.Native ->
            IlMachineState.allocateNativeMemory MemoryBlockInitialization.ZeroInitialized (blockSize case) state
            |> fun (ptr, state) -> state, ptr
        | RootKind.Stack ->
            let state, thread = methodFrame state

            IlMachineState.allocateStackMemory thread MemoryBlockInitialization.ZeroInitialized (blockSize case) state
            |> fun (ptr, state) -> state, ptr

    let private atByte (offset : int) (blockStart : ManagedPointerSource) : ManagedPointerSource =
        match blockStart with
        | ManagedPointerSource.Byref {
                                         Root = ByrefRoot.NativeMemoryByte (block, 0)
                                         Projections = []
                                     } ->
            ManagedPointerSource.Byref
                {
                    Root = ByrefRoot.NativeMemoryByte (block, offset)
                    Projections = []
                }
        | ManagedPointerSource.Byref {
                                         Root = ByrefRoot.StackMemoryByte (thread, frame, block, 0)
                                         Projections = []
                                     } ->
            ManagedPointerSource.Byref
                {
                    Root = ByrefRoot.StackMemoryByte (thread, frame, block, offset)
                    Projections = []
                }
        | other -> failwith $"expected a byref to byte 0 of a fresh block, got %O{other}"

    /// The block's contents stored the way a guest stores them: a typed cell per `*p = ...`.
    let private populate (case : Case) (blockStart : ManagedPointerSource) (state : IlMachineState) : IlMachineState =
        let intCell (offset : int) (state : IlMachineState) : IlMachineState =
            IlMachineState.writeManagedByrefBytesOrTypedCell
                bct
                state
                (atByte offset blockStart)
                (CliType.Numeric (CliNumericType.Int32 (1000 + offset / 4)))

        let state =
            (state, [ 0..4 .. case.NestAt - 4 ]) ||> List.fold (fun s o -> intCell o s)

        let state =
            IlMachineState.writeManagedByrefBytesOrTypedCell
                bct
                state
                (atByte case.NestAt blockStart)
                (nest case.NestValues)

        (state, [ case.NestAt + nestSize .. 4 .. blockSize case - 4 ])
        ||> List.fold (fun s o -> intCell o s)

    /// The byref a guest's `Unsafe.Add(ref Unsafe.As<_, T>(ref p->Path), Step)` produces, built
    /// through the production constructors.
    let private build (case : Case) (blockStart : ManagedPointerSource) (state : IlMachineState) =
        (atByte case.NestAt blockStart, case.Path)
        ||> List.fold (fun ptr name ->
            ManagedPointerSource.appendProjection (ByrefProjection.Field (FieldId.named name)) ptr
        )
        |> ManagedPointerByteView.addByteOffset state (viewType case.View) (case.Step * viewSize case.View)

    let private rootOffset (ptr : ManagedPointerSource) : int =
        match ptr with
        | ManagedPointerSource.Byref {
                                         Root = ByrefRoot.NativeMemoryByte (_, offset)
                                         Projections = _
                                     }
        | ManagedPointerSource.Byref {
                                         Root = ByrefRoot.StackMemoryByte (_, _, _, offset)
                                         Projections = _
                                     } -> offset
        | other -> failwith $"expected a raw-memory byref, got %O{other}"

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 1000

    [<Test>]
    let ``a raw root stays at its struct while a field prefix follows it`` () : unit =
        let property (case : Case) : unit =
            let state, blockStart = allocate case
            let ptr = build case blockStart state

            if List.isEmpty case.Path then
                // With nothing to anchor, the whole displacement folds into the root.
                let root =
                    match atByte (accessAddress case) blockStart with
                    | ManagedPointerSource.Byref {
                                                     Root = root
                                                     Projections = _
                                                 } -> root
                    | other -> failwith $"unreachable: %O{other}"

                ptr
                |> shouldEqual (
                    ManagedPointerSource.Byref
                        {
                            Root = root
                            Projections = [ ByrefProjection.ReinterpretAs (viewType case.View) ]
                        }
                )
            else
                rootOffset ptr |> shouldEqual case.NestAt

        Check.One (config, Prop.forAll (Arb.fromGen caseGen) property)

    [<Test>]
    let ``a load through a field-prefixed raw byref reads the byte image`` () : unit =
        let property (case : Case) : unit =
            let state, blockStart = allocate case
            let state = populate case blockStart state
            let ptr = build case blockStart state
            let image = initialImage case
            let address = accessAddress case

            let expected =
                match case.View with
                | View.Int32 -> CliType.Numeric (CliNumericType.Int32 (BitConverter.ToInt32 (image, address)))
                | View.Byte -> CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim image.[address]))

            IlMachineState.readManagedByrefBytesAs bct state ptr (viewTemplate case.View)
            |> shouldEqual expected

        Check.One (config, Prop.forAll (Arb.fromGen caseGen) property)

    [<Test>]
    let ``a store through a field-prefixed raw byref changes exactly its bytes`` () : unit =
        let property (case : Case) : unit =
            let state, blockStart = allocate case
            let state = populate case blockStart state
            let ptr = build case blockStart state
            let image = initialImage case
            let address = accessAddress case

            let written, writtenBytes =
                match case.View with
                | View.Int32 -> CliType.Numeric (CliNumericType.Int32 case.Written), BitConverter.GetBytes case.Written
                | View.Byte ->
                    let b = byte<int> (case.Written &&& 0xFF)
                    CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim b)), [| b |]

            let state = IlMachineState.writeManagedByrefBytesOrTypedCell bct state ptr written
            writtenBytes.CopyTo (image, address)

            // Read the block back a byte at a time from bare roots, which name no field and so
            // depend on nothing the byref under test decided.
            let actual =
                Array.init
                    (blockSize case)
                    (fun offset ->
                        match
                            IlMachineState.readManagedByrefBytesAs
                                bct
                                state
                                (atByte offset blockStart)
                                (viewTemplate View.Byte)
                        with
                        | CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim b)) -> b
                        | other -> failwith $"byte %d{offset} read back as %O{other}"
                    )

            actual |> shouldEqual image

        Check.One (config, Prop.forAll (Arb.fromGen caseGen) property)
