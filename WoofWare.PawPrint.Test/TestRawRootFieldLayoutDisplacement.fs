namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// A `Field` step off a localloc or native-heap root is address arithmetic: `ldflda D::f` through
/// a `D*` is `offsetof(D, f)` bytes past the pointer, read from `D`'s layout, whatever the memory
/// there holds — a `D`, a value of some other type, or nothing yet.
///
/// Byrefs are built as a guest builds them — `Field` steps from a pointer at any byte of the block,
/// then optionally `Unsafe.As` to an `int` or `byte` view and `Unsafe.Add` — over blocks holding a
/// mixture of values: `int`s, `long`s, `Outer`s, `Holder`s and `Twin`s (which hold a pointer, and
/// are laid out alike), and bytes nothing has written. The field chain's declaring types need not
/// match what the block holds where the byref points. The oracle is a byte model of the block with the struct layouts stated here
/// rather than as PawPrint computes them, and says:
///
/// * a byref's flat address is its root's byte plus the stated field offsets plus its cursor, and
///   `StorageLocation.resolve` reports exactly that, so `Unsafe.AreSame` of two byrefs is equality
///   of their addresses;
/// * an access touching no pointer is served with the model's bytes, and a store changes exactly
///   its own bytes;
/// * an access touching a pointer is served only when it is exactly the pointer or exactly one
///   `int` field of a stored `Holder`, and refused otherwise. The refusals of accesses that miss
///   the pointer but are not one field of the `Holder` are where naming stops, not a claim about
///   the real runtime;
/// * no store disturbs any other byte, and every pointer keeps its provenance.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestRawRootFieldLayoutDisplacement =

    let private loggerFactory = snd (LoggerFactory.makeTest ())

    let private corelibPath : string = typeof<obj>.Assembly.Location

    let private corelib : DumpedAssembly = Assembly.readFile loggerFactory corelibPath

    let private bct : BaseClassTypes<DumpedAssembly> = Corelib.getBaseTypes corelib

    let private corpusSource : string =
        """
namespace PawPrint.RawRootLayout;

public struct Inner { public int A; public int B; }
public struct Outer { public int Lead; public Inner I; public int Tail; }
public unsafe struct Holder { public int* P; public int A; public int B; }
public unsafe struct Twin { public int* Q; public int X; public int Y; }

[System.Runtime.InteropServices.StructLayout(System.Runtime.InteropServices.LayoutKind.Explicit)]
public unsafe struct Union
{
    [System.Runtime.InteropServices.FieldOffset(0)] public int* P;
    [System.Runtime.InteropServices.FieldOffset(0)] public nint N;
    [System.Runtime.InteropServices.FieldOffset(8)] public int A;
    [System.Runtime.InteropServices.FieldOffset(12)] public int B;
}

[System.Runtime.InteropServices.StructLayout(System.Runtime.InteropServices.LayoutKind.Explicit)]
public unsafe struct Nest
{
    [System.Runtime.InteropServices.FieldOffset(0)] public Holder H;
    [System.Runtime.InteropServices.FieldOffset(0)] public long L;
    [System.Runtime.InteropServices.FieldOffset(8)] public Union U;
    [System.Runtime.InteropServices.FieldOffset(24)] public int Tail;
}
"""

    let private corpusAssembly : DumpedAssembly =
        let bytes =
            Roslyn.compileAssembly
                "PawPrint.RawRootLayout"
                Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                []
                [ corpusSource ]

        use stream = new MemoryStream (bytes)
        AssemblyApi.read loggerFactory (Some "PawPrint.RawRootLayout.dll") stream

    let private typeInfoNamed (name : string) : TypeInfo<GenericParamFromMetadata, TypeDefn> =
        corpusAssembly.TypeDefs.Values
        |> Seq.find (fun t -> t.Name = name && t.Namespace = "PawPrint.RawRootLayout")

    let private concretize
        (state : IlMachineState)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : IlMachineState * ConcreteTypeHandle
        =
        let kind = LoadedTypeInfo.signatureTypeKind bct state._LoadedAssemblies typeInfo

        IlMachineTypeResolution.concretizeType
            loggerFactory
            bct
            state
            typeInfo.AssemblyFullName
            ImmutableArray.Empty
            ImmutableArray.Empty
            (TypeDefn.FromDefinition (typeInfo.Identity, kind))

    /// The state with the corpus loaded and its types concretized, and their handles.
    let private baseState, innerHandle, outerHandle, holderHandle, twinHandle, unionHandle, nestHandle =
        let dirs = ImmutableArray.Create (Path.GetDirectoryName corelibPath)
        let state = IlMachineState.initial loggerFactory dirs corelib

        let state =
            { state with
                ConcreteTypes = Corelib.concretizeAll state._LoadedAssemblies bct AllConcreteTypes.Empty
            }

        let state = state.WithLoadedAssembly corpusAssembly
        let state, inner = concretize state (typeInfoNamed "Inner")
        let state, outer = concretize state (typeInfoNamed "Outer")
        let state, holder = concretize state (typeInfoNamed "Holder")
        let state, twin = concretize state (typeInfoNamed "Twin")
        let state, union = concretize state (typeInfoNamed "Union")
        let state, nest = concretize state (typeInfoNamed "Nest")
        state, inner, outer, holder, twin, union, nest

    let private concreteType (handle : ConcreteTypeHandle) : ConcreteType<ConcreteTypeHandle> =
        AllConcreteTypes.lookup handle baseState.ConcreteTypes
        |> Option.defaultWith (fun () -> failwith $"%O{handle} has no registry entry")

    let private zeroOf (handle : ConcreteTypeHandle) : CliType =
        IlMachineManagedByref.zeroForConcreteType bct baseState (concreteType handle)

    let private fieldId (declaring : ConcreteTypeHandle) (name : string) : FieldId =
        let typeInfo =
            corpusAssembly.TypeDefs.Values
            |> Seq.find (fun t -> t.Identity = (concreteType declaring).Identity)

        let field = typeInfo.Fields |> List.find (fun f -> f.Name = name)
        FieldId.metadata declaring field.Handle field.Name

    /// The struct a field chain starts from.
    [<RequireQualifiedAccess>]
    type private Declaring =
        | Inner
        | Outer
        | Holder
        /// Laid out as `Holder` is, but a different type.
        | Twin
        /// Explicit layout: a pointer and a native int overlapping at 0.
        | Union
        /// Explicit layout: a `Holder` overlapping a `long` at 0, and a `Union` at 8 overlapping
        /// the `Holder`'s `int` fields.
        | Nest

    let private declaringHandle (declaring : Declaring) : ConcreteTypeHandle =
        match declaring with
        | Declaring.Inner -> innerHandle
        | Declaring.Outer -> outerHandle
        | Declaring.Holder -> holderHandle
        | Declaring.Twin -> twinHandle
        | Declaring.Union -> unionHandle
        | Declaring.Nest -> nestHandle

    /// What an access reads or writes: the view a guest's `Unsafe.As` names, or the type of the
    /// field a chain ends at.
    [<RequireQualifiedAccess>]
    type private Access =
        | Byte
        | Int32
        | Inner
        | Pointer

    let private accessSize (access : Access) : int =
        match access with
        | Access.Byte -> 1
        | Access.Int32 -> 4
        | Access.Inner -> 8
        | Access.Pointer -> 8

    /// Every field chain from each struct, with the byte offset of its end and the type there, as
    /// the C# above lays them out: sequential, with no padding.
    let private chains : (Declaring * string list * int * Access) list =
        [
            Declaring.Outer, [ "Lead" ], 0, Access.Int32
            Declaring.Outer, [ "I" ], 4, Access.Inner
            Declaring.Outer, [ "I" ; "A" ], 4, Access.Int32
            Declaring.Outer, [ "I" ; "B" ], 8, Access.Int32
            Declaring.Outer, [ "Tail" ], 12, Access.Int32
            Declaring.Holder, [ "P" ], 0, Access.Pointer
            Declaring.Holder, [ "A" ], 8, Access.Int32
            Declaring.Holder, [ "B" ], 12, Access.Int32
            Declaring.Twin, [ "Q" ], 0, Access.Pointer
            Declaring.Twin, [ "X" ], 8, Access.Int32
            Declaring.Twin, [ "Y" ], 12, Access.Int32
            Declaring.Inner, [ "A" ], 0, Access.Int32
            Declaring.Inner, [ "B" ], 4, Access.Int32
        ]

    /// The `FieldId`s of a chain, each keyed to the struct that declares it.
    let private chainFieldIds (declaring : Declaring) (names : string list) : FieldId list =
        let rec go (handle : ConcreteTypeHandle) (names : string list) : FieldId list =
            match names with
            | [] -> []
            | "I" :: rest when handle = outerHandle -> fieldId handle "I" :: go innerHandle rest
            | "H" :: rest when handle = nestHandle -> fieldId handle "H" :: go holderHandle rest
            | "U" :: rest when handle = nestHandle -> fieldId handle "U" :: go unionHandle rest
            | name :: rest -> fieldId handle name :: go handle rest

        go (declaringHandle declaring) names

    let private blockSize : int = 48

    /// A value stored in the block, as the typed cell a guest's `*(T*)q = v` installs.
    [<RequireQualifiedAccess>]
    type private Stored =
        | Int32 of int
        | Int64 of int64
        | Outer of lead : int * a : int * b : int * tail : int
        /// A `Holder`, or a `Twin` when `twin` is set.
        | Holder of twin : bool * pointer : CliRuntimePointer * a : int * b : int

    let private storedSize (stored : Stored) : int =
        match stored with
        | Stored.Int32 _ -> 4
        | Stored.Int64 _ -> 8
        | Stored.Outer _
        | Stored.Holder _ -> 16

    /// One typed cell of the model. The byte-addressable ones are distinguished by whether they are
    /// structs, because a pointer store may replace a same-width scalar cell but not a struct.
    [<RequireQualifiedAccess>]
    type private ModelCell =
        | Scalar of size : int
        | Struct of size : int
        /// A `Holder`, or a `Twin` when `twin` is set.
        | Holder of twin : bool * pointer : CliRuntimePointer
        | Pointer of CliRuntimePointer

    let private modelCellSize (cell : ModelCell) : int =
        match cell with
        | ModelCell.Scalar size
        | ModelCell.Struct size -> size
        | ModelCell.Holder _ -> 16
        | ModelCell.Pointer _ -> 8

    let private hasNoByteImage (cell : ModelCell) : bool =
        match cell with
        | ModelCell.Scalar _
        | ModelCell.Struct _ -> false
        | ModelCell.Holder _
        | ModelCell.Pointer _ -> true

    /// The block: its cells by offset, and every byte not inside a pointer.
    type private Model =
        {
            Cells : Map<int, ModelCell>
            Image : byte[]
        }

    let private intersecting (address : int) (size : int) (model : Model) : (int * ModelCell) list =
        model.Cells
        |> Map.toList
        |> List.filter (fun (offset, cell) -> offset < address + size && address < offset + modelCellSize cell)

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

    let private storedGen : Gen<Stored option> =
        let int = Gen.choose (-100000, 100000)

        Gen.frequency
            [
                2, Gen.constant None
                1, int |> Gen.map (Stored.Int32 >> Some)
                1, int |> Gen.map (fun v -> Stored.Int64 (int64<int> v * 65537L) |> Some)
                2, Gen.map4 (fun a b c d -> Stored.Outer (a, b, c, d) |> Some) int int int int
                2,
                Gen.map4
                    (fun twin p a b -> Stored.Holder (twin, p, a, b) |> Some)
                    (Gen.elements [ false ; true ])
                    pointerGen
                    int
                    int
            ]

    /// A block's contents in address order, each at a 4-aligned offset; `None` is 4 bytes nothing
    /// writes.
    let private contentsGen : Gen<(int * Stored) list> =
        let rec go (offset : int) : Gen<(int * Stored) list> =
            if offset >= blockSize then
                Gen.constant []
            else
                gen {
                    let! stored = storedGen

                    match stored with
                    | Some s when offset + storedSize s <= blockSize ->
                        let! rest = go (offset + storedSize s)
                        return (offset, s) :: rest
                    | _ -> return! go (offset + 4)
                }

        go 0

    [<RequireQualifiedAccess>]
    type private RootKind =
        | Stack
        | Native

    /// A byref as a guest spells it: a pointer at byte `Root` of the block, then either a field
    /// chain from `Declaring` or none, then optionally a view advanced by `Unsafe.Add`.
    type private ByrefSpec =
        {
            Root : int
            Chain : (Declaring * string list) option
            View : (Access * int) option
        }

    let private chainEnd (spec : ByrefSpec) : int * Access option =
        match spec.Chain with
        | None -> 0, None
        | Some (declaring, names) ->
            chains
            |> List.pick (fun (d, n, offset, access) ->
                if d = declaring && n = names then
                    Some (offset, Some access)
                else
                    None
            )

    let private address (spec : ByrefSpec) : int =
        let chainOffset, _ = chainEnd spec

        let viewOffset =
            match spec.View with
            | None -> 0
            | Some (view, steps) -> steps * accessSize view

        spec.Root + chainOffset + viewOffset

    let private accessOf (spec : ByrefSpec) : Access =
        match spec.View, chainEnd spec with
        | Some (view, _), _ -> view
        | None, (_, Some access) -> access
        | None, (_, None) -> failwith "a byref with neither a field chain nor a view has no access type"

    /// Every byref whose access lies in the block, for the given views.
    let private byrefGen (views : Access list) : Gen<ByrefSpec> =
        let roots = [ 0..4 .. blockSize - 4 ]

        // A field's own type, as `ldfld`/`stfld` or `ldflda` then `ldobj` access it.
        let fieldAccesses =
            [
                for root in roots do
                    for declaring, names, chainOffset, access in chains do
                        if root + chainOffset + accessSize access <= blockSize then
                            yield
                                {
                                    Root = root
                                    Chain = Some (declaring, names)
                                    View = None
                                }
            ]

        // `Unsafe.Add(ref Unsafe.As<_, V>(ref p->Chain), steps)`.
        let viewedAccesses =
            [
                for root in roots do
                    for declaring, names, chainOffset, _ in chains do
                        for view in views do
                            let size = accessSize view

                            for steps in -blockSize .. blockSize do
                                let a = root + chainOffset + steps * size

                                if a >= 0 && a + size <= blockSize then
                                    yield
                                        {
                                            Root = root
                                            Chain = Some (declaring, names)
                                            View = Some (view, steps)
                                        }
            ]

        // `*(V*)q`, naming no field: the baseline every field access must agree with.
        let flatAccesses =
            [
                for root in roots do
                    for view in views do
                        yield
                            {
                                Root = root
                                Chain = None
                                View = Some (view, 0)
                            }
            ]

        Gen.frequency
            [
                3, Gen.elements fieldAccesses
                4, Gen.elements viewedAccesses
                1, Gen.elements flatAccesses
            ]

    let private viewType (access : Access) : ConcreteType<ConcreteTypeHandle> =
        match access with
        | Access.Byte -> concreteType (AllConcreteTypes.getRequiredNonGenericHandle baseState.ConcreteTypes bct.Byte)
        | Access.Int32 -> concreteType (AllConcreteTypes.getRequiredNonGenericHandle baseState.ConcreteTypes bct.Int32)
        | Access.Inner -> concreteType innerHandle
        | Access.Pointer -> failwith "no view is taken as a pointer here"

    let private innerZero : CliType = zeroOf innerHandle

    let private pointerZero : CliType =
        CliType.getFieldById (fieldId holderHandle "P") (zeroOf holderHandle)

    let private template (access : Access) : CliType =
        match access with
        | Access.Byte -> CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim 0uy))
        | Access.Int32 -> CliType.Numeric (CliNumericType.Int32 0)
        | Access.Inner -> innerZero
        | Access.Pointer -> pointerZero

    let private int32Cell (value : int) : CliType =
        CliType.Numeric (CliNumericType.Int32 value)

    let private innerValue (a : int) (b : int) : CliType =
        innerZero
        |> CliType.withFieldSetById (fieldId innerHandle "A") (int32Cell a)
        |> CliType.withFieldSetById (fieldId innerHandle "B") (int32Cell b)

    let private storedValue (stored : Stored) : CliType =
        match stored with
        | Stored.Int32 v -> int32Cell v
        | Stored.Int64 v -> CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim v))
        | Stored.Outer (lead, a, b, tail) ->
            zeroOf outerHandle
            |> CliType.withFieldSetById (fieldId outerHandle "Lead") (int32Cell lead)
            |> CliType.withFieldSetById (fieldId outerHandle "I") (innerValue a b)
            |> CliType.withFieldSetById (fieldId outerHandle "Tail") (int32Cell tail)
        | Stored.Holder (twin, pointer, a, b) ->
            let handle, p, fa, fb =
                if twin then
                    twinHandle, "Q", "X", "Y"
                else
                    holderHandle, "P", "A", "B"

            zeroOf handle
            |> CliType.withFieldSetById (fieldId handle p) (CliType.RuntimePointer pointer)
            |> CliType.withFieldSetById (fieldId handle fa) (int32Cell a)
            |> CliType.withFieldSetById (fieldId handle fb) (int32Cell b)

    let private modelOf (contents : (int * Stored) list) : Model =
        let image = Array.zeroCreate<byte> blockSize

        let put (offset : int) (bytes : byte[]) = bytes.CopyTo (image, offset)

        let cells =
            contents
            |> List.map (fun (offset, stored) ->
                match stored with
                | Stored.Int32 v ->
                    put offset (BitConverter.GetBytes v)
                    offset, ModelCell.Scalar 4
                | Stored.Int64 v ->
                    put offset (BitConverter.GetBytes v)
                    offset, ModelCell.Scalar 8
                | Stored.Outer (lead, a, b, tail) ->
                    [ lead ; a ; b ; tail ]
                    |> List.iteri (fun i v -> put (offset + 4 * i) (BitConverter.GetBytes v))

                    offset, ModelCell.Struct 16
                | Stored.Holder (twin, pointer, a, b) ->
                    put (offset + 8) (BitConverter.GetBytes a)
                    put (offset + 12) (BitConverter.GetBytes b)
                    offset, ModelCell.Holder (twin, pointer)
            )
            |> Map.ofList

        {
            Cells = cells
            Image = image
        }

    let private methodFrame (state : IlMachineState) : IlMachineState * ThreadId =
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

    /// The state holding a zero-initialised block, and a function from a byte offset to a bare
    /// byref at that byte.
    let private allocate (rootKind : RootKind) : IlMachineState * (int -> ManagedPointerSource) =
        let state, blockStart =
            match rootKind with
            | RootKind.Native ->
                IlMachineState.allocateNativeMemory MemoryBlockInitialization.ZeroInitialized blockSize baseState
                |> fun (ptr, state) -> state, ptr
            | RootKind.Stack ->
                let state, thread = methodFrame baseState

                IlMachineState.allocateStackMemory thread MemoryBlockInitialization.ZeroInitialized blockSize state
                |> fun (ptr, state) -> state, ptr

        let atByte (offset : int) : ManagedPointerSource =
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

        state, atByte

    let private populate
        (atByte : int -> ManagedPointerSource)
        (contents : (int * Stored) list)
        (state : IlMachineState)
        : IlMachineState
        =
        (state, contents)
        ||> List.fold (fun state (offset, stored) ->
            IlMachineState.writeManagedByrefBytesOrTypedCell bct state (atByte offset) (storedValue stored)
        )

    /// The byref `spec` describes, built through the production constructors.
    let private build (atByte : int -> ManagedPointerSource) (state : IlMachineState) (spec : ByrefSpec) =
        let withChain =
            match spec.Chain with
            | None -> atByte spec.Root
            | Some (declaring, names) ->
                (atByte spec.Root, chainFieldIds declaring names)
                ||> List.fold (fun ptr field -> ManagedPointerSource.appendProjection (ByrefProjection.Field field) ptr)

        match spec.View with
        | None -> withChain
        | Some (view, steps) ->
            ManagedPointerByteView.addByteOffset state (viewType view) (steps * accessSize view) withChain

    /// The type of the struct cell starting at `offset`, if one does. A `Struct 8` is the `Inner` an
    /// `Inner` store leaves when it replaces a pointer outright.
    let private declaredAt (model : Model) (offset : int) : Declaring option =
        match Map.tryFind offset model.Cells with
        | Some (ModelCell.Struct 16) -> Some Declaring.Outer
        | Some (ModelCell.Struct 8) -> Some Declaring.Inner
        | Some (ModelCell.Holder (false, _)) -> Some Declaring.Holder
        | Some (ModelCell.Holder (true, _)) -> Some Declaring.Twin
        | _ -> None

    /// How an access through a field chain is served when the block already holds a value of the
    /// chain's declaring type at the root, so that the chain is resolved in that value by identity
    /// rather than read from the type's layout (see `tryAnchorRawRootFieldPrefix`). The address is
    /// the same either way; this is the rule `TestPointerStructByteViewCells` states for a typed
    /// root, applied along the chain.
    [<RequireQualifiedAccess>]
    type private ChainVerdict =
        /// The innermost cell of the chain containing the access holds no pointer.
        | Bytes
        /// The access is exactly the pointer field the chain names, which only the readers and
        /// writers that follow the chain structurally serve.
        | ThePointer
        | Refused

    /// `None` when the chain rule does not apply: no value of the declaring type at the root, or
    /// an access that leaves it, which steps out into the block.
    let private chainVerdict
        (model : Model)
        (spec : ByrefSpec)
        (address : int)
        (access : Access)
        : ChainVerdict option
        =
        match spec.Chain with
        | None -> None
        | Some (declaring, names) ->
            if declaredAt model spec.Root <> Some declaring then
                None
            else

            let holdsPointer = declaring = Declaring.Holder || declaring = Declaring.Twin

            let structSize =
                match declaring with
                | Declaring.Inner -> 8
                | Declaring.Outer
                | Declaring.Holder
                | Declaring.Twin -> 16
                | Declaring.Union
                | Declaring.Nest ->
                    failwith "the load and store properties store no Union or Nest, so declaredAt never names one"

            // Innermost first: the chain's own cell, each cell enclosing it, then the struct.
            let cells =
                [
                    for n in names.Length .. -1 .. 1 do
                        let prefix = List.take n names

                        let offset, access =
                            chains
                            |> List.pick (fun (d, p, offset, access) ->
                                if d = declaring && p = prefix then
                                    Some (offset, access)
                                else
                                    None
                            )

                        yield spec.Root + offset, accessSize access, (access = Access.Pointer)
                    yield spec.Root, structSize, holdsPointer
                ]

            let size = accessSize access

            match
                cells
                |> List.tryFind (fun (start, extent, _) -> start <= address && address + size <= start + extent)
            with
            | None -> None
            | Some (_, _, false) -> Some ChainVerdict.Bytes
            | Some (_, _, true) ->
                if access = Access.Int32 && (address = spec.Root + 8 || address = spec.Root + 12) then
                    Some ChainVerdict.Bytes
                elif access = Access.Pointer && address = spec.Root then
                    Some ChainVerdict.ThePointer
                else
                    Some ChainVerdict.Refused

    let private holderPointerAt (model : Model) (offset : int) : CliRuntimePointer =
        match Map.tryFind offset model.Cells with
        | Some (ModelCell.Holder (_, pointer)) -> pointer
        | other -> failwith $"expected a Holder at %d{offset}, got %A{other}"

    /// What the model says a load of `access` at `address` through `spec` does: `None` when it is
    /// refused. `structural` says whether the reader follows the chain structurally rather than
    /// reading the bytes at its end.
    let private expectedLoad
        (model : Model)
        (spec : ByrefSpec)
        (structural : bool)
        (address : int)
        (access : Access)
        : Choice<byte[], CliRuntimePointer> option
        =
        let size = accessSize access

        match chainVerdict model spec address access with
        | Some ChainVerdict.Bytes -> Some (Choice1Of2 model.Image.[address .. address + size - 1])
        | Some ChainVerdict.ThePointer ->
            if structural then
                Some (Choice2Of2 (holderPointerAt model spec.Root))
            else
                None
        | Some ChainVerdict.Refused -> None
        | None ->

        match intersecting address size model with
        | touched when touched |> List.exists (snd >> hasNoByteImage) ->
            match touched, access with
            | [ offset, ModelCell.Holder _ ], Access.Int32 when address = offset + 8 || address = offset + 12 ->
                Some (Choice1Of2 model.Image.[address .. address + 3])
            | [ offset, ModelCell.Holder (_, pointer) ], Access.Pointer when address = offset ->
                Some (Choice2Of2 pointer)
            | [ offset, ModelCell.Pointer pointer ], Access.Pointer when address = offset -> Some (Choice2Of2 pointer)
            | _ -> None
        | _ -> Some (Choice1Of2 model.Image.[address .. address + size - 1])

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 1500

    let private tryRun (f : unit -> 'a) : 'a option =
        try
            Some (f ())
        with _ ->
            None

    type private LoadCase =
        {
            RootKind : RootKind
            Contents : (int * Stored) list
            Byref : ByrefSpec
        }

    let private loadCaseGen : Gen<LoadCase> =
        gen {
            let! rootKind = Gen.elements [ RootKind.Stack ; RootKind.Native ]
            let! contents = contentsGen
            let! byref = byrefGen [ Access.Int32 ; Access.Byte ]

            return
                {
                    RootKind = rootKind
                    Contents = contents
                    Byref = byref
                }
        }

    [<Test>]
    let ``a load through a field of a raw root reads the bytes at its layout address`` () : unit =
        let property (case : LoadCase) : unit =
            let state, atByte = allocate case.RootKind
            let state = populate atByte case.Contents state
            let model = modelOf case.Contents
            let ptr = build atByte state case.Byref
            let access = accessOf case.Byref
            let a = address case.Byref

            // `ldobj T`, `ldind`, and a load of whatever the byref's own type says, respectively;
            // the last follows the chain structurally.
            let actuals =
                [
                    false,
                    tryRun (fun () ->
                        IlMachineState.readManagedByrefAs
                            bct
                            state
                            (template access)
                            (ManagedPointerSource.requireAddressed ptr)
                    )
                    false,
                    tryRun (fun () ->
                        IlMachineState.readManagedByrefBytesAs
                            bct
                            state
                            (ManagedPointerSource.requireAddressed ptr)
                            (template access)
                    )
                    true,
                    tryRun (fun () ->
                        IlMachineState.readManagedByref bct state (ManagedPointerSource.requireAddressed ptr)
                    )
                ]

            // `ldfld` spells a load of a field's own type differently: through the byref to its
            // container, naming the field.
            let actualLdfld =
                match case.Byref.View, case.Byref.Chain with
                | None, Some (declaring, names) ->
                    let fields = chainFieldIds declaring names

                    let parent =
                        build
                            atByte
                            state
                            { case.Byref with
                                Chain = None
                            }

                    let parent =
                        (parent, List.take (fields.Length - 1) fields)
                        ||> List.fold (fun ptr field ->
                            ManagedPointerSource.appendProjection (ByrefProjection.Field field) ptr
                        )

                    Some (
                        true,
                        tryRun (fun () ->
                            IlMachineState.readManagedByrefField
                                bct
                                state
                                (ManagedPointerSource.requireAddressed parent)
                                (List.last fields)
                        )
                    )
                | _ -> None

            let check (structural : bool, actual : CliType option) =
                match expectedLoad model case.Byref structural a access, actual with
                | None, None -> ()
                | None, Some v -> failwith $"expected a refusal at %d{a} for %O{access}, got %O{v}"
                | Some expected, None -> failwith $"expected %A{expected} at %d{a} for %O{access}, got a refusal"
                | Some (Choice1Of2 bytes), Some v -> CliType.ToBytes v |> shouldEqual bytes
                | Some (Choice2Of2 pointer), Some v -> v |> shouldEqual (CliType.RuntimePointer pointer)

            // A pointer read from bytes that hold no pointer has no model answer.
            let modelHasAnswer =
                access <> Access.Pointer
                || intersecting a 8 model |> List.exists (snd >> hasNoByteImage)

            if modelHasAnswer then
                actuals |> List.iter check
                actualLdfld |> Option.iter check

        Check.One (config, Prop.forAll (Arb.fromGen loadCaseGen) property)

    type private StoreCase =
        {
            RootKind : RootKind
            Contents : (int * Stored) list
            Byref : ByrefSpec
            Value : Choice<byte[], CliRuntimePointer>
            /// Which production writer the store goes through.
            Writer : int
        }

    let private storeCaseGen : Gen<StoreCase> =
        gen {
            let! rootKind = Gen.elements [ RootKind.Stack ; RootKind.Native ]
            let! contents = contentsGen
            let! byref = byrefGen [ Access.Int32 ; Access.Byte ]
            let access = accessOf byref

            let! value =
                match access with
                | Access.Pointer -> pointerGen |> Gen.map Choice2Of2
                | _ ->
                    Gen.arrayOfLength (accessSize access) (Gen.choose (0, 255) |> Gen.map byte<int>)
                    |> Gen.map Choice1Of2

            let! writer = Gen.choose (0, 2)

            return
                {
                    RootKind = rootKind
                    Contents = contents
                    Byref = byref
                    Value = value
                    Writer = writer
                }
        }

    /// The model after a store of `value` at `address` through `spec`, or `None` when the store
    /// is refused. `structural` says whether the writer follows the chain structurally.
    let private expectedStore
        (model : Model)
        (spec : ByrefSpec)
        (structural : bool)
        (address : int)
        (access : Access)
        (value : Choice<byte[], CliRuntimePointer>)
        : Model option
        =
        let withBytes (bytes : byte[]) =
            let image = Array.copy model.Image
            bytes.CopyTo (image, address)

            { model with
                Image = image
            }

        match chainVerdict model spec address access, value with
        | Some ChainVerdict.Bytes, Choice1Of2 bytes -> Some (withBytes bytes)
        | Some ChainVerdict.ThePointer, Choice2Of2 pointer ->
            if structural then
                let twin =
                    match Map.tryFind spec.Root model.Cells with
                    | Some (ModelCell.Holder (twin, _)) -> twin
                    | other -> failwith $"expected a Holder at %d{spec.Root}, got %A{other}"

                Some
                    { model with
                        Cells = Map.add spec.Root (ModelCell.Holder (twin, pointer)) model.Cells
                    }
            else
                None
        | Some ChainVerdict.Refused, _ -> None
        | Some verdict, _ -> failwith $"chain verdict %A{verdict} does not fit a store of %A{value}"
        | None, _ ->

        match value with
        | Choice2Of2 pointer ->
            let replaced =
                match intersecting address 8 model with
                | [] -> Some (Map.add address (ModelCell.Pointer pointer) model.Cells)
                | [ offset, (ModelCell.Scalar 8 | ModelCell.Pointer _) ] when offset = address ->
                    Some (Map.add address (ModelCell.Pointer pointer) model.Cells)
                | [ offset, ModelCell.Holder (twin, _) ] when offset = address ->
                    Some (Map.add address (ModelCell.Holder (twin, pointer)) model.Cells)
                | _ -> None

            replaced
            |> Option.map (fun cells ->
                { model with
                    Cells = cells
                }
            )
        | Choice1Of2 bytes ->
            let size = bytes.Length
            let image = Array.copy model.Image
            bytes.CopyTo (image, address)

            match intersecting address size model with
            | touched when touched |> List.exists (snd >> hasNoByteImage) ->
                match touched with
                | [ offset, ModelCell.Holder _ ] when size = 4 && (address = offset + 8 || address = offset + 12) ->
                    Some
                        { model with
                            Image = image
                        }
                | [ offset, ModelCell.Pointer _ ] when offset = address && size = 8 ->
                    // The only byte-addressable store as wide as a pointer here is an `Inner`,
                    // which replaces the pointer's cell outright.
                    Some
                        {
                            Cells = Map.add address (ModelCell.Struct 8) model.Cells
                            Image = image
                        }
                | _ -> None
            | _ ->
                Some
                    { model with
                        Image = image
                    }

    let private valueOf (access : Access) (value : Choice<byte[], CliRuntimePointer>) : CliType =
        match value with
        | Choice2Of2 pointer -> CliType.RuntimePointer pointer
        | Choice1Of2 bytes ->
            match access with
            | Access.Byte -> CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim bytes.[0]))
            | Access.Int32 -> int32Cell (BitConverter.ToInt32 (bytes, 0))
            | Access.Inner -> innerValue (BitConverter.ToInt32 (bytes, 0)) (BitConverter.ToInt32 (bytes, 4))
            | Access.Pointer -> failwith "a pointer store carries a pointer"

    /// Everything a model claims about the block, read back through bare byrefs, which name no
    /// field and so depend on nothing the byref under test decided.
    let private checkBlock (atByte : int -> ManagedPointerSource) (state : IlMachineState) (model : Model) : unit =
        let read (offset : int) (access : Access) : CliType =
            IlMachineState.readManagedByrefBytesAs
                bct
                state
                (ManagedPointerSource.requireAddressed (atByte offset))
                (template access)

        let inPointerCell (offset : int) =
            intersecting offset 1 model |> List.exists (snd >> hasNoByteImage)

        for KeyValue (offset, cell) in model.Cells do
            match cell with
            | ModelCell.Holder (_, pointer) ->
                read offset Access.Pointer |> shouldEqual (CliType.RuntimePointer pointer)

                CliType.ToBytes (read (offset + 8) Access.Int32)
                |> shouldEqual model.Image.[offset + 8 .. offset + 11]

                CliType.ToBytes (read (offset + 12) Access.Int32)
                |> shouldEqual model.Image.[offset + 12 .. offset + 15]
            | ModelCell.Pointer pointer -> read offset Access.Pointer |> shouldEqual (CliType.RuntimePointer pointer)
            | ModelCell.Scalar _
            | ModelCell.Struct _ -> ()

        for offset in 0 .. blockSize - 1 do
            if not (inPointerCell offset) then
                CliType.ToBytes (read offset Access.Byte)
                |> shouldEqual [| model.Image.[offset] |]

    [<Test>]
    let ``a store through a field of a raw root writes exactly its layout address`` () : unit =
        let property (case : StoreCase) : unit =
            let state, atByte = allocate case.RootKind
            let state = populate atByte case.Contents state
            let model = modelOf case.Contents
            let ptr = build atByte state case.Byref
            let access = accessOf case.Byref
            let a = address case.Byref
            let value = valueOf access case.Value

            let write () =
                match case.Writer, access with
                // `stind.i1` / `stind.i4`. A pointer field is stored by `stfld`, whose value is the
                // field's own pointer type rather than the native int `stind.i` carries.
                | 1, (Access.Byte | Access.Int32) -> IlMachineState.writeIndirectPrimitiveStore bct state ptr value
                | 2, _ -> IlMachineState.writeManagedByrefBytesOrTypedCell bct state ptr value
                | _ -> IlMachineState.writeManagedByrefWithBase bct state ptr value

            // `writeManagedByrefWithBase` is `stfld`'s writer, and follows the chain structurally.
            let structural =
                match case.Writer, access with
                | 1, (Access.Byte | Access.Int32)
                | 2, _ -> false
                | _ -> true

            match expectedStore model case.Byref structural a access case.Value, tryRun write with
            | None, None -> ()
            | None, Some _ -> failwith $"expected a refusal of %O{value} at %d{a}, but the store was served"
            | Some _, None -> failwith $"expected %O{value} at %d{a} to be stored, but the store was refused"
            | Some expected, Some state -> checkBlock atByte state expected

        Check.One (config, Prop.forAll (Arb.fromGen storeCaseGen) property)

    type private IdentityCase =
        {
            RootKind : RootKind
            Contents : (int * Stored) list
            Left : ByrefSpec
            Right : ByrefSpec
        }

    let private identityCaseGen : Gen<IdentityCase> =
        gen {
            let! rootKind = Gen.elements [ RootKind.Stack ; RootKind.Native ]
            let! contents = contentsGen
            let! left = byrefGen [ Access.Int32 ; Access.Byte ]
            // Any byref; one to the same address or a neighbour spelt flat; or the same spelling
            // off another byte.
            let! right =
                Gen.oneof
                    [
                        byrefGen [ Access.Int32 ; Access.Byte ]
                        Gen.elements [ 0..4 .. blockSize - 4 ]
                        |> Gen.map (fun root ->
                            { left with
                                Root = root
                            }
                        )
                        Gen.elements [ -4 ; 0 ; 4 ]
                        |> Gen.map (fun delta ->
                            {
                                Root = max 0 (min (blockSize - 4) (address left + delta))
                                Chain = None
                                View = Some (Access.Int32, 0)
                            }
                        )
                    ]

            return
                {
                    RootKind = rootKind
                    Contents = contents
                    Left = left
                    Right = right
                }
        }

    [<Test>]
    let ``a field of a raw root is located at its layout address, whatever the block holds`` () : unit =
        let property (case : IdentityCase) : unit =
            let state, atByte = allocate case.RootKind
            let state = populate atByte case.Contents state
            let left = build atByte state case.Left
            let right = build atByte state case.Right

            let locate (ptr : ManagedPointerSource) : int64 =
                match StorageLocation.resolve bct state ptr with
                | StorageLocation.LocationResolution.Located (_, Some (_, offset)) -> offset
                | other -> failwith $"%O{ptr} has no byte coordinate: %O{other}"

            locate left |> shouldEqual (int64<int> (address case.Left))
            locate right |> shouldEqual (int64<int> (address case.Right))

            let normalisation =
                ManagedPointerByteView.normalisationContextForPointers state [ left ; right ]

            let outcome =
                ManagedPointerSource.ceqNormalisedDeferred
                    "test"
                    (ManagedPointerSource.normaliseForComparison normalisation left)
                    (ManagedPointerSource.normaliseForComparison normalisation right)

            let expected = address case.Left = address case.Right

            // Where structure alone decides, it must decide correctly; where it defers, the byte
            // coordinates decide.
            match outcome with
            | CeqOutcome.Decided answer -> answer |> shouldEqual expected
            | CeqOutcome.NeedsByteLocation (_, _, diagnostic) ->
                // One spelling off two bytes moves both by the same field offsets, so structure
                // alone decides it, which is what a caller with no state to resolve a deferral with
                // relies on. A byte cursor after a `Field` is deferred regardless.
                let sameSpellingNoCursor =
                    case.Left.Chain.IsSome
                    && case.Left.Chain = case.Right.Chain
                    && case.Left.View = case.Right.View
                    && (
                        match case.Left.View with
                        | None
                        | Some (_, 0) -> true
                        | Some _ -> false
                    )

                if sameSpellingNoCursor then
                    failwith $"one spelling off two bytes was deferred rather than decided: %s{diagnostic}"

            StorageLocation.resolveCeq bct state outcome |> shouldEqual expected

        Check.One (config, Prop.forAll (Arb.fromGen identityCaseGen) property)

    /// `stind.i` of a native int carrying a pointer's provenance, through `ref p->P` into a block
    /// nothing has written. Such a value has no bytes, so the store must install it as a cell at
    /// the field's address, which it can only find from `Holder`'s layout.
    [<TestCase false>]
    [<TestCase true>]
    let ``stind of a provenance-carrying native int through a field of a fresh raw root installs it``
        (native : bool)
        : unit
        =
        let state, atByte = allocate (if native then RootKind.Native else RootKind.Stack)

        let tagged =
            ManagedPointerSource.Byref
                {
                    Root = ByrefRoot.LocalVariable (ThreadId.ThreadId 0, FrameId.FrameId 0, 3us)
                    Projections = []
                }
            |> NativeIntSource.ManagedPointer
            |> CliNumericType.NativeInt
            |> CliType.Numeric

        let viaField =
            ManagedPointerSource.appendProjection (ByrefProjection.Field (fieldId holderHandle "P")) (atByte 16)

        let state = IlMachineState.writeIndirectPrimitiveStore bct state viaField tagged

        IlMachineState.readManagedByrefBytesAs bct state (ManagedPointerSource.requireAddressed (atByte 16)) tagged
        |> shouldEqual tagged

    /// Every field chain an address-only property walks, with the byte offset of its end, as the
    /// C# above lays them out. `Union` and `Nest` overlap fields holding pointers, a native int and
    /// a nested struct, so reading any one of those fields' values could fail where its address
    /// cannot.
    let private addressChains : (Declaring * string list * int) list =
        [
            for declaring, names, offset, _ in chains do
                yield declaring, names, offset
            yield Declaring.Union, [ "P" ], 0
            yield Declaring.Union, [ "N" ], 0
            yield Declaring.Union, [ "A" ], 8
            yield Declaring.Union, [ "B" ], 12
            yield Declaring.Nest, [ "H" ], 0
            yield Declaring.Nest, [ "H" ; "P" ], 0
            yield Declaring.Nest, [ "H" ; "A" ], 8
            yield Declaring.Nest, [ "H" ; "B" ], 12
            yield Declaring.Nest, [ "L" ], 0
            yield Declaring.Nest, [ "U" ], 8
            yield Declaring.Nest, [ "U" ; "P" ], 8
            yield Declaring.Nest, [ "U" ; "N" ], 8
            yield Declaring.Nest, [ "U" ; "A" ], 16
            yield Declaring.Nest, [ "U" ; "B" ], 20
            yield Declaring.Nest, [ "Tail" ], 24
        ]

    let private addressOf (spec : ByrefSpec) : int =
        let chainOffset =
            match spec.Chain with
            | None -> 0
            | Some (declaring, names) ->
                addressChains
                |> List.pick (fun (d, n, offset) -> if d = declaring && n = names then Some offset else None)

        let viewOffset =
            match spec.View with
            | None -> 0
            | Some (view, steps) -> steps * accessSize view

        spec.Root + chainOffset + viewOffset

    /// A byref as a guest spells it, over any chain of `addressChains`: the address need not lie
    /// in the block, because nothing here reads it.
    let private addressByrefGen : Gen<ByrefSpec> =
        gen {
            let! root = Gen.elements [ 0..4 .. blockSize - 4 ]

            let! chain =
                Gen.frequency
                    [
                        6, Gen.elements addressChains |> Gen.map (fun (d, n, _) -> Some (d, n))
                        1, Gen.constant None
                    ]

            let! view =
                match chain with
                | None -> Gen.elements [ Access.Int32 ; Access.Byte ] |> Gen.map (fun v -> Some (v, 0))
                | Some _ ->
                    Gen.frequency
                        [
                            2, Gen.constant None
                            3,
                            Gen.map2
                                (fun v steps -> Some (v, steps))
                                (Gen.elements [ Access.Int32 ; Access.Byte ])
                                (Gen.choose (-6, 6))
                        ]

            return
                {
                    Root = root
                    Chain = chain
                    View = view
                }
        }

    let private unionValue (pointer : CliRuntimePointer) : CliType =
        zeroOf unionHandle
        |> CliType.withFieldSetById (fieldId unionHandle "P") (CliType.RuntimePointer pointer)

    let private nestValue (pointer : CliRuntimePointer) : CliType =
        zeroOf nestHandle
        |> CliType.withFieldSetById (fieldId nestHandle "H") (storedValue (Stored.Holder (false, pointer, 1, 2)))

    /// What a block holds, as typed cells in address order: every stored kind of the other
    /// properties, and `Union`s and `Nest`s holding pointers.
    let private addressContentsGen : Gen<(int * CliType) list> =
        let cellGen : Gen<CliType option> =
            Gen.frequency
                [
                    3, storedGen |> Gen.map (Option.map storedValue)
                    1, pointerGen |> Gen.map (unionValue >> Some)
                    1, pointerGen |> Gen.map (nestValue >> Some)
                ]

        let rec go (offset : int) : Gen<(int * CliType) list> =
            if offset >= blockSize then
                Gen.constant []
            else
                gen {
                    let! cell = cellGen

                    match cell with
                    | Some c when offset + CliType.sizeOf c <= blockSize ->
                        let! rest = go (offset + CliType.sizeOf c)
                        return (offset, c) :: rest
                    | _ -> return! go (offset + 4)
                }

        go 0

    [<RequireQualifiedAccess>]
    type private AddressRoot =
        | Stack
        | Native
        /// A native block freed after the byrefs into it were taken: their addresses are still
        /// facts, and comparing them reads neither.
        | FreedNative

    type private AddressCase =
        {
            Root : AddressRoot
            Contents : (int * CliType) list
            Left : ByrefSpec
            Right : ByrefSpec
        }

    let private addressCaseGen : Gen<AddressCase> =
        gen {
            let! root = Gen.elements [ AddressRoot.Stack ; AddressRoot.Native ; AddressRoot.FreedNative ]
            let! contents = addressContentsGen
            let! left = addressByrefGen
            // Often the same address, or a neighbour, spelt another way.
            let! right =
                Gen.frequency
                    [
                        2, addressByrefGen
                        1,
                        Gen.elements [ -4 ; 0 ; 4 ]
                        |> Gen.map (fun delta ->
                            {
                                Root = addressOf left + delta
                                Chain = None
                                View = Some (Access.Int32, 0)
                            }
                        )
                        1,
                        Gen.elements [ 0..4 .. blockSize - 4 ]
                        |> Gen.map (fun root ->
                            { left with
                                Root = root
                            }
                        )
                    ]

            return
                {
                    Root = root
                    Contents = contents
                    Left = left
                    Right = right
                }
        }

    [<Test>]
    let ``address comparisons and differences of raw-root byrefs are flat-offset arithmetic and never throw``
        ()
        : unit
        =
        let property (case : AddressCase) : unit =
            let state, atByte =
                allocate (
                    match case.Root with
                    | AddressRoot.Stack -> RootKind.Stack
                    | AddressRoot.Native
                    | AddressRoot.FreedNative -> RootKind.Native
                )

            let state =
                (state, case.Contents)
                ||> List.fold (fun state (offset, value) ->
                    IlMachineState.writeManagedByrefBytesOrTypedCell bct state (atByte offset) value
                )

            let left = build atByte state case.Left
            let right = build atByte state case.Right

            let state =
                match case.Root, atByte 0 with
                | AddressRoot.FreedNative,
                  ManagedPointerSource.Byref {
                                                 Root = ByrefRoot.NativeMemoryByte (block, _)
                                                 Projections = _
                                             } -> IlMachineState.freeNativeMemory block state
                | _ -> state

            let a1 = int64<int> (addressOf case.Left)
            let a2 = int64<int> (addressOf case.Right)

            let locate (ptr : ManagedPointerSource) : int64 =
                match StorageLocation.resolve bct state ptr with
                | StorageLocation.LocationResolution.Located (_, Some (_, offset)) -> offset
                | other -> failwith $"%O{ptr} has no byte coordinate: %O{other}"

            locate left |> shouldEqual a1
            locate right |> shouldEqual a2

            let wrappings : (string * (ManagedPointerSource -> EvalStackValue)) list =
                [
                    "byref", EvalStackValue.ManagedPointer
                    "native int", (fun p -> EvalStackValue.NativeInt (NativeIntSource.ManagedPointer p))
                    "widened int64",
                    (fun p ->
                        EvalStackValue.Int64 (Int64Source.WidenedNativeInt (NativeIntSource.ManagedPointer p, false))
                    )
                ]

            for name, wrap in wrappings do
                EvalStackValueComparisons.ceqDeferred state.PointerHashState (wrap left) (wrap right)
                |> StorageLocation.resolveCeq bct state
                |> fun equal -> (name, equal) |> shouldEqual (name, (a1 = a2))

            // `clt.un` of two C# pointers, and their difference.
            let asNativeInt (p : ManagedPointerSource) =
                EvalStackValue.NativeInt (NativeIntSource.ManagedPointer p)

            EvalStackValueComparisons.cltUnDeferred (asNativeInt left) (asNativeInt right)
            |> StorageLocation.resolveOrder bct state
            |> shouldEqual (a1 < a2)

            match BinaryArithmetic.execute bct ArithmeticOperation.sub state (asNativeInt left) (asNativeInt right) with
            | EvalStackValue.NativeInt (NativeIntSource.Verbatim difference), _ -> difference |> shouldEqual (a1 - a2)
            | other, _ -> failwith $"pointer difference of %O{left} and %O{right} was %O{other}"

        Check.One (config, Prop.forAll (Arb.fromGen addressCaseGen) property)
