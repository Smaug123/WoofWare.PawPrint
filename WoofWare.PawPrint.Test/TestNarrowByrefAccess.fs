namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.Runtime.InteropServices
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// A byref names a byte address. The width and shape of an access come from the access — the value
/// being stored, or the type being loaded — and never from the storage the byref happens to be
/// rooted at.
///
/// The byte-addressable half of that rule is reachable from C# and is pinned end to end by
/// `sourcesPure/NarrowStructStoreThroughWideSlot.cs`. This file exists for the other half: storage
/// whose value has no byte image, because it holds managed pointers. No C# source can name such a
/// struct — `MethodBaseInvoker`'s `StackAllocatedByRefs` is an `[InlineArray(4)]` of `ref byte`,
/// which C# only tolerates behind `#pragma warning disable CS9184` inside CoreLib — so the shapes
/// below are built by hand.
///
/// `sourcesPure/ReflectionInvokeMethodMultipleArguments.cs` does exercise them end to end, via the
/// guest that builds that very buffer; these tests are what say *which* byte moved, and what the
/// bytes either side of it did, when that guest goes wrong.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestNarrowByrefAccess =

    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private baseClassTypes : BaseClassTypes<DumpedAssembly> =
        Corelib.getBaseTypes corelib

    let private loadedAssemblies : LoadedAssemblies =
        LoadedAssemblies.ofAssemblies [ corelib ]

    /// `System.ByReference` is not one of the types `Corelib.concretizeAll` registers up front — in
    /// a real run it is concretized on demand, which `ByReference.Create` always does long before
    /// anything reads the buffer back. Register it here so these tests can name the same type the
    /// guest does.
    let private concreteTypes : AllConcreteTypes =
        let baseline =
            Corelib.concretizeAll loadedAssemblies baseClassTypes AllConcreteTypes.Empty

        match baseClassTypes.ByReference with
        | None -> failwith "this corelib has no System.ByReference; these tests have no subject"
        | Some byReference ->

        let ctx : TypeConcretization.ConcretizationContext<DumpedAssembly> =
            {
                TypeConcretization.ConcretizationContext.ConcreteTypes = baseline
                TypeConcretization.ConcretizationContext.LoadedAssemblies = loadedAssemblies
                TypeConcretization.ConcretizationContext.BaseTypes = baseClassTypes
            }

        let signatureTypeKind =
            DumpedAssembly.signatureTypeKind baseClassTypes loadedAssemblies byReference

        let _handle, ctx =
            TypeConcretization.concretizeType
                ctx
                IAssemblyLoad.alreadyLoadedOnly
                byReference.Assembly
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.FromDefinition (byReference.Identity, signatureTypeKind))

        ctx.ConcreteTypes

    let private handleOf (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>) : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle concreteTypes typeInfo

    let private int32Handle : ConcreteTypeHandle = handleOf baseClassTypes.Int32
    let private int64Handle : ConcreteTypeHandle = handleOf baseClassTypes.Int64
    let private intPtrHandle : ConcreteTypeHandle = handleOf baseClassTypes.IntPtr

    /// The concrete type of `System.ByReference`, the single-field wrapper CoreLib stores a
    /// managed pointer in. `PrimitiveLikeStruct.kind` classifies it `FlattenToManagedPointer`
    /// (Corelib.fs:325), which is the equivalence the write path leans on to put one of these into
    /// a cell declared as a bare `ref byte`.
    let private byReferenceHandle : ConcreteTypeHandle =
        match baseClassTypes.ByReference with
        | None -> failwith "this corelib has no System.ByReference; these tests have no subject"
        | Some ty -> handleOf ty

    let private byteType : ConcreteType<ConcreteTypeHandle> =
        ConcreteType.makeFromIdentity
            baseClassTypes.Byte.Identity
            baseClassTypes.Byte.Namespace
            baseClassTypes.Byte.Name
            ImmutableArray<ConcreteTypeHandle>.Empty

    let private state () : IlMachineState =
        let _, loggerFactory = LoggerFactory.makeTest ()

        { IlMachineState.initial loggerFactory ImmutableArray.Empty corelib with
            ConcreteTypes = concreteTypes
        }

    let private field (name : string) (ty : ConcreteTypeHandle) (contents : CliType) : CliField =
        {
            Id = FieldId.named name
            Name = name
            Contents = contents
            Offset = None
            Type = ty
            MarshallingDescriptor = None
        }

    let private structOf (declared : ConcreteTypeHandle) (fields : CliField list) : CliValueType =
        fields
        |> CliValueType.OfFields baseClassTypes concreteTypes declared Layout.Default CharSet.Ansi

    /// A stand-in for `MethodBaseInvoker.StackAllocatedByRefs`: `slots` managed-pointer cells laid
    /// out end to end. A managed pointer is a root plus projections in this interpreter, not a bit
    /// pattern, so this whole value has no byte image — which is exactly what makes it interesting,
    /// since the byte machinery that serves every ordinary narrow write cannot serve this one.
    let private pointerSlots (slots : ManagedPointerSource list) : CliValueType =
        slots
        |> List.mapi (fun i slot ->
            field $"_arg%d{i}" intPtrHandle (CliType.RuntimePointer (CliRuntimePointer.Managed slot))
        )
        |> structOf int32Handle

    /// A `System.ByReference` holding `target`: an 8-byte single-field wrapper whose one field is a
    /// managed pointer, so it too has no byte image.
    let private byReferenceTo (target : ManagedPointerSource) : CliType =
        [
            field "Value" intPtrHandle (CliType.RuntimePointer (CliRuntimePointer.Managed target))
        ]
        |> structOf byReferenceHandle
        |> CliType.ValueType

    let private somewhere (n : int) : ManagedPointerSource =
        ManagedPointerSource.Byref (ByrefRoot.HeapValue (ManagedHeapAddress.ManagedHeapAddress n), [])

    /// Allocate `contents` on the heap; a byref rooted there names its first byte.
    let private storageAt (contents : CliValueType) : IlMachineState * ManagedHeapAddress =
        let addr, state =
            IlMachineState.allocateManagedObject int32Handle contents (state ())

        state, addr

    let private contentsOf (state : IlMachineState) (addr : ManagedHeapAddress) : CliValueType =
        (ManagedHeap.get addr state.ManagedHeap).Contents

    let private slotPointer (value : CliValueType) (index : int) : CliRuntimePointer =
        match CliValueType.DereferenceField $"_arg%d{index}" value with
        | CliType.RuntimePointer p -> p
        | other -> failwith $"slot %d{index} held %O{other}, not a runtime pointer"

    /// Guard the premise of every test below: if this storage ever acquires a byte image, the tests
    /// would still pass while covering the ordinary byte path instead of the cell-naming one.
    [<Test>]
    let ``the storage these tests use really has no byte image`` () : unit =
        let storage = pointerSlots [ for i in 1..4 -> somewhere i ]

        match CliValueType.ByteAddressability storage with
        | CliByteAddressability.ByteAddressable ->
            failwith "a value type holding managed pointers was classified byte-addressable"
        | CliByteAddressability.Rejected _ -> ()

        match CliType.ByteAddressability (byReferenceTo (somewhere 1)) with
        | CliByteAddressability.ByteAddressable -> failwith "System.ByReference was classified byte-addressable"
        | CliByteAddressability.Rejected _ -> ()

    /// The shape `*(ByReference*)(p + 0) = ...` produces: `p + 0` is `p` (BinaryArithmetic.fs:347),
    /// so the destination byref carries no projections at all and the write must take its width
    /// from the value rather than from the slot.
    [<Test>]
    let ``a narrow write through a bare byref touches only the bytes it covers`` () : unit =
        let state, addr =
            storageAt (pointerSlots [ for _ in 1..4 -> ManagedPointerSource.Null ])

        let destination = ManagedPointerSource.Byref (ByrefRoot.HeapValue addr, [])
        let payload = somewhere 42

        let state =
            IlMachineState.writeManagedByrefWithBase baseClassTypes state destination (byReferenceTo payload)

        let after = contentsOf state addr

        // The slot is still four pointers wide and still declares the type it was allocated as.
        // Getting this wrong is not a wrong number but a wrong *shape*: before this was modelled
        // the 32-byte slot became an 8-byte `ByReference` and the three cells after it ceased to
        // exist.
        CliType.sizeOf (CliType.ValueType after) |> shouldEqual 32
        after.Declared |> shouldEqual (pointerSlots []).Declared

        slotPointer after 0 |> shouldEqual (CliRuntimePointer.Managed payload)

        for i in 1..3 do
            slotPointer after i
            |> shouldEqual (CliRuntimePointer.Managed ManagedPointerSource.Null)

    /// The shape `*(ByReference*)(p + 1) = ...` produces. Pointer arithmetic on a byref is a byte
    /// cursor, so the destination is `[ReinterpretAs System.Byte; ByteOffset 8]` — and the width of
    /// the store is emphatically not one byte.
    [<Test>]
    let ``a narrow write through a byte cursor touches only the bytes it covers`` () : unit =
        let state, addr =
            storageAt (pointerSlots [ for _ in 1..4 -> ManagedPointerSource.Null ])

        let destination =
            ManagedPointerSource.Byref (
                ByrefRoot.HeapValue addr,
                [ ByrefProjection.ReinterpretAs byteType ; ByrefProjection.ByteOffset 8 ]
            )

        let payload = somewhere 43

        let state =
            IlMachineState.writeManagedByrefWithBase baseClassTypes state destination (byReferenceTo payload)

        let after = contentsOf state addr

        CliType.sizeOf (CliType.ValueType after) |> shouldEqual 32
        slotPointer after 1 |> shouldEqual (CliRuntimePointer.Managed payload)

        for i in [ 0 ; 2 ; 3 ] do
            slotPointer after i
            |> shouldEqual (CliRuntimePointer.Managed ManagedPointerSource.Null)

    /// The destination cell is declared `ref byte`, so it holds a bare `CliType.RuntimePointer`; the
    /// value stored is a `System.ByReference`, a single-field wrapper around exactly that. The two
    /// are the same eight bytes on a real runtime, and PawPrint already says so — the eval stack
    /// flattens a `ByReference` to a managed pointer. The cell must keep its own declared shape:
    /// restamping it as a `ByReference` would change what the *next* read of that field flattens
    /// to.
    [<Test>]
    let ``storing a wrapper into a bare pointer cell keeps the cell's shape`` () : unit =
        let state, addr =
            storageAt (pointerSlots [ for _ in 1..4 -> ManagedPointerSource.Null ])

        let destination = ManagedPointerSource.Byref (ByrefRoot.HeapValue addr, [])

        let state =
            IlMachineState.writeManagedByrefWithBase baseClassTypes state destination (byReferenceTo (somewhere 44))

        match CliValueType.DereferenceField "_arg0" (contentsOf state addr) with
        | CliType.RuntimePointer _ -> ()
        | CliType.ValueType vt ->
            failwith $"the cell was restamped as %O{vt.Declared} instead of keeping its bare pointer shape"
        | other -> failwith $"the cell became %O{other}"

    /// The read direction of the same rule, and the one `NativeReflectionInvocation.readArgument`
    /// depends on: `args[i]` is a `ByReference` reached through a `System.Byte` cursor, so the
    /// pointer's own type view is a byte and the width has to come from the caller instead.
    [<Test>]
    let ``a narrow read takes its width from the template, not from the pointer`` () : unit =
        let first = somewhere 45
        let second = somewhere 46

        let state, addr =
            storageAt (pointerSlots [ first ; second ; ManagedPointerSource.Null ; ManagedPointerSource.Null ])

        let template = byReferenceTo ManagedPointerSource.Null

        let bare = ManagedPointerSource.Byref (ByrefRoot.HeapValue addr, [])

        let cursor =
            ManagedPointerSource.Byref (
                ByrefRoot.HeapValue addr,
                [ ByrefProjection.ReinterpretAs byteType ; ByrefProjection.ByteOffset 8 ]
            )

        let readPointer (src : ManagedPointerSource) : ManagedPointerSource =
            match IlMachineState.readManagedByrefAs baseClassTypes state template src with
            | CliType.RuntimePointer (CliRuntimePointer.Managed p) -> p
            | CliType.ValueType vt ->
                match CliValueType.DereferenceField "Value" vt with
                | CliType.RuntimePointer (CliRuntimePointer.Managed p) -> p
                | other -> failwith $"wrapper field held %O{other}"
            | other -> failwith $"read gave %O{other}"

        // A byref that already names one pointer cell, rather than the buffer containing it. There
        // is nothing to name *inside* such a cell, so the access either is the whole cell or has no
        // answer — and it has to be the former, because a `System.ByReference` is read out of
        // exactly this: a slot declared `ref byte`, holding what the wrapper wraps.
        let namedCell =
            ManagedPointerSource.Byref (ByrefRoot.HeapValue addr, [ ByrefProjection.Field (FieldId.named "_arg1") ])

        readPointer bare |> shouldEqual first
        readPointer cursor |> shouldEqual second
        readPointer namedCell |> shouldEqual second

    /// The narrow route must not swallow the whole-slot one. A same-size store through a bare byref
    /// replaces the slot outright, including its declared type — `stobj IntPtr` over a bare
    /// `NativeInt` cell relies on exactly that restamp (IlMachineManagedByref.fs:415-429).
    [<Test>]
    let ``an equal-size write through a bare byref still replaces the whole slot`` () : unit =
        let state, addr = storageAt (pointerSlots [ ManagedPointerSource.Null ])
        let destination = ManagedPointerSource.Byref (ByrefRoot.HeapValue addr, [])
        let replacement = byReferenceTo (somewhere 47)

        let state =
            IlMachineState.writeManagedByrefWithBase baseClassTypes state destination replacement

        let after = contentsOf state addr
        after.Declared |> shouldEqual byReferenceHandle

    /// A store wider than the slot it addresses is not a narrow write with a sign flipped; it is a
    /// buffer overrun, and there is no honest thing to do with the bytes past the end.
    [<Test>]
    let ``a write wider than the slot fails loudly`` () : unit =
        let state, addr = storageAt (pointerSlots [ ManagedPointerSource.Null ])
        let destination = ManagedPointerSource.Byref (ByrefRoot.HeapValue addr, [])
        let tooWide = CliType.ValueType (pointerSlots [ somewhere 48 ; somewhere 49 ])

        let outcome =
            try
                IlMachineState.writeManagedByrefWithBase baseClassTypes state destination tooWide
                |> ignore

                Choice1Of2 ()
            with e ->
                Choice2Of2 e

        match outcome with
        | Choice1Of2 () -> failwith "expected a 16-byte store into an 8-byte slot to be refused"
        | Choice2Of2 _ -> ()

    /// The frame condition, over the byte-addressable storage where an independent oracle exists: a
    /// narrow store through a bare byref must leave the byte image equal to the original with
    /// exactly the leading bytes replaced. A byte array spliced by hand is that oracle; it knows
    /// nothing about cells, so an implementation that got the extent from the storage rather than
    /// from the value cannot agree with it.
    [<Test>]
    let ``a narrow write splices the byte image and changes nothing else`` () : unit =
        let property (wideFieldCount : int) (narrowFieldCount : int) (seed : int) : bool =
            // Strictly narrower: an equal-size store is the whole-slot write, which deliberately
            // restamps the slot's declared type and so is a different claim (pinned separately by
            // "an equal-size write through a bare byref still replaces the whole slot").
            let wideFieldCount = 2 + abs (wideFieldCount % 5)
            let narrowFieldCount = 1 + abs (narrowFieldCount % (wideFieldCount - 1))

            let wide =
                [
                    for i in 0 .. wideFieldCount - 1 ->
                        field
                            $"W%d{i}"
                            int64Handle
                            (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim (int64 (seed + i)))))
                ]
                |> structOf int32Handle

            let narrow =
                [
                    for i in 0 .. narrowFieldCount - 1 ->
                        field
                            $"N%d{i}"
                            int64Handle
                            (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim (int64 (seed * 31 + i + 1)))))
                ]
                |> structOf int64Handle

            let state, addr = storageAt wide
            let destination = ManagedPointerSource.Byref (ByrefRoot.HeapValue addr, [])
            let before = CliType.ToBytes (CliType.ValueType wide)
            let payload = CliType.ToBytes (CliType.ValueType narrow)

            let expected = Array.copy before
            System.Array.Copy (payload, 0, expected, 0, payload.Length)

            let state =
                IlMachineState.writeManagedByrefWithBase baseClassTypes state destination (CliType.ValueType narrow)

            let after = contentsOf state addr

            CliType.ToBytes (CliType.ValueType after) = expected
            && after.Declared = wide.Declared

        Check.QuickThrowOnFailure property
