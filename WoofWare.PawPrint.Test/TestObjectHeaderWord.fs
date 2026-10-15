namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Every heap object's header word: the `int32` CoreCLR keeps 4 bytes before an object's
/// method-table pointer, which .NET 11's managed thin lock (`ObjectHeader.AcquireThinLock` and
/// its siblings) reads and compare-exchanges through `(int*)&obj.GetMethodTableRef() - 1`.
///
/// The pointer is built here as CoreLib builds it: `RawData::Data`'s projection, then
/// `Unsafe.Subtract` by one `nint` to the method-table slot, then one `int` lower. Every guest that
/// takes a `lock` on .NET 11 is the end-to-end check that the interpreter's own arithmetic lands in
/// the same place.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestObjectHeaderWord =

    let private corelib : DumpedAssembly = ThreadFixtures.corelib

    let private baseClassTypes : BaseClassTypes<DumpedAssembly> =
        BaseClassTypes.ofCorelib corelib

    let private concreteTypes : AllConcreteTypes =
        Corelib.concretizeAll (LoadedAssemblies.ofAssemblies [ corelib ]) baseClassTypes AllConcreteTypes.Empty

    let private concreteTypeFor (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>) : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle concreteTypes typeInfo

    let private int32Handle : ConcreteTypeHandle = concreteTypeFor baseClassTypes.Int32

    let private objectHandle : ConcreteTypeHandle =
        concreteTypeFor baseClassTypes.Object

    let private nintHandle : ConcreteTypeHandle = concreteTypeFor baseClassTypes.IntPtr

    let private int32Template : CliType = CliType.Numeric (CliNumericType.Int32 0)

    let private state () : IlMachineState =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let initialState = IlMachineState.initial loggerFactory ImmutableArray.Empty corelib

        { initialState with
            TypeSystem =
                { initialState.TypeSystem with
                    ConcreteTypes = concreteTypes
                }
        }

    let private rawDataField : FieldInfo<GenericParamFromMetadata, TypeDefn> =
        match corelib.TryGetTopLevelTypeDef "System.Runtime.CompilerServices" "RawData" with
        | None -> failwith "System.Runtime.CompilerServices.RawData not found in CoreLib"
        | Some rawData -> rawData.Fields |> List.find (fun field -> field.Name = "Data")

    /// What a test object is.
    type Holder =
        /// A class instance holding an `int32` and an object reference, so its payload has no
        /// byte image, as `System.Threading.Thread`'s, which `Thread.Start` locks, has none.
        | MixedObject of int32
        /// A boxed `int32`, whose payload is byte-addressable.
        | BoxedInt of int32
        /// An `object`, whose payload is empty.
        | EmptyObject
        /// An `int32[]`, possibly empty.
        | IntArray of int32 list
        /// An `object[]` of nulls, possibly empty.
        | ObjectArray of length : int

    let private field (name : string) (ty : ConcreteTypeHandle) (contents : CliType) : CliField =
        {
            Id = FieldId.named name
            Name = name
            Contents = contents
            Offset = None
            Type = ty
            MarshallingDescriptor = None
        }

    let private allocate (holder : Holder) (state : IlMachineState) : ManagedHeapAddress * IlMachineState =
        let layout (fields : CliField list) =
            fields
            |> SynthesisedLayoutKind.ofFields
                baseClassTypes
                state.TypeSystem.ConcreteTypes
                objectHandle
                Layout.Default
                System.Runtime.InteropServices.CharSet.Ansi

        match holder with
        | Holder.MixedObject value ->
            let contents =
                layout
                    [
                        field "A" int32Handle (CliType.Numeric (CliNumericType.Int32 value))
                        field "B" objectHandle (CliType.ObjectRef None)
                    ]

            IlMachineState.allocateManagedObject objectHandle contents state
        | Holder.BoxedInt value ->
            let contents =
                layout [ field "m_value" int32Handle (CliType.Numeric (CliNumericType.Int32 value)) ]

            IlMachineState.allocateManagedObject int32Handle contents state
        | Holder.EmptyObject -> IlMachineState.allocateManagedObject objectHandle (layout []) state
        | Holder.IntArray values ->
            let arr, state =
                IlMachineState.allocateArray
                    (ConcreteTypeHandle.OneDimArrayZero int32Handle)
                    (fun () -> int32Template)
                    values.Length
                    state

            let state =
                (state, List.indexed values)
                ||> List.fold (fun state (i, v) ->
                    IlMachineState.setArrayValue arr (CliType.Numeric (CliNumericType.Int32 v)) i state
                )

            arr, state
        | Holder.ObjectArray length ->
            IlMachineState.allocateArray
                (ConcreteTypeHandle.OneDimArrayZero objectHandle)
                (fun () -> CliType.ObjectRef None)
                length
                state

    /// `ref obj.GetMethodTableRef()`: `Unsafe.Subtract(ref Unsafe.As<byte, nint>(ref obj.GetRawData()), 1)`.
    let private methodTableSlot (addr : ManagedHeapAddress) (state : IlMachineState) : ManagedPointerSource =
        match RuntimeFieldProjection.tryProjectFieldAddress baseClassTypes rawDataField addr state with
        | None -> failwith "RawData::Data did not project"
        | Some rawData -> ManagedPointerByteView.addByteOffset state nintHandle -8 rawData

    /// `ObjectHeader.GetHeaderPtr`: `(int*)ppMethodTable - 1`.
    let private headerWord (addr : ManagedHeapAddress) (state : IlMachineState) : AddressedByref =
        methodTableSlot addr state
        |> ManagedPointerByteView.addByteOffsetToByteView state -4
        |> ManagedPointerSource.requireAddressed

    /// The header word of `addr`, read at the width `ldind.i4` asks for. The pointer views the word
    /// as an `nint`, as `GetMethodTableRef` leaves it (the `(int*)` cast adds no view), so the
    /// operation's width is what makes this a read of the word.
    let private readHeader (addr : ManagedHeapAddress) (state : IlMachineState) : int32 =
        match
            IlMachineManagedByref.readManagedByrefBytesAs baseClassTypes state (headerWord addr state) int32Template
        with
        | CliType.Numeric (CliNumericType.Int32 v) -> v
        | other -> failwith $"the header word of %O{addr} read as %O{other}"

    let private writeHeader (addr : ManagedHeapAddress) (value : int32) (state : IlMachineState) : IlMachineState =
        IlMachineManagedByref.writeIndirectPrimitiveStore
            baseClassTypes
            state
            (headerWord addr state)
            (CliType.Numeric (CliNumericType.Int32 value))

    /// Everything the object's payload holds, for checking that header traffic leaves it alone.
    let private payload (addr : ManagedHeapAddress) (state : IlMachineState) : string =
        match ManagedHeap.tryGetArrayShape addr state.ManagedHeap with
        | Some shape ->
            List.init shape.Length (fun i -> IlMachineState.getArrayValue addr i state)
            |> sprintf "%A"
        | None -> ManagedHeap.get addr state.ManagedHeap |> sprintf "%A"

    let private holderGen : Gen<Holder> =
        let int32s = ArbMap.defaults |> ArbMap.generate<int32>

        Gen.oneof
            [
                int32s |> Gen.map Holder.MixedObject
                int32s |> Gen.map Holder.BoxedInt
                Gen.constant Holder.EmptyObject
                Gen.listOfLength 3 int32s
                |> Gen.zip (Gen.choose (0, 3))
                |> Gen.map (fun (n, values) -> Holder.IntArray (List.truncate n values))
                Gen.choose (0, 3) |> Gen.map Holder.ObjectArray
            ]

    /// One step against the header words: `None` reads, `Some v` stores `v`.
    type private Step = int * int32 option

    [<Test>]
    let ``each object's header word holds the last int32 stored to it, from zero, and its payload is untouched``
        ()
        : unit
        =
        let gen =
            gen {
                let! holders = Gen.nonEmptyListOf holderGen |> Gen.map (List.truncate 5)

                let! steps =
                    Gen.listOf (
                        Gen.zip
                            (Gen.choose (0, holders.Length - 1))
                            (Gen.optionOf (ArbMap.defaults |> ArbMap.generate<int32>))
                    )

                return holders, (steps : Step list)
            }

        let property (holders : Holder list, steps : Step list) =
            let addrs, state =
                ((([] : ManagedHeapAddress list), state ()), holders)
                ||> List.fold (fun (addrs, state) holder ->
                    let addr, state = allocate holder state
                    addr :: addrs, state
                )
                |> fun (addrs, state) -> List.rev addrs, state

            let payloads = addrs |> List.map (fun addr -> payload addr state)

            let finalModel, state =
                ((Map.empty, state), steps)
                ||> List.fold (fun (model, state) (which, step) ->
                    let addr = addrs.[which]

                    match step with
                    | None ->
                        readHeader addr state
                        |> shouldEqual (Map.tryFind which model |> Option.defaultValue 0)

                        model, state
                    | Some value -> Map.add which value model, writeHeader addr value state
                )

            addrs
            |> List.iteri (fun i addr ->
                readHeader addr state
                |> shouldEqual (Map.tryFind i finalModel |> Option.defaultValue 0)

                payload addr state |> shouldEqual payloads.[i]
            )

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 200, Prop.forAll (Arb.fromGen gen) property)

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

    /// The header word is an `int32` to the code that owns it, and nothing in CoreLib reads part
    /// of it or reads across it, so an access that is not the whole word is refused rather than
    /// rendered from bytes that would include the method-table pointer or the padding.
    [<TestCase(-12, 2)>]
    [<TestCase(-10, 2)>]
    [<TestCase(-16, 8)>]
    [<TestCase(-12, 8)>]
    [<TestCase(-14, 4)>]
    let ``an access overlapping the header word that is not exactly the word is refused``
        (offsetFromRawData : int, size : int)
        : unit
        =
        for holder in [ Holder.BoxedInt 5 ; Holder.IntArray [ 1 ; 2 ] ] do
            let addr, state = allocate holder (state ())

            let pointer =
                match RuntimeFieldProjection.tryProjectFieldAddress baseClassTypes rawDataField addr state with
                | None -> failwith "RawData::Data did not project"
                | Some rawData -> ManagedPointerByteView.addByteOffsetToByteView state offsetFromRawData rawData
                |> ManagedPointerSource.requireAddressed

            let template =
                match size with
                | 2 -> CliType.Numeric (CliNumericType.Int16 0s)
                | 4 -> int32Template
                | 8 -> CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L))
                | other -> failwith $"no template of size %d{other}"

            messageOf (fun () -> IlMachineManagedByref.readManagedByrefBytesAs baseClassTypes state pointer template)
            |> shouldContainText "object header word"

            messageOf (fun () ->
                IlMachineManagedByref.writeIndirectPrimitiveStore baseClassTypes state pointer template
            )
            |> shouldContainText "object header word"

    /// A typed read (`ldind` through a plain managed pointer, or the `ref` compare-exchanges) reads
    /// as wide as the type its byref views the word as, so a narrower or wider view is a partial
    /// access and is refused, where reading the whole word would silently answer a different
    /// question; a four-byte view reads the word in that view's type.
    [<Test>]
    let ``a typed read of the header word reads it in the byref's own view, and refuses a view of another width``
        ()
        : unit
        =
        let viewAt (viewType : ConcreteTypeHandle) (addr : ManagedHeapAddress) (state : IlMachineState) =
            match RuntimeFieldProjection.tryProjectFieldAddress baseClassTypes rawDataField addr state with
            | None -> failwith "RawData::Data did not project"
            | Some rawData -> ManagedPointerByteView.addByteOffset state viewType -12 rawData
            |> ManagedPointerSource.requireAddressed

        for holder in [ Holder.BoxedInt 5 ; Holder.MixedObject 7 ; Holder.EmptyObject ] do
            let addr, state = allocate holder (state ())
            let state = writeHeader addr (System.BitConverter.SingleToInt32Bits 3.25f) state

            IlMachineManagedByref.readManagedByref
                baseClassTypes
                state
                (viewAt (concreteTypeFor baseClassTypes.Single) addr state)
            |> shouldEqual (CliType.Numeric (CliNumericType.Float32 3.25f))

            IlMachineManagedByref.readManagedByref baseClassTypes state (viewAt int32Handle addr state)
            |> shouldEqual (CliType.Numeric (CliNumericType.Int32 (System.BitConverter.SingleToInt32Bits 3.25f)))

            for narrowOrWide in [ baseClassTypes.Byte ; baseClassTypes.Int16 ; baseClassTypes.IntPtr ] do
                messageOf (fun () ->
                    IlMachineManagedByref.readManagedByref
                        baseClassTypes
                        state
                        (viewAt (concreteTypeFor narrowOrWide) addr state)
                )
                |> shouldContainText "object header word"
