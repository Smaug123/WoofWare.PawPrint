namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.Runtime.InteropServices
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// A byref's projection chain can interleave `Field`, `ReinterpretAs` and `ByteOffset` steps in any
/// order the guest's `ldflda`, `Unsafe.As` and `Unsafe.Add` calls produce, and every access through
/// it — `ldfld` of a further field, `ldind`/`ldobj` of the byref itself, and the matching stores —
/// has to be served from where the whole chain lands. Which step comes last, and how many `Field`s
/// follow a `ReinterpretAs`, must not decide whether an access is served at all.
///
/// The oracle is structural navigation of the storage the test built. Every `ReinterpretAs` the
/// generator emits is an identity view — the element type over an `[InlineArray]`-shaped buffer, or
/// a cell's own type over that cell — so the chain names a cell the test can also reach by field
/// path, and a correct byref layer must read exactly that cell and write exactly that cell. Element
/// types are drawn from CoreLib so that each reinterpret target is a real type the interpreter
/// resolves from metadata; the universe holds both storage with a byte image, served bytewise, and
/// storage holding references, served by naming the cell a chain picks out.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestByteViewChainAccess =

    // Factory intentionally undisposed: corelib.Logger outlives this scope.
    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private bct : BaseClassTypes<DumpedAssembly> = Corelib.getBaseTypes corelib

    let private loaded : LoadedAssemblies = LoadedAssemblies.ofAssemblies [ corelib ]

    let private loggerFactory = snd (LoggerFactory.makeTest ())

    /// Element types. The first three have a byte image and the last two hold object references
    /// and have none; in each group all but `Guid` nest a value type inside another.
    let private universe : (string * string) list =
        [
            "System", "Range"
            "System", "DateTimeOffset"
            "System", "Guid"
            "System.Reflection", "CustomAttributeNamedArgument"
            "System.Runtime.CompilerServices", "ValueTaskAwaiter"
        ]

    type private Element =
        {
            Name : string
            Handle : ConcreteTypeHandle
            Type : ConcreteType<ConcreteTypeHandle>
            Zero : CliType
        }

    /// A state in which every universe type, and every type its fields mention, is concretized:
    /// the interpreter resolves a `ReinterpretAs` target without loading anything, exactly as it
    /// does after `Concretization.concretizeMethod`'s priming sweep.
    let private prepared : IlMachineState * Element list =
        let initial =
            { IlMachineState.initial loggerFactory ImmutableArray.Empty corelib with
                ConcreteTypes = Corelib.concretizeAll loaded bct AllConcreteTypes.Empty
            }

        ((initial, []), universe)
        ||> List.fold (fun (state, acc) (ns, name) ->
            let typeInfo =
                match corelib.TryGetTopLevelTypeDef ns name with
                | Some t -> t
                | None -> failwith $"%s{ns}.%s{name} not found in corelib"

            let state, handle =
                LoadedTypeInfo.typeInfoToTypeDefn' bct state._LoadedAssemblies typeInfo
                |> IlMachineState.concretizeType
                    loggerFactory
                    bct
                    state
                    corelib.DefinitionFullName
                    ImmutableArray.Empty
                    ImmutableArray.Empty

            let zero, state = IlMachineState.cliTypeZeroOfHandle state bct handle

            let ty =
                AllConcreteTypes.lookup handle state.ConcreteTypes
                |> Option.defaultWith (fun () -> failwith $"%s{name} was concretized but has no registry entry")

            state,
            acc
            @ [
                {
                    Name = name
                    Handle = handle
                    Type = ty
                    Zero = zero
                }
            ]
        )

    let private preparedState : IlMachineState = fst prepared
    let private elements : Element list = snd prepared

    let private typeOf (handle : ConcreteTypeHandle) : ConcreteType<ConcreteTypeHandle> =
        AllConcreteTypes.lookup handle preparedState.ConcreteTypes
        |> Option.defaultWith (fun () -> failwith $"cell type %O{handle} has no registry entry")

    let private storageDeclared : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle preparedState.ConcreteTypes bct.TypedReference

    let private elementCount : int = 3

    let private slotId (i : int) : FieldId = FieldId.named $"_%d{i}"

    /// An `[InlineArray(elementCount)]` stand-in: `elementCount` cells of the element type.
    let private storageOf (element : Element) (values : CliType list) : CliValueType =
        values
        |> List.mapi (fun i value ->
            {
                Id = slotId i
                Name = $"_%d{i}"
                Contents = value
                Offset = None
                Type = element.Handle
                MarshallingDescriptor = None
            }
        )
        |> SynthesisedLayoutKind.ofFields bct preparedState.ConcreteTypes storageDeclared Layout.Default CharSet.Ansi

    let private fieldsOf (value : CliType) : CliField list =
        match value with
        | CliType.ValueType cvt -> CliValueType.TryAllFields cvt |> List.map CliConcreteField.ToCliField
        | _ -> []

    /// Every non-composite cell of `value`, by path. Comparing these rather than whole values
    /// ignores bookkeeping a write legitimately changes (edit stamps, which of two equivalent
    /// storage representations holds the bytes) and nothing a guest can observe.
    let rec private leaves (path : FieldId list) (value : CliType) : (FieldId list * CliType) list =
        match value with
        | CliType.ValueType _ -> fieldsOf value |> List.collect (fun f -> leaves (path @ [ f.Id ]) f.Contents)
        | other -> [ path, other ]

    /// A value of the same shape as `template`, with every leaf drawn afresh. Floats are drawn from
    /// integers so that structural equality is value equality.
    let rec private valueLike (template : CliType) : Gen<CliType> =
        let small = Gen.choose (-200, 200)

        match template with
        | CliType.Numeric n ->
            match n with
            | CliNumericType.Int32 _ -> small |> Gen.map CliNumericType.Int32
            | CliNumericType.Int64 _ ->
                small
                |> Gen.map (fun i -> CliNumericType.Int64 (Int64Source.Verbatim (int64<int> i * 1_000_003L)))
            | CliNumericType.NativeInt _ ->
                small
                |> Gen.map (fun i -> CliNumericType.NativeInt (NativeIntSource.Verbatim (int64<int> i)))
            | CliNumericType.NativeFloat _ -> small |> Gen.map (fun i -> CliNumericType.NativeFloat (float i))
            | CliNumericType.Int8 _ -> small |> Gen.map (fun i -> CliNumericType.Int8 (int8<int> (i % 100)))
            | CliNumericType.Int16 _ -> small |> Gen.map (fun i -> CliNumericType.Int16 (int16<int> i))
            | CliNumericType.UInt8 _ ->
                Gen.choose (0, 255)
                |> Gen.map (fun i -> CliNumericType.UInt8 (UInt8Source.Verbatim (byte<int> i)))
            | CliNumericType.UInt16 _ ->
                Gen.choose (0, 60000)
                |> Gen.map (fun i -> CliNumericType.UInt16 (uint16<int> i))
            | CliNumericType.Float32 _ -> small |> Gen.map (fun i -> CliNumericType.Float32 (float32<int> i))
            | CliNumericType.Float64 _ -> small |> Gen.map (fun i -> CliNumericType.Float64 (float i))
            |> Gen.map CliType.Numeric
        | CliType.Bool _ -> Gen.elements [ 0uy ; 1uy ] |> Gen.map CliType.Bool
        | CliType.Char _ ->
            Gen.zip (Gen.choose (0, 255)) (Gen.choose (0, 255))
            |> Gen.map (fun (hi, lo) -> CliType.Char (byte<int> hi, byte<int> lo))
        | CliType.ObjectRef _ ->
            Gen.oneof
                [
                    Gen.constant None
                    Gen.choose (1, 50) |> Gen.map (ManagedHeapAddress.ManagedHeapAddress >> Some)
                ]
            |> Gen.map CliType.ObjectRef
        | CliType.RuntimePointer _ -> Gen.constant template
        | CliType.ValueType cvt ->
            (Gen.constant cvt, fieldsOf template)
            ||> List.fold (fun acc f ->
                gen {
                    let! acc = acc
                    let! v = valueLike f.Contents
                    return CliValueType.WithFieldSetById f.Id v acc
                }
            )
            |> Gen.map CliType.ValueType

    /// A projection chain over `storage`, and the field path of the cell it names.
    ///
    /// At the storage itself the chain either selects a slot by `Field` or reinterprets the whole
    /// buffer as its element type, which is what indexing an `[InlineArray]` does. Below that it
    /// may take a `Field`, reinterpret a composite cell as its own type, or — straight after a
    /// reinterpret at slot granularity — move to another slot by `ByteOffset`, the `Unsafe.Add`
    /// of an indexer. Those are the only moves `ManagedPointerSource.appendProjection` accepts in
    /// those positions, so every chain generated is one the interpreter can actually build.
    ///
    /// A move to another slot from a slot the chain selected by `Field` leaves the cell that
    /// `Field` names, and the byref layer serves that only for storage with a byte image, by
    /// lifting the access out to the enclosing cell. Storage holding references has no bytes to
    /// lift through, so for it the generator moves between slots only from the buffer-wide
    /// reinterpret.
    let private chainGen
        (element : Element)
        (slotOffset : int -> int)
        (storage : CliType)
        : Gen<ByrefProjection list * FieldId list>
        =
        let elementHasByteImage =
            match CliType.ByteAddressability element.Zero with
            | CliByteAddressability.ByteAddressable -> true
            | CliByteAddressability.SymbolicallyAddressable _
            | CliByteAddressability.Rejected _ -> false

        let rec go
            (revProjs : ByrefProjection list)
            (path : FieldId list)
            (budget : int)
            : Gen<ByrefProjection list * FieldId list>
            =
            let stop = Gen.constant (List.rev revProjs, path)

            let reinterpretedWholeBuffer =
                match List.tryLast revProjs with
                | Some (ByrefProjection.ReinterpretAs _) -> true
                | _ -> false

            let canAdvance =
                match revProjs, path with
                | (ByrefProjection.ReinterpretAs _ | ByrefProjection.ByteOffset _) :: _, [ _ ] ->
                    reinterpretedWholeBuffer || elementHasByteImage
                | _ -> false

            let moves : Gen<ByrefProjection list * FieldId list> list =
                if budget = 0 then
                    []
                else
                    match path with
                    | [] ->
                        [
                            Gen.choose (0, elementCount - 1)
                            |> Gen.bind (fun i ->
                                go (ByrefProjection.Field (slotId i) :: revProjs) [ slotId i ] (budget - 1)
                            )
                            go (ByrefProjection.ReinterpretAs element.Type :: revProjs) [ slotId 0 ] (budget - 1)
                        ]
                    | _ ->
                        let cell = CliType.getCellAtPath path storage

                        let descend =
                            match fieldsOf cell with
                            | [] -> []
                            | fields ->
                                [
                                    Gen.elements fields
                                    |> Gen.bind (fun f ->
                                        go (ByrefProjection.Field f.Id :: revProjs) (path @ [ f.Id ]) (budget - 1)
                                    )
                                ]

                        // A second reinterpret straight after the first collapses into it, so it
                        // would only spend budget.
                        let reinterpret =
                            match revProjs, cell with
                            | ByrefProjection.ReinterpretAs _ :: _, _ -> []
                            | _, CliType.ValueType cvt ->
                                [
                                    go
                                        (ByrefProjection.ReinterpretAs (typeOf cvt.Declared) :: revProjs)
                                        path
                                        (budget - 1)
                                ]
                            | _ -> []

                        let advance =
                            match path with
                            | [ slot ] when canAdvance ->
                                let current = [ 0 .. elementCount - 1 ] |> List.find (fun i -> slotId i = slot)

                                [
                                    [ 0 .. elementCount - 1 ]
                                    |> List.filter (fun j -> j <> current)
                                    |> Gen.elements
                                    |> Gen.bind (fun j ->
                                        let step = ByrefProjection.ByteOffset (slotOffset j - slotOffset current)
                                        go (step :: revProjs) [ slotId j ] (budget - 1)
                                    )
                                ]
                            | _ -> []

                        descend @ reinterpret @ advance

            match moves with
            | [] -> stop
            | _ -> Gen.frequency ((1, stop) :: (moves |> List.map (fun m -> 3, m)))

        go [] [] 6

    type private Case =
        {
            Element : Element
            Storage : CliValueType
            Projections : ByrefProjection list
            /// The cell the chain names, as a field path from the storage.
            Path : FieldId list
            /// A field of that cell to access through the chain, when the cell has fields.
            Field : CliField option
            /// A value to store into that field.
            FieldValue : CliType option
            /// A value to store through the chain itself.
            CellValue : CliType
        }

        override this.ToString () : string =
            $"%s{this.Element.Name}: %A{this.Projections} naming %A{this.Path}, field %A{this.Field |> Option.map (fun f -> f.Name)}"

    let private caseGen : Gen<Case> =
        gen {
            let! element = Gen.elements elements
            let! values = Gen.listOfLength elementCount (valueLike element.Zero)
            let storage = storageOf element values

            let slotOffset (i : int) : int =
                CliValueType.GetFieldLayoutById (slotId i) storage |> fst

            let! projs, path = chainGen element slotOffset (CliType.ValueType storage)
            let cell = CliType.getCellAtPath path (CliType.ValueType storage)

            let! field =
                match fieldsOf cell with
                | [] -> Gen.constant None
                | fields -> Gen.elements fields |> Gen.map Some

            let! fieldValue =
                match field with
                | None -> Gen.constant None
                | Some f -> valueLike f.Contents |> Gen.map Some

            let! cellValue = valueLike cell

            return
                {
                    Element = element
                    Storage = storage
                    Projections = projs
                    Path = path
                    Field = field
                    FieldValue = fieldValue
                    CellValue = cellValue
                }
        }

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 400

    /// Allocate the case's storage on the heap and point a byref at it through the case's chain.
    let private rooted (case : Case) : IlMachineState * ManagedHeapAddress * ManagedPointerSource =
        let addr, state =
            IlMachineState.allocateManagedObject storageDeclared case.Storage preparedState

        let src =
            (ManagedPointerSource.Byref
                {
                    Root = ByrefRoot.HeapValue addr
                    Projections = []
                },
             case.Projections)
            ||> List.fold (fun src proj -> ManagedPointerSource.appendProjection proj src)

        state, addr, src

    let private storageAfter (state : IlMachineState) (addr : ManagedHeapAddress) : CliType =
        CliType.ValueType (ManagedHeap.get addr state.ManagedHeap).Contents

    /// The premise of every property below: the universe really does span both kinds of storage,
    /// and really does nest, so that the generator can put more than one `Field` after a
    /// `ReinterpretAs`. If CoreLib's layout of these types ever changes, this fails rather than the
    /// properties quietly covering less.
    [<Test>]
    let ``the element universe spans both storage kinds and nests`` () : unit =
        let byteAddressable, referenceHolding =
            elements
            |> List.partition (fun e ->
                match CliType.ByteAddressability e.Zero with
                | CliByteAddressability.ByteAddressable -> true
                | _ -> false
            )

        List.length byteAddressable |> shouldEqual 3
        List.length referenceHolding |> shouldEqual 2

        let nests (e : Element) : bool =
            fieldsOf e.Zero
            |> List.exists (fun f -> not (List.isEmpty (fieldsOf f.Contents)))

        elements
        |> List.filter (fun e -> e.Name <> "Guid")
        |> List.forall nests
        |> shouldEqual true

    /// `ldind`/`ldobj` through the chain reads the cell the chain names.
    [<Test>]
    let ``reading through a chain reads the cell it names`` () : unit =
        let property (case : Case) : bool =
            let state, _, src = rooted case

            let read =
                IlMachineState.readManagedByref bct state (ManagedPointerSource.requireAddressed src)

            let expected = CliType.getCellAtPath case.Path (CliType.ValueType case.Storage)
            leaves [] read = leaves [] expected

        Check.One (config, Prop.forAll (Arb.fromGen caseGen) property)

    /// `ldfld` through the chain reads that field of the cell the chain names.
    [<Test>]
    let ``reading a field through a chain reads that field of the cell it names`` () : unit =
        let property (case : Case) : bool =
            match case.Field with
            | None -> true
            | Some field ->
                let state, _, src = rooted case

                let read =
                    IlMachineState.readManagedByrefField bct state (ManagedPointerSource.requireAddressed src) field.Id

                let expected =
                    CliType.getCellAtPath (case.Path @ [ field.Id ]) (CliType.ValueType case.Storage)

                leaves [] read = leaves [] expected

        Check.One (config, Prop.forAll (Arb.fromGen caseGen) property)

    /// `stfld` through the chain changes that field of the cell the chain names, and nothing else.
    [<Test>]
    let ``writing a field through a chain writes that field of the cell it names and nothing else`` () : unit =
        let property (case : Case) : bool =
            match case.Field, case.FieldValue with
            | Some field, Some value ->
                let state, addr, src = rooted case

                let state =
                    IlMachineState.writeManagedByrefWithBase
                        bct
                        state
                        (ManagedPointerSource.requireAddressed (
                            ManagedPointerSource.appendProjection (ByrefProjection.Field field.Id) src
                        ))
                        value

                let expected =
                    CliType.withCellAtPathSet (case.Path @ [ field.Id ]) value (CliType.ValueType case.Storage)

                leaves [] (storageAfter state addr) = leaves [] expected
            | _ -> true

        Check.One (config, Prop.forAll (Arb.fromGen caseGen) property)

    /// `stind`/`stobj` through the chain replaces the cell the chain names, and nothing else.
    [<Test>]
    let ``writing through a chain writes the cell it names and nothing else`` () : unit =
        let property (case : Case) : bool =
            match case.Path with
            | [] -> true
            | path ->
                let state, addr, src = rooted case

                let state =
                    IlMachineState.writeManagedByrefWithBase
                        bct
                        state
                        (ManagedPointerSource.requireAddressed src)
                        case.CellValue

                let expected =
                    CliType.withCellAtPathSet path case.CellValue (CliType.ValueType case.Storage)

                leaves [] (storageAfter state addr) = leaves [] expected

        Check.One (config, Prop.forAll (Arb.fromGen caseGen) property)
