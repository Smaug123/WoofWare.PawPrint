namespace WoofWare.PawPrint.Test

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Struct layout aggregation checked against CoreCLR over finite, emitted shapes. The same
/// shape supplies the already classified fields; no PawPrint type resolver or layout walk runs.
/// Pinning and marshaller cleanup observe GC pointers and blittability; UnmanagedCallersOnly
/// signature preparation observes the interop verdict that combines the remaining flags.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestStructLayoutFacts =

    [<RequireQualifiedAccess>]
    type private Leaf =
        | Int32
        | Boolean
        | Char
        | Reference
        | Pointer
        | Enum
        | Byref
        | TypedReference
        | Decimal
        | Int128
        | UInt128
        | DecimalFloatingPoint of Type

    [<RequireQualifiedAccess>]
    type private Kind =
        | Leaf of Leaf
        | Struct of Shape

    and private Field =
        {
            Kind : Kind
            MarshalAs : UnmanagedType option
        }

    and private Shape =
        {
            Layout : TypeLayoutKind
            CharSet : CharSet
            Fields : Field list
        }

    let private decimalTypes : Type list =
        [ "Decimal32" ; "Decimal64" ; "Decimal128" ]
        |> List.choose (fun name -> typeof<obj>.Assembly.GetType ("System.Numerics." + name) |> Option.ofObj)

    let private leaves : Leaf list =
        [
            Leaf.Int32
            Leaf.Boolean
            Leaf.Char
            Leaf.Reference
            Leaf.Pointer
            Leaf.Enum
            Leaf.Byref
            Leaf.TypedReference
            Leaf.Decimal
            Leaf.Int128
            Leaf.UInt128
        ]
        @ List.map Leaf.DecimalFloatingPoint decimalTypes

    let rec private isByrefLike (kind : Kind) : bool =
        match kind with
        | Kind.Leaf Leaf.Byref
        | Kind.Leaf Leaf.TypedReference -> true
        | Kind.Leaf _ -> false
        | Kind.Struct shape -> shape.Fields |> List.exists (fun field -> isByrefLike field.Kind)

    let rec private sizeBound (kind : Kind) : int =
        match kind with
        | Kind.Leaf _ -> 16
        | Kind.Struct shape -> max 16 (shape.Fields |> List.sumBy (fun field -> sizeBound field.Kind))

    let private charsets : CharSet list =
        [ CharSet.Ansi ; CharSet.Unicode ; CharSet.Auto ]

    let private marshalChoices : UnmanagedType list =
        [
            UnmanagedType.I4
            UnmanagedType.U2
            UnmanagedType.Struct
            UnmanagedType.Bool
            UnmanagedType.LPStr
        ]

    let rec private genShape
        (allowTypedReference : bool)
        (depth : int)
        (width : int)
        (nesting : int)
        (marshalling : int)
        : Gen<Shape>
        =
        gen {
            let! count = Gen.choose (0, width)
            // CoreCLR can reject a nested struct containing TypedReference during field
            // placement (classlayoutinfo.cpp's alignment check). Exercise it only at the root.
            let availableLeaves =
                if allowTypedReference then
                    leaves
                else
                    List.except [ Leaf.TypedReference ] leaves

            let genKind =
                if depth = 0 then
                    Gen.elements availableLeaves |> Gen.map Kind.Leaf
                else
                    Gen.frequency
                        [
                            4, Gen.elements availableLeaves |> Gen.map Kind.Leaf
                            nesting, genShape false (depth - 1) width nesting marshalling |> Gen.map Kind.Struct
                        ]

            let! fields =
                Gen.listOfLength
                    count
                    (gen {
                        let! kind = genKind

                        let! marshalAs =
                            Gen.frequency
                                [
                                    4, Gen.constant None
                                    marshalling, Gen.elements marshalChoices |> Gen.map Some
                                ]

                        return
                            {
                                Kind = kind
                                MarshalAs = marshalAs
                            }
                    })

            let! layout =
                if fields |> List.exists (fun field -> isByrefLike field.Kind) then
                    Gen.constant TypeLayoutKind.Sequential
                else
                    Gen.elements [ TypeLayoutKind.Auto ; TypeLayoutKind.Sequential ; TypeLayoutKind.Explicit ]

            let! charSet = Gen.elements charsets

            return
                {
                    Layout = layout
                    CharSet = charSet
                    Fields = fields
                }
        }

    let private genTop : Gen<Shape> =
        gen {
            let! width = Gen.choose (1, 4)
            let! nesting = Gen.choose (1, 5)
            let! marshalling = Gen.choose (0, 4)
            return! genShape true 2 width nesting marshalling
        }

    let rec private shrinkShape (shape : Shape) : Shape seq =
        seq {
            for index in 0 .. shape.Fields.Length - 1 do
                yield
                    { shape with
                        Fields = List.removeAt index shape.Fields
                    }

                let field = shape.Fields.[index]

                if field.MarshalAs.IsSome then
                    yield
                        { shape with
                            Fields =
                                List.updateAt
                                    index
                                    { field with
                                        MarshalAs = None
                                    }
                                    shape.Fields
                        }

                match field.Kind with
                | Kind.Leaf _ -> ()
                | Kind.Struct child ->
                    for smaller in shrinkShape child do
                        yield
                            { shape with
                                Fields =
                                    List.updateAt
                                        index
                                        { field with
                                            Kind = Kind.Struct smaller
                                        }
                                        shape.Fields
                            }
        }

    let private attribute (ty : Type) : CustomAttributeBuilder =
        CustomAttributeBuilder (ty.GetConstructor Type.EmptyTypes, [||])

    let private structAttributes (shape : Shape) : TypeAttributes =
        let layout =
            match shape.Layout with
            | TypeLayoutKind.Auto -> TypeAttributes.AutoLayout
            | TypeLayoutKind.Sequential -> TypeAttributes.SequentialLayout
            | TypeLayoutKind.Explicit -> TypeAttributes.ExplicitLayout

        let charset =
            match shape.CharSet with
            | CharSet.Ansi -> TypeAttributes.AnsiClass
            | CharSet.Unicode -> TypeAttributes.UnicodeClass
            | CharSet.Auto -> TypeAttributes.AutoClass
            | other -> failwith $"fixture charset %O{other} is not ANSI, Unicode or Auto"

        TypeAttributes.Public ||| TypeAttributes.Sealed ||| layout ||| charset

    type private Observation =
        {
            MethodName : string
            Facts : StructLayoutFacts
        }

    let private emit (name : string) (disabled : bool) (top : Shape) : byte[] * Observation list =
        let builder = PersistedAssemblyBuilder (AssemblyName name, typeof<obj>.Assembly)

        if disabled then
            builder.SetCustomAttribute (attribute typeof<DisableRuntimeMarshallingAttribute>)

        let md = builder.DefineDynamicModule name

        let ucos =
            md.DefineType ("Ucos", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        let enumType =
            (md.DefineEnum ("E", TypeAttributes.Public, typeof<int>)).CreateType ()

        let observations = ResizeArray<Observation> ()

        let observe (t : Type) (facts : StructLayoutFacts) : unit =
            let methodName = $"M%d{observations.Count}"

            let mb =
                ucos.DefineMethod (
                    methodName,
                    MethodAttributes.Public ||| MethodAttributes.Static,
                    typeof<Void>,
                    [| t |]
                )

            mb.SetCustomAttribute (attribute typeof<UnmanagedCallersOnlyAttribute>)
            mb.GetILGenerator().Emit OpCodes.Ret

            observations.Add
                {
                    MethodName = methodName
                    Facts = facts
                }

        let numeric
            (t : Type)
            (selfKind : StructLayoutFacts.SelfKind)
            (isDecimal : bool)
            : Type * FieldBlittability.Element<StructLayoutFacts>
            =
            // These CoreLib numeric types have only primitive fields: none contributes GC,
            // auto-layout or nested special flags. The host checks their own flags too.
            let facts = StructLayoutFacts.ofStruct TypeLayoutKind.Sequential true selfKind []
            observe t facts
            t, FieldBlittability.Element.ValueType (isDecimal, facts)

        let rec typeOf (kind : Kind) : Type * FieldBlittability.Element<StructLayoutFacts> =
            match kind with
            | Kind.Struct shape ->
                let fields = shape.Fields |> List.map (fun field -> field, typeOf field.Kind)

                let inputs =
                    fields
                    |> List.map (fun (field, (_, element)) ->
                        Option.map FieldMarshalDescriptor.Other field.MarshalAs, element
                    )

                let ansi = shape.CharSet <> CharSet.Unicode

                let aggregate
                    (fields : (FieldMarshalDescriptor option * FieldBlittability.Element<StructLayoutFacts>) list)
                    : StructLayoutFacts
                    =
                    StructLayoutFacts.ofStruct shape.Layout ansi StructLayoutFacts.SelfKind.Ordinary fields

                let facts = aggregate inputs
                aggregate (List.rev inputs) |> shouldEqual facts
                aggregate (inputs @ inputs) |> shouldEqual facts

                if shape.Layout = TypeLayoutKind.Auto then
                    // Layout-owned flags cannot escape an auto-layout child into its parent.
                    facts.HasLayout |> shouldEqual false
                    facts.IsBlittable |> shouldEqual false
                    facts.IsAutoLayoutOrHasAutoLayoutField |> shouldEqual true
                    facts.IsInt128OrHasInt128Fields |> shouldEqual false
                    facts.IsDecimalFloatingPointOrHasDecimalFloatingPointFields |> shouldEqual false

                let tb =
                    md.DefineType ($"S%d{observations.Count}", structAttributes shape, typeof<ValueType>)

                if isByrefLike kind then
                    tb.SetCustomAttribute (attribute typeof<IsByRefLikeAttribute>)

                let mutable offset = 0

                fields
                |> List.iteri (fun i (field, (fieldType, _)) ->
                    let fb = tb.DefineField ($"F%d{i}", fieldType, FieldAttributes.Public)

                    match field.MarshalAs with
                    | None -> ()
                    | Some native ->
                        fb.SetCustomAttribute (
                            CustomAttributeBuilder (
                                typeof<MarshalAsAttribute>.GetConstructor [| typeof<UnmanagedType> |],
                                [| box native |]
                            )
                        )

                    if shape.Layout = TypeLayoutKind.Explicit then
                        fb.SetOffset offset
                        offset <- offset + sizeBound field.Kind
                )

                let t = tb.CreateType ()
                observe t facts
                t, FieldBlittability.Element.ValueType (false, facts)
            | Kind.Leaf Leaf.Int32 -> typeof<int>, FieldBlittability.Element.FourBytes
            | Kind.Leaf Leaf.Boolean -> typeof<bool>, FieldBlittability.Element.Boolean
            | Kind.Leaf Leaf.Char -> typeof<char>, FieldBlittability.Element.Char
            | Kind.Leaf Leaf.Reference -> typeof<string>, FieldBlittability.Element.Reference
            | Kind.Leaf Leaf.Pointer -> typeof<int>.MakePointerType (), FieldBlittability.Element.Pointer
            | Kind.Leaf Leaf.Enum -> enumType, FieldBlittability.Element.FourBytes
            | Kind.Leaf Leaf.Byref -> typeof<int>.MakeByRefType (), FieldBlittability.Element.Byref
            | Kind.Leaf Leaf.TypedReference ->
                typeof<obj>.Assembly.GetType "System.TypedReference", FieldBlittability.Element.TypedReference
            | Kind.Leaf Leaf.Decimal -> numeric typeof<decimal> StructLayoutFacts.SelfKind.Ordinary true
            | Kind.Leaf Leaf.Int128 -> numeric typeof<Int128> StructLayoutFacts.SelfKind.Int128 false
            | Kind.Leaf Leaf.UInt128 -> numeric typeof<UInt128> StructLayoutFacts.SelfKind.Int128 false
            | Kind.Leaf (Leaf.DecimalFloatingPoint t) -> numeric t StructLayoutFacts.SelfKind.DecimalFloatingPoint false

        typeOf (Kind.Struct top) |> ignore
        ucos.CreateType () |> ignore
        use stream = new MemoryStream ()
        builder.Save stream
        stream.ToArray (), List.ofSeq observations

    let private refused (method : System.Reflection.MethodInfo) : bool =
        try
            RuntimeHelpers.PrepareMethod method.MethodHandle
            false
        with :? InvalidProgramException ->
            true

    let private pinnable (t : Type) : bool =
        let boxed = RuntimeHelpers.GetUninitializedObject t

        try
            let handle = GCHandle.Alloc (boxed, GCHandleType.Pinned)
            handle.Free ()
            true
        with :? ArgumentException ->
            false

    let private hostBlittable (t : Type) : bool =
        let size =
            try
                Some (Marshal.SizeOf t)
            with :? ArgumentException ->
                None

        match size with
        | None -> false
        | Some size ->
            let buffer = Marshal.AllocHGlobal size

            try
                for i in 0 .. size - 1 do
                    Marshal.WriteByte (buffer, i, 0xABuy)

                try
                    Marshal.DestroyStructure (buffer, t)

                    [ 0 .. size - 1 ]
                    |> List.forall (fun i -> Marshal.ReadByte (buffer, i) = 0xABuy)
                with
                | :? ArgumentException
                | :? TypeLoadException
                | :? MarshalDirectiveException -> false
            finally
                Marshal.FreeHGlobal buffer

    let private check (name : string) (top : Shape) : StructLayoutFacts list =
        let context = Runtime.Loader.AssemblyLoadContext (name, isCollectible = true)

        try
            let allFacts =
                [ false ; true ]
                |> List.map (fun disabled ->
                    let image, observations = emit (name + string disabled) disabled top
                    use stream = new MemoryStream (image)
                    let assembly = context.LoadFromStream stream
                    let ucos = assembly.GetType "Ucos"

                    observations
                    |> List.map (fun observation ->
                        let method = ucos.GetMethod observation.MethodName
                        let t = method.GetParameters().[0].ParameterType
                        let facts = observation.Facts

                        let required =
                            facts.IsInt128OrHasInt128Fields
                            || facts.IsDecimalFloatingPointOrHasDecimalFloatingPointFields
                            || (if disabled then
                                    facts.ContainsGcPointers || facts.IsAutoLayoutOrHasAutoLayoutField
                                else
                                    not facts.IsBlittable)

                        let actual = refused method

                        if actual <> required then
                            failwith
                                $"Host marshalling verdict for %O{t}, disabled=%b{disabled}: host=%b{actual}, facts=%A{facts}; shape=%A{top}"

                        facts.HasLayout |> shouldEqual (t.IsLayoutSequential || t.IsExplicitLayout)

                        if not t.IsByRefLike then
                            let canPin = pinnable t
                            facts.ContainsGcPointers |> shouldEqual (not canPin)

                            if canPin then
                                facts.IsBlittable |> shouldEqual (hostBlittable t)

                        facts
                    )
                )

            allFacts.[0] |> shouldEqual allFacts.[1]
            allFacts.[0]
        finally
            context.Unload ()

    [<Test>]
    let ``Generated struct trees agree with CoreCLR and ignore field order and duplication`` () : unit =
        let observed = Dictionary<string, int> ()

        let count (key : string) : unit =
            observed.[key] <- observed.GetValueOrDefault key + 1

        let mutable cases = 0

        let property (shape : Shape) : unit =
            cases <- cases + 1

            for facts in check $"StructFacts%d{cases}" shape do
                count $"layout=%b{facts.HasLayout}"
                count $"blittable=%b{facts.IsBlittable}"
                count $"gc=%b{facts.ContainsGcPointers}"
                count $"auto=%b{facts.IsAutoLayoutOrHasAutoLayoutField}"
                count $"int128=%b{facts.IsInt128OrHasInt128Fields}"
                count $"decimal=%b{facts.IsDecimalFloatingPointOrHasDecimalFloatingPointFields}"

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 1000,
            Prop.forAll (Arb.fromGenShrink (genTop, shrinkShape)) property
        )

        for flag in [ "layout" ; "blittable" ; "gc" ; "auto" ; "int128" ] do
            for value in [ false ; true ] do
                let key = $"%s{flag}=%b{value}"

                if observed.GetValueOrDefault key < 5 then
                    failwith $"Insufficient coverage for %s{key}: %A{observed}"

        if decimalTypes.IsEmpty then
            observed.GetValueOrDefault "decimal=true" |> shouldEqual 0
        else
            observed.GetValueOrDefault "decimal=true" |> shouldBeGreaterThan 5

        TestContext.WriteLine
            $"%s{RuntimeInformation.FrameworkDescription}: %d{cases} generated shapes, observations %A{observed}"

    [<Test>]
    let ``Layout propagation is observed with other refusal reasons absent`` () : unit =
        let field (kind : Kind) : Field =
            {
                Kind = kind
                MarshalAs = None
            }

        let shape (layout : TypeLayoutKind) (charset : CharSet) (kinds : Kind list) : Shape =
            {
                Layout = layout
                CharSet = charset
                Fields = List.map field kinds
            }

        let leaf (value : Leaf) : Kind = Kind.Leaf value

        let wrap (layout : TypeLayoutKind) (kind : Kind) : Kind =
            Kind.Struct (shape layout CharSet.Ansi [ kind ])

        let nestedAuto =
            wrap TypeLayoutKind.Sequential (wrap TypeLayoutKind.Auto (leaf Leaf.Int32))

        let differingCharset =
            Kind.Struct (shape TypeLayoutKind.Sequential CharSet.Unicode [ leaf Leaf.Char ])

        let cases =
            [
                for layout in [ TypeLayoutKind.Auto ; TypeLayoutKind.Sequential ; TypeLayoutKind.Explicit ] do
                    yield shape layout CharSet.Ansi []

                    for kind in
                        [
                            leaf Leaf.Int128
                            leaf Leaf.UInt128
                            leaf Leaf.Reference
                            leaf Leaf.Enum
                            nestedAuto
                            differingCharset
                        ]
                        @ List.map (leaf << Leaf.DecimalFloatingPoint) decimalTypes do
                        yield shape layout CharSet.Ansi [ leaf Leaf.Int32 ; kind ; leaf Leaf.Int32 ]
                        yield shape layout CharSet.Unicode [ wrap TypeLayoutKind.Sequential kind ]
                for kind in
                    [
                        leaf Leaf.Byref
                        leaf Leaf.TypedReference
                        wrap TypeLayoutKind.Sequential (leaf Leaf.Byref)
                    ] do
                    yield shape TypeLayoutKind.Sequential CharSet.Ansi [ kind ]
                yield
                    { shape TypeLayoutKind.Sequential CharSet.Ansi [ differingCharset ] with
                        Fields =
                            [
                                {
                                    Kind = differingCharset
                                    MarshalAs = Some UnmanagedType.Struct
                                }
                            ]
                    }
            ]

        cases
        |> List.iteri (fun i shape -> check $"StructFactsWitness%d{i}" shape |> ignore)
