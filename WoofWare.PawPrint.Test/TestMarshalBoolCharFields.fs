namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Runtime.InteropServices
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// A `bool` or `char` field's native form is decided by the field's `[MarshalAs]` and its struct's
/// `CharSet`, and CoreCLR refuses every other native type on either. This sweeps the whole of that
/// space: each field type, under each way of spelling a `CharSet`, with no descriptor and with
/// every `UnmanagedType` there is, sandwiched between two bytes so that the field's width and
/// alignment both show in the offsets.
///
/// The oracle is real .NET, in-process, for the reason `TestMarshalEnumFieldLayout` gives: the
/// corpus is a library with no entry point and no static state. `Marshal.SizeOf` and
/// `Marshal.OffsetOf` give the layout, and `Marshal.DestroyStructure` over a dirty buffer gives
/// blittability, since only the struct stub of a non-blittable type zeroes the buffer.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestMarshalBoolCharFields =

    let private loggerFactory = snd (LoggerFactory.makeTest ())

    let private corelibPath : string = typeof<obj>.Assembly.Location
    let private runtimeDir : string = Path.GetDirectoryName corelibPath
    let private corelib : DumpedAssembly = Assembly.readFile loggerFactory corelibPath
    let private bct : BaseClassTypes<DumpedAssembly> = Corelib.getBaseTypes corelib

    let private corpusNamespace : string = "PawPrint.MarshalBoolChar"

    type private Shape =
        {
            Name : string
            FieldType : string
            /// `None` omits `CharSet` from `[StructLayout]`.
            CharSet : CharSet option
            /// `None` omits `[MarshalAs]`.
            MarshalAs : UnmanagedType option
        }

    let private render (shape : Shape) : string =
        let charSet =
            match shape.CharSet with
            | None -> ""
            | Some c -> $", CharSet = CharSet.%O{c}"

        let marshalAs =
            match shape.MarshalAs with
            | None -> ""
            // C# insists on these arguments for these two, whatever the field is.
            | Some UnmanagedType.ByValTStr
            | Some UnmanagedType.ByValArray -> $"[MarshalAs(UnmanagedType.%O{shape.MarshalAs.Value}, SizeConst = 1)] "
            | Some UnmanagedType.CustomMarshaler ->
                "[MarshalAs(UnmanagedType.CustomMarshaler, MarshalType = \"Nothing\")] "
            | Some u -> $"[MarshalAs((UnmanagedType)%d{int u})] "

        $"[StructLayout(LayoutKind.Sequential%s{charSet})] public struct %s{shape.Name} {{ public byte A; %s{marshalAs}public %s{shape.FieldType} F; public byte B; }}"

    let private shapes : Shape list =
        let descriptors =
            None
            :: (Enum.GetValues typeof<UnmanagedType>
                |> Seq.cast<UnmanagedType>
                |> Seq.distinct
                // C# refuses to put `VBByRefStr` on a field at all (CS7054). It is named by string
                // because naming the case is itself an obsolescence warning.
                |> Seq.filter (fun u -> Enum.GetName u <> "VBByRefStr")
                |> Seq.map Some
                |> List.ofSeq)

        [
            for fieldType in [ "bool" ; "char" ] do
                for charSet in [ None ; Some CharSet.Ansi ; Some CharSet.Unicode ; Some CharSet.Auto ] do
                    for marshalAs in descriptors do
                        yield
                            {
                                Name = ""
                                FieldType = fieldType
                                CharSet = charSet
                                MarshalAs = marshalAs
                            }
        ]
        |> List.mapi (fun i shape ->
            { shape with
                Name = $"Shape%d{i}"
            }
        )

    let private corpusBytes : byte array =
        let body = shapes |> List.map render |> String.concat "\n"

        Roslyn.compileAssembly
            corpusNamespace
            Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
            []
            [
                $"#pragma warning disable 618\nusing System.Runtime.InteropServices;\nnamespace %s{corpusNamespace};\n%s{body}\n"
            ]

    let private corpusAssembly : DumpedAssembly =
        use stream = new MemoryStream (corpusBytes)
        AssemblyApi.read loggerFactory (Some $"%s{corpusNamespace}.dll") stream

    let private corpusRuntimeAssembly : System.Reflection.Assembly =
        System.Reflection.Assembly.Load corpusBytes

    let private hostType (shape : Shape) : Type =
        corpusRuntimeAssembly.GetType $"%s{corpusNamespace}.%s{shape.Name}"
        |> Option.ofObj
        |> Option.defaultWith (fun () -> failwith $"corpus does not contain %s{shape.Name}")

    [<RequireQualifiedAccess>]
    type private HostAnswer =
        | Layout of size : int * offsets : int list * blittable : bool
        | CannotMarshal

    /// Real .NET's layout of `t`, and whether it is blittable: only the struct stub of a
    /// non-blittable type runs a Cleanup pass, and that zeroes the image, so `DestroyStructure`
    /// over a dirty buffer leaves it dirty exactly when the type is blittable.
    let private hostAnswerOf (t : Type) : HostAnswer =
        match
            (try
                Some (Marshal.SizeOf t)
             with :? ArgumentException ->
                 None)
        with
        | None -> HostAnswer.CannotMarshal
        | Some size ->
            let offsets =
                t.GetFields ()
                |> Array.sortBy _.MetadataToken
                |> Array.map (fun field -> int (Marshal.OffsetOf (t, field.Name)))
                |> List.ofArray

            let dirty = 0xABuy
            let buffer = Marshal.AllocHGlobal size

            try
                for i in 0 .. size - 1 do
                    Marshal.WriteByte (buffer, i, dirty)

                Marshal.DestroyStructure (buffer, t)

                let untouched =
                    [ 0 .. size - 1 ] |> List.forall (fun i -> Marshal.ReadByte (buffer, i) = dirty)

                HostAnswer.Layout (size, offsets, untouched)
            finally
                Marshal.FreeHGlobal buffer

    let private hostAnswers : (Shape * HostAnswer) list =
        shapes |> List.map (fun shape -> shape, hostAnswerOf (hostType shape))

    let private initialState : IlMachineState =
        let dirs = ImmutableArray.CreateRange [ runtimeDir ]
        let state = IlMachineState.initial loggerFactory dirs corelib

        { state with
            ConcreteTypes = Corelib.concretizeAll state._LoadedAssemblies bct AllConcreteTypes.Empty
        }

    let private baseState : IlMachineState =
        initialState.WithLoadedAssembly corpusAssembly

    /// The type named `name` in `assembly`, concretized in `state`, which must already know the
    /// assembly.
    let private concretizeNamed
        (state : IlMachineState)
        (assembly : DumpedAssembly)
        (name : string)
        : IlMachineState * ConcreteTypeHandle
        =
        let typeInfo =
            assembly.TypeDefs
            |> Seq.map (fun kvp -> kvp.Value)
            |> Seq.filter (fun ti -> ti.Name = name)
            |> Seq.exactlyOne

        IlMachineTypeResolution.concretizeType
            loggerFactory
            bct
            state
            typeInfo.AssemblyFullName
            ImmutableArray.Empty
            ImmutableArray.Empty
            (TypeDefn.FromDefinition (typeInfo.Identity, System.Reflection.Metadata.SignatureTypeKind.ValueType))

    /// PawPrint's answer to `hostAnswerOf` for the type named `name` in `assembly`.
    let private pawPrintAnswerOf
        (state : IlMachineState)
        (assembly : DumpedAssembly)
        (name : string)
        : Result<HostAnswer, string>
        =
        let state, handle = concretizeNamed state assembly name

        match IlMachineState.cliTypeZeroOfHandle state bct handle with
        | CliType.ValueType vt, state ->
            match CliValueType.TryComputeMarshalLayout state.ConcreteTypes state._LoadedAssemblies bct vt with
            | Result.Error (MarshalSizeError.NotMarshalable _) -> Result.Ok HostAnswer.CannotMarshal
            | Result.Error (MarshalSizeError.NotImplemented reason) -> Result.Error reason
            | Result.Ok (size, placements) ->
                let blittable =
                    StructMarshalStub.isBlittableStruct
                        state.ConcreteTypes
                        state._LoadedAssemblies
                        bct
                        (CliType.ValueType vt)

                HostAnswer.Layout (size.Size, placements |> List.map _.NativeOffset, blittable)
                |> Result.Ok
        | other, _ -> failwith $"%s{name} should be a value type, but its zero is %O{other}"

    [<Test>]
    let ``bool and char fields lay out, refuse and blit as real .NET does`` () : unit =
        let failures =
            hostAnswers
            |> List.choose (fun (shape, expected) ->
                let actual = pawPrintAnswerOf baseState corpusAssembly shape.Name

                if actual = Result.Ok expected then
                    None
                else
                    Some $"%s{render shape}\n  real .NET: %A{expected}\n  PawPrint:  %A{actual}"
            )

        match failures with
        | [] -> ()
        | _ ->
            let described = String.concat "\n" failures
            failwith $"%d{failures.Length} of %d{shapes.Length} shapes disagree:\n%s{described}"

    [<Test>]
    let ``the sweep sees every native form, and refusals`` () : unit =
        // Vacuity guard: the comparison above means little unless real .NET actually produced each
        // native width, both blittability answers, and refusals, somewhere in the sweep.
        let fieldWidths =
            hostAnswers
            |> List.choose (fun (shape, answer) ->
                match answer with
                | HostAnswer.Layout (_, [ _ ; f ; b ], blittable) -> Some (shape.FieldType, b - f, blittable)
                | HostAnswer.Layout _
                | HostAnswer.CannotMarshal -> None
            )
            |> Set.ofList

        fieldWidths
        |> shouldEqual (
            Set.ofList
                [
                    "bool", 4, false // BOOL
                    "bool", 1, false // C bool
                    "char", 1, false // ANSI
                    "char", 2, true // UTF-16
                ]
        )

        hostAnswers
        |> List.filter (fun (_, answer) -> answer = HostAnswer.CannotMarshal)
        |> List.length
        |> shouldBeGreaterThan (shapes.Length / 2)

    /// Structs holding enums over `char` and `bool`, which C# cannot declare. The enums themselves
    /// are Ansi, as `DefineEnum` makes them, so a struct that judged an enum field under the
    /// enum's own `CharSet` rather than its container's would get the Unicode ones wrong.
    let private enumCorpusBytes : byte array =
        let builder =
            System.Reflection.Emit.PersistedAssemblyBuilder (
                System.Reflection.AssemblyName "PawPrint.MarshalBoolCharEnums",
                typeof<obj>.Assembly
            )

        let md = builder.DefineDynamicModule "PawPrint.MarshalBoolCharEnums"

        let defineEnum (name : string) (underlying : Type) : Type =
            let e = md.DefineEnum (name, System.Reflection.TypeAttributes.Public, underlying)
            e.DefineLiteral ("One", Convert.ChangeType (1, underlying)) |> ignore
            e.CreateType ()

        let enumOverChar = defineEnum "EChar" typeof<char>
        let enumOverBool = defineEnum "EBool" typeof<bool>

        for name, charSet, field in
            [
                "CharEnumUnicode", System.Reflection.TypeAttributes.UnicodeClass, enumOverChar
                "CharEnumAnsi", System.Reflection.TypeAttributes.AnsiClass, enumOverChar
                "BoolEnum", System.Reflection.TypeAttributes.AnsiClass, enumOverBool
            ] do
            let t =
                md.DefineType (
                    name,
                    System.Reflection.TypeAttributes.Public
                    ||| System.Reflection.TypeAttributes.Sealed
                    ||| System.Reflection.TypeAttributes.SequentialLayout
                    ||| charSet,
                    typeof<ValueType>
                )

            t.DefineField ("A", typeof<byte>, System.Reflection.FieldAttributes.Public)
            |> ignore

            t.DefineField ("F", field, System.Reflection.FieldAttributes.Public) |> ignore

            t.DefineField ("B", typeof<byte>, System.Reflection.FieldAttributes.Public)
            |> ignore

            t.CreateType () |> ignore

        use stream = new MemoryStream ()
        builder.Save stream
        stream.ToArray ()

    [<Test>]
    let ``An enum field over bool or char marshals as its underlying type under its container's CharSet`` () : unit =
        let dumped =
            use stream = new MemoryStream (enumCorpusBytes)
            AssemblyApi.read loggerFactory (Some "PawPrint.MarshalBoolCharEnums.dll") stream

        let hostAssembly = System.Reflection.Assembly.Load enumCorpusBytes
        let state = initialState.WithLoadedAssembly dumped

        let answers =
            [ "CharEnumUnicode" ; "CharEnumAnsi" ; "BoolEnum" ]
            |> List.map (fun name -> name, hostAnswerOf (hostAssembly.GetType name), pawPrintAnswerOf state dumped name)

        for name, host, pawPrint in answers do
            if pawPrint <> Result.Ok host then
                failwith $"%s{name}: real .NET %A{host}, PawPrint %A{pawPrint}"

        // Vacuity guard: the Unicode one is the case a container-blind walk gets wrong.
        answers
        |> List.map (fun (_, host, _) -> host)
        |> shouldEqual
            [
                HostAnswer.Layout (6, [ 0 ; 2 ; 4 ], true)
                HostAnswer.Layout (3, [ 0 ; 1 ; 2 ], false)
                HostAnswer.Layout (12, [ 0 ; 4 ; 8 ], false)
            ]

    /// Two assemblies of types carrying `BestFitMappingAttribute`, one with the attribute on the
    /// assembly too, and the flags CoreCLR's `ReadBestFitCustomAttribute` reads for each. There is
    /// no host oracle for these, since off Windows CoreLib's ANSI conversion ignores them; the
    /// table is CoreCLR's rule, including that a type-level attribute without the named argument
    /// leaves the assembly's `ThrowOnUnmappableChar` in place.
    let private bestFitCases : (string * (string * (string * (bool * bool))) list) list =
        [
            "",
            [
                "public struct Plain { public char C; }", ("Plain", (true, false))
                "[BestFitMapping(false)] public struct Off { public char C; }", ("Off", (false, false))
                "[BestFitMapping(true, ThrowOnUnmappableChar = true)] public struct OnThrow { public char C; }",
                ("OnThrow", (true, true))
            ]
            "[assembly: BestFitMapping(false, ThrowOnUnmappableChar = true)]",
            [
                "public struct Inherits { public char C; }", ("Inherits", (false, true))
                "[BestFitMapping(true)] public struct KeepsThrow { public char C; }", ("KeepsThrow", (true, true))
                "[BestFitMapping(true, ThrowOnUnmappableChar = false)] public struct Overrides { public char C; }",
                ("Overrides", (true, false))
            ]
        ]

    [<Test>]
    let ``bestFitFlags reads BestFitMappingAttribute as CoreCLR does`` () : unit =
        bestFitCases
        |> List.iteri (fun i (assemblyAttribute, types) ->
            let name = $"PawPrint.MarshalBestFit%d{i}"

            let source =
                let body = types |> List.map fst |> String.concat "\n"
                $"using System.Runtime.InteropServices;\n%s{assemblyAttribute}\n%s{body}\n"

            let dumped =
                use stream =
                    new MemoryStream (
                        Roslyn.compileAssembly
                            name
                            Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                            []
                            [ source ]
                    )

                AssemblyApi.read loggerFactory (Some $"%s{name}.dll") stream

            let state = initialState.WithLoadedAssembly dumped

            for _, (typeName, expected) in types do
                let state, handle = concretizeNamed state dumped typeName

                StructMarshalStub.bestFitFlags state.ConcreteTypes state._LoadedAssemblies handle
                |> shouldEqual expected

                // And the struct stub's ANSI conversion is handed exactly those flags.
                let zero, state = IlMachineState.cliTypeZeroOfHandle state bct handle

                match StructMarshalStub.tryComputePlan state.ConcreteTypes state._LoadedAssemblies bct zero with
                | Result.Ok plan ->
                    plan.Steps
                    |> List.map _.Kind
                    |> shouldEqual [ StructMarshalFieldKind.AnsiChar (fst expected, snd expected) ]
                | Result.Error err -> failwith $"%s{typeName}: expected a marshal plan, got %s{err.Reason}"
        )

    /// An empty field-marshal blob is no descriptor to `IsFieldBlittable` and a refused one to
    /// `ParseNativeTypeInfo` (see `FieldMarshalDescriptor.Empty`), so CoreCLR lays the struct out
    /// only when it is blittable. A `bool` field never is. A `char` field is not under an ANSI
    /// `CharSet`, and is under a Unicode one, where the other fields decide; PawPrint does not
    /// model that.
    [<Test>]
    let ``An empty marshalling blob on a bool or char field is refused exactly where CoreCLR refuses it`` () : unit =
        match BoolCharMarshal.ofBoolField (Some FieldMarshalDescriptor.Empty) with
        | Result.Error (MarshalSizeError.NotMarshalable _) -> ()
        | other -> failwith $"bool: expected NotMarshalable, got %O{other}"

        for charSet, expectRefusal in
            [
                CharSet.None, true
                CharSet.Ansi, true
                CharSet.Auto, true
                CharSet.Unicode, false
            ] do
            match BoolCharMarshal.ofCharField charSet (Some FieldMarshalDescriptor.Empty), expectRefusal with
            | Result.Error (MarshalSizeError.NotMarshalable _), true
            | Result.Error (MarshalSizeError.NotImplemented _), false -> ()
            | other, _ -> failwith $"char under %O{charSet}: got %O{other}"
