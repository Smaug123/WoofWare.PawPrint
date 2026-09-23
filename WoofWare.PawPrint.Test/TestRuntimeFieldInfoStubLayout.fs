namespace WoofWare.PawPrint.Test

open System.IO
open FsUnitTyped
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.PawPrint

/// `RuntimeFieldInfoStubLayout.classify` recognises exactly the `RuntimeFieldInfoStub` layouts
/// PawPrint knows how to write and read, and names the fields of any other.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestRuntimeFieldInfoStubLayout =

    /// The instance fields of .NET 10's `RuntimeFieldInfoStub`, as C# (type, name) pairs, in
    /// declaration order.
    let private net10Fields : (string * string) list =
        [
            "object", "m_keepalive"
            "object", "m_c"
            "object", "m_d"
            "int", "m_b"
            "object", "m_e"
            "object", "m_f"
            "RuntimeFieldHandleInternal", "m_fieldHandle"
        ]

    let private typeAlphabet : string list =
        [
            "object"
            "int"
            "IntPtr"
            "RuntimeFieldHandleInternal"
            "string"
            "long"
        ]

    let private replaceAt (i : int) (x : 'a) (l : 'a list) : 'a list =
        l |> List.mapi (fun j y -> if i = j then x else y)

    let private removeAt (i : int) (l : 'a list) : 'a list =
        l |> List.indexed |> List.filter (fun (j, _) -> j <> i) |> List.map snd

    let private insertAt (i : int) (x : 'a) (l : 'a list) : 'a list = List.take i l @ [ x ] @ List.skip i l

    /// Every stub field list one edit away from net10's, plus net10's itself, and a relayout of
    /// the kind .NET 11 made: padding reordered, `m_b` and `m_fieldHandle` both bare `IntPtr`s.
    let private stubVariants : (string * string) list list =
        let n = net10Fields.Length

        [
            yield net10Fields

            for i in 0 .. n - 1 do
                yield removeAt i net10Fields

                let ty, name = net10Fields.[i]
                yield replaceAt i (ty, name + "_") net10Fields

                for other in typeAlphabet do
                    if other <> ty then
                        yield replaceAt i (other, name) net10Fields

            for i in 0 .. n - 2 do
                yield replaceAt i net10Fields.[i + 1] (replaceAt (i + 1) net10Fields.[i] net10Fields)

            for i in 0..n do
                yield insertAt i ("object", "m_extra") net10Fields

            yield
                [
                    "object", "m_keepalive"
                    "object", "m_c"
                    "object", "m_d"
                    "object", "m_e"
                    "object", "m_f"
                    "IntPtr", "m_b"
                    "IntPtr", "m_fieldHandle"
                ]
        ]
        |> List.distinct

    /// Candidate `RuntimeFieldHandleInternal`s, by name, with their bodies and whether the layout
    /// accepts a stub wrapping one.
    let private handleVariants : (string * string * bool) list =
        [
            "RuntimeFieldHandleInternal", "internal IntPtr m_handle;", true
            "HandleWithStatic", "internal IntPtr m_handle; internal static int s_count;", true
            "WideHandle", "internal IntPtr m_handle; internal IntPtr m_extra;", false
            "LongHandle", "internal long m_handle;", false
            "RenamedHandle", "internal IntPtr m_value;", false
            "EmptyHandle", "", false
        ]

    let private stubClass (name : string) (fields : (string * string) list) : string =
        let body =
            fields
            |> List.map (fun (ty, field) -> $"        private %s{ty} %s{field};")
            |> String.concat "\n"

        $"    internal sealed class %s{name}\n    {{\n%s{body}\n    }}"

    let private fabricated : Lazy<DumpedAssembly> =
        lazy
            let stubs =
                stubVariants |> List.mapi (fun i fields -> stubClass $"Stub%i{i}" fields)

            let handles =
                handleVariants
                |> List.map (fun (name, body, _) -> $"    internal struct %s{name} {{ %s{body} }}")

            let overHandles =
                handleVariants
                |> List.map (fun (name, _, _) ->
                    net10Fields
                    |> replaceAt 6 (name, "m_fieldHandle")
                    |> stubClass $"StubOver%s{name}"
                )

            // A stub carrying an extra static field: statics are not part of the layout.
            let withStatic =
                (stubClass "StubWithStatic" net10Fields)
                    .Replace ("private object m_keepalive;", "private static int s_extra; private object m_keepalive;")

            let source =
                [
                    yield "using System;"
                    yield "namespace Fabricated"
                    yield "{"
                    yield! stubs
                    yield! handles
                    yield! overHandles
                    yield withStatic
                    yield "}"
                ]
                |> String.concat "\n"

            let image =
                Roslyn.compileAssembly "FabricatedStubs" OutputKind.DynamicallyLinkedLibrary [] [ source ]

            let _, loggerFactory = LoggerFactory.makeTest ()
            use _loggerFactoryResource = loggerFactory
            Assembly.read loggerFactory (Some "FabricatedStubs") (new MemoryStream (image))

    let private typeNamed (assembly : DumpedAssembly) (name : string) : TypeInfo<GenericParamFromMetadata, TypeDefn> =
        assembly.TypeDefs.Values
        |> Seq.filter (fun ty -> ty.Namespace = "Fabricated" && ty.Name = name)
        |> Seq.exactlyOne

    let private isRecognised (result : Result<RuntimeFieldInfoStubLayout, string>) : bool =
        match result with
        | Ok (RuntimeFieldInfoStubLayout.WrappedHandle _) -> true
        | Error _ -> false

    [<Test>]
    let ``the host's CoreLib stub has the wrapped-handle layout`` () =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        use stream = File.OpenRead corelibPath
        let corelib = Assembly.read loggerFactory (Some corelibPath) stream
        let baseClassTypes = Corelib.getBaseTypes corelib

        match
            RuntimeFieldInfoStubLayout.classify
                corelib
                baseClassTypes.RuntimeFieldInfoStub
                baseClassTypes.RuntimeFieldHandleInternal
        with
        | Ok (RuntimeFieldInfoStubLayout.WrappedHandle fields) ->
            [
                fields.Keepalive
                fields.C
                fields.D
                fields.B
                fields.E
                fields.F
                fields.FieldHandle
                fields.HandleInternalHandle
            ]
            |> List.map (fun field -> field.Name)
            |> shouldEqual
                [
                    "m_keepalive"
                    "m_c"
                    "m_d"
                    "m_b"
                    "m_e"
                    "m_f"
                    "m_fieldHandle"
                    "m_handle"
                ]
        | Error refusal -> failwith refusal

    [<Test>]
    let ``classify recognises exactly net10's stub field list`` () =
        let assembly = fabricated.Force ()
        let handleInternal = typeNamed assembly "RuntimeFieldHandleInternal"

        let recognised =
            stubVariants
            |> List.mapi (fun i fields ->
                let result =
                    RuntimeFieldInfoStubLayout.classify assembly (typeNamed assembly $"Stub%i{i}") handleInternal

                let expected = (fields = net10Fields)

                if isRecognised result <> expected then
                    failwith
                        $"Stub%i{i} with fields %A{fields} classified as %A{result}; expected recognised=%b{expected}"

                expected
            )
            |> List.filter id
            |> List.length

        // Vacuity guard: net10's own list is among the variants, and nothing else is accepted.
        recognised |> shouldEqual 1
        stubVariants.Length |> shouldBeGreaterThan 60

    [<Test>]
    let ``static fields are not part of the layout`` () =
        let assembly = fabricated.Force ()

        RuntimeFieldInfoStubLayout.classify
            assembly
            (typeNamed assembly "StubWithStatic")
            (typeNamed assembly "RuntimeFieldHandleInternal")
        |> isRecognised
        |> shouldEqual true

    [<Test>]
    let ``classify recognises only a RuntimeFieldHandleInternal of one IntPtr m_handle`` () =
        let assembly = fabricated.Force ()

        for (name, _, expected) in handleVariants do
            let result =
                RuntimeFieldInfoStubLayout.classify
                    assembly
                    (typeNamed assembly $"StubOver%s{name}")
                    (typeNamed assembly name)

            if isRecognised result <> expected then
                failwith $"StubOver%s{name} classified as %A{result}; expected recognised=%b{expected}"

    [<Test>]
    let ``m_fieldHandle must be the RuntimeFieldHandleInternal classify is given`` () =
        let assembly = fabricated.Force ()

        // `StubOverWideHandle` has the right field names and a value-type `m_fieldHandle`, but
        // that value type is not the one passed as the handle type.
        RuntimeFieldInfoStubLayout.classify
            assembly
            (typeNamed assembly "StubOverWideHandle")
            (typeNamed assembly "RuntimeFieldHandleInternal")
        |> isRecognised
        |> shouldEqual false

    [<Test>]
    let ``a relayout of net11's kind is refused, naming the fields found`` () =
        let assembly = fabricated.Force ()
        let index = stubVariants.Length - 1

        match
            RuntimeFieldInfoStubLayout.classify
                assembly
                (typeNamed assembly $"Stub%i{index}")
                (typeNamed assembly "RuntimeFieldHandleInternal")
        with
        | Ok layout -> failwith $"expected a refusal, got %A{layout}"
        | Error refusal ->
            refusal
            |> shouldContainText
                $"Fabricated.Stub%i{index} {{ obj m_keepalive; obj m_c; obj m_d; obj m_e; obj m_f; intptr m_b; intptr m_fieldHandle }}"

            refusal
            |> shouldContainText "with Fabricated.RuntimeFieldHandleInternal { intptr m_handle }"
