namespace WoofWare.PawPrint.Test

open System.IO
open FsUnitTyped
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.PawPrint

/// `BindToMethodDetailsLayout.classify` recognises exactly .NET 11's `Delegate.BindToMethodDetails`,
/// the struct `Delegate_BindToMethodInfo` writes, and refuses every other.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestBindToMethodDetailsLayout =

    /// The instance fields of a candidate struct, as C# (type, name) pairs in declaration order.
    type private Variant = (string * string) list

    let private net11 : Variant =
        [
            "int", "selfReferentialTarget"
            "IntPtr", "methodPtr"
            "IntPtr", "methodPtrAux"
            "IntPtr", "extraData"
            "GCHandle", "loaderAllocatorGCHandle"
        ]

    let private typeAlphabet : string list =
        [ "int" ; "uint" ; "IntPtr" ; "long" ; "object" ; "GCHandle" ; "OtherHandle" ]

    /// `fields` and every field list one edit away from it: a field removed, renamed, retyped,
    /// swapped with its neighbour, or an extra field inserted.
    let private variants : Variant list =
        let n = net11.Length
        let replaceAt (i : int) (x : string * string) = List.updateAt i x

        [
            yield net11
            for i in 0 .. n - 1 do
                yield List.removeAt i net11

                let ty, name = net11.[i]
                yield replaceAt i (ty, name + "_") net11

                for other in typeAlphabet do
                    if other <> ty then
                        yield replaceAt i (other, name) net11

            for i in 0 .. n - 2 do
                yield replaceAt i net11.[i + 1] (replaceAt (i + 1) net11.[i] net11)

            for i in 0..n do
                yield List.insertAt i ("int", "extra") net11
        ]
        |> List.distinct

    let private structBody (fields : Variant) : string =
        fields
        |> List.map (fun (ty, field) -> $"public %s{ty} %s{field};")
        |> String.concat " "

    /// A `Delegate{i}` per variant, each with a nested `BindToMethodDetails` declaring it; a
    /// `Delegate` whose struct carries statics beside .NET 11's fields; and a top-level
    /// `BindToMethodDetails` with .NET 11's fields, nested in nothing. The assembly declares its
    /// own `System.Runtime.InteropServices.GCHandle`, as CoreLib does, and an `OtherHandle` beside it.
    let private fabricated : Lazy<DumpedAssembly> =
        lazy
            let delegateWith (name : string) (body : string) : string =
                $"    internal abstract class Delegate%s{name} {{ internal struct BindToMethodDetails {{ %s{body} }} }}"

            let source =
                [
                    yield "using System;"
                    yield "using System.Runtime.InteropServices;"
                    yield "namespace System.Runtime.InteropServices"
                    yield "{"
                    yield "    public struct GCHandle { private IntPtr _handle; }"
                    yield "    public struct OtherHandle { private IntPtr _handle; }"
                    yield "}"
                    yield "namespace Fabricated"
                    yield "{"
                    yield!
                        variants
                        |> List.mapi (fun i variant -> delegateWith $"%i{i}" (structBody variant))
                    yield delegateWith "WithStatics" ("public static int s_count; " + structBody net11)
                    yield $"    internal struct BindToMethodDetails {{ %s{structBody net11} }}"
                    yield "    internal abstract class DelegateTopLevel { }"
                    yield "}"
                ]
                |> String.concat "\n"

            let image =
                Roslyn.compileAssembly "FabricatedBindToMethodDetails" OutputKind.DynamicallyLinkedLibrary [] [ source ]

            let _, loggerFactory = LoggerFactory.makeTest ()
            use _loggerFactoryResource = loggerFactory
            Assembly.read loggerFactory (Some "FabricatedBindToMethodDetails") (new MemoryStream (image))

    let private typesNamed
        (assembly : DumpedAssembly)
        (name : string)
        : TypeInfo<GenericParamFromMetadata, TypeDefn> list
        =
        assembly.TypeDefs.Values |> Seq.filter (fun ty -> ty.Name = name) |> Seq.toList

    let private delegateNamed
        (assembly : DumpedAssembly)
        (name : string)
        : TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        typesNamed assembly name |> List.exactlyOne

    /// The `BindToMethodDetails` nested in `delegateType`.
    let private nestedDetails
        (assembly : DumpedAssembly)
        (delegateType : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        typesNamed assembly "BindToMethodDetails"
        |> List.filter (fun ty -> ty.DeclaringType = delegateType.TypeDefHandle)
        |> List.exactlyOne

    let private fieldNames (fields : BindToMethodDetailsFields) : string list =
        [
            fields.SelfReferentialTarget.Name
            fields.MethodPtr.Name
            fields.MethodPtrAux.Name
            fields.ExtraData.Name
            fields.LoaderAllocatorGcHandle.Name
        ]

    [<Test>]
    let ``exactly .NET 11's struct is recognised, with each field in its place`` () =
        let assembly = fabricated.Force ()

        variants
        |> List.iteri (fun i variant ->
            let delegateType = delegateNamed assembly $"Delegate%i{i}"

            let result =
                BindToMethodDetailsLayout.classify assembly delegateType (nestedDetails assembly delegateType)

            match result with
            | Ok fields when variant = net11 -> fieldNames fields |> shouldEqual (List.map snd net11)
            | Ok _ -> failwith $"recognised %A{variant}, which is not .NET 11's BindToMethodDetails"
            | Error refusal when variant = net11 -> failwith $"refused .NET 11's BindToMethodDetails: %s{refusal}"
            | Error refusal ->
                refusal |> shouldContainText "not the struct PawPrint knows"

                for _, name in variant do
                    refusal |> shouldContainText name
        )

    [<Test>]
    let ``statics beside .NET 11's fields are not compared`` () =
        let assembly = fabricated.Force ()
        let delegateType = delegateNamed assembly "DelegateWithStatics"

        match BindToMethodDetailsLayout.classify assembly delegateType (nestedDetails assembly delegateType) with
        | Ok fields -> fieldNames fields |> shouldEqual (List.map snd net11)
        | Error refusal -> failwith refusal

    [<Test>]
    let ``a struct not nested in the delegate type is refused`` () =
        let assembly = fabricated.Force ()

        let topLevel =
            typesNamed assembly "BindToMethodDetails"
            |> List.filter (fun ty -> ty.Namespace = "Fabricated")
            |> List.exactlyOne

        match BindToMethodDetailsLayout.classify assembly (delegateNamed assembly "DelegateTopLevel") topLevel with
        | Ok _ -> failwith "recognised a BindToMethodDetails nested in nothing"
        | Error refusal -> refusal |> shouldContainText "is not nested in Fabricated.DelegateTopLevel"

    let runtimes : TestCaseData list = FrameworkUnderTest.everyRuntime

    [<TestCaseSource(nameof runtimes)>]
    let ``each runtime's CoreLib declares the struct only where its QCall writes one`` (runtime : EmulatedRuntime) =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let corelib =
            AssemblyApi.readFile loggerFactory (FrameworkUnderTest.requireCoreLibPathOf runtime)

        let baseClassTypes = BaseClassTypes.ofCorelib corelib

        let declared =
            typesNamed corelib "BindToMethodDetails"
            |> List.filter (fun ty -> ty.DeclaringType = baseClassTypes.DelegateType.TypeDefHandle)

        match runtime, declared with
        | EmulatedRuntime.Net10, [] -> ()
        | EmulatedRuntime.Net11, [ details ] ->
            match BindToMethodDetailsLayout.classify corelib baseClassTypes.DelegateType details with
            | Ok fields -> fieldNames fields |> shouldEqual (List.map snd net11)
            | Error refusal -> failwith refusal
        | _, declared -> failwith $"%O{runtime}'s CoreLib declares %d{declared.Length} Delegate.BindToMethodDetails"
