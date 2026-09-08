namespace WoofWare.PawPrint.Test

open System.IO
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open FsUnitTyped
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.PawPrint

/// <summary>
/// <c>MethodSpec.make</c> carries the row's <c>Method</c> column over unchanged: the token it
/// records is the entity handle the metadata reader decoded, for both parents a MethodSpec may
/// have.
/// </summary>
[<TestFixture>]
module TestMethodSpecToken =

    /// A MethodSpec over a MethodDef (`Local.Ident<int>`) and one over a MemberRef
    /// (`System.Array.Empty<int>`).
    let private source =
        """
public static class Local
{
    public static T Ident<T>(T x) => x;
    public static int Use() => Ident<int>(1) + System.Array.Empty<int>().Length;
}
"""

    [<Test>]
    let ``Every MethodSpec records the handle of its Method column`` () : unit =
        let image =
            Roslyn.compileAssembly "MethodSpecTokenAssembly" OutputKind.DynamicallyLinkedLibrary [] [ source ]

        let _, loggerFactory = LoggerFactory.makeTest ()

        let dumped =
            use stream = new MemoryStream (image)
            global.WoofWare.PawPrint.AssemblyApi.read loggerFactory None stream

        use stream = new MemoryStream (image)
        use peReader = new PEReader (stream)
        let reader = peReader.GetMetadataReader ()

        let parentKinds =
            [
                for row in 1 .. reader.GetTableRowCount TableIndex.MethodSpec do
                    let handle = MetadataTokens.MethodSpecificationHandle row
                    let expected : EntityHandle = (reader.GetMethodSpecification handle).Method
                    expected.IsNil |> shouldEqual false

                    let actual : EntityHandle =
                        dumped.MethodSpecs.[handle].Method
                        |> MetadataToken.toInt
                        |> MetadataTokens.EntityHandle

                    actual |> shouldEqual expected
                    yield expected.Kind
            ]

        // Both parents a MethodSpec can have are present, so neither arm passes vacuously.
        parentKinds |> List.contains HandleKind.MethodDefinition |> shouldEqual true
        parentKinds |> List.contains HandleKind.MemberReference |> shouldEqual true
