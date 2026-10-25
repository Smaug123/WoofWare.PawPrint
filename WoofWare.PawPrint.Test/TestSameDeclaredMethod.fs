namespace WoofWare.PawPrint.Test

open System.IO
open System.Reflection.Metadata.Ecma335
open FsUnitTyped
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.PawPrint

/// `MethodInfo.sameDeclaredMethod` over the methods of two assemblies compiled from the same
/// source, whose MethodDefs therefore hold the same row numbers: it holds of two methods exactly
/// where they are one MethodDef of one assembly.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSameDeclaredMethod =

    let private source : string =
        """
public class C
{
    public virtual int M() { return 1; }
    public virtual int N() { return 2; }
    public int P() { return 3; }
}
"""

    let private read (name : string) : DumpedAssembly =
        let image =
            Roslyn.compileAssembly name OutputKind.DynamicallyLinkedLibrary [] [ source ]

        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        Assembly.read loggerFactory (Some name) (new MemoryStream (image))

    [<Test>]
    let ``two methods are the same declared method exactly where they are one assembly's one MethodDef`` () : unit =
        let methods =
            [ read "SameDeclaredA" ; read "SameDeclaredB" ]
            |> List.collect (fun assembly -> assembly.Methods.Values |> Seq.toList)

        let handles (assemblyName : string) =
            methods
            |> List.filter (fun m -> m.DeclaringAssemblyFullName.StartsWith assemblyName)
            |> List.map (fun m ->
                MetadataTokens.GetRowNumber (
                    System.Reflection.Metadata.MethodDefinitionHandle.op_Implicit m.TryMetadata.Value.Handle
                    : System.Reflection.Metadata.EntityHandle
                )
            )
            |> Set.ofList

        // The premise: without it, comparing handles alone would pass for the wrong reason.
        handles "SameDeclaredA" |> shouldEqual (handles "SameDeclaredB")
        handles "SameDeclaredA" |> Set.count |> shouldBeGreaterThan 1

        for a in methods do
            for b in methods do
                let expected =
                    a.DeclaringAssemblyFullName = b.DeclaringAssemblyFullName
                    && a.TryMetadata.Value.Handle = b.TryMetadata.Value.Handle

                MethodInfo.sameDeclaredMethod a b |> shouldEqual expected
