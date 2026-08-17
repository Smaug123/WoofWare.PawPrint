namespace WoofWare.PawPrint.Test

open System.IO
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// <summary>
/// Tests for <c>TypeRef.Handle</c>, the row a <c>TypeRef</c> was parsed from.
/// </summary>
/// <remarks>
/// Name, namespace and resolution scope describe *what* is referenced; the handle says *which row*
/// did the referencing, and one module may hold several rows describing the same type. Only the
/// handle tells those apart, so the questions worth asserting are that it records the row actually
/// parsed and that it agrees with the key the assembly files it under.
/// </remarks>
[<TestFixture>]
module TestTypeRefHandle =

    /// An assembly with enough references to be worth walking. Corelib has no TypeRef rows at all,
    /// so it cannot serve here.
    let private corpus : DumpedAssembly =
        let source =
            """
using System;
using System.Collections.Generic;
using System.Text;

public class Corpus
{
    public DateTime When { get; set; }
    public TimeSpan HowLong { get; set; }
    public List<int> Ints { get; set; }
    public StringBuilder Builder { get; set; }
    public Uri Link { get; set; }

    public void Use()
    {
        Console.Out.Flush();
        GC.KeepAlive(this);
    }
}
"""

        let image =
            Roslyn.compileAssembly
                "TypeRefHandleTestAssembly"
                Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                []
                [ source ]

        let _, loggerFactory = LoggerFactory.makeTest ()
        use stream = new MemoryStream (image)
        AssemblyApi.read loggerFactory None stream

    [<Test>]
    let ``the corpus actually has type references`` () : unit =
        // Guards every assertion below: an empty TypeRef table would make them all vacuous.
        corpus.TypeRefs.Count |> shouldBeGreaterThan 3

    [<Test>]
    let ``every TypeRef records the row it is filed under`` () : unit =
        // `DumpedAssembly.TypeRefs` keys each row by its handle, and each value now carries that
        // handle too. That denormalisation has to be checked somewhere, or a parse that filed a row
        // under the wrong key would go unnoticed — and would then make two distinct references
        // compare equal.
        for KeyValue (handle, typeRef) in corpus.TypeRefs do
            typeRef.Handle.Get |> shouldEqual handle

    [<Test>]
    let ``TypeRef.make records the handle it was asked for`` () : unit =
        let metadataReader = corpus.PeReader.GetMetadataReader ()

        for handle in metadataReader.TypeReferences do
            (TypeRef.make metadataReader handle).Handle.Get |> shouldEqual handle

    [<Test>]
    let ``the parsed row agrees with the metadata reader`` () : unit =
        // The handle is only useful if it names the row the other fields were read from, so check
        // the description against what the reader says about that very row.
        let metadataReader = corpus.PeReader.GetMetadataReader ()

        for KeyValue (_, typeRef) in corpus.TypeRefs do
            let raw = metadataReader.GetTypeReference typeRef.Handle.Get

            metadataReader.GetString raw.Name |> shouldEqual typeRef.Name
            metadataReader.GetString raw.Namespace |> shouldEqual typeRef.Namespace

    [<Test>]
    let ``distinct rows are distinguishable even when they describe the same type`` () : unit =
        // The property the handle exists for. Roslyn emits one row per referenced type, so a
        // duplicate is built here rather than found: two `TypeRef`s identical but for their row.
        let existing = corpus.TypeRefs |> Seq.head |> (fun kvp -> kvp.Value)

        let duplicate =
            { existing with
                Handle = ComparableTypeReferenceHandle.Make (MetadataTokens.TypeReferenceHandle 0x00FFFFFF)
            }

        duplicate.Name |> shouldEqual existing.Name
        duplicate.Namespace |> shouldEqual existing.Namespace
        duplicate.ResolutionScope |> shouldEqual existing.ResolutionScope

        // Equal as descriptions, distinct as references. Before the handle was recorded these were
        // indistinguishable, which is what let a comparison treat two rows as one.
        duplicate |> shouldNotEqual existing

    [<Test>]
    let ``a handle renders as its metadata token`` () : unit =
        // `TypeReferenceHandle` inherits `ToString` from `obj`, so an unwrapped handle renders as
        // its type name and tells two references apart not at all.
        let rendered =
            (ComparableTypeReferenceHandle.Make (MetadataTokens.TypeReferenceHandle 5)).ToString ()

        rendered |> shouldEqual "TypeRef(0x01000005)"

    [<Test>]
    let ``handles compare by row`` () : unit =
        let make (row : int) =
            ComparableTypeReferenceHandle.Make (MetadataTokens.TypeReferenceHandle row)

        make 1 |> shouldEqual (make 1)
        make 1 |> shouldNotEqual (make 2)
        compare (make 1) (make 2) |> shouldBeSmallerThan 0
        compare (make 2) (make 1) |> shouldBeGreaterThan 0
        compare (make 3) (make 3) |> shouldEqual 0
