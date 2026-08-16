namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Reflection.Metadata
open System.Reflection.PortableExecutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// <summary>
/// Tests for <see cref="LocalSignatureDecoding" />, the locals counterpart of
/// <see cref="MethodSignatureDecoding" />.
/// </summary>
[<TestFixture>]
module TestLocalSignatureDecoding =

    let private corelibReader : PEReader =
        new PEReader (File.OpenRead typeof<obj>.Assembly.Location)

    let private metadataReader : MetadataReader = corelibReader.GetMetadataReader ()

    let private assemblyName : AssemblyName =
        metadataReader.GetAssemblyDefinition().GetAssemblyName ()

    let private decode (blob : byte[]) : Collections.Immutable.ImmutableArray<TypeDefn> =
        LocalSignatureDecoding.decode assemblyName metadataReader blob

    /// The real encoder, driven exactly as `DynamicILGenerator.DeclareLocal` drives it.
    let private encode (localTypes : Type list) : byte[] =
        let helper = SignatureHelper.GetLocalVarSigHelper ()

        for t in localTypes do
            helper.AddArgument t

        helper.GetSignature ()

    [<Test>]
    let ``locals round-trip in order`` () : unit =
        decode (encode [ typeof<int> ; typeof<string> ; typeof<double> ])
        |> Seq.toList
        |> shouldEqual
            [
                TypeDefn.PrimitiveType PrimitiveType.Int32
                TypeDefn.PrimitiveType PrimitiveType.String
                TypeDefn.PrimitiveType PrimitiveType.Double
            ]

    /// The case `MetadataReader.DecodeLocalSignature` refuses outright, and which every dynamic
    /// method that declares no locals produces.
    [<Test>]
    let ``a zero-count signature decodes to no locals`` () : unit =
        decode (encode []) |> Seq.toList |> shouldEqual []

    /// The locals counterpart of `a parameter count exceeding the blob is reported as truncation`:
    /// `SignatureDecoder` already refuses this, but with a message blaming the
    /// `ELEMENT_TYPE_INTERNAL` gap rather than the truncation that actually occurred.
    [<Test>]
    let ``a local count exceeding the blob is reported as truncation`` () : unit =
        // LOCAL_SIG (0x07), then 0x1FFFFFFF as a four-byte compressed integer.
        let absurd = [| 0x07uy ; 0xDFuy ; 0xFFuy ; 0xFFuy ; 0xFFuy |]

        let exn = Assert.Throws<Exception> (fun () -> decode absurd |> ignore)

        exn.Message |> shouldContainText "truncated or corrupt"
        exn.Message |> shouldContainText "536870911"

    /// The bound must not reject a legitimate signature whose locals are one byte each.
    [<Test>]
    let ``the local-count bound admits a maximally tight signature`` () : unit =
        decode (encode (List.replicate 8 typeof<int>)) |> Seq.length |> shouldEqual 8
