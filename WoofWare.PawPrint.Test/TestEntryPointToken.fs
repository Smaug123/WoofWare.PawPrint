namespace WoofWare.PawPrint.Test

open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open FsUnitTyped
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.PawPrint

/// <summary>
/// How <c>DumpedAssembly.MainMethod</c> is recovered from the CLI header's
/// <c>EntryPointTokenOrRelativeVirtualAddress</c> field.
/// </summary>
/// <remarks>
/// Roslyn only ever emits a MethodDef token there, so every other shape the field can take is
/// reached by patching the compiled image: the field sits at offset 20 of the CLI header and
/// <c>Flags</c> at offset 16 (ECMA-335 II.25.3.3). The host CLR is the oracle for the cases it
/// loads: <c>Assembly.EntryPoint</c> is what CoreCLR's <c>Assembly::GetEntryPoint</c>
/// (vm/assembly.cpp) answers for the same bytes.
/// </remarks>
[<TestFixture>]
module TestEntryPointToken =

    let private source =
        """
public static class Helper
{
    public static int One() => 1;
    public static int Two() => 2;
}

public static class Entry
{
    public static int Main(string[] args) => Helper.One() + Helper.Two();
}
"""

    let private compile (kind : OutputKind) : byte[] =
        Roslyn.compileAssembly "EntryPointTokenAssembly" kind [] [ source ]

    let private read (image : byte[]) : DumpedAssembly =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use stream = new MemoryStream (image)
        global.WoofWare.PawPrint.AssemblyApi.read loggerFactory None stream

    let private corHeaderStart (image : byte[]) : int =
        use stream = new MemoryStream (image)
        use peReader = new PEReader (stream)
        peReader.PEHeaders.CorHeaderStartOffset

    let private methodDefRowCount (image : byte[]) : int =
        use stream = new MemoryStream (image)
        use peReader = new PEReader (stream)
        peReader.GetMetadataReader().GetTableRowCount TableIndex.MethodDef

    /// A copy of `image` whose CLI header names `token` as the entry point.
    let private withEntryPointToken (token : int) (image : byte[]) : byte[] =
        let patched = Array.copy image
        (System.BitConverter.GetBytes token).CopyTo (patched, corHeaderStart image + 20)
        patched

    /// A copy of `image` with `flag` added to the CLI header's `Flags`.
    let private withCorFlag (flag : CorFlags) (image : byte[]) : byte[] =
        let offset = corHeaderStart image + 16
        let existing = System.BitConverter.ToInt32 (image, offset)
        let patched = Array.copy image
        (System.BitConverter.GetBytes (existing ||| int flag)).CopyTo (patched, offset)
        patched

    let private tokenOf (handle : MethodDefinitionHandle) : int =
        MetadataTokens.GetToken (MethodDefinitionHandle.op_Implicit handle : EntityHandle)

    let private entryPointToken (image : byte[]) : int =
        System.BitConverter.ToInt32 (image, corHeaderStart image + 20)

    /// What the host CLR reports as the entry point of the same bytes.
    let private hostEntryPoint (image : byte[]) : MethodInfo = (Assembly.Load image).EntryPoint

    [<Test>]
    let ``A console application's entry point is the MethodDef the host CLR reports`` () : unit =
        let image = compile OutputKind.ConsoleApplication
        let dumped = read image

        let handle =
            match dumped.MainMethod with
            | Some handle -> handle
            | None -> failwith "expected an entry point"

        dumped.Methods.[handle].Name |> shouldEqual "Main"

        let host = hostEntryPoint image
        host |> shouldNotEqual null
        tokenOf handle |> shouldEqual host.MetadataToken

    [<Test>]
    let ``A library has no entry point`` () : unit =
        let image = compile OutputKind.DynamicallyLinkedLibrary
        // The field is literally zero in a DLL, so this is the arm the header reaches directly.
        entryPointToken image |> shouldEqual 0
        (read image).MainMethod |> shouldEqual None
        hostEntryPoint image |> shouldEqual null

    /// Every MethodDef row can be named as the entry point, and the handle recovered names that
    /// row: the round trip through `MetadataTokens.GetToken` is exact for every row the table has.
    [<Test>]
    let ``Every MethodDef token round-trips to the same row`` () : unit =
        let image = compile OutputKind.ConsoleApplication
        let rows = methodDefRowCount image
        // Three methods plus the two constructors Roslyn adds is the least this source yields.
        rows |> shouldBeGreaterThan 1

        for row in 1..rows do
            let token = 0x06000000 ||| row
            let dumped = read (withEntryPointToken token image)

            match dumped.MainMethod with
            | Some handle ->
                tokenOf handle |> shouldEqual token

                MetadataTokens.GetRowNumber (MethodDefinitionHandle.op_Implicit handle : EntityHandle)
                |> shouldEqual row
            | None -> failwith $"row %d{row}: expected an entry point"

    /// CoreCLR asks `IsNilToken`, which looks only at the row, so a MethodDef token with row 0
    /// means "no entry point" just as a zero field does.
    [<Test>]
    let ``A nil MethodDef token means no entry point`` () : unit =
        let image = compile OutputKind.ConsoleApplication |> withEntryPointToken 0x06000000
        (read image).MainMethod |> shouldEqual None
        hostEntryPoint image |> shouldEqual null

    [<Test>]
    let ``A MethodDef row beyond the table is refused`` () : unit =
        let image = compile OutputKind.ConsoleApplication
        let rows = methodDefRowCount image
        let token = 0x06000000 ||| (rows + 1)
        let patched = withEntryPointToken token image

        let exn = Assert.Throws<System.Exception> (fun () -> read patched |> ignore)
        exn.Message |> shouldContainText $"%d{rows + 1}"
        exn.Message |> shouldContainText $"%d{rows}"
        // The host answers "no entry point" here rather than refusing the image; PawPrint refuses
        // because the header names a method that does not exist, which is not something a well
        // formed image does.
        hostEntryPoint patched |> shouldEqual null

    [<Test>]
    let ``A token of another table is refused`` () : unit =
        let image = compile OutputKind.ConsoleApplication
        // TypeDef row 1 exists, so a reader that masked the row out and trusted it would
        // hand back a MethodDef handle for row 1 without complaint.
        let patched = withEntryPointToken 0x02000001 image

        let exn = Assert.Throws<System.Exception> (fun () -> read patched |> ignore)
        exn.Message |> shouldContainText "TypeDefinition"
        exn.Message |> shouldContainText "0x02000001"
        hostEntryPoint patched |> shouldEqual null

    /// ECMA-335 II.25.3.3 allows a File token here, naming the module of a multi-module
    /// assembly that holds the entry point. PawPrint has no multi-module support, so this is
    /// a refusal rather than a wrong answer.
    [<Test>]
    let ``A File token is refused as unsupported`` () : unit =
        let image = compile OutputKind.ConsoleApplication |> withEntryPointToken 0x26000001

        let exn = Assert.Throws<System.Exception> (fun () -> read image |> ignore)
        exn.Message |> shouldContainText "File"
        exn.Message |> shouldContainText "0x26000001"

    /// With `CorFlags.NativeEntryPoint` set the field is an RVA of native code, not a token.
    [<Test>]
    let ``A native entry point is refused`` () : unit =
        let image =
            compile OutputKind.ConsoleApplication |> withCorFlag CorFlags.NativeEntryPoint

        let rva = entryPointToken image

        let exn = Assert.Throws<System.Exception> (fun () -> read image |> ignore)
        exn.Message |> shouldContainText "NativeEntryPoint"
        exn.Message |> shouldContainText $"0x%08x{rva}"
        // `PEDecoder::CheckCorHeader` (utilcode/pedecoder.cpp) rejects an IL-only image that
        // claims a native entry point, so the host refuses these bytes outright.
        Assert.Throws<System.BadImageFormatException> (fun () -> hostEntryPoint image |> ignore)
        |> ignore
