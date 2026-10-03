namespace WoofWare.PawPrint.Test

open System.IO
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open Microsoft.CodeAnalysis
open NUnit.Framework

/// One interface overriding two variance-compatible instantiations of a method with default bodies,
/// called through a third instantiation both cast to: `J : I<Exception>, I<object>` of `I<in T>`,
/// called through `I<ArgumentException>`. CoreCLR's variant search finds `J` and takes the first of
/// its bodies naming a compatible instantiation. For an instance method that is the first in `J`'s
/// method table (`TryGetCandidateImplementation`), which is the order the bodies are declared in,
/// whatever the order of the MethodImpl rows naming them; for a static virtual it is the body of the
/// first MethodImpl row (`MethodTable::TryResolveVirtualStaticMethodOnThisType`).
///
/// C# emits the rows in the order of the bodies, so the image is patched to swap them.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFabricatedDefaultBodyOrder =

    /// The library, with `J` declaring the `I<Exception>` body (returning 1) first if `exceptionFirst`,
    /// and the `I<object>` body (returning 2) first otherwise; `M` is a static virtual if `statics`.
    let private source (statics : bool) (exceptionFirst : bool) : string =
        let modifier = if statics then "static " else ""
        let exceptionBody = $"%s{modifier}int I<Exception>.M() => 1;"
        let objectBody = $"%s{modifier}int I<object>.M() => 2;"

        let first, second =
            if exceptionFirst then
                exceptionBody, objectBody
            else
                objectBody, exceptionBody

        $"""
using System;
public interface I<in T> {{ %s{if statics then "static abstract " else ""}int M(); }}
public interface J : I<Exception>, I<object>
{{
    %s{first}
    %s{second}
}}
public class C : J {{ }}
"""

    /// `image` with its two MethodImpl rows swapped. Both rows belong to `J`, so the table stays
    /// sorted by its class column, as ECMA-335 II.22.27 requires.
    let private swapMethodImplRows (image : byte[]) : byte[] =
        use pe = new PEReader (new MemoryStream (image))
        let reader = pe.GetMetadataReader ()

        let rows =
            List.init
                (reader.GetTableRowCount TableIndex.MethodImpl)
                (fun i -> MetadataTokens.MethodImplementationHandle (i + 1))

        match rows with
        | [ a ; b ] when (reader.GetMethodImplementation a).Type = (reader.GetMethodImplementation b).Type -> ()
        | _ -> failwith $"expected two MethodImpl rows on one type, found %i{rows.Length}"

        let start =
            pe.PEHeaders.MetadataStartOffset
            + reader.GetTableMetadataOffset TableIndex.MethodImpl

        let size = reader.GetTableRowSize TableIndex.MethodImpl
        let patched = Array.copy image
        Array.blit image start patched (start + size) size
        Array.blit image (start + size) patched start size
        patched

    let private instanceDriver : string =
        """
public static class Driver
{
    public static int Main(string[] args) => ((I<System.ArgumentException>) new C()).M();
}
"""

    let private staticDriver : string =
        """
public static class Driver
{
    static int Call<T>() where T : I<System.ArgumentException> => T.M();
    public static int Main(string[] args) => Call<C>();
}
"""

    let private library (statics : bool) (exceptionFirst : bool) (rowsSwapped : bool) : byte[] =
        let image =
            Roslyn.compileAssembly "BodyOrder" OutputKind.DynamicallyLinkedLibrary [] [ source statics exceptionFirst ]

        if rowsSwapped then swapMethodImplRows image else image

    [<TestCase(true, false)>]
    [<TestCase(true, true)>]
    [<TestCase(false, false)>]
    [<TestCase(false, true)>]
    let ``the first body in method-table order runs, whatever order the MethodImpl rows are in``
        (exceptionFirst : bool)
        (rowsSwapped : bool)
        : unit
        =
        FabricatedGuest.run
            "BodyOrder"
            (library false exceptionFirst rowsSwapped)
            "BodyOrderDriver"
            instanceDriver
            (if exceptionFirst then 1 else 2)

    [<TestCase(true, false)>]
    [<TestCase(true, true)>]
    [<TestCase(false, false)>]
    [<TestCase(false, true)>]
    let ``for a static virtual, the body of the first MethodImpl row runs``
        (exceptionFirst : bool)
        (rowsSwapped : bool)
        : unit
        =
        FabricatedGuest.run
            "BodyOrder"
            (library true exceptionFirst rowsSwapped)
            "BodyOrderDriver"
            staticDriver
            (if exceptionFirst <> rowsSwapped then 1 else 2)
