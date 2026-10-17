namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.PawPrint

/// `Debuggable.allowsJitOptimisation`, which reads an assembly's `DebuggableAttribute` as CoreCLR's
/// `Assembly::GetDebuggingCustomAttributes` does.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestDebuggable =

    let private source : string =
        """
namespace Probe;

public static class Empty { }
"""

    let private allows (image : byte[]) : bool option =
        let _, loggerFactory = LoggerFactory.makeTest ()

        Assembly.read loggerFactory (Some "Probe.dll") (new MemoryStream (image))
        |> Debuggable.allowsJitOptimisation

    [<Test>]
    let ``a Debug build disallows optimisation and a Release build allows it`` () : unit =
        Roslyn.compileAssembly "Probe" OutputKind.DynamicallyLinkedLibrary [] [ source ]
        |> allows
        |> shouldEqual (Some false)

        Roslyn.compileOptimizedAssembly "Probe" OutputKind.DynamicallyLinkedLibrary [ source ]
        |> allows
        |> shouldEqual (Some true)

    /// Blobs, and what CoreCLR makes of each.
    let blobs : TestCaseData list =
        [
            // `DebuggingModes.Default | DisableOptimizations | IgnoreSymbolStoreSequencePoints |
            // EnableEditAndContinue`, as C# emits for a Debug build.
            [ 1 ; 0 ; 0x07 ; 0x01 ; 0 ; 0 ; 0 ; 0 ], Some false
            // `DebuggingModes.IgnoreSymbolStoreSequencePoints`, as C# emits for a Release build.
            [ 1 ; 0 ; 0x02 ; 0 ; 0 ; 0 ; 0 ; 0 ], Some true
            // `DisableOptimizations` without `Default`, which CoreCLR allows optimisation under for
            // compatibility.
            [ 1 ; 0 ; 0 ; 0x01 ; 0 ; 0 ; 0 ; 0 ], Some true
            // The `(bool isJITTrackingEnabled, bool isJITOptimizerDisabled)` constructor.
            [ 1 ; 0 ; 1 ; 1 ; 0 ; 0 ], Some false
            [ 1 ; 0 ; 1 ; 0 ; 0 ; 0 ], Some true
            [ 1 ; 0 ; 0 ; 1 ; 0 ; 0 ], Some true
            // Any other size is not read.
            [ 1 ; 0 ; 1 ; 1 ; 0 ; 0 ; 0 ], Some true
            [], Some true
            // A bad prolog fails the load.
            [ 0 ; 0 ; 1 ; 1 ; 0 ; 0 ], None
            [ 1 ; 1 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ], None
        ]
        |> List.map (fun (blob, expected) ->
            TestCaseData(blob |> List.map byte |> Array.ofList, expected)
                .SetArgDisplayNames (sprintf "%A" blob, sprintf "%A" expected)
        )

    [<TestCaseSource(nameof blobs)>]
    let ``a blob is read as CoreCLR reads it`` (blob : byte[]) (expected : bool option) : unit =
        Debuggable.blobAllowsOptimisation (ImmutableArray.Create<byte> blob)
        |> shouldEqual expected
