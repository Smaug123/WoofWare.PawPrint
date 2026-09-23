namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// The `[UnsafeAccessor]` shapes whose answer on real .NET PawPrint does not model, and so must
/// refuse rather than approximate. A refusal is a host failure, not a guest exception, so no
/// `sourcesPure` guest can observe it; these pin that each shape reaches its own refusal rather
/// than some neighbouring answer. The shapes on the other side of each boundary, which PawPrint
/// does answer, are `sourcesPure/UnsafeAccessorArrayMemberLookup.cs` and
/// `sourcesPure/UnsafeAccessorSharedGenericTarget.cs`.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestUnsafeAccessorRefusals =

    let private assy = typeof<RunResult>.Assembly

    let private runToFailure (name : string) (source : string) : exn =
        let image = Roslyn.compileWithSymbols [ source ]

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", name ]

        use _loggerFactoryResource = loggerFactory

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        Assert.Catch (fun () ->
            BoundedRun.runWith
                loggerFactory
                BoundedRun.defaultMaxSteps
                name
                (Some name)
                peImage
                (HostConfig.Default dotnetRuntimes)
            |> ignore<RunOutcome>
        )

    /// A guest declaring `declaration` inside a class that also declares a generic struct `GS<T>`,
    /// whose `Main` runs `call`.
    let private guest (declaration : string) (call : string) : string =
        $"""
using System.Collections.Generic;
using System.Runtime.CompilerServices;

class Guest
{{
    struct GS<T> {{ }}

    %s{declaration}

    static int Main()
    {{
        %s{call};
        return 0;
    }}
}}
"""

    /// An array target whose type mentions a method type parameter instantiated with something
    /// other than its own canonical form. CoreCLR searches the array over the canonical
    /// instantiation and names it -- measured on real .NET 10 as `System.__Canon[]` for the first
    /// and third, and `GS`1[System.__Canon][]` for the second -- and PawPrint has no canonical
    /// forms to name.
    let sharedArrayCases : TestCaseData list =
        let fieldOfArray =
            """[UnsafeAccessor(UnsafeAccessorKind.Field, Name = "x")] static extern ref int F<T>(T[] a);"""

        let fieldOfListArray =
            """[UnsafeAccessor(UnsafeAccessorKind.Field, Name = "x")] static extern ref int F<T>(List<T>[] a);"""

        [
            "a reference-type element", fieldOfArray, "F<string>(new string[1])"
            "a value-type element over a reference type", fieldOfArray, "F<GS<string>>(new GS<string>[1])"
            "a reference type inside the element", fieldOfListArray, "F<string>(new List<string>[1])"
        ]
        |> List.map (fun (label, declaration, call) ->
            (TestCaseData [| box declaration ; box call |]).SetName $"shared array: %s{label}"
        )

    [<TestCaseSource(nameof sharedArrayCases)>]
    let ``an array target over a shared instantiation is refused`` (declaration : string, call : string) : unit =
        let exn = runToFailure "SharedArray.cs" (guest declaration call)

        exn.Message
        |> shouldContainText "names an array whose type mentions method type parameter 0"

    /// Measured on real .NET 10, the instance-method kind over an array binds the array's
    /// constructor when the signature matches one (`int[,]` with two `int`s), and the JIT then
    /// refuses the stub; with a signature matching none (`int[]` with a `string`), it reports
    /// `.ctor` missing. PawPrint does not model which constructors an array has, so it refuses
    /// both.
    [<TestCase("int i, int j")>]
    [<TestCase("string s")>]
    let ``an array's constructor through the instance-method kind is refused`` (parameters : string) : unit =
        let arguments = if parameters = "string s" then "\"s\"" else "1, 1"

        let source =
            guest
                $"""[UnsafeAccessor(UnsafeAccessorKind.Method, Name = ".ctor")] static extern void Ctor(int[,] a, %s{parameters});"""
                $"Ctor(new int[1, 1], %s{arguments})"

        let exn = runToFailure "ArrayCtorAsMethod.cs" source

        exn.Message
        |> shouldContainText "names an array's .ctor through the instance-method kind"
