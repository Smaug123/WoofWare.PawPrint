namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// `typeof(X).IsValueType` is folded at its `ldtoken` (`TypeofIntrinsicFold`), so for that shape
/// neither `Type.get_IsValueType` nor the `RuntimeType.IsValueTypeImpl` chain under it ever runs.
/// The differential guest `sourcesPure/TypeofIsValueTypeFold.cs` checks the *answers*; this checks
/// that the fold fired, which no answer can show, by asking the method-concretisation memo which
/// methods were entered. The control guest asks the same questions through a `Type` local, the
/// shape the fold must leave alone, and shows the memo does record the chain when it runs.
[<TestFixture>]
module TestTypeofIsValueTypeFold =

    let private assy = System.Reflection.Assembly.GetExecutingAssembly ()

    /// `__PROBE__` is the body of a `bool`-returning method with a type parameter `T`.
    let private template =
        """
using System;

static class Probe<T>
{
    public static bool Ask()
    {
        __PROBE__
    }
}

static class Program
{
    static bool AskMethod<T>()
    {
        __PROBE__
    }

    static int Main()
    {
        int failures = 0;
        if (!Probe<int>.Ask()) failures++;
        if (Probe<string>.Ask()) failures++;
        if (!AskMethod<double>()) failures++;
        if (AskMethod<object>()) failures++;
        return failures;
    }
}
"""

    let private folded = "return typeof(T).IsValueType;"

    let private throughLocal = "Type t = typeof(T); return t.IsValueType;"

    let private runGuest (probe : string) : IlMachineState =
        let source = template.Replace ("__PROBE__", probe)
        let image = Roslyn.compile [ source ]

        let messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", "TypeofIsValueTypeFold.cs" ]

        use _loggerFactoryResource = loggerFactory

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        try
            match
                Program.run loggerFactory (Some "TypeofIsValueTypeFold.cs") peImage (HostConfig.Default dotnetRuntimes)
            with
            | RunOutcome.NormalExit (state, _)
            | RunOutcome.ProcessExit (state, _) ->
                state.LatchedExitCode |> shouldEqual 0
                state
            | other -> failwith $"guest did not exit normally: %O{other}"
        with _ ->
            for message in messages () do
                System.Console.Error.WriteLine $"{message}"

            reraise ()

    /// Every method the run entered is concretised on the way in, so the memo's values are the
    /// set of methods that ran at least once.
    let private enteredMethodNamed (name : string) (state : IlMachineState) : bool =
        state._ConcretisedMethods
        |> Map.exists (fun _ concretised -> concretised.Method.Name = name)

    [<Test>]
    let ``typeof(T).IsValueType never enters the getter or IsValueTypeImpl`` () =
        let state = runGuest folded
        enteredMethodNamed "get_IsValueType" state |> shouldEqual false
        enteredMethodNamed "IsValueTypeImpl" state |> shouldEqual false

    [<Test>]
    let ``IsValueType through a Type local enters the getter and IsValueTypeImpl`` () =
        let state = runGuest throughLocal
        enteredMethodNamed "get_IsValueType" state |> shouldEqual true
        enteredMethodNamed "IsValueTypeImpl" state |> shouldEqual true
