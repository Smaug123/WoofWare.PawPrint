namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open System.Reflection.Metadata.Ecma335
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// `IlMachineState._ConcretisedMethods` and `_ZeroValues` each claim that a hit answers what a
/// miss would have. Nothing downstream can tell a wrong entry from a right one, so this checks
/// the claims directly: run a guest that calls methods under several instantiations and
/// allocates value types of several shapes, then recompute every memoised entry from scratch
/// against the final state and compare.
[<TestFixture>]
module TestFrameSetupMemo =

    let private assy = System.Reflection.Assembly.GetExecutingAssembly ()

    /// Methods concretised under type generics (`Ops<T>` twice over), under method generics
    /// (`Swap<T>`), synthesised ones (the multi-dimensional array's accessors), and value-type
    /// zeros of several shapes: a generic struct at two instantiations, a struct nesting another,
    /// and CoreLib's own (`ValueTuple`, `KeyValuePair`, `Dictionary` entries).
    let private source =
        """
using System;
using System.Collections.Generic;

struct Pair<T>
{
    public T First;
    public T Second;
    public Pair(T a, T b) { First = a; Second = b; }
}

struct Nested
{
    public Pair<int> Ints;
    public Pair<string> Strings;
    public long Tail;
}

static class Ops<T>
{
    public static Pair<T> Make(T a, T b) => new Pair<T>(a, b);
    public static T Pick(Pair<T> p, bool first) => first ? p.First : p.Second;
}

static class Program
{
    static void Swap<T>(ref T a, ref T b) { T t = a; a = b; b = t; }

    static int Main()
    {
        var d = new Dictionary<string, int>();
        d["a"] = 1;
        d["b"] = 2;
        var l = new List<(int, string)>();
        l.Add((1, "x"));
        l.Add((2, "y"));
        var p = Ops<int>.Make(3, 4);
        var q = Ops<string>.Make("s", "t");
        var n = new Nested();
        n.Ints = p;
        n.Strings = q;
        var grid = new int[2, 3];
        grid[1, 2] = 7;
        int x = 1, y = 2;
        Swap(ref x, ref y);
        var arr = new Nested[3];
        arr[1].Tail = 5;
        int total = d["a"] + d["b"] + l[1].Item1 + Ops<int>.Pick(p, false) + q.First.Length + grid[1, 2] + y + (int)arr[1].Tail + n.Ints.Second;
        return total == 27 ? 0 : 1;
    }
}
"""

    let private runGuest () : IlMachineState =
        let image = Roslyn.compile [ source ]

        let messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", "FrameSetupMemo.cs" ]

        use _loggerFactoryResource = loggerFactory

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        try
            match Program.run loggerFactory (Some "FrameSetupMemo.cs") peImage (HostConfig.Default dotnetRuntimes) with
            | RunOutcome.NormalExit (state, _)
            | RunOutcome.ProcessExit (state, _) ->
                state.LatchedExitCode |> shouldEqual 0
                state
            | other -> failwith $"guest did not exit normally: %O{other}"
        with _ ->
            for message in messages () do
                System.Console.Error.WriteLine $"{message}"

            reraise ()

    /// The corelib instance the run resolved against, so that a fresh computation reads the
    /// same metadata objects the memoised one did.
    let private baseClassTypesOf (state : IlMachineState) : BaseClassTypes<DumpedAssembly> =
        state._LoadedAssemblies.DefinitionNames
        |> Seq.find (fun name -> name.StartsWith ("System.Private.CoreLib,", System.StringComparison.Ordinal))
        |> state._LoadedAssemblies.ByDefinitionName
        |> Corelib.getBaseTypes

    [<Test>]
    let ``Every memoised method concretisation agrees with concretising the definition afresh`` () =
        let state = runGuest ()
        let baseClassTypes = baseClassTypesOf state
        let entries = state._ConcretisedMethods |> Map.toList

        // The guest is chosen to reach every shape of key; a run that memoised only
        // non-generic metadata methods would pass the comparison below without testing the claim.
        let withTypeGenerics =
            entries |> List.filter (fun (key, _) -> not key.TypeGenerics.IsEmpty)

        let withMethodGenerics =
            entries |> List.filter (fun (key, _) -> not key.MethodGenerics.IsEmpty)

        let synthesised = entries |> List.filter (fun (key, _) -> key.Synthesised.IsSome)

        if
            entries.Length < 100
            || withTypeGenerics.Length < 20
            || withMethodGenerics.Length < 2
            || synthesised.Length < 1
        then
            failwith
                $"the guest memoised %d{entries.Length} methods, %d{withTypeGenerics.Length} under type generics, %d{withMethodGenerics.Length} under method generics, %d{synthesised.Length} synthesised; it is not exercising the key"

        for key, memoised in entries do
            match key.MethodRow with
            | None ->
                // A synthesised method is minted by the interpreter for a shape it recognises,
                // so the memo can be compared only against the concretisation it stored, and
                // what is checked here is that the entry describes the key it is filed under.
                memoised.Method.IdentityKey |> snd |> shouldEqual key.Synthesised

                memoised.Method.DeclaringTypeGenerics
                |> List.ofSeq
                |> shouldEqual key.TypeGenerics
            | Some row ->
                let assy =
                    state._LoadedAssemblies.ByDefinitionName key.DeclaringType.AssemblyFullName

                let definition =
                    assy.Methods.[MetadataTokens.MethodDefinitionHandle row]
                    |> MethodInfo.mapTypeGenerics (fun (par, _) -> TypeDefn.GenericTypeParameter par.SequenceNumber)

                let fresh, concreteTypes, assemblies =
                    Concretization.concretizeMethod
                        state.ConcreteTypes
                        (IlMachineState.loader state.LoggerFactory state)
                        state._LoadedAssemblies
                        baseClassTypes
                        definition
                        (ImmutableArray.CreateRange key.TypeGenerics)
                        (ImmutableArray.CreateRange key.MethodGenerics)

                fresh.Owner |> shouldEqual memoised.Method.Owner
                fresh.IdentityKey |> shouldEqual memoised.Method.IdentityKey
                fresh.Name |> shouldEqual memoised.Method.Name
                fresh.Signature |> shouldEqual memoised.Method.Signature
                fresh.Generics |> shouldEqual memoised.Method.Generics
                fresh.DeclaringTypeGenerics |> shouldEqual memoised.Method.DeclaringTypeGenerics
                fresh.IsStatic |> shouldEqual memoised.Method.IsStatic

                let localsOf (m : MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>) =
                    MethodInfo.tryIlBody m |> Option.bind (fun body -> body.LocalVars)

                localsOf fresh |> shouldEqual (localsOf memoised.Method)

                AllConcreteTypes.findExistingConcreteType
                    concreteTypes
                    fresh.RequiredDeclaringType.Identity
                    fresh.DeclaringTypeGenerics
                |> shouldEqual (Some memoised.DeclaringTypeHandle)

                // A miss's side effects on the state persist, which is what lets a hit skip
                // them: so concretising again against the final state loads and registers
                // nothing new.
                List.ofSeq assemblies.DefinitionNames
                |> shouldEqual (List.ofSeq state._LoadedAssemblies.DefinitionNames)

                // The registry is compared whole: the fresh walk must mint no handle.
                (concreteTypes = state.ConcreteTypes) |> shouldEqual true

    [<Test>]
    let ``Every memoised zero value agrees with building it afresh`` () =
        let state = runGuest ()
        let baseClassTypes = baseClassTypesOf state
        let entries = state._ZeroValues |> Map.toList

        let valueTypes =
            entries
            |> List.filter (fun (_, zero) ->
                match zero with
                | CliType.ValueType _ -> true
                | _ -> false
            )

        if entries.Length < 50 || valueTypes.Length < 10 then
            failwith
                $"the guest memoised %d{entries.Length} zeros, %d{valueTypes.Length} of value types; it is not exercising the memo"

        for handle, memoised in entries do
            let fresh, concreteTypes, assemblies =
                CliType.zeroOf
                    (IlMachineState.loader state.LoggerFactory state)
                    state.ConcreteTypes
                    state._LoadedAssemblies
                    baseClassTypes
                    handle

            fresh |> shouldEqual memoised

            List.ofSeq assemblies.DefinitionNames
            |> shouldEqual (List.ofSeq state._LoadedAssemblies.DefinitionNames)

            (concreteTypes = state.ConcreteTypes) |> shouldEqual true
