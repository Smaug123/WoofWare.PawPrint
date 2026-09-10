namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open System.Reflection.Metadata.Ecma335
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// `IlMachineState._MemberResolutions` claims that a hit answers what a miss would have. Nothing
/// downstream can tell a wrong entry from a right one, so this checks the claim directly: run a
/// guest that reads MemberRef rows under several generic contexts, then resolve every key the
/// run memoised from scratch against the final state and compare.
[<TestFixture>]
module TestMemberResolutionMemo =

    let private assy = System.Reflection.Assembly.GetExecutingAssembly ()

    /// Generic contexts of every kind the key distinguishes: a generic type's own members read
    /// from its methods (`Pair<T>` from `Ops<T>`), the same rows under a second instantiation,
    /// a generic *method*'s context (`First<T>`), and CoreLib's own generic collections, whose
    /// bodies read `Entry<TKey, TValue>` and `T[]` rows under the caller's instantiation.
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

static class Ops<T>
{
    public static Pair<T> Make(T a, T b) => new Pair<T>(a, b);
    public static T Pick(Pair<T> p, bool first) => first ? p.First : p.Second;
}

static class Program
{
    static T First<T>(List<T> items) => items[0];

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
        int total = d["a"] + d["b"] + l[1].Item1 + Ops<int>.Pick(p, false) + q.First.Length + First(new List<int> { 5 });
        return total == 15 ? 0 : 1;
    }
}
"""

    let private runGuest () : IlMachineState * ThreadId =
        let image = Roslyn.compile [ source ]

        let messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", "MemberResolutionMemo.cs" ]

        use _loggerFactoryResource = loggerFactory

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        try
            match
                Program.run loggerFactory (Some "MemberResolutionMemo.cs") peImage (HostConfig.Default dotnetRuntimes)
            with
            | RunOutcome.NormalExit (state, thread)
            | RunOutcome.ProcessExit (state, thread) ->
                state.LatchedExitCode |> shouldEqual 0
                state, thread
            | other -> failwith $"guest did not exit normally: %O{other}"
        with _ ->
            for message in messages () do
                System.Console.Error.WriteLine $"{message}"

            reraise ()

    [<Test>]
    let ``Every memoised member resolution agrees with resolving the row afresh`` () =
        let state, thread = runGuest ()
        // The corelib instance the run resolved against, so that a fresh resolution reads the
        // same metadata objects the memoised one did.
        let baseClassTypes =
            state._LoadedAssemblies.DefinitionNames
            |> Seq.find (fun name -> name.StartsWith ("System.Private.CoreLib,", System.StringComparison.Ordinal))
            |> state._LoadedAssemblies.ByDefinitionName
            |> Corelib.getBaseTypes

        let entries = state._MemberResolutions |> Map.toList

        // The guest is chosen to reach every shape of key; a run that memoised only
        // context-free rows would pass the comparison below without testing the claim.
        let withTypeGenerics =
            entries |> List.filter (fun (key, _) -> not key.DeclaringTypeGenerics.IsEmpty)

        let withMethodGenerics =
            entries |> List.filter (fun (key, _) -> not key.MethodGenerics.IsEmpty)

        let fields =
            entries
            |> List.filter (fun (_, resolved) ->
                match resolved.Member with
                | Choice2Of2 _ -> true
                | Choice1Of2 _ -> false
            )

        if
            entries.Length < 50
            || withTypeGenerics.Length < 20
            || withMethodGenerics.Length < 1
            || fields.Length < 10
        then
            failwith
                $"the guest memoised %d{entries.Length} rows, %d{withTypeGenerics.Length} under type generics, %d{withMethodGenerics.Length} under method generics, %d{fields.Length} fields; it is not exercising the key"

        for key, memoised in entries do
            let assy = state._LoadedAssemblies.ByDefinitionName key.Assembly

            let toTypeDefn (handle : ConcreteTypeHandle) : TypeDefn =
                Concretization.concreteHandleToTypeDefn
                    baseClassTypes
                    handle
                    state.ConcreteTypes
                    state._LoadedAssemblies

            let typeGenerics =
                key.DeclaringTypeGenerics |> Seq.map toTypeDefn |> ImmutableArray.CreateRange

            let methodGenerics =
                key.MethodGenerics |> Seq.map toTypeDefn |> ImmutableArray.CreateRange

            let afterState, declaringAssembly, fresh, targetTypeGenerics =
                IlMachineMemberResolution.resolveMemberWithGenerics
                    state.LoggerFactory
                    baseClassTypes
                    thread
                    assy
                    typeGenerics
                    methodGenerics
                    (MetadataTokens.MemberReferenceHandle key.MemberRow)
                    state

            declaringAssembly.FullName |> shouldEqual memoised.DeclaringAssembly.FullName

            // `MethodInfo` carries no structural equality, so a method is compared on what
            // identifies it: its owner (identity plus instantiation), its definition row, its
            // name, and its signature under the row's instantiation.
            match fresh, memoised.Member with
            | Choice1Of2 fresh, Choice1Of2 memoised ->
                fresh.Owner |> shouldEqual memoised.Owner
                fresh.IdentityKey |> shouldEqual memoised.IdentityKey
                fresh.Name |> shouldEqual memoised.Name
                fresh.Signature |> shouldEqual memoised.Signature
                fresh.Generics.Length |> shouldEqual memoised.Generics.Length
            | Choice2Of2 fresh, Choice2Of2 memoised -> fresh |> shouldEqual memoised
            | Choice1Of2 _, Choice2Of2 _ -> failwith $"row %d{key.MemberRow} memoised a field but resolves to a method"
            | Choice2Of2 _, Choice1Of2 _ -> failwith $"row %d{key.MemberRow} memoised a method but resolves to a field"

            targetTypeGenerics |> shouldEqual memoised.TargetTypeGenerics

            // A miss's side effects on the state persist, which is what lets a hit skip them:
            // so resolving again against the final state must load and register nothing new.
            afterState._LoadedAssemblies.DefinitionNames
            |> List.ofSeq
            |> shouldEqual (List.ofSeq state._LoadedAssemblies.DefinitionNames)

            afterState._MemberResolutions.Count
            |> shouldEqual state._MemberResolutions.Count
