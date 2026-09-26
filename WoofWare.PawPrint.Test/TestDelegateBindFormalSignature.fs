namespace WoofWare.PawPrint.Test

open System
open System.IO
open NUnit.Framework
open WoofWare.PawPrint

/// `CreateDelegate` over methods of open generic definitions, every method against every delegate
/// type and every bound target, held to real .NET: the rules of `IsLocationAssignable`
/// (comdelegate.cpp) for a type naming a definition's own variables, under each kind of
/// constraint, and the binding `BindToMethod` makes when such a comparison passes.
///
/// Real .NET's outcome for each combination is computed in this process, over the corpus loaded
/// as a library -- creating a delegate touches no process-global, and none of these is invoked --
/// and baked into a guest that returns 0 if PawPrint agrees on every combination and otherwise one
/// more than the index of the first it disagrees on. The same image also runs under real .NET, out
/// of process, where it must return 0: that is what shows the table describes the guest's types.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
// Runs a guest under the interpreter; see AGENTS.md for why such fixtures are `Explicit`.
[<Category("Guest")>]
[<Explicit>]
module TestDelegateBindFormalSignature =

    let private corpusNamespace : string = "PawPrint.DelegateFormal"

    let private corpusSource : string =
        """
using System;

namespace PawPrint.DelegateFormal;

public class Base { }
public class Derived : Base { }
public interface IFace { }

public class G<T>
{
    public enum E { A }
    public static string TakesT(T x) { return ""; }
    public static T ReturnsT() { return default(T); }
    public static string TakesTInt(T x, int n) { return ""; }
    public static string TakesE(E e) { return ""; }
    public static E ReturnsE() { return default(E); }
    public static G<T> ReturnsSelf() { return null; }
    public static string TakesSelf(G<T> g) { return ""; }
    public string Instance(int n) { return ""; }
}

public class GC<T> where T : class
{
    public static string TakesT(T x) { return ""; }
    public static T ReturnsT() { return null; }
    public static string TakesTInt(T x, int n) { return ""; }
}

public class GS<T> where T : struct
{
    public static string TakesT(T x) { return ""; }
    public static T ReturnsT() { return default(T); }
}

public class GB<T> where T : Base
{
    public static string TakesT(T x) { return ""; }
    public static T ReturnsT() { return null; }
    public static string TakesTInt(T x, int n) { return ""; }
}

public class GI<T> where T : IFace
{
    public static T ReturnsT() { return default(T); }
    public static string TakesTInt(T x, int n) { return ""; }
}

// `T : U, U : class` does not make `T` an object reference: the `class` flag does not propagate
// through a variable-to-variable constraint (`ConstrainedAsObjRefHelper`).
public class GU<T, U> where T : U where U : class
{
    public static T ReturnsT() { return default(T); }
    public static U ReturnsU() { return null; }
}

public interface IContra<in T> where T : class
{
    string M();
}

public class ContraImpl : IContra<object>
{
    public string M() { return "impl"; }
}
"""

    /// A method as the guest names it (`typeof(G<>)`) and as the host's reflection does (`G`1`).
    let private methods : (string * string * string) list =
        [
            for name in
                [
                    "TakesT"
                    "ReturnsT"
                    "TakesTInt"
                    "TakesE"
                    "ReturnsE"
                    "ReturnsSelf"
                    "TakesSelf"
                    "Instance"
                ] do
                yield "G<>", "G`1", name
            for name in [ "TakesT" ; "ReturnsT" ; "TakesTInt" ] do
                yield "GC<>", "GC`1", name
            for name in [ "TakesT" ; "ReturnsT" ] do
                yield "GS<>", "GS`1", name
            for name in [ "TakesT" ; "ReturnsT" ; "TakesTInt" ] do
                yield "GB<>", "GB`1", name
            for name in [ "ReturnsT" ; "TakesTInt" ] do
                yield "GI<>", "GI`1", name
            for name in [ "ReturnsT" ; "ReturnsU" ] do
                yield "GU<,>", "GU`2", name
            yield "IContra<>", "IContra`1", "M"
        ]

    /// A delegate type as C# spells it, and as the host builds it over the loaded corpus.
    let private delegateTypes : (string * (System.Reflection.Assembly -> Type)) list =
        let corpusType (name : string) (assembly : System.Reflection.Assembly) : Type =
            assembly.GetType $"%s{corpusNamespace}.%s{name}"
            |> Option.ofObj
            |> Option.defaultWith (fun () -> failwith $"corpus does not contain %s{name}")

        let fixedType (t : Type) (_ : System.Reflection.Assembly) : Type = t

        [
            "Func<object>", fixedType typeof<Func<obj>>
            "Func<string>", fixedType typeof<Func<string>>
            "Func<int>", fixedType typeof<Func<int>>
            "Func<ValueType>", fixedType typeof<Func<ValueType>>
            "Func<Base>", (fun a -> typedefof<Func<obj>>.MakeGenericType (corpusType "Base" a))
            "Func<IFace>", (fun a -> typedefof<Func<obj>>.MakeGenericType (corpusType "IFace" a))
            "Func<int, string>", fixedType typeof<Func<int, string>>
            "Func<long, string>", fixedType typeof<Func<int64, string>>
            "Func<object, string>", fixedType typeof<Func<obj, string>>
            "Func<string, string>", fixedType typeof<Func<string, string>>
            "Func<Derived, string>",
            (fun a -> typedefof<Func<obj, obj>>.MakeGenericType (corpusType "Derived" a, typeof<string>))
            "Func<Derived, int, string>",
            (fun a ->
                typedefof<Func<obj, obj, obj>>.MakeGenericType (corpusType "Derived" a, typeof<int>, typeof<string>)
            )
            "Func<string, int, string>", fixedType typeof<Func<string, int, string>>
        ]

    /// What `CreateDelegate` is handed besides the delegate type: nothing, which asks for an open
    /// delegate, or an object to close over, as C# spells it and as the host builds it.
    let private targets : (string * (System.Reflection.Assembly -> obj) option) list =
        let construct (name : string) (assembly : System.Reflection.Assembly) : obj =
            Activator.CreateInstance (assembly.GetType $"%s{corpusNamespace}.%s{name}")

        [
            "open", None
            "null", Some (fun _ -> null)
            "\"x\"", Some (fun _ -> box "x")
            "new Derived()", Some (construct "Derived")
            "new ContraImpl()", Some (construct "ContraImpl")
        ]

    let private corpusBytes : byte array =
        Roslyn.compileAssembly
            corpusNamespace
            Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
            []
            [ corpusSource ]

    let private hostCorpus : System.Reflection.Assembly =
        System.Reflection.Assembly.Load corpusBytes

    /// The outcome as the guest reports it: `bound`, or the exception's type name.
    let private hostOutcome
        (hostTypeName : string)
        (methodName : string)
        (delegateType : Type)
        (target : obj option)
        : string
        =
        let declaring = hostCorpus.GetType $"%s{corpusNamespace}.%s{hostTypeName}"
        let method = declaring.GetMethod methodName

        try
            match target with
            | None -> method.CreateDelegate delegateType |> ignore
            | Some target -> method.CreateDelegate (delegateType, target) |> ignore

            "bound"
        with e ->
            e.GetType().Name

    let private cases : (string * string * string * string * string) list =
        [
            for guestType, hostType, methodName in methods do
                for delegateSpelling, delegateType in delegateTypes do
                    for targetSpelling, target in targets do
                        let outcome =
                            hostOutcome
                                hostType
                                methodName
                                (delegateType hostCorpus)
                                (target |> Option.map (fun t -> t hostCorpus))

                        yield guestType, methodName, delegateSpelling, targetSpelling, outcome
        ]

    let private guestSource : string =
        let checks =
            cases
            |> List.mapi (fun i (guestType, methodName, delegateSpelling, targetSpelling, outcome) ->
                let create =
                    if targetSpelling = "open" then
                        $"typeof(%s{guestType}).GetMethod(\"%s{methodName}\").CreateDelegate(typeof(%s{delegateSpelling}))"
                    else
                        $"typeof(%s{guestType}).GetMethod(\"%s{methodName}\").CreateDelegate(typeof(%s{delegateSpelling}), %s{targetSpelling})"

                $"        if (Outcome(() => %s{create}) != \"%s{outcome}\") return %d{i + 1};"
            )
            |> String.concat "\n"

        $"""
using System;

namespace %s{corpusNamespace};

public static class FormalSignatureSweep
{{
    static string Outcome(Func<Delegate> create)
    {{
        try
        {{
            create();
            return "bound";
        }}
        catch (Exception e)
        {{
            return e.GetType().Name;
        }}
    }}

    public static int Main(string[] args)
    {{
%s{checks}
        return 0;
    }}
}}
"""

    let private describe (exitCode : int) : string =
        if exitCode = 0 then
            "agreed on every combination"
        elif exitCode < 1 || exitCode > cases.Length then
            $"returned %d{exitCode}, which names no combination"
        else
            let guestType, methodName, delegateSpelling, targetSpelling, outcome =
                cases.[exitCode - 1]

            $"disagreed first on typeof(%s{guestType}).GetMethod(\"%s{methodName}\").CreateDelegate(typeof(%s{delegateSpelling})) with target %s{targetSpelling}, where real .NET's outcome is %s{outcome}"

    [<Test>]
    let ``the sweep reaches every outcome`` () : unit =
        // Vacuity guard: a corpus whose every combination failed the same way would pass the
        // comparison below while testing nothing about the rules it names.
        let count (outcome : string) : int =
            cases |> List.filter (fun (_, _, _, _, o) -> o = outcome) |> List.length

        Assert.That (count "bound", Is.GreaterThan 0)
        Assert.That (count "ArgumentException", Is.GreaterThan 100)
        Assert.That (count "InvalidOperationException", Is.GreaterThan 20)

        let outcomes = cases |> List.map (fun (_, _, _, _, o) -> o) |> Set.ofList

        Assert.That (outcomes, Is.EquivalentTo [ "bound" ; "ArgumentException" ; "InvalidOperationException" ])

    [<Test>]
    let ``CreateDelegate over open generic definitions agrees with real .NET`` () : unit =
        let image =
            Roslyn.compileAssembly
                "PawPrintTestAssembly"
                Microsoft.CodeAnalysis.OutputKind.ConsoleApplication
                []
                [ corpusSource ; guestSource ]

        let sourceName = "DelegateBindFormalSignatureSweep"

        let messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", sourceName ]

        use _loggerFactoryResource = loggerFactory

        let realResult, pawPrintExitCode =
            DifferentialOracle.alongsideInterpreted
                (fun () -> RealRuntime.executeWithRealRuntime [||] image)
                (fun () ->
                    use peImage = new MemoryStream (image)

                    let outcome =
                        try
                            Program.run
                                loggerFactory
                                (Some sourceName)
                                peImage
                                (HostConfig.Default (FrameworkUnderTest.runtimeDirs ()))
                        with _ ->
                            for message in messages () do
                                Console.Error.WriteLine $"{message}"

                            reraise ()

                    match outcome with
                    | RunOutcome.NormalExit (terminalState, _)
                    | RunOutcome.ProcessExit (terminalState, _) -> terminalState.LatchedExitCode
                    | RunOutcome.GuestUnhandledException (_, _, exn) ->
                        failwith $"%s{sourceName}: guest threw an unhandled exception: %O{exn.ExceptionObject}"
                    | RunOutcome.Aborted (_, _, fatal) -> failwith $"%s{sourceName}: guest aborted: %O{fatal}"
                    | RunOutcome.SignalTerminated (_, signal) ->
                        failwith $"%s{sourceName}: guest was signalled: %O{signal}"
                )

        match realResult with
        | RealRuntimeResult.NormalExit 0 -> ()
        | other ->
            failwith
                $"real .NET did not agree with its own in-process answers (%O{other}); the baked-in table is not describing the guest's types"

        if pawPrintExitCode <> 0 then
            failwith $"PawPrint %s{describe pawPrintExitCode}"
