namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// A method whose row is managed IL with no body (`MethodWithoutIlImage`), reached by a call, an
/// interface dispatch to a default body, a constrained call of a static virtual default body (on a
/// class, on the interface itself, and on a type that does not implement it), a call from a `DynamicMethod`, a `calli` through its
/// address, and a delegate.
///
/// Real .NET loads the types, binds each call and dispatches to the method as to any
/// implementation, and raises `BadImageFormatException` only when the method is invoked. PawPrint
/// reads the image, binds and dispatches likewise, and then refuses the invocation with a TODO,
/// because it does not yet raise that exception there. Each driver catches the exception, so
/// real .NET exits 0.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFabricatedMethodWithoutIl =

    let private driver (body : string) : string =
        $$"""
using System;
using System.Runtime.CompilerServices;

public static class Program
{
    {{body}}
}
"""

    /// Each driver reaches the method its own way, in a method of its own so that real .NET
    /// compiling it could not escape `Main`'s handler either.
    let private cases : (string * string * string) list =
        [
            "Call",
            "NoBody",
            """
    [MethodImpl(MethodImplOptions.NoInlining)]
    static void Reach() => Holder.NoBody();

    public static int Main()
    {
        try { Reach(); return 1; }
        catch (BadImageFormatException) { return 0; }
    }
"""
            "DefaultInterfaceBody",
            "DefaultRun",
            """
    [MethodImpl(MethodImplOptions.NoInlining)]
    static void Reach(IThing thing) => thing.DefaultRun();

    public static int Main()
    {
        IThing thing = new Thing();
        try { Reach(thing); return 1; }
        catch (BadImageFormatException) { return 0; }
    }
"""
            "StaticVirtualDefault",
            "StaticRun",
            """
    [MethodImpl(MethodImplOptions.NoInlining)]
    static void Reach<T>() where T : IThing => T.StaticRun();

    public static int Main()
    {
        try { Reach<Thing>(); return 1; }
        catch (BadImageFormatException) { return 0; }
    }
"""
            "StaticVirtualOwnBody",
            "StaticRun",
            """
    [MethodImpl(MethodImplOptions.NoInlining)]
    static void Reach<T>() where T : IThing => T.StaticRun();

    // Constrained to the interface itself, which has no interface of its own to search for a
    // default: the method's own body is the one that runs.
    public static int Main()
    {
        try { Reach<IThing>(); return 1; }
        catch (BadImageFormatException) { return 0; }
    }
"""
            "StaticVirtualUnrelated",
            "StaticRun",
            """
    public static int Main()
    {
        try { Reacher.ReachUnrelated(); return 1; }
        catch (BadImageFormatException) { return 0; }
    }
"""
            "DynamicMethodCall",
            "NoBody",
            """
    public static int Main()
    {
        var method = new System.Reflection.Emit.DynamicMethod("Reach", typeof(void), Type.EmptyTypes, typeof(Program).Module);
        var il = method.GetILGenerator();
        il.Emit(System.Reflection.Emit.OpCodes.Call, typeof(Holder).GetMethod("NoBody"));
        il.Emit(System.Reflection.Emit.OpCodes.Ret);
        Action reach = (Action)method.CreateDelegate(typeof(Action));
        try { reach(); return 1; }
        catch (BadImageFormatException) { return 0; }
    }
"""
            "FunctionPointer",
            "NoBody",
            """
    [MethodImpl(MethodImplOptions.NoInlining)]
    static unsafe void Reach()
    {
        delegate*<void> pointer = &Holder.NoBody;
        pointer();
    }

    public static int Main()
    {
        try { Reach(); return 1; }
        catch (BadImageFormatException) { return 0; }
    }
"""
            "Delegate",
            "NoBody",
            """
    public static int Main()
    {
        // Binding succeeds; only the invocation fails.
        Action action = Holder.NoBody;
        try { action(); return 1; }
        catch (BadImageFormatException) { return 0; }
    }
"""
        ]

    let private caseNames : string list = cases |> List.map (fun (name, _, _) -> name)

    [<TestCaseSource(nameof caseNames)>]
    let ``a method with no IL binds and dispatches, and only its invocation is refused`` (caseName : string) : unit =
        let _, method, body = cases |> List.find (fun (name, _, _) -> name = caseName)

        // `DynamicMethod` refuses unless dynamic code is supported, which a real runtime's default
        // says it is and PawPrint's does not.
        let appContext =
            AppContextProperties.ofMap (
                Map.ofList
                    [
                        "System.Runtime.CompilerServices.RuntimeFeature.IsDynamicCodeSupported", "true"
                    ]
            )

        let onHost, onPawPrint =
            FabricatedGuest.runOnBothUnder
                appContext
                MethodWithoutIlImage.name
                (MethodWithoutIlImage.fabricate ())
                $"Driver%s{caseName}"
                (driver body)

        onHost |> shouldEqual (RealRuntimeResult.NormalExit 0)

        match onPawPrint with
        | FabricatedOutcome.Exited code ->
            failwith $"expected PawPrint to refuse invoking %s{method}, but the guest exited %d{code}"
        | FabricatedOutcome.Failed e ->
            e.Message |> shouldContainText "TODO"

            e.Message
            |> shouldContainText $"::%s{method}, an IL method whose row names no body"

            e.Message |> shouldContainText "BadImageFormatException"
