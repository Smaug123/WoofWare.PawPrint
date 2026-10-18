namespace WoofWare.PawPrint.Test

open NUnit.Framework

/// Two interfaces that each override their common parent's default body, neither more specific
/// than the other: `ILeft : IFoo` and `IRight : IFoo`, each with `int IFoo.Frob()`, implemented by a
/// type that names both. C# refuses to compile that type, so the entry assembly is compiled against
/// a build of the library in which only `ILeft` overrides, and runs against one in which both do.
///
/// CoreCLR's `MethodTable::FindDefaultInterfaceImplementation` finds two most specific bodies and
/// throws `AmbiguousImplementationException` (`ThrowAmbiguousResolutionException`). Measured: every
/// shape here throws it, and does so alike with tiered compilation off, since the interface is not
/// variant.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestCrossAssemblyAmbiguousDefault =

    let private libraryName = "Ambiguous.Lib"

    let private entryName = "Ambiguous.Entry"

    let private unchanged : string =
        """
namespace Ambiguous;

public interface IFoo { int Frob() => 1; }

public interface ILeft : IFoo { int IFoo.Frob() => 2; }

public interface ISFoo { static virtual int SFrob() => 1; }

public interface ISLeft : ISFoo { static int ISFoo.SFrob() => 2; }
"""

    /// The build the entry assembly is compiled against: only `ILeft` and `ISLeft` override.
    let private libraryBefore : CrossAssemblySpec =
        CrossAssemblySpec.library
            libraryName
            []
            [
                unchanged
                """
namespace Ambiguous;

public interface IRight : IFoo { }

public interface ISRight : ISFoo { }
"""
            ]

    /// The build the entry assembly runs against: `IRight` and `ISRight` override too.
    let private libraryAfter : CrossAssemblySpec =
        CrossAssemblySpec.library
            libraryName
            []
            [
                unchanged
                """
namespace Ambiguous;

public interface IRight : IFoo { int IFoo.Frob() => 3; }

public interface ISRight : ISFoo { static int ISFoo.SFrob() => 3; }
"""
            ]

    let private entrySupport : string =
        """
using System;
using System.Runtime;
using Ambiguous;

namespace AmbiguousEntry;

public class C : ILeft, IRight { }
public sealed class SealedC : ILeft, IRight { }
public struct S : ILeft, IRight { }
public class SC : ISLeft, ISRight { }
public struct SS : ISLeft, ISRight { }
public struct GS<U> : ISLeft, ISRight { }

public static class Holder<T> where T : ISFoo
{
    public static Func<int> Pointer() => T.SFrob;
}

public static class Cases
{
    public const int Threw = 0;
    public const int Returned = 100;
    public const int ThrewSomethingElse = 200;

    /// `Threw` if `f` throws exactly `AmbiguousImplementationException`, `Returned` plus what it
    /// returned if it returns, and `ThrewSomethingElse` otherwise.
    public static int ExpectAmbiguous(Func<int> f)
    {
        try
        {
            return Returned + f();
        }
        catch (Exception e) when (e.GetType() == typeof(AmbiguousImplementationException))
        {
            // The runtime's HResult, which the parameterless constructor also sets.
            return e.HResult == unchecked((int)0x8013106A) ? Threw : 201;
        }
        catch (Exception e)
        {
            Console.WriteLine("unexpected " + e.GetType().FullName + ": " + e.Message);
            return ThrewSomethingElse;
        }
    }

    public static int ThroughInterface(IFoo f) => f.Frob();

    public static int Constrained<T>(T t) where T : IFoo => t.Frob();

    public static int Static<T>() where T : ISFoo => T.SFrob();

    public static Func<int> StaticPointer<T>() where T : ISFoo => T.SFrob;

    public static int reached;

    /// Counts its runs before the `ldftn`, so a case can tell the `ldftn` throwing from the call
    /// throwing on entry.
    public static Func<int> CountedStaticPointer<T>() where T : ISFoo
    {
        reached++;
        return T.SFrob;
    }

    public static int CaseInterfaceCall() => ExpectAmbiguous(() => ThroughInterface(new C()));

    public static int CaseConstrainedOnClass() => ExpectAmbiguous(() => Constrained(new SealedC()));

    public static int CaseConstrainedOnValueType() => ExpectAmbiguous(() => Constrained(new S()));

    public static int CaseBoxedValueType() => ExpectAmbiguous(() => ThroughInterface(new S()));

    public static int CaseStaticOnClass() => ExpectAmbiguous(() => Static<SC>());

    public static int CaseStaticOnValueType() => ExpectAmbiguous(() => Static<SS>());

    /// `ldvirtftn` resolves the slot as it builds the delegate, so the delegate is never made.
    public static int CaseLdvirtftn()
    {
        bool made = false;

        int result =
            ExpectAmbiguous(() =>
            {
                Func<int> d = ((IFoo)new C()).Frob;
                made = true;
                return d();
            });

        return made ? 50 : result;
    }

    /// As `CaseLdvirtftn`, `CreateDelegate` resolves the slot as it binds.
    public static int CaseCreateDelegate()
    {
        bool made = false;

        int result =
            ExpectAmbiguous(() =>
            {
                var d =
                    (Func<int>)Delegate.CreateDelegate(typeof(Func<int>), new C(), typeof(IFoo).GetMethod("Frob"));
                made = true;
                return d();
            });

        return made ? 50 : result;
    }

    /// An open delegate over an interface method dispatches as it is invoked.
    public static int CaseOpenDelegate()
    {
        var d = (Func<IFoo, int>)Delegate.CreateDelegate(typeof(Func<IFoo, int>), typeof(IFoo).GetMethod("Frob"));
        return ExpectAmbiguous(() => d(new C()));
    }

    /// Measured: where CoreCLR runs the method `make` names as shared generic code, the delegate is
    /// made, and invoking it throws. Calling it through a delegate keeps the JIT from inlining it
    /// into exact code, where the `ldftn` would throw.
    public static int MadeThenThrows(Func<Func<int>> make)
    {
        Func<int> d;

        try
        {
            d = make();
        }
        catch (Exception e)
        {
            Console.WriteLine("making the delegate threw " + e.GetType().FullName);
            return 50;
        }

        return ExpectAmbiguous(d);
    }

    public static int CaseStaticPointer() => MadeThenThrows(StaticPointer<SC>);

    public static int CaseStaticPointerOnSharedValueType() => MadeThenThrows(StaticPointer<GS<string>>);

    public static int CaseStaticPointerInSharedType() => MadeThenThrows(Holder<SC>.Pointer);

    /// Measured: CoreCLR compiles `CountedStaticPointer<SS>` for `SS` alone and puts the throw at
    /// the `ldftn`, so the delegate is never made.
    public static int CaseStaticPointerOnValueType()
    {
        bool made = false;

        int result =
            ExpectAmbiguous(() =>
            {
                Func<int> d = CountedStaticPointer<SS>();
                made = true;
                return d();
            });

        if (made)
            return 50;

        return reached == 1 ? result : 60;
    }
}
"""

    let private entry (case : string) : CrossAssemblySpec =
        CrossAssemblySpec.entryPoint
            entryName
            [ libraryName ]
            [
                entrySupport
                $"""
namespace AmbiguousEntry;

public static class Program
{{
    public static int Main(string[] args) => Cases.%s{case}();
}}
"""
            ]

    let private agrees (case : string) : unit =
        {
            Assemblies = [ libraryBefore ; entry case ]
            EntryAssemblyName = entryName
            ExpectedReturnCode = 0
        }
        |> CrossAssemblyHarness.runTestReplacing [ libraryAfter ]

    let private refuses (case : string) : unit =
        {
            Assemblies = [ libraryBefore ; entry case ]
            EntryAssemblyName = entryName
            ExpectedReturnCode = 0
        }
        |> CrossAssemblyHarness.runTestExpectingRefusal [ "Ldftn" ; "ambiguous" ; "shared" ] [] [ libraryAfter ]

    [<Test>]
    let ``an ambiguous interface call throws AmbiguousImplementationException`` () : unit = agrees "CaseInterfaceCall"

    [<Test>]
    let ``an ambiguous constrained call on a class throws`` () : unit = agrees "CaseConstrainedOnClass"

    [<Test>]
    let ``an ambiguous constrained call on a value type throws`` () : unit = agrees "CaseConstrainedOnValueType"

    [<Test>]
    let ``an ambiguous interface call on a boxed value type throws`` () : unit = agrees "CaseBoxedValueType"

    [<Test>]
    let ``an ambiguous static virtual call on a class throws`` () : unit = agrees "CaseStaticOnClass"

    [<Test>]
    let ``an ambiguous static virtual call on a value type throws`` () : unit = agrees "CaseStaticOnValueType"

    [<Test>]
    let ``ldvirtftn of an ambiguous method throws before the delegate is made`` () : unit = agrees "CaseLdvirtftn"

    [<Test>]
    let ``a delegate bound to an ambiguous method throws before it is made`` () : unit = agrees "CaseCreateDelegate"

    [<Test>]
    let ``an open delegate invoked on a receiver whose slot is ambiguous throws`` () : unit = agrees "CaseOpenDelegate"

    [<Test>]
    let ``a pointer to an ambiguous static virtual in exact code throws at the ldftn`` () : unit =
        agrees "CaseStaticPointerOnValueType"

    [<Test>]
    let ``a pointer to an ambiguous static virtual in shared code is refused`` () : unit = refuses "CaseStaticPointer"

    [<Test>]
    let ``a pointer to an ambiguous static virtual over a value type with a class argument is refused`` () : unit =
        refuses "CaseStaticPointerOnSharedValueType"

    [<Test>]
    let ``a pointer to an ambiguous static virtual in a type CoreCLR shares is refused`` () : unit =
        refuses "CaseStaticPointerInSharedType"
