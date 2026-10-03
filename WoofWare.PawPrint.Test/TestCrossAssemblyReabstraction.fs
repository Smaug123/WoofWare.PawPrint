namespace WoofWare.PawPrint.Test

open NUnit.Framework

/// A default interface method reabstracted by a more specific interface: `IBar : IFoo` declaring
/// `abstract int IFoo.Frob();` over `IFoo`'s default body. C# refuses to compile a class that
/// implements `IBar` without supplying `Frob`, so the entry assembly is compiled against a build of
/// the library without the reabstraction, and runs against one with it.
///
/// CoreCLR takes the most specific default body (`MethodTable::FindDefaultInterfaceImplementation`)
/// with abstract candidates in the running, and a call landing on an abstract one throws
/// `EntryPointNotFoundException` (`MethodTable::FindDispatchImpl`, methodtable.cpp: "we hit a
/// reabstraction").
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestCrossAssemblyReabstraction =

    let libraryName = "Reabstraction.Lib"

    let entryName = "Reabstraction.Entry"

    /// The interfaces every build of the library declares the same way.
    let private unchanged : string =
        """
namespace Reabstraction;

public interface IFoo { int Frob() => 1; }

public interface IQux : IFoo { int IFoo.Frob() => 4; }

public interface ISFoo { static virtual int SFrob() => 1; }

public interface IV<out T> { int Frob() => 6; }

public interface IVDef<out T> { int Frob() => 7; }

public interface IVDefBaz : IVDef<string> { int IVDef<string>.Frob() => 8; }
"""

    /// The build the entry assembly is compiled against: nothing is reabstracted.
    let libraryBefore : CrossAssemblySpec =
        CrossAssemblySpec.library
            libraryName
            []
            [
                unchanged
                """
namespace Reabstraction;

public interface IBar : IFoo { }

public interface IBaz : IBar { int IFoo.Frob() => 3; }

public interface ISBar : ISFoo { }

public interface IVBar : IV<string> { }

public interface IVDefBar : IVDef<object> { }
"""
            ]

    /// The build the entry assembly runs against: `IBar`, `ISBar`, `IVBar` and `IVDefBar` each
    /// reabstract the method their parent gives a default body. `IBaz` supplies a body again,
    /// below `IBar`'s reabstraction.
    let libraryAfter : CrossAssemblySpec =
        CrossAssemblySpec.library
            libraryName
            []
            [
                unchanged
                """
namespace Reabstraction;

public interface IBar : IFoo { abstract int IFoo.Frob(); }

public interface IBaz : IBar { int IFoo.Frob() => 3; }

public interface ISBar : ISFoo { static abstract int ISFoo.SFrob(); }

public interface IVBar : IV<string> { abstract int IV<string>.Frob(); }

public interface IVDefBar : IVDef<object> { abstract int IVDef<object>.Frob(); }
"""
            ]

    /// The entry assembly's types and helpers. Each `Program.Case*` method is one call shape; a
    /// test's `Main` calls one of them.
    let private entrySupport : string =
        """
using System;
using Reabstraction;

namespace ReabstractionEntry;

public class C : IBar { }
public struct S : IBar { }
public sealed class SealedC : IBar { }
public class Reimplemented : IBaz { }
public class Diamond : IBar, IQux { }
public class Base : IFoo { public int Frob() => 5; }
public class Derived : Base, IBar { }
public class SC : ISBar { }
public struct SS : ISBar { }
public class VNo : IVBar { }
public class VDef : IVDefBar, IVDefBaz { }
public class VExact : IVBar, IV<object> { }

public static class Cases
{
    public const int Threw = 0;
    public const int Returned = 100;
    public const int ThrewSomethingElse = 200;

    /// `Threw` if `f` throws exactly `TException`, `Returned` plus what it returned if it
    /// returns, and `ThrewSomethingElse` otherwise.
    public static int Expect<TException>(Func<int> f) where TException : Exception
    {
        try
        {
            return Returned + f();
        }
        catch (Exception e) when (e.GetType() == typeof(TException))
        {
            return Threw;
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

    // Uncaught, for the escape analysis to be asked about.
    public static int ConstrainedOnValueType() => Constrained(new S());

    public static int ConstrainedOnSealedClass() => Constrained(new SealedC());

    public static int CaseInterfaceCall() =>
        Expect<EntryPointNotFoundException>(() => ThroughInterface(new C()));

    public static int CaseConstrainedOnValueType() =>
        Expect<EntryPointNotFoundException>(() => Constrained(new S()));

    public static int CaseConstrainedOnClass() =>
        Expect<EntryPointNotFoundException>(() => Constrained(new SealedC()));

    public static int CaseBoxedValueType() =>
        Expect<EntryPointNotFoundException>(() => ThroughInterface(new S()));

    public static int CaseStaticOnClass() =>
        Expect<EntryPointNotFoundException>(() => Static<SC>());

    public static int CaseStaticOnValueType() =>
        Expect<EntryPointNotFoundException>(() => Static<SS>());

    /// `ldvirtftn` resolves the slot as it builds the delegate, so the delegate is never made.
    public static int CaseLdvirtftn()
    {
        bool made = false;

        int result =
            Expect<EntryPointNotFoundException>(() =>
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
            Expect<EntryPointNotFoundException>(() =>
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
        return Expect<EntryPointNotFoundException>(() => d(new C()));
    }

    public static int CaseReimplementedBelowReabstraction() =>
        Expect<EntryPointNotFoundException>(() => ThroughInterface(new Reimplemented()));

    public static int CaseClassImplementationFromBase() =>
        Expect<EntryPointNotFoundException>(() => ThroughInterface(new Derived()));

    public static int CaseDiamond() =>
        Expect<System.Runtime.AmbiguousImplementationException>(() => ThroughInterface(new Diamond()));

    /// No entry is `IV<object>` itself, so the variance pass runs, and the most specific candidate
    /// it finds is `IVBar`'s reabstraction.
    public static int CaseVariantReabstraction() =>
        Expect<EntryPointNotFoundException>(() => ((IV<object>)new VNo()).Frob());

    /// The exact pass finds `IVDefBar`'s reabstraction, which is more specific than `IVDef<object>`'s
    /// own body, so the variance pass that would find `IVDefBaz`'s body never runs.
    public static int CaseExactReabstractionBeforeVariance() =>
        Expect<EntryPointNotFoundException>(() => ((IVDef<object>)new VDef()).Frob());

    /// `IV<object>` is an entry, so the exact pass finds its own body, and `IVBar`'s reabstraction of
    /// `IV<string>`, reached only through variance, is no candidate in it.
    public static int CaseVarianceOnlyReabstraction() =>
        Expect<EntryPointNotFoundException>(() => ((IV<object>)new VExact()).Frob());

    /// Measured: the delegate is made, and invoking it throws.
    public static int CaseStaticPointer()
    {
        Func<int> d;

        try
        {
            d = StaticPointer<SC>();
        }
        catch (Exception e)
        {
            Console.WriteLine("making the delegate threw " + e.GetType().FullName);
            return 50;
        }

        return Expect<EntryPointNotFoundException>(d);
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
namespace ReabstractionEntry;

public static class Program
{{
    public static int Main(string[] args) => Cases.%s{case}();
}}
"""
            ]

    /// The entry assembly, compiled against `libraryBefore`, for a test that reads the images rather
    /// than running them. Every build of it declares every case.
    let entryAssembly : CrossAssemblySpec = entry "CaseInterfaceCall"

    let private agrees (case : string) (expected : int) : unit =
        {
            Assemblies = [ libraryBefore ; entry case ]
            EntryAssemblyName = entryName
            ExpectedReturnCode = expected
        }
        |> CrossAssemblyHarness.runTestReplacing [ libraryAfter ]

    let private refuses (case : string) (expected : int) (messageContains : string list) : unit =
        {
            Assemblies = [ libraryBefore ; entry case ]
            EntryAssemblyName = entryName
            ExpectedReturnCode = expected
        }
        |> CrossAssemblyHarness.runTestExpectingRefusal messageContains [] [ libraryAfter ]

    [<Test>]
    let ``an interface call landing on a reabstraction throws EntryPointNotFoundException`` () : unit =
        agrees "CaseInterfaceCall" 0

    [<Test>]
    let ``a constrained call on a value type landing on a reabstraction throws`` () : unit =
        agrees "CaseConstrainedOnValueType" 0

    [<Test>]
    let ``a constrained call on a class landing on a reabstraction throws`` () : unit =
        agrees "CaseConstrainedOnClass" 0

    [<Test>]
    let ``an interface call on a boxed value type landing on a reabstraction throws`` () : unit =
        agrees "CaseBoxedValueType" 0

    [<Test>]
    let ``a static virtual call on a class landing on a reabstraction throws`` () : unit = agrees "CaseStaticOnClass" 0

    [<Test>]
    let ``a static virtual call on a value type landing on a reabstraction throws`` () : unit =
        agrees "CaseStaticOnValueType" 0

    [<Test>]
    let ``ldvirtftn of a reabstracted method throws before the delegate is made`` () : unit = agrees "CaseLdvirtftn" 0

    [<Test>]
    let ``a delegate bound to a reabstracted method throws before it is made`` () : unit = agrees "CaseCreateDelegate" 0

    [<Test>]
    let ``an open delegate invoked on a receiver whose slot is reabstracted throws`` () : unit =
        agrees "CaseOpenDelegate" 0

    [<Test>]
    let ``a body below the reabstraction is more specific than it`` () : unit =
        agrees "CaseReimplementedBelowReabstraction" 103

    [<Test>]
    let ``a class implementation inherited from a base class is not reabstracted`` () : unit =
        agrees "CaseClassImplementationFromBase" 105

    [<Test>]
    let ``a reabstraction through variance throws`` () : unit = agrees "CaseVariantReabstraction" 0

    /// PawPrint's exact pass also accepts `IVDefBaz`'s body for `IVDef<string>`, which CoreCLR's
    /// considers only once variance is allowed, so it sees two most specific bodies and declines to
    /// choose between them.
    [<Test>]
    let ``a reabstraction in the exact pass hides a body the variance pass would find`` () : unit =
        refuses "CaseExactReabstractionBeforeVariance" 0 [ "through a variant interface" ]

    [<Test>]
    let ``a reabstraction reached only through variance does not hide an exact body`` () : unit =
        agrees "CaseVarianceOnlyReabstraction" 106

    /// The reabstraction and `IQux`'s body are equally specific, so the call is ambiguous, which
    /// PawPrint does not yet raise in the guest.
    [<Test>]
    let ``a reabstraction as specific as a body is ambiguous`` () : unit =
        refuses "CaseDiamond" 0 [ "multiple most-specific default interface implementations" ]

    [<Test>]
    let ``a pointer to a reabstracted static virtual is refused`` () : unit =
        refuses "CaseStaticPointer" 0 [ "Ldftn" ; "reabstract" ]
