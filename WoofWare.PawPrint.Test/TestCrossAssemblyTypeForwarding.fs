namespace WoofWare.PawPrint.Test

open NUnit.Framework

/// <summary>
/// What <c>Assembly.GetType</c> answers when the assembly it is asked only *names* the type, and
/// another assembly defines it.
/// </summary>
/// <remarks>
/// A single-assembly guest cannot reach this at all: an <c>ExportedType</c> forwarder row needs a
/// second assembly to point at, and the type it names must be absent from the asking assembly's own
/// <c>TypeDef</c> table. These cases put the forwarder between the two.
///
/// Each guest starts from <c>typeof(Marker).Assembly</c>, a type the facade genuinely defines, so
/// it reaches <c>AssemblyNative_GetTypeCore</c> without an assembly *bind* first: the
/// <c>Type.GetType("Name, Assembly")</c> spelling would stop earlier, at
/// <c>AssemblyNative_InternalLoad</c>, which PawPrint does not implement.
/// </remarks>
[<TestFixture>]
module TestCrossAssemblyTypeForwarding =

    /// The defining assembly's sources, shared by every case below. Nothing in the entry assemblies
    /// references it directly, so it is still unloaded when the forwarder is followed.
    let private libSources =
        [
            """
namespace TypeForwardCross;

public sealed class Target
{
    public static int Answer => 42;
}

/// Derives from a class in a third assembly, so that resolving it needs more than the assembly
/// the forwarder points at.
public sealed class Derived : TypeForwardBase.Root
{
}

public sealed class Outer
{
    public sealed class Inner
    {
        public static int Answer => 7;
    }
}

public sealed class Gen<T>
{
}

public sealed class Bystander
{
}
"""
        ]

    /// The facade's sources: forwarder rows for everything in the library, plus one type it really
    /// does define so a guest can get hold of its `Assembly` without binding by name.
    let private facadeSources =
        [
            """
using System.Runtime.CompilerServices;

[assembly: TypeForwardedTo(typeof(TypeForwardCross.Target))]
[assembly: TypeForwardedTo(typeof(TypeForwardCross.Outer))]
[assembly: TypeForwardedTo(typeof(TypeForwardCross.Gen<>))]
[assembly: TypeForwardedTo(typeof(TypeForwardCross.Bystander))]
[assembly: TypeForwardedTo(typeof(TypeForwardCross.Derived))]

namespace TypeForwardFacade;

public sealed class Marker
{
}
"""
        ]

    /// The base class of one of the forwarded types, in an assembly of its own so that a test can
    /// take it away or replace it independently of the library.
    let private baseSources =
        [
            """
namespace TypeForwardBase;

public class Root
{
}
"""
        ]

    let private assemblies (entrySource : string) =
        [
            CrossAssemblySpec.library "TypeForward.Base" [] baseSources
            CrossAssemblySpec.library "TypeForward.Lib" [ "TypeForward.Base" ] libSources
            CrossAssemblySpec.library "TypeForward.Facade" [ "TypeForward.Lib" ; "TypeForward.Base" ] facadeSources
            CrossAssemblySpec.entryPoint "TypeForward.Entry" [ "TypeForward.Facade" ] [ entrySource ]
        ]

    [<Test>]
    let ``a forwarded type resolves to the assembly that defines it`` () : unit =
        {
            Assemblies =
                assemblies
                    """
using System;
using System.Reflection;

class Program
{
    static int Main()
    {
        Assembly facade = typeof(TypeForwardFacade.Marker).Assembly;

        // The facade names TypeForwardCross.Target only through an ExportedType row, so answering
        // this at all means following the forwarder into the library — which is not loaded yet.
        Type t = facade.GetType("TypeForwardCross.Target", throwOnError: false);
        if (t is null) return 1;

        if (t.FullName != "TypeForwardCross.Target") return 2;

        // The answer is the *defining* assembly, not the one that was asked.
        if (t.Assembly.GetName().Name != "TypeForward.Lib") return 3;
        if (ReferenceEquals(t.Assembly, facade)) return 4;

        // A name the facade neither defines nor forwards is still a miss, rather than anything the
        // forwarder walk invented on the way.
        if (facade.GetType("TypeForwardCross.Absent", throwOnError: false) is not null) return 5;

        // The facade has not stopped being able to answer for its own types.
        Type marker = facade.GetType("TypeForwardFacade.Marker", throwOnError: false);
        if (marker is null) return 6;
        if (!ReferenceEquals(marker.Assembly, facade)) return 7;

        return 0;
    }
}
"""
            EntryAssemblyName = "TypeForward.Entry"
            ExpectedReturnCode = 0
        }
        |> CrossAssemblyHarness.runTest

    [<Test>]
    let ``a forwarder whose target assembly is absent answers null, or throws FileNotFoundException`` () : unit =
        {
            Assemblies =
                assemblies
                    """
using System;
using System.IO;
using System.Reflection;

class Program
{
    static int Main()
    {
        Assembly facade = typeof(TypeForwardFacade.Marker).Assembly;

        // TypeForward.Lib was there when the facade was compiled and is not there now, so the
        // forwarder names an assembly nothing supplies.
        if (facade.GetType("TypeForwardCross.Target", throwOnError: false) is not null) return 1;

        // Asked to throw, the same lookup reports the failed *bind* rather than a missing type:
        // the runtime raises FileNotFoundException out of the lookup, and it is only the
        // throwOnError:false case above that swallows it.
        try
        {
            Type t = facade.GetType("TypeForwardCross.Target", throwOnError: true);
            return 2;
        }
        catch (FileNotFoundException)
        {
        }
        catch (Exception e)
        {
            Console.WriteLine("unexpected: " + e.GetType().FullName);
            return 3;
        }

        // The facade itself loaded fine, and still answers for its own types: the failure is
        // specific to following the forwarder, not a wholesale refusal to look anything up.
        if (facade.GetType("TypeForwardFacade.Marker", throwOnError: false) is null) return 4;

        return 0;
    }
}
"""
            EntryAssemblyName = "TypeForward.Entry"
            ExpectedReturnCode = 0
        }
        |> CrossAssemblyHarness.runTestWithout [ "TypeForward.Lib" ]

    [<Test>]
    let ``a forwarder into an assembly that no longer declares the type answers null, or throws TypeLoadException``
        ()
        : unit
        =
        {
            Assemblies =
                assemblies
                    """
using System;
using System.IO;
using System.Reflection;

class Program
{
    static int Main()
    {
        Assembly facade = typeof(TypeForwardFacade.Marker).Assembly;

        // TypeForward.Lib binds — it is right there under the name the forwarder asks for — and
        // the build of it that is present declares no TypeForwardCross.Target.
        if (facade.GetType("TypeForwardCross.Target", throwOnError: false) is not null) return 1;

        // A different failure from an unbindable assembly, and the runtime says so: nothing failed
        // to *load*, so what is reported is that the type could not be resolved.
        try
        {
            Type t = facade.GetType("TypeForwardCross.Target", throwOnError: true);
            return 2;
        }
        catch (TypeLoadException)
        {
        }
        catch (FileNotFoundException)
        {
            // The wrong one of the two: everything the chain names did load.
            return 3;
        }
        catch (Exception e)
        {
            Console.WriteLine("unexpected: " + e.GetType().FullName);
            return 4;
        }

        // The replacement library is genuinely present and readable, so "absent type" is not
        // standing in for "absent assembly".
        if (facade.GetType("TypeForwardCross.Bystander", throwOnError: false) is null) return 5;

        return 0;
    }
}
"""
            EntryAssemblyName = "TypeForward.Entry"
            ExpectedReturnCode = 0
        }
        |> CrossAssemblyHarness.runTestReplacing
            [
                // Same assembly name, without Target. `Bystander` is forwarded by the facade too,
                // and is present in both builds, so the guest can tell "this library loaded" from
                // "this library is missing".
                CrossAssemblySpec.library
                    "TypeForward.Lib"
                    []
                    [
                        """
namespace TypeForwardCross;

public sealed class Bystander
{
}
"""
                    ]
            ]

    [<Test>]
    let ``a forwarded type whose base class is gone throws TypeLoadException, asked to throw or not`` () : unit =
        {
            Assemblies =
                assemblies
                    """
using System;
using System.IO;
using System.Reflection;

class Program
{
    static int Main()
    {
        Assembly facade = typeof(TypeForwardFacade.Marker).Assembly;

        // TypeForward.Base is present and binds; the build of it that is present does not declare
        // Root, which TypeForwardCross.Derived derives from. Loading the type therefore fails on
        // its base, which is a *type* load failure, not an assembly one — so unlike every other
        // way of not arriving, throwOnError:false does not turn it into a null.
        foreach (bool throwOnError in new[] { false, true })
        {
            try
            {
                Type t = facade.GetType("TypeForwardCross.Derived", throwOnError);
                Console.WriteLine("unexpectedly resolved, throwOnError=" + throwOnError);
                return 1;
            }
            catch (TypeLoadException)
            {
            }
            catch (Exception e)
            {
                Console.WriteLine("unexpected " + e.GetType().FullName + ", throwOnError=" + throwOnError);
                return 2;
            }
        }

        // A forwarded type in the same library that does not touch the broken base still resolves,
        // so this is specific to the base chain rather than the library being unusable.
        Type ok = facade.GetType("TypeForwardCross.Target", throwOnError: false);
        if (ok is null) return 3;
        if (ok.Assembly.GetName().Name != "TypeForward.Lib") return 4;

        return 0;
    }
}
"""
            EntryAssemblyName = "TypeForward.Entry"
            ExpectedReturnCode = 0
        }
        |> CrossAssemblyHarness.runTestReplacing
            [
                // Same assembly name, no Root.
                CrossAssemblySpec.library
                    "TypeForward.Base"
                    []
                    [
                        """
namespace TypeForwardBase;

public class Stranger
{
}
"""
                    ]
            ]

    // Parked on a hole that has nothing to do with forwarding, and which no `GetTypeCore` caller
    // can dodge: a nested name reaches the QCall as `ReadOnlySpan<string> nestedTypeNames`, and
    // the `LibraryImport` stub that marshals it reads each destination slot of its `localloc`
    // buffer before writing it (`Ldind_i` at IL_006E of `RuntimeAssembly.GetTypeCore`, into a
    // local the very next store overwrites). CoreLib is compiled `[SkipLocalsInit]`, so PawPrint
    // marks that buffer `Uninitialized` and refuses the read outright, which is the whole point of
    // modelling initialisation — but it stops every nested lookup, forwarded or not, before the
    // QCall is even entered. Un-park once such a read is allowed to yield a value.
    [<Explicit "blocked on reading an uninitialised localloc buffer in the span-marshalling stub">]
    [<Test>]
    let ``a nested type under a forwarded type is looked up in the defining assembly`` () : unit =
        {
            Assemblies =
                assemblies
                    """
using System;
using System.Reflection;

class Program
{
    static int Main()
    {
        Assembly facade = typeof(TypeForwardFacade.Marker).Assembly;

        // `Outer+Inner` arrives at the QCall as the top-level name plus one nested name. The
        // forwarder only names `Outer`; `Inner` exists solely as a nested TypeDef in the library,
        // so the walk for it has to continue there rather than back in the facade.
        Type inner = facade.GetType("TypeForwardCross.Outer+Inner", throwOnError: false);
        if (inner is null) return 1;

        if (inner.FullName != "TypeForwardCross.Outer+Inner") return 2;
        if (inner.Assembly.GetName().Name != "TypeForward.Lib") return 3;

        Type outer = facade.GetType("TypeForwardCross.Outer", throwOnError: false);
        if (outer is null) return 4;
        if (!ReferenceEquals(inner.DeclaringType, outer)) return 5;

        // A nested name that does not exist under a forwarded parent is a miss, not a fall back to
        // some top-level type of that name.
        if (facade.GetType("TypeForwardCross.Outer+Absent", throwOnError: false) is not null) return 6;

        return 0;
    }
}
"""
            EntryAssemblyName = "TypeForward.Entry"
            ExpectedReturnCode = 0
        }
        |> CrossAssemblyHarness.runTest

    [<Test>]
    let ``a forwarded generic type resolves to its generic type definition`` () : unit =
        {
            Assemblies =
                assemblies
                    """
using System;
using System.Reflection;

class Program
{
    static int Main()
    {
        Assembly facade = typeof(TypeForwardFacade.Marker).Assembly;

        // Arity is spelled in the metadata name, and `GetType` on a bare generic name answers the
        // open definition — the same thing `typeof(Gen<>)` denotes.
        Type gen = facade.GetType("TypeForwardCross.Gen`1", throwOnError: false);
        if (gen is null) return 1;

        if (!gen.IsGenericTypeDefinition) return 2;
        if (gen.GetGenericArguments().Length != 1) return 3;
        if (gen.Assembly.GetName().Name != "TypeForward.Lib") return 4;

        return 0;
    }
}
"""
            EntryAssemblyName = "TypeForward.Entry"
            ExpectedReturnCode = 0
        }
        |> CrossAssemblyHarness.runTest
