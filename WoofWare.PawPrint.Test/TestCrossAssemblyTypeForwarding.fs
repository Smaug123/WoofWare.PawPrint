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

    [<Test>]
    let ``a case-insensitive lookup folds ASCII, through a forwarder and without one`` () : unit =
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

        // The control: without ignoreCase, a folded name finds nothing, so the rows below are
        // testing the folding rather than some accident of exact matching.
        if (facade.GetType("typeforwardcross.target", throwOnError: false) is not null) return 1;

        // Folded, through the forwarder: namespace and name both differ in case from the metadata.
        Type t = facade.GetType("typeforwardcross.target", throwOnError: false, ignoreCase: true);
        if (t is null) return 2;
        if (t.FullName != "TypeForwardCross.Target") return 3;
        if (t.Assembly.GetName().Name != "TypeForward.Lib") return 4;

        // Upper-case and mixed-case spellings land on the same type.
        if (!ReferenceEquals(facade.GetType("TYPEFORWARDCROSS.TARGET", false, true), t)) return 5;
        if (!ReferenceEquals(facade.GetType("tYpEfOrWaRdCrOsS.tArGeT", false, true), t)) return 6;

        // And the facade's own type, which is a plain TypeDef rather than a forwarder row: the
        // folding is in the lookup, not in the forwarding.
        Type marker = facade.GetType("typeforwardfacade.marker", false, true);
        if (marker is null) return 7;
        if (!ReferenceEquals(marker, typeof(TypeForwardFacade.Marker))) return 8;

        // A folded name that matches nothing is still a miss.
        if (facade.GetType("typeforwardcross.nosuchtype", false, true) is not null) return 9;

        // Case-insensitivity does not make an arity-mangled generic name match without its arity.
        if (facade.GetType("typeforwardcross.gen", false, true) is not null) return 10;
        if (facade.GetType("typeforwardcross.gen`1", false, true) is null) return 11;

        return 0;
    }
}
"""
            EntryAssemblyName = "TypeForward.Entry"
            ExpectedReturnCode = 0
        }
        |> CrossAssemblyHarness.runTest

    [<Test>]
    let ``a forwarder into a case-colliding target is refused`` () : unit =
        // The walk asks the far side for the forwarder row's own spelling, exactly; CoreCLR
        // carries the fold across the hop instead. Measured on .NET 10: a facade forwarding
        // `TARGET` into a library declaring both `Target` and `TARGET` answers with `Target` — for
        // the shouty query too, even though that one matches a declaration exactly. PawPrint would
        // otherwise answer `TARGET` silently, which is a different type rather than a missing one.
        {
            Assemblies =
                [
                    CrossAssemblySpec.library
                        "FoldCollide.Lib"
                        []
                        [
                            """
namespace FoldCollide
{
    public class Target { }
    public class TARGET { }
}
"""
                        ]
                    CrossAssemblySpec.library
                        "FoldCollide.Facade"
                        [ "FoldCollide.Lib" ]
                        [
                            """
using System.Runtime.CompilerServices;

// The shouty one specifically, so the row's own spelling is a real declaration on the far side
// and the "target does not declare this" refusal cannot be what fires.
[assembly: TypeForwardedTo(typeof(FoldCollide.TARGET))]

namespace FoldCollideFacade
{
    public sealed class Marker
    {
    }
}
"""
                        ]
                    CrossAssemblySpec.entryPoint
                        "FoldCollide.Entry"
                        [ "FoldCollide.Facade" ]
                        [
                            """
using System;
using System.Reflection;

class Program
{
    static int Main()
    {
        Assembly facade = typeof(FoldCollideFacade.Marker).Assembly;

        // Exact: the forwarder row's own spelling, and it resolves to that declaration.
        Type exact = facade.GetType("FoldCollide.TARGET", throwOnError: false);
        if (exact is null) return 1;
        if (exact.FullName != "FoldCollide.TARGET") return 2;

        // Folded: the same query answers with the *other* declaration.
        Type folded = facade.GetType("FoldCollide.TARGET", throwOnError: false, ignoreCase: true);
        if (folded is null) return 3;
        if (folded.FullName != "FoldCollide.Target") return 4;

        return 0;
    }
}
"""
                        ]
                ]
            EntryAssemblyName = "FoldCollide.Entry"
            ExpectedReturnCode = 0
        }
        |> CrossAssemblyHarness.runTestExpectingRefusal
            [
                "followed a forwarder into"
                "does not have one answer"
                "FoldCollide.Target"
            ]
            []

    [<Test>]
    let ``the module pseudo-type is not something GetType can name`` () : unit =
        // ECMA-335 II.22.37's first `TypeDef` row parents an assembly's module-scope functions and
        // variables. `ClassLoader::PopulateAvailableClassHashTable` skips it, so it is in none of
        // the tables `Assembly.GetType` searches. Measured: real .NET answers null at either
        // casing, and `GetTypes()` omits it too. Both spellings are here because the
        // case-sensitive lookup was answering with it as well.
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

        foreach (string name in new[] { "<Module>", "<module>" })
        {
            foreach (bool ignoreCase in new[] { false, true })
            {
                if (facade.GetType(name, throwOnError: false, ignoreCase) is not null) return 1;
            }
        }

        // It is genuinely in the metadata — this is the lookup declining to name it, not the
        // assembly lacking the row.
        if (Array.Exists(facade.GetTypes(), t => t.Name == "<Module>")) return 2;

        // A real type in the same assembly still resolves, so the exclusion is that row and not
        // the whole table.
        if (facade.GetType("TypeForwardFacade.Marker", throwOnError: false) is null) return 3;

        return 0;
    }
}
"""
            EntryAssemblyName = "TypeForward.Entry"
            ExpectedReturnCode = 0
        }
        |> CrossAssemblyHarness.runTest

    /// A facade that both *defines* a type and *forwards* a differently-cased one of the same
    /// folded name. Legal, and Roslyn emits it happily.
    let private collidingFacadeSources =
        [
            """
using System.Runtime.CompilerServices;

[assembly: TypeForwardedTo(typeof(TypeForwardCross.Target))]

namespace TypeForwardCross
{
    // Folds to the same name as the forwarded TypeForwardCross.Target.
    public class TARGET
    {
    }
}

namespace TypeForwardFacade
{
    public sealed class Marker
    {
    }
}
"""
        ]

    let private collidingAssemblies (entrySource : string) =
        [
            CrossAssemblySpec.library "TypeForward.Base" [] baseSources
            CrossAssemblySpec.library "TypeForward.Lib" [ "TypeForward.Base" ] libSources
            CrossAssemblySpec.library
                "TypeForward.Facade"
                [ "TypeForward.Lib" ; "TypeForward.Base" ]
                collidingFacadeSources
            CrossAssemblySpec.entryPoint "TypeForward.Entry" [ "TypeForward.Facade" ] [ entrySource ]
        ]

    [<Test>]
    let ``a folded name matching both a definition and a forwarder is refused`` () : unit =
        // Measured on .NET 10: with the facade defining `N.TARGET` and forwarding `N.Target`, a
        // case-insensitive query answers with the *forwarded* type — in preference to the facade's
        // own type even when that one matches exactly. Under `ignoreCase` CoreCLR has one table,
        // not two consulted in order, so which of two folded matches comes back is the same
        // unspecified hash ordering that makes two colliding definitions ambiguous. The guest
        // below records what the real runtime answers; PawPrint declines to guess it.
        {
            Assemblies =
                collidingAssemblies
                    """
using System;
using System.Reflection;

class Program
{
    static int Main()
    {
        Assembly facade = typeof(TypeForwardFacade.Marker).Assembly;

        // Exact lookups are unaffected: each name means the row that spells it that way.
        Type exact = facade.GetType("TypeForwardCross.TARGET", throwOnError: false);
        if (exact is null) return 1;
        if (exact.Assembly.GetName().Name != "TypeForward.Facade") return 2;

        // Folded, the forwarded type wins over the facade's own exact match.
        Type folded = facade.GetType("TypeForwardCross.TARGET", throwOnError: false, ignoreCase: true);
        if (folded is null) return 3;
        if (folded.Assembly.GetName().Name != "TypeForward.Lib") return 4;
        if (folded.FullName != "TypeForwardCross.Target") return 5;

        return 0;
    }
}
"""
            EntryAssemblyName = "TypeForward.Entry"
            ExpectedReturnCode = 0
        }
        |> CrossAssemblyHarness.runTestExpectingRefusal
            [ "is ambiguous between the type it defines" ; "TypeForwardCross.TARGET" ]
            []

    [<Test>]
    let ``a folded lookup that must cross a forwarder hop is refused`` () : unit =
        // Measured on .NET 10: with the library replaced by a build that declares the type under a
        // different casing, a case-insensitive query still resolves — CoreCLR carries the fold
        // across the hop. PawPrint follows the forwarder row's own spelling exactly on the far
        // side, so it would answer null; it stops instead.
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

        // The exact spelling the forwarder row carries is gone from the target.
        if (facade.GetType("TypeForwardCross.Target", throwOnError: false) is not null) return 1;

        // Folded, the real runtime finds the differently-cased declaration on the far side.
        Type t = facade.GetType("TypeForwardCross.Target", throwOnError: false, ignoreCase: true);
        if (t is null) return 2;
        if (t.FullName != "TypeForwardCross.TARGET") return 3;
        if (t.Assembly.GetName().Name != "TypeForward.Lib") return 4;

        return 0;
    }
}
"""
            EntryAssemblyName = "TypeForward.Entry"
            ExpectedReturnCode = 0
        }
        |> CrossAssemblyHarness.runTestExpectingRefusal
            [
                "followed a forwarder out of"
                "Folding is not yet carried across a forwarder hop"
            ]
            [
                // Same assembly name, declaring the forwarded type under a different casing.
                CrossAssemblySpec.library
                    "TypeForward.Lib"
                    []
                    [
                        """
namespace TypeForwardCross;

public sealed class TARGET
{
}

public sealed class Bystander
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
