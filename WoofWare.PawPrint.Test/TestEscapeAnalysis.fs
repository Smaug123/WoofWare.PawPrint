namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open FsUnitTyped
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PawPrint.Analysis

/// The escape analysis over a fixture whose every method states what must, and must not, escape
/// it. Each expectation was written from the analysis's stated envelope before it was run.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestEscapeAnalysis =

    let private source =
        """
using System;

namespace Fixture;

public class GenericFailure<T> : Exception { }

public class Locking
{
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.Synchronized)]
    public void Instance() { }
}

public static class Intrinsics
{
    // `Unsafe.As<TFrom, TTo>(ref TFrom)` is a body CoreCLR's VM substitutes; the read through the
    // result is the caller's own.
    public static uint Reinterpret(ref int x) => System.Runtime.CompilerServices.Unsafe.As<int, uint>(ref x);

    // A barrier the JIT emits in place of the method's call to itself.
    public static void Fence() => System.Threading.Interlocked.MemoryBarrier();

    // A capability query, which the JIT answers as a constant for the CPU.
    public static bool Accelerated() => System.Runtime.Intrinsics.Vector128.IsHardwareAccelerated;
}

public static class Natives
{
    // FCalls into the C runtime's maths library, which the JIT may instead expand into an
    // instruction; neither faults.
    public static double Sine(double x) => Math.Sin(x);
    public static double Power(double x, double y) => Math.Pow(x, y);
    public static double Fused(double a, double b, double c) => Math.FusedMultiplyAdd(a, b, c);

    // An FCall no contract describes.
    public static int ThreadId() => Environment.CurrentManagedThreadId;

    // The shadow `System.MathF` in this assembly, not CoreLib's.
    public static float Impostor() => MathF.Sin(1f);
}

public static class Cases
{
    public static void ThrowsDirectly() { throw new InvalidOperationException("boom"); }

    public static void CaughtExactly()
    {
        try { ThrowsDirectly(); }
        catch (InvalidOperationException) { }
    }

    // SystemException is CoreLib's, so seeing that it covers InvalidOperationException needs the
    // base chain of a type in another assembly.
    public static void CaughtByBase()
    {
        try { ThrowsDirectly(); }
        catch (SystemException) { }
    }

    public static void UnrelatedCatch()
    {
        try { ThrowsDirectly(); }
        catch (ArgumentException) { }
    }

    public static void FinallyDoesNotCatch()
    {
        try { ThrowsDirectly(); }
        finally { GC.KeepAlive(null); }
    }

    public static void PropagatesOneHop() { UnrelatedCatch(); }

    // Entering it takes a monitor, which an interrupted wait abandons before any IL runs.
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.Synchronized)]
    public static void Synchronised() { }

    public static void Unsynchronised() { }

    public static void CatchesInterruption()
    {
        try { Synchronised(); }
        catch (System.Threading.ThreadInterruptedException) { }
    }

    // Its body releases the monitor itself, so releasing it again on the way out throws, past
    // the body's handlers.
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.Synchronized)]
    public static void ReleasesEarly()
    {
        try { System.Threading.Monitor.Exit(typeof(Cases)); }
        catch (Exception) { }
    }

    public static void TwoSources(bool b)
    {
        if (b) { ThrowsDirectly(); }
        else { throw new FormatException(); }
    }

    public static void CatchesBoth(bool b)
    {
        try { TwoSources(b); }
        catch (Exception) { }
    }

    public static int Leaf(int a) => a;

    public static void Recursive(int n)
    {
        if (n <= 0) { ThrowsDirectly(); }
        else { Recursive(n - 1); }
    }

    public static void Rethrows()
    {
        try { ThrowsDirectly(); }
        catch (Exception) { throw; }
    }

    public static int Divide(int a, int b) => a / b;

    public static int Index(int[] a, int i) => a[i];

    // Division raises only faults the analysis names, so what a clause catches of it is known.
    public static int RethrowsDivision(int a, int b)
    {
        try { return Divide(a, b); }
        catch (DivideByZeroException) { throw; }
    }

    // What the first clause swallows never reaches the second, so the second rethrows only the
    // rest.
    public static int RethrowsWhatEarlierClausesLeave(int[] a, int i)
    {
        try { return Index(a, i); }
        catch (IndexOutOfRangeException) { return 0; }
        catch (Exception) { throw; }
    }

    // Whatever an unknown callee raises, what a clause for one exception type catches is that type.
    public static void RethrowsTheTypeItCatches(Animal a)
    {
        try { CallsVirtual(a); }
        catch (InvalidOperationException) { throw; }
        catch (Exception) { }
    }

    // A C# assembly wraps a thrown non-exception, which `catch (Exception)` then catches, and
    // `throw;` rethrows the object it wraps.
    public static void RethrowsAnythingCaughtAsException(Animal a)
    {
        try { CallsVirtual(a); }
        catch (Exception) { throw; }
    }

    // `throw;` rethrows what the innermost enclosing handler caught.
    public static int RethrowsInnermost(int[] a, int i, int d)
    {
        try { return Index(a, i); }
        catch (IndexOutOfRangeException)
        {
            try { return Divide(1, d); }
            catch (DivideByZeroException) { throw; }
        }
    }

    // The second clause stops what the filter declines, so only what the filter accepts escapes.
    public static int RethrowsFromFilteredHandler(int a, int b)
    {
        try { return Divide(a, b); }
        catch (Exception) when (b == 0) { throw; }
        catch (Exception) { return 0; }
    }

    // The inner clause's protected block is in the outer clause's handler, and catches what the
    // outer clause's `throw;` rethrows.
    public static int RethrowsWithinAHandler(int a, int b)
    {
        try { return Divide(a, b); }
        catch (DivideByZeroException)
        {
            try { throw; }
            catch (DivideByZeroException) { throw; }
        }
    }

    // The outer clause catches what the inner one rethrows.
    public static int RethrowsWhatIsRethrown(int a, int b)
    {
        try
        {
            try { return Divide(a, b); }
            catch (DivideByZeroException) { throw; }
        }
        catch (DivideByZeroException) { throw; }
    }

    public static void ThrowsNull() { throw null; }

    public static void ThrowsLocalDerived() { throw new Derived(); }

    public static void CaughtByLocalBase()
    {
        try { ThrowsLocalDerived(); }
        catch (LocalBase) { }
    }

    static InvalidOperationException MakeInvalid() => new InvalidOperationException("made");

    // Only the helper's declared return type is known of what is thrown.
    public static void ThrowsHelperResult() { throw MakeInvalid(); }

    public static void HelperCaughtByBase()
    {
        try { ThrowsHelperResult(); }
        catch (SystemException) { }
    }

    // ObjectDisposedException derives from InvalidOperationException, so it catches only some of
    // what the helper may return.
    public static void HelperNotCaughtBySubclass()
    {
        try { ThrowsHelperResult(); }
        catch (ObjectDisposedException) { }
    }

    public static string CallsVirtual(Animal a) => a.Speak();

    // C# calls an instance method with callvirt; a non-virtual target is not dispatched.
    public static void CallsNonVirtualViaCallvirt(Sealed s) { s.Boom(); }

    // Cases has no type initializer, so calling into it cannot fail on one.
    public static int CallsLeaf() => Leaf(1);

    public static void CoreLibThrowHelper(object o) { ArgumentNullException.ThrowIfNull(o); }

    // The parameter holds an object of the type it is declared with, or a subtype.
    public static void ThrowsParameter(Exception e) { throw e; }

    // The helper's return type spells its own type variable, but still names the exception's class.
    public static void ThrowsGenericReturned() { throw MakeGeneric<int>(); }
    static GenericFailure<T> MakeGeneric<T>() => new GenericFailure<T>();
}

// A static virtual is dispatched on the type argument: the default body is not what runs for
// ThrowingParse.
public interface IParse
{
    static virtual int Parse() => 1;
}

public sealed class ThrowingParse : IParse
{
    public static int Parse() => throw new FormatException();
}

public static class StaticVirtualCases
{
    public static int CallParse<T>() where T : IParse => T.Parse();
}

// Each instantiation has its own initializer: initializing G<string> runs G<int>'s, which can fail.
public class G<T>
{
    public static int Value = 1 / G<int>.Value;
}

public class LocalBase : Exception { }

public class Derived : LocalBase { }

public class Animal
{
    public virtual string Speak() => "...";
}

public sealed class Sealed
{
    public void Boom() { throw new FormatException(); }
}

public static class Boom
{
    public static readonly int Value = int.Parse("not a number");

    public static int M() => Value;
}

public static class CctorCases
{
    public static int CallsBoom() => Boom.M();
}

public class Shadowed
{
    public int Field;
}

public static class ShadowCases
{
    // A catch for a locally declared System.NullReferenceException must not absorb the one the
    // runtime raises.
    public static int DereferencesNull(Shadowed s)
    {
        try { return s.Field; }
        catch (System.NullReferenceException) { return -1; }
    }
}
"""

    /// Local shadows of CoreLib types: an exception the last case catches, and a class whose FCall
    /// only CoreLib could implement.
    let private shadow =
        """
namespace System;

public class NullReferenceException : Exception { }

public static class MathF
{
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.InternalCall)]
    public static extern float Sin(float x);
}
"""

    /// What a fixture method's answer must hold. A thrown type is written `=T` for `Exactly T` and
    /// `<:T` for `SubtypeOf T`.
    type private Expectation =
        {
            Method : string * string
            Contains : string list
            Excludes : string list
            Unknown : bool option
        }

    let private expect (ty : string) (name : string) : Expectation =
        {
            Method = ty, name
            Contains = []
            Excludes = []
            Unknown = None
        }

    let private expectations : Expectation list =
        let ioe = "=System.InvalidOperationException"

        [
            { expect "Fixture.Cases" "ThrowsDirectly" with
                Contains = [ ioe ]
            }
            { expect "Fixture.Cases" "CaughtExactly" with
                Excludes = [ ioe ]
            }
            { expect "Fixture.Cases" "CaughtByBase" with
                Excludes = [ ioe ]
            }
            { expect "Fixture.Cases" "UnrelatedCatch" with
                Contains = [ ioe ]
            }
            { expect "Fixture.Cases" "FinallyDoesNotCatch" with
                Contains = [ ioe ]
            }
            { expect "Fixture.Cases" "PropagatesOneHop" with
                Contains = [ ioe ]
            }
            { expect "Fixture.Cases" "Synchronised" with
                Contains =
                    [
                        "=System.Threading.ThreadInterruptedException"
                        "=System.Threading.SynchronizationLockException"
                    ]
                // Its monitor is its type's, never null.
                Excludes = [ "=System.ArgumentNullException" ]
            }
            { expect "Fixture.Cases" "ReleasesEarly" with
                Contains = [ "=System.Threading.SynchronizationLockException" ]
            }
            // Reached by `call` on a null receiver, it locks on null.
            { expect "Fixture.Locking" "Instance" with
                Contains =
                    [
                        "=System.ArgumentNullException"
                        "=System.Threading.ThreadInterruptedException"
                        "=System.Threading.SynchronizationLockException"
                    ]
            }
            { expect "Fixture.Cases" "Unsynchronised" with
                Excludes =
                    [
                        "=System.Threading.ThreadInterruptedException"
                        "=System.Threading.SynchronizationLockException"
                        "=System.ArgumentNullException"
                    ]
            }
            { expect "Fixture.Cases" "CatchesInterruption" with
                Excludes = [ "=System.Threading.ThreadInterruptedException" ]
            }
            { expect "Fixture.Cases" "TwoSources" with
                Contains = [ ioe ; "=System.FormatException" ]
            }
            // `catch (Exception)` absorbs everything, including what could not be named.
            { expect "Fixture.Cases" "CatchesBoth" with
                Excludes = [ ioe ; "=System.FormatException" ; "=System.StackOverflowException" ]
                Unknown = Some false
            }
            { expect "Fixture.Cases" "Leaf" with
                Unknown = Some false
            }
            { expect "Fixture.Cases" "Recursive" with
                Contains = [ ioe ]
            }
            // The `throw;` is in the handler, outside the region its catch protects, and rethrows
            // what that catch caught.
            { expect "Fixture.Cases" "Rethrows" with
                Contains = [ ioe ]
            }
            { expect "Fixture.Cases" "RethrowsDivision" with
                Contains = [ "=System.DivideByZeroException" ; "=System.OverflowException" ]
                Unknown = Some false
            }
            { expect "Fixture.Cases" "RethrowsWhatEarlierClausesLeave" with
                Contains = [ "=System.NullReferenceException" ]
                Excludes = [ "=System.IndexOutOfRangeException" ]
                Unknown = Some false
            }
            { expect "Fixture.Cases" "RethrowsTheTypeItCatches" with
                Contains = [ "<:System.InvalidOperationException" ]
                Unknown = Some false
            }
            { expect "Fixture.Cases" "RethrowsAnythingCaughtAsException" with
                Unknown = Some true
            }
            { expect "Fixture.Cases" "RethrowsInnermost" with
                Contains = [ "=System.DivideByZeroException" ]
                Excludes = [ "=System.IndexOutOfRangeException" ]
                Unknown = Some false
            }
            { expect "Fixture.Cases" "RethrowsFromFilteredHandler" with
                Contains = [ "=System.DivideByZeroException" ; "=System.OverflowException" ]
                Unknown = Some false
            }
            { expect "Fixture.Cases" "RethrowsWithinAHandler" with
                Contains = [ "=System.DivideByZeroException" ]
                Unknown = Some false
            }
            { expect "Fixture.Cases" "RethrowsWhatIsRethrown" with
                Contains = [ "=System.DivideByZeroException" ]
                Unknown = Some false
            }
            { expect "Fixture.Cases" "ThrowsNull" with
                Contains = [ "=System.NullReferenceException" ]
                Unknown = Some false
            }
            { expect "Fixture.Cases" "ThrowsLocalDerived" with
                Contains = [ "=Fixture.Derived" ]
            }
            { expect "Fixture.Cases" "CaughtByLocalBase" with
                Excludes = [ "=Fixture.Derived" ]
            }
            { expect "Fixture.Cases" "ThrowsHelperResult" with
                Contains = [ "<:System.InvalidOperationException" ]
            }
            { expect "Fixture.Cases" "HelperCaughtByBase" with
                Excludes = [ "<:System.InvalidOperationException" ]
            }
            { expect "Fixture.Cases" "HelperNotCaughtBySubclass" with
                Contains = [ "<:System.InvalidOperationException" ]
            }
            { expect "Fixture.Cases" "CallsVirtual" with
                Unknown = Some true
            }
            { expect "Fixture.Cases" "CallsNonVirtualViaCallvirt" with
                Contains = [ "=System.FormatException" ]
            }
            { expect "Fixture.Cases" "CallsLeaf" with
                Excludes = [ "=System.TypeInitializationException" ]
            }
            // A value is of the type the IL spells for it.
            { expect "Fixture.Cases" "ThrowsParameter" with
                Contains = [ "<:System.Exception" ]
                Unknown = Some false
            }
            { expect "Fixture.Cases" "ThrowsGenericReturned" with
                Contains = [ "<:Fixture.GenericFailure`1" ]
                Unknown = Some false
            }
            { expect "Fixture.Cases" "CoreLibThrowHelper" with
                Contains = [ "=System.ArgumentNullException" ]
            }
            { expect "Fixture.StaticVirtualCases" "CallParse" with
                Unknown = Some true
            }
            { expect "Fixture.G`1" ".cctor" with
                Contains = [ "=System.TypeInitializationException" ]
            }
            // `Boom`'s initializer fails, and calling `M` runs it first.
            { expect "Fixture.CctorCases" "CallsBoom" with
                Contains = [ "=System.TypeInitializationException" ]
            }
            { expect "Fixture.ShadowCases" "DereferencesNull" with
                Contains = [ "=System.NullReferenceException" ]
            }
            { expect "Fixture.Intrinsics" "Reinterpret" with
                Contains = [ "=System.NullReferenceException" ]
                Unknown = Some false
            }
            { expect "Fixture.Intrinsics" "Fence" with
                Unknown = Some false
            }
            { expect "Fixture.Intrinsics" "Accelerated" with
                Unknown = Some false
            }
            { expect "Fixture.Natives" "Sine" with
                Excludes = [ "=System.NullReferenceException" ]
                Unknown = Some false
            }
            { expect "Fixture.Natives" "Power" with
                Unknown = Some false
            }
            { expect "Fixture.Natives" "Fused" with
                Unknown = Some false
            }
            { expect "Fixture.Natives" "ThreadId" with
                Unknown = Some true
            }
            { expect "Fixture.Natives" "Impostor" with
                Unknown = Some true
            }
        ]

    /// An answer as the expectations spell it: `=T` for `Exactly T`, `<:T` for `SubtypeOf T`.
    let private render (analysis : EscapeAnalysisState) (escapes : Escapes) : Set<string> =
        escapes.Types
        |> Seq.map (fun thrown ->
            match thrown with
            | ThrownType.Exactly ty -> "=" + EscapeAnalysis.typeName analysis ty
            | ThrownType.SubtypeOf ty -> "<:" + EscapeAnalysis.typeName analysis ty
        )
        |> Set.ofSeq

    /// The method of this name on the type of this full name, in `assembly`.
    let private methodNamed (assembly : DumpedAssembly) (typeName : string) (methodName : string) : MethodKey =
        assembly.Methods
        |> Seq.pick (fun (KeyValue (handle, method)) ->
            if
                method.Name = methodName
                && TypeInfo.fullName
                    (fun h -> assembly.TypeDefs.[h])
                    assembly.TypeDefs.[method.RequiredDeclaringType.Definition.Get] = typeName
            then
                Some (MethodKey.make assembly handle)
            else
                None
        )

    /// The architecture the framework under test's JIT compiles for.
    let private hostTarget () : JitTarget =
        match Runtime.InteropServices.RuntimeInformation.ProcessArchitecture with
        | Runtime.InteropServices.Architecture.Arm64 -> JitTarget.Arm64
        | Runtime.InteropServices.Architecture.X64 -> JitTarget.X64
        | other -> failwith $"No JIT table describes the host's architecture, %O{other}"

    /// An analysis over `corelib` and `assemblies`, with `bind` applied to the load context, loading
    /// any other assembly from `runtimeDirs`, for the JIT compiling for `target` on a CPU `profile`
    /// describes.
    let private analysisOf
        (corelib : DumpedAssembly)
        (runtimeDirs : string seq)
        (target : JitTarget)
        (profile : HardwareIntrinsicsProfile)
        (assemblies : DumpedAssembly list)
        (bind : LoadedAssemblies -> LoadedAssemblies)
        : EscapeAnalysisState
        =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let baseClassTypes = BaseClassTypes.ofCorelib corelib
        let loaded = LoadedAssemblies.ofAssemblies (corelib :: assemblies) |> bind

        EscapeAnalysis.create
            loggerFactory
            runtimeDirs
            target
            profile
            {
                ConcreteTypes = Corelib.concretizeAll loaded baseClassTypes AllConcreteTypes.Empty
                LoadedAssemblies = loaded
                BaseTypes = baseClassTypes
            }

    let private hostCoreLib () : DumpedAssembly =
        let _, loggerFactory = LoggerFactory.makeTest ()

        Assembly.readFile
            loggerFactory
            (Path.Combine (FrameworkUnderTest.sharedFrameworkDirectory (), "System.Private.CoreLib.dll"))

    /// An analysis over the host's CoreLib and `assemblies`, with `bind` applied to the load
    /// context, for the host's JIT on a CPU `profile` describes.
    let private analysisUnder
        (profile : HardwareIntrinsicsProfile)
        (assemblies : DumpedAssembly list)
        (bind : LoadedAssemblies -> LoadedAssemblies)
        : EscapeAnalysisState
        =
        analysisOf (hostCoreLib ()) (FrameworkUnderTest.runtimeDirs ()) (hostTarget ()) profile assemblies bind

    /// An analysis over CoreLib and `assemblies`, with `bind` applied to the load context, on a CPU
    /// with no instruction sets.
    let private analysisOver
        (assemblies : DumpedAssembly list)
        (bind : LoadedAssemblies -> LoadedAssemblies)
        : EscapeAnalysisState
        =
        analysisUnder HardwareIntrinsicsProfile.ScalarOnly assemblies bind

    /// How `analysis`'s answers for `fixture` fail `expectations`, and the analysis to ask next.
    let private unmet
        (analysis : EscapeAnalysisState)
        (fixture : DumpedAssembly)
        (expectations : Expectation list)
        : EscapeAnalysisState * string list
        =
        ((analysis, []), expectations)
        ||> List.fold (fun (analysis, failures) expectation ->
            let analysis, escapes =
                EscapeAnalysis.escapes analysis (methodNamed fixture (fst expectation.Method) (snd expectation.Method))

            let shown = render analysis escapes

            let describe () =
                let ty, name = expectation.Method
                $"%s{ty}::%s{name}: %A{Set.toList shown}, unknown %b{escapes.Unknown}"

            let failures =
                [
                    yield! failures

                    for wanted in expectation.Contains do
                        if not (shown.Contains wanted) then
                            yield $"%s{describe ()} lacks %s{wanted}"

                    for unwanted in expectation.Excludes do
                        if shown.Contains unwanted then
                            yield $"%s{describe ()} has %s{unwanted}"

                    match expectation.Unknown with
                    | Some unknown when unknown <> escapes.Unknown ->
                        yield $"%s{describe ()}, expected unknown %b{unknown}"
                    | _ -> ()
                ]

            analysis, failures
        )

    [<Test>]
    let ``each fixture method's escaping exceptions are as stated`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let image =
            Roslyn.compileAssembly "EscapeFixture" OutputKind.DynamicallyLinkedLibrary [] [ source ; shadow ]

        let fixture =
            Assembly.read loggerFactory (Some "EscapeFixture.dll") (new MemoryStream (image))

        match unmet (analysisOver [ fixture ] id) fixture expectations with
        | _, [] -> ()
        | _, failures -> failures |> String.concat Environment.NewLine |> failwith

    /// Code a capability query guards, compiled optimized as CoreLib is, so that the query's result
    /// is branched on directly.
    let private guardedSource =
        """
using System;
using System.Runtime.Intrinsics;

namespace Guarded;

// Exceptions whose construction raises nothing, unlike CoreLib's, which look up their messages.
public class Guarded : Exception { }

public class HandlerRan : Exception { }

public static class Cases
{
    public static bool Flag;

    // `call get_IsHardwareAccelerated; brfalse`.
    public static void WhenAccelerated()
    {
        if (Vector128.IsHardwareAccelerated) throw new Guarded();
    }

    // `call get_IsHardwareAccelerated; brtrue`.
    public static void UnlessAccelerated()
    {
        if (!Vector128.IsHardwareAccelerated) throw new Guarded();
    }

    // The query's result is compared, not branched on.
    public static void ComparedWithField()
    {
        if (Vector128.IsHardwareAccelerated == Flag) throw new Guarded();
    }

    // The throw is reached past the query as well as through it.
    public static void AlsoReachedOtherwise()
    {
        if (Flag ? Vector128.IsHardwareAccelerated : true) throw new Guarded();
    }

    public static void HandlerOfGuardedTry()
    {
        if (Vector128.IsHardwareAccelerated)
        {
            try { WhenAccelerated(); }
            catch (Guarded) { throw new HandlerRan(); }
        }
    }

    public static void GuardInHandler()
    {
        try { UnlessAccelerated(); }
        catch (Guarded) { if (!Vector128.IsHardwareAccelerated) throw new HandlerRan(); }
    }
}
"""

    /// What each guarded method lets escape on a CPU that accelerates `Vector128` and on one that
    /// does not.
    let private guardedExpectations : (bool * Expectation) list =
        let guarded = "=Guarded.Guarded"
        let handlerRan = "=Guarded.HandlerRan"

        [
            false,
            { expect "Guarded.Cases" "WhenAccelerated" with
                Excludes = [ guarded ]
                Unknown = Some false
            }
            true,
            { expect "Guarded.Cases" "WhenAccelerated" with
                Contains = [ guarded ]
                Unknown = Some false
            }
            false,
            { expect "Guarded.Cases" "UnlessAccelerated" with
                Contains = [ guarded ]
                Unknown = Some false
            }
            true,
            { expect "Guarded.Cases" "UnlessAccelerated" with
                Excludes = [ guarded ]
                Unknown = Some false
            }
            for accelerated in [ false ; true ] do
                accelerated,
                { expect "Guarded.Cases" "ComparedWithField" with
                    Contains = [ guarded ]
                    Unknown = Some false
                }

                accelerated,
                { expect "Guarded.Cases" "AlsoReachedOtherwise" with
                    Contains = [ guarded ]
                    Unknown = Some false
                }
            // The handler's protected block runs only on an accelerating CPU.
            false,
            { expect "Guarded.Cases" "HandlerOfGuardedTry" with
                Excludes = [ guarded ; handlerRan ]
                Unknown = Some false
            }
            true,
            { expect "Guarded.Cases" "HandlerOfGuardedTry" with
                Contains = [ handlerRan ]
                Excludes = [ guarded ]
                Unknown = Some false
            }
            false,
            { expect "Guarded.Cases" "GuardInHandler" with
                Contains = [ handlerRan ]
                Excludes = [ guarded ]
                Unknown = Some false
            }
            true,
            { expect "Guarded.Cases" "GuardInHandler" with
                Excludes = [ guarded ; handlerRan ]
                Unknown = Some false
            }
        ]

    [<Test>]
    let ``code a capability query guards is analysed only on a CPU it runs on`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let image =
            Roslyn.compileOptimizedAssembly "GuardedFixture" OutputKind.DynamicallyLinkedLibrary [ guardedSource ]

        let fixture =
            Assembly.read loggerFactory (Some "GuardedFixture.dll") (new MemoryStream (image))

        let failures =
            [
                for accelerated in [ false ; true ] do
                    let profile =
                        if accelerated then
                            { HardwareIntrinsicsProfile.ScalarOnly with
                                IsHardwareAccelerated =
                                    Set.singleton
                                        {
                                            Namespace = "System.Runtime.Intrinsics"
                                            Path = [ "Vector128" ]
                                        }
                            }
                        else
                            HardwareIntrinsicsProfile.ScalarOnly

                    let expectations =
                        guardedExpectations
                        |> List.filter (fun (on, _) -> on = accelerated)
                        |> List.map snd

                    let _, failures = unmet (analysisUnder profile [ fixture ] id) fixture expectations

                    for failure in failures do
                        yield $"accelerated %b{accelerated}: %s{failure}"
            ]

        if not failures.IsEmpty then
            failures |> String.concat Environment.NewLine |> failwith

    /// The CoreLibs the CoreLib-wide tests read, the host's and the pinned linux-x64 one, each with
    /// the CPUs they answer for: one with no instruction sets, and one with every instruction set
    /// whose class in that CoreLib is a placeholder.
    let coreLibsAndProfiles : TestCaseData list =
        [
            for coreLib in [ "host" ; "linux-x64" ] do
                for profile in [ "scalar-only" ; "every instruction set" ] do
                    TestCaseData(coreLib, profile).SetArgDisplayNames ($"%s{coreLib} CoreLib", $"%s{profile} CPU")
        ]

    /// A CoreLib the CoreLib-wide tests read, where to load the rest of its framework from, and the
    /// JIT target it was built for.
    let private coreLibNamed (name : string) : DumpedAssembly * string list * JitTarget =
        match name with
        | "host" -> hostCoreLib (), List.ofSeq (FrameworkUnderTest.runtimeDirs ()), hostTarget ()
        | "linux-x64" ->
            match Environment.GetEnvironmentVariable "DOTNET_LINUX_FRAMEWORK_DIR" with
            | null
            | "" ->
                Assert.Ignore "DOTNET_LINUX_FRAMEWORK_DIR is unset; run inside the Nix devshell"
                failwith "unreachable: Assert.Ignore did not throw"
            | dir ->
                let _, loggerFactory = LoggerFactory.makeTest ()

                Assembly.readFile loggerFactory (Path.Combine (dir, "System.Private.CoreLib.dll")),
                [ dir ],
                JitTarget.X64
        | other -> failwith $"unknown CoreLib %s{other}"

    /// Whether `method`'s IL calls, constructs and throws nothing but through its calls to itself, so
    /// that whatever else it raises is what its instructions raise by themselves.
    let private callsOnlyItself (corelib : DumpedAssembly) (method : MethodDefinitionHandle) : bool =
        match corelib.Methods.[method].Body with
        | MethodBody.Il body ->
            body.Instructions
            |> List.forall (fun (op, _) ->
                match op with
                | IlOp.UnaryMetadataToken ((UnaryMetadataTokenIlOp.Call | UnaryMetadataTokenIlOp.Callvirt | UnaryMetadataTokenIlOp.Newobj | UnaryMetadataTokenIlOp.Calli | UnaryMetadataTokenIlOp.Jmp | UnaryMetadataTokenIlOp.Ldftn | UnaryMetadataTokenIlOp.Ldvirtftn),
                                           operand) ->
                    match operand with
                    | MetadataOperand.FromMetadata token -> token.Token = MetadataToken.MethodDef method
                    | _ -> false
                | IlOp.Nullary NullaryIlOp.Throw
                | IlOp.Nullary NullaryIlOp.Rethrow -> false
                | _ -> true
            )
        | _ -> false

    let private profileNamed (corelib : DumpedAssembly) (name : string) : HardwareIntrinsicsProfile =
        match name with
        | "scalar-only" -> HardwareIntrinsicsProfile.ScalarOnly
        | "every instruction set" ->
            let expansions =
                [
                    for KeyValue (handle, _) in corelib.Methods do
                        if IntrinsicBody.isIntrinsic corelib handle then
                            match IntrinsicBody.classify corelib handle with
                            | IntrinsicBody.JitExpansion expansion -> yield expansion
                            | _ -> ()
                ]

            {
                IsSupported =
                    expansions
                    |> List.choose (fun expansion ->
                        match expansion with
                        | JitExpansion.IsSupportedQuery c
                        | JitExpansion.HardwareInstruction c -> Some c
                        | _ -> None
                    )
                    |> Set.ofList
                IsHardwareAccelerated =
                    expansions
                    |> List.choose (fun expansion ->
                        match expansion with
                        | JitExpansion.IsHardwareAcceleratedQuery c -> Some c
                        | _ -> None
                    )
                    |> Set.ofList
            }
        | other -> failwith $"unknown profile %s{other}"

    /// Every intrinsic in CoreLib, answered from what CoreCLR runs for it: the IL its VM substitutes
    /// where there is some, and otherwise its own IL with its call to itself performed as the JIT
    /// expands it for the CPU: a capability query answers a constant, an instruction the CPU lacks
    /// throws `PlatformNotSupportedException`, and one it has raises what the JIT's tables say.
    [<TestCaseSource(nameof coreLibsAndProfiles)>]
    let ``each CoreLib intrinsic is analysed as what CoreCLR runs for it``
        (coreLibName : string)
        (profileName : string)
        : unit
        =
        let corelib, runtimeDirs, target = coreLibNamed coreLibName
        let profile = profileNamed corelib profileName
        let mutable analysis = analysisOf corelib runtimeDirs target profile [] id
        let failures = ResizeArray<string> ()
        let mutable substituted = 0
        let mutable primitives = 0
        let mutable placeholders = 0
        let mutable constants = 0
        let mutable unsupported = 0
        let mutable instructions = 0
        let mutable bareInstructions = 0

        let rangeHelperEscapes, rangeHelperShown =
            let next, escapes =
                EscapeAnalysis.escapes
                    analysis
                    (MethodKey.make corelib (IntrinsicBody.argumentOutOfRangeHelper corelib))

            analysis <- next
            let shown = render analysis escapes

            // The helper constructs and throws the exception, and constructing it can exhaust memory.
            for wanted in [ "=System.ArgumentOutOfRangeException" ; "=System.OutOfMemoryException" ] do
                shown |> shouldContain wanted

            escapes, shown

        let instructionFaultName (fault : InstructionFault) : string =
            match fault with
            | InstructionFault.NullAddress -> "=System.NullReferenceException"
            | InstructionFault.ImmediateOutOfRange -> "=System.ArgumentOutOfRangeException"
            | InstructionFault.ZeroDivisor -> "=System.DivideByZeroException"
            | InstructionFault.QuotientOverflow -> "=System.OverflowException"

        let mutable wholePrimitives = 0

        let faultName (fault : PrimitiveFault) : string =
            match fault with
            | PrimitiveFault.NullReference -> "=System.NullReferenceException"
            | PrimitiveFault.DataMisaligned -> "=System.DataMisalignedException"

        for KeyValue (handle, method) in corelib.Methods do
            if IntrinsicBody.isIntrinsic corelib handle then
                let next, escapes = EscapeAnalysis.escapes analysis (MethodKey.make corelib handle)
                analysis <- next
                let shown = render analysis escapes

                let describe () =
                    $"%s{method.RequiredDeclaringType.Name}::%s{method.Name}: %A{Set.toList shown}, unknown %b{escapes.Unknown}"

                match VmSubstitution.unsafeStub corelib handle, IntrinsicBody.classify corelib handle with
                | Some _, _ ->
                    substituted <- substituted + 1

                    if escapes.Unknown then
                        failures.Add $"%s{describe ()}: the VM's IL for it is all there is to see"
                | None, IntrinsicBody.JitExpansion (JitExpansion.Primitive primitive) ->
                    primitives <- primitives + 1

                    for fault, _ in (IntrinsicPrimitive.contract primitive).Raises do
                        if not (shown.Contains (faultName fault)) then
                            failures.Add $"%s{describe ()} lacks %s{faultName fault}, which %A{primitive} can raise"
                | None, IntrinsicBody.VmSubstitution ->
                    // The VM chooses the substitute by instantiation; where it is a primitive, that
                    // is the whole method.
                    match IntrinsicPrimitive.recognise corelib handle with
                    | Some primitive ->
                        wholePrimitives <- wholePrimitives + 1

                        let wanted =
                            (IntrinsicPrimitive.contract primitive).Raises
                            |> List.map (fst >> faultName)
                            |> Set.ofList

                        if escapes.Unknown || shown <> wanted then
                            failures.Add $"%s{describe ()}: %A{primitive} raises exactly %A{Set.toList wanted}"
                    | None -> ()
                | None, IntrinsicBody.JitExpansion expansion ->
                    placeholders <- placeholders + 1

                    match IntrinsicBody.expandSelfCall profile expansion with
                    | SelfCallExpansion.Constant _ ->
                        constants <- constants + 1

                        if escapes.Unknown then
                            failures.Add $"%s{describe ()}: a capability query is a constant"
                    | SelfCallExpansion.ThrowPlatformNotSupported ->
                        unsupported <- unsupported + 1

                        // The JIT calls CoreLib's throw helper, whose `newobj` can exhaust memory.
                        for wanted in [ "=System.PlatformNotSupportedException" ; "=System.OutOfMemoryException" ] do
                            if not (shown.Contains wanted) then
                                failures.Add $"%s{describe ()} lacks %s{wanted}: the CPU lacks the instruction"
                    | SelfCallExpansion.HardwareInstruction intrinsicClass ->
                        match HardwareInstruction.contract target intrinsicClass method.Name with
                        | InstructionContract.Unknown ->
                            if not escapes.Unknown then
                                failures.Add $"%s{describe ()}: the JIT's tables do not say what it raises"
                        | InstructionContract.Raises faults ->
                            instructions <- instructions + 1

                            // An out-of-range immediate is thrown by a call to CoreLib's helper,
                            // so the instruction raises what the helper does.
                            let viaHelper = faults.Contains InstructionFault.ImmediateOutOfRange

                            // A body that does more than call itself, such as `Avx2.GatherVector128`
                            // constructing the exception for a `scale` it rejects, may be unknown
                            // for that.
                            if callsOnlyItself corelib handle then
                                bareInstructions <- bareInstructions + 1
                                let expected = viaHelper && rangeHelperEscapes.Unknown

                                if escapes.Unknown <> expected then
                                    failures.Add
                                        $"%s{describe ()}: the JIT's tables say it raises %A{faults}, so unknown should be %b{expected}"

                            let wanted =
                                faults
                                |> Seq.filter (fun fault -> fault <> InstructionFault.ImmediateOutOfRange)
                                |> Seq.map instructionFaultName
                                |> Set.ofSeq
                                |> Set.union (if viaHelper then rangeHelperShown else Set.empty)

                            for wanted in wanted do
                                if not (shown.Contains wanted) then
                                    failures.Add $"%s{describe ()} lacks %s{wanted}"
                    // Answered by the branch above.
                    | SelfCallExpansion.Primitive _ -> ()
                    | SelfCallExpansion.Unrecognised ->
                        if not escapes.Unknown then
                            failures.Add $"%s{describe ()}: what the JIT emits is not known"
                | None, _ -> ()

        if failures.Count > 0 then
            failures |> Seq.truncate 40 |> String.concat Environment.NewLine |> failwith

        substituted |> shouldBeGreaterThan 20
        primitives |> shouldBeGreaterThan 10
        placeholders |> shouldBeGreaterThan 20
        wholePrimitives |> shouldBeGreaterThan 0
        constants |> shouldBeGreaterThan 20

        match profileName with
        | "scalar-only" ->
            unsupported |> shouldBeGreaterThan 1000
            instructions |> shouldEqual 0
        | _ ->
            unsupported |> shouldEqual 0
            instructions |> shouldBeGreaterThan 1000
            bareInstructions |> shouldBeGreaterThan 1000

    [<Test>]
    let ``a profile supporting another architecture's instruction set is refused`` () : unit =
        let foreign = JitTarget.all |> List.find (fun target -> target <> hostTarget ())

        let profile =
            { HardwareIntrinsicsProfile.ScalarOnly with
                IsSupported =
                    Set.singleton
                        {
                            Namespace = JitTarget.instructionSetNamespace foreign
                            Path = [ "Aes" ]
                        }
            }

        Assert.Throws<ArgumentException> (fun () -> analysisUnder profile [] id |> ignore<EscapeAnalysisState>)
        |> ignore<ArgumentException>

    /// A client compiled against one version of a provider, run against another that lacks what it
    /// uses. The JIT binds a body's tokens before the body runs, so the failure comes out of the
    /// caller and the client's own `catch` cannot stop it.
    [<Test>]
    let ``what the provider no longer has fails to bind, past the method's own handlers`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let version1 =
            """
namespace Provider;

public class Parent
{
    public static int Value = 1;
    public static void Gone() { }
    public static void Varargs(__arglist) { }
}

public class GoneType { }

public class GoneException : System.Exception { }

public struct GoneStruct { }
"""

        let version2 =
            """
namespace Provider;

public class Parent
{
    public static void Varargs(__arglist) { }
}
"""

        let client =
            """
namespace Client;

public static class Uses
{
    public static int Read() => Provider.Parent.Value;
    public static void CallGone() { Provider.Parent.Gone(); }
    public static void CaughtCallGone()
    {
        try { Provider.Parent.Gone(); }
        catch (System.MissingMethodException) { }
    }
    public static object UseGoneType() => new Provider.GoneType();
    public static bool IsGone(object o) => o is Provider.GoneType;
    public static object ListOfGone() => new System.Collections.Generic.List<Provider.GoneType>();
    public static bool LocalOfGone()
    {
        Provider.GoneType x = null;
        return x != null;
    }
    public static string VirtualOnLocalOfGone()
    {
        Provider.GoneType x = null;
        return x.ToString();
    }
    public static int CatchGone(int x)
    {
        try { return 1 / x; }
        catch (Provider.GoneException) { return 42; }
    }
    static void Accept(Provider.GoneType x) { }
    public static void PassGone() { Accept(null); }
    public static unsafe int CallGoneIndirectly(delegate*<Provider.GoneStruct> f)
    {
        try { f(); return 0; }
        catch (System.Exception) { return 1; }
    }
    public static void PassGoneAsVararg() { Provider.Parent.Varargs(__arglist((Provider.GoneType)null)); }
    public static bool CaughtLocalOfGone()
    {
        try
        {
            Provider.GoneType x = null;
            return x != null;
        }
        catch (System.TypeLoadException) { return false; }
    }
    public interface IRuns { int Run(); }
    public struct UsesGone : IRuns
    {
        public int Run()
        {
            Provider.GoneType x = null;
            return x == null ? 0 : 1;
        }
    }
    // The JIT binds no function pointer's signature, so this runs without `GoneType`.
    public unsafe struct UsesGonePointer : IRuns
    {
        public int Run()
        {
            delegate*<Provider.GoneType> f = null;
            return f == null ? 0 : 1;
        }
    }
    // Only a method nothing calls names `GoneType`, so the JIT never reads it.
    public struct HelperUsesGone : IRuns
    {
        public int Run() => 0;
        static bool Helper()
        {
            Provider.GoneType x = null;
            return x == null;
        }
    }
    // The method the call names has a default body using `GoneType`, but the receiver implements
    // the method itself, so that body never runs.
    public interface INamesGone
    {
        int Go()
        {
            Provider.GoneType x = null;
            return x == null ? 0 : 1;
        }
    }
    public struct OverridesGone : INamesGone { public int Go() => 0; }
    static int ThroughNamesGone<T>(T x) where T : INamesGone => x.Go();
    static void Generic<T>() { }
    public static void InstantiateWithGone() { Generic<Provider.GoneType>(); }
    static int Through<T>(T x) where T : IRuns => x.Run();
    public static int ConstrainedReachesGoneLocal() => Through(new UsesGone());
    public static int ConstrainedReachesGonePointer() => Through(new UsesGonePointer());
    public static int ConstrainedBesideGoneHelper() => Through(new HelperUsesGone());
    public static int ConstrainedPastGoneOverridden() => ThroughNamesGone(new OverridesGone());
    public static int ConstrainedOnGone()
    {
        var x = new Provider.GoneStruct();
        return x.GetHashCode();
    }
}
"""

        let compile (name : string) (references : byte[] list) (text : string) : byte[] =
            Roslyn.compileAssembly
                name
                OutputKind.DynamicallyLinkedLibrary
                (references
                 |> List.map (fun image -> MetadataReference.CreateFromImage (ImmutableArray.CreateRange image)))
                [ text ]

        let read (name : string) (image : byte[]) : DumpedAssembly =
            Assembly.read loggerFactory (Some $"%s{name}.dll") (new MemoryStream (image))

        let providerImage1 = compile "Provider" [] version1
        let clientAssembly = read "Client" (compile "Client" [ providerImage1 ] client)

        let answers (provider : DumpedAssembly) : string -> Set<string> * bool =
            let providerReference =
                clientAssembly.AssemblyReferences.Values
                |> Seq.find (fun r -> r.Name.Name = "Provider")

            let mutable analysis =
                analysisOver
                    [ clientAssembly ; provider ]
                    (fun loaded -> fst (loaded.WithBoundReference providerReference provider))

            fun methodName ->
                let next, escapes =
                    EscapeAnalysis.escapes analysis (methodNamed clientAssembly "Client.Uses" methodName)

                analysis <- next
                render analysis escapes, escapes.Unknown

        let against1 = answers (read "Provider" providerImage1)
        let against2 = answers (read "Provider" (compile "Provider" [] version2))

        for methodName, failure in
            [
                "Read", "=System.MissingFieldException"
                "CallGone", "=System.MissingMethodException"
                "CaughtCallGone", "=System.MissingMethodException"
                "UseGoneType", "=System.TypeLoadException"
                // A type token of its own.
                "IsGone", "=System.TypeLoadException"
                // A type argument of the member's parent, whose definition does resolve.
                "ListOfGone", "=System.TypeLoadException"
                // Named by no instruction, only by the type of a local.
                "LocalOfGone", "=System.TypeLoadException"
                // The type of a local a `callvirt` is made on.
                "VirtualOnLocalOfGone", "=System.TypeLoadException"
                "CaughtLocalOfGone", "=System.TypeLoadException"
                // Named only by a `catch` clause.
                "CatchGone", "=System.TypeLoadException"
                // Named only by the signature of the method called.
                "PassGone", "=System.TypeLoadException"
                // Named only by a vararg call site's extra arguments, which no definition declares.
                "PassGoneAsVararg", "=System.TypeLoadException"
                // Named only by an indirect call's signature.
                "CallGoneIndirectly", "=System.TypeLoadException"
                // A type argument of a generic method called.
                "InstantiateWithGone", "=System.TypeLoadException"
                // The type a `constrained.` prefix names.
                "ConstrainedOnGone", "=System.TypeLoadException"
            ] do
            let bound, _ = against1 methodName
            let unbound, _ = against2 methodName

            if bound.Contains failure then
                failwith $"%s{methodName} against the provider it was compiled against: %A{Set.toList bound}"

            if not (unbound.Contains failure) then
                failwith $"%s{methodName} against the provider lacking what it uses: %A{Set.toList unbound}"

        // With no provider at all, binding any token naming it fails to find the assembly, and the
        // real runtime raises `FileNotFoundException`.
        let againstNone =
            let mutable analysis = analysisOver [ clientAssembly ] id

            fun methodName ->
                let next, escapes =
                    EscapeAnalysis.escapes analysis (methodNamed clientAssembly "Client.Uses" methodName)

                analysis <- next
                render analysis escapes

        let unmet =
            [
                "Read"
                "CallGone"
                "CaughtCallGone"
                "UseGoneType"
                "IsGone"
                "ListOfGone"
                "LocalOfGone"
                "VirtualOnLocalOfGone"
                "CaughtLocalOfGone"
                "CatchGone"
                "PassGone"
                "PassGoneAsVararg"
                "CallGoneIndirectly"
            ]
            |> List.choose (fun methodName ->
                let shown = againstNone methodName

                if shown.Contains "=System.IO.FileNotFoundException" then
                    None
                else
                    Some $"%s{methodName} with no provider: %A{Set.toList shown}"
            )

        if not unmet.IsEmpty then
            failwith (String.concat "\n" unmet)

        // A `constrained.` call reaching a method whose local's type is gone: the JIT throws
        // compiling that method, out of the call, where the caller's handlers see it.
        match against2 "ConstrainedReachesGoneLocal" with
        | shown, false when shown.Contains "=System.TypeLoadException" -> ()
        | shown, unknown ->
            failwith
                $"ConstrainedReachesGoneLocal against the provider lacking its local's type: %A{Set.toList shown}, unknown %b{unknown}"

        // A method the call does not reach names the type, so nothing fails.
        match against2 "ConstrainedBesideGoneHelper" with
        | shown, false when not (shown.Contains "=System.TypeLoadException") -> ()
        | shown, unknown ->
            failwith
                $"ConstrainedBesideGoneHelper against the provider lacking its helper's local's type: %A{Set.toList shown}, unknown %b{unknown}"

        // The method the call names is not the one that runs, so nothing fails; the analysis may
        // not decide the call, but must not say it fails.
        match against2 "ConstrainedPastGoneOverridden" with
        | shown, _ when not (shown.Contains "=System.TypeLoadException") -> ()
        | shown, unknown ->
            failwith
                $"ConstrainedPastGoneOverridden against the provider lacking the named method's local's type: %A{Set.toList shown}, unknown %b{unknown}"

        // Answering at all is the claim: it is a summary rather than a crash.
        against2 "ConstrainedReachesGonePointer" |> ignore<Set<string> * bool>

    /// Methods spelling a class whose base class is in an assembly nothing has loaded yet, as
    /// `this` or in a type token: the analysis loads it when it needs the base chain, as it loads
    /// any other assembly.
    [<Test>]
    let ``a class whose base is in an assembly not yet loaded is spelled by loading it`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let source =
            """
namespace Derives;

public class Child : System.ComponentModel.Component
{
    public int Divide(int a, int b) => a / b;
}

public static class Spells
{
    public static bool Check(object x) => x is Child;
    public static Child Cast(object x) => (Child)x;
    public static object Make() => new Child();
}
"""

        let image =
            Roslyn.compileAssembly "Derives" OutputKind.DynamicallyLinkedLibrary [] [ source ]

        let fixture =
            Assembly.read loggerFactory (Some "Derives.dll") (new MemoryStream (image))

        // Each asked of an analysis of its own, which has loaded nothing for another.
        let analysis, escapes =
            EscapeAnalysis.escapes (analysisOver [ fixture ] id) (methodNamed fixture "Derives.Child" "Divide")

        let shown = render analysis escapes

        if escapes.Unknown || not (shown.Contains "=System.DivideByZeroException") then
            failwith $"Child.Divide: %A{Set.toList shown}, unknown %b{escapes.Unknown}"

        // Answering at all is the claim: it is a summary rather than a crash.
        for name in [ "Check" ; "Cast" ; "Make" ] do
            EscapeAnalysis.escapes (analysisOver [ fixture ] id) (methodNamed fixture "Derives.Spells" name)
            |> ignore<EscapeAnalysisState * Escapes>

    /// A client whose struct has a method, called by nothing, using a type from an assembly that is
    /// not present at all. The JIT never reads that method, so a call on the struct runs.
    [<Test>]
    let ``a constrained call is resolved beside a method using an assembly that is missing`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let optional = "namespace Optional; public class X { }"

        let client =
            """
namespace Client;

public interface IRuns { int Run(int n); }

public struct UsesOptional : IRuns
{
    public int Run(int n) => 1 / n;
    static bool Helper()
    {
        Optional.X x = null;
        return x == null;
    }
}

public static class Uses
{
    static int Through<T>(T x, int n) where T : IRuns => x.Run(n);
    public static int Go(int n) => Through(new UsesOptional(), n);
}
"""

        let optionalImage =
            Roslyn.compileAssembly "Optional" OutputKind.DynamicallyLinkedLibrary [] [ optional ]

        let clientAssembly =
            Roslyn.compileAssembly
                "Client"
                OutputKind.DynamicallyLinkedLibrary
                [ MetadataReference.CreateFromImage (ImmutableArray.CreateRange optionalImage) ]
                [ client ]
            |> fun image -> Assembly.read loggerFactory (Some "Client.dll") (new MemoryStream (image))

        let analysis = analysisOver [ clientAssembly ] id

        let analysis, escapes =
            EscapeAnalysis.escapes analysis (methodNamed clientAssembly "Client.Uses" "Go")

        let shown = render analysis escapes

        if escapes.Unknown || not (shown.Contains "=System.DivideByZeroException") then
            failwith $"Go: %A{Set.toList shown}, unknown %b{escapes.Unknown}"

    [<Test>]
    let ``a catch absorbs an exception however deep its base chain`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let depth = 300

        let classes =
            [ 1..depth ]
            |> List.map (fun i ->
                let parent = if i = 1 then "System.Exception" else $"E%d{i - 1}"
                $"public class E%d{i} : %s{parent} {{ }}"
            )
            |> String.concat "\n"

        let source =
            $"""
namespace Deep;

%s{classes}

public static class Throws
{{
    public static void Caught()
    {{
        try {{ throw new E%d{depth}(); }}
        catch (System.Exception) {{ }}
    }}
}}
"""

        let image =
            Roslyn.compileAssembly "Deep" OutputKind.DynamicallyLinkedLibrary [] [ source ]

        let assembly =
            Assembly.read loggerFactory (Some "Deep.dll") (new MemoryStream (image))

        let analysis = analysisOver [ assembly ] id

        let analysis, escapes =
            EscapeAnalysis.escapes analysis (methodNamed assembly "Deep.Throws" "Caught")

        render analysis escapes |> shouldNotContain $"=Deep.E%d{depth}"

    [<Test>]
    let ``a module initializer runs when another module first binds to what it declares`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let provider =
            """
namespace Provider;

static class Init
{
    [System.Runtime.CompilerServices.ModuleInitializer]
    internal static void Run() => throw new System.InvalidOperationException();
}

public static class P
{
    public static int Field;
    public static void M() { }
    public static void CallsM() { M(); }
}

public class Base { }

public class Foreign { }
"""

        let client =
            """
namespace Client;

public class Derived : Provider.Base
{
    public static void Static() { }
}

public class Local<T> { }

public class DerivedOfForeign : Local<Provider.Foreign>
{
    public static void Static() { }
}

public static class Uses
{
    public static void CallM() { Provider.P.M(); }
    public static int CaughtCallM()
    {
        try { Provider.P.M(); return 0; }
        catch (System.TypeInitializationException) { return 1; }
    }
    public static int ReadField() => Provider.P.Field;
    public static object NewDerived() => new Derived();
    public static System.Type TypeOfDerived() => typeof(Derived);
    public static void CallDerivedStatic() { Derived.Static(); }
    public static void CallDerivedOfForeignStatic() { DerivedOfForeign.Static(); }
    public static int Local() => 1;
}
"""

        let compile (name : string) (references : byte[] list) (text : string) : byte[] =
            Roslyn.compileAssembly
                name
                OutputKind.DynamicallyLinkedLibrary
                (references
                 |> List.map (fun image -> MetadataReference.CreateFromImage (ImmutableArray.CreateRange image)))
                [ text ]

        let providerImage = compile "Provider" [] provider
        let clientImage = compile "Client" [ providerImage ] client

        let read (name : string) (image : byte[]) : DumpedAssembly =
            Assembly.read loggerFactory (Some $"%s{name}.dll") (new MemoryStream (image))

        let providerAssembly = read "Provider" providerImage
        let clientAssembly = read "Client" clientImage

        let providerReference =
            clientAssembly.AssemblyReferences.Values
            |> Seq.find (fun r -> r.Name.Name = "Provider")

        let mutable analysis =
            analysisOver
                [ clientAssembly ; providerAssembly ]
                (fun loaded -> fst (loaded.WithBoundReference providerReference providerAssembly))

        let reportsInitialisation (assembly : DumpedAssembly) (typeName : string) (methodName : string) : bool =
            let next, escapes =
                EscapeAnalysis.escapes analysis (methodNamed assembly typeName methodName)

            analysis <- next
            render analysis escapes |> Set.contains "=System.TypeInitializationException"

        // Each in a context of its own, since a module initializer that failed fails every later
        // binding the same way.
        let escapesOnRealRuntime (methodName : string) : bool =
            let context =
                new TestMethodReferenceResolution.ImagesContext (
                    Map.ofList [ "Provider", providerImage ; "Client", clientImage ]
                )

            try
                let uses = context.LoadFromAssemblyName(AssemblyName "Client").GetType "Client.Uses"

                try
                    uses.GetMethod(methodName).Invoke ((null : obj), Array.empty<obj>)
                    |> ignore<obj>

                    false
                with :? TargetInvocationException ->
                    true
            finally
                context.Unload ()

        for methodName in
            [
                "CallM"
                "CaughtCallM"
                "ReadField"
                "NewDerived"
                // Provider is reached only through Derived's base type.
                "TypeOfDerived"
                "CallDerivedStatic"
                // Provider is reached only through an argument of that base type.
                "CallDerivedOfForeignStatic"
            ] do
            escapesOnRealRuntime methodName |> shouldEqual true

            if not (reportsInitialisation clientAssembly "Client.Uses" methodName) then
                failwith $"%s{methodName} binds into Provider, whose module initializer throws"

        escapesOnRealRuntime "Local" |> shouldEqual false
        reportsInitialisation clientAssembly "Client.Uses" "Local" |> shouldEqual false
        // A module's own code runs only once its initializer has.
        reportsInitialisation providerAssembly "Provider.P" "CallsM"
        |> shouldEqual false

    /// `int32[,]`'s constructor taking lower bounds and lengths raises `ArgumentOutOfRangeException`
    /// for bounds whose upper end overflows; the one taking lengths alone does not. C# spells
    /// neither, so the calls are emitted directly.
    [<Test>]
    let ``a multidimensional array constructor taking lower bounds can reject them`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let metadata = MetadataBuilder ()
        let ilStream = BlobBuilder ()
        let bodies = MethodBodyStreamEncoder ilStream

        metadata.AddModule (
            0,
            metadata.GetOrAddString "ArrayCtors.dll",
            metadata.GetOrAddGuid (Guid "3c9d1b2a-4e5f-4a6b-9c8d-7e6f5a4b3c2d"),
            Unchecked.defaultof<GuidHandle>,
            Unchecked.defaultof<GuidHandle>
        )
        |> ignore<ModuleDefinitionHandle>

        metadata.AddAssembly (
            metadata.GetOrAddString "ArrayCtors",
            Version (1, 0, 0, 0),
            Unchecked.defaultof<StringHandle>,
            Unchecked.defaultof<BlobHandle>,
            Unchecked.defaultof<AssemblyFlags>,
            AssemblyHashAlgorithm.None
        )
        |> ignore<AssemblyDefinitionHandle>

        let corelibName = typeof<obj>.Assembly.GetName ()

        let corelibRef =
            metadata.AddAssemblyReference (
                metadata.GetOrAddString corelibName.Name,
                corelibName.Version,
                Unchecked.defaultof<StringHandle>,
                metadata.GetOrAddBlob (corelibName.GetPublicKeyToken ()),
                Unchecked.defaultof<AssemblyFlags>,
                Unchecked.defaultof<BlobHandle>
            )

        let objectRef =
            metadata.AddTypeReference (
                (AssemblyReferenceHandle.op_Implicit corelibRef : EntityHandle),
                metadata.GetOrAddString "System",
                metadata.GetOrAddString "Object"
            )

        let rankTwo =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .TypeSpecificationSignature()
                .Array (
                    (fun element -> element.Int32 ()),
                    (fun shape -> shape.Shape (2, ImmutableArray.Empty, ImmutableArray.Create (0, 0)))
                )

            metadata.AddTypeSpecification (metadata.GetOrAddBlob blob)

        let constructor (arity : int) : MemberReferenceHandle =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .MethodSignature(isInstanceMethod = true)
                .Parameters (
                    arity,
                    (fun returnType -> returnType.Void ()),
                    (fun parameters ->
                        for _ in 1..arity do
                            parameters.AddParameter().Type().Int32 ()
                    )
                )

            metadata.AddMemberReference (
                (TypeSpecificationHandle.op_Implicit rankTwo : EntityHandle),
                metadata.GetOrAddString ".ctor",
                metadata.GetOrAddBlob blob
            )

        let body (arity : int) : int =
            let code = InstructionEncoder (BlobBuilder ())

            for _ in 1..arity do
                code.LoadConstantI4 1

            code.OpCode ILOpCode.Newobj
            code.Token (MemberReferenceHandle.op_Implicit (constructor arity) : EntityHandle)
            code.OpCode ILOpCode.Pop
            code.OpCode ILOpCode.Ret
            bodies.AddMethodBody code

        let staticVoid =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .MethodSignature()
                .Parameters (0, (fun returnType -> returnType.Void ()), ignore<ParametersEncoder>)

            metadata.GetOrAddBlob blob

        let addMethod (name : string) (arity : int) =
            metadata.AddMethodDefinition (
                MethodAttributes.Public ||| MethodAttributes.Static,
                MethodImplAttributes.IL,
                metadata.GetOrAddString name,
                staticVoid,
                body arity,
                MetadataTokens.ParameterHandle 1
            )

        let lowerBounds = addMethod "LowerBounds" 4
        addMethod "Lengths" 2 |> ignore<MethodDefinitionHandle>

        metadata.AddTypeDefinition (
            TypeAttributes.Class,
            Unchecked.defaultof<StringHandle>,
            metadata.GetOrAddString "<Module>",
            Unchecked.defaultof<EntityHandle>,
            MetadataTokens.FieldDefinitionHandle 1,
            lowerBounds
        )
        |> ignore<TypeDefinitionHandle>

        metadata.AddTypeDefinition (
            TypeAttributes.Public
            ||| TypeAttributes.Class
            ||| TypeAttributes.Abstract
            ||| TypeAttributes.Sealed,
            metadata.GetOrAddString "W",
            metadata.GetOrAddString "Arrays",
            (TypeReferenceHandle.op_Implicit objectRef : EntityHandle),
            MetadataTokens.FieldDefinitionHandle 1,
            lowerBounds
        )
        |> ignore<TypeDefinitionHandle>

        let peBuilder =
            ManagedPEBuilder (
                PEHeaderBuilder (imageCharacteristics = Characteristics.Dll),
                MetadataRootBuilder metadata,
                ilStream
            )

        let image = BlobBuilder ()
        peBuilder.Serialize image |> ignore<BlobContentId>

        let assembly =
            Assembly.read loggerFactory (Some "ArrayCtors.dll") (new MemoryStream (image.ToArray ()))

        let mutable analysis = analysisOver [ assembly ] id

        let answer (name : string) =
            let next, escapes =
                EscapeAnalysis.escapes analysis (methodNamed assembly "W.Arrays" name)

            analysis <- next
            render analysis escapes

        let withBounds = answer "LowerBounds"
        let withoutBounds = answer "Lengths"
        withBounds |> shouldContain "=System.ArgumentOutOfRangeException"
        withBounds |> shouldContain "=System.OverflowException"
        withoutBounds |> shouldNotContain "=System.ArgumentOutOfRangeException"
        withoutBounds |> shouldContain "=System.OverflowException"

    /// `ldvirtftn` of an interface method, on an object implementing `IDynamicInterfaceCastable`,
    /// asks that object's `GetInterfaceImplementation`, which may throw anything; of a class's
    /// virtual method, it runs nothing. C# spells neither without a delegate constructor after it,
    /// so the instructions are emitted directly.
    [<Test>]
    let ``resolving an interface method's pointer may run the receiver's code`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let metadata = MetadataBuilder ()
        let ilStream = BlobBuilder ()
        let bodies = MethodBodyStreamEncoder ilStream

        metadata.AddModule (
            0,
            metadata.GetOrAddString "Pointers.dll",
            metadata.GetOrAddGuid (Guid "8e2b4d6f-1a3c-4e5f-9b7d-2c4e6a8b0d1f"),
            Unchecked.defaultof<GuidHandle>,
            Unchecked.defaultof<GuidHandle>
        )
        |> ignore<ModuleDefinitionHandle>

        metadata.AddAssembly (
            metadata.GetOrAddString "Pointers",
            Version (1, 0, 0, 0),
            Unchecked.defaultof<StringHandle>,
            Unchecked.defaultof<BlobHandle>,
            Unchecked.defaultof<AssemblyFlags>,
            AssemblyHashAlgorithm.None
        )
        |> ignore<AssemblyDefinitionHandle>

        let corelibName = typeof<obj>.Assembly.GetName ()

        let corelibRef =
            metadata.AddAssemblyReference (
                metadata.GetOrAddString corelibName.Name,
                corelibName.Version,
                Unchecked.defaultof<StringHandle>,
                metadata.GetOrAddBlob (corelibName.GetPublicKeyToken ()),
                Unchecked.defaultof<AssemblyFlags>,
                Unchecked.defaultof<BlobHandle>
            )

        let objectRef =
            metadata.AddTypeReference (
                (AssemblyReferenceHandle.op_Implicit corelibRef : EntityHandle),
                metadata.GetOrAddString "System",
                metadata.GetOrAddString "Object"
            )

        // Type definitions are numbered in the order they are added: `<Module>`, `IFoo`, `Base`,
        // `Resolves`; methods likewise: `IFoo.M`, `Base.M`, then `Resolve` and `ResolveBase`.
        let interfaceType = MetadataTokens.TypeDefinitionHandle 2
        let baseType = MetadataTokens.TypeDefinitionHandle 3
        let interfaceMethod = MetadataTokens.MethodDefinitionHandle 1
        let baseMethod = MetadataTokens.MethodDefinitionHandle 2

        let instanceInt32 =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .MethodSignature(isInstanceMethod = true)
                .Parameters (0, (fun returnType -> returnType.Type().Int32 ()), ignore<ParametersEncoder>)

            metadata.GetOrAddBlob blob

        let staticTaking (parameterType : TypeDefinitionHandle) =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .MethodSignature()
                .Parameters (
                    1,
                    (fun returnType -> returnType.Void ()),
                    (fun parameters ->
                        parameters
                            .AddParameter()
                            .Type()
                            .Type ((TypeDefinitionHandle.op_Implicit parameterType : EntityHandle), false)
                    )
                )

            metadata.GetOrAddBlob blob

        let virtualMethod =
            MethodAttributes.Public
            ||| MethodAttributes.Virtual
            ||| MethodAttributes.NewSlot
            ||| MethodAttributes.HideBySig

        metadata.AddMethodDefinition (
            virtualMethod ||| MethodAttributes.Abstract,
            MethodImplAttributes.IL,
            metadata.GetOrAddString "M",
            instanceInt32,
            -1,
            MetadataTokens.ParameterHandle 1
        )
        |> ignore<MethodDefinitionHandle>

        let returnZero =
            let code = InstructionEncoder (BlobBuilder ())
            code.LoadConstantI4 0
            code.OpCode ILOpCode.Ret
            bodies.AddMethodBody code

        metadata.AddMethodDefinition (
            virtualMethod,
            MethodImplAttributes.IL,
            metadata.GetOrAddString "M",
            instanceInt32,
            returnZero,
            MetadataTokens.ParameterHandle 1
        )
        |> ignore<MethodDefinitionHandle>

        let resolving (target : MethodDefinitionHandle) =
            let code = InstructionEncoder (BlobBuilder ())
            code.LoadArgument 0
            code.OpCode ILOpCode.Ldvirtftn
            code.Token (MethodDefinitionHandle.op_Implicit target : EntityHandle)
            code.OpCode ILOpCode.Pop
            code.OpCode ILOpCode.Ret
            bodies.AddMethodBody code

        for name, parameterType, target in
            [
                "Resolve", interfaceType, interfaceMethod
                "ResolveBase", baseType, baseMethod
            ] do
            metadata.AddMethodDefinition (
                MethodAttributes.Public ||| MethodAttributes.Static,
                MethodImplAttributes.IL,
                metadata.GetOrAddString name,
                staticTaking parameterType,
                resolving target,
                MetadataTokens.ParameterHandle 1
            )
            |> ignore<MethodDefinitionHandle>

        metadata.AddTypeDefinition (
            TypeAttributes.Class,
            Unchecked.defaultof<StringHandle>,
            metadata.GetOrAddString "<Module>",
            Unchecked.defaultof<EntityHandle>,
            MetadataTokens.FieldDefinitionHandle 1,
            interfaceMethod
        )
        |> ignore<TypeDefinitionHandle>

        metadata.AddTypeDefinition (
            TypeAttributes.Public ||| TypeAttributes.Interface ||| TypeAttributes.Abstract,
            metadata.GetOrAddString "W",
            metadata.GetOrAddString "IFoo",
            Unchecked.defaultof<EntityHandle>,
            MetadataTokens.FieldDefinitionHandle 1,
            interfaceMethod
        )
        |> ignore<TypeDefinitionHandle>

        metadata.AddTypeDefinition (
            TypeAttributes.Public ||| TypeAttributes.Class,
            metadata.GetOrAddString "W",
            metadata.GetOrAddString "Base",
            (TypeReferenceHandle.op_Implicit objectRef : EntityHandle),
            MetadataTokens.FieldDefinitionHandle 1,
            baseMethod
        )
        |> ignore<TypeDefinitionHandle>

        metadata.AddTypeDefinition (
            TypeAttributes.Public
            ||| TypeAttributes.Class
            ||| TypeAttributes.Abstract
            ||| TypeAttributes.Sealed,
            metadata.GetOrAddString "W",
            metadata.GetOrAddString "Resolves",
            (TypeReferenceHandle.op_Implicit objectRef : EntityHandle),
            MetadataTokens.FieldDefinitionHandle 1,
            MetadataTokens.MethodDefinitionHandle 3
        )
        |> ignore<TypeDefinitionHandle>

        let peBuilder =
            ManagedPEBuilder (
                PEHeaderBuilder (imageCharacteristics = Characteristics.Dll),
                MetadataRootBuilder metadata,
                ilStream
            )

        let image = BlobBuilder ()
        peBuilder.Serialize image |> ignore<BlobContentId>

        let assembly =
            Assembly.read loggerFactory (Some "Pointers.dll") (new MemoryStream (image.ToArray ()))

        let analysis = analysisOver [ assembly ] id

        let _, throughInterface =
            EscapeAnalysis.escapes analysis (methodNamed assembly "W.Resolves" "Resolve")

        let _, throughClass =
            EscapeAnalysis.escapes analysis (methodNamed assembly "W.Resolves" "ResolveBase")

        throughInterface.Unknown |> shouldEqual true
        throughClass.Unknown |> shouldEqual false

    [<Test>]
    let ``binding a member of a type variable turns on the instantiation, past the method's handlers`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let metadata = MetadataBuilder ()
        let ilStream = BlobBuilder ()
        let bodies = MethodBodyStreamEncoder ilStream

        metadata.AddModule (
            0,
            metadata.GetOrAddString "Binds.dll",
            metadata.GetOrAddGuid (Guid "3b5d7f91-2a4c-4e6b-8d0f-1a3c5e7b9d2f"),
            Unchecked.defaultof<GuidHandle>,
            Unchecked.defaultof<GuidHandle>
        )
        |> ignore<ModuleDefinitionHandle>

        metadata.AddAssembly (
            metadata.GetOrAddString "Binds",
            Version (1, 0, 0, 0),
            Unchecked.defaultof<StringHandle>,
            Unchecked.defaultof<BlobHandle>,
            Unchecked.defaultof<AssemblyFlags>,
            AssemblyHashAlgorithm.None
        )
        |> ignore<AssemblyDefinitionHandle>

        let corelibName = typeof<obj>.Assembly.GetName ()

        let corelibRef =
            metadata.AddAssemblyReference (
                metadata.GetOrAddString corelibName.Name,
                corelibName.Version,
                Unchecked.defaultof<StringHandle>,
                metadata.GetOrAddBlob (corelibName.GetPublicKeyToken ()),
                Unchecked.defaultof<AssemblyFlags>,
                Unchecked.defaultof<BlobHandle>
            )

        let objectRef : EntityHandle =
            metadata.AddTypeReference (
                (AssemblyReferenceHandle.op_Implicit corelibRef : EntityHandle),
                metadata.GetOrAddString "System",
                metadata.GetOrAddString "Object"
            )
            |> TypeReferenceHandle.op_Implicit

        // `!!0`, the method's own type parameter, as a MemberRef parent.
        let typeVariable : EntityHandle =
            let blob = BlobBuilder ()
            BlobEncoder(blob).TypeSpecificationSignature().GenericMethodTypeParameter 0
            TypeSpecificationHandle.op_Implicit (metadata.AddTypeSpecification (metadata.GetOrAddBlob blob))

        let staticVoid (genericParameters : int) : BlobHandle =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .MethodSignature(genericParameterCount = genericParameters)
                .Parameters (0, (fun returnType -> returnType.Void ()), ignore<ParametersEncoder>)

            metadata.GetOrAddBlob blob

        let valueField : EntityHandle =
            let blob = BlobBuilder ()
            BlobEncoder(blob).Field().Type().Int32 ()

            metadata.AddMemberReference (typeVariable, metadata.GetOrAddString "Value", metadata.GetOrAddBlob blob)
            |> MemberReferenceHandle.op_Implicit

        let methodM : EntityHandle =
            metadata.AddMemberReference (typeVariable, metadata.GetOrAddString "M", staticVoid 0)
            |> MemberReferenceHandle.op_Implicit

        // `try { <use> } catch (object) { }`: a catch-all around the only use of the member.
        let catchingAll (usesMember : InstructionEncoder -> unit) : int =
            let flow = ControlFlowBuilder ()
            let code = InstructionEncoder (BlobBuilder (), flow)
            let tryStart = code.DefineLabel ()
            let handlerStart = code.DefineLabel ()
            let handlerEnd = code.DefineLabel ()
            code.MarkLabel tryStart
            usesMember code
            code.Branch (ILOpCode.Leave_s, handlerEnd)
            code.MarkLabel handlerStart
            code.OpCode ILOpCode.Pop
            code.Branch (ILOpCode.Leave_s, handlerEnd)
            code.MarkLabel handlerEnd
            code.OpCode ILOpCode.Ret
            flow.AddCatchRegion (tryStart, handlerStart, handlerStart, handlerEnd, objectRef)
            bodies.AddMethodBody code

        let readField =
            metadata.AddMethodDefinition (
                MethodAttributes.Public ||| MethodAttributes.Static,
                MethodImplAttributes.IL,
                metadata.GetOrAddString "ReadField",
                staticVoid 1,
                catchingAll (fun code ->
                    code.OpCode ILOpCode.Ldsfld
                    code.Token valueField
                    code.OpCode ILOpCode.Pop
                ),
                MetadataTokens.ParameterHandle 1
            )

        let callMethod =
            metadata.AddMethodDefinition (
                MethodAttributes.Public ||| MethodAttributes.Static,
                MethodImplAttributes.IL,
                metadata.GetOrAddString "CallMethod",
                staticVoid 1,
                catchingAll (fun code -> code.Call methodM),
                MetadataTokens.ParameterHandle 1
            )

        for method in [ readField ; callMethod ] do
            metadata.AddGenericParameter (
                (MethodDefinitionHandle.op_Implicit method : EntityHandle),
                GenericParameterAttributes.None,
                metadata.GetOrAddString "T",
                0
            )
            |> ignore<GenericParameterHandle>

        metadata.AddTypeDefinition (
            TypeAttributes.Class,
            Unchecked.defaultof<StringHandle>,
            metadata.GetOrAddString "<Module>",
            Unchecked.defaultof<EntityHandle>,
            MetadataTokens.FieldDefinitionHandle 1,
            readField
        )
        |> ignore<TypeDefinitionHandle>

        metadata.AddTypeDefinition (
            TypeAttributes.Public
            ||| TypeAttributes.Class
            ||| TypeAttributes.Abstract
            ||| TypeAttributes.Sealed,
            metadata.GetOrAddString "W",
            metadata.GetOrAddString "Binds",
            objectRef,
            MetadataTokens.FieldDefinitionHandle 1,
            readField
        )
        |> ignore<TypeDefinitionHandle>

        let peBuilder =
            ManagedPEBuilder (
                PEHeaderBuilder (imageCharacteristics = Characteristics.Dll),
                MetadataRootBuilder metadata,
                ilStream
            )

        let image =
            let blob = BlobBuilder ()
            peBuilder.Serialize blob |> ignore<BlobContentId>
            blob.ToArray ()

        // The real runtime, at an instantiation lacking the member, shared (`object`) and not
        // (`int`): the binding failure gets past the catch-all.
        let context =
            System.Runtime.Loader.AssemblyLoadContext ("Binds", isCollectible = true)

        try
            let binds = context.LoadFromStream(new MemoryStream (image)).GetType "W.Binds"

            for name, missing in
                [
                    "ReadField", typeof<MissingFieldException>
                    "CallMethod", typeof<MissingMethodException>
                ] do
                for instantiation in [ typeof<obj> ; typeof<int> ] do
                    let e =
                        Assert.Throws<TargetInvocationException> (fun () ->
                            binds
                                .GetMethod(name)
                                .MakeGenericMethod(instantiation)
                                .Invoke ((null : obj), Array.empty<obj>)
                            |> ignore<obj>
                        )

                    e.InnerException.GetType () |> shouldEqual missing
        finally
            context.Unload ()

        let assembly =
            Assembly.read loggerFactory (Some "Binds.dll") (new MemoryStream (image))

        let analysis = analysisOver [ assembly ] id

        for name in [ "ReadField" ; "CallMethod" ] do
            let _, escapes =
                EscapeAnalysis.escapes analysis (methodNamed assembly "W.Binds" name)

            escapes.Unknown |> shouldEqual true

    /// Where the object a `throw` raises comes from, in an emitted method.
    [<RequireQualifiedAccess>]
    type private Raise =
        /// `newobj object::.ctor; throw`.
        | Constructed
        /// `call object Make(); throw`, whose static type is only `object`.
        | Returned
        /// `call object MakeException(); throw`: an exception, whose static type is only `object`.
        | ReturnedException
        /// `call Throw()`, which throws a constructed object.
        | Callee
        /// `ldstr; newobj RuntimeWrappedException::.ctor(object); throw`: a wrapper thrown
        /// explicitly, which a clause in an assembly that does not wrap sees unwrapped.
        | ConstructedWrapper
        /// `call Exception MakeWrapper(); throw`: the same wrapper, whose static type is only
        /// `Exception`.
        | ReturnedWrapper
        /// `newobj Exception::.ctor(); newobj RuntimeWrappedException::.ctor(object); throw`: a
        /// wrapper of an exception, which a clause in an assembly that does not wrap sees as that
        /// exception.
        | ConstructedWrapperOfException
        /// What `Make` returns, stored in and loaded back from an `object[]`: the analysis does not
        /// follow what an array holds, so it does not know the operand's type.
        | Untyped
        /// `newobj Nullable<int>(1); box Nullable<int>; throw`: boxing a nullable boxes its value, or
        /// makes null, so what is thrown is an `Int32`, not a `Nullable<int>`.
        | BoxedNullable

    /// The `catch` clause, if any, around an emitted method's raise.
    [<RequireQualifiedAccess>]
    type private Clause =
        | Uncaught
        | Catch of ns : string * name : string

    let private raises : Raise list =
        [
            Raise.Constructed
            Raise.Returned
            Raise.ReturnedException
            Raise.Callee
            Raise.ConstructedWrapper
            Raise.ReturnedWrapper
            Raise.ConstructedWrapperOfException
            Raise.Untyped
            Raise.BoxedNullable
        ]

    let private clauses : Clause list =
        [
            Clause.Uncaught
            Clause.Catch ("System", "Exception")
            Clause.Catch ("System.Runtime.CompilerServices", "RuntimeWrappedException")
            Clause.Catch ("System", "Object")
            // Every exception implements it, but a clause for an interface catches nothing.
            Clause.Catch ("System.Runtime.Serialization", "ISerializable")
            // Unrelated to exceptions: it catches only a thrown wrapper of a string, in an assembly
            // that does not wrap.
            Clause.Catch ("System", "String")
        ]

    /// What the handler of an emitted method's clause does with what it caught.
    [<RequireQualifiedAccess>]
    type private Handling =
        | Swallows
        /// `rethrow`s it, beside a second clause, for `object`, that swallows whatever the first
        /// does not catch, so that what escapes is exactly what the first caught.
        | Rethrows

    let private emittedCases : (Raise * Clause * Handling) list =
        [
            for raise in raises do
                for clause in clauses do
                    yield raise, clause, Handling.Swallows

                    match clause with
                    | Clause.Uncaught -> ()
                    | Clause.Catch _ -> yield raise, clause, Handling.Rethrows
        ]

    let private emittedMethodName (raise : Raise) (clause : Clause) (handling : Handling) : string =
        let handled =
            match handling with
            | Handling.Swallows -> ""
            | Handling.Rethrows -> "_rethrown"

        match clause with
        | Clause.Uncaught -> $"%A{raise}"
        | Clause.Catch (_, name) -> $"%A{raise}_%s{name}%s{handled}"

    /// An assembly `W.Throws` with one static method per case, plus the helpers `Make`,
    /// `MakeException`, `Throw` and `MakeWrapper`, carrying one `RuntimeCompatibilityAttribute` per blob in `attributes`, in order.
    let private emitThrows (attributes : byte[] list) : byte[] =
        let metadata = MetadataBuilder ()
        let ilStream = BlobBuilder ()
        let bodies = MethodBodyStreamEncoder ilStream

        metadata.AddModule (
            0,
            metadata.GetOrAddString "Throws.dll",
            metadata.GetOrAddGuid (Guid "5a1e7c3b-9d2f-4b8e-a6c4-1f3e5d7b9a2c"),
            Unchecked.defaultof<GuidHandle>,
            Unchecked.defaultof<GuidHandle>
        )
        |> ignore<ModuleDefinitionHandle>

        let assemblyDefinition =
            metadata.AddAssembly (
                metadata.GetOrAddString "Throws",
                Version (1, 0, 0, 0),
                Unchecked.defaultof<StringHandle>,
                Unchecked.defaultof<BlobHandle>,
                Unchecked.defaultof<AssemblyFlags>,
                AssemblyHashAlgorithm.None
            )

        let corelibName = typeof<obj>.Assembly.GetName ()

        let corelibRef =
            metadata.AddAssemblyReference (
                metadata.GetOrAddString corelibName.Name,
                corelibName.Version,
                Unchecked.defaultof<StringHandle>,
                metadata.GetOrAddBlob (corelibName.GetPublicKeyToken ()),
                Unchecked.defaultof<AssemblyFlags>,
                Unchecked.defaultof<BlobHandle>
            )

        let typeRef (ns : string) (name : string) : EntityHandle =
            metadata.AddTypeReference (
                (AssemblyReferenceHandle.op_Implicit corelibRef : EntityHandle),
                metadata.GetOrAddString ns,
                metadata.GetOrAddString name
            )
            |> TypeReferenceHandle.op_Implicit

        let objectRef = typeRef "System" "Object"

        // `Nullable<int>`, and its constructor taking the value.
        let nullableOfInt : EntityHandle =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .TypeSpecificationSignature()
                .GenericInstantiation(typeRef "System" "Nullable`1", 1, true)
                .AddArgument()
                .Int32 ()

            metadata.AddTypeSpecification (metadata.GetOrAddBlob blob)
            |> TypeSpecificationHandle.op_Implicit

        let nullableConstructor : EntityHandle =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .MethodSignature(isInstanceMethod = true)
                .Parameters (
                    1,
                    (fun returnType -> returnType.Void ()),
                    (fun parameters -> parameters.AddParameter().Type().GenericTypeParameter 0)
                )

            metadata.AddMemberReference (nullableOfInt, metadata.GetOrAddString ".ctor", metadata.GetOrAddBlob blob)
            |> MemberReferenceHandle.op_Implicit

        let signature (isInstance : bool) (returnsObject : bool) : BlobHandle =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .MethodSignature(isInstanceMethod = isInstance)
                .Parameters (
                    0,
                    (fun returnType ->
                        if returnsObject then
                            returnType.Type().Object ()
                        else
                            returnType.Void ()
                    ),
                    ignore<ParametersEncoder>
                )

            metadata.GetOrAddBlob blob

        let constructorOf (ty : EntityHandle) : EntityHandle =
            metadata.AddMemberReference (ty, metadata.GetOrAddString ".ctor", signature true false)
            |> MemberReferenceHandle.op_Implicit

        let objectConstructor = constructorOf objectRef
        let exceptionRef = typeRef "System" "Exception"
        let exceptionConstructor = constructorOf exceptionRef

        let wrapperConstructor : EntityHandle =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .MethodSignature(isInstanceMethod = true)
                .Parameters (
                    1,
                    (fun returnType -> returnType.Void ()),
                    (fun parameters -> parameters.AddParameter().Type().Object ())
                )

            metadata.AddMemberReference (
                typeRef "System.Runtime.CompilerServices" "RuntimeWrappedException",
                metadata.GetOrAddString ".ctor",
                metadata.GetOrAddBlob blob
            )
            |> MemberReferenceHandle.op_Implicit

        let compatibilityConstructor =
            constructorOf (typeRef "System.Runtime.CompilerServices" "RuntimeCompatibilityAttribute")

        for blob in attributes do
            metadata.AddCustomAttribute (
                (AssemblyDefinitionHandle.op_Implicit assemblyDefinition : EntityHandle),
                compatibilityConstructor,
                metadata.GetOrAddBlob blob
            )
            |> ignore<CustomAttributeHandle>

        // Method definitions are numbered in the order they are added: `Make`, `MakeException`,
        // `Throw`, `MakeWrapper`, then the cases in the order of `raises` and `clauses`.
        let make = MetadataTokens.MethodDefinitionHandle 1
        let makeException = MetadataTokens.MethodDefinitionHandle 2
        let throw = MetadataTokens.MethodDefinitionHandle 3
        let makeWrapper = MetadataTokens.MethodDefinitionHandle 4

        let constructWrapper (code : InstructionEncoder) : unit =
            code.LoadString (metadata.GetOrAddUserString "sentinel")
            code.OpCode ILOpCode.Newobj
            code.Token wrapperConstructor

        let emitRaise (code : InstructionEncoder) (raise : Raise) : unit =
            match raise with
            | Raise.Constructed ->
                code.OpCode ILOpCode.Newobj
                code.Token objectConstructor
                code.OpCode ILOpCode.Throw
            | Raise.Returned ->
                code.Call make
                code.OpCode ILOpCode.Throw
            | Raise.ReturnedException ->
                code.Call makeException
                code.OpCode ILOpCode.Throw
            | Raise.Callee -> code.Call throw
            | Raise.ConstructedWrapper ->
                constructWrapper code
                code.OpCode ILOpCode.Throw
            | Raise.ReturnedWrapper ->
                code.Call makeWrapper
                code.OpCode ILOpCode.Throw
            | Raise.ConstructedWrapperOfException ->
                code.OpCode ILOpCode.Newobj
                code.Token exceptionConstructor
                code.OpCode ILOpCode.Newobj
                code.Token wrapperConstructor
                code.OpCode ILOpCode.Throw
            | Raise.Untyped ->
                code.LoadConstantI4 1
                code.OpCode ILOpCode.Newarr
                code.Token objectRef
                code.OpCode ILOpCode.Dup
                code.LoadConstantI4 0
                code.Call make
                code.OpCode ILOpCode.Stelem_ref
                code.LoadConstantI4 0
                code.OpCode ILOpCode.Ldelem_ref
                code.OpCode ILOpCode.Throw
            | Raise.BoxedNullable ->
                code.LoadConstantI4 1
                code.OpCode ILOpCode.Newobj
                code.Token nullableConstructor
                code.OpCode ILOpCode.Box
                code.Token nullableOfInt
                code.OpCode ILOpCode.Throw

        let addMethod (name : string) (returnsObject : bool) (body : int) : unit =
            metadata.AddMethodDefinition (
                MethodAttributes.Public ||| MethodAttributes.Static,
                MethodImplAttributes.IL,
                metadata.GetOrAddString name,
                signature false returnsObject,
                body,
                MetadataTokens.ParameterHandle 1
            )
            |> ignore<MethodDefinitionHandle>

        let makeBody =
            let code = InstructionEncoder (BlobBuilder ())
            code.OpCode ILOpCode.Newobj
            code.Token objectConstructor
            code.OpCode ILOpCode.Ret
            bodies.AddMethodBody code

        addMethod "Make" true makeBody

        let makeExceptionBody =
            let code = InstructionEncoder (BlobBuilder ())
            code.OpCode ILOpCode.Newobj
            code.Token exceptionConstructor
            code.OpCode ILOpCode.Ret
            bodies.AddMethodBody code

        addMethod "MakeException" true makeExceptionBody

        let throwBody =
            let code = InstructionEncoder (BlobBuilder ())
            emitRaise code Raise.Constructed
            bodies.AddMethodBody code

        addMethod "Throw" false throwBody

        let makeWrapperBody =
            let code = InstructionEncoder (BlobBuilder ())
            constructWrapper code
            code.OpCode ILOpCode.Ret
            bodies.AddMethodBody code

        let returnsException =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .MethodSignature()
                .Parameters (
                    0,
                    (fun returnType -> returnType.Type().Type (exceptionRef, false)),
                    ignore<ParametersEncoder>
                )

            metadata.GetOrAddBlob blob

        metadata.AddMethodDefinition (
            MethodAttributes.Public ||| MethodAttributes.Static,
            MethodImplAttributes.IL,
            metadata.GetOrAddString "MakeWrapper",
            returnsException,
            makeWrapperBody,
            MetadataTokens.ParameterHandle 1
        )
        |> ignore<MethodDefinitionHandle>

        for raise, clause, handling in emittedCases do
            let body =
                match clause, handling with
                | Clause.Uncaught, _ ->
                    let code = InstructionEncoder (BlobBuilder ())
                    emitRaise code raise
                    code.OpCode ILOpCode.Ret
                    bodies.AddMethodBody code
                | Clause.Catch (ns, name), Handling.Swallows ->
                    let flow = ControlFlowBuilder ()
                    let code = InstructionEncoder (BlobBuilder (), flow)
                    let tryStart = code.DefineLabel ()
                    let handlerStart = code.DefineLabel ()
                    let handlerEnd = code.DefineLabel ()
                    code.MarkLabel tryStart
                    emitRaise code raise
                    code.Branch (ILOpCode.Leave_s, handlerEnd)
                    code.MarkLabel handlerStart
                    code.OpCode ILOpCode.Pop
                    code.Branch (ILOpCode.Leave_s, handlerEnd)
                    code.MarkLabel handlerEnd
                    code.OpCode ILOpCode.Ret
                    flow.AddCatchRegion (tryStart, handlerStart, handlerStart, handlerEnd, typeRef ns name)
                    bodies.AddMethodBody code
                | Clause.Catch (ns, name), Handling.Rethrows ->
                    let flow = ControlFlowBuilder ()
                    let code = InstructionEncoder (BlobBuilder (), flow)
                    let tryStart = code.DefineLabel ()
                    let rethrowing = code.DefineLabel ()
                    let swallowing = code.DefineLabel ()
                    let handlersEnd = code.DefineLabel ()
                    code.MarkLabel tryStart
                    emitRaise code raise
                    code.Branch (ILOpCode.Leave_s, handlersEnd)
                    code.MarkLabel rethrowing
                    code.OpCode ILOpCode.Rethrow
                    code.MarkLabel swallowing
                    code.OpCode ILOpCode.Pop
                    code.Branch (ILOpCode.Leave_s, handlersEnd)
                    code.MarkLabel handlersEnd
                    code.OpCode ILOpCode.Ret
                    flow.AddCatchRegion (tryStart, rethrowing, rethrowing, swallowing, typeRef ns name)
                    flow.AddCatchRegion (tryStart, rethrowing, swallowing, handlersEnd, objectRef)
                    bodies.AddMethodBody code

            addMethod (emittedMethodName raise clause handling) false body

        metadata.AddTypeDefinition (
            TypeAttributes.Class,
            Unchecked.defaultof<StringHandle>,
            metadata.GetOrAddString "<Module>",
            Unchecked.defaultof<EntityHandle>,
            MetadataTokens.FieldDefinitionHandle 1,
            make
        )
        |> ignore<TypeDefinitionHandle>

        metadata.AddTypeDefinition (
            TypeAttributes.Public
            ||| TypeAttributes.Class
            ||| TypeAttributes.Abstract
            ||| TypeAttributes.Sealed,
            metadata.GetOrAddString "W",
            metadata.GetOrAddString "Throws",
            objectRef,
            MetadataTokens.FieldDefinitionHandle 1,
            make
        )
        |> ignore<TypeDefinitionHandle>

        let peBuilder =
            ManagedPEBuilder (
                PEHeaderBuilder (imageCharacteristics = Characteristics.Dll),
                MetadataRootBuilder metadata,
                ilStream
            )

        let image = BlobBuilder ()
        peBuilder.Serialize image |> ignore<BlobContentId>
        image.ToArray ()

    /// Run each of `methods` of `W.Throws` in `image` on the real runtime, answering which let an
    /// exception escape.
    let private escapingOnRealRuntime (image : byte[]) (methods : string list) : Map<string, bool> =
        let context =
            System.Runtime.Loader.AssemblyLoadContext ("Throws", isCollectible = true)

        try
            let throws = context.LoadFromStream(new MemoryStream (image)).GetType "W.Throws"

            methods
            |> List.map (fun name ->
                let escaped =
                    try
                        throws.GetMethod(name).Invoke ((null : obj), Array.empty<obj>) |> ignore<obj>
                        false
                    with :? TargetInvocationException ->
                        true

                name, escaped
            )
            |> Map.ofList
        finally
            context.Unload ()

    [<Test>]
    let ``a thrown object that is not an exception is caught as the catching assembly sees it`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let cases =
            emittedCases
            |> List.map (fun (raise, clause, handling) ->
                raise, clause, handling, emittedMethodName raise clause handling
            )

        for wraps in [ false ; true ] do
            let image =
                emitThrows (
                    if wraps then
                        [
                            TestRuntimeCompatibility.compatibilityBlob
                                1s
                                [ TestRuntimeCompatibility.wrapProperty [ 1uy ] ]
                        ]
                    else
                        []
                )

            let assembly =
                Assembly.read loggerFactory (Some "Throws.dll") (new MemoryStream (image))

            RuntimeCompatibility.wrapsNonExceptionThrows assembly |> shouldEqual wraps

            // An untyped raise throws what `Raise.Returned` throws, and a boxed nullable a boxed
            // `Int32`, neither an exception, as the runtime is already asked about; what is checked
            // of them is how the analysis treats an unknown exception.
            let runtime =
                cases
                |> List.filter (fun (raise, _, _, _) -> raise <> Raise.Untyped && raise <> Raise.BoxedNullable)
                |> List.map (fun (_, _, _, name) -> name)
                |> escapingOnRealRuntime image

            let mutable analysis = analysisOver [ assembly ] id

            for raise, clause, handling, name in cases do
                let next, escapes =
                    EscapeAnalysis.escapes analysis (methodNamed assembly "W.Throws" name)

                analysis <- next

                let describe () =
                    let wrapping = if wraps then "wraps" else "does not wrap"
                    $"%s{name} in an assembly that %s{wrapping}: %A{render analysis escapes}, unknown %b{escapes.Unknown}"

                match raise with
                | Raise.Untyped
                | Raise.BoxedNullable ->
                    // A `rethrow` re-raises what its clause caught of an unknown exception, which
                    // is nothing for a clause for an interface, and one of the clause's type for a
                    // clause that sees only exceptions: for a type other than an ancestor of
                    // `RuntimeWrappedException`, in an assembly that wraps.
                    let absorbed =
                        match clause, handling with
                        | Clause.Catch ("System.Runtime.Serialization", "ISerializable"), Handling.Rethrows -> true
                        | Clause.Catch ("System", "String"), Handling.Rethrows -> wraps
                        | _, Handling.Rethrows -> false
                        | Clause.Catch ("System", "Object"), _ -> true
                        | Clause.Catch ("System", "Exception"), _ -> wraps
                        | _ -> false

                    if escapes.Unknown = absorbed then
                        failwith $"%s{describe ()}; expected unknown %b{not absorbed}"
                | _ ->
                    // What the analysis reports the raise as.
                    let reported =
                        match raise with
                        | Raise.ConstructedWrapper
                        | Raise.ConstructedWrapperOfException ->
                            [ "=System.Runtime.CompilerServices.RuntimeWrappedException" ]
                        | Raise.ReturnedWrapper -> [ "<:System.Exception" ]
                        | _ -> [ "=System.Object" ; "<:System.Object" ]

                    let analysisEscapes =
                        render analysis escapes
                        |> Set.exists (fun shown -> List.contains shown reported)

                    // Everything that escapes is reported. A returned object's type is known only
                    // as its static type, which may cover values a clause stops and values it does
                    // not, and whatever could not stop them all is reported to let it escape; and a
                    // clause in an assembly that does not wrap sees a thrown wrapper as whatever it
                    // wraps, which the analysis does not know. Otherwise the answer is exact.
                    let exact =
                        match raise with
                        | Raise.Returned
                        | Raise.ReturnedException
                        | Raise.ReturnedWrapper -> false
                        | Raise.ConstructedWrapper
                        | Raise.ConstructedWrapperOfException -> wraps
                        | _ -> true

                    if
                        (runtime.[name] && not analysisEscapes)
                        || (exact && analysisEscapes <> runtime.[name])
                    then
                        failwith $"%s{describe ()}; the runtime lets it escape: %b{runtime.[name]}"

    /// What the analysis must say of a call that dispatches on a receiver of this kind.
    [<RequireQualifiedAccess>]
    type private DispatchClaim =
        /// The `constrained.` type decides the method, whose body the analysis sees: exactly its
        /// arithmetic exceptions escape, and nothing unknown.
        | Precise
        /// The receiver may be of a derived class that overrides the method, so what runs is
        /// unknown.
        | Unknown
        /// The method that runs is one the analysis cannot see into, so only soundness is claimed.
        | SoundOnly

    /// A receiver in the dispatch fixture, and which of `DivideByZeroException` and
    /// `OverflowException` the method it supplies raises.
    type private DispatchReceiver =
        {
            Name : string
            Claim : DispatchClaim
            Raises : string list
        }

    /// A way for a non-generic runner to reach a `constrained.` call on its receiver, as a C#
    /// expression over the receiver type `R`, the receiver `x` and the operands `a` and `b`, with
    /// the exceptions its own handlers stop.
    type private DispatchShape =
        {
            Name : string
            Call : string -> string
            Absorbs : string list
        }

    let private dividesByZero = "System.DivideByZeroException"
    let private overflows = "System.OverflowException"

    let private probeReceivers : DispatchReceiver list =
        [
            {
                Name = "Quiet"
                Claim = DispatchClaim.Precise
                Raises = []
            }
            {
                Name = "Adds"
                Claim = DispatchClaim.Precise
                Raises = [ overflows ]
            }
            {
                Name = "Divides"
                Claim = DispatchClaim.Precise
                Raises = [ dividesByZero ; overflows ]
            }
            {
                Name = "ExplicitAdds"
                Claim = DispatchClaim.Precise
                Raises = [ overflows ]
            }
            {
                Name = "UsesDefault"
                Claim = DispatchClaim.Precise
                Raises = [ dividesByZero ; overflows ]
            }
            {
                Name = "SealedDivides"
                Claim = DispatchClaim.Precise
                Raises = [ dividesByZero ; overflows ]
            }
            {
                Name = "ShadowsDefault"
                Claim = DispatchClaim.Precise
                Raises = [ dividesByZero ; overflows ]
            }
            {
                Name = "OpenDivides"
                Claim = DispatchClaim.Unknown
                Raises = [ dividesByZero ; overflows ]
            }
        ]

    let private probeShapes : DispatchShape list =
        [
            {
                Name = "Direct"
                Call = fun _ -> "Shapes.Direct(x, a, b)"
                Absorbs = []
            }
            {
                Name = "OnType"
                Call = fun r -> $"Holder<%s{r}>.Call(x, a, b)"
                Absorbs = []
            }
            {
                Name = "Relayed"
                Call = fun _ -> "Shapes.Relayed(x, a, b)"
                Absorbs = []
            }
            {
                Name = "TypeToMethod"
                Call = fun r -> $"Holder<%s{r}>.Relay(x, a, b)"
                Absorbs = []
            }
            {
                Name = "Caught"
                Call = fun _ -> "Shapes.Caught(x, a, b)"
                Absorbs = [ dividesByZero ]
            }
            {
                Name = "Wrapped"
                Call = fun r -> $"Shapes.Direct(new Wrapper<%s{r}>(x), a, b)"
                Absorbs = []
            }
            {
                Name = "Rethrown"
                Call = fun _ -> "Shapes.Rethrown(x, a, b)"
                Absorbs = []
            }
        ]

    /// The receivers, the generic methods that call `Probe` on a type variable, and the
    /// non-generic runners, one per shape and receiver, that close each instantiation.
    let private dispatchSource : string =
        let declarations =
            """
using System;

namespace Dispatch;

public interface IProbe
{
    int Probe(int a, int b) => a / b;
}

public struct Quiet : IProbe { public int Probe(int a, int b) => unchecked(a + b); }
public struct Adds : IProbe { public int Probe(int a, int b) => checked(a + b); }
public struct Divides : IProbe { public int Probe(int a, int b) => a / b; }
public struct ExplicitAdds : IProbe { int IProbe.Probe(int a, int b) => checked(a + b); }
public struct UsesDefault : IProbe { }

// `IShadowsProbe.Probe` is a new method, not an implementation of `IProbe.Probe`, so a call
// through `IProbe` runs `IProbe`'s default body.
public interface IShadowsProbe : IProbe { new int Probe(int a, int b) => checked(a + b); }
public struct ShadowsDefault : IShadowsProbe { }
public sealed class SealedDivides : IProbe { public int Probe(int a, int b) => a / b; }
public class OpenDivides : IProbe { public virtual int Probe(int a, int b) => a / b; }

public struct Wrapper<T> : IProbe where T : IProbe
{
    private T inner;
    public Wrapper(T inner) { this.inner = inner; }
    public int Probe(int a, int b) => inner.Probe(a, b);
}

public struct HashDivides
{
    public int Zero;
    public override int GetHashCode() => 1 / Zero;
}

public struct NoHash { public int Zero; }

public static class Shapes
{
    public static int Direct<T>(T x, int a, int b) where T : IProbe => x.Probe(a, b);
    public static int Relayed<T>(T x, int a, int b) where T : IProbe => Direct(x, a, b);

    public static int Caught<T>(T x, int a, int b) where T : IProbe
    {
        try { return x.Probe(a, b); }
        catch (DivideByZeroException) { return 0; }
    }

    // Catches everything, so what escapes is what the `throw;` re-raises.
    public static int Rethrown<T>(T x, int a, int b) where T : IProbe
    {
        try { return x.Probe(a, b); }
        catch (Exception) { throw; }
    }

    public static int Hash<T>(T x) => x.GetHashCode();

    // Generic methods whose calls do not mention their own type variables: one instantiating a
    // generic method, and one whose `constrained.` prefix names a closed type.
    public static int ClosedInside<T>(T ignored, int a, int b) => Direct(new Divides(), a, b);
    public static int HashInside<T>(T ignored, int z) { var h = new HashDivides { Zero = z }; return h.GetHashCode(); }

    // Each step instantiates itself at a deeper type, so no bound on nesting holds them all.
    public static int Grow<T>(T x, int n) where T : IProbe =>
        n == 0 ? x.Probe(1, 0) : Grow(new Wrapper<T>(x), n - 1);
}

public static class Holder<T> where T : IProbe
{
    public static int Call(T x, int a, int b) => x.Probe(a, b);
    public static int Relay(T x, int a, int b) => Shapes.Direct(x, a, b);
}
"""

        let runners =
            [
                for shape in probeShapes do
                    for receiver in probeReceivers do
                        yield
                            $"    public static int %s{shape.Name}_%s{receiver.Name}(int a, int b) {{ var x = new %s{receiver.Name}(); return %s{shape.Call receiver.Name}; }}"
                yield
                    "    public static int Hash_HashDivides(int a, int b) { var x = new HashDivides { Zero = b }; return Shapes.Hash(x); }"
                yield
                    "    public static int Hash_NoHash(int a, int b) { var x = new NoHash { Zero = b }; return Shapes.Hash(x); }"
                yield "    public static int Grow_Divides(int a, int b) => Shapes.Grow(new Divides(), 3);"
            ]
            |> String.concat "\n"

        declarations + "\npublic static class Runners\n{\n" + runners + "\n}\n"

    /// Operands that make each receiver's `Probe` raise each exception it can.
    let private dispatchInputs : (int * int) list =
        [ 1, 0 ; Int32.MaxValue, 1 ; Int32.MinValue, -1 ; 1, 1 ]

    /// The full names of the exceptions each of `runners` of `<ns>.Runners` in `image` lets escape on
    /// the real runtime, over `dispatchInputs`.
    let private dispatchOnRealRuntime
        (ns : string)
        (image : byte[])
        (runners : string list)
        : Map<string, Set<string>>
        =
        let context = System.Runtime.Loader.AssemblyLoadContext (ns, isCollectible = true)

        try
            let ty = context.LoadFromStream(new MemoryStream (image)).GetType (ns + ".Runners")

            runners
            |> List.map (fun name ->
                let thrown =
                    dispatchInputs
                    |> List.choose (fun (a, b) ->
                        try
                            ty.GetMethod(name).Invoke ((null : obj), [| box a ; box b |]) |> ignore<obj>
                            None
                        with :? TargetInvocationException as e ->
                            Some (e.InnerException.GetType().FullName)
                    )
                    |> Set.ofList

                name, thrown
            )
            |> Map.ofList
        finally
            context.Unload ()

    /// What is wrong with the analysis's answers for `cases` of `<ns>.Runners` in `fixture`, each a
    /// runner, which of `DivideByZeroException` and `OverflowException` it must report, and what it
    /// must claim, given what `runtime` says each lets escape on the real runtime.
    let private dispatchFailures
        (fixture : DumpedAssembly)
        (ns : string)
        (runtime : Map<string, Set<string>>)
        (cases : (string * Set<string> * DispatchClaim) list)
        (analysis : EscapeAnalysisState)
        : EscapeAnalysisState * string list
        =
        let arithmetic = Set.ofList [ dividesByZero ; overflows ]
        let mutable analysis = analysis

        let failures =
            [
                for name, reported, claim in cases do
                    let next, escapes =
                        EscapeAnalysis.escapes analysis (methodNamed fixture (ns + ".Runners") name)

                    analysis <- next

                    let shown = render analysis escapes

                    let shownArithmetic = arithmetic |> Set.filter (fun ty -> shown.Contains ("=" + ty))

                    let describe () =
                        $"%s{name}: %A{Set.toList shown}, unknown %b{escapes.Unknown}; the runtime raised %A{Set.toList runtime.[name]}"

                    // The fixture exercises what it claims to: every exception the receiver's
                    // method can raise is raised by one of the inputs, unless the analysis cannot
                    // tell it from one it can.
                    if not (Set.isSubset runtime.[name] reported) then
                        yield $"%s{describe ()}, more than the case expects"

                    if claim <> DispatchClaim.SoundOnly && runtime.[name].IsEmpty <> reported.IsEmpty then
                        yield $"%s{describe ()}, but the case expects %A{Set.toList reported}"

                    // Everything that escapes on the real runtime is reported.
                    if not escapes.Unknown then
                        for thrown in runtime.[name] do
                            if not (shown.Contains ("=" + thrown)) then
                                yield $"%s{describe ()} lacks %s{thrown}"

                    match claim with
                    | DispatchClaim.Precise ->
                        if escapes.Unknown || shownArithmetic <> reported then
                            yield
                                $"%s{describe ()}; expected exactly %A{Set.toList reported} of the arithmetic exceptions, and nothing unknown"
                    | DispatchClaim.Unknown ->
                        if not escapes.Unknown then
                            yield $"%s{describe ()}; expected unknown"
                    | DispatchClaim.SoundOnly -> ()
            ]

        analysis, failures

    [<Test>]
    let ``a constrained call on a type variable runs what each closed instantiation supplies`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let image =
            Roslyn.compileAssembly "Dispatch" OutputKind.DynamicallyLinkedLibrary [] [ dispatchSource ]

        let fixture =
            Assembly.read loggerFactory (Some "Dispatch.dll") (new MemoryStream (image))

        // Each runner, what the analysis must report of `DivideByZeroException` and
        // `OverflowException`, and what it must claim.
        let cases =
            [
                for shape in probeShapes do
                    for receiver in probeReceivers do
                        let reported =
                            receiver.Raises
                            |> List.filter (fun raised -> not (List.contains raised shape.Absorbs))

                        yield $"%s{shape.Name}_%s{receiver.Name}", Set.ofList reported, receiver.Claim
                yield "Hash_HashDivides", Set.ofList [ dividesByZero ; overflows ], DispatchClaim.Precise
                yield "Hash_NoHash", Set.empty, DispatchClaim.SoundOnly
                yield "Grow_Divides", Set.ofList [ dividesByZero ; overflows ], DispatchClaim.SoundOnly
            ]

        let runtime =
            cases
            |> List.map (fun (name, _, _) -> name)
            |> dispatchOnRealRuntime "Dispatch" image

        let arithmetic = Set.ofList [ dividesByZero ; overflows ]

        let analysis, failures =
            dispatchFailures fixture "Dispatch" runtime cases (analysisOver [ fixture ] id)

        // A generic definition asked about by itself has no instantiation to resolve a call on its
        // type variable against, but a call it spells without one is resolved all the same.
        let analysis, direct =
            EscapeAnalysis.escapes analysis (methodNamed fixture "Dispatch.Shapes" "Direct")

        let failures =
            if direct.Unknown then
                failures
            else
                failures
                @ [
                    $"Shapes.Direct, uninstantiated: %A{Set.toList (render analysis direct)}, expected unknown"
                ]

        let _, failures =
            ((analysis, failures), [ "ClosedInside" ; "HashInside" ])
            ||> List.fold (fun (analysis, failures) name ->
                let analysis, escapes =
                    EscapeAnalysis.escapes analysis (methodNamed fixture "Dispatch.Shapes" name)

                let shown = render analysis escapes

                if
                    escapes.Unknown
                    || arithmetic |> Set.filter (fun ty -> shown.Contains ("=" + ty)) <> arithmetic
                then
                    analysis,
                    failures
                    @ [
                        $"Shapes.%s{name}, uninstantiated: %A{Set.toList shown}, unknown %b{escapes.Unknown}; expected both arithmetic exceptions, and nothing unknown"
                    ]
                else
                    analysis, failures
            )

        match failures with
        | [] -> ()
        | failures -> failures |> String.concat Environment.NewLine |> failwith

    /// Callers of `Probe` through `callvirt`, with no `constrained.` prefix, whose receiver's class
    /// the IL may or may not decide, and a `throw` whose operand two paths make.
    let private receiverSource : string =
        """
using System;

namespace Receivers;

public class Base { public virtual int Probe(int a, int b) => unchecked(a + b); }
public class Divides : Base { public override int Probe(int a, int b) => a / b; }
public sealed class SealedAdds : Base { public override int Probe(int a, int b) => checked(a + b); }
public class OpenDivides : Base { public override int Probe(int a, int b) => a / b; }
public sealed class SealedGeneric<T> : Base { public override int Probe(int a, int b) => a / b; }

public interface IProbe { int Probe(int a, int b); }
public sealed class SealedViaInterface : IProbe { public int Probe(int a, int b) => a / b; }
public struct ValueAdds : IProbe { public int Probe(int a, int b) => checked(a + b); }

public class First : Exception { public First() : base("first") { } }
public class Second : Exception { public Second() : base("second") { } }

public static class Through
{
    public static int Sealed(SealedAdds x, int a, int b) => x.Probe(a, b);
    public static int Open(OpenDivides x, int a, int b) => x.Probe(a, b);
    public static int Generic<T>(T x, int a, int b) where T : Base => x.Probe(a, b);
    public static SealedGeneric<T> MakeSealed<T>() => new SealedGeneric<T>();
}

public static class Runners
{
    public static int NewObject(int a, int b) => new Divides().Probe(a, b);
    public static int SealedParameter(int a, int b) => Through.Sealed(new SealedAdds(), a, b);
    public static int OpenParameter(int a, int b) => Through.Open(new OpenDivides(), a, b);
    public static int Joined(int a, int b) => (b == 0 ? (Base)new Divides() : new SealedAdds()).Probe(a, b);
    public static int Interface(int a, int b) => ((IProbe)new SealedViaInterface()).Probe(a, b);
    public static int BoxedStruct(int a, int b) => ((IProbe)new ValueAdds()).Probe(a, b);
    public static int GenericSealed(int a, int b) => Through.Generic(new SealedAdds(), a, b);
    public static int GenericOpen(int a, int b) => Through.Generic(new OpenDivides(), a, b);
    // The receiver's type is the callee's return type, spelled with the callee's type variable.
    public static int GenericReturned(int a, int b) => Through.MakeSealed<int>().Probe(a, b);
    public static int ThrowJoined(int a, int b) =>
        throw (b == 0 ? (Exception)new First() : new Second());
}
"""

    [<Test>]
    let ``a callvirt runs the override of the receiver's class where the IL decides that class`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let image =
            Roslyn.compileAssembly "Receivers" OutputKind.DynamicallyLinkedLibrary [] [ receiverSource ]

        let fixture =
            Assembly.read loggerFactory (Some "Receivers.dll") (new MemoryStream (image))

        let both = Set.ofList [ dividesByZero ; overflows ]

        let cases =
            [
                "NewObject", both, DispatchClaim.Precise
                "SealedParameter", Set.singleton overflows, DispatchClaim.Precise
                "OpenParameter", both, DispatchClaim.Unknown
                "Joined", both, DispatchClaim.Precise
                "Interface", both, DispatchClaim.Precise
                "BoxedStruct", Set.singleton overflows, DispatchClaim.Precise
                "GenericSealed", Set.singleton overflows, DispatchClaim.Precise
                "GenericOpen", both, DispatchClaim.Unknown
                "GenericReturned", both, DispatchClaim.SoundOnly
            ]

        let runtime =
            "ThrowJoined" :: (cases |> List.map (fun (name, _, _) -> name))
            |> dispatchOnRealRuntime "Receivers" image

        let analysis, failures =
            dispatchFailures fixture "Receivers" runtime cases (analysisOver [ fixture ] id)

        // A `throw` of what either arm of a join makes raises exactly what each arm makes.
        let joined = Set.ofList [ "Receivers.First" ; "Receivers.Second" ]

        let analysis, escapes =
            EscapeAnalysis.escapes analysis (methodNamed fixture "Receivers.Runners" "ThrowJoined")

        let shown = render analysis escapes
        let raised = runtime.["ThrowJoined"]

        let failures =
            if
                raised <> joined
                || escapes.Unknown
                || not (joined |> Set.forall (fun ty -> shown.Contains ("=" + ty)))
                || shown |> Set.exists (fun ty -> ty.StartsWith "<:")
            then
                failures
                @ [
                    $"ThrowJoined: %A{Set.toList shown}, unknown %b{escapes.Unknown}; the runtime raised %A{Set.toList raised}"
                ]
            else
                failures

        match failures with
        | [] -> ()
        | failures -> failures |> String.concat Environment.NewLine |> failwith

    /// The receivers of the static dispatch fixture, and which of `DivideByZeroException` and
    /// `OverflowException` the `Probe` each supplies raises. A static method has no receiver object,
    /// so the type a `constrained.` prefix names decides what runs, even a class others derive from.
    let private staticReceivers : DispatchReceiver list =
        [
            {
                Name = "Quiet"
                Claim = DispatchClaim.Precise
                Raises = []
            }
            {
                Name = "Adds"
                Claim = DispatchClaim.Precise
                Raises = [ overflows ]
            }
            {
                Name = "Divides"
                Claim = DispatchClaim.Precise
                Raises = [ dividesByZero ; overflows ]
            }
            {
                Name = "ExplicitAdds"
                Claim = DispatchClaim.Precise
                Raises = [ overflows ]
            }
            {
                Name = "UsesDefault"
                Claim = DispatchClaim.Precise
                Raises = [ dividesByZero ; overflows ]
            }
            {
                Name = "OpenAdds"
                Claim = DispatchClaim.Precise
                Raises = [ overflows ]
            }
            {
                Name = "InheritsAdds"
                Claim = DispatchClaim.Precise
                Raises = [ overflows ]
            }
            {
                Name = "ReimplementsQuiet"
                Claim = DispatchClaim.Precise
                Raises = []
            }
            // Hides `OpenAdds.Probe` without re-listing the interface, so the base class's runs.
            {
                Name = "ShadowsQuiet"
                Claim = DispatchClaim.Precise
                Raises = [ overflows ]
            }
            {
                Name = "AbstractAdds"
                Claim = DispatchClaim.Precise
                Raises = [ overflows ]
            }
            {
                Name = "OpenUsesDefault"
                Claim = DispatchClaim.Precise
                Raises = [ dividesByZero ; overflows ]
            }
            // Implements `IStatic` through `IShadow`, whose `new static virtual Probe` is another
            // method, so `IStatic`'s own default body runs.
            {
                Name = "ThroughShadow"
                Claim = DispatchClaim.Precise
                Raises = [ dividesByZero ; overflows ]
            }
        ]

    let private staticShapes : DispatchShape list =
        [
            {
                Name = "Direct"
                Call = fun r -> $"Shapes.Direct<%s{r}>(a, b)"
                Absorbs = []
            }
            {
                Name = "OnType"
                Call = fun r -> $"Holder<%s{r}>.Call(a, b)"
                Absorbs = []
            }
            {
                Name = "Relayed"
                Call = fun r -> $"Shapes.Relayed<%s{r}>(a, b)"
                Absorbs = []
            }
            {
                Name = "TypeToMethod"
                Call = fun r -> $"Holder<%s{r}>.Relay(a, b)"
                Absorbs = []
            }
            {
                Name = "Caught"
                Call = fun r -> $"Shapes.Caught<%s{r}>(a, b)"
                Absorbs = [ dividesByZero ]
            }
            {
                Name = "Wrapped"
                Call = fun r -> $"Shapes.Direct<Wrapper<%s{r}>>(a, b)"
                Absorbs = []
            }
            {
                Name = "Rethrown"
                Call = fun r -> $"Shapes.Rethrown<%s{r}>(a, b)"
                Absorbs = []
            }
        ]

    /// The receivers, the generic methods that call the static virtual `Probe` on a type variable,
    /// and the non-generic runners, one per shape and receiver, that close each instantiation.
    let private staticDispatchSource : string =
        let declarations =
            """
using System;

namespace StaticDispatch;

public interface IStatic
{
    static virtual int Probe(int a, int b) => a / b;
}

public struct Quiet : IStatic { public static int Probe(int a, int b) => unchecked(a + b); }
public struct Adds : IStatic { public static int Probe(int a, int b) => checked(a + b); }
public struct Divides : IStatic { public static int Probe(int a, int b) => a / b; }
public struct ExplicitAdds : IStatic { static int IStatic.Probe(int a, int b) => checked(a + b); }
public struct UsesDefault : IStatic { }
public class OpenAdds : IStatic { public static int Probe(int a, int b) => checked(a + b); }
public class InheritsAdds : OpenAdds { }
public class ReimplementsQuiet : OpenAdds, IStatic { static int IStatic.Probe(int a, int b) => unchecked(a + b); }
public class ShadowsQuiet : OpenAdds { public static new int Probe(int a, int b) => unchecked(a + b); }
public abstract class AbstractAdds : IStatic { public static int Probe(int a, int b) => checked(a + b); }
public class OpenUsesDefault : IStatic { }
public interface IShadow : IStatic { static new virtual int Probe(int a, int b) => checked(a + b); }
public class ThroughShadow : IShadow { }

public struct Wrapper<T> : IStatic where T : IStatic
{
    public static int Probe(int a, int b) => T.Probe(a, b);
}

public interface IVariant<in T>
{
    static abstract int Probe(int a, int b);
}

public class VariantBase : IVariant<string> { static int IVariant<string>.Probe(int a, int b) => checked(a + b); }
public class VariantDerived : VariantBase, IVariant<object> { static int IVariant<object>.Probe(int a, int b) => a / b; }

public interface IExact<in T> { static abstract int Probe(int a, int b); }
public interface IExactString : IExact<string> { static int IExact<string>.Probe(int a, int b) => a / b; }
public interface IExactObject : IExactString, IExact<object> { static int IExact<object>.Probe(int a, int b) => checked(a + b); }
public class ExactDefault : IExactObject { }

public class ExactBoth : IExact<string>, IExact<object>
{
    static int IExact<string>.Probe(int a, int b) => a / b;
    static int IExact<object>.Probe(int a, int b) => checked(a + b);
}


public static class Shapes
{
    public static int Direct<T>(int a, int b) where T : IStatic => T.Probe(a, b);
    public static int Relayed<T>(int a, int b) where T : IStatic => Direct<T>(a, b);

    public static int Caught<T>(int a, int b) where T : IStatic
    {
        try { return T.Probe(a, b); }
        catch (DivideByZeroException) { return 0; }
    }

    // Catches everything, so what escapes is what the `throw;` re-raises.
    public static int Rethrown<T>(int a, int b) where T : IStatic
    {
        try { return T.Probe(a, b); }
        catch (Exception) { throw; }
    }

    public static int Variant<T>(int a, int b) where T : IVariant<string> => T.Probe(a, b);
    public static int Exact<T>(int a, int b) where T : IExact<string> => T.Probe(a, b);
}

public static class Holder<T> where T : IStatic
{
    public static int Call(int a, int b) => T.Probe(a, b);
    public static int Relay(int a, int b) => Shapes.Direct<T>(a, b);
}
"""

        let runners =
            [
                for shape in staticShapes do
                    for receiver in staticReceivers do
                        yield
                            $"    public static int %s{shape.Name}_%s{receiver.Name}(int a, int b) => %s{shape.Call receiver.Name};"
                yield "    public static int Direct_IStatic(int a, int b) => Shapes.Direct<IStatic>(a, b);"
                yield "    public static int Variant_VariantBase(int a, int b) => Shapes.Variant<VariantBase>(a, b);"
                yield
                    "    public static int Variant_VariantDerived(int a, int b) => Shapes.Variant<VariantDerived>(a, b);"
                yield "    public static int Exact_ExactDefault(int a, int b) => Shapes.Exact<ExactDefault>(a, b);"
                yield "    public static int Exact_ExactBoth(int a, int b) => Shapes.Exact<ExactBoth>(a, b);"
            ]
            |> String.concat "\n"

        declarations + "\npublic static class Runners\n{\n" + runners + "\n}\n"

    [<Test>]
    let ``a constrained call of a static virtual runs what the type it names supplies`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let image =
            Roslyn.compileAssembly "StaticDispatch" OutputKind.DynamicallyLinkedLibrary [] [ staticDispatchSource ]

        let fixture =
            Assembly.read loggerFactory (Some "StaticDispatch.dll") (new MemoryStream (image))

        let cases =
            [
                for shape in staticShapes do
                    for receiver in staticReceivers do
                        let reported =
                            receiver.Raises
                            |> List.filter (fun raised -> not (List.contains raised shape.Absorbs))

                        yield $"%s{shape.Name}_%s{receiver.Name}", Set.ofList reported, receiver.Claim
                // An interface named as the type runs its own default body.
                yield "Direct_IStatic", Set.ofList [ dividesByZero ; overflows ], DispatchClaim.Precise
                yield "Variant_VariantBase", Set.ofList [ overflows ], DispatchClaim.Precise
                // CoreCLR looks for a variant match on each class before its base, so the derived
                // class's implementation through `IVariant<object>` runs, not the base class's
                // through `IVariant<string>`.
                yield "Variant_VariantDerived", Set.ofList [ dividesByZero ; overflows ], DispatchClaim.Precise
                // CoreCLR looks for exactly the call's instantiation before a variance-compatible
                // one, for a default body and for a MethodImpl alike, so `IExact<string>`'s runs.
                yield "Exact_ExactDefault", Set.ofList [ dividesByZero ; overflows ], DispatchClaim.Precise
                yield "Exact_ExactBoth", Set.ofList [ dividesByZero ; overflows ], DispatchClaim.Precise
            ]

        let runtime =
            cases
            |> List.map (fun (name, _, _) -> name)
            |> dispatchOnRealRuntime "StaticDispatch" image

        let analysis, failures =
            dispatchFailures fixture "StaticDispatch" runtime cases (analysisOver [ fixture ] id)

        // A generic definition asked about by itself has no type to resolve the call against.
        let analysis, direct =
            EscapeAnalysis.escapes analysis (methodNamed fixture "StaticDispatch.Shapes" "Direct")

        let failures =
            if direct.Unknown then
                failures
            else
                failures
                @ [
                    $"Shapes.Direct, uninstantiated: %A{Set.toList (render analysis direct)}, expected unknown"
                ]

        match failures with
        | [] -> ()
        | failures -> failures |> String.concat Environment.NewLine |> failwith

    [<Test>]
    let ``a constrained call of a static virtual with two equally specific default bodies raises the ambiguity in the caller``
        ()
        : unit
        =
        let _, loggerFactory = LoggerFactory.makeTest ()

        // C# refuses a type whose interfaces' default bodies conflict, so the client is compiled
        // against a library in which only `IL` overrides `Probe`, and run against one in which
        // `IR` does too.
        let library (bothOverride : bool) : string =
            let right =
                if bothOverride then
                    "{ static int IStatic.Probe(int a, int b) => checked(a + b); }"
                else
                    "{ }"

            $"""
namespace StaticLib;

public interface IStatic {{ static virtual int Probe(int a, int b) => a / b; }}
public interface IL : IStatic {{ static int IStatic.Probe(int a, int b) => checked(a + b); }}
public interface IR : IStatic %s{right}
"""

        let client =
            """
using System;
using System.Runtime;
using StaticLib;

namespace StaticClient;

public struct S : IL, IR { }
public class C : IL, IR { }

public static class Shapes
{
    public static int Direct<T>(int a, int b) where T : IStatic => T.Probe(a, b);

    public static int Caught<T>(int a, int b) where T : IStatic
    {
        try { return T.Probe(a, b); }
        catch (AmbiguousImplementationException) { return -1; }
    }
}

public static class Runners
{
    public static int Direct_S(int a, int b) => Shapes.Direct<S>(a, b);
    public static int Direct_C(int a, int b) => Shapes.Direct<C>(a, b);
    public static int Caught_S(int a, int b) => Shapes.Caught<S>(a, b);
    public static int Caught_C(int a, int b) => Shapes.Caught<C>(a, b);
}
"""

        let compile (name : string) (references : byte[] list) (text : string) : byte[] =
            Roslyn.compileAssembly
                name
                OutputKind.DynamicallyLinkedLibrary
                (references
                 |> List.map (fun image -> MetadataReference.CreateFromImage (ImmutableArray.CreateRange image)))
                [ text ]

        let clientImage =
            compile "StaticClient" [ compile "StaticLib" [] (library false) ] client

        let libraryImage = compile "StaticLib" [] (library true)
        let runners = [ "Direct_S" ; "Direct_C" ; "Caught_S" ; "Caught_C" ]

        // What each runner lets escape on the real runtime, with the library in which both
        // interfaces override `Probe`.
        let runtime =
            let context =
                System.Runtime.Loader.AssemblyLoadContext ("StaticClient", isCollectible = true)

            try
                let lib = context.LoadFromStream (new MemoryStream (libraryImage))

                context.add_Resolving (fun _ name -> if name.Name = "StaticLib" then lib else null)

                let ty =
                    context.LoadFromStream(new MemoryStream (clientImage)).GetType "StaticClient.Runners"

                runners
                |> List.map (fun name ->
                    let thrown =
                        try
                            ty.GetMethod(name).Invoke ((null : obj), [| box 1 ; box 1 |]) |> ignore<obj>
                            None
                        with :? TargetInvocationException as e ->
                            Some (e.InnerException.GetType().FullName)

                    name, thrown
                )
                |> Map.ofList
            finally
                context.Unload ()

        let ambiguity = "System.Runtime.AmbiguousImplementationException"

        runtime
        |> shouldEqual (
            Map.ofList
                [
                    "Direct_S", Some ambiguity
                    "Direct_C", Some ambiguity
                    "Caught_S", None
                    "Caught_C", None
                ]
        )

        let read (name : string) (image : byte[]) : DumpedAssembly =
            Assembly.read loggerFactory (Some $"%s{name}.dll") (new MemoryStream (image))

        let clientAssembly = read "StaticClient" clientImage
        let libraryAssembly = read "StaticLib" libraryImage

        let libraryReference =
            clientAssembly.AssemblyReferences.Values
            |> Seq.find (fun r -> r.Name.Name = "StaticLib")

        let mutable analysis =
            analysisOver
                [ clientAssembly ; libraryAssembly ]
                (fun loaded -> fst (loaded.WithBoundReference libraryReference libraryAssembly))

        let failures =
            [
                for name in runners do
                    let next, escapes =
                        EscapeAnalysis.escapes analysis (methodNamed clientAssembly "StaticClient.Runners" name)

                    analysis <- next
                    let shown = render analysis escapes

                    // The runtime throws in the caller, whose own `catch` stops it.
                    let expected = runtime.[name].IsSome

                    if escapes.Unknown || shown.Contains ("=" + ambiguity) <> expected then
                        yield
                            $"%s{name}: %A{Set.toList shown}, unknown %b{escapes.Unknown}; expected the ambiguity to escape: %b{expected}, and nothing unknown"
            ]

        match failures with
        | [] -> ()
        | failures -> failures |> String.concat Environment.NewLine |> failwith

    [<Test>]
    let ``a constrained call of a static virtual runs the initializer of the type it lands on`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let source =
            """
using System;

namespace StaticInit;

public interface IStatic { static abstract int Probe(int a, int b); }

// The interface has no initializer; the implementing type's fails.
public class FailsInit : IStatic
{
    static readonly int Zero;
    static FailsInit() { Zero = 1 / Zero; }
    public static int Probe(int a, int b) => unchecked(a + b);
}

public static class Shapes
{
    public static int Direct<T>(int a, int b) where T : IStatic => T.Probe(a, b);
}

public static class Runners
{
    public static int Direct_FailsInit(int a, int b) => Shapes.Direct<FailsInit>(a, b);
}
"""

        let image =
            Roslyn.compileAssembly "StaticInit" OutputKind.DynamicallyLinkedLibrary [] [ source ]

        let fixture =
            Assembly.read loggerFactory (Some "StaticInit.dll") (new MemoryStream (image))

        let initialization = "System.TypeInitializationException"

        dispatchOnRealRuntime "StaticInit" image [ "Direct_FailsInit" ]
        |> shouldEqual (Map.ofList [ "Direct_FailsInit", Set.singleton initialization ])

        let analysis, escapes =
            EscapeAnalysis.escapes
                (analysisOver [ fixture ] id)
                (methodNamed fixture "StaticInit.Runners" "Direct_FailsInit")

        let shown = render analysis escapes

        if not escapes.Unknown && not (shown.Contains ("=" + initialization)) then
            failwith $"Direct_FailsInit: %A{Set.toList shown}, unknown false; lacks %s{initialization}"

    [<Test>]
    let ``a constrained call landing in another module runs that module's initializer`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let contracts =
            """
namespace Contracts;

public interface IStatic { static abstract int Probe(int a, int b); }
public interface IProbe { int Probe(int a, int b); }
"""

        // The default bodies bind a token of their own module, which runs its initializer first.
        let defaults =
            """
namespace Defaults;

static class Init
{
    [System.Runtime.CompilerServices.ModuleInitializer]
    internal static void Run() => throw new System.InvalidOperationException();
}

static class Helper
{
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)]
    public static int Add(int a, int b) => a + b;
}

public interface IStaticDefault : Contracts.IStatic { static int Contracts.IStatic.Probe(int a, int b) => Helper.Add(a, b); }
public interface IInstanceDefault : Contracts.IProbe { int Contracts.IProbe.Probe(int a, int b) => Helper.Add(a, b); }
"""

        // Nothing here names Defaults' members; only dispatch reaches them.
        let client =
            """
namespace Client;

public struct StaticUser : Defaults.IStaticDefault { }
public struct InstanceUser : Defaults.IInstanceDefault { }

public static class Shapes
{
    public static int Static<T>(int a, int b) where T : Contracts.IStatic => T.Probe(a, b);
    public static int Instance<T>(T x, int a, int b) where T : Contracts.IProbe => x.Probe(a, b);

    public static int CaughtStatic<T>(int a, int b) where T : Contracts.IStatic
    {
        try { return T.Probe(a, b); }
        catch (System.TypeInitializationException) { return -1; }
    }
}

public static class Runners
{
    public static int Static() => Shapes.Static<StaticUser>(1, 2);
    public static int Instance() => Shapes.Instance(new InstanceUser(), 1, 2);
    public static int CaughtStatic() => Shapes.CaughtStatic<StaticUser>(1, 2);
}
"""

        let compile (name : string) (references : byte[] list) (text : string) : byte[] =
            Roslyn.compileAssembly
                name
                OutputKind.DynamicallyLinkedLibrary
                (references
                 |> List.map (fun image -> MetadataReference.CreateFromImage (ImmutableArray.CreateRange image)))
                [ text ]

        let contractsImage = compile "Contracts" [] contracts
        let defaultsImage = compile "Defaults" [ contractsImage ] defaults
        let clientImage = compile "Client" [ contractsImage ; defaultsImage ] client

        let initialization = "System.TypeInitializationException"

        // Each in a context of its own, since a module initializer that failed fails every later
        // binding the same way.
        let onRealRuntime (methodName : string) : string option =
            let context =
                new TestMethodReferenceResolution.ImagesContext (
                    Map.ofList
                        [
                            "Contracts", contractsImage
                            "Defaults", defaultsImage
                            "Client", clientImage
                        ]
                )

            try
                let runners =
                    context.LoadFromAssemblyName(AssemblyName "Client").GetType "Client.Runners"

                try
                    runners.GetMethod(methodName).Invoke ((null : obj), Array.empty<obj>)
                    |> ignore<obj>

                    None
                with :? TargetInvocationException as e ->
                    Some (e.InnerException.GetType().FullName)
            finally
                context.Unload ()

        let read (name : string) (image : byte[]) : DumpedAssembly =
            Assembly.read loggerFactory (Some $"%s{name}.dll") (new MemoryStream (image))

        let assemblies =
            [
                read "Contracts" contractsImage
                read "Defaults" defaultsImage
                read "Client" clientImage
            ]

        let clientAssembly = List.last assemblies

        let bind (loaded : LoadedAssemblies) : LoadedAssemblies =
            (loaded, clientAssembly.AssemblyReferences.Values)
            ||> Seq.fold (fun loaded reference ->
                match assemblies |> List.tryFind (fun a -> a.Name.Name = reference.Name.Name) with
                | Some target -> fst (loaded.WithBoundReference reference target)
                | None -> loaded
            )

        let mutable analysis = analysisOver assemblies bind

        let failures =
            [
                for methodName, escapes in [ "Static", true ; "Instance", true ; "CaughtStatic", false ] do
                    let thrown = onRealRuntime methodName

                    if thrown <> (if escapes then Some initialization else None) then
                        yield $"%s{methodName} on the real runtime: %A{thrown}"

                    let next, summary =
                        EscapeAnalysis.escapes analysis (methodNamed clientAssembly "Client.Runners" methodName)

                    analysis <- next
                    let shown = render analysis summary

                    if summary.Unknown || shown.Contains ("=" + initialization) <> escapes then
                        yield
                            $"%s{methodName}: %A{Set.toList shown}, unknown %b{summary.Unknown}; expected %s{initialization} to escape: %b{escapes}, and nothing unknown"
            ]

        match failures with
        | [] -> ()
        | failures -> failures |> String.concat Environment.NewLine |> failwith

    /// `Run.Call(int, int)`, a non-generic method whose `constrained. Dyn callvirt IProbe::Probe`
    /// names a sealed class that does not implement `IProbe`, though `IProbe` gives `Probe` a
    /// default body. `Dyn` implements `IDynamicInterfaceCastable`, whose `GetInterfaceImplementation`
    /// divides by zero. C# writes a `constrained.` call only on a type that implements the method,
    /// so the IL is emitted directly.
    let private emitDynamicReceiver () : byte[] =
        let builder =
            System.Reflection.Emit.PersistedAssemblyBuilder (AssemblyName "Dynamic", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "Dynamic"

        let probeInterface =
            modul.DefineType ("IProbe", TypeAttributes.Public ||| TypeAttributes.Interface ||| TypeAttributes.Abstract)

        let probe =
            probeInterface.DefineMethod (
                "Probe",
                MethodAttributes.Public
                ||| MethodAttributes.Virtual
                ||| MethodAttributes.HideBySig
                ||| MethodAttributes.NewSlot,
                typeof<int>,
                [| typeof<int> ; typeof<int> |]
            )

        do
            // The default body: `a + b`, which raises nothing.
            let il = probe.GetILGenerator ()
            il.Emit System.Reflection.Emit.OpCodes.Ldarg_1
            il.Emit System.Reflection.Emit.OpCodes.Ldarg_2
            il.Emit System.Reflection.Emit.OpCodes.Add
            il.Emit System.Reflection.Emit.OpCodes.Ret

        probeInterface.CreateType () |> ignore<Type>

        let dyn =
            modul.DefineType (
                "Dyn",
                TypeAttributes.Public ||| TypeAttributes.Sealed ||| TypeAttributes.Class,
                typeof<obj>,
                [| typeof<System.Runtime.InteropServices.IDynamicInterfaceCastable> |]
            )

        let constructor = dyn.DefineDefaultConstructor MethodAttributes.Public

        let implementing =
            MethodAttributes.Public
            ||| MethodAttributes.Virtual
            ||| MethodAttributes.Final
            ||| MethodAttributes.HideBySig
            ||| MethodAttributes.NewSlot

        do
            let isImplemented =
                dyn.DefineMethod (
                    "IsInterfaceImplemented",
                    implementing,
                    typeof<bool>,
                    [| typeof<RuntimeTypeHandle> ; typeof<bool> |]
                )

            let il = isImplemented.GetILGenerator ()
            il.Emit System.Reflection.Emit.OpCodes.Ldc_I4_1
            il.Emit System.Reflection.Emit.OpCodes.Ret

            let getImplementation =
                dyn.DefineMethod (
                    "GetInterfaceImplementation",
                    implementing,
                    typeof<RuntimeTypeHandle>,
                    [| typeof<RuntimeTypeHandle> |]
                )

            // `1 / 0`, then a value of the right type for the verifier's sake.
            let il = getImplementation.GetILGenerator ()
            il.Emit System.Reflection.Emit.OpCodes.Ldc_I4_1
            il.Emit System.Reflection.Emit.OpCodes.Ldc_I4_0
            il.Emit System.Reflection.Emit.OpCodes.Div
            il.Emit System.Reflection.Emit.OpCodes.Pop
            il.Emit (System.Reflection.Emit.OpCodes.Ldarg_1)
            il.Emit System.Reflection.Emit.OpCodes.Ret

        dyn.CreateType () |> ignore<Type>

        let run =
            modul.DefineType ("Run", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        do
            let call =
                run.DefineMethod (
                    "Call",
                    MethodAttributes.Public ||| MethodAttributes.Static,
                    typeof<int>,
                    [| typeof<int> ; typeof<int> |]
                )

            let il = call.GetILGenerator ()
            let receiver = il.DeclareLocal dyn
            il.Emit (System.Reflection.Emit.OpCodes.Newobj, constructor)
            il.Emit (System.Reflection.Emit.OpCodes.Stloc, receiver)
            il.Emit (System.Reflection.Emit.OpCodes.Ldloca, receiver)
            il.Emit System.Reflection.Emit.OpCodes.Ldarg_0
            il.Emit System.Reflection.Emit.OpCodes.Ldarg_1
            il.Emit (System.Reflection.Emit.OpCodes.Constrained, dyn)
            il.Emit (System.Reflection.Emit.OpCodes.Callvirt, probe)
            il.Emit System.Reflection.Emit.OpCodes.Ret

        run.CreateType () |> ignore<Type>

        use stream = new MemoryStream ()
        builder.Save stream
        stream.ToArray ()

    [<Test>]
    let ``a constrained call on a class that does not implement the method is not resolved`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let image = emitDynamicReceiver ()

        let assembly =
            Assembly.read loggerFactory (Some "Dynamic.dll") (new MemoryStream (image))

        // The real runtime asks `Dyn` for an implementation, rather than running the default body.
        let thrown =
            let context =
                System.Runtime.Loader.AssemblyLoadContext ("Dynamic", isCollectible = true)

            try
                let run = context.LoadFromStream(new MemoryStream (image)).GetType "Run"

                try
                    run.GetMethod("Call").Invoke ((null : obj), [| box 1 ; box 2 |]) |> ignore<obj>
                    None
                with :? TargetInvocationException as e ->
                    Some (e.InnerException.GetType().FullName)
            finally
                context.Unload ()

        thrown |> shouldEqual (Some "System.DivideByZeroException")

        let analysis, escapes =
            EscapeAnalysis.escapes (analysisOver [ assembly ] id) (methodNamed assembly "Run" "Call")

        if not escapes.Unknown then
            failwith
                $"Run.Call: %A{Set.toList (render analysis escapes)}, expected unknown: the receiver's class decides what runs"

    [<Test>]
    let ``a constrained call with two equally specific default bodies raises the ambiguity`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let assembly =
            Assembly.read
                loggerFactory
                (Some "Diamond.dll")
                (new MemoryStream (TestAmbiguousDefaultInterfaceDispatch.fabricate false))

        let analysis, escapes =
            EscapeAnalysis.escapes (analysisOver [ assembly ] id) (methodNamed assembly "Run" "Call")

        let shown = render analysis escapes

        if
            escapes.Unknown
            || not (shown.Contains "=System.Runtime.AmbiguousImplementationException")
        then
            failwith
                $"Run.Call: %A{Set.toList shown}, unknown %b{escapes.Unknown}; expected AmbiguousImplementationException, and nothing unknown"

    [<Test>]
    let ``a constrained call whose default bodies conflict only through variance is not called ambiguous`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let image =
            Roslyn.compileAssembly
                "Variant"
                OutputKind.DynamicallyLinkedLibrary
                []
                [ TestAmbiguousDefaultInterfaceDispatch.variantSource ]

        let assembly =
            Assembly.read loggerFactory (Some "Variant.dll") (new MemoryStream (image))

        // On the real runtime, one of the bodies runs and divides by zero.
        let analysis, escapes =
            EscapeAnalysis.escapes (analysisOver [ assembly ] id) (methodNamed assembly "Run" "Go")

        let shown = render analysis escapes

        if not escapes.Unknown && not (shown.Contains "=System.DivideByZeroException") then
            failwith $"Run.Go: %A{Set.toList shown}, unknown false; lacks DivideByZeroException"

    /// `TestCrossAssemblyReabstraction` runs the same images, and pins that the real runtime throws
    /// `EntryPointNotFoundException` from each of these calls.
    [<Test>]
    let ``a constrained call landing on a reabstraction raises EntryPointNotFoundException`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let compiled =
            CrossAssemblyHarness.compileAssemblies
                [
                    TestCrossAssemblyReabstraction.libraryBefore
                    TestCrossAssemblyReabstraction.entryAssembly
                    TestCrossAssemblyReabstraction.libraryAfter
                ]

        let read (name : string) : DumpedAssembly =
            Assembly.read loggerFactory (Some $"%s{name}.dll") (new MemoryStream (compiled.[name]))

        let library = read TestCrossAssemblyReabstraction.libraryName
        let entry = read TestCrossAssemblyReabstraction.entryName

        let analysis = analysisOver [ library ; entry ] id

        let failures =
            (([], analysis),
             [
                 "ConstrainedOnValueType"
                 "ConstrainedOnSealedClass"
                 "StaticOnValueType"
                 "InterfaceCallOnNewObject"
             ])
            ||> List.fold (fun (failures, analysis) methodName ->
                let analysis, escapes =
                    EscapeAnalysis.escapes analysis (methodNamed entry "ReabstractionEntry.Cases" methodName)

                let shown = render analysis escapes

                if escapes.Unknown || not (shown.Contains "=System.EntryPointNotFoundException") then
                    $"Cases.%s{methodName}: %A{Set.toList shown}, unknown %b{escapes.Unknown}; expected EntryPointNotFoundException, and nothing unknown"
                    :: failures,
                    analysis
                else
                    failures, analysis
            )
            |> fst

        if not failures.IsEmpty then
            failwith (String.concat "\n" (List.rev failures))
