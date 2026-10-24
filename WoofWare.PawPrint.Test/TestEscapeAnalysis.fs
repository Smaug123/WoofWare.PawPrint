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

    // The monitor is that of the object `newobj` made.
    public static void OnNew() => new Locking().Instance();
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

    // An FCall the contract table describes, which reads the thread's ID.
    public static int ThreadId() => Environment.CurrentManagedThreadId;

    // An FCall no contract describes.
    public static int Collections() => GC.CollectionCount(0);

    // A QCall the contract table describes, which allocates the thread's object on first use.
    public static System.Threading.Thread Current() => System.Threading.Thread.CurrentThread;

    // An object's RuntimeType, which a QCall allocates on first use; a null object raises before
    // that. The QCall finishes loading a type whose load failed, but an object's type is fully
    // loaded.
    public static Type TypeOf(object o) => o.GetType();

    // That RuntimeType, a sealed class, whose override a virtual call on it runs: `GetArrayRank`
    // throws ArgumentException for a type that is not an array.
    public static int RankOf(object o) => o.GetType().GetArrayRank();

    // A type's module, which CoreLib reads through an FCall the contract table describes, or makes
    // on first use through a QCall it describes; and the type's metadata token, another such FCall.
    public static System.Reflection.Module ModuleOf() => typeof(Natives).Module;
    public static int TokenOf() => typeof(Natives).MetadataToken;

    // Whether a type is a generic parameter, an FCall the contract table describes; asked of one
    // that is not, its parameter attributes throw before reading metadata through two more.
    public static bool IsParameter() => typeof(Natives).IsGenericParameter;
    public static System.Reflection.GenericParameterAttributes ParameterAttributes() =>
        typeof(Natives).GenericParameterAttributes;

    // A P/Invoke into libSystem.Native, the framework's own shim.
    public static long Timestamp() => System.Diagnostics.Stopwatch.GetTimestamp();

    // The shadow `System.MathF` in this assembly, not CoreLib's.
    public static float Impostor() => MathF.Sin(1f);

    // A String constructor, an FCall whose implementation is `String.Ctor(char, int)`'s IL.
    public static string Repeat(int count) => new string('a', count);
}

// CoreLib's exceptions take their messages from CoreLib's resource lookup.
public static class Messages
{
    public static object Construct() => new InvalidOperationException();

    public static void Swallowed()
    {
        try { _ = new InvalidOperationException(); }
        catch { }
    }

    public static void Rethrown()
    {
        try { _ = new InvalidOperationException(); }
        catch { throw; }
    }

    // A clause for a class that RuntimeWrappedException does not derive from re-raises what the
    // analysis cannot name as that class or a subclass.
    public static void RethrownAsTimeout()
    {
        try { _ = new InvalidOperationException(); }
        catch (TimeoutException) { throw; }
    }

    // The rethrow re-raises only a TimeoutException, which the outer clause absorbs.
    public static void RethrownThenAbsorbed()
    {
        try
        {
            try { _ = new InvalidOperationException(); }
            catch (TimeoutException) { throw; }
            catch { }
        }
        catch (TimeoutException) { }
    }

    public static void OutOfMemoryCaught()
    {
        try { _ = new InvalidOperationException(); }
        catch (OutOfMemoryException) { }
    }

    // The shadow `System.SR` in this assembly, not CoreLib's.
    public static string Impostor() => SR.InternalGetResourceString("key");
}

// The runtime makes the exception of each of these faults by running its parameterless
// constructor, which looks up its message.
public static class Faults
{
    public sealed class Cell { public int Value; }

    public static int Field(Cell cell) => cell.Value;

    public static int Element(int[] array, int index) => array[index];

    public static string Cast(object value) => (string)value;

    public static int Sum(int x, int y) => checked(x + y);

    public static int Quotient(int x, int y) => x / y;

    // A multidimensional array's accessor is a method the runtime supplies, which faults as
    // `ldelem` does.
    public static int Grid(int[,] grid) => grid[0, 0];

    // Running out of memory raises an object the runtime made in advance, so nothing is looked up.
    public static object Allocate() => new object();
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

    // The inner `throw;` rethrows what the outer one did.
    public static void RethrowsARethrow(Animal a)
    {
        try { CallsVirtual(a); }
        catch (Exception)
        {
            try { throw; }
            catch (Exception) { throw; }
        }
    }

    // The first clause absorbs everything, so the second, tried after it, rethrows nothing.
    public static void RethrowsWhatAnEarlierClauseLeaves(Animal a)
    {
        try { CallsVirtual(a); }
        catch (Exception) { }
        catch { throw; }
    }

    // The rethrow re-raises only what its own protected block raises, not the virtual call after it,
    // which its own clause absorbs.
    public static int RethrowsBesideAnAbsorbedCall(Animal a, int x, int y)
    {
        int r;
        try { r = Divide(x, y); }
        catch (Exception) { throw; }
        try { _ = a.Speak(); }
        catch (Exception) { }
        return r;
    }

    // What the first call raises is absorbed; only the second's escapes.
    public static void OneOfTwoCaught(object o, Animal a)
    {
        try { _ = o.ToString(); }
        catch (Exception) { }
        _ = a.Speak();
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

    // Not virtual, so a call names it whatever the receiver's class; the class decides `Speak`.
    public string SpeakVia() => Speak();

    public virtual string Echo() => "";
}

public class Bark : Exception { }

public class Hiss : Exception { }

public class Dog : Animal
{
    public override string Speak() => throw new Bark();

    // Only ever run on a Dog.
    public override string Echo() => Speak();
}

public sealed class Kennel
{
    public Animal Make() => new Dog();
}

public sealed class Puppy : Animal
{
    public override string Speak() => throw new Bark();
}

public interface ICaller
{
    string Call(Animal a);
}

public struct Caller : ICaller
{
    public string Call(Animal a) => a.Speak();
}

public class Cat : Animal
{
    public override string Speak() => throw new Hiss();
}

// A constructor calls a virtual method on the object being constructed.
public class Greeter
{
    public Greeter() { Greet(); }
    public virtual void Greet() { }
}

public class LoudGreeter : Greeter
{
    public override void Greet() => throw new Bark();
}

// What a caller passes decides what the callee's virtual calls run.
public static class ContextCases
{
    // Asked about by itself, a parameter holds an object of its declared class or a subclass;
    // this one is sealed.
    public static string CallsSealedParameter(Puppy p) => p.Speak();

    public static string PassesDog() => Cases.CallsVirtual(new Dog());

    public static void PassesBoth()
    {
        Cases.CallsVirtual(new Dog());
        Cases.CallsVirtual(new Cat());
    }

    public static string CallsThroughThis() => new Dog().SpeakVia();

    static Animal Identity(Animal a) => a;

    public static string ReturnsWhatItIsPassed() => Identity(new Dog()).Speak();

    // The call returning the dog is on a receiver that may be null, so it can fault too.
    public static string ReturnsWhatItMakes(Kennel kennel) => kennel.Make().Speak();

    static string Forwards(Animal a) => Cases.CallsVirtual(a);

    public static string PassesDogTwoDeep() => Forwards(new Dog());

    static string Recurses(Animal a, int n)
    {
        if (n > 0) { Recurses(new Cat(), n - 1); }
        return a.Speak();
    }

    public static string RecursesWithCat() => Recurses(new Dog(), 1);

    public static string PassesNull() => Cases.CallsVirtual(null);

    // The parameter no longer holds what the caller passed.
    static string Reassigns(Animal a, Animal other)
    {
        a = other;
        return a.Speak();
    }

    public static string PassesDogToReassigned(Animal other) => Reassigns(new Dog(), other);

    static void Touch(ref Animal a) { }

    // Code given the parameter's address may store anything in it.
    static string TakesAddress(Animal a)
    {
        Touch(ref a);
        return a.Speak();
    }

    public static string PassesDogByAddress() => TakesAddress(new Dog());

    // Each class the receiver may be of is what `this` is in the method it dispatches to.
    public static string EchoesEither(bool b) => (b ? (Animal)new Dog() : new Cat()).Echo();

    static string Via<T>(T caller, Animal a) where T : ICaller => caller.Call(a);

    // The `constrained.` call passes on what it is given.
    public static string PassesDogViaConstrained() => Via(new Caller(), new Dog());

    public static Greeter ConstructsLoud() => new LoudGreeter();

    public static Greeter ConstructsQuiet() => new Greeter();

    // `Type ==` calls `Equals` only when neither operand is a RuntimeType.
    public static bool SameType() => typeof(string) == typeof(object);

    // `ThrowIfNull` throws only for a null argument, which its callers here decide.
    static void Check(object o) => ArgumentNullException.ThrowIfNull(o);

    static object Make() => new object();

    public static void ThrowsIfNullOnNew() => ArgumentNullException.ThrowIfNull(new object());

    public static void ChecksNew() => Check(new object());

    public static void ChecksString() => Check("s");

    public static void ChecksMade() => Check(Make());

    public static void ChecksNull() => Check(null);

    public static void ChecksEither(bool b) => Check(b ? new object() : null);

    public static void ChecksParameter(object o) => Check(o);

    static object Unset;

    // A field may hold null.
    public static void ChecksField() => Check(Unset);

    static object First(object[] xs) => xs[0];

    // An element may be null, so what returns one may too.
    public static void ChecksElement() => Check(First(new object[1]));

    // `ThrowIf` throws only for a true condition, which its callers here decide.
    public static void DisposedIfFalse() => ObjectDisposedException.ThrowIf(false, Unset);

    public static void DisposedIfTrue() => ObjectDisposedException.ThrowIf(true, Unset);

    public static void DisposedIfParameter(bool b) => ObjectDisposedException.ThrowIf(b, Unset);

    static void DisposedIf(bool b) => ObjectDisposedException.ThrowIf(b, Unset);

    public static void PassesFalseOn() => DisposedIf(false);

    public static void PassesTrueOn() => DisposedIf(true);

    // The parameter no longer holds what the caller passed.
    static void DisposedUnless(bool b)
    {
        b = !b;
        ObjectDisposedException.ThrowIf(b, Unset);
    }

    public static void PassesFalseToNegated() => DisposedUnless(false);

    // `Monitor.Enter` throws if the `lockTaken` it is passed is already true, which the local a
    // `lock` passes is not.
    public static void Locks(object o) { lock (o) { } }

    public static void EntersTaken(object o)
    {
        bool taken = true;
        System.Threading.Monitor.Enter(o, ref taken);
    }

    // Each iteration passes the address of the same local, which an earlier iteration's callee
    // could have handed to another thread, so what it holds when `Enter` reads it is not known.
    public static void LocksInLoop(object o, int n)
    {
        for (var i = 0; i < n; i++) { lock (o) { } }
    }

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

// Its initializer calls a static method of its own type, which the initializing thread runs
// without running the initializer again.
public class SelfInit
{
    static SelfInit() { Helper(); }
    static void Helper() { }
}

public class Shadowed
{
    public int Field;
}

public class AccessBase
{
    protected static void Inherited() { }
    public static void Shown() { }
    private static void Hidden() { }

    // A private member of its own type, which CoreCLR always lets the type access.
    public static void CallsOwnPrivate() => Hidden();

    public class Nested
    {
        // ... and lets a type nested in it access.
        public static void CallsEnclosingPrivate() => Hidden();
    }
}

public class AccessDerived : AccessBase
{
    // A protected member of a base class, which CoreCLR lets a derived class access, but which
    // its visibility alone does not show that.
    public static void CallsProtected() => Inherited();

    public static void CallsPublic() => Shown();
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

public static class SR
{
    public static string InternalGetResourceString(string key) => key;
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
            /// Exactly the assumptions the answer relied on.
            Assumes : Assumption list option
            /// Exactly the places that make the answer unknown, as `renderSite` spells them.
            Sources : string list option
        }

    let private expect (ty : string) (name : string) : Expectation =
        {
            Method = ty, name
            Contains = []
            Excludes = []
            Unknown = None
            Assumes = None
            Sources = None
        }

    /// The assumptions that constructing a CoreLib exception relies on: asking for its message runs
    /// `SR`'s type initializer, the lookup may throw a type initializer's own exception again, and
    /// it dispatches on what fields of CoreLib's hold.
    let private constructing : Assumption list =
        [
            Assumption.CoreLibTypeInitializers
            Assumption.StackTracePreserved
            Assumption.FieldsWrittenVisibly
        ]

    let private expectations : Expectation list =
        let ioe = "=System.InvalidOperationException"

        // Waiting for a contended monitor calls the waiting thread's synchronization context, whose
        // `Wait` may be any override.
        let synchronizationContextWait =
            "System.Threading.SynchronizationContext::InvokeWaitMethodHelper IL VirtualCall System.Threading.SynchronizationContext::Wait"

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
            // The `lockTaken` the JIT passes `Monitor.Enter` is false. Taking the monitor may allocate
            // its sync block, or wait and be interrupted; and a wait may run the thread's
            // `SynchronizationContext.Wait`, which may throw anything.
            { expect "Fixture.Cases" "Synchronised" with
                Contains =
                    [
                        "=System.OutOfMemoryException"
                        "=System.Threading.ThreadInterruptedException"
                        "=System.Threading.SynchronizationLockException"
                    ]
                // Its monitor is its type's, never null.
                Excludes = [ "=System.ArgumentNullException" ; "=System.ArgumentException" ]
                Unknown = Some true
                // The JIT may make its monitor, its type's RuntimeType, on first use, which allocates
                // and does no more: its type is fully loaded once its code runs.
                Sources = Some [ synchronizationContextWait ]
            }
            { expect "Fixture.Cases" "ReleasesEarly" with
                Contains = [ "=System.Threading.SynchronizationLockException" ]
            }
            // Reached by `call` on a null receiver, it locks on null.
            { expect "Fixture.Locking" "Instance" with
                Contains =
                    [
                        "=System.ArgumentNullException"
                        "=System.OutOfMemoryException"
                        "=System.Threading.ThreadInterruptedException"
                        "=System.Threading.SynchronizationLockException"
                    ]
                Excludes = [ "=System.ArgumentException" ]
                Unknown = Some true
                Sources = Some [ synchronizationContextWait ]
            }
            { expect "Fixture.Locking" "OnNew" with
                Contains =
                    [
                        "=System.OutOfMemoryException"
                        "=System.Threading.ThreadInterruptedException"
                        "=System.Threading.SynchronizationLockException"
                    ]
                Excludes = [ "=System.ArgumentNullException" ; "=System.ArgumentException" ]
                Unknown = Some true
                Sources = Some [ synchronizationContextWait ]
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
                Sources = Some []
            }
            { expect "Fixture.Cases" "RethrowsAnythingCaughtAsException" with
                Unknown = Some true
                Sources = Some [ "Fixture.Cases::CallsVirtual IL VirtualCall Fixture.Animal::Speak" ]
            }
            { expect "Fixture.Cases" "RethrowsARethrow" with
                Unknown = Some true
                Sources = Some [ "Fixture.Cases::CallsVirtual IL VirtualCall Fixture.Animal::Speak" ]
            }
            { expect "Fixture.Cases" "RethrowsWhatAnEarlierClauseLeaves" with
                Unknown = Some false
                Sources = Some []
            }
            { expect "Fixture.Cases" "RethrowsBesideAnAbsorbedCall" with
                Contains = [ "=System.DivideByZeroException" ]
                Unknown = Some false
                Sources = Some []
            }
            { expect "Fixture.Cases" "OneOfTwoCaught" with
                Unknown = Some true
                Sources = Some [ "Fixture.Cases::OneOfTwoCaught IL VirtualCall Fixture.Animal::Speak" ]
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
                Sources = Some [ "Fixture.Cases::CallsVirtual IL VirtualCall Fixture.Animal::Speak" ]
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
                Sources = Some [ "Fixture.StaticVirtualCases::CallParse IL VirtualCall Fixture.IParse::Parse" ]
            }
            { expect "Fixture.G`1" ".cctor" with
                Contains = [ "=System.TypeInitializationException" ]
            }
            // `Boom`'s initializer fails, and `M` reading its field runs it first. Should wrapping
            // what it raised fail, that escapes instead.
            { expect "Fixture.CctorCases" "CallsBoom" with
                Contains = [ "=System.TypeInitializationException" ; "=System.FormatException" ]
            }
            { expect "Fixture.SelfInit" ".cctor" with
                Excludes = [ "=System.TypeInitializationException" ]
                Unknown = Some false
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
                Unknown = Some false
            }
            { expect "Fixture.Natives" "TypeOf" with
                Contains = [ "=System.NullReferenceException" ; "=System.OutOfMemoryException" ]
                // It loads no type, so binds no assembly.
                Excludes = [ "=System.TypeLoadException" ; "=System.IO.FileNotFoundException" ]
                Unknown = Some false
            }
            { expect "Fixture.Natives" "RankOf" with
                Contains = [ "=System.ArgumentException" ]
            }
            { expect "Fixture.Natives" "ModuleOf" with
                Unknown = Some false
            }
            { expect "Fixture.Natives" "TokenOf" with
                Unknown = Some false
            }
            { expect "Fixture.Natives" "IsParameter" with
                Unknown = Some false
            }
            { expect "Fixture.Natives" "ParameterAttributes" with
                Contains = [ "=System.InvalidOperationException" ]
                Unknown = Some false
            }
            { expect "Fixture.Natives" "Collections" with
                Unknown = Some true
            }
            { expect "Fixture.Natives" "Current" with
                Contains = [ "=System.OutOfMemoryException" ]
                Unknown = Some false
            }
            { expect "Fixture.Natives" "Timestamp" with
                Unknown = Some false
            }
            { expect "Fixture.Natives" "Impostor" with
                Unknown = Some true
                Sources = Some [ "System.MathF::Sin entry NativeBody" ]
            }
            // `String.Ctor(char, int)` throws for a negative count. It allocates through an FCall
            // no contract describes, so it is not wholly known.
            { expect "Fixture.Natives" "Repeat" with
                Contains = [ "=System.ArgumentOutOfRangeException" ]
                Unknown = Some true
            }
            { expect "Fixture.Natives" "ThreadId" with
                Assumes = Some []
            }
            // Visibility alone settles each of these accesses but the protected one, which the
            // analysis assumes CoreCLR allows.
            { expect "Fixture.AccessDerived" "CallsProtected" with
                Unknown = Some false
                Assumes = Some [ Assumption.MembersAccessible ]
                Sources = Some []
            }
            { expect "Fixture.AccessDerived" "CallsPublic" with
                Unknown = Some false
                Assumes = Some []
            }
            { expect "Fixture.AccessBase" "CallsOwnPrivate" with
                Unknown = Some false
                Assumes = Some []
            }
            { expect "Fixture.AccessBase+Nested" "CallsEnclosingPrivate" with
                Unknown = Some false
                Assumes = Some []
            }
            // The resource lookup's contract, which only an interrupted wait for its lock reaches
            // with this exception.
            { expect "Fixture.Messages" "Construct" with
                Contains = [ "=System.Threading.ThreadInterruptedException" ]
                Excludes = [ ioe ]
                Unknown = Some false
                Assumes = Some constructing
                Sources = Some []
            }
            // Whatever the lookup raises is caught.
            // The handler absorbs what an assumption's contract leaves out, but not what the call
            // relies on of what fields hold, which the analysis takes to reach every caller.
            { expect "Fixture.Messages" "Swallowed" with
                Excludes = [ "=System.Threading.ThreadInterruptedException" ]
                Unknown = Some false
                Assumes = Some [ Assumption.FieldsWrittenVisibly ]
            }
            { expect "Fixture.Messages" "Rethrown" with
                Contains = [ "=System.Threading.ThreadInterruptedException" ]
                Unknown = Some false
                Assumes = Some constructing
            }
            { expect "Fixture.Messages" "RethrownAsTimeout" with
                Contains = [ "=System.Threading.ThreadInterruptedException" ]
                Excludes = [ "<:System.TimeoutException" ]
                Unknown = Some false
                Assumes = Some constructing
            }
            { expect "Fixture.Messages" "RethrownThenAbsorbed" with
                Unknown = Some false
                Assumes = Some [ Assumption.FieldsWrittenVisibly ]
            }
            { expect "Fixture.Messages" "OutOfMemoryCaught" with
                Contains = [ "=System.Threading.ThreadInterruptedException" ]
                Excludes = [ "=System.OutOfMemoryException" ]
                Unknown = Some false
                Assumes = Some constructing
            }
            { expect "Fixture.Messages" "Impostor" with
                Excludes = [ "=System.Threading.ThreadInterruptedException" ]
                Unknown = Some false
                Assumes = Some []
            }
            // Each fault's constructor looks up its message, and the lookup's contract has this
            // exception, which none of these methods raises otherwise.
            { expect "Fixture.Faults" "Field" with
                Contains =
                    [
                        "=System.NullReferenceException"
                        "=System.Threading.ThreadInterruptedException"
                    ]
                Unknown = Some false
                Assumes = Some constructing
            }
            { expect "Fixture.Faults" "Element" with
                Contains =
                    [
                        "=System.IndexOutOfRangeException"
                        "=System.Threading.ThreadInterruptedException"
                    ]
                Unknown = Some false
                Assumes = Some constructing
            }
            // The JIT calls `CastHelpers.ChkCastClass`, which, where the cast cache has no answer,
            // falls back to the runtime's `IsInstanceOf_NoCacheLookup`.
            { expect "Fixture.Faults" "Cast" with
                Contains =
                    [
                        "=System.InvalidCastException"
                        "=System.Threading.ThreadInterruptedException"
                    ]
                Unknown = Some false
                Assumes = Some (Assumption.NamedTypesLoad :: Assumption.DynamicCastsAsDocumented :: constructing)
            }
            { expect "Fixture.Faults" "Sum" with
                Contains = [ "=System.OverflowException" ; "=System.Threading.ThreadInterruptedException" ]
                Unknown = Some false
                Assumes = Some constructing
            }
            { expect "Fixture.Faults" "Quotient" with
                Contains =
                    [
                        "=System.DivideByZeroException"
                        "=System.Threading.ThreadInterruptedException"
                    ]
                Unknown = Some false
                Assumes = Some constructing
            }
            { expect "Fixture.Faults" "Grid" with
                Contains =
                    [
                        "=System.IndexOutOfRangeException"
                        "=System.Threading.ThreadInterruptedException"
                    ]
                Unknown = Some false
                Assumes = Some constructing
            }
            { expect "Fixture.Faults" "Allocate" with
                Contains = [ "=System.OutOfMemoryException" ]
                Excludes = [ "=System.Threading.ThreadInterruptedException" ]
                Unknown = Some false
                Assumes = Some []
            }
            { expect "Fixture.ContextCases" "CallsSealedParameter" with
                Contains = [ "=Fixture.Bark" ]
                Unknown = Some false
            }
            { expect "Fixture.ContextCases" "PassesDog" with
                Contains = [ "=Fixture.Bark" ]
                Excludes = [ "=Fixture.Hiss" ]
                Unknown = Some false
                Sources = Some []
            }
            // Each call's summary of the helper is its own.
            { expect "Fixture.ContextCases" "PassesBoth" with
                Contains = [ "=Fixture.Bark" ; "=Fixture.Hiss" ]
                Unknown = Some false
            }
            { expect "Fixture.ContextCases" "CallsThroughThis" with
                Contains = [ "=Fixture.Bark" ]
                Excludes = [ "=Fixture.Hiss" ]
                Unknown = Some false
            }
            { expect "Fixture.ContextCases" "ReturnsWhatItIsPassed" with
                Contains = [ "=Fixture.Bark" ]
                Excludes = [ "=Fixture.Hiss" ]
                Unknown = Some false
            }
            { expect "Fixture.ContextCases" "ReturnsWhatItMakes" with
                Contains = [ "=Fixture.Bark" ]
                Excludes = [ "=Fixture.Hiss" ]
                Unknown = Some false
            }
            { expect "Fixture.ContextCases" "PassesDogTwoDeep" with
                Contains = [ "=Fixture.Bark" ]
                Excludes = [ "=Fixture.Hiss" ]
                Unknown = Some false
            }
            { expect "Fixture.ContextCases" "RecursesWithCat" with
                Contains = [ "=Fixture.Bark" ; "=Fixture.Hiss" ]
                Unknown = Some false
                Sources = Some []
            }
            { expect "Fixture.ContextCases" "PassesNull" with
                Contains = [ "=System.NullReferenceException" ]
                Excludes = [ "=Fixture.Bark" ; "=Fixture.Hiss" ]
                Unknown = Some false
            }
            { expect "Fixture.ContextCases" "PassesDogToReassigned" with
                Unknown = Some true
                Sources = Some [ "Fixture.ContextCases::Reassigns IL VirtualCall Fixture.Animal::Speak" ]
            }
            { expect "Fixture.ContextCases" "PassesDogByAddress" with
                Unknown = Some true
                Sources = Some [ "Fixture.ContextCases::TakesAddress IL VirtualCall Fixture.Animal::Speak" ]
            }
            { expect "Fixture.ContextCases" "EchoesEither" with
                Contains = [ "=Fixture.Bark" ]
                Excludes = [ "=Fixture.Hiss" ]
                Unknown = Some false
            }
            { expect "Fixture.ContextCases" "PassesDogViaConstrained" with
                Contains = [ "=Fixture.Bark" ]
                Excludes = [ "=Fixture.Hiss" ]
                Unknown = Some false
            }
            { expect "Fixture.ContextCases" "ConstructsLoud" with
                Contains = [ "=Fixture.Bark" ]
                Unknown = Some false
            }
            { expect "Fixture.ContextCases" "ConstructsQuiet" with
                Excludes = [ "=Fixture.Bark" ]
                Unknown = Some false
            }
            { expect "Fixture.ContextCases" "ThrowsIfNullOnNew" with
                Excludes = [ "=System.ArgumentNullException" ]
                Unknown = Some false
            }
            { expect "Fixture.ContextCases" "ChecksNew" with
                Excludes = [ "=System.ArgumentNullException" ]
                Unknown = Some false
            }
            { expect "Fixture.ContextCases" "ChecksString" with
                Excludes = [ "=System.ArgumentNullException" ]
                Unknown = Some false
            }
            // What a call returns is never null when every `ret` of what it reaches returns an
            // object `newobj` made.
            { expect "Fixture.ContextCases" "ChecksMade" with
                Excludes = [ "=System.ArgumentNullException" ]
                Unknown = Some false
            }
            { expect "Fixture.ContextCases" "ChecksNull" with
                Contains = [ "=System.ArgumentNullException" ]
                Unknown = Some false
            }
            { expect "Fixture.ContextCases" "ChecksEither" with
                Contains = [ "=System.ArgumentNullException" ]
                Unknown = Some false
            }
            { expect "Fixture.ContextCases" "ChecksParameter" with
                Contains = [ "=System.ArgumentNullException" ]
                Unknown = Some false
            }
            { expect "Fixture.ContextCases" "ChecksField" with
                Contains = [ "=System.ArgumentNullException" ]
                Unknown = Some false
            }
            { expect "Fixture.ContextCases" "ChecksElement" with
                Contains = [ "=System.ArgumentNullException" ]
                Unknown = Some false
            }
            { expect "Fixture.ContextCases" "SameType" with
                Unknown = Some false
            }
            { expect "Fixture.ContextCases" "DisposedIfFalse" with
                Excludes = [ "=System.ObjectDisposedException" ]
                Unknown = Some false
            }
            { expect "Fixture.ContextCases" "DisposedIfTrue" with
                Contains = [ "=System.ObjectDisposedException" ]
            }
            { expect "Fixture.ContextCases" "DisposedIfParameter" with
                Contains = [ "=System.ObjectDisposedException" ]
            }
            { expect "Fixture.ContextCases" "PassesFalseOn" with
                Excludes = [ "=System.ObjectDisposedException" ]
                Unknown = Some false
            }
            { expect "Fixture.ContextCases" "PassesTrueOn" with
                Contains = [ "=System.ObjectDisposedException" ]
            }
            { expect "Fixture.ContextCases" "PassesFalseToNegated" with
                Contains = [ "=System.ObjectDisposedException" ]
            }
            { expect "Fixture.ContextCases" "Locks" with
                Excludes = [ "=System.ArgumentException" ]
            }
            { expect "Fixture.ContextCases" "EntersTaken" with
                Contains = [ "=System.ArgumentException" ]
            }
            { expect "Fixture.ContextCases" "LocksInLoop" with
                Contains = [ "=System.ArgumentException" ]
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

    /// Whether `site`, over a CoreLib of `runtime`, is one of the QCalls that .NET 11's
    /// `SpanHelpers` copies and clears memory through, which the contract table has no row for
    /// (`Net11Frontier.spanHelpersMemoryQCalls`). Never on .NET 10.
    let private isNet11MemoryQCall
        (runtime : EmulatedRuntime)
        (analysis : EscapeAnalysisState)
        (site : OpaqueSite)
        : bool
        =
        match runtime with
        | EmulatedRuntime.Net10 -> false
        | EmulatedRuntime.Net11 ->
            site.Reason = Opacity.NativeBody
            && Set.contains
                (EscapeAnalysis.methodName analysis site.Method)
                (Set.ofList [ "System.SpanHelpers::memmove" ; "System.SpanHelpers::memset" ])

    /// Whether `key`'s answer, over a CoreLib of `runtime`, is unknown only where it reaches one of
    /// the QCalls `isNet11MemoryQCall` names. Never on .NET 10.
    let private unknownOnlyThroughNet11MemoryQCalls
        (runtime : EmulatedRuntime)
        (analysis : EscapeAnalysisState)
        (key : MethodKey)
        : EscapeAnalysisState * bool
        =
        let analysis, sources = EscapeAnalysis.unknownSources analysis key

        analysis, not sources.IsEmpty && Seq.forall (isNet11MemoryQCall runtime analysis) sources

    /// A place the analysis cannot see through, as the expectations spell it: the method it is in,
    /// `IL` at an instruction of the method's IL or `entry` at none, why, and the method the
    /// instruction names.
    let private renderSite (analysis : EscapeAnalysisState) (site : OpaqueSite) : string =
        let where =
            match site.Offset with
            | Some _ -> "IL"
            | None -> "entry"

        let names =
            match site.Names with
            | Some named -> " " + EscapeAnalysis.methodName analysis named
            | None -> ""

        $"%s{EscapeAnalysis.methodName analysis site.Method} %s{where} %A{site.Reason}%s{names}"

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
    /// describes, allowed `assumptions`.
    let private analysisOf
        (corelib : DumpedAssembly)
        (runtimeDirs : string seq)
        (target : JitTarget)
        (profile : HardwareIntrinsicsProfile)
        (assumptions : Set<Assumption>)
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
            assumptions
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

    /// The parameterless instance constructor of `corelib`'s exception type of this namespace and
    /// name.
    let private parameterlessConstructor (corelib : DumpedAssembly) (ns : string) (name : string) : MethodKey =
        let ty =
            match corelib.TryGetTopLevelTypeDef ns name with
            | Some ty -> ty
            | None -> failwith $"CoreLib defines no %s{ns}.%s{name}"

        ty.Methods
        |> Seq.filter (fun m -> m.Name = ".ctor" && not m.IsStatic && m.Signature.ParameterTypes.IsEmpty)
        |> Seq.exactlyOne
        |> fun m -> MethodKey.make corelib m.TryMetadata.Value.Handle

    /// What the parameterless constructor of the host CoreLib's exception type of this full name
    /// lets escape: the runtime runs it to make that exception where it raises one by itself.
    let private making (analysis : EscapeAnalysisState) (fullName : string) : EscapeAnalysisState * Set<string> =
        let dot = fullName.LastIndexOf '.'

        let key =
            parameterlessConstructor (hostCoreLib ()) (fullName.Substring (0, dot)) (fullName.Substring (dot + 1))

        let analysis, escapes = EscapeAnalysis.escapes analysis key
        analysis, render analysis escapes

    /// The host CoreLib's `TypeLoadException(string, string, string, int)` constructor, which the
    /// runtime runs to make the exception of a type that fails to bind.
    let private typeLoadConstructor () : MethodKey =
        let corelib = hostCoreLib ()

        match corelib.TryGetTopLevelTypeDef "System" "TypeLoadException" with
        | None -> failwith "CoreLib defines no System.TypeLoadException"
        | Some ty ->
            ty.Methods
            |> Seq.filter (fun m -> m.Name = ".ctor" && not m.IsStatic && m.Signature.ParameterTypes.Length = 4)
            |> Seq.exactlyOne
            |> fun m -> MethodKey.make corelib m.TryMetadata.Value.Handle

    /// The host CoreLib's `String.Format(string, object, object, object)`, which formats a
    /// `TypeLoadException`'s message.
    let private stringFormat () : MethodKey =
        let corelib = hostCoreLib ()

        let parameters =
            TypeDefn.PrimitiveType PrimitiveType.String
            :: List.replicate 3 (TypeDefn.PrimitiveType PrimitiveType.Object)

        match corelib.TryGetTopLevelTypeDef "System" "String" with
        | None -> failwith "CoreLib defines no System.String"
        | Some ty ->
            ty.Methods
            |> Seq.filter (fun m -> m.Name = "Format" && m.IsStatic && m.Signature.ParameterTypes = parameters)
            |> Seq.exactlyOne
            |> fun m -> MethodKey.make corelib m.TryMetadata.Value.Handle

    /// An analysis over the host's CoreLib and `assemblies`, with `bind` applied to the load
    /// context, for the host's JIT on a CPU `profile` describes.
    let private analysisUnder
        (profile : HardwareIntrinsicsProfile)
        (assemblies : DumpedAssembly list)
        (bind : LoadedAssemblies -> LoadedAssemblies)
        : EscapeAnalysisState
        =
        analysisOf
            (hostCoreLib ())
            (FrameworkUnderTest.runtimeDirs ())
            (hostTarget ())
            profile
            Assumption.all
            assemblies
            bind

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

                    // Allowed, the assumption that no thread is aborted removes from every answer what it
                    // would otherwise raise.
                    let expected (assumes : Assumption list) : Set<Assumption> =
                        if EscapeAnalysis.allows analysis Assumption.NoThreadAborts then
                            Set.ofList assumes |> Set.add Assumption.NoThreadAborts
                        else
                            Set.ofList assumes

                    match expectation.Assumes with
                    | Some assumes when expected assumes <> escapes.Assumes ->
                        yield
                            $"%s{describe ()}, assuming %A{Set.toList escapes.Assumes}, expected %A{Set.toList (expected assumes)}"
                    | _ -> ()
                ]

            match expectation.Sources with
            | None -> analysis, failures
            | Some sources ->
                let analysis, found =
                    EscapeAnalysis.unknownSources
                        analysis
                        (methodNamed fixture (fst expectation.Method) (snd expectation.Method))

                let found = found |> Seq.map (renderSite analysis) |> Set.ofSeq

                if found = Set.ofList sources then
                    analysis, failures
                else
                    analysis,
                    failures
                    @ [ $"%s{describe ()}, unknown from %A{Set.toList found}, expected %A{sources}" ]
        )

    [<Test>]
    let ``each fixture method's escaping exceptions are as stated`` () : unit =
        Net11Frontier.park
            "On .NET 11 a `lock` runs CoreLib's managed thin lock instead of .NET 10's `Monitor` InternalCalls, so the escape analysis finds many more exceptions able to escape the fixture's `ContextCases.Locks` than the fixture states, which are .NET 10's. Un-park when the fixture states each runtime's escapes."

        let _, loggerFactory = LoggerFactory.makeTest ()

        let image =
            Roslyn.compileAssembly "EscapeFixture" OutputKind.DynamicallyLinkedLibrary [] [ source ; shadow ]

        let fixture =
            Assembly.read loggerFactory (Some "EscapeFixture.dll") (new MemoryStream (image))

        match unmet (analysisOver [ fixture ] id) fixture expectations with
        | _, [] -> ()
        | _, failures -> failures |> String.concat Environment.NewLine |> failwith

    /// How the places `unknownSources` reports for each of `methods` fail to agree with `escapes`:
    /// there is one exactly when the answer is unknown, and each at an instruction is at one of the
    /// IL CoreCLR runs for its method. `assemblies` must hold every method a place is in.
    let private sourcesDisagree
        (analysis : EscapeAnalysisState)
        (assemblies : DumpedAssembly list)
        (methods : MethodKey list)
        : string list
        =
        let instructionAt (key : MethodKey) (offset : int) : bool =
            let assembly =
                match
                    assemblies
                    |> List.tryFind (fun a -> a.DefinitionFullName = key.AssemblyFullName)
                with
                | Some assembly -> assembly
                | None -> failwith $"A place is in %O{key}, of an assembly the test does not hold"

            let instructions =
                match VmSubstitution.unsafeStub assembly key.Method.Get, assembly.Methods.[key.Method.Get].Body with
                | Some stub, _
                | None, MethodBody.Il stub -> stub.Instructions
                | None, _ -> []

            instructions |> List.exists (fun (_, at) -> at = offset)

        ((analysis, []), methods)
        ||> List.fold (fun (analysis, failures) key ->
            let analysis, escapes = EscapeAnalysis.escapes analysis key
            let analysis, sources = EscapeAnalysis.unknownSources analysis key

            let failures =
                [
                    yield! failures

                    if Set.isEmpty sources = escapes.Unknown then
                        yield
                            $"%s{EscapeAnalysis.methodName analysis key}: unknown %b{escapes.Unknown}, from %d{sources.Count} places"

                    for site in sources do
                        match site.Offset with
                        | Some offset when not (instructionAt site.Method offset) ->
                            yield
                                $"%s{EscapeAnalysis.methodName analysis key}: %A{site.Reason} in %s{EscapeAnalysis.methodName analysis site.Method} at IL_%04x{offset}, which is no instruction"
                        | _ -> ()
                ]

            analysis, failures
        )
        |> snd

    [<Test>]
    let ``a fixture method has a place the analysis cannot see through exactly when its answer is unknown`` () : unit =
        Net11Frontier.park Net11Frontier.synchronizedMethodHelpers

        let _, loggerFactory = LoggerFactory.makeTest ()

        let image =
            Roslyn.compileAssembly "EscapeFixture" OutputKind.DynamicallyLinkedLibrary [] [ source ; shadow ]

        let fixture =
            Assembly.read loggerFactory (Some "EscapeFixture.dll") (new MemoryStream (image))

        let corelib = hostCoreLib ()

        let methods =
            fixture.Methods
            |> Seq.map (fun (KeyValue (handle, _)) -> MethodKey.make fixture handle)
            |> List.ofSeq

        // Allowed no assumption, the places may be deep in CoreLib, past its own handlers and rethrows.
        for assumptions in [ Assumption.all ; Set.empty ] do
            let analysis =
                analysisOf
                    corelib
                    (FrameworkUnderTest.runtimeDirs ())
                    (hostTarget ())
                    HardwareIntrinsicsProfile.ScalarOnly
                    assumptions
                    [ fixture ]
                    id

            match sourcesDisagree analysis [ fixture ; corelib ] methods with
            | [] -> ()
            | failures -> failures |> String.concat Environment.NewLine |> failwith

    [<Test>]
    let ``a CoreLib method has a place the analysis cannot see through exactly when its answer is unknown`` () : unit =
        Net11Frontier.park Net11Frontier.synchronizedMethodHelpers

        let corelib = hostCoreLib ()

        let analysis =
            analysisOf
                corelib
                (FrameworkUnderTest.runtimeDirs ())
                (hostTarget ())
                HardwareIntrinsicsProfile.ScalarOnly
                Assumption.all
                []
                id

        // Every fortieth method definition: a sample spread over the whole of CoreLib.
        let methods =
            corelib.Methods
            |> Seq.map (fun (KeyValue (handle, _)) -> MethodKey.make corelib handle)
            |> Seq.sort
            |> Seq.chunkBySize 40
            |> Seq.map Array.head
            |> List.ofSeq

        match sourcesDisagree analysis [ corelib ] methods with
        | [] -> ()
        | failures -> failures |> String.concat Environment.NewLine |> failwith

    /// How each of `methods`' answer with the classes its callers pass fails to be at least as precise
    /// as its answer without them: unknown only where that was, and otherwise naming only what that
    /// named and assuming only what that assumed.
    let private lessPreciseWithContexts (analysis : EscapeAnalysisState) (methods : MethodKey list) : string list =
        let blind = EscapeAnalysis.withoutArgumentContexts analysis

        ((analysis, blind, []), methods)
        ||> List.fold (fun (analysis, blind, failures) key ->
            let analysis, sighted = EscapeAnalysis.escapes analysis key
            let blind, unsighted = EscapeAnalysis.escapes blind key

            let failures =
                if
                    sighted.Unknown && not unsighted.Unknown
                    || not unsighted.Unknown
                       && not (
                           Set.isSubset sighted.Types unsighted.Types
                           && Set.isSubset sighted.Assumes unsighted.Assumes
                       )
                then
                    $"%s{EscapeAnalysis.methodName analysis key}: %A{render analysis sighted}, unknown %b{sighted.Unknown}, assuming %A{sighted.Assumes}; without contexts %A{render blind unsighted}, unknown %b{unsighted.Unknown}, assuming %A{unsighted.Assumes}"
                    :: failures
                else
                    failures

            analysis, blind, failures
        )
        |> fun (_, _, failures) -> List.rev failures

    [<Test>]
    let ``what callers pass makes a fixture method's answer only more precise`` () : unit =
        Net11Frontier.park Net11Frontier.synchronizedMethodHelpers

        let _, loggerFactory = LoggerFactory.makeTest ()

        let image =
            Roslyn.compileAssembly "EscapeFixture" OutputKind.DynamicallyLinkedLibrary [] [ source ; shadow ]

        let fixture =
            Assembly.read loggerFactory (Some "EscapeFixture.dll") (new MemoryStream (image))

        let methods =
            fixture.Methods
            |> Seq.map (fun (KeyValue (handle, _)) -> MethodKey.make fixture handle)
            |> List.ofSeq

        match lessPreciseWithContexts (analysisOver [ fixture ] id) methods with
        | [] -> ()
        | failures -> failures |> String.concat Environment.NewLine |> failwith

    [<Test>]
    let ``what callers pass makes a CoreLib method's answer only more precise`` () : unit =
        Net11Frontier.park Net11Frontier.synchronizedMethodHelpers

        let analysis = analysisOver [] id
        let corelib = hostCoreLib ()

        // Every fortieth method definition, from the eleventh: a sample spread over the whole of
        // CoreLib, beside the one the test of unknown places takes.
        let methods =
            corelib.Methods
            |> Seq.map (fun (KeyValue (handle, _)) -> MethodKey.make corelib handle)
            |> Seq.sort
            |> Seq.chunkBySize 40
            |> Seq.choose (Array.tryItem 10)
            |> List.ofSeq

        match lessPreciseWithContexts analysis methods with
        | [] -> ()
        | failures -> failures |> String.concat Environment.NewLine |> failwith

    /// Whether `escapes` covers an exception of the class `thrown`: it is unknown, names the class,
    /// or names one the class derives from as a subtype.
    let private covers (analysis : EscapeAnalysisState) (escapes : Escapes) (thrown : Type) : bool =
        let ancestors =
            Seq.unfold (fun (ty : Type) -> if isNull ty then None else Some (ty.FullName, ty.BaseType)) thrown
            |> Set.ofSeq

        let shown = render analysis escapes

        escapes.Unknown
        || shown.Contains ("=" + thrown.FullName)
        || ancestors |> Set.exists (fun ancestor -> shown.Contains ("<:" + ancestor))

    [<Test>]
    let ``what a method passing its callee a known class throws on the real runtime is in its answer`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let image =
            Roslyn.compileAssembly "EscapeFixture" OutputKind.DynamicallyLinkedLibrary [] [ source ; shadow ]

        let fixture =
            Assembly.read loggerFactory (Some "EscapeFixture.dll") (new MemoryStream (image))

        let context =
            System.Runtime.Loader.AssemblyLoadContext ("EscapeFixture", isCollectible = true)

        try
            let cases =
                context.LoadFromStream(new MemoryStream (image)).GetType "Fixture.ContextCases"

            let methods =
                cases.GetMethods (BindingFlags.Public ||| BindingFlags.Static)
                |> Array.filter (fun method -> method.GetParameters().Length = 0)
                |> List.ofArray

            methods.Length |> shouldBeGreaterThan 0

            let thrownBy =
                methods
                |> List.choose (fun method ->
                    try
                        method.Invoke ((null : obj), Array.empty<obj>) |> ignore<obj>
                        None
                    with :? TargetInvocationException as e ->
                        Some (method.Name, e.InnerException.GetType ())
                )

            // The fixture's cases throw at least these, so the check is not vacuous.
            thrownBy |> List.map fst |> shouldContain "PassesDog"
            thrownBy |> List.map fst |> shouldContain "ConstructsLoud"

            let failures =
                ((analysisOver [ fixture ] id, []), thrownBy)
                ||> List.fold (fun (analysis, failures) (name, thrown) ->
                    let analysis, escapes =
                        EscapeAnalysis.escapes analysis (methodNamed fixture "Fixture.ContextCases" name)

                    if covers analysis escapes thrown then
                        analysis, failures
                    else
                        analysis,
                        $"Fixture.ContextCases::%s{name} threw %s{thrown.FullName}, but the analysis says %A{render analysis escapes}, unknown %b{escapes.Unknown}"
                        :: failures
                )
                |> snd

            match failures with
            | [] -> ()
            | failures -> failures |> String.concat Environment.NewLine |> failwith
        finally
            context.Unload ()

    [<Test>]
    let ``allowed no assumption, no answer assumes anything, and one that relied on none is unchanged`` () : unit =
        Net11Frontier.park Net11Frontier.synchronizedMethodHelpers

        let _, loggerFactory = LoggerFactory.makeTest ()

        let image =
            Roslyn.compileAssembly "EscapeFixture" OutputKind.DynamicallyLinkedLibrary [] [ source ; shadow ]

        let fixture =
            Assembly.read loggerFactory (Some "EscapeFixture.dll") (new MemoryStream (image))

        let analysis =
            analysisOf
                (hostCoreLib ())
                (FrameworkUnderTest.runtimeDirs ())
                (hostTarget ())
                HardwareIntrinsicsProfile.ScalarOnly
                Set.empty
                [ fixture ]
                id

        // An answer that relied on no assumption follows from the code alone, so it holds as the
        // fixture states it; of any other, only that it now assumes nothing is checked.
        let assumingNothing =
            expectations
            |> List.map (fun expectation ->
                match expectation.Assumes with
                | Some [] -> expectation
                | _ ->
                    { expect (fst expectation.Method) (snd expectation.Method) with
                        Assumes = Some []
                    }
            )

        match unmet analysis fixture assumingNothing with
        | _, [] -> ()
        | _, failures -> failures |> String.concat Environment.NewLine |> failwith

    /// Not allowed to assume an access CoreCLR may refuse, the analysis cannot see past it.
    [<Test>]
    let ``an access visibility does not settle is unknown where it may not be assumed`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let image =
            Roslyn.compileAssembly "EscapeFixture" OutputKind.DynamicallyLinkedLibrary [] [ source ; shadow ]

        let fixture =
            Assembly.read loggerFactory (Some "EscapeFixture.dll") (new MemoryStream (image))

        let analysis =
            analysisOf
                (hostCoreLib ())
                (FrameworkUnderTest.runtimeDirs ())
                (hostTarget ())
                HardwareIntrinsicsProfile.ScalarOnly
                (Set.remove Assumption.MembersAccessible Assumption.all)
                [ fixture ]
                id

        let method = methodNamed fixture "Fixture.AccessDerived" "CallsProtected"
        let analysis, escapes = EscapeAnalysis.escapes analysis method
        escapes.Unknown |> shouldEqual true
        let analysis, sources = EscapeAnalysis.unknownSources analysis method

        sources
        |> Seq.map (renderSite analysis)
        |> List.ofSeq
        |> shouldEqual [ "Fixture.AccessDerived::CallsProtected IL UndecidedAccess" ]

    [<Test>]
    let ``each method an assumption summarises raises exactly its contract when assumed, and some is unknown otherwise``
        ()
        : unit
        =
        let corelib = hostCoreLib ()

        let summarised : Map<Assumption, MethodKey list> =
            [
                for KeyValue (handle, _) in corelib.Methods do
                    match Assumption.summarises corelib handle with
                    | Some assumption -> yield assumption, MethodKey.make corelib handle
                    | None -> ()
            ]
            |> List.groupBy fst
            |> List.map (fun (assumption, keys) -> assumption, List.map snd keys)
            |> Map.ofList

        // `JitOptimises`, `MembersAccessible`, `FieldsWrittenVisibly`, `MetadataWellFormed` and
        // `NoThreadAborts` stand in for no method.
        summarised
        |> Map.keys
        |> Set.ofSeq
        |> shouldEqual (
            Assumption.all
            - set
                [
                    Assumption.JitOptimises
                    Assumption.MembersAccessible
                    Assumption.FieldsWrittenVisibly
                    Assumption.MetadataWellFormed
                    Assumption.NoThreadAborts
                ]
        )

        // Each stands in for one method, but the one about CoreLib's type initializers, which
        // stands in for each of them.
        for KeyValue (assumption, keys) in summarised do
            match assumption with
            | Assumption.CoreLibTypeInitializers -> keys.Length |> shouldBeGreaterThan 1
            | Assumption.NamedTypesLoad
            | Assumption.StackTracePreserved
            | Assumption.DynamicCastsAsDocumented -> keys.Length |> shouldEqual 1
            | Assumption.JitOptimises
            | Assumption.MembersAccessible
            | Assumption.FieldsWrittenVisibly
            | Assumption.MetadataWellFormed
            | Assumption.NoThreadAborts -> failwith $"%A{assumption} summarises %A{keys}"

        let analysis (assumptions : Set<Assumption>) : EscapeAnalysisState =
            analysisOf
                corelib
                (FrameworkUnderTest.runtimeDirs ())
                (hostTarget ())
                HardwareIntrinsicsProfile.ScalarOnly
                assumptions
                []
                id

        let mutable assuming = analysis Assumption.all

        for KeyValue (assumption, keys) in summarised do
            // Running out of memory or stack is something no assumption rules out.
            let contract =
                Assumption.raises assumption
                |> List.map (fun name -> name.FullName)
                |> Set.ofList

            for exhaustion in [ "System.OutOfMemoryException" ; "System.StackOverflowException" ] do
                if not (contract.Contains exhaustion) then
                    failwith $"%A{assumption}'s contract leaves out %s{exhaustion}"

            // The parameterless constructors of the exceptions it makes before raising them.
            let constructors =
                Assumption.constructs assumption
                |> List.map (fun name -> parameterlessConstructor corelib name.Namespace name.Name)

            let constructed (state : EscapeAnalysisState) : EscapeAnalysisState * Escapes list =
                ((state, []), constructors)
                ||> List.fold (fun (state, answers) constructor ->
                    let state, answer = EscapeAnalysis.escapes state constructor
                    state, answer :: answers
                )

            // Allowed everything, each raises its contract and what those constructors raise.
            let next, made = constructed assuming
            assuming <- next

            for key in keys do
                let next, escapes = EscapeAnalysis.escapes assuming key
                assuming <- next

                render assuming escapes
                |> shouldEqual (
                    made
                    |> List.map (render assuming)
                    |> Set.unionMany
                    |> Set.union (contract |> Set.map (fun name -> "=" + name))
                )

                escapes.Unknown |> shouldEqual false

                escapes.Assumes
                |> shouldEqual (
                    made
                    |> List.map _.Assumes
                    |> Set.unionMany
                    |> Set.add assumption
                    |> Set.add Assumption.NoThreadAborts
                )

            // Those constructors are known allowed only the assumptions about what constructing a
            // CoreLib exception runs: its message's lookup runs `SR`'s type initializer, and may
            // throw a type initializer's own exception again.
            let constructing =
                Set.ofList [ Assumption.StackTracePreserved ; Assumption.CoreLibTypeInitializers ]

            let _, made = constructed (analysis constructing)

            for answer in made do
                answer.Unknown |> shouldEqual false
                answer.Assumes |> shouldEqual constructing

            // Every other assumption allowed does not stand in for this one, though a method's own
            // code may rely on another: none relies on it, and some is unknown.
            let mutable without = analysis (Set.remove assumption Assumption.all)

            let someUnknown =
                keys
                |> List.exists (fun key ->
                    let next, escapes = EscapeAnalysis.escapes without key
                    without <- next
                    escapes.Assumes.Contains assumption |> shouldEqual false
                    escapes.Unknown
                )

            if not someUnknown then
                failwith $"Allowed every other assumption, every method %A{assumption} summarises is known"

    /// The places the analysis cannot see through in `corelib`'s `Exception.InternalPreserveStackTrace`
    /// asked about by itself, as `renderSite` spells them, among which are the virtual getters it
    /// reads, on an exception of any class.
    let private preservingStackTrace
        (analysis : EscapeAnalysisState)
        (corelib : DumpedAssembly)
        : EscapeAnalysisState * Set<string>
        =
        let analysis, sources =
            EscapeAnalysis.unknownSources analysis (methodNamed corelib "System.Exception" "InternalPreserveStackTrace")

        let rendered = sources |> Seq.map (renderSite analysis) |> Set.ofSeq

        for getter in [ "get_Source" ; "get_StackTrace" ] do
            let site =
                $"System.Exception::InternalPreserveStackTrace IL VirtualCall System.Exception::%s{getter}"

            if not (rendered.Contains site) then
                failwith $"Preserving a stack trace is hidden at %A{Set.toList rendered}, not at %s{site}"

        analysis, rendered

    [<Test>]
    let ``Object.GetType raises what making its RuntimeType raises, besides what constructing those exceptions raises``
        ()
        : unit
        =
        let corelib = hostCoreLib ()
        let runtime = FrameworkUnderTest.runtime ()

        let key =
            corelib.Methods
            |> Seq.filter (fun (KeyValue (handle, _)) -> ObjectGetType.isGetType corelib handle)
            |> Seq.exactlyOne
            |> fun (KeyValue (handle, _)) -> MethodKey.make corelib handle

        let analysis =
            analysisOf
                corelib
                (FrameworkUnderTest.runtimeDirs ())
                (hostTarget ())
                HardwareIntrinsicsProfile.ScalarOnly
                Assumption.all
                []
                id

        // Called on null it raises NullReferenceException, and on .NET 11, asked about a runtime-async
        // continuation, NotSupportedException, each made by its parameterless constructor.
        let made =
            match runtime with
            | EmulatedRuntime.Net10 -> [ "System", "NullReferenceException" ]
            | EmulatedRuntime.Net11 -> [ "System", "NullReferenceException" ; "System", "NotSupportedException" ]

        // What running each parameterless constructor raises in place of its exception.
        let analysis, constructing =
            ((analysis, Set.empty), made)
            ||> List.fold (fun (analysis, raised) (ns, name) ->
                let ctor =
                    corelib.Methods
                    |> Seq.filter (fun (KeyValue (_, method)) ->
                        let declaringType = corelib.TypeDefs.[method.RequiredDeclaringType.Definition.Get]

                        declaringType.Namespace = ns
                        && declaringType.Name = name
                        && method.Name = ".ctor"
                        && method.Signature.ParameterTypes.IsEmpty
                    )
                    |> Seq.exactlyOne
                    |> fun (KeyValue (handle, _)) -> MethodKey.make corelib handle

                let analysis, escapes = EscapeAnalysis.escapes analysis ctor
                escapes.Unknown |> shouldEqual false
                analysis, Set.union raised (render analysis escapes)
            )

        let analysis, escapes = EscapeAnalysis.escapes analysis key

        // Allocating the RuntimeType can run out of memory, and its calls out of stack.
        render analysis escapes
        |> shouldEqual (
            made @ [ "System", "OutOfMemoryException" ; "System", "StackOverflowException" ]
            |> List.map (fun (ns, name) -> $"=%s{ns}.%s{name}")
            |> Set.ofList
            |> Set.union constructing
        )

        escapes.Unknown |> shouldEqual false

    [<Test>]
    let ``the cast helper that stores into an array of references is known, write barrier and all`` () : unit =
        let corelib = hostCoreLib ()

        let key =
            corelib.Methods
            |> Seq.filter (fun (KeyValue (_, method)) ->
                let declaringType = corelib.TypeDefs.[method.RequiredDeclaringType.Definition.Get]

                declaringType.Namespace = "System.Runtime.CompilerServices"
                && declaringType.Name = "CastHelpers"
                && method.Name = "StelemRef"
            )
            |> Seq.exactlyOne
            |> fun (KeyValue (handle, _)) -> MethodKey.make corelib handle

        let analysis =
            analysisOf
                corelib
                (FrameworkUnderTest.runtimeDirs ())
                (hostTarget ())
                HardwareIntrinsicsProfile.ScalarOnly
                Assumption.all
                []
                id

        let analysis, escapes = EscapeAnalysis.escapes analysis key
        let rendered = render analysis escapes

        // A null array, an index out of its bounds, and an element of a type it cannot hold.
        for raised in
            [
                "=System.NullReferenceException"
                "=System.IndexOutOfRangeException"
                "=System.ArrayTypeMismatchException"
            ] do
            rendered |> shouldContain raised

        escapes.Unknown |> shouldEqual false

    [<Test>]
    let ``CoreLib's resource lookup raises exactly its contract, besides what throwing an initializer's exception again does``
        ()
        : unit
        =
        let corelib = hostCoreLib ()

        let key =
            corelib.Methods
            |> Seq.filter (fun (KeyValue (handle, _)) -> ResourceLookup.isLookup corelib handle)
            |> Seq.exactlyOne
            |> fun (KeyValue (handle, _)) -> MethodKey.make corelib handle

        let analysis (assumptions : Set<Assumption>) : EscapeAnalysisState =
            analysisOf
                corelib
                (FrameworkUnderTest.runtimeDirs ())
                (hostTarget ())
                HardwareIntrinsicsProfile.ScalarOnly
                assumptions
                []
                id

        // A type initializer it runs may fail, and the runtime may throw that one's own exception
        // again, which reads its stack trace.
        let assuming, escapes =
            EscapeAnalysis.escapes
                (analysis (Set.ofList [ Assumption.StackTracePreserved ; Assumption.NoThreadAborts ]))
                key

        render assuming escapes
        |> shouldEqual (
            ResourceLookup.raises @ Assumption.raises Assumption.StackTracePreserved
            |> List.map (fun name -> "=" + name.FullName)
            |> Set.ofList
        )

        escapes.Unknown |> shouldEqual false

        escapes.Assumes
        |> shouldEqual (Set.ofList [ Assumption.StackTracePreserved ; Assumption.NoThreadAborts ])

        // Allowed every other assumption, only reading that stack trace is hidden.
        let unassuming, escapes =
            EscapeAnalysis.escapes (analysis (Set.remove Assumption.StackTracePreserved Assumption.all)) key

        escapes.Unknown |> shouldEqual true
        escapes.Assumes.Contains Assumption.StackTracePreserved |> shouldEqual false
        let unassuming, sources = EscapeAnalysis.unknownSources unassuming key
        let unassuming, preserving = preservingStackTrace unassuming corelib

        sources
        |> Seq.map (renderSite unassuming)
        |> Set.ofSeq
        |> shouldEqual preserving

    /// The runtime makes each exception a native raises by running its parameterless constructor,
    /// whose own exceptions escape in its place, and the exceptions of the methods it calls escape
    /// it too.
    [<Test>]
    let ``a native method the contract table describes raises what its row says, what making those runs, and what its calls raise``
        ()
        : unit
        =
        Net11Frontier.park Net11Frontier.nativeContractTable

        let corelib = hostCoreLib ()

        let tabulated =
            [
                for KeyValue (handle, _) in corelib.Methods do
                    match NativeMethod.recognise corelib handle with
                    | Some (NativeMethod.Tabulated row) -> yield MethodKey.make corelib handle, row
                    | _ -> ()
            ]

        // Every row names at least one method.
        tabulated
        |> List.map snd
        |> List.distinct
        |> List.length
        |> shouldEqual (NativeContractTable.rows.Force ()).Length

        let analysis =
            analysisOf
                corelib
                (FrameworkUnderTest.runtimeDirs ())
                (hostTarget ())
                HardwareIntrinsicsProfile.ScalarOnly
                Assumption.all
                []
                id

        let _ =
            (analysis, tabulated)
            ||> List.fold (fun analysis (key, row) ->
                let analysis, made =
                    ((analysis, Set.empty), row.Contract.Raises)
                    ||> List.fold (fun (analysis, made) name ->
                        let analysis, escapes =
                            EscapeAnalysis.escapes
                                analysis
                                (parameterlessConstructor corelib name.Namespace name.Name)

                        escapes.Unknown |> shouldEqual false
                        analysis, made |> Set.add ("=" + name.FullName) |> Set.union (render analysis escapes)
                    )

                let analysis, (made, unknown) =
                    ((analysis, (made, false)), row.Contract.Runs)
                    ||> List.fold (fun (analysis, (made, unknown)) call ->
                        let call = NativeCall.resolve (FrameworkUnderTest.runtime ()) call

                        let analysis, escapes =
                            EscapeAnalysis.escapes analysis (methodNamed corelib call.DeclaringType call.Method)

                        analysis, (Set.union made (render analysis escapes), unknown || escapes.Unknown)
                    )

                let analysis, escapes = EscapeAnalysis.escapes analysis key
                render analysis escapes |> shouldEqual made
                escapes.Unknown |> shouldEqual unknown
                analysis
            )

        ()

    /// A row that runs CoreCLR's lookup of a resource message (`NativeCall.ResourceMessage`) runs the
    /// CoreLib method the runtime under test calls for it.
    [<Test>]
    let ``a native's lookup of a resource message runs the runtime's method for it`` () : unit =
        let analysis = analysisOver [] id
        let key, _ = EscapeAnalysis.nativeCallee analysis NativeCall.ResourceMessage

        let expected =
            match FrameworkUnderTest.runtime () with
            | EmulatedRuntime.Net10 -> "System.Environment::GetResourceStringLocal"
            | EmulatedRuntime.Net11 -> "System.Environment::GetResourceString"

        EscapeAnalysis.methodName analysis key |> shouldEqual expected

    /// Methods that do nothing, or that catch everything their bodies raise.
    let private abortSource =
        """
using System;
using System.Threading;

namespace Aborts;

public static class Cases
{
    public static int Constant() => 1;

    public static void CatchesAll()
    {
        try { Constant(); } catch { }
    }

    public static void Filters()
    {
        try { Constant(); } catch (Exception) when (true) { }
    }

    public static void CatchesAbort()
    {
        try { Constant(); } catch (ThreadAbortException) { }
    }

    public static void Throws() => throw new InvalidOperationException();
}
"""

    /// A requested abort (`Assumption.NoThreadAborts`) can arise in any method, and no handler stops
    /// it, since a catch raises it again at its end. Where CoreCLR cannot allocate the abort's
    /// exception, it raises its preallocated `OutOfMemoryException` instead, which can arise where
    /// no handler of the method protects it. Assuming no thread is aborted removes both, and every
    /// answer says it relies on that.
    [<Test>]
    let ``every method can raise ThreadAbortException, or OutOfMemoryException in its place, unless no thread is aborted``
        ()
        : unit
        =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let corelib = hostCoreLib ()

        let fixture =
            Roslyn.compileOptimizedAssembly "Aborts" OutputKind.DynamicallyLinkedLibrary [ abortSource ]
            |> fun image -> Assembly.read loggerFactory (Some "Aborts.dll") (new MemoryStream (image))

        let analysis (assumptions : Set<Assumption>) : EscapeAnalysisState =
            analysisOf
                corelib
                (FrameworkUnderTest.runtimeDirs ())
                (hostTarget ())
                HardwareIntrinsicsProfile.ScalarOnly
                assumptions
                [ fixture ]
                id

        let methods =
            [
                for name in [ "Constant" ; "CatchesAll" ; "Filters" ; "CatchesAbort" ; "Throws" ] do
                    yield name, methodNamed fixture "Aborts.Cases" name
                // An FCall the contract table describes, and a method an assumption's contract
                // stands in for.
                yield "GetToken", methodNamed corelib "System.RuntimeTypeHandle" "GetToken"
                yield "InternalPreserveStackTrace", methodNamed corelib "System.Exception" "InternalPreserveStackTrace"
            ]

        let abort = "=System.Threading.ThreadAbortException"
        let outOfMemory = "=System.OutOfMemoryException"

        let answers (analysis : EscapeAnalysisState) : (string * (bool * bool * bool)) list =
            let _, answers =
                ((analysis, []), methods)
                ||> List.fold (fun (analysis, answers) (name, key) ->
                    let analysis, escapes = EscapeAnalysis.escapes analysis key
                    let rendered = render analysis escapes

                    analysis,
                    (name,
                     (rendered.Contains abort,
                      rendered.Contains outOfMemory,
                      escapes.Assumes.Contains Assumption.NoThreadAborts))
                    :: answers
                )

            List.rev answers

        // Raising both, and not assuming them away.
        answers (analysis (Set.remove Assumption.NoThreadAborts Assumption.all))
        |> shouldEqual (methods |> List.map (fun (name, _) -> name, (true, true, false)))

        // Not raising the abort, and assuming it away. These raise no `OutOfMemoryException` then,
        // so the abort's is not added where the assumption is allowed.
        let raisesNoOutOfMemory = set [ "Constant" ; "CatchesAll" ; "CatchesAbort" ]

        answers (analysis Assumption.all)
        |> List.iter (fun (name, (raisesAbort, raisesOutOfMemory, assumes)) ->
            (name, raisesAbort, assumes) |> shouldEqual (name, false, true)

            if raisesNoOutOfMemory.Contains name then
                (name, raisesOutOfMemory) |> shouldEqual (name, false)
        )

    /// `FromIntPtr` makes a reflection stub over the handle, as the runtime's `ldtoken` of a method
    /// or a field does by `FromPtr`, reading the loader allocator of the member by an FCall.
    [<Test>]
    let ``a method or field handle made from a pointer reads its loader allocator by a native the analysis sees through``
        ()
        : unit
        =
        let corelib = hostCoreLib ()

        let analysis =
            analysisOf
                corelib
                (FrameworkUnderTest.runtimeDirs ())
                (hostTarget ())
                HardwareIntrinsicsProfile.ScalarOnly
                Assumption.all
                []
                id

        let _ =
            (analysis, [ "System.RuntimeMethodHandle" ; "System.RuntimeFieldHandle" ])
            ||> List.fold (fun analysis typeName ->
                let key = methodNamed corelib typeName "FromIntPtr"
                let analysis, sources = EscapeAnalysis.unknownSources analysis key

                sources |> Seq.map (renderSite analysis) |> List.ofSeq |> shouldEqual []

                analysis
            )

        ()

    /// The `CastHelpers` methods that `jithelpers.h` binds the JIT's `isinst` and `castclass`
    /// helpers to, and whether each throws. All but `IsInstanceOfClass` fall back, where `CastCache`
    /// has no answer, to the QCall `IsInstanceOf_NoCacheLookup`; the `ChkCast` ones ask it to throw.
    let private castHelpers : (string * bool) list =
        [
            "IsInstanceOfAny", false
            "IsInstanceOfInterface", false
            "IsInstanceOfClass", false
            "ChkCastAny", true
            "ChkCastInterface", true
            "ChkCastClass", true
            "ChkCastClassSpecial", true
        ]

    /// The assumptions the QCall `IsInstanceOf_NoCacheLookup` relies on: that the constraints of a
    /// generic parameter of an open target load, and what an `IDynamicInterfaceCastable` answers.
    let private castFallbackAssumes : Set<Assumption> =
        Set.ofList [ Assumption.NamedTypesLoad ; Assumption.DynamicCastsAsDocumented ]

    [<Test>]
    let ``every cast helper the JIT calls is seen through, the throwing ones raising InvalidCastException`` () : unit =
        Net11Frontier.park Net11Frontier.nativeContractTable

        let corelib = hostCoreLib ()

        let analysis =
            analysisOf
                corelib
                (FrameworkUnderTest.runtimeDirs ())
                (hostTarget ())
                HardwareIntrinsicsProfile.ScalarOnly
                Assumption.all
                []
                id

        let _ =
            (analysis, castHelpers)
            ||> List.fold (fun analysis (name, throws) ->
                let key = methodNamed corelib "System.Runtime.CompilerServices.CastHelpers" name

                let analysis, sources = EscapeAnalysis.unknownSources analysis key

                (name, sources |> Seq.map (renderSite analysis) |> List.ofSeq)
                |> shouldEqual (name, [])

                let analysis, escapes = EscapeAnalysis.escapes analysis key

                if throws then
                    (name, (render analysis escapes).Contains "=System.InvalidCastException")
                    |> shouldEqual (name, true)

                let falls = name <> "IsInstanceOfClass"

                (name, Set.intersect escapes.Assumes castFallbackAssumes)
                |> shouldEqual (name, (if falls then castFallbackAssumes else Set.empty))

                analysis
            )

        ()

    [<Test>]
    let ``a cast helper's fallback is hidden without either assumption it relies on`` () : unit =
        Net11Frontier.park Net11Frontier.nativeContractTable

        let corelib = hostCoreLib ()

        let analysis (assumptions : Set<Assumption>) : EscapeAnalysisState =
            analysisOf
                corelib
                (FrameworkUnderTest.runtimeDirs ())
                (hostTarget ())
                HardwareIntrinsicsProfile.ScalarOnly
                assumptions
                []
                id

        let key =
            methodNamed corelib "System.Runtime.CompilerServices.CastHelpers" "ChkCastAny"

        // Without named types loading, the QCall's own body is hidden.
        let unloading, sources =
            EscapeAnalysis.unknownSources (analysis (Set.remove Assumption.NamedTypesLoad Assumption.all)) key

        sources
        |> Seq.map (fun site -> EscapeAnalysis.methodName unloading site.Method, site.Reason)
        |> List.ofSeq
        |> function
            | [ method, Opacity.NativeBody ] when method.Contains "IsInstanceOf_NoCacheLookup" -> ()
            | other -> failwith $"Without NamedTypesLoad, ChkCastAny is hidden at %A{other}"

        // Without what an `IDynamicInterfaceCastable` answers, the wrapper's call of it is.
        let undynamic, sources =
            EscapeAnalysis.unknownSources (analysis (Set.remove Assumption.DynamicCastsAsDocumented Assumption.all)) key

        let rendered = sources |> Seq.map (renderSite undynamic) |> Set.ofSeq

        let site =
            "System.Runtime.InteropServices.DynamicInterfaceCastableHelpers::IsInterfaceImplemented IL VirtualCall "
            + "System.Runtime.InteropServices.IDynamicInterfaceCastable::IsInterfaceImplemented"

        if not (rendered.Contains site) then
            failwith
                $"Without DynamicCastsAsDocumented, ChkCastAny is hidden at %A{Set.toList rendered}, not at %s{site}"

    /// Instructions of each shape that the JIT compiles to a call of a `CastHelpers` method at tier
    /// 0, and generic ones whose instance decides which: `isinst`, `castclass` and `unbox.any` of a
    /// reference type call the method `getCastingHelperStatic` (jitinterface.cpp) picks
    /// (`impCastClassOrIsInstToTree`); `unbox` and `unbox.any` of a value type, the one
    /// `getUnBoxHelper` picks; `box` of a struct, the one `getBoxHelper` picks
    /// (`impImportAndPushBox`), as does a `constrained.` call that boxes its receiver to call a
    /// method the type does not declare (`CORINFO_BOX_THIS`); `ldelema` of a reference type with no
    /// `readonly.` prefix, `LdelemaRef`; and `stelem.ref`, and `stelem` of a reference type,
    /// `StelemRef` (importer.cpp).
    let private castSource =
        """
using System;
using System.Collections.Generic;

namespace Casts;

public class Open { }

public struct Comparable : IComparable
{
    public int X;
    public int CompareTo(object o) => 0;
}

public enum Colour { Red }

public struct Plain { public int X; }

public struct Named
{
    public int X;
    public override string ToString() => "named";
    public override int GetHashCode() => 1;
}

public interface IDefaulted
{
    int Default() => 1;
}

public struct Defaulting : IDefaulted { public int X; }

public struct Implementing : IDefaulted
{
    public int X;
    public int Default() => 2;
}

public struct Explicit : IDefaulted
{
    public int X;
    int IDefaulted.Default() => 3;
}

public static class Cases
{
    public static object Nothing(object o) => o;

    public static object AsString(object o) => o as string;
    public static object AsOpen(object o) => o as Open;
    public static bool IsInt(object o) => o is int;
    public static bool IsNullableInt(object o) => o is int?;
    public static object AsDelegate(object o) => o as Action;
    public static object AsInterface(object o) => o as IDisposable;
    public static object AsGenericInterface(object o) => o as IList<int>;
    public static object AsVariantInterface(object o) => o as IEnumerable<object>;
    public static object AsVariantDelegate(object o) => o as Action<object>;
    public static object AsArray(object o) => o as string[];

    public static object CastOpen(object o) => (Open)o;
    public static object CastInterface(object o) => (IDisposable)o;
    public static object CastVariantInterface(object o) => (IEnumerable<object>)o;
    public static object CastArray(object o) => (int[])o;

    public static int UnboxInt(object o) => (int)o;
    public static Colour UnboxEnum(object o) => (Colour)o;
    public static Comparable UnboxStruct(object o) => (Comparable)o;
    public static int? UnboxNullable(object o) => (int?)o;
    public static int UnboxedField(object o) => ((Comparable)o).X;

    public static object BoxInt(int i) => i;
    public static object BoxEnum(Colour c) => c;
    public static object BoxStruct(Comparable c) => c;
    public static object BoxNullable(int? i) => i;

    // The JIT folds this to a constant, making no call; the analysis follows the calls regardless.
    public static bool BoxedIsInterface(Comparable c) => (object)c is IComparable;

    public static object BoxOf<T>(T t) => t;
    public static object BoxOfString(string s) => BoxOf(s);
    public static object BoxOfInt(int i) => BoxOf(i);
    public static object BoxOfEnum(Colour c) => BoxOf(c);
    public static object BoxOfStruct(Comparable c) => BoxOf(c);
    public static object BoxOfNullable(int? i) => BoxOf(i);

    // The JIT folds a byref-like type's box away, or refuses it.
    public static bool IsNull<T>(T t) where T : allows ref struct => t == null;
    public static bool IsNullOfSpan(Span<int> s) => IsNull(s);

    public static ref string ElementOfStrings(string[] a) => ref a[0];
    public static ref int ElementOfInts(int[] a) => ref a[0];
    public static ref T ElementOf<T>(T[] a) => ref a[0];
    public static ref string ElementOfString(string[] a) => ref ElementOf(a);
    public static ref int ElementOfInt(int[] a) => ref ElementOf(a);
    // A `constrained.` call on an element takes its address with `readonly. ldelema`.
    public static int HashOfElement<T>(T[] a) => a[0].GetHashCode();

    // A `constrained.` call boxes a value type's receiver where the method that runs is not one the
    // type declares.
    public static string NameOfNamed(Named n) => n.ToString();
    public static Type TypeOf<T>(T t) => t.GetType();
    public static Type TypeOfPlain(Plain p) => TypeOf(p);
    public static Type TypeOfNamed(Named n) => TypeOf(n);
    public static Type TypeOfNullable(int? i) => TypeOf(i);
    public static Type TypeOfInt(int i) => TypeOf(i);
    public static Type TypeOfColour(Colour c) => TypeOf(c);
    public static Type TypeOfString(string s) => TypeOf(s);
    public static int HashOf<T>(T t) => t.GetHashCode();
    public static int HashOfNamed(Named n) => HashOf(n);
    public static int HashOfNullable(int? i) => HashOf(i);
    public static string NameOf<T>(T t) => t.ToString();
    public static string NameOfNamedOf(Named n) => NameOf(n);
    public static int DefaultOf<T>(T t) where T : IDefaulted => t.Default();
    public static int DefaultOfDefaulting(Defaulting d) => DefaultOf(d);
    public static int DefaultOfImplementing(Implementing i) => DefaultOf(i);
    public static int DefaultOfExplicit(Explicit e) => DefaultOf(e);

    public static void Store(object[] a, object o) => a[0] = o;
    public static void StoreString(string[] a, string s) => a[0] = s;
    public static void StoreNull(object[] a) => a[0] = null;
    public static void StoreInt(int[] a, int i) => a[0] = i;
    public static void StoreStruct(Comparable[] a, Comparable c) => a[0] = c;
    public static void StoreOf<T>(T[] a, T t) => a[0] = t;
    public static void StoreOfString(string[] a, string s) => StoreOf(a, s);
    public static void StoreOfInt(int[] a, int i) => StoreOf(a, i);
    public static void StoreOfStruct(Comparable[] a, Comparable c) => StoreOf(a, c);

    public static bool Is<T>(object o) => o is T;
    public static bool IsOfString(object o) => Is<string>(o);
    public static bool IsOfInt(object o) => Is<int>(o);
    public static bool IsOfNullableInt(object o) => Is<int?>(o);
    public static bool IsOfSharedStruct(object o) => Is<KeyValuePair<int, string>>(o);
    public static bool IsOfNullableSharedStruct(object o) => Is<KeyValuePair<int, string>?>(o);

    public static bool IsNullable<T>(object o) where T : struct => o is T?;
    public static bool IsNullableOfInt(object o) => IsNullable<int>(o);
    public static bool IsNullableOfSharedStruct(object o) => IsNullable<KeyValuePair<int, string>>(o);

    public static bool IsList<T>(object o) => o is List<T>;
    public static bool IsListOfString(object o) => IsList<string>(o);

    // `unbox.any` of a type variable: `castclass` where it is a reference type.
    public static T Cast<T>(object o) => (T)o;
    public static string CastOfString(object o) => Cast<string>(o);
    public static int CastOfInt(object o) => Cast<int>(o);
    public static int? CastOfNullableInt(object o) => Cast<int?>(o);
}
"""

    /// The `CastHelpers` methods the JIT calls for an instruction, by name.
    let private helperEntries : string list =
        [
            "IsInstanceOfAny"
            "IsInstanceOfInterface"
            "IsInstanceOfClass"
            "ChkCastAny"
            "ChkCastInterface"
            "ChkCastClass"
            "Unbox"
            "Unbox_Nullable"
            "Box"
            "Box_Nullable"
            "LdelemaRef"
            "StelemRef"
        ]

    /// Which of `helperEntries` `method` of `declaring` in `fixture` reaches, besides those
    /// constructing an `InvalidCastException` does.
    let private helpersReached
        (analysis : EscapeAnalysisState)
        (fixture : DumpedAssembly)
        (declaring : string)
        (method : string)
        : EscapeAnalysisState * Set<string>
        =
        let corelib = hostCoreLib ()

        let entries =
            helperEntries
            |> List.map (fun name -> methodNamed corelib "System.Runtime.CompilerServices.CastHelpers" name, name)
            |> Map.ofList

        let analysis, reached =
            EscapeAnalysis.reachable analysis (methodNamed fixture declaring method)

        let analysis, constructing =
            EscapeAnalysis.reachable analysis (parameterlessConstructor corelib "System" "InvalidCastException")

        let named (keys : Set<MethodKey>) : Set<string> =
            keys |> Seq.choose (fun key -> Map.tryFind key entries) |> Set.ofSeq

        analysis, Set.difference (named reached) (named constructing)

    [<Test>]
    let ``an instruction calls the CastHelpers method the JIT picks for its type`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let fixture =
            Roslyn.compileOptimizedAssembly "Casts" OutputKind.DynamicallyLinkedLibrary [ castSource ]
            |> fun image -> Assembly.read loggerFactory (Some "Casts.dll") (new MemoryStream (image))

        let expectations =
            [
                "Nothing", []
                "AsString", [ "IsInstanceOfClass" ]
                "AsOpen", [ "IsInstanceOfClass" ]
                "IsInt", [ "IsInstanceOfClass" ]
                // CoreCLR's `isinst` of `Nullable<T>` asks about a boxed `T` (ECMA-335 III.4.6).
                "IsNullableInt", [ "IsInstanceOfClass" ]
                "AsDelegate", [ "IsInstanceOfClass" ]
                "AsInterface", [ "IsInstanceOfInterface" ]
                "AsGenericInterface", [ "IsInstanceOfInterface" ]
                "AsVariantInterface", [ "IsInstanceOfAny" ]
                "AsVariantDelegate", [ "IsInstanceOfAny" ]
                // `jithelpers.h` binds the array helper to the `Any` one.
                "AsArray", [ "IsInstanceOfAny" ]
                "CastOpen", [ "ChkCastClass" ]
                "CastInterface", [ "ChkCastInterface" ]
                "CastVariantInterface", [ "ChkCastAny" ]
                "CastArray", [ "ChkCastAny" ]
                "UnboxInt", [ "Unbox" ]
                "UnboxEnum", [ "Unbox" ]
                "UnboxStruct", [ "Unbox" ]
                "UnboxNullable", [ "Unbox_Nullable" ]
                "UnboxedField", [ "Unbox" ]
                // A primitive or an enum is boxed inline.
                "BoxInt", []
                "BoxEnum", []
                "BoxStruct", [ "Box" ]
                // `Box_Nullable` boxes the value by `Box`.
                "BoxNullable", [ "Box" ; "Box_Nullable" ]
                "BoxedIsInterface", [ "Box" ; "IsInstanceOfInterface" ]
                // A `box` of a reference type does nothing.
                "BoxOfString", []
                "BoxOfInt", []
                "BoxOfEnum", []
                "BoxOfStruct", [ "Box" ]
                "BoxOfNullable", [ "Box" ; "Box_Nullable" ]
                "IsNullOfSpan", []
                "ElementOfStrings", [ "LdelemaRef" ]
                "ElementOfInts", []
                // Shared code takes the address of a `__Canon`, a reference type's.
                "ElementOfString", [ "LdelemaRef" ]
                "ElementOfInt", []
                // The definition stands for every instantiation, as below.
                "HashOfElement", [ "Box" ; "Box_Nullable" ]
                // A struct's own override runs on the receiver's address.
                "NameOfNamed", []
                // `Object.GetType` is no method of the struct's, whatever it overrides.
                "TypeOfPlain", [ "Box" ]
                "TypeOfNamed", [ "Box" ]
                "TypeOfNullable", [ "Box" ; "Box_Nullable" ]
                // A primitive or an enum is boxed inline, and a reference type not at all.
                "TypeOfInt", []
                "TypeOfColour", []
                "TypeOfString", []
                "HashOfNamed", []
                // `Nullable<T>` declares `GetHashCode`.
                "HashOfNullable", []
                "NameOfNamedOf", []
                // A default interface body is no method of the struct's; one it implements, even
                // explicitly, is.
                "DefaultOfDefaulting", [ "Box" ]
                "DefaultOfImplementing", []
                "DefaultOfExplicit", []
                // A store of null calls it too, which tests for null itself.
                "Store", [ "StelemRef" ]
                "StoreString", [ "StelemRef" ]
                "StoreNull", [ "StelemRef" ]
                "StoreInt", []
                "StoreStruct", []
                // Shared code stores a `__Canon`, a reference type's, by `stelem` of the type variable.
                "StoreOfString", [ "StelemRef" ]
                "StoreOfInt", []
                "StoreOfStruct", []
                // Shared code looks the type up, and casts to `__Canon` by the `Any` helper.
                "IsOfString", [ "IsInstanceOfAny" ]
                "IsOfInt", [ "IsInstanceOfClass" ]
                "IsOfNullableInt", [ "IsInstanceOfClass" ]
                // A value type shared by its argument is still a class to cast to.
                "IsOfSharedStruct", [ "IsInstanceOfClass" ]
                // Shared, `Nullable<T>` is not asked about as `T`, so takes the `Any` helper.
                "IsOfNullableSharedStruct", [ "IsInstanceOfAny" ]
                "IsNullableOfInt", [ "IsInstanceOfClass" ]
                "IsNullableOfSharedStruct", [ "IsInstanceOfAny" ]
                "IsListOfString", [ "IsInstanceOfClass" ]
                "CastOfString", [ "ChkCastAny" ]
                // `unbox.any` of a value type unboxes.
                "CastOfInt", [ "Unbox" ]
                "CastOfNullableInt", [ "Unbox_Nullable" ]
                // The definitions stand for every instantiation.
                "Is", [ "IsInstanceOfAny" ; "IsInstanceOfClass" ]
                "IsNullable", [ "IsInstanceOfAny" ; "IsInstanceOfClass" ]
                "IsList", [ "IsInstanceOfClass" ]
                "Cast", [ "ChkCastAny" ; "Unbox" ; "Unbox_Nullable" ]
                "BoxOf", [ "Box" ; "Box_Nullable" ]
                "IsNull", [ "Box" ; "Box_Nullable" ]
                "ElementOf", [ "LdelemaRef" ]
                "StoreOf", [ "StelemRef" ]
                "TypeOf", [ "Box" ; "Box_Nullable" ]
                "HashOf", [ "Box" ; "Box_Nullable" ]
                "NameOf", [ "Box" ; "Box_Nullable" ]
                "DefaultOf", [ "Box" ; "Box_Nullable" ]
            ]

        let mutable analysis = analysisOver [ fixture ] id

        let failures =
            [
                for method, expected in expectations do
                    let next, reached = helpersReached analysis fixture "Casts.Cases" method
                    analysis <- next

                    if reached <> Set.ofList expected then
                        yield $"%s{method}: reaches %A{Set.toList reached}, not %A{expected}"
            ]

        if not failures.IsEmpty then
            failwith (String.concat "\n" failures)

    [<Test>]
    let ``a store into an array of references relies on what the cast helpers' fallback does`` () : unit =
        Net11Frontier.park Net11Frontier.nativeContractTable

        let _, loggerFactory = LoggerFactory.makeTest ()

        let fixture =
            Roslyn.compileOptimizedAssembly "Casts" OutputKind.DynamicallyLinkedLibrary [ castSource ]
            |> fun image -> Assembly.read loggerFactory (Some "Casts.dll") (new MemoryStream (image))

        let analysis (assumptions : Set<Assumption>) : EscapeAnalysisState =
            analysisOf
                (hostCoreLib ())
                (FrameworkUnderTest.runtimeDirs ())
                (hostTarget ())
                HardwareIntrinsicsProfile.ScalarOnly
                assumptions
                [ fixture ]
                id

        let key = methodNamed fixture "Casts.Cases" "Store"

        let assuming, escapes = EscapeAnalysis.escapes (analysis Assumption.all) key
        escapes.Unknown |> shouldEqual false

        Set.intersect escapes.Assumes castFallbackAssumes
        |> shouldEqual castFallbackAssumes

        render assuming escapes |> shouldContain "=System.ArrayTypeMismatchException"

        // Without what an `IDynamicInterfaceCastable` answers, the store is hidden where the
        // fallback asks it.
        let undynamic, sources =
            EscapeAnalysis.unknownSources (analysis (Set.remove Assumption.DynamicCastsAsDocumented Assumption.all)) key

        let rendered = sources |> Seq.map (renderSite undynamic) |> Set.ofSeq

        let site =
            "System.Runtime.InteropServices.DynamicInterfaceCastableHelpers::IsInterfaceImplemented IL VirtualCall "
            + "System.Runtime.InteropServices.IDynamicInterfaceCastable::IsInterfaceImplemented"

        if not (rendered.Contains site) then
            failwith $"Without DynamicCastsAsDocumented, a store is hidden at %A{Set.toList rendered}, not at %s{site}"

    /// C# unboxes a `Nullable<T>` only by `unbox.any`, but `unbox` of one is IL too, and the JIT
    /// calls `Unbox_Nullable` for it (`getUnBoxHelper`).
    [<Test>]
    let ``an unbox of a Nullable calls the CastHelpers method the JIT picks for it`` () : unit =
        let image =
            let builder =
                Emit.PersistedAssemblyBuilder (AssemblyName "Unboxes", typeof<obj>.Assembly)

            let modul = builder.DefineDynamicModule "Unboxes"

            let emitted =
                modul.DefineType (
                    "Emitted",
                    TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed
                )

            let il =
                emitted
                    .DefineMethod(
                        "UnboxNullable",
                        MethodAttributes.Public ||| MethodAttributes.Static,
                        typeof<bool>,
                        [| typeof<obj> |]
                    )
                    .GetILGenerator ()

            il.Emit Emit.OpCodes.Ldarg_0
            il.Emit (Emit.OpCodes.Unbox, typeof<Nullable<int>>)
            il.Emit (Emit.OpCodes.Call, typeof<Nullable<int>>.GetProperty("HasValue").GetGetMethod ())
            il.Emit Emit.OpCodes.Ret
            emitted.CreateType () |> ignore<Type>

            use stream = new MemoryStream ()
            builder.Save stream
            stream.ToArray ()

        let _, loggerFactory = LoggerFactory.makeTest ()

        let fixture =
            Assembly.read loggerFactory (Some "Unboxes.dll") (new MemoryStream (image))

        helpersReached (analysisOver [ fixture ] id) fixture "Emitted" "UnboxNullable"
        |> snd
        |> shouldEqual (Set.singleton "Unbox_Nullable")

    /// C# stores into an array of arrays or of pointers by `stelem.ref` or `stelem.i`, but `stelem`
    /// of an array type or a pointer type is IL too. The JIT calls `StelemRef` for an array, a
    /// reference type, and not for a pointer, which it stores as an integer (`CEE_STELEM`).
    [<Test>]
    let ``a stelem of an array type calls StelemRef, and one of a pointer type does not`` () : unit =
        let image =
            let builder =
                Emit.PersistedAssemblyBuilder (AssemblyName "Stores", typeof<obj>.Assembly)

            let modul = builder.DefineDynamicModule "Stores"

            let emitted =
                modul.DefineType (
                    "Emitted",
                    TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed
                )

            let store (name : string) (element : Type) =
                let il =
                    emitted
                        .DefineMethod(
                            name,
                            MethodAttributes.Public ||| MethodAttributes.Static,
                            typeof<Void>,
                            [| element.MakeArrayType () ; element |]
                        )
                        .GetILGenerator ()

                il.Emit Emit.OpCodes.Ldarg_0
                il.Emit Emit.OpCodes.Ldc_I4_0
                il.Emit Emit.OpCodes.Ldarg_1
                il.Emit (Emit.OpCodes.Stelem, element)
                il.Emit Emit.OpCodes.Ret

            store "StoreArray" typeof<int[]>
            store "StorePointer" (typeof<int>.MakePointerType ())
            emitted.CreateType () |> ignore<Type>

            use stream = new MemoryStream ()
            builder.Save stream
            stream.ToArray ()

        let _, loggerFactory = LoggerFactory.makeTest ()

        let fixture =
            Assembly.read loggerFactory (Some "Stores.dll") (new MemoryStream (image))

        let analysis = analysisOver [ fixture ] id
        let analysis, array = helpersReached analysis fixture "Emitted" "StoreArray"
        array |> shouldEqual (Set.singleton "StelemRef")
        let _, pointer = helpersReached analysis fixture "Emitted" "StorePointer"
        pointer |> shouldEqual Set.empty

    /// Whether `fixture`, read from `image`, reaches a `CastHelpers` box from each parameterless
    /// method of `cases` exactly where the real runtime allocates calling it, where each such method
    /// allocates nothing but by boxing. `boxing` is one the real runtime must allocate for and
    /// `notBoxing` one it must not, so that the comparison is not vacuous.
    let private boxesWhereTheRealRuntimeAllocates
        (image : byte[])
        (fixture : DumpedAssembly)
        (cases : string)
        (boxing : string)
        (notBoxing : string)
        : unit
        =
        let context =
            System.Runtime.Loader.AssemblyLoadContext ("Boxes", isCollectible = true)

        try
            let methods =
                context
                    .LoadFromStream(new MemoryStream (image))
                    .GetType(cases)
                    .GetMethods (BindingFlags.Public ||| BindingFlags.Static)
                |> List.ofArray

            // The first calls compile the case and what it calls, and make the `RuntimeType`
            // `GetType` answers.
            let allocates (case : MethodInfo) : bool =
                let run = case.CreateDelegate<Action> ()
                run.Invoke ()
                run.Invoke ()
                let before = GC.GetAllocatedBytesForCurrentThread ()

                for _ in 1..64 do
                    run.Invoke ()

                GC.GetAllocatedBytesForCurrentThread () - before > 0L

            let observed = methods |> List.map (fun case -> case.Name, allocates case)

            observed |> List.filter snd |> List.map fst |> shouldContain boxing

            observed |> List.filter (snd >> not) |> List.map fst |> shouldContain notBoxing

            let failures =
                ((analysisOver [ fixture ] id, []), observed)
                ||> List.fold (fun (analysis, failures) (name, allocated) ->
                    let analysis, reached = helpersReached analysis fixture cases name
                    let boxes = reached.Contains "Box" || reached.Contains "Box_Nullable"

                    if boxes = allocated then
                        analysis, failures
                    else
                        analysis,
                        $"%s{cases}::%s{name}: the real runtime allocates %b{allocated}, but the analysis reaches %A{Set.toList reached}"
                        :: failures
                )
                |> snd

            match failures with
            | [] -> ()
            | failures -> failures |> String.concat Environment.NewLine |> failwith
        finally
            context.Unload ()

    /// Calls the JIT compiles without optimising (`MethodImplOptions.NoOptimization`), as it compiles
    /// a method at tier 0. Each case makes one `constrained.` call on a struct, a `Nullable<T>` or a
    /// string, of a method that allocates nothing, so a case allocates only where the JIT boxes the
    /// call's receiver.
    let private boxThisSource =
        """
using System;
using System.Runtime.CompilerServices;

namespace BoxThis;

public struct Plain { public int X; }

public struct Named
{
    public int X;
    public override string ToString() => "named";
    public override int GetHashCode() => 1;
}

public interface IDefaulted { int Default() => 1; }

public interface IGenericDefaulted<T> { int Default() => 2; }

public struct Defaulting : IDefaulted, IGenericDefaulted<string> { public int X; }

public struct Implementing : IDefaulted, IGenericDefaulted<string>
{
    public int X;
    public int Default() => 3;
}

public struct Explicit : IDefaulted
{
    public int X;
    int IDefaulted.Default() => 4;
}

public static class Calls
{
    [MethodImpl(MethodImplOptions.NoOptimization)]
    public static Type TypeOf<T>(T t) => t.GetType();

    [MethodImpl(MethodImplOptions.NoOptimization)]
    public static int HashOf<T>(T t) => t.GetHashCode();

    [MethodImpl(MethodImplOptions.NoOptimization)]
    public static string NameOf<T>(T t) => t.ToString();

    [MethodImpl(MethodImplOptions.NoOptimization)]
    public static int DefaultOf<T>(T t) where T : IDefaulted => t.Default();

    [MethodImpl(MethodImplOptions.NoOptimization)]
    public static int GenericDefaultOf<T>(T t) where T : IGenericDefaulted<string> => t.Default();
}

public static class Cases
{
    [MethodImpl(MethodImplOptions.NoOptimization)]
    public static void TypeOfPlain() => Calls.TypeOf(new Plain());

    [MethodImpl(MethodImplOptions.NoOptimization)]
    public static void TypeOfNamed() => Calls.TypeOf(new Named());

    [MethodImpl(MethodImplOptions.NoOptimization)]
    public static void TypeOfNullable() => Calls.TypeOf((int?)1);

    [MethodImpl(MethodImplOptions.NoOptimization)]
    public static void TypeOfString() => Calls.TypeOf("s");

    [MethodImpl(MethodImplOptions.NoOptimization)]
    public static void HashOfNamed() => Calls.HashOf(new Named());

    [MethodImpl(MethodImplOptions.NoOptimization)]
    public static void HashOfNullable() => Calls.HashOf((int?)1);

    [MethodImpl(MethodImplOptions.NoOptimization)]
    public static void NameOfNamed() => Calls.NameOf(new Named());

    [MethodImpl(MethodImplOptions.NoOptimization)]
    public static void NameOfNamedDirectly()
    {
        var n = new Named();
        n.ToString();
    }

    [MethodImpl(MethodImplOptions.NoOptimization)]
    public static void DefaultOfDefaulting() => Calls.DefaultOf(new Defaulting());

    [MethodImpl(MethodImplOptions.NoOptimization)]
    public static void DefaultOfImplementing() => Calls.DefaultOf(new Implementing());

    [MethodImpl(MethodImplOptions.NoOptimization)]
    public static void DefaultOfExplicit() => Calls.DefaultOf(new Explicit());

    [MethodImpl(MethodImplOptions.NoOptimization)]
    public static void GenericDefaultOfDefaulting() => Calls.GenericDefaultOf(new Defaulting());

    [MethodImpl(MethodImplOptions.NoOptimization)]
    public static void GenericDefaultOfImplementing() => Calls.GenericDefaultOf(new Implementing());
}
"""

    [<Test>]
    let ``a constrained call reaches a CastHelpers box exactly where the real runtime allocates for it`` () : unit =
        let image =
            Roslyn.compileOptimizedAssembly "BoxThis" OutputKind.DynamicallyLinkedLibrary [ boxThisSource ]

        let _, loggerFactory = LoggerFactory.makeTest ()

        let fixture =
            Assembly.read loggerFactory (Some "BoxThis.dll") (new MemoryStream (image))

        boxesWhereTheRealRuntimeAllocates image fixture "BoxThis.Cases" "DefaultOfDefaulting" "DefaultOfImplementing"

    /// C# calls a struct's own method with `call`, but `constrained.` may name one, and CoreCLR
    /// takes a method that is not virtual to be none the type declares
    /// (`TryResolveConstraintMethodApprox`, methodtable.cpp): it boxes the receiver and passes the
    /// box where the method expects the struct's address. It boxes nothing for one that is virtual.
    [<Test>]
    let ``a constrained call of a struct's own method boxes its receiver where the method is not virtual`` () : unit =
        let image =
            let builder =
                Emit.PersistedAssemblyBuilder (AssemblyName "OwnMethods", typeof<obj>.Assembly)

            let modul = builder.DefineDynamicModule "OwnMethods"

            let measured =
                modul.DefineType (
                    "IMeasured",
                    TypeAttributes.Public ||| TypeAttributes.Interface ||| TypeAttributes.Abstract
                )

            let measure =
                measured.DefineMethod (
                    "Measure",
                    MethodAttributes.Public
                    ||| MethodAttributes.Virtual
                    ||| MethodAttributes.Abstract
                    ||| MethodAttributes.HideBySig
                    ||| MethodAttributes.NewSlot,
                    typeof<int>,
                    [||]
                )

            let measured = measured.CreateType ()

            let owner =
                modul.DefineType (
                    "Owner",
                    TypeAttributes.Public
                    ||| TypeAttributes.Sealed
                    ||| TypeAttributes.SequentialLayout,
                    typeof<ValueType>
                )

            owner.AddInterfaceImplementation measured

            owner.DefineField ("X", typeof<int>, FieldAttributes.Public)
            |> ignore<Emit.FieldBuilder>

            let constant (method : Emit.MethodBuilder) =
                let il = method.GetILGenerator ()
                il.Emit Emit.OpCodes.Ldc_I4_1
                il.Emit Emit.OpCodes.Ret
                method

            let plain =
                owner.DefineMethod ("Plain", MethodAttributes.Public ||| MethodAttributes.HideBySig, typeof<int>, [||])
                |> constant

            let final =
                owner.DefineMethod (
                    "Measure",
                    MethodAttributes.Public
                    ||| MethodAttributes.Virtual
                    ||| MethodAttributes.Final
                    ||| MethodAttributes.HideBySig
                    ||| MethodAttributes.NewSlot,
                    typeof<int>,
                    [||]
                )
                |> constant

            owner.DefineMethodOverride (final, measure)
            let owner = owner.CreateType ()

            let emitted =
                modul.DefineType (
                    "Emitted",
                    TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed
                )

            let case (name : string) (called : Emit.MethodBuilder) =
                let method =
                    emitted.DefineMethod (name, MethodAttributes.Public ||| MethodAttributes.Static, typeof<Void>, [||])

                method.SetImplementationFlags MethodImplAttributes.NoOptimization
                let il = method.GetILGenerator ()
                il.DeclareLocal owner |> ignore<Emit.LocalBuilder>
                il.Emit (Emit.OpCodes.Ldloca_S, 0uy)
                il.Emit (Emit.OpCodes.Constrained, owner)
                il.Emit (Emit.OpCodes.Callvirt, called)
                il.Emit Emit.OpCodes.Pop
                il.Emit Emit.OpCodes.Ret

            case "OwnPlain" plain
            case "OwnFinal" final
            emitted.CreateType () |> ignore<Type>

            use stream = new MemoryStream ()
            builder.Save stream
            stream.ToArray ()

        let _, loggerFactory = LoggerFactory.makeTest ()

        let fixture =
            Assembly.read loggerFactory (Some "OwnMethods.dll") (new MemoryStream (image))

        boxesWhereTheRealRuntimeAllocates image fixture "Emitted" "OwnPlain" "OwnFinal"

    /// A struct that inherits two equally specific default bodies of one interface method has none
    /// of its own, so a `constrained.` call of the method boxes the receiver, and the dispatch on
    /// the box then raises `AmbiguousImplementationException`. Measured on .NET 10 under MinOpts:
    /// the JIT calls `CORINFO_HELP_BOX` before the interface call.
    [<Test>]
    let ``a constrained call whose dispatch fails boxes its receiver before it raises`` () : unit =
        let image =
            let builder =
                Emit.PersistedAssemblyBuilder (AssemblyName "Diamond", typeof<obj>.Assembly)

            let modul = builder.DefineDynamicModule "Diamond"

            let interfaceAttributes =
                TypeAttributes.Public ||| TypeAttributes.Interface ||| TypeAttributes.Abstract

            let root = modul.DefineType ("IRoot", interfaceAttributes)

            let rootMethod =
                root.DefineMethod (
                    "M",
                    MethodAttributes.Public
                    ||| MethodAttributes.Virtual
                    ||| MethodAttributes.Abstract
                    ||| MethodAttributes.HideBySig
                    ||| MethodAttributes.NewSlot,
                    typeof<int>,
                    [||]
                )

            let root = root.CreateType ()

            let side (name : string) (value : int) : Type =
                let side = modul.DefineType (name, interfaceAttributes)
                side.AddInterfaceImplementation root

                let body =
                    side.DefineMethod (
                        "IRoot.M",
                        MethodAttributes.Private
                        ||| MethodAttributes.Virtual
                        ||| MethodAttributes.Final
                        ||| MethodAttributes.HideBySig
                        ||| MethodAttributes.NewSlot,
                        typeof<int>,
                        [||]
                    )

                let il = body.GetILGenerator ()
                il.Emit (Emit.OpCodes.Ldc_I4, value)
                il.Emit Emit.OpCodes.Ret
                side.DefineMethodOverride (body, rootMethod)
                side.CreateType ()

            let left = side "ILeft" 1
            let right = side "IRight" 2

            let diamond =
                modul.DefineType (
                    "Diamond",
                    TypeAttributes.Public
                    ||| TypeAttributes.Sealed
                    ||| TypeAttributes.SequentialLayout,
                    typeof<ValueType>
                )

            diamond.AddInterfaceImplementation root
            diamond.AddInterfaceImplementation left
            diamond.AddInterfaceImplementation right

            diamond.DefineField ("X", typeof<int>, FieldAttributes.Public)
            |> ignore<Emit.FieldBuilder>

            let diamond = diamond.CreateType ()

            let emitted =
                modul.DefineType (
                    "Emitted",
                    TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed
                )

            let il =
                emitted
                    .DefineMethod("Call", MethodAttributes.Public ||| MethodAttributes.Static, typeof<int>, [||])
                    .GetILGenerator ()

            il.DeclareLocal diamond |> ignore<Emit.LocalBuilder>
            il.Emit (Emit.OpCodes.Ldloca_S, 0uy)
            il.Emit (Emit.OpCodes.Initobj, diamond)
            il.Emit (Emit.OpCodes.Ldloca_S, 0uy)
            il.Emit (Emit.OpCodes.Constrained, diamond)
            il.Emit (Emit.OpCodes.Callvirt, rootMethod)
            il.Emit Emit.OpCodes.Ret
            emitted.CreateType () |> ignore<Type>

            use stream = new MemoryStream ()
            builder.Save stream
            stream.ToArray ()

        let context =
            System.Runtime.Loader.AssemblyLoadContext ("Diamond", isCollectible = true)

        try
            let raised =
                try
                    context
                        .LoadFromStream(new MemoryStream (image))
                        .GetType("Emitted")
                        .GetMethod("Call")
                        .Invoke ((null : obj), Array.empty<obj>)
                    |> ignore<obj>

                    None
                with :? TargetInvocationException as e ->
                    Some (e.InnerException.GetType ())

            raised
            |> shouldEqual (Some typeof<System.Runtime.AmbiguousImplementationException>)
        finally
            context.Unload ()

        let _, loggerFactory = LoggerFactory.makeTest ()

        let fixture =
            Assembly.read loggerFactory (Some "Diamond.dll") (new MemoryStream (image))

        let analysis = analysisOver [ fixture ] id
        let analysis, reached = helpersReached analysis fixture "Emitted" "Call"
        reached |> shouldContain "Box"

        let analysis, escapes =
            EscapeAnalysis.escapes analysis (methodNamed fixture "Emitted" "Call")

        render analysis escapes
        |> shouldContain "=System.Runtime.AmbiguousImplementationException"

    /// C# makes a ref struct implement every method of an interface itself, but IL may leave it to
    /// inherit a default interface body. A `constrained.` call of that body on the ref struct boxes
    /// the receiver, which CoreCLR's JIT refuses to compile (`getBoxHelper`, jitinterface.cpp,
    /// raises `InvalidProgramException`), whether the type is named directly or through a type
    /// variable that allows it. It refuses before the method runs, so no handler in the method catches
    /// it, and no value a caller passes keeps it from compiling the call.
    [<Test>]
    let ``a constrained call that would box a ref struct is opaque, and the real runtime refuses it`` () : unit =
        let image =
            let builder =
                Emit.PersistedAssemblyBuilder (AssemblyName "RefBoxes", typeof<obj>.Assembly)

            let modul = builder.DefineDynamicModule "RefBoxes"

            let defaulted =
                modul.DefineType (
                    "IDefaulted",
                    TypeAttributes.Public ||| TypeAttributes.Interface ||| TypeAttributes.Abstract
                )

            let defaultBody =
                defaulted.DefineMethod (
                    "Default",
                    MethodAttributes.Public
                    ||| MethodAttributes.Virtual
                    ||| MethodAttributes.HideBySig
                    ||| MethodAttributes.NewSlot,
                    typeof<int>,
                    [||]
                )

            let il = defaultBody.GetILGenerator ()
            il.Emit Emit.OpCodes.Ldc_I4_1
            il.Emit Emit.OpCodes.Ret
            let defaulted = defaulted.CreateType ()

            let refStruct =
                modul.DefineType (
                    "RefDefaulting",
                    TypeAttributes.Public
                    ||| TypeAttributes.Sealed
                    ||| TypeAttributes.SequentialLayout,
                    typeof<ValueType>
                )

            refStruct.AddInterfaceImplementation defaulted

            refStruct.SetCustomAttribute (
                Emit.CustomAttributeBuilder (
                    typeof<System.Runtime.CompilerServices.IsByRefLikeAttribute>.GetConstructor [||],
                    [||]
                )
            )

            refStruct.DefineField ("X", typeof<int>, FieldAttributes.Public)
            |> ignore<Emit.FieldBuilder>

            let refStruct = refStruct.CreateType ()

            let calls =
                modul.DefineType ("Calls", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

            let defaultOf =
                calls.DefineMethod ("DefaultOf", MethodAttributes.Public ||| MethodAttributes.Static)

            let parameter = (defaultOf.DefineGenericParameters [| "T" |]).[0]
            // `gpAllowByRefLike` (corhdr.h), which `allows ref struct` sets.
            parameter.SetGenericParameterAttributes (enum<GenericParameterAttributes> 0x20)
            parameter.SetInterfaceConstraints [| defaulted |]
            defaultOf.SetReturnType typeof<int>
            defaultOf.SetParameters [| parameter.AsType () |]
            let il = defaultOf.GetILGenerator ()
            il.Emit (Emit.OpCodes.Ldarga_S, 0uy)
            il.Emit (Emit.OpCodes.Constrained, parameter.AsType ())
            il.Emit (Emit.OpCodes.Callvirt, defaultBody)
            il.Emit Emit.OpCodes.Ret

            let caller (name : string) (call : Emit.ILGenerator -> unit) =
                let il =
                    calls
                        .DefineMethod(name, MethodAttributes.Public ||| MethodAttributes.Static, typeof<int>, [||])
                        .GetILGenerator ()

                il.DeclareLocal refStruct |> ignore<Emit.LocalBuilder>
                il.Emit (Emit.OpCodes.Ldloca_S, 0uy)
                il.Emit (Emit.OpCodes.Initobj, refStruct)
                call il
                il.Emit Emit.OpCodes.Ret

            caller
                "Directly"
                (fun il ->
                    il.Emit (Emit.OpCodes.Ldloca_S, 0uy)
                    il.Emit (Emit.OpCodes.Constrained, refStruct)
                    il.Emit (Emit.OpCodes.Callvirt, defaultBody)
                )

            caller
                "ThroughVariable"
                (fun il ->
                    il.Emit Emit.OpCodes.Ldloc_0
                    il.Emit (Emit.OpCodes.Call, defaultOf.MakeGenericMethod [| refStruct |])
                )

            caller
                "Caught"
                (fun il ->
                    il.BeginExceptionBlock () |> ignore<Emit.Label>
                    il.Emit (Emit.OpCodes.Ldloca_S, 0uy)
                    il.Emit (Emit.OpCodes.Constrained, refStruct)
                    il.Emit (Emit.OpCodes.Callvirt, defaultBody)
                    il.Emit Emit.OpCodes.Pop
                    il.BeginCatchBlock typeof<obj>
                    il.Emit Emit.OpCodes.Pop
                    il.EndExceptionBlock ()
                    il.Emit Emit.OpCodes.Ldc_I4_0
                )

            let gated =
                calls.DefineMethod (
                    "Gated",
                    MethodAttributes.Public ||| MethodAttributes.Static,
                    typeof<int>,
                    [| typeof<bool> |]
                )

            let il = gated.GetILGenerator ()
            il.DeclareLocal refStruct |> ignore<Emit.LocalBuilder>
            let skip = il.DefineLabel ()
            il.Emit Emit.OpCodes.Ldarg_0
            il.Emit (Emit.OpCodes.Brfalse, skip)
            il.Emit (Emit.OpCodes.Ldloca_S, 0uy)
            il.Emit (Emit.OpCodes.Initobj, refStruct)
            il.Emit (Emit.OpCodes.Ldloca_S, 0uy)
            il.Emit (Emit.OpCodes.Constrained, refStruct)
            il.Emit (Emit.OpCodes.Callvirt, defaultBody)
            il.Emit Emit.OpCodes.Ret
            il.MarkLabel skip
            il.Emit Emit.OpCodes.Ldc_I4_0
            il.Emit Emit.OpCodes.Ret

            // At tier 0 the JIT compiles a block behind a branch on a constant too; optimising, it
            // folds the branch and never sees the call.
            let unreached =
                calls.DefineMethod ("Unreached", MethodAttributes.Public ||| MethodAttributes.Static, typeof<int>, [||])

            unreached.SetImplementationFlags MethodImplAttributes.NoOptimization
            let il = unreached.GetILGenerator ()
            il.DeclareLocal refStruct |> ignore<Emit.LocalBuilder>
            let skip = il.DefineLabel ()
            il.Emit Emit.OpCodes.Ldc_I4_0
            il.Emit (Emit.OpCodes.Brfalse, skip)
            il.Emit (Emit.OpCodes.Ldloca_S, 0uy)
            il.Emit (Emit.OpCodes.Initobj, refStruct)
            il.Emit (Emit.OpCodes.Ldloca_S, 0uy)
            il.Emit (Emit.OpCodes.Constrained, refStruct)
            il.Emit (Emit.OpCodes.Callvirt, defaultBody)
            il.Emit Emit.OpCodes.Ret
            il.MarkLabel skip
            il.Emit Emit.OpCodes.Ldc_I4_0
            il.Emit Emit.OpCodes.Ret

            let gatedOff =
                calls.DefineMethod ("GatedOff", MethodAttributes.Public ||| MethodAttributes.Static, typeof<int>, [||])

            // Optimising, the JIT inlines `Gated`, folds the branch on the constant, and never sees
            // the call; at tier 0 it inlines nothing.
            gatedOff.SetImplementationFlags MethodImplAttributes.NoOptimization
            let il = gatedOff.GetILGenerator ()
            il.Emit Emit.OpCodes.Ldc_I4_0
            il.Emit (Emit.OpCodes.Call, gated)
            il.Emit Emit.OpCodes.Ret

            calls.CreateType () |> ignore<Type>

            use stream = new MemoryStream ()
            builder.Save stream
            stream.ToArray ()

        let _, loggerFactory = LoggerFactory.makeTest ()

        let fixture =
            Assembly.read loggerFactory (Some "RefBoxes.dll") (new MemoryStream (image))

        let context =
            System.Runtime.Loader.AssemblyLoadContext ("RefBoxes", isCollectible = true)

        try
            let calls = context.LoadFromStream(new MemoryStream (image)).GetType "Calls"

            for name in [ "Directly" ; "ThroughVariable" ; "Caught" ; "Unreached" ; "GatedOff" ] do
                let raised =
                    try
                        calls.GetMethod(name).Invoke ((null : obj), Array.empty<obj>) |> ignore<obj>
                        None
                    with :? TargetInvocationException as e ->
                        Some (e.InnerException.GetType ())

                (name, raised) |> shouldEqual (name, Some typeof<InvalidProgramException>)
        finally
            context.Unload ()

        let mutable analysis = analysisOver [ fixture ] id

        for name, refused in
            [
                "Directly", "Directly"
                "ThroughVariable", "DefaultOf"
                "Caught", "Caught"
                "Unreached", "Unreached"
                "GatedOff", "Gated"
            ] do
            let next, sources =
                EscapeAnalysis.unknownSources analysis (methodNamed fixture "Calls" name)

            analysis <- next

            sources
            |> Seq.map (renderSite analysis)
            |> Set.ofSeq
            |> shouldEqual (Set.singleton $"Calls::%s{refused} entry BoxedByRefLikeReceiver")

        // Asked about by itself, the definition stands for every instantiation, a ref struct's
        // among them.
        let analysis, definition =
            EscapeAnalysis.unknownSources analysis (methodNamed fixture "Calls" "DefaultOf")

        // Some instance boxes a struct, which .NET 11's `CastHelpers.Box` copies through a QCall
        // the table has no row for (`Net11Frontier.spanHelpersMemoryQCalls`).
        definition
        |> Seq.filter (isNet11MemoryQCall (FrameworkUnderTest.runtime ()) analysis >> not)
        |> Seq.map (renderSite analysis)
        |> Set.ofSeq
        |> shouldEqual (
            Set.ofList
                [
                    "Calls::DefaultOf entry BoxedByRefLikeReceiver"
                    "Calls::DefaultOf IL VirtualCall IDefaulted::Default"
                ]
        )

    /// The `CastHelpers` methods that `jithelpers.h` binds the JIT's helpers for boxing and unboxing
    /// to, and an exception each can raise: a box allocates, and an unbox of the wrong type throws
    /// through the QCall `ThrowInvalidCastException`.
    let private boxingHelpers : (string * string) list =
        [
            "Box", "=System.OutOfMemoryException"
            "Box_Nullable", "=System.OutOfMemoryException"
            "Unbox", "=System.InvalidCastException"
            "Unbox_Nullable", "=System.InvalidCastException"
            "Unbox_TypeTest", "=System.InvalidCastException"
        ]

    [<Test>]
    let ``every box and unbox helper the JIT calls is seen through`` () : unit =
        Net11Frontier.park Net11Frontier.nativeContractTable

        let corelib = hostCoreLib ()

        let analysis =
            analysisOf
                corelib
                (FrameworkUnderTest.runtimeDirs ())
                (hostTarget ())
                HardwareIntrinsicsProfile.ScalarOnly
                Assumption.all
                []
                id

        let _ =
            (analysis, boxingHelpers)
            ||> List.fold (fun analysis (name, raised) ->
                let key = methodNamed corelib "System.Runtime.CompilerServices.CastHelpers" name

                let analysis, sources = EscapeAnalysis.unknownSources analysis key

                (name, sources |> Seq.map (renderSite analysis) |> List.ofSeq)
                |> shouldEqual (name, [])

                let analysis, escapes = EscapeAnalysis.escapes analysis key

                (name, (render analysis escapes).Contains raised) |> shouldEqual (name, true)

                analysis
            )

        ()

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

    // Each architecture's base instruction set, and a class nested twice in one. On a CPU of the
    // other architecture, CoreLib's query is a stub returning false, and the JIT compiles a call of
    // it to false all the same.
    public static void X86Guarded()
    {
        if (System.Runtime.Intrinsics.X86.X86Base.IsSupported) throw new Guarded();
    }

    public static void ArmGuarded()
    {
        if (System.Runtime.Intrinsics.Arm.ArmBase.IsSupported) throw new Guarded();
    }

    public static void NestedGuarded()
    {
        if (System.Runtime.Intrinsics.X86.Avx10v2.V512.X64.IsSupported) throw new Guarded();
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
            // Neither CPU has any of these instruction sets.
            for accelerated in [ false ; true ] do
                for guardedBy in [ "X86Guarded" ; "ArmGuarded" ; "NestedGuarded" ] do
                    accelerated,
                    { expect "Guarded.Cases" guardedBy with
                        Excludes = [ guarded ]
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

    /// Code that asks whether a `typeof` is a value type, which C# compiles to `ldtoken; call
    /// GetTypeFromHandle; callvirt get_IsValueType`.
    let private valueTypeQuerySource =
        """
using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;

namespace ValueTypeQuery;

// Exceptions whose construction raises nothing, unlike CoreLib's, which look up their messages.
public class Value : Exception { }

public class Reference : Exception { }

public static class Cases
{
    public static void OfInt()
    {
        if (typeof(int).IsValueType) throw new Value();
        throw new Reference();
    }

    public static void OfString()
    {
        if (typeof(string).IsValueType) throw new Value();
        throw new Reference();
    }

    // The base types of value types are classes.
    public static void OfEnum()
    {
        if (typeof(Enum).IsValueType) throw new Value();
        throw new Reference();
    }

    public static void Generic<T>()
    {
        if (typeof(T).IsValueType) throw new Value();
        throw new Reference();
    }

    public static void WithInt() => Generic<int>();

    public static void WithString() => Generic<string>();

    public static void WithNullable() => Generic<int?>();

    public static void WithDayOfWeek() => Generic<DayOfWeek>();

    // A type argument with a custom modifier in it: the `modreq` that marks an `out` parameter.
    public static unsafe void OfModifiedArgument()
    {
        if (typeof(List<delegate*<out int, void>[]>).IsValueType) throw new Value();
        throw new Reference();
    }

    [MethodImpl(MethodImplOptions.NoOptimization)]
    public static void Unoptimized()
    {
        if (typeof(int).IsValueType) throw new Value();
        throw new Reference();
    }

    // The getter's operand arrives two ways, so it is not the one `GetTypeFromHandle` it would
    // have to be for the JIT to fold the query.
    public static void EitherType(bool flag)
    {
        if ((flag ? typeof(int) : typeof(string)).IsValueType) throw new Value();
        throw new Reference();
    }
}
"""

    /// What each method of `valueTypeQuerySource` lets escape where the JIT compiles the query to a
    /// constant in place of the calls.
    let private valueTypeQueryExpectations : Expectation list =
        let value = "=ValueTypeQuery.Value"
        let reference = "=ValueTypeQuery.Reference"

        let folded (name : string) (contains : string list) (excludes : string list) =
            { expect "ValueTypeQuery.Cases" name with
                Contains = contains
                Excludes = excludes
                Unknown = Some false
                // A `throw` of null raises a `NullReferenceException`, which the runtime constructs.
                Assumes = Some (Assumption.JitOptimises :: constructing)
            }

        [
            folded "OfInt" [ value ] [ reference ]
            folded "OfString" [ reference ] [ value ]
            folded "OfEnum" [ reference ] [ value ]
            // The open definition does not decide the branch, but makes no call either way.
            folded "Generic" [ value ; reference ] []
            folded "WithInt" [ value ] [ reference ]
            folded "WithString" [ reference ] [ value ]
            folded "WithNullable" [ value ] [ reference ]
            folded "WithDayOfWeek" [ value ] [ reference ]
            folded "OfModifiedArgument" [ value ; reference ] []
        ]

    /// The image of `valueTypeQuerySource`, optimized as CoreLib is unless `debug`.
    let private valueTypeQueryFixture (debug : bool) : DumpedAssembly =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let image =
            if debug then
                Roslyn.compileAssembly "ValueTypeQuery" OutputKind.DynamicallyLinkedLibrary [] [ valueTypeQuerySource ]
            else
                Roslyn.compileOptimizedAssembly
                    "ValueTypeQuery"
                    OutputKind.DynamicallyLinkedLibrary
                    [ valueTypeQuerySource ]

        Assembly.read loggerFactory (Some "ValueTypeQuery.dll") (new MemoryStream (image))

    /// Each of `methods` makes the calls: its answer is unknown, from what `get_IsValueType` reaches,
    /// and relies on no assumption that the JIT optimises.
    let private makesTheCalls
        (analysis : EscapeAnalysisState)
        (fixture : DumpedAssembly)
        (methods : string list)
        : unit
        =
        let mutable analysis = analysis

        for name in methods do
            let next, escapes =
                EscapeAnalysis.escapes analysis (methodNamed fixture "ValueTypeQuery.Cases" name)

            analysis <- next

            if not escapes.Unknown || escapes.Assumes.Contains Assumption.JitOptimises then
                failwith $"%s{name}: unknown %b{escapes.Unknown}, assuming %A{Set.toList escapes.Assumes}"

    [<Test>]
    let ``asking whether a typeof is a value type is a constant, which the instance decides, where the JIT optimises``
        ()
        : unit
        =
        let fixture = valueTypeQueryFixture false
        Debuggable.allowsJitOptimisation fixture |> shouldEqual (Some true)

        let analysis, failures =
            unmet (analysisOver [ fixture ] id) fixture valueTypeQueryExpectations

        if not failures.IsEmpty then
            failures |> String.concat Environment.NewLine |> failwith

        // A method marked `NoOptimization` makes the calls, as does one in which the getter's
        // operand is not the `GetTypeFromHandle` straight before it.
        makesTheCalls analysis fixture [ "Unoptimized" ; "EitherType" ]

    [<Test>]
    let ``asking whether a typeof is a value type makes the calls where the assembly disables optimisation`` () : unit =
        let fixture = valueTypeQueryFixture true
        Debuggable.allowsJitOptimisation fixture |> shouldEqual (Some false)

        valueTypeQueryExpectations
        |> List.map (fun expectation -> snd expectation.Method)
        |> makesTheCalls (analysisOver [ fixture ] id) fixture

    /// Methods of `Probe.Cases` that branch on whether a `typeof` is a value type, in IL that C#
    /// does not write:
    ///   - `Asks`, of `int`;
    ///   - `AsksAfterReturnAddress`, of `int`, after calling `StubHelpers.NextCallReturnAddress`;
    ///   - `AsksOfEither(bool)`, of `int` or `string`, whose two `ldtoken`s branch to one call of
    ///     `GetTypeFromHandle`;
    ///   - `AsksOfOther`, of what `Other(RuntimeTypeHandle)`, which is not `GetTypeFromHandle`,
    ///     returns for the handle of `string`: the type of `int`.
    let private emitValueTypeQueries () : byte[] =
        let builder =
            Emit.PersistedAssemblyBuilder (AssemblyName "ReturnAddress", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "ReturnAddress"

        let cases =
            modul.DefineType (
                "Probe.Cases",
                TypeAttributes.Public
                ||| TypeAttributes.Class
                ||| TypeAttributes.Abstract
                ||| TypeAttributes.Sealed
            )

        let nextCallReturnAddress =
            typeof<obj>.Assembly
                .GetType("System.StubHelpers.StubHelpers", true)
                .GetMethod ("NextCallReturnAddress", BindingFlags.NonPublic ||| BindingFlags.Static)

        let getTypeFromHandle =
            typeof<Type>.GetMethod ("GetTypeFromHandle", [| typeof<RuntimeTypeHandle> |])

        let isValueType = typeof<Type>.GetProperty("IsValueType").GetGetMethod ()
        let exceptionConstructor = typeof<Exception>.GetConstructor Type.EmptyTypes

        let other =
            cases.DefineMethod (
                "Other",
                MethodAttributes.Public ||| MethodAttributes.Static,
                typeof<Type>,
                [| typeof<RuntimeTypeHandle> |]
            )

        let il = other.GetILGenerator ()
        il.Emit (Emit.OpCodes.Ldtoken, typeof<int>)
        il.Emit (Emit.OpCodes.Call, getTypeFromHandle)
        il.Emit Emit.OpCodes.Ret

        for name, asksReturnAddress, ofEither, ofOther in
            [
                "Asks", false, false, false
                "AsksAfterReturnAddress", true, false, false
                "AsksOfEither", false, true, false
                "AsksOfOther", false, false, true
            ] do
            let parameters = if ofEither then [| typeof<bool> |] else [||]

            let method =
                cases.DefineMethod (name, MethodAttributes.Public ||| MethodAttributes.Static, typeof<Void>, parameters)

            let il = method.GetILGenerator ()
            let otherwise = il.DefineLabel ()

            if asksReturnAddress then
                il.Emit (Emit.OpCodes.Call, nextCallReturnAddress)
                il.Emit Emit.OpCodes.Pop

            if ofEither then
                let ofInt = il.DefineLabel ()
                let asked = il.DefineLabel ()
                il.Emit Emit.OpCodes.Ldarg_0
                il.Emit (Emit.OpCodes.Brtrue, ofInt)
                il.Emit (Emit.OpCodes.Ldtoken, typeof<string>)
                il.Emit (Emit.OpCodes.Br, asked)
                il.MarkLabel ofInt
                il.Emit (Emit.OpCodes.Ldtoken, typeof<int>)
                il.MarkLabel asked
            elif ofOther then
                il.Emit (Emit.OpCodes.Ldtoken, typeof<string>)
            else
                il.Emit (Emit.OpCodes.Ldtoken, typeof<int>)

            il.Emit (Emit.OpCodes.Call, (if ofOther then other :> MethodInfo else getTypeFromHandle))
            il.Emit (Emit.OpCodes.Callvirt, isValueType)
            il.Emit (Emit.OpCodes.Brfalse, otherwise)
            il.Emit (Emit.OpCodes.Newobj, exceptionConstructor)
            il.Emit Emit.OpCodes.Throw
            il.MarkLabel otherwise
            il.Emit (Emit.OpCodes.Newobj, exceptionConstructor)
            il.Emit Emit.OpCodes.Throw

        cases.CreateType () |> ignore<Type>
        use stream = new MemoryStream ()
        builder.Save stream
        stream.ToArray ()

    [<Test>]
    let ``asking whether a typeof is a value type makes the calls in IL the JIT does not fold`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let fixture =
            Assembly.read loggerFactory (Some "ReturnAddress.dll") (new MemoryStream (emitValueTypeQueries ()))

        let analysis = analysisOver [ fixture ] id

        let analysis, asks =
            EscapeAnalysis.escapes analysis (methodNamed fixture "Probe.Cases" "Asks")

        asks.Assumes.Contains Assumption.JitOptimises |> shouldEqual true

        // The JIT expands only the intrinsics it must in a method that asks for the next call's
        // return address, a call that more than one way reaches has an operand it cannot name, and
        // only a type that `GetTypeFromHandle` made is a `typeof`.
        let mutable analysis = analysis

        for name in [ "AsksAfterReturnAddress" ; "AsksOfEither" ; "AsksOfOther" ] do
            let next, escapes =
                EscapeAnalysis.escapes analysis (methodNamed fixture "Probe.Cases" name)

            analysis <- next

            if escapes.Assumes.Contains Assumption.JitOptimises then
                failwith $"%s{name} relies on the JIT's folding the query"

    [<Test>]
    let ``asking whether a typeof is a value type makes the calls unless the JIT is assumed to optimise`` () : unit =
        let fixture = valueTypeQueryFixture false

        let analysis =
            analysisOf
                (hostCoreLib ())
                (FrameworkUnderTest.runtimeDirs ())
                (hostTarget ())
                HardwareIntrinsicsProfile.ScalarOnly
                (Set.remove Assumption.JitOptimises Assumption.all)
                [ fixture ]
                id

        valueTypeQueryExpectations
        |> List.map (fun expectation -> snd expectation.Method)
        |> makesTheCalls analysis fixture

    /// A client asking whether a `typeof` is a value type, of classes whose base classes are in an
    /// assembly the analysis can find but has not loaded: `Derived`, whose base the provider has,
    /// and `DerivedFromGone`, whose base the provider no longer has.
    [<Test>]
    let ``asking whether a typeof is a value type loads its base classes`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let provider (withGone : bool) : string =
            "namespace Provider; public class Base { }"
            + (if withGone then " public class GoneType { }" else "")

        let client =
            """
using System;

namespace Client;

public class Value : Exception { }

public class Reference : Exception { }

public class Derived : Provider.Base { }

public class DerivedFromGone : Provider.GoneType { }

public static class Cases
{
    static void Generic<T>()
    {
        if (typeof(T).IsValueType) throw new Value();
        throw new Reference();
    }

    public static void OfDerived()
    {
        if (typeof(Derived).IsValueType) throw new Value();
        throw new Reference();
    }

    public static void WithDerived() => Generic<Derived>();

    public static void OfDerivedFromGone()
    {
        if (typeof(DerivedFromGone).IsValueType) throw new Value();
        throw new Reference();
    }

    public static void WithDerivedFromGone() => Generic<DerivedFromGone>();
}
"""

        let providerImage (withGone : bool) : byte[] =
            Roslyn.compileOptimizedAssembly "Provider" OutputKind.DynamicallyLinkedLibrary [ provider withGone ]

        let clientImage =
            Roslyn.compileOptimizedAssemblyWithReferences
                "Client"
                OutputKind.DynamicallyLinkedLibrary
                [
                    MetadataReference.CreateFromImage (ImmutableArray.CreateRange (providerImage true))
                ]
                [ client ]

        let fixture =
            Assembly.read loggerFactory (Some "Client.dll") (new MemoryStream (clientImage))

        // The provider is where the analysis looks for assemblies, but not loaded.
        let directory = Directory.CreateTempSubdirectory "escape-analysis-provider"

        try
            File.WriteAllBytes (Path.Combine (directory.FullName, "Provider.dll"), providerImage false)

            let analysis =
                analysisOf
                    (hostCoreLib ())
                    (Seq.append [ directory.FullName ] (FrameworkUnderTest.runtimeDirs ()))
                    (hostTarget ())
                    HardwareIntrinsicsProfile.ScalarOnly
                    Assumption.all
                    [ fixture ]
                    id

            let value = "=Client.Value"
            let reference = "=Client.Reference"

            let _, failures =
                unmet
                    analysis
                    fixture
                    [
                        { expect "Client.Cases" "OfDerived" with
                            Contains = [ reference ]
                            Excludes = [ value ]
                        }
                        { expect "Client.Cases" "WithDerived" with
                            Contains = [ reference ]
                            Excludes = [ value ]
                        }
                        // CoreCLR cannot load `DerivedFromGone`, so the method that names it fails
                        // before it runs, and an instance asking of it is never made.
                        { expect "Client.Cases" "OfDerivedFromGone" with
                            Contains = [ "=System.TypeLoadException" ]
                        }
                        { expect "Client.Cases" "WithDerivedFromGone" with
                            Contains = [ "=System.TypeLoadException" ]
                        }
                    ]

            if not failures.IsEmpty then
                failures |> String.concat Environment.NewLine |> failwith
        finally
            directory.Delete true

    /// Code that compares two `typeof`s, which C# compiles to `ldtoken; call GetTypeFromHandle`
    /// twice, then `call op_Equality` or `op_Inequality`.
    let private typeEqualitySource =
        """
using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;

namespace TypeEquality;

// Exceptions whose construction raises nothing, unlike CoreLib's, which look up their messages.
public class Same : Exception { }

public class Different : Exception { }

public static class Cases
{
    public static void Equal()
    {
        if (typeof(int) == typeof(int)) throw new Same();
        throw new Different();
    }

    public static void Unequal()
    {
        if (typeof(int) != typeof(long)) throw new Different();
        throw new Same();
    }

    public static void Generic<T>()
    {
        if (typeof(T) == typeof(int)) throw new Same();
        throw new Different();
    }

    public static void WithInt() => Generic<int>();

    public static void WithString() => Generic<string>();

    // The same type, spelled by the caller's instantiation on one side and by this body on the
    // other.
    public static void Constructed<T>()
    {
        if (typeof(T) == typeof(List<int>)) throw new Same();
        throw new Different();
    }

    public static void WithList() => Constructed<List<int>>();

    public static void WithListOfLong() => Constructed<List<long>>();

    // The comparison decides no branch.
    public static bool Compared() => typeof(int) == typeof(string);

    [MethodImpl(MethodImplOptions.NoOptimization)]
    public static void Unoptimized()
    {
        if (typeof(int) == typeof(int)) throw new Same();
        throw new Different();
    }

    // The comparison's first operand arrives two ways, so it is not the one `GetTypeFromHandle`
    // it would have to be for the JIT to compare handles; it compares references instead.
    public static void EitherType(bool flag)
    {
        if ((flag ? typeof(int) : typeof(string)) == typeof(int)) throw new Same();
        throw new Different();
    }

    // Function pointer types, which the analysis does not tell apart.
    public static unsafe void FunctionPointers()
    {
        if (typeof(delegate*<int>) == typeof(delegate*<int>)) throw new Same();
        throw new Different();
    }

    // Function pointer types inside type arguments.
    public static unsafe void NestedFunctionPointers()
    {
        if (typeof(List<delegate* unmanaged[Cdecl]<void>[]>) == typeof(List<delegate* unmanaged[Stdcall]<void>[]>))
            throw new Same();
        throw new Different();
    }

    // A type argument with a custom modifier in it: the `modreq` that marks an `out` parameter.
    public static unsafe void ModifiedArgument()
    {
        if (typeof(List<delegate*<out int, void>[]>) == typeof(int)) throw new Same();
        throw new Different();
    }

    // Two typeofs passed to an intrinsic of `Type` that is not a comparison of them.
    public static void Assignable()
    {
        if (typeof(int).IsAssignableTo(typeof(object))) throw new Same();
        throw new Different();
    }
}
"""

    /// What each method of `typeEqualitySource` lets escape where the JIT compares the two types'
    /// handles in place of the calls.
    let private typeEqualityExpectations : Expectation list =
        let same = "=TypeEquality.Same"
        let different = "=TypeEquality.Different"

        let folded (name : string) (contains : string list) (excludes : string list) =
            { expect "TypeEquality.Cases" name with
                Contains = contains
                Excludes = excludes
                Unknown = Some false
                // A `throw` of null raises a `NullReferenceException`, which the runtime constructs.
                Assumes = Some (Assumption.JitOptimises :: constructing)
            }

        [
            folded "Equal" [ same ] [ different ]
            folded "Unequal" [ different ] [ same ]
            // The open definition does not decide the branch, but makes no call either way.
            folded "Generic" [ same ; different ] []
            folded "WithInt" [ same ] [ different ]
            folded "WithString" [ different ] [ same ]
            folded "Constructed" [ same ; different ] []
            folded "WithList" [ same ] [ different ]
            folded "WithListOfLong" [ different ] [ same ]
            folded "FunctionPointers" [ same ; different ] []
            folded "NestedFunctionPointers" [ same ; different ] []
            folded "ModifiedArgument" [ same ; different ] []
            { expect "TypeEquality.Cases" "Compared" with
                Excludes = [ same ; different ]
                Unknown = Some false
                Assumes = Some [ Assumption.JitOptimises ]
            }
        ]

    /// The image of `typeEqualitySource`, optimized as CoreLib is unless `debug`.
    let private typeEqualityFixture (debug : bool) : DumpedAssembly =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let image =
            if debug then
                Roslyn.compileAssembly "TypeEquality" OutputKind.DynamicallyLinkedLibrary [] [ typeEqualitySource ]
            else
                Roslyn.compileOptimizedAssembly
                    "TypeEquality"
                    OutputKind.DynamicallyLinkedLibrary
                    [ typeEqualitySource ]

        Assembly.read loggerFactory (Some "TypeEquality.dll") (new MemoryStream (image))

    /// Whether each of `methods` of the type `typeName` in `fixture` reaches exactly `expected` of
    /// `among`, and relies on the JIT's folding a query of types exactly where `folded`.
    let private reachesOf
        (among : Set<MethodKey>)
        (expected : Set<MethodKey>)
        (folded : bool)
        (typeName : string)
        (analysis : EscapeAnalysisState)
        (fixture : DumpedAssembly)
        (methods : string list)
        : unit
        =
        let mutable analysis = analysis

        for name in methods do
            let key = methodNamed fixture typeName name
            let next, reached = EscapeAnalysis.reachable analysis key
            let next, escapes = EscapeAnalysis.escapes next key
            analysis <- next
            let reached = Set.intersect reached among

            if
                reached <> expected
                || escapes.Assumes.Contains Assumption.JitOptimises <> folded
            then
                failwith $"%s{name}: reaches %A{Set.toList reached}, assuming %A{Set.toList escapes.Assumes}"

    /// The methods of `Type` that comparing two `typeof`s calls, where the JIT does not compare
    /// their handles in place of the calls: `op_Inequality` calls `op_Equality`.
    let private typeComparisonCalls () : Set<MethodKey> =
        let corelib = hostCoreLib ()

        [ "GetTypeFromHandle" ; "op_Equality" ]
        |> List.map (methodNamed corelib "System.Type")
        |> Set.ofList

    /// Whether each of `methods` of `typeEqualitySource` calls `Type.GetTypeFromHandle` and
    /// `Type.op_Equality`, or neither, as `calls` says; and relies on the JIT's comparing handles
    /// in place of the calls exactly where it calls neither.
    let private comparesTypesByCalling
        (calls : bool)
        (analysis : EscapeAnalysisState)
        (fixture : DumpedAssembly)
        (methods : string list)
        : unit
        =
        let called = typeComparisonCalls ()

        reachesOf called (if calls then called else Set.empty) (not calls) "TypeEquality.Cases" analysis fixture methods

    [<Test>]
    let ``comparing two typeofs is a comparison of their handles, decided by the instance, where the JIT optimises``
        ()
        : unit
        =
        let fixture = typeEqualityFixture false
        Debuggable.allowsJitOptimisation fixture |> shouldEqual (Some true)

        let analysis, failures =
            unmet (analysisOver [ fixture ] id) fixture typeEqualityExpectations

        if not failures.IsEmpty then
            failures |> String.concat Environment.NewLine |> failwith

        typeEqualityExpectations
        |> List.map (fun expectation -> snd expectation.Method)
        |> comparesTypesByCalling false analysis fixture

        let analysis, assignable =
            EscapeAnalysis.escapes analysis (methodNamed fixture "TypeEquality.Cases" "Assignable")

        assignable.Assumes.Contains Assumption.JitOptimises |> shouldEqual false

        // A method marked `NoOptimization` makes the calls. One in which the comparison's first
        // operand is not the `GetTypeFromHandle` straight before it makes that operand's call, and
        // compares references in place of the operator's.
        comparesTypesByCalling true analysis fixture [ "Unoptimized" ]

        let called = typeComparisonCalls ()

        reachesOf
            called
            (Set.singleton (methodNamed (hostCoreLib ()) "System.Type" "GetTypeFromHandle"))
            true
            "TypeEquality.Cases"
            analysis
            fixture
            [ "EitherType" ]

    [<Test>]
    let ``comparing two typeofs makes the calls where the assembly disables optimisation`` () : unit =
        let fixture = typeEqualityFixture true
        Debuggable.allowsJitOptimisation fixture |> shouldEqual (Some false)

        typeEqualityExpectations
        |> List.map (fun expectation -> snd expectation.Method)
        |> comparesTypesByCalling true (analysisOver [ fixture ] id) fixture

    [<Test>]
    let ``comparing two typeofs makes the calls unless the JIT is assumed to optimise`` () : unit =
        let fixture = typeEqualityFixture false

        let analysis =
            analysisOf
                (hostCoreLib ())
                (FrameworkUnderTest.runtimeDirs ())
                (hostTarget ())
                HardwareIntrinsicsProfile.ScalarOnly
                (Set.remove Assumption.JitOptimises Assumption.all)
                [ fixture ]
                id

        typeEqualityExpectations
        |> List.map (fun expectation -> snd expectation.Method)
        |> comparesTypesByCalling true analysis fixture

    /// Code that loads the `RuntimeType` of a type, which C# compiles to `ldtoken`, in generic code
    /// that CoreCLR shares between instantiations and in code it does not.
    let private runtimeTypeLookupSource =
        """
using System;
using System.Collections.Generic;

namespace RuntimeTypeLookup;

public static class Cases
{
    public static Type Of<T>() => typeof(T);

    public static Type OfString() => Of<string>();

    public static Type OfInt() => Of<int>();

    // A value type whose argument is a reference type, which CoreCLR shares too.
    public static Type OfSharedStruct() => Of<KeyValuePair<int, string>>();

    public static Type OfList<T>() => typeof(List<T>);

    public static Type OfListOfString() => OfList<string>();

    public static Type OfListOfInt() => OfList<int>();

    // Shared by its second argument, though the type it loads names only its first.
    public static Type First<T, U>() => typeof(T);

    public static Type FirstOfPartlyShared() => First<int, string>();

    public static Type Exact() => typeof(string);

    // Queries of types the JIT folds, comparing handles where it would make the `RuntimeType`.
    public static bool IsValue<T>() => typeof(T).IsValueType;

    public static bool IsValueOfString() => IsValue<string>();

    public static bool Same<T>() => typeof(T) == typeof(string);

    public static bool SameOfString() => Same<string>();

    public static bool SameReversed<T>() => typeof(string) == typeof(T);

    public static bool SameReversedOfString() => SameReversed<string>();

    public static Type OfBoxString() => Box<string>.Of();

    public static Type OfBoxInt() => Box<int>.Of();

    // Code that runs only on a CPU with 128-bit vectors.
    public static Type Accelerated<T>() =>
        System.Runtime.Intrinsics.Vector128.IsHardwareAccelerated ? typeof(T) : null;

    public static Type AcceleratedOfString() => Accelerated<string>();
}

// A type variable of the declaring type, rather than of the method.
public static class Box<T>
{
    public static Type Of() => typeof(T);
}
"""

    /// That none of `methods` of `runtimeTypeLookupSource` reaches
    /// `RuntimeTypeHandle.GetRuntimeTypeFromHandle`, and that each one's answer is known: where the
    /// JIT calls that method to make a type's `RuntimeType`, it passes a fully loaded type, so the
    /// analysis takes the call to raise only what allocating the `RuntimeType` raises, in place of
    /// the method's IL.
    let private runsNoLookupIl
        (analysis : EscapeAnalysisState)
        (fixture : DumpedAssembly)
        (methods : string list)
        : unit
        =
        let lookup =
            methodNamed (hostCoreLib ()) "System.RuntimeTypeHandle" "GetRuntimeTypeFromHandle"

        let mutable analysis = analysis

        for name in methods do
            let key = methodNamed fixture "RuntimeTypeLookup.Cases" name
            let next, reached = EscapeAnalysis.reachable analysis key
            let next, escapes = EscapeAnalysis.escapes next key
            analysis <- next

            if reached.Contains lookup || escapes.Unknown then
                failwith
                    $"%s{name}: reaches GetRuntimeTypeFromHandle %b{reached.Contains lookup}, unknown %b{escapes.Unknown}"

    [<Test>]
    let ``loading a type that shared code looks up at run time makes its RuntimeType`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let fixture =
            Roslyn.compileOptimizedAssembly
                "RuntimeTypeLookup"
                OutputKind.DynamicallyLinkedLibrary
                [ runtimeTypeLookupSource ]
            |> fun image -> Assembly.read loggerFactory (Some "RuntimeTypeLookup.dll") (new MemoryStream (image))

        let analysis = analysisOver [ fixture ] id

        runsNoLookupIl
            analysis
            fixture
            [
                // The definitions stand for every instantiation, the shared ones among them.
                "Of"
                "OfString"
                "OfSharedStruct"
                "OfList"
                "OfListOfString"
                "OfBoxString"
                // Where the type is exact, the JIT embeds its `RuntimeType`, which it makes as it
                // compiles.
                "OfInt"
                "OfListOfInt"
                "FirstOfPartlyShared"
                "Exact"
                "OfBoxInt"
                "IsValueOfString"
                "SameOfString"
                "SameReversedOfString"
                // The analysis asks about a CPU with no vector instructions.
                "AcceleratedOfString"
            ]

    /// An `ldtoken` of a type variable, and nothing else, which the JIT compiles to a call making
    /// the type's `RuntimeTypeHandle` where the code is shared and to a constant where it is not.
    /// Calling `Type.GetTypeFromHandle`, as C#'s `typeof` does, raises more than the call the JIT
    /// makes, so these leave it out.
    [<Test>]
    let ``a type that shared code looks up raises only what allocating its RuntimeType raises`` () : unit =
        let image =
            let builder =
                Emit.PersistedAssemblyBuilder (AssemblyName "Handles", typeof<obj>.Assembly)

            let modul = builder.DefineDynamicModule "Handles"

            let emitted =
                modul.DefineType (
                    "Emitted",
                    TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed
                )

            let handleOf =
                emitted.DefineMethod (
                    "HandleOf",
                    MethodAttributes.Public ||| MethodAttributes.Static,
                    typeof<Void>,
                    [||]
                )

            let parameter = handleOf.DefineGenericParameters([| "T" |]).[0]
            let il = handleOf.GetILGenerator ()
            il.Emit (Emit.OpCodes.Ldtoken, parameter)
            il.Emit Emit.OpCodes.Pop
            il.Emit Emit.OpCodes.Ret

            // The same, where its argument says to.
            let handleIf =
                emitted.DefineMethod (
                    "HandleIf",
                    MethodAttributes.Public ||| MethodAttributes.Static,
                    typeof<Void>,
                    [| typeof<bool> |]
                )

            let parameter = handleIf.DefineGenericParameters([| "T" |]).[0]
            let il = handleIf.GetILGenerator ()
            let skip = il.DefineLabel ()
            il.Emit Emit.OpCodes.Ldarg_0
            il.Emit (Emit.OpCodes.Brfalse, skip)
            il.Emit (Emit.OpCodes.Ldtoken, parameter)
            il.Emit Emit.OpCodes.Pop
            il.MarkLabel skip
            il.Emit Emit.OpCodes.Ret

            for name, argument in [ "OfString", typeof<string> ; "OfInt", typeof<int> ] do
                let il =
                    emitted
                        .DefineMethod(name, MethodAttributes.Public ||| MethodAttributes.Static, typeof<Void>, [||])
                        .GetILGenerator ()

                il.Emit (Emit.OpCodes.Call, handleOf.MakeGenericMethod argument)
                il.Emit Emit.OpCodes.Ret

            let il =
                emitted
                    .DefineMethod(
                        "NeverOfString",
                        MethodAttributes.Public ||| MethodAttributes.Static,
                        typeof<Void>,
                        [||]
                    )
                    .GetILGenerator ()

            il.Emit Emit.OpCodes.Ldc_I4_0
            il.Emit (Emit.OpCodes.Call, handleIf.MakeGenericMethod typeof<string>)
            il.Emit Emit.OpCodes.Ret

            emitted.CreateType () |> ignore<Type>

            use stream = new MemoryStream ()
            builder.Save stream
            stream.ToArray ()

        let _, loggerFactory = LoggerFactory.makeTest ()

        let fixture =
            Assembly.read loggerFactory (Some "Handles.dll") (new MemoryStream (image))

        let mutable analysis = analysisOver [ fixture ] id

        let answer (name : string) : Set<string> * bool =
            let next, escapes =
                EscapeAnalysis.escapes analysis (methodNamed fixture "Emitted" name)

            analysis <- next
            render analysis escapes, escapes.Unknown

        let calling = "=System.StackOverflowException"
        let allocating = "=System.OutOfMemoryException"

        // The definition stands for every instantiation, the shared ones among them.
        answer "HandleOf" |> shouldEqual (Set.ofList [ allocating ; calling ], false)
        answer "OfString" |> shouldEqual (Set.ofList [ allocating ; calling ], false)
        // The JIT embeds an exact type's handle, which it makes as it compiles.
        answer "OfInt" |> shouldEqual (Set.singleton calling, false)
        // A run that skips the `ldtoken` makes nothing.
        answer "HandleIf" |> shouldEqual (Set.ofList [ allocating ; calling ], false)
        answer "NeverOfString" |> shouldEqual (Set.singleton calling, false)

    [<Test>]
    let ``CoreLib's own calls of GetRuntimeTypeFromHandle may finish loading the type`` () : unit =
        // Its callers in CoreLib pass handles that no contract says are of fully loaded types, so
        // its slow path, which loads the type fully first, is opaque to them.
        let corelib = hostCoreLib ()
        let analysis = analysisOver [] id

        let method =
            methodNamed corelib "System.RuntimeTypeHandle" "GetRuntimeTypeFromHandle"

        let analysis, escapes = EscapeAnalysis.escapes analysis method
        escapes.Unknown |> shouldEqual true
        let analysis, sources = EscapeAnalysis.unknownSources analysis method

        sources
        |> Seq.map (renderSite analysis)
        |> List.ofSeq
        |> shouldEqual [ "System.RuntimeTypeHandle::GetRuntimeTypeFromHandleSlow entry NativeBody" ]

    /// Code that loads a handle of a field, which C# compiles to `ldtoken`: the data of a
    /// `ReadOnlySpan<T>`, by `RuntimeHelpers.CreateSpan`, and that of an array, by
    /// `RuntimeHelpers.InitializeArray`.
    let private memberStubSource =
        """
using System;
using System.Runtime.CompilerServices;

namespace MemberStubs;

public enum Small : short { A, B, C }

public static class Cases
{
    public static ReadOnlySpan<int> Span() => new int[] { 1, 2, 3 };

    public static ReadOnlySpan<Small> SpanOfEnum() => new Small[] { Small.A, Small.B, Small.C };

    [MethodImpl(MethodImplOptions.NoOptimization)]
    public static ReadOnlySpan<int> SpanUnoptimised() => new int[] { 1, 2, 3 };

    public static int[] Array() => new int[] { 1, 2, 3 };
}
"""

    /// Methods of `Probe.Cases` that load a handle of a member, in IL that C# does not write. Each
    /// `Span...` calls `RuntimeHelpers.CreateSpan` with the handle:
    ///   - `OfMethod`, of a method, and `OfField`, of a field, which they drop;
    ///   - `SpanFolded`, `CreateSpan<int>` of a field of twelve bytes of data;
    ///   - `SpanOfStruct`, `CreateSpan` of that field for a struct of an `int`, and
    ///     `SpanOfGeneric`, for a `KeyValuePair<int, int>`, neither of which is a primitive type;
    ///   - `SpanOfPlainField`, `CreateSpan<int>` of a static field that has no data;
    ///   - `SpanTooShort`, `CreateSpan<int>` of a field of two bytes of data;
    ///   - `SpanOfAutoLayout`, `CreateSpan<int>` of a field whose type is an auto-layout struct
    ///     that metadata gives a size of sixteen, which CoreCLR ignores, making it one byte long;
    ///   - `SpanJoined(bool)`, whose two `ldtoken`s of the twelve bytes branch to one call;
    ///   - `SpanAfterReturnAddress`, of the twelve bytes, after calling
    ///     `StubHelpers.NextCallReturnAddress`.
    let private emitMemberTokens () : byte[] =
        let builder =
            Emit.PersistedAssemblyBuilder (AssemblyName "MemberTokens", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "MemberTokens"

        let pair =
            modul.DefineType (
                "Probe.Pair",
                TypeAttributes.Public
                ||| TypeAttributes.Sealed
                ||| TypeAttributes.SequentialLayout,
                typeof<ValueType>
            )

        pair.DefineField ("First", typeof<int>, FieldAttributes.Public)
        |> ignore<Emit.FieldBuilder>

        let pair = pair.CreateType ()

        let cases =
            modul.DefineType (
                "Probe.Cases",
                TypeAttributes.Public
                ||| TypeAttributes.Class
                ||| TypeAttributes.Abstract
                ||| TypeAttributes.Sealed
            )

        let twelve =
            cases.DefineInitializedData (
                "Twelve",
                Array.init 12 byte,
                FieldAttributes.Static ||| FieldAttributes.Assembly
            )

        let two =
            cases.DefineInitializedData ("Two", [| 1uy ; 2uy |], FieldAttributes.Static ||| FieldAttributes.Assembly)

        let sixteen =
            cases.DefineInitializedData (
                "Sixteen",
                Array.init 16 byte,
                FieldAttributes.Static ||| FieldAttributes.Assembly
            )

        let plain =
            cases.DefineField ("Plain", typeof<int>, FieldAttributes.Static ||| FieldAttributes.Public)

        let nextCallReturnAddress =
            typeof<obj>.Assembly
                .GetType("System.StubHelpers.StubHelpers", true)
                .GetMethod ("NextCallReturnAddress", BindingFlags.NonPublic ||| BindingFlags.Static)

        let createSpan (element : Type) : MethodInfo =
            typeof<Runtime.CompilerServices.RuntimeHelpers>.GetMethod("CreateSpan").MakeGenericMethod [| element |]

        let target =
            cases.DefineMethod ("Target", MethodAttributes.Public ||| MethodAttributes.Static, typeof<Void>, [||])

        target.GetILGenerator().Emit Emit.OpCodes.Ret

        let define (name : string) (parameters : Type[]) (emit : Emit.ILGenerator -> unit) =
            let method =
                cases.DefineMethod (name, MethodAttributes.Public ||| MethodAttributes.Static, typeof<Void>, parameters)

            let il = method.GetILGenerator ()
            emit il
            il.Emit Emit.OpCodes.Pop
            il.Emit Emit.OpCodes.Ret

        define "OfMethod" [||] (fun il -> il.Emit (Emit.OpCodes.Ldtoken, target))
        define "OfField" [||] (fun il -> il.Emit (Emit.OpCodes.Ldtoken, plain))

        for name, field, element in
            [
                "SpanFolded", twelve, typeof<int>
                "SpanOfStruct", twelve, pair
                "SpanOfGeneric", twelve, typeof<Collections.Generic.KeyValuePair<int, int>>
                "SpanOfPlainField", plain, typeof<int>
                "SpanTooShort", two, typeof<int>
                "SpanOfAutoLayout", sixteen, typeof<int>
            ] do
            define
                name
                [||]
                (fun il ->
                    il.Emit (Emit.OpCodes.Ldtoken, field)
                    il.Emit (Emit.OpCodes.Call, createSpan element)
                )

        define
            "SpanJoined"
            [| typeof<bool> |]
            (fun il ->
                let other = il.DefineLabel ()
                let joined = il.DefineLabel ()
                il.Emit Emit.OpCodes.Ldarg_0
                il.Emit (Emit.OpCodes.Brtrue, other)
                il.Emit (Emit.OpCodes.Ldtoken, twelve)
                il.Emit (Emit.OpCodes.Br, joined)
                il.MarkLabel other
                il.Emit (Emit.OpCodes.Ldtoken, twelve)
                il.MarkLabel joined
                il.Emit (Emit.OpCodes.Call, createSpan typeof<int>)
            )

        define
            "SpanAfterReturnAddress"
            [||]
            (fun il ->
                il.Emit (Emit.OpCodes.Call, nextCallReturnAddress)
                il.Emit Emit.OpCodes.Pop
                il.Emit (Emit.OpCodes.Ldtoken, twelve)
                il.Emit (Emit.OpCodes.Call, createSpan typeof<int>)
            )

        cases.CreateType () |> ignore<Type>
        use stream = new MemoryStream ()
        builder.Save stream
        let image = stream.ToArray ()

        // No `TypeBuilder` makes an auto-layout type with a size, which metadata allows, so the
        // image's TypeDef row for the type of `Sixteen`'s data loses its layout flags.
        use reader = new PEReader (new MemoryStream (image))
        let metadata = reader.GetMetadataReader ()

        let row =
            metadata.TypeDefinitions
            |> Seq.filter (fun handle -> metadata.GetTypeDefinition(handle).GetLayout().Size = 16)
            |> Seq.exactlyOne

        let attributes =
            metadata.GetTypeDefinition(row).Attributes &&& ~~~TypeAttributes.LayoutMask

        let offset =
            reader.PEHeaders.MetadataStartOffset
            + metadata.GetTableMetadataOffset TableIndex.TypeDef
            + (MetadataTokens.GetRowNumber (TypeDefinitionHandle.op_Implicit row : EntityHandle)
               - 1)
              * metadata.GetTableRowSize TableIndex.TypeDef

        BitConverter.GetBytes(int attributes).CopyTo (image, offset)
        image

    /// Which of `RuntimeMethodInfoStub.FromPtr`, `RuntimeFieldInfoStub.FromPtr` and
    /// `RuntimeHelpers.CreateSpan` each method of `typeName` in `fixture` reaches, by name.
    let private reachesStubs
        (analysis : EscapeAnalysisState)
        (fixture : DumpedAssembly)
        (typeName : string)
        (methods : string list)
        : EscapeAnalysisState * Map<string, string list>
        =
        let corelib = hostCoreLib ()

        let watched =
            [
                "method stub", methodNamed corelib "System.RuntimeMethodInfoStub" "FromPtr"
                "field stub", methodNamed corelib "System.RuntimeFieldInfoStub" "FromPtr"
                "CreateSpan", methodNamed corelib "System.Runtime.CompilerServices.RuntimeHelpers" "CreateSpan"
            ]

        ((analysis, Map.empty), methods)
        ||> List.fold (fun (analysis, reaches) name ->
            let analysis, reached =
                EscapeAnalysis.reachable analysis (methodNamed fixture typeName name)

            let names =
                watched |> List.filter (fun (_, key) -> reached.Contains key) |> List.map fst

            analysis, Map.add name names reaches
        )

    [<Test>]
    let ``loading a handle of a method or a field makes its stub, unless the JIT expands CreateSpan over it``
        ()
        : unit
        =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let fixture =
            Roslyn.compileOptimizedAssembly "MemberStubs" OutputKind.DynamicallyLinkedLibrary [ memberStubSource ]
            |> fun image -> Assembly.read loggerFactory (Some "MemberStubs.dll") (new MemoryStream (image))

        let analysis = analysisOver [ fixture ] id
        let names = [ "Span" ; "SpanOfEnum" ; "SpanUnoptimised" ; "Array" ]
        let analysis, reaches = reachesStubs analysis fixture "MemberStubs.Cases" names

        reaches
        |> shouldEqual (
            Map.ofList
                [
                    // The JIT expands `CreateSpan` to a span over the data, calling neither.
                    "Span", []
                    "SpanOfEnum", []
                    // Where it does not optimise, it calls both.
                    "SpanUnoptimised", [ "field stub" ; "CreateSpan" ]
                    // It expands `InitializeArray` only where it optimises fully, which a method
                    // compiled first is not.
                    "Array", [ "field stub" ]
                ]
        )

        // The expansion raises nothing, which the JIT is assumed to make.
        let _, span =
            EscapeAnalysis.escapes analysis (methodNamed fixture "MemberStubs.Cases" "Span")

        span.Unknown |> shouldEqual false
        span.Types |> shouldEqual Set.empty

        span.Assumes
        |> shouldEqual (Set.ofList [ Assumption.JitOptimises ; Assumption.NoThreadAborts ])

        // Not assumed to, it calls both.
        let unassuming =
            analysisOf
                (hostCoreLib ())
                (FrameworkUnderTest.runtimeDirs ())
                (hostTarget ())
                HardwareIntrinsicsProfile.ScalarOnly
                (Set.remove Assumption.JitOptimises Assumption.all)
                [ fixture ]
                id

        let _, reaches =
            reachesStubs unassuming fixture "MemberStubs.Cases" [ "Span" ; "SpanOfEnum" ]

        reaches
        |> shouldEqual (
            Map.ofList
                [
                    "Span", [ "field stub" ; "CreateSpan" ]
                    "SpanOfEnum", [ "field stub" ; "CreateSpan" ]
                ]
        )

    [<Test>]
    let ``loading a handle of a member makes its stub where the JIT cannot expand CreateSpan over it`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let fixture =
            Assembly.read loggerFactory (Some "MemberTokens.dll") (new MemoryStream (emitMemberTokens ()))

        let analysis = analysisOver [ fixture ] id

        let names =
            [
                "OfMethod"
                "OfField"
                "SpanFolded"
                "SpanOfStruct"
                "SpanOfGeneric"
                "SpanOfPlainField"
                "SpanTooShort"
                "SpanOfAutoLayout"
                "SpanJoined"
                "SpanAfterReturnAddress"
            ]

        let _, reaches = reachesStubs analysis fixture "Probe.Cases" names
        let both = [ "field stub" ; "CreateSpan" ]

        reaches
        |> shouldEqual (
            Map.ofList
                [
                    "OfMethod", [ "method stub" ]
                    "OfField", [ "field stub" ]
                    "SpanFolded", []
                    "SpanOfStruct", both
                    "SpanOfGeneric", both
                    "SpanOfPlainField", both
                    "SpanTooShort", both
                    "SpanOfAutoLayout", both
                    "SpanJoined", both
                    "SpanAfterReturnAddress", both
                ]
        )

    /// Code that compares some type with a `typeof`, which C# compiles to whatever pushes the type,
    /// `ldtoken; call GetTypeFromHandle`, then `call op_Equality` or `op_Inequality`.
    let private typeOperandSource =
        """
using System;
using System.Runtime.CompilerServices;

namespace TypeOperand;

// Exceptions whose construction raises nothing, unlike CoreLib's, which look up their messages.
public class Same : Exception { }

public class Different : Exception { }

public static class Cases
{
    public static void OfType(Type t)
    {
        if (t == typeof(string)) throw new Same();
        throw new Different();
    }

    public static void NotOfType(Type t)
    {
        if (t != typeof(string)) throw new Different();
        throw new Same();
    }

    public static void Generic<T>(Type t)
    {
        if (t == typeof(T[])) throw new Same();
        throw new Different();
    }

    public static void WithInt(Type t) => Generic<int>(t);

    public static void WithString(Type t) => Generic<string>(t);

    // The type arrives two ways, which the JIT reads before it compares.
    public static void EitherOperand(bool flag, Type a, Type b)
    {
        if ((flag ? a : b) == typeof(string)) throw new Same();
        throw new Different();
    }

    // The comparison decides no branch.
    public static bool Compared(Type t) => t == typeof(string);

    // An object's type, which tier 0 gets by calling `GetType`.
    public static void OfObject(object o)
    {
        if (o.GetType() == typeof(string)) throw new Same();
        throw new Different();
    }

    [MethodImpl(MethodImplOptions.NoOptimization)]
    public static void Unoptimized(Type t)
    {
        if (t == typeof(string)) throw new Same();
        throw new Different();
    }

    // The `typeof` first, which the JIT folds too, but the analysis does not look for.
    public static void Reversed(Type t)
    {
        if (typeof(string) == t) throw new Same();
        throw new Different();
    }

    // The `typeof` arrives two ways, so it is not the one `GetTypeFromHandle` the JIT would have to
    // see.
    public static void EitherTypeof(bool flag, Type t)
    {
        if (t == (flag ? typeof(int) : typeof(string))) throw new Same();
        throw new Different();
    }
}
"""

    /// What each method of `typeOperandSource` lets escape where the JIT compares references in
    /// place of the calls.
    let private typeOperandExpectations : Expectation list =
        let same = "=TypeOperand.Same"
        let different = "=TypeOperand.Different"

        let folded (name : string) =
            { expect "TypeOperand.Cases" name with
                Contains = [ same ; different ]
                Unknown = Some false
                // A `throw` of null raises a `NullReferenceException`, which the runtime constructs.
                Assumes = Some (Assumption.JitOptimises :: constructing)
            }

        [
            folded "OfType"
            folded "NotOfType"
            folded "WithInt"
            folded "EitherOperand"
            // Shared code looks `T[]` up, and makes its `RuntimeType`, which only allocates.
            folded "Generic"
            folded "WithString"
            { expect "TypeOperand.Cases" "Compared" with
                Excludes = [ same ; different ]
                Unknown = Some false
                Assumes = Some [ Assumption.JitOptimises ]
            }
        ]

    /// The image of `typeOperandSource`, optimized as CoreLib is unless `debug`.
    let private typeOperandFixture (debug : bool) : DumpedAssembly =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let image =
            if debug then
                Roslyn.compileAssembly "TypeOperand" OutputKind.DynamicallyLinkedLibrary [] [ typeOperandSource ]
            else
                Roslyn.compileOptimizedAssembly "TypeOperand" OutputKind.DynamicallyLinkedLibrary [ typeOperandSource ]

        Assembly.read loggerFactory (Some "TypeOperand.dll") (new MemoryStream (image))

    /// The names of the methods of `typeOperandSource` that the JIT folds where it optimises.
    let private typeOperandFolded : string list =
        "OfObject"
        :: (typeOperandExpectations |> List.map (fun expectation -> snd expectation.Method))

    [<Test>]
    let ``comparing a type with a typeof is a comparison of references, where the JIT optimises`` () : unit =
        let fixture = typeOperandFixture false
        Debuggable.allowsJitOptimisation fixture |> shouldEqual (Some true)

        let analysis, failures =
            unmet (analysisOver [ fixture ] id) fixture typeOperandExpectations

        if not failures.IsEmpty then
            failures |> String.concat Environment.NewLine |> failwith

        let called = typeComparisonCalls ()

        reachesOf called Set.empty true "TypeOperand.Cases" analysis fixture typeOperandFolded

        // The JIT still makes the `typeof`'s `RuntimeType` to compare with, which it looks up at
        // run time where the code is shared, but it runs no IL to make it.
        let lookup =
            Set.singleton (methodNamed (hostCoreLib ()) "System.RuntimeTypeHandle" "GetRuntimeTypeFromHandle")

        reachesOf
            lookup
            Set.empty
            true
            "TypeOperand.Cases"
            analysis
            fixture
            [ "Generic" ; "WithString" ; "OfType" ; "WithInt" ]

        // Tier 0 does not expand `Object.GetType`, so an object's type is got by calling it.
        let getType = methodNamed (hostCoreLib ()) "System.Object" "GetType"

        reachesOf
            (Set.singleton getType)
            (Set.singleton getType)
            true
            "TypeOperand.Cases"
            analysis
            fixture
            [ "OfObject" ]

        // A method marked `NoOptimization` makes the calls, as do the shapes that are not a
        // `typeof` straight before the operator.
        reachesOf
            called
            called
            false
            "TypeOperand.Cases"
            analysis
            fixture
            [ "Unoptimized" ; "Reversed" ; "EitherTypeof" ]

    [<Test>]
    let ``comparing a type with a typeof makes the calls where the assembly disables optimisation`` () : unit =
        let fixture = typeOperandFixture true
        Debuggable.allowsJitOptimisation fixture |> shouldEqual (Some false)
        let called = typeComparisonCalls ()

        reachesOf called called false "TypeOperand.Cases" (analysisOver [ fixture ] id) fixture typeOperandFolded

    [<Test>]
    let ``comparing a type with a typeof makes the calls unless the JIT is assumed to optimise`` () : unit =
        let fixture = typeOperandFixture false

        let analysis =
            analysisOf
                (hostCoreLib ())
                (FrameworkUnderTest.runtimeDirs ())
                (hostTarget ())
                HardwareIntrinsicsProfile.ScalarOnly
                (Set.remove Assumption.JitOptimises Assumption.all)
                [ fixture ]
                id

        let called = typeComparisonCalls ()
        reachesOf called called false "TypeOperand.Cases" analysis fixture typeOperandFolded

    /// Code that branches on `RuntimeHelpers.IsReferenceOrContainsReferences<T>()`, which C#
    /// compiles to a call of it, then `brtrue` or `brfalse`.
    let private containsReferencesSource =
        """
using System.Runtime.CompilerServices;

namespace ContainsReferences;

// Exceptions whose construction raises nothing, unlike CoreLib's, which look up their messages.
public class Holds : System.Exception { }

public class HoldsNone : System.Exception { }

public struct WithReference { public string S; }

public struct Plain { public int X; public long Y; }

public static class Cases
{
    public static void Generic<T>()
    {
        if (RuntimeHelpers.IsReferenceOrContainsReferences<T>()) throw new Holds();
        throw new HoldsNone();
    }

    public static void WithInt() => Generic<int>();

    public static void WithString() => Generic<string>();

    public static void WithReference() => Generic<WithReference>();

    public static void WithPlain() => Generic<Plain>();

    // A generic struct, whose fields' types are its arguments.
    public static void WithPairOfString() => Generic<System.Collections.Generic.KeyValuePair<int, string>>();

    public static void WithPairOfInt() => Generic<System.Collections.Generic.KeyValuePair<int, int>>();

    public static void Negated<T>()
    {
        if (!RuntimeHelpers.IsReferenceOrContainsReferences<T>()) throw new HoldsNone();
        throw new Holds();
    }

    public static void NegatedWithInt() => Negated<int>();

    public static void Exact()
    {
        if (RuntimeHelpers.IsReferenceOrContainsReferences<Plain>()) throw new Holds();
        throw new HoldsNone();
    }

    // The branch is on the answer and something else.
    public static void Combined<T>(bool flag)
    {
        if (flag && RuntimeHelpers.IsReferenceOrContainsReferences<T>()) throw new Holds();
        throw new HoldsNone();
    }

    public static void CombinedWithInt(bool flag) => Combined<int>(flag);

    // The answer is the same however the method is compiled.
    [MethodImpl(MethodImplOptions.NoOptimization)]
    public static void Unoptimized<T>()
    {
        if (RuntimeHelpers.IsReferenceOrContainsReferences<T>()) throw new Holds();
        throw new HoldsNone();
    }

    public static void UnoptimizedWithString() => Unoptimized<string>();

    // No branch tests the answer straight away.
    public static bool Answer<T>() => RuntimeHelpers.IsReferenceOrContainsReferences<T>();
}
"""

    /// What each method of `containsReferencesSource` lets escape, which is so whether or not the
    /// JIT is assumed to optimise: the answer is a constant for each instance however the code is
    /// compiled.
    let private containsReferencesExpectations : Expectation list =
        let holds = "=ContainsReferences.Holds"
        let holdsNone = "=ContainsReferences.HoldsNone"

        let decided (name : string) (contains : string list) (excludes : string list) =
            { expect "ContainsReferences.Cases" name with
                Contains = contains
                Excludes = excludes
                Unknown = Some false
                // A `throw` of null raises a `NullReferenceException`, which the runtime constructs.
                Assumes = Some constructing
            }

        [
            // The open definition does not decide the branch.
            decided "Generic" [ holds ; holdsNone ] []
            decided "WithInt" [ holdsNone ] [ holds ]
            decided "WithString" [ holds ] [ holdsNone ]
            decided "WithReference" [ holds ] [ holdsNone ]
            decided "WithPlain" [ holdsNone ] [ holds ]
            decided "WithPairOfString" [ holds ] [ holdsNone ]
            decided "WithPairOfInt" [ holdsNone ] [ holds ]
            decided "NegatedWithInt" [ holdsNone ] [ holds ]
            decided "Exact" [ holdsNone ] [ holds ]
            decided "CombinedWithInt" [ holdsNone ] [ holds ]
            decided "UnoptimizedWithString" [ holds ] [ holdsNone ]
            { expect "ContainsReferences.Cases" "Answer" with
                Excludes = [ holds ; holdsNone ]
                Unknown = Some false
                Assumes = Some []
            }
        ]

    /// The image of `containsReferencesSource`, optimized as CoreLib is.
    let private containsReferencesFixture () : DumpedAssembly =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let image =
            Roslyn.compileOptimizedAssembly
                "ContainsReferences"
                OutputKind.DynamicallyLinkedLibrary
                [ containsReferencesSource ]

        Assembly.read loggerFactory (Some "ContainsReferences.dll") (new MemoryStream (image))

    [<Test>]
    let ``a branch on whether a type holds references goes the way the instance's type decides`` () : unit =
        let fixture = containsReferencesFixture ()

        let _, failures =
            unmet (analysisOver [ fixture ] id) fixture containsReferencesExpectations

        if not failures.IsEmpty then
            failures |> String.concat Environment.NewLine |> failwith

    [<Test>]
    let ``a branch on whether a type holds references is decided without assuming the JIT optimises`` () : unit =
        let fixture = containsReferencesFixture ()

        let analysis =
            analysisOf
                (hostCoreLib ())
                (FrameworkUnderTest.runtimeDirs ())
                (hostTarget ())
                HardwareIntrinsicsProfile.ScalarOnly
                (Set.remove Assumption.JitOptimises Assumption.all)
                [ fixture ]
                id

        let _, failures = unmet analysis fixture containsReferencesExpectations

        if not failures.IsEmpty then
            failures |> String.concat Environment.NewLine |> failwith

    /// A client whose `Cases` branches on whether a struct holds references, where answering loads
    /// what nothing has loaded yet, and where the structs nest deeply.
    [<Test>]
    let ``whether a struct holds references loads its fields' base types, once each`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let depth = 24

        // `Level{n}` lays two `Level{n-1}`s over each other, so it is four bytes long however deep,
        // and holds no references.
        let level (n : int) : string =
            [
                "[StructLayout(LayoutKind.Explicit)]"
                $"public struct Level%d{n}"
                "{"
                $"    [FieldOffset(0)] public Level%d{n - 1} A;"
                $"    [FieldOffset(0)] public Level%d{n - 1} B;"
                "}"
            ]
            |> String.concat "\n"

        let levels = [ 1..depth ] |> List.map level |> String.concat "\n"

        let provider =
            """
namespace Provider;

public class Base { }
"""

        let client =
            """
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace Client;

public class Holds : System.Exception { }

public class HoldsNone : System.Exception { }

public class Derived : Provider.Base { }

public struct HoldsDerived { public Derived D; }

public struct Level0 { public int X; }
"""
            + levels
            + $"""
public static class Cases
{{
    static void Generic<T>()
    {{
        if (RuntimeHelpers.IsReferenceOrContainsReferences<T>()) throw new Holds();
        throw new HoldsNone();
    }}

    public static void WithHoldsDerived() => Generic<HoldsDerived>();

    public static void WithDeep() => Generic<Level%d{depth}>();
}}
"""

        let providerImage =
            Roslyn.compileOptimizedAssembly "Provider" OutputKind.DynamicallyLinkedLibrary [ provider ]

        let clientImage =
            Roslyn.compileOptimizedAssemblyWithReferences
                "Client"
                OutputKind.DynamicallyLinkedLibrary
                [ MetadataReference.CreateFromImage (ImmutableArray.CreateRange providerImage) ]
                [ client ]

        let fixture =
            Assembly.read loggerFactory (Some "Client.dll") (new MemoryStream (clientImage))

        // The provider is where the analysis looks for assemblies, but not loaded.
        let directory = Directory.CreateTempSubdirectory "escape-analysis-provider"

        try
            File.WriteAllBytes (Path.Combine (directory.FullName, "Provider.dll"), providerImage)

            let analysis =
                analysisOf
                    (hostCoreLib ())
                    (Seq.append [ directory.FullName ] (FrameworkUnderTest.runtimeDirs ()))
                    (hostTarget ())
                    HardwareIntrinsicsProfile.ScalarOnly
                    Assumption.all
                    [ fixture ]
                    id

            let holds = "=Client.Holds"
            let holdsNone = "=Client.HoldsNone"
            let stopwatch = Diagnostics.Stopwatch.StartNew ()

            let _, failures =
                unmet
                    analysis
                    fixture
                    [
                        { expect "Client.Cases" "WithHoldsDerived" with
                            Contains = [ holds ]
                            Excludes = [ holdsNone ]
                        }
                        { expect "Client.Cases" "WithDeep" with
                            Contains = [ holdsNone ]
                            Excludes = [ holds ]
                        }
                    ]

            if not failures.IsEmpty then
                failures |> String.concat Environment.NewLine |> failwith

            // Each struct is walked once, not once for each way of reaching it, of which the deepest
            // has two to the twenty-fourth.
            stopwatch.Elapsed |> shouldBeSmallerThan (TimeSpan.FromSeconds 20.0)
        finally
            directory.Delete true

    /// An assembly whose `Probe.Cases` branches on a call of a generic intrinsic of `RuntimeHelpers`
    /// straight away, throwing `NotSupportedException` where the answer is false and
    /// `InvalidOperationException` where it is true: `OnReferences`, of
    /// `IsReferenceOrContainsReferences<int>()`, and `OnBitwiseEquatable`, of CoreLib's internal
    /// `IsBitwiseEquatable<int>()`, which C# cannot call.
    let private emitRuntimeHelpersBranches () : byte[] =
        let builder =
            Emit.PersistedAssemblyBuilder (AssemblyName "RuntimeHelpersBranches", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "RuntimeHelpersBranches"

        let cases =
            modul.DefineType (
                "Probe.Cases",
                TypeAttributes.Public
                ||| TypeAttributes.Class
                ||| TypeAttributes.Abstract
                ||| TypeAttributes.Sealed
            )

        let runtimeHelpers = typeof<Runtime.CompilerServices.RuntimeHelpers>

        for name, query in
            [
                "OnReferences", runtimeHelpers.GetMethod "IsReferenceOrContainsReferences"
                "OnBitwiseEquatable",
                runtimeHelpers.GetMethod ("IsBitwiseEquatable", BindingFlags.NonPublic ||| BindingFlags.Static)
            ] do
            let method =
                cases.DefineMethod (name, MethodAttributes.Public ||| MethodAttributes.Static, typeof<Void>, [||])

            let il = method.GetILGenerator ()
            let holds = il.DefineLabel ()
            il.Emit (Emit.OpCodes.Call, query.MakeGenericMethod [| typeof<int> |])
            il.Emit (Emit.OpCodes.Brtrue, holds)
            il.Emit (Emit.OpCodes.Newobj, typeof<NotSupportedException>.GetConstructor Type.EmptyTypes)
            il.Emit Emit.OpCodes.Throw
            il.MarkLabel holds
            il.Emit (Emit.OpCodes.Newobj, typeof<InvalidOperationException>.GetConstructor Type.EmptyTypes)
            il.Emit Emit.OpCodes.Throw

        cases.CreateType () |> ignore<Type>
        use stream = new MemoryStream ()
        builder.Save stream
        stream.ToArray ()

    [<Test>]
    let ``only a call of IsReferenceOrContainsReferences is decided by whether its type holds references`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let fixture =
            Assembly.read
                loggerFactory
                (Some "RuntimeHelpersBranches.dll")
                (new MemoryStream (emitRuntimeHelpersBranches ()))

        let notSupported = "=System.NotSupportedException"
        let invalidOperation = "=System.InvalidOperationException"

        let _, failures =
            unmet
                (analysisOver [ fixture ] id)
                fixture
                [
                    // `int` holds no references.
                    { expect "Probe.Cases" "OnReferences" with
                        Contains = [ notSupported ]
                        Excludes = [ invalidOperation ]
                    }
                    // The analysis does not decide whether a type is bitwise equatable, so it follows
                    // both ways, though `int` holds no references.
                    { expect "Probe.Cases" "OnBitwiseEquatable" with
                        Contains = [ notSupported ; invalidOperation ]
                    }
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
            let dir = FrameworkUnderTest.requireLinuxFrameworkDirectory ()
            let _, loggerFactory = LoggerFactory.makeTest ()

            Assembly.readFile loggerFactory (Path.Combine (dir, "System.Private.CoreLib.dll")), [ dir ], JitTarget.X64
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

        let mutable analysis =
            analysisOf corelib runtimeDirs target profile Assumption.all [] id

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

        // What the parameterless constructor of each exception these faults raise raises.
        let mutable constructorShown = Map.empty

        for ns, name in
            [
                "System", "NullReferenceException"
                "System", "DataMisalignedException"
                "System", "DivideByZeroException"
                "System", "OverflowException"
            ] do
            let next, escapes =
                EscapeAnalysis.escapes analysis (parameterlessConstructor corelib ns name)

            analysis <- next
            constructorShown <- constructorShown.Add ($"%s{ns}.%s{name}", render analysis escapes)

        let constructorShown = constructorShown

        // A fault raises its exception, and what the constructor the runtime runs to make it raises.
        let made (fullName : string) (making : ExceptionMaking option) : Set<string> =
            match making with
            | Some ExceptionMaking.ParameterlessConstructor -> Set.add ("=" + fullName) constructorShown.[fullName]
            | _ -> Set.singleton ("=" + fullName)

        let instructionFaultShown (fault : InstructionFault) : Set<string> =
            let fullName =
                match fault with
                | InstructionFault.NullAddress -> "System.NullReferenceException"
                | InstructionFault.ImmediateOutOfRange -> "System.ArgumentOutOfRangeException"
                | InstructionFault.ZeroDivisor -> "System.DivideByZeroException"
                | InstructionFault.QuotientOverflow -> "System.OverflowException"

            made fullName (ExceptionMaking.ofInstructionFault fault)

        let mutable wholePrimitives = 0

        let faultShown (fault : PrimitiveFault) : Set<string> =
            let fullName =
                match fault with
                | PrimitiveFault.NullReference -> "System.NullReferenceException"
                | PrimitiveFault.DataMisaligned -> "System.DataMisalignedException"

            made fullName (Some (ExceptionMaking.ofPrimitiveFault fault))

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

                    let next, net11Frontier =
                        unknownOnlyThroughNet11MemoryQCalls
                            (EmulatedRuntime.ofCoreLib corelib)
                            analysis
                            (MethodKey.make corelib handle)

                    analysis <- next

                    if escapes.Unknown && not net11Frontier then
                        failures.Add $"%s{describe ()}: the VM's IL for it is all there is to see"
                | None, IntrinsicBody.JitExpansion (JitExpansion.Primitive primitive) ->
                    primitives <- primitives + 1

                    for fault, _ in (IntrinsicPrimitive.contract primitive).Raises do
                        for wanted in faultShown fault do
                            if not (shown.Contains wanted) then
                                failures.Add
                                    $"%s{describe ()} lacks %s{wanted}, which %A{primitive} can raise by its fault %A{fault}"
                | None, IntrinsicBody.VmSubstitution ->
                    // The VM chooses the substitute by instantiation; where it is a primitive, that
                    // is the whole method.
                    match IntrinsicPrimitive.recognise corelib handle with
                    | Some primitive ->
                        wholePrimitives <- wholePrimitives + 1

                        let wanted =
                            (IntrinsicPrimitive.contract primitive).Raises
                            |> List.map (fst >> faultShown)
                            |> Set.unionMany

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
                        match
                            HardwareInstruction.contract
                                (EmulatedRuntime.ofCoreLib corelib)
                                target
                                intrinsicClass
                                method.Name
                        with
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
                                |> Seq.map instructionFaultShown
                                |> Set.unionMany
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
    public static void PassGoneAsVararg() { Provider.Parent.Varargs(__arglist(default(Provider.GoneStruct))); }
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
    public struct HoldsGone { public Provider.GoneStruct G; }
    static int BranchOnReferences<T>() => System.Runtime.CompilerServices.RuntimeHelpers.IsReferenceOrContainsReferences<T>() ? 1 : 0;
    public static int BranchOnHoldsGone() => BranchOnReferences<HoldsGone>();
    public class DerivedFromGone : Provider.GoneType { }
    public struct HoldsDerivedFromGone { public DerivedFromGone D; }
    public static int BranchOnHoldsDerivedFromGone() => BranchOnReferences<HoldsDerivedFromGone>();
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

        let answers
            (provider : DumpedAssembly)
            : (string -> Set<string> * bool) *
              ((EscapeAnalysisState -> EscapeAnalysisState * Set<string>) -> Set<string>)
            =
            let providerReference =
                clientAssembly.AssemblyReferences.Values
                |> Seq.find (fun r -> r.Name.Name = "Provider")

            let mutable analysis =
                analysisOver
                    [ clientAssembly ; provider ]
                    (fun loaded -> fst (loaded.WithBoundReference providerReference provider))

            (fun methodName ->
                let next, escapes =
                    EscapeAnalysis.escapes analysis (methodNamed clientAssembly "Client.Uses" methodName)

                analysis <- next
                render analysis escapes, escapes.Unknown
            ),
            fun ask ->
                let next, made = ask analysis
                analysis <- next
                made

        let against1, _ = answers (read "Provider" providerImage1)
        let against2, making2 = answers (read "Provider" (compile "Provider" [] version2))

        let failures =
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
                // Named only by a vararg call site's extra arguments, which no definition declares: a
                // value type, which the JIT loads where a call passes it.
                "PassGoneAsVararg", "=System.TypeLoadException"
                // Named only by an indirect call's signature.
                "CallGoneIndirectly", "=System.TypeLoadException"
                // A type argument of a generic method called.
                "InstantiateWithGone", "=System.TypeLoadException"
                // The type a `constrained.` prefix names.
                "ConstrainedOnGone", "=System.TypeLoadException"
            ]

        // Whether a struct holds references reads the types of its fields, one of which the
        // provider no longer has. CoreCLR cannot load the struct, so no instance with it runs, and
        // the analysis answers without deciding the branch.
        against2 "BranchOnHoldsGone" |> ignore
        // So too where a field's type derives from one the provider no longer has.
        against2 "BranchOnHoldsDerivedFromGone" |> ignore

        for methodName, failure in failures do
            let bound, _ = against1 methodName
            let unbound, _ = against2 methodName

            if bound.Contains failure then
                failwith $"%s{methodName} against the provider it was compiled against: %A{Set.toList bound}"

            if not (unbound.Contains failure) then
                failwith $"%s{methodName} against the provider lacking what it uses: %A{Set.toList unbound}"

        // The runtime makes a missing member's exception by running its parameterless constructor,
        // which looks up its message; what that raises escapes in its place.
        for methodName, failure in
            [
                "Read", "System.MissingFieldException"
                "CallGone", "System.MissingMethodException"
            ] do
            let made = making2 (fun analysis -> making analysis failure)
            let unbound, _ = against2 methodName

            if not (Set.isSubset made unbound) then
                failwith
                    $"%s{methodName} against the provider lacking what it uses: %A{Set.toList unbound}, lacking some of what making %s{failure} raises, %A{Set.toList made}"

        // The runtime makes a missing type's exception by running a constructor taking its name,
        // which formats its message; what that raises escapes in its place.
        let madeTypeLoad =
            making2 (fun analysis ->
                let analysis, escapes = EscapeAnalysis.escapes analysis (typeLoadConstructor ())
                analysis, render analysis escapes
            )

        let unmade =
            failures
            |> List.filter (fun (_, failure) -> failure = "=System.TypeLoadException")
            |> List.map fst
            |> List.append [ "ConstrainedReachesGoneLocal" ]
            |> List.choose (fun methodName ->
                let unbound, _ = against2 methodName
                let missing = Set.difference madeTypeLoad unbound

                if missing.IsEmpty then
                    None
                else
                    Some $"%s{methodName} against the provider lacking what it uses lacks %A{Set.toList missing}"
            )

        if not unmade.IsEmpty then
            failwith (String.concat "\n" unmade)

        // With no provider at all, binding any token naming it fails to find the assembly. The real
        // runtime raises `FileNotFoundException` if nothing else supplies one; but first it runs the
        // program's resolve handlers, which may throw, or load an assembly whose code then runs.
        let againstNone =
            let mutable analysis = analysisOver [ clientAssembly ] id

            fun methodName ->
                let next, escapes =
                    EscapeAnalysis.escapes analysis (methodNamed clientAssembly "Client.Uses" methodName)

                analysis <- next
                render analysis escapes, escapes.Unknown

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
                match againstNone methodName with
                | shown, true when shown.Contains "=System.IO.FileNotFoundException" -> None
                | shown, unknown -> Some $"%s{methodName} with no provider: %A{Set.toList shown}, unknown %b{unknown}"
            )

        if not unmet.IsEmpty then
            failwith (String.concat "\n" unmet)

        let opaqueIn (method : MethodKey) (analysis : EscapeAnalysisState) : EscapeAnalysisState * Set<string> =
            let analysis, sources = EscapeAnalysis.unknownSources analysis method
            analysis, Set.map (renderSite analysis) sources

        // A `constrained.` call reaching a method whose local's type is gone: the JIT throws
        // compiling that method, out of the call, where the caller's handlers see it. Nothing is
        // opaque but what making that exception runs.
        match against2 "ConstrainedReachesGoneLocal" with
        | shown, _ when shown.Contains "=System.TypeLoadException" ->
            let opaque =
                making2 (opaqueIn (methodNamed clientAssembly "Client.Uses" "ConstrainedReachesGoneLocal"))

            let others = Set.difference opaque (making2 (opaqueIn (typeLoadConstructor ())))

            if not others.IsEmpty then
                failwith
                    $"ConstrainedReachesGoneLocal against the provider lacking its local's type is opaque beyond making its exception: %A{Set.toList others}"
        | shown, unknown ->
            failwith
                $"ConstrainedReachesGoneLocal against the provider lacking its local's type: %A{Set.toList shown}, unknown %b{unknown}"

        // Making that exception is opaque where `String.Format` formats the strings it is given, and
        // nowhere else.
        let makingOpaque = making2 (opaqueIn (typeLoadConstructor ()))

        if makingOpaque.IsEmpty then
            failwith "Making a TypeLoadException is no longer opaque; the analysis's documentation says it is"

        let beyondFormat =
            Set.difference makingOpaque (making2 (opaqueIn (stringFormat ())))

        if not beyondFormat.IsEmpty then
            failwith $"Making a TypeLoadException is opaque beyond what String.Format is: %A{Set.toList beyondFormat}"

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

    /// A client compiled against one version of a provider, run against another lacking a type
    /// that a type of the client, or of a third assembly `Middle`, derives from, implements or
    /// holds. The JIT loads in full each type
    /// a token names, and each type argument of a method it instantiates, before the body runs, so
    /// the load's `TypeLoadException` comes out of the caller, past the client's own `catch`. The
    /// host's CoreCLR, running each method against the same two assemblies, says which fail.
    [<Test>]
    let ``a type a token names fails to load where what it derives from, implements or holds is gone`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let provider (complete : bool) : string =
            "namespace Provider; public class Kept { }"
            + (if complete then
                   " public class Gone { } public interface IGone { } public struct GoneStruct { }"
               else
                   "")

        let client =
            """
using System.Collections.Generic;

namespace Client;

public class DerivesFromKept : Provider.Kept { }
public class DerivesFromGone : Provider.Gone { }
public class ImplementsGone : Provider.IGone { }
public struct HoldsGone { public Provider.GoneStruct G; }

public static class Uses
{
    static void Generic<T>() { }
    public static bool IsDerivesFromKept(object o) => o is DerivesFromKept;
    public static bool IsDerivesFromGone(object o) => o is DerivesFromGone;
    public static bool IsMiddleDerivesFromGone(object o) => o is Middle.DerivesFromGone;
    public static System.Type TypeOfDerivesFromGone() => typeof(DerivesFromGone);
    public static bool IsImplementsGone(object o) => o is ImplementsGone;
    public static object ArrayOfHoldsGone() => new HoldsGone[1];
    public static object BoxHoldsGone() => default(HoldsGone);
    public static bool IsListOfDerivesFromGone(object o) => o is List<DerivesFromGone>;
    public static void InstantiateWithDerivesFromGone() { Generic<DerivesFromGone>(); }
    public static bool CaughtIsDerivesFromGone(object o)
    {
        try { return o is DerivesFromGone; }
        catch (System.TypeLoadException) { return false; }
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

        let completeProvider = compile "Provider" [] (provider true)
        let incompleteProvider = compile "Provider" [] (provider false)

        let middleImage =
            compile "Middle" [ completeProvider ] "namespace Middle; public class DerivesFromGone : Provider.Gone { }"

        let clientImage = compile "Client" [ completeProvider ; middleImage ] client

        let methods =
            [
                "IsDerivesFromKept", false
                "IsDerivesFromGone", true
                // A type reference, to a type of another assembly whose base class is gone.
                "IsMiddleDerivesFromGone", true
                "TypeOfDerivesFromGone", true
                "IsImplementsGone", true
                "ArrayOfHoldsGone", true
                "BoxHoldsGone", true
                "IsListOfDerivesFromGone", true
                "InstantiateWithDerivesFromGone", true
                "CaughtIsDerivesFromGone", true
            ]

        // Whether each method throws `TypeLoadException` on the host's CoreCLR.
        let hostThrows : Map<string, bool> =
            let context =
                Runtime.Loader.AssemblyLoadContext ("TypeTokenLoads", isCollectible = true)

            context.add_Resolving (
                Func<_, _, _> (fun (context : Runtime.Loader.AssemblyLoadContext) (name : AssemblyName) ->
                    match name.Name with
                    | "Provider" -> context.LoadFromStream (new MemoryStream (incompleteProvider))
                    | "Middle" -> context.LoadFromStream (new MemoryStream (middleImage))
                    | _ -> null
                )
            )

            try
                let uses =
                    context.LoadFromStream(new MemoryStream (clientImage)).GetType "Client.Uses"

                methods
                |> List.map (fun (name, _) ->
                    let method = uses.GetMethod name

                    // Reflection passes a null argument to a value-type parameter as its default.
                    let arguments : obj[] = Array.create (method.GetParameters ()).Length null

                    let throws =
                        try
                            method.Invoke ((null : obj), arguments) |> ignore<obj>
                            false
                        with :? TargetInvocationException as e ->
                            e.InnerException :? TypeLoadException

                    name, throws
                )
                |> Map.ofList
            finally
                context.Unload ()

        let read (name : string) (image : byte[]) : DumpedAssembly =
            Assembly.read loggerFactory (Some $"%s{name}.dll") (new MemoryStream (image))

        let clientAssembly = read "Client" clientImage
        let providerAssembly = read "Provider" incompleteProvider
        let middleAssembly = read "Middle" middleImage

        let referenceTo (from : DumpedAssembly) (name : string) =
            from.AssemblyReferences.Values |> Seq.find (fun r -> r.Name.Name = name)

        let mutable analysis =
            analysisOver
                [ clientAssembly ; providerAssembly ; middleAssembly ]
                (fun loaded ->
                    let loaded =
                        fst (loaded.WithBoundReference (referenceTo clientAssembly "Provider") providerAssembly)

                    let loaded =
                        fst (loaded.WithBoundReference (referenceTo clientAssembly "Middle") middleAssembly)

                    fst (loaded.WithBoundReference (referenceTo middleAssembly "Provider") providerAssembly)
                )

        let failures =
            [
                for name, expected in methods do
                    if hostThrows.[name] <> expected then
                        yield $"%s{name}: the host throws TypeLoadException %b{hostThrows.[name]}, not %b{expected}"

                    let next, escapes =
                        EscapeAnalysis.escapes analysis (methodNamed clientAssembly "Client.Uses" name)

                    analysis <- next
                    let shown = render analysis escapes

                    if shown.Contains "=System.TypeLoadException" <> expected then
                        yield $"%s{name}: %A{Set.toList shown}, unknown %b{escapes.Unknown}"
            ]

        if not failures.IsEmpty then
            failures |> String.concat Environment.NewLine |> failwith

    /// The names of the types that a spelling in `assembly` names, arguments, elements and
    /// signatures included.
    let rec private namedTypes (assembly : DumpedAssembly) (spelling : TypeDefn) : string list =
        match spelling with
        | TypeDefn.FromDefinition (identity, _) -> [ assembly.TypeDefs.[identity.TypeDefinition.Get].Name ]
        | TypeDefn.FromReference (reference, _) -> [ reference.Name ]
        | TypeDefn.GenericInstantiation (generic, arguments) ->
            List.collect (namedTypes assembly) (generic :: List.ofSeq arguments)
        | TypeDefn.Array (element, _)
        | TypeDefn.OneDimensionalArrayLowerBoundZero element
        | TypeDefn.Pointer element
        | TypeDefn.Byref element
        | TypeDefn.Pinned element -> namedTypes assembly element
        | TypeDefn.Modified modified -> namedTypes assembly modified.Unmodified
        | TypeDefn.FunctionPointer signature ->
            let returned =
                match signature.ReturnType with
                | MethodReturnType.Void -> []
                | MethodReturnType.Returns returned -> [ returned ]

            List.collect (namedTypes assembly) (returned @ signature.ParameterTypes)
        | TypeDefn.PrimitiveType _
        | TypeDefn.GenericTypeParameter _
        | TypeDefn.GenericMethodParameter _
        | TypeDefn.Void -> []

    /// A client compiled against one version of a provider, run against another lacking a type
    /// that the type of a local of the client names, derives from, implements or holds. The JIT
    /// loads in full the type of each local before the body runs, other than a pointer's,
    /// a byref's or a function pointer's, so the load's `TypeLoadException` comes out of the
    /// caller, past the client's own `catch`. No instruction of these methods names the type by a
    /// token. The host's CoreCLR, running each method against the same two assemblies, says which
    /// fail.
    [<Test>]
    let ``the type of a local fails to load where what it names, derives from, implements or holds is gone`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let provider (complete : bool) : string =
            "namespace Provider; public class Kept { }"
            + (if complete then
                   " public class Gone { } public interface IGone { } public struct GoneStruct { }"
               else
                   "")

        // Each local of a type the JIT loads has its address taken, so that Roslyn keeps it; a
        // method taking a byref to it loads nothing at the caller. A pointer is read twice instead.
        let client =
            """
using System.Collections.Generic;

namespace Client;

public class DerivesFromKept : Provider.Kept { }
public class DerivesFromGone : Provider.Gone { }
public class ImplementsGone : Provider.IGone { }
public struct HoldsGone { public Provider.GoneStruct G; }

public static unsafe class Locals
{
    static void OutDerivesFromKept(out DerivesFromKept x) => throw null;
    static void OutGone(out Provider.Gone x) => throw null;
    static void OutDerivesFromGone(out DerivesFromGone x) => throw null;
    static void OutImplementsGone(out ImplementsGone x) => throw null;
    static void OutHoldsGone(out HoldsGone x) => throw null;
    static void OutArrayOfHoldsGone(out HoldsGone[] x) => throw null;
    static void OutListOfDerivesFromGone(out List<DerivesFromGone> x) => throw null;

    public static void OfDerivesFromKept() { DerivesFromKept x; OutDerivesFromKept(out x); }
    public static void OfGone() { Provider.Gone x; OutGone(out x); }
    public static void OfDerivesFromGone() { DerivesFromGone x; OutDerivesFromGone(out x); }
    public static void OfImplementsGone() { ImplementsGone x; OutImplementsGone(out x); }
    public static void OfHoldsGone() { HoldsGone x; OutHoldsGone(out x); }
    public static void OfArrayOfHoldsGone() { HoldsGone[] x; OutArrayOfHoldsGone(out x); }
    public static void OfListOfDerivesFromGone() { List<DerivesFromGone> x; OutListOfDerivesFromGone(out x); }
    public static bool CaughtOfDerivesFromGone()
    {
        try { DerivesFromGone x; OutDerivesFromGone(out x); return true; }
        catch (System.TypeLoadException) { return false; }
    }
    public static bool OfPointerToGoneStruct(void* q)
    {
        Provider.GoneStruct* p = (Provider.GoneStruct*)q;
        return p == null || p == (Provider.GoneStruct*)1;
    }
    public static bool OfFunctionPointerOfGoneStruct(void* q)
    {
        delegate*<Provider.GoneStruct, void> f = (delegate*<Provider.GoneStruct, void>)q;
        return f == null || f == (delegate*<Provider.GoneStruct, void>)1;
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

        let completeProvider = compile "Provider" [] (provider true)
        let incompleteProvider = compile "Provider" [] (provider false)
        let clientImage = compile "Client" [ completeProvider ] client

        // Each method, the name of the type its local spells, and whether the JIT fails to load it.
        let methods =
            [
                "OfDerivesFromKept", "DerivesFromKept", false
                "OfGone", "Gone", true
                "OfDerivesFromGone", "DerivesFromGone", true
                "OfImplementsGone", "ImplementsGone", true
                "OfHoldsGone", "HoldsGone", true
                "OfArrayOfHoldsGone", "HoldsGone", true
                "OfListOfDerivesFromGone", "DerivesFromGone", true
                "CaughtOfDerivesFromGone", "DerivesFromGone", true
                "OfPointerToGoneStruct", "GoneStruct", false
                "OfFunctionPointerOfGoneStruct", "GoneStruct", false
            ]

        // Whether each method throws `TypeLoadException` on the host's CoreCLR.
        let hostThrows : Map<string, bool> =
            let context =
                Runtime.Loader.AssemblyLoadContext ("LocalTypeLoads", isCollectible = true)

            context.add_Resolving (
                Func<_, _, _> (fun (context : Runtime.Loader.AssemblyLoadContext) (name : AssemblyName) ->
                    match name.Name with
                    | "Provider" -> context.LoadFromStream (new MemoryStream (incompleteProvider))
                    | _ -> null
                )
            )

            try
                let locals =
                    context.LoadFromStream(new MemoryStream (clientImage)).GetType "Client.Locals"

                methods
                |> List.map (fun (name, _, _) ->
                    let method = locals.GetMethod name

                    // Reflection passes a null argument to a pointer parameter as a null pointer.
                    let arguments : obj[] = Array.create (method.GetParameters ()).Length null

                    let throws =
                        try
                            method.Invoke ((null : obj), arguments) |> ignore<obj>
                            false
                        with :? TargetInvocationException as e ->
                            e.InnerException :? TypeLoadException

                    name, throws
                )
                |> Map.ofList
            finally
                context.Unload ()

        let read (name : string) (image : byte[]) : DumpedAssembly =
            Assembly.read loggerFactory (Some $"%s{name}.dll") (new MemoryStream (image))

        let clientAssembly = read "Client" clientImage
        let providerAssembly = read "Provider" incompleteProvider

        let referenceTo (from : DumpedAssembly) (name : string) =
            from.AssemblyReferences.Values |> Seq.find (fun r -> r.Name.Name = name)

        let mutable analysis =
            analysisOver
                [ clientAssembly ; providerAssembly ]
                (fun loaded -> fst (loaded.WithBoundReference (referenceTo clientAssembly "Provider") providerAssembly))

        let named = namedTypes clientAssembly

        let failures =
            [
                for name, localType, expected in methods do
                    // Roslyn keeps the local whose type the method is about.
                    let locals =
                        match
                            (clientAssembly.Methods.Values
                             |> Seq.filter (fun m -> m.Name = name)
                             |> Seq.exactlyOne)
                                .Body
                        with
                        | MethodBody.Il body -> body.LocalVars |> Option.map List.ofSeq |> Option.defaultValue []
                        | _ -> []

                    if not (locals |> List.exists (fun local -> List.contains localType (named local))) then
                        yield $"%s{name}: no local spells %s{localType}: %A{locals}"

                    if hostThrows.[name] <> expected then
                        yield $"%s{name}: the host throws TypeLoadException %b{hostThrows.[name]}, not %b{expected}"

                    let next, escapes =
                        EscapeAnalysis.escapes analysis (methodNamed clientAssembly "Client.Locals" name)

                    analysis <- next
                    let shown = render analysis escapes

                    if shown.Contains "=System.TypeLoadException" <> expected then
                        yield $"%s{name}: %A{Set.toList shown}, unknown %b{escapes.Unknown}"
            ]

        if not failures.IsEmpty then
            failures |> String.concat Environment.NewLine |> failwith

    /// A client compiled against one version of a provider, run against another lacking a type
    /// that a parameter or the return type of a client method names, derives from, implements or
    /// holds. The JIT compiling the method loads in full its return type and the type of each
    /// parameter but a pointer, a byref or a function pointer, as it does a local's, so the load's
    /// `TypeLoadException` comes out of the call to the method, past the method's own `catch`.
    /// A caller's JIT loads less of a callee's signature, so the host calls each method through a
    /// function pointer whose signature names none of these types. No instruction of these
    /// methods names the type by a token.
    [<Test>]
    let ``the signature of a method fails to load where what it names, derives from, implements or holds is gone``
        ()
        : unit
        =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let provider (complete : bool) : string =
            "namespace Provider; public class Kept { }"
            + (if complete then
                   " public class Gone { } public interface IGone { } public struct GoneStruct { }"
               else
                   "")

        let client =
            """
using System.Collections.Generic;

namespace Client;

public class DerivesFromKept : Provider.Kept { }
public class DerivesFromGone : Provider.Gone { }
public class ImplementsGone : Provider.IGone { }
public struct HoldsGone { public Provider.GoneStruct G; }

public static unsafe class Signatures
{
    public static nint ParameterOfDerivesFromKept(DerivesFromKept x) => 0;
    public static nint ParameterOfGone(Provider.Gone x) => 0;
    public static nint ParameterOfDerivesFromGone(DerivesFromGone x) => 0;
    public static nint ParameterOfImplementsGone(ImplementsGone x) => 0;
    public static nint ParameterOfArrayOfHoldsGone(HoldsGone[] x) => 0;
    public static nint ParameterOfListOfDerivesFromGone(List<DerivesFromGone> x) => 0;
    public static nint ParameterOfPointerToGoneStruct(Provider.GoneStruct* x) => 0;
    public static nint ParameterOfByrefToGoneStruct(ref Provider.GoneStruct x) => 0;
    public static nint CaughtParameterOfDerivesFromGone(DerivesFromGone x)
    {
        try { return 0; }
        catch (System.TypeLoadException) { return 1; }
    }
    public static DerivesFromKept ReturnsDerivesFromKept(nint x) => null;
    public static DerivesFromGone ReturnsDerivesFromGone(nint x) => null;
    public static List<DerivesFromGone> ReturnsListOfDerivesFromGone(nint x) => null;
    public static Provider.GoneStruct* ReturnsPointerToGoneStruct(nint x) => null;
    public static ref HoldsGone ReturnsByrefToHoldsGone(nint x) => ref *(HoldsGone*)x;
}

public static unsafe class Probe
{
    public static bool ThrowsTypeLoad(nint method)
    {
        try { ((delegate*<nint, nint>)method)(0); return false; }
        catch (System.TypeLoadException) { return true; }
        catch { return false; }
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

        let completeProvider = compile "Provider" [] (provider true)
        let incompleteProvider = compile "Provider" [] (provider false)
        let clientImage = compile "Client" [ completeProvider ] client

        // Each method, the name of a type its signature spells, and whether the JIT fails to load
        // its signature.
        let methods =
            [
                "ParameterOfDerivesFromKept", "DerivesFromKept", false
                "ParameterOfGone", "Gone", true
                "ParameterOfDerivesFromGone", "DerivesFromGone", true
                "ParameterOfImplementsGone", "ImplementsGone", true
                "ParameterOfArrayOfHoldsGone", "HoldsGone", true
                "ParameterOfListOfDerivesFromGone", "DerivesFromGone", true
                "ParameterOfPointerToGoneStruct", "GoneStruct", false
                "ParameterOfByrefToGoneStruct", "GoneStruct", false
                "CaughtParameterOfDerivesFromGone", "DerivesFromGone", true
                "ReturnsDerivesFromKept", "DerivesFromKept", false
                "ReturnsDerivesFromGone", "DerivesFromGone", true
                "ReturnsListOfDerivesFromGone", "DerivesFromGone", true
                "ReturnsPointerToGoneStruct", "GoneStruct", true
                "ReturnsByrefToHoldsGone", "HoldsGone", true
            ]

        // Whether calling each method throws `TypeLoadException` on the host's CoreCLR.
        let hostThrows : Map<string, bool> =
            let context =
                Runtime.Loader.AssemblyLoadContext ("SignatureTypeLoads", isCollectible = true)

            context.add_Resolving (
                Func<_, _, _> (fun (context : Runtime.Loader.AssemblyLoadContext) (name : AssemblyName) ->
                    match name.Name with
                    | "Provider" -> context.LoadFromStream (new MemoryStream (incompleteProvider))
                    | _ -> null
                )
            )

            try
                let loaded = context.LoadFromStream (new MemoryStream (clientImage))
                let signatures = loaded.GetType "Client.Signatures"
                let probe = (loaded.GetType "Client.Probe").GetMethod "ThrowsTypeLoad"

                methods
                |> List.map (fun (name, _, _) ->
                    // Looking a method up by name alone reads none of its signature's types.
                    let pointer = (signatures.GetMethod name).MethodHandle.GetFunctionPointer ()
                    name, probe.Invoke ((null : obj), [| box pointer |]) |> unbox<bool>
                )
                |> Map.ofList
            finally
                context.Unload ()

        let read (name : string) (image : byte[]) : DumpedAssembly =
            Assembly.read loggerFactory (Some $"%s{name}.dll") (new MemoryStream (image))

        let clientAssembly = read "Client" clientImage
        let providerAssembly = read "Provider" incompleteProvider

        let referenceTo (from : DumpedAssembly) (name : string) =
            from.AssemblyReferences.Values |> Seq.find (fun r -> r.Name.Name = name)

        let mutable analysis =
            analysisOver
                [ clientAssembly ; providerAssembly ]
                (fun loaded -> fst (loaded.WithBoundReference (referenceTo clientAssembly "Provider") providerAssembly))

        let failures =
            [
                for name, signatureType, expected in methods do
                    let method =
                        clientAssembly.Methods.Values
                        |> Seq.filter (fun m -> m.Name = name)
                        |> Seq.exactlyOne

                    let spelled =
                        match method.Signature.ReturnType with
                        | MethodReturnType.Void -> method.Signature.ParameterTypes
                        | MethodReturnType.Returns returned -> returned :: method.Signature.ParameterTypes

                    if
                        not (
                            spelled
                            |> List.exists (fun ty -> List.contains signatureType (namedTypes clientAssembly ty))
                        )
                    then
                        yield $"%s{name}: its signature does not spell %s{signatureType}: %A{spelled}"

                    // Nothing in the body names a type by a token.
                    match method.Body with
                    | MethodBody.Il body ->
                        let tokens =
                            body.Instructions
                            |> List.filter (fun (op, _) ->
                                match op with
                                | IlOp.UnaryMetadataToken _ -> true
                                | _ -> false
                            )

                        if not tokens.IsEmpty then
                            yield $"%s{name}: its body names tokens: %A{tokens}"
                    | body -> yield $"%s{name}: no IL body: %A{body}"

                    if hostThrows.[name] <> expected then
                        yield $"%s{name}: the host throws TypeLoadException %b{hostThrows.[name]}, not %b{expected}"

                    let next, escapes =
                        EscapeAnalysis.escapes analysis (methodNamed clientAssembly "Client.Signatures" name)

                    analysis <- next
                    let shown = render analysis escapes

                    if shown.Contains "=System.TypeLoadException" <> expected then
                        yield $"%s{name}: %A{Set.toList shown}, unknown %b{escapes.Unknown}"
            ]

        if not failures.IsEmpty then
            failures |> String.concat Environment.NewLine |> failwith

    /// How compiling an `[UnmanagedCallersOnly]` method ends on the host's CoreCLR, which
    /// `RuntimeHelpers.PrepareMethod` asks of it: the JIT compiles the method as native code calls
    /// it, a reverse P/Invoke, and the exception compiling it raises comes out of `PrepareMethod`.
    [<RequireQualifiedAccess>]
    type private UcoCompilation =
        | Compiled
        | Raises of exceptionName : string

    /// A provider whose `GoneStruct` the client is compiled against, and a version of it without
    /// that type, which the client runs against.
    let private ucoProvider (complete : bool) : byte[] =
        Roslyn.compileAssembly
            "Provider"
            OutputKind.DynamicallyLinkedLibrary
            []
            [
                "namespace Provider; public struct Kept { }"
                + (if complete then " public struct GoneStruct { }" else "")
            ]

    /// How compiling each method of `typeName` named in `methods` ends on the host, with the
    /// assembly `image` loaded beside `provider`.
    let private ucoCompilations
        (image : byte[])
        (provider : byte[])
        (typeName : string)
        (methods : string list)
        : Map<string, UcoCompilation>
        =
        let context =
            Runtime.Loader.AssemblyLoadContext ("UnmanagedCallersOnlyConventions", isCollectible = true)

        context.add_Resolving (
            Func<_, _, _> (fun (context : Runtime.Loader.AssemblyLoadContext) (name : AssemblyName) ->
                match name.Name with
                | "Provider" -> context.LoadFromStream (new MemoryStream (provider))
                | _ -> null
            )
        )

        try
            let declaring = (context.LoadFromStream (new MemoryStream (image))).GetType typeName

            methods
            |> List.map (fun name ->
                // Looking a method up by name alone reads none of its signature's types.
                let handle = (declaring.GetMethod name).MethodHandle

                let outcome =
                    try
                        Runtime.CompilerServices.RuntimeHelpers.PrepareMethod handle
                        UcoCompilation.Compiled
                    with e ->
                        UcoCompilation.Raises (e.GetType().FullName)

                name, outcome
            )
            |> Map.ofList
        finally
            context.Unload ()

    /// As `analysisOver`, over the test host's own framework rather than the framework under test:
    /// for an oracle that compiles on the test host's own CoreCLR, in process.
    let private analysisOverHostImage
        (assemblies : DumpedAssembly list)
        (bind : LoadedAssemblies -> LoadedAssemblies)
        : EscapeAnalysisState
        =
        let _, loggerFactory = LoggerFactory.makeTest ()

        analysisOf
            (Assembly.readFile loggerFactory HostImage.coreLibPath)
            HostImage.runtimeDirs
            (hostTarget ())
            HardwareIntrinsicsProfile.ScalarOnly
            Assumption.all
            assemblies
            bind

    /// What the escape analysis says compiling each of `methods`, of `typeName` in the client
    /// `image`, raises against `provider`: `Raises "System.TypeLoadException"` where the answer
    /// names it, otherwise `Raises "Unknown"` where the answer is unknown, and otherwise
    /// `Compiled`; with the places that make each answer unknown. The analysis is over the test
    /// host's own framework, as `ucoCompilations` compiles on the test host's own CoreCLR.
    let private ucoAnalysed
        (image : byte[])
        (provider : byte[])
        (typeName : string)
        (methods : string list)
        : Map<string, UcoCompilation * Set<string>>
        =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let read (name : string) (image : byte[]) : DumpedAssembly =
            Assembly.read loggerFactory (Some $"%s{name}.dll") (new MemoryStream (image))

        let client = read "Client" image
        let providerAssembly = read "Provider" provider

        let reference =
            client.AssemblyReferences.Values |> Seq.find (fun r -> r.Name.Name = "Provider")

        let mutable analysis =
            analysisOverHostImage
                [ client ; providerAssembly ]
                (fun loaded -> fst (loaded.WithBoundReference reference providerAssembly))

        methods
        |> List.map (fun name ->
            let key = methodNamed client typeName name
            let next, escapes = EscapeAnalysis.escapes analysis key
            let next, sources = EscapeAnalysis.unknownSources next key
            analysis <- next

            let outcome =
                if (render analysis escapes).Contains "=System.TypeLoadException" then
                    UcoCompilation.Raises "System.TypeLoadException"
                elif escapes.Unknown then
                    UcoCompilation.Raises "Unknown"
                else
                    UcoCompilation.Compiled

            name, (outcome, sources |> Seq.map (renderSite analysis) |> Set.ofSeq)
        )
        |> Map.ofList

    /// Each method takes a pointer to a type missing at run time. CoreCLR compiles an
    /// `[UnmanagedCallersOnly]` method for the calling convention it reads from the attribute's
    /// `CallConvs`, matching each type by its serialised name alone; a second base convention, or
    /// none, gives the platform's default, and the modifiers combine with Swift. Only where it
    /// reads Swift's, on a target that compiles Swift calls, does the JIT load what a pointer
    /// parameter points to (`lvaInitSpecialSwiftParam`), and fail to. The host says which.
    [<Test>]
    let ``the JIT loads a pointer parameter's target only where CoreCLR reads Swift's convention from UnmanagedCallersOnly``
        ()
        : unit
        =
        let client =
            """
using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace Client;

public static unsafe class Uco
{
    [UnmanagedCallersOnly] public static void NoCallConvs(Provider.GoneStruct* x) { }
    [UnmanagedCallersOnly(CallConvs = new[] { typeof(CallConvSwift) })] public static void Swift(Provider.GoneStruct* x) { }
    [UnmanagedCallersOnly(CallConvs = new[] { typeof(CallConvSwift) })] public static void SwiftKept(Provider.Kept* x) { }
    [UnmanagedCallersOnly(CallConvs = new[] { typeof(CallConvSwift) })] public static void SwiftByte(byte* x) { }
    [UnmanagedCallersOnly(CallConvs = new[] { typeof(CallConvCdecl) })] public static void Cdecl(Provider.GoneStruct* x) { }
    [UnmanagedCallersOnly(CallConvs = new[] { typeof(CallConvSwift), typeof(CallConvCdecl) })] public static void SwiftThenCdecl(Provider.GoneStruct* x) { }
    [UnmanagedCallersOnly(CallConvs = new[] { typeof(CallConvCdecl), typeof(CallConvSwift) })] public static void CdeclThenSwift(Provider.GoneStruct* x) { }
    [UnmanagedCallersOnly(CallConvs = new[] { typeof(CallConvSwift), typeof(CallConvSwift) })] public static void SwiftTwice(Provider.GoneStruct* x) { }
    [UnmanagedCallersOnly(CallConvs = new[] { typeof(CallConvSwift), typeof(CallConvSuppressGCTransition) })] public static void SwiftSuppress(Provider.GoneStruct* x) { }
    [UnmanagedCallersOnly(CallConvs = new[] { typeof(CallConvMemberFunction), typeof(CallConvSwift) })] public static void MemberSwift(Provider.GoneStruct* x) { }
    [UnmanagedCallersOnly(CallConvs = new[] { typeof(CallConvMemberFunction) })] public static void Member(Provider.GoneStruct* x) { }
    [UnmanagedCallersOnly(CallConvs = new Type[0])] public static void Empty(Provider.GoneStruct* x) { }
    [UnmanagedCallersOnly(CallConvs = new[] { typeof(CallConvSwift) }, EntryPoint = "e")] public static void SwiftEntryPoint(Provider.GoneStruct* x) { }
    [UnmanagedCallersOnly(EntryPoint = "e", CallConvs = new[] { typeof(CallConvSwift) })] public static void EntryPointSwift(Provider.GoneStruct* x) { }
}
"""

        let clientImage =
            Roslyn.compileAssembly
                "Client"
                OutputKind.DynamicallyLinkedLibrary
                [
                    MetadataReference.CreateFromImage (ImmutableArray.CreateRange (ucoProvider true))
                ]
                [ client ]

        let typeLoad = UcoCompilation.Raises "System.TypeLoadException"

        let expected =
            [
                "NoCallConvs", UcoCompilation.Compiled
                "Swift", typeLoad
                "SwiftKept", UcoCompilation.Compiled
                "SwiftByte", UcoCompilation.Compiled
                "Cdecl", UcoCompilation.Compiled
                "SwiftThenCdecl", UcoCompilation.Compiled
                "CdeclThenSwift", UcoCompilation.Compiled
                "SwiftTwice", UcoCompilation.Compiled
                "SwiftSuppress", typeLoad
                "MemberSwift", typeLoad
                "Member", UcoCompilation.Compiled
                "Empty", UcoCompilation.Compiled
                "SwiftEntryPoint", typeLoad
                "EntryPointSwift", typeLoad
            ]

        let names = List.map fst expected
        let incomplete = ucoProvider false
        let host = ucoCompilations clientImage incomplete "Client.Uco" names
        let analysed = ucoAnalysed clientImage incomplete "Client.Uco" names

        let failures =
            [
                for name, wanted in expected do
                    if host.[name] <> wanted then
                        yield $"%s{name}: the host's JIT %A{host.[name]}, not %A{wanted}"

                    let outcome, sources = analysed.[name]

                    if outcome <> wanted then
                        yield $"%s{name}: the analysis says %A{outcome}, unknown at %A{Set.toList sources}"
            ]

        if not failures.IsEmpty then
            failures |> String.concat Environment.NewLine |> failwith

    /// `CallConvs = null`, which C# accepts, serialises the array's length as the null sentinel,
    /// and CoreCLR reads the elements of the null array it parsed: compiling the method faults in
    /// the runtime and the process dies (`TryGetCallingConventionFromTypeArray`). The analysis
    /// cannot say that, so its answer is unknown, from the method's entry. The host runs the
    /// compilation in a process of its own.
    [<Test>]
    let ``compiling an UnmanagedCallersOnly method whose CallConvs is null kills the process, which the analysis does not name``
        ()
        : unit
        =
        let guest =
            """
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

public static class Program
{
    [UnmanagedCallersOnly(CallConvs = null)]
    public static void NullArray(int x) { }

    public static int Main()
    {
        RuntimeHelpers.PrepareMethod(typeof(Program).GetMethod("NullArray").MethodHandle);
        return 0;
    }
}
"""

        match
            RealRuntime.executeWithRealRuntime
                [||]
                (Roslyn.compileAssembly "NullCallConvs" OutputKind.ConsoleApplication [] [ guest ])
        with
        // A signal reports as `128 + signo`: SIGSEGV's 11.
        | RealRuntimeResult.NormalExit 139
        | RealRuntimeResult.Aborted _ -> ()
        | other -> failwith $"Compiling the method ends %A{other}, not in the process's death"

        let client =
            """
using System.Runtime.InteropServices;

namespace Client;

public static unsafe class Uco
{
    [UnmanagedCallersOnly(CallConvs = null)] public static void NullArray(Provider.GoneStruct* x) { }
}
"""

        let clientImage =
            Roslyn.compileAssembly
                "Client"
                OutputKind.DynamicallyLinkedLibrary
                [
                    MetadataReference.CreateFromImage (ImmutableArray.CreateRange (ucoProvider true))
                ]
                [ client ]

        let outcome, sources =
            (ucoAnalysed clientImage (ucoProvider false) "Client.Uco" [ "NullArray" ]).["NullArray"]

        (outcome, Set.toList sources)
        |> shouldEqual (UcoCompilation.Raises "Unknown", [ "Client.Uco::NullArray entry UnreadableCallingConvention" ])

    /// An assembly `Probe` whose class `C` declares `[UnmanagedCallersOnly]` methods `Uco` (static)
    /// and `Virt` (virtual, which no valid UCO is, but whose delegate the JIT refuses before that
    /// matters), a static `Plain` without the attribute, and, each returning the delegate it makes
    /// (an `Action<int>`, or null):
    /// - `Adjacent`, `AdjacentPlain`, `Virtual`: the target's address instruction right before the
    ///   `newobj` of the delegate's constructor (`ldftn`, or `dup; ldvirtftn` for `Virtual`);
    /// - `ViaLocal`: the address stored in a local and loaded back before the `newobj`;
    /// - `Caught`: `Adjacent` inside a `try` that catches `NotSupportedException`.
    let private delegateProbes : Lazy<byte[]> =
        lazy
            let builder =
                Emit.PersistedAssemblyBuilder (AssemblyName "Probe", typeof<obj>.Assembly)

            let modul = builder.DefineDynamicModule "Probe"
            let c = modul.DefineType ("C", TypeAttributes.Public)

            c.DefineDefaultConstructor MethodAttributes.Public
            |> ignore<Emit.ConstructorBuilder>

            let unmanagedCallersOnly =
                Emit.CustomAttributeBuilder (
                    typeof<Runtime.InteropServices.UnmanagedCallersOnlyAttribute>.GetConstructor Type.EmptyTypes,
                    [||]
                )

            let target (name : string) (attributes : MethodAttributes) (uco : bool) : MethodInfo =
                let method = c.DefineMethod (name, attributes, typeof<Void>, [| typeof<int> |])

                if uco then
                    method.SetCustomAttribute unmanagedCallersOnly

                method.GetILGenerator().Emit Emit.OpCodes.Ret
                method :> MethodInfo

            let uco = target "Uco" (MethodAttributes.Public ||| MethodAttributes.Static) true

            let plain =
                target "Plain" (MethodAttributes.Public ||| MethodAttributes.Static) false

            let virt = target "Virt" (MethodAttributes.Public ||| MethodAttributes.Virtual) true

            let constructor =
                typeof<Action<int>>.GetConstructor [| typeof<obj> ; typeof<nativeint> |]

            let define (name : string) (emit : Emit.ILGenerator -> unit) : unit =
                let method =
                    c.DefineMethod (
                        name,
                        MethodAttributes.Public ||| MethodAttributes.Static,
                        typeof<obj>,
                        [| c :> Type |]
                    )

                let il = method.GetILGenerator ()
                emit il
                il.Emit Emit.OpCodes.Ret

            define
                "Adjacent"
                (fun il ->
                    il.Emit Emit.OpCodes.Ldnull
                    il.Emit (Emit.OpCodes.Ldftn, uco)
                    il.Emit (Emit.OpCodes.Newobj, constructor)
                )

            define
                "AdjacentPlain"
                (fun il ->
                    il.Emit Emit.OpCodes.Ldnull
                    il.Emit (Emit.OpCodes.Ldftn, plain)
                    il.Emit (Emit.OpCodes.Newobj, constructor)
                )

            define
                "Virtual"
                (fun il ->
                    il.Emit Emit.OpCodes.Ldarg_0
                    il.Emit Emit.OpCodes.Dup
                    il.Emit (Emit.OpCodes.Ldvirtftn, virt)
                    il.Emit (Emit.OpCodes.Newobj, constructor)
                )

            define
                "ViaLocal"
                (fun il ->
                    let address = il.DeclareLocal typeof<nativeint>
                    il.Emit (Emit.OpCodes.Ldftn, uco)
                    il.Emit (Emit.OpCodes.Stloc, address)
                    il.Emit Emit.OpCodes.Ldnull
                    il.Emit (Emit.OpCodes.Ldloc, address)
                    il.Emit (Emit.OpCodes.Newobj, constructor)
                )

            define
                "Caught"
                (fun il ->
                    let made = il.DeclareLocal typeof<obj>
                    il.BeginExceptionBlock () |> ignore<Emit.Label>
                    il.Emit Emit.OpCodes.Ldnull
                    il.Emit (Emit.OpCodes.Ldftn, uco)
                    il.Emit (Emit.OpCodes.Newobj, constructor)
                    il.Emit (Emit.OpCodes.Stloc, made)
                    il.BeginCatchBlock typeof<NotSupportedException>
                    il.Emit Emit.OpCodes.Pop
                    il.EndExceptionBlock ()
                    il.Emit (Emit.OpCodes.Ldloc, made)
                )

            c.CreateType () |> ignore<Type>
            use image = new MemoryStream ()
            builder.Save image
            image.ToArray ()

    /// .NET 10's CoreCLR refuses to make a delegate over an `[UnmanagedCallersOnly]` method
    /// (`COMDelegate::GetDelegateCtor`, `NotSupported_UnmanagedCallersOnlyTarget`), which it asks
    /// where the JIT compiles the `newobj` of the delegate's constructor and sees the method the
    /// target's address instruction names (`fgOptimizeDelegateConstructor`), before it walks any
    /// signature for a thunk. That fails the compilation of the method making the delegate, past
    /// every handler of it, on both tiers. Where the address reaches the `newobj` any other way, the
    /// JIT does not ask, and the runtime's construction of the delegate (`Delegate_Construct`) does
    /// not refuse it. The host compiles each probe, at tier 0 and fully optimised, against which the
    /// analysis reads the host's own image (.NET 11 refuses no such delegate,
    /// `UnmanagedCallersOnly.refusesDelegateTarget`).
    [<Test>]
    let ``making a delegate over an UnmanagedCallersOnly method fails the compilation where the JIT names the target``
        ()
        : unit
        =
        let expected =
            [
                "Adjacent", true
                "AdjacentPlain", false
                "Virtual", true
                "ViaLocal", false
                "Caught", true
            ]

        let failures =
            [
                for collectible in [ false ; true ] do
                    let context =
                        Runtime.Loader.AssemblyLoadContext ("DelegateOverUco", isCollectible = collectible)

                    try
                        let c =
                            (context.LoadFromStream (new MemoryStream (delegateProbes.Value))).GetType "C"

                        for name, refused in expected do
                            let raised =
                                try
                                    Runtime.CompilerServices.RuntimeHelpers.PrepareMethod
                                        (c.GetMethod name).MethodHandle

                                    None
                                with e ->
                                    Some (e.GetType().FullName)

                            let wanted =
                                if refused then
                                    Some "System.NotSupportedException"
                                else
                                    None

                            if raised <> wanted then
                                yield
                                    $"%s{name} (collectible %b{collectible}): the host's JIT raises %A{raised}, not %A{wanted}"
                    finally
                        if collectible then
                            context.Unload ()
            ]

        let _, loggerFactory = LoggerFactory.makeTest ()

        let assembly =
            Assembly.read loggerFactory (Some "Probe.dll") (new MemoryStream (delegateProbes.Value))

        let mutable analysis = analysisOverHostImage [ assembly ] id

        let failures =
            [
                yield! failures

                for name, refused in expected do
                    let next, escapes = EscapeAnalysis.escapes analysis (methodNamed assembly "C" name)
                    analysis <- next
                    let shown = render analysis escapes

                    if shown.Contains "=System.NotSupportedException" <> refused then
                        yield $"%s{name}: the analysis says %A{Set.toList shown}"
            ]

        if not failures.IsEmpty then
            failures |> String.concat Environment.NewLine |> failwith

    /// .NET 11's `UnmanagedCallersOnlyAttribute` declares an `AssociatedSourceType` named argument,
    /// which .NET 10's does not, and each runtime's VM parses the attribute's blob against its own
    /// attribute: .NET 10 refuses a blob naming it (a `COMException`), and .NET 11 reads the
    /// conventions beside it. A `SwiftSelf*` parameter shows which it read: the JIT refuses one only
    /// for Swift's convention (`lvaInitSpecialSwiftParam`'s `BADCODE`, an
    /// `InvalidProgramException`). C# cannot spell the argument against .NET 10's reference
    /// assemblies, so the guest spells an `EntryPoint` of the same length and the image is patched.
    /// The runtime under test runs the guest, in a process of its own.
    [<Test>]
    let ``each runtime reads an UnmanagedCallersOnly naming AssociatedSourceType as its own attribute declares``
        ()
        : unit
        =
        let guest =
            """
using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Runtime.InteropServices.Swift;

public static unsafe class Program
{
    [UnmanagedCallersOnly(CallConvs = new[] { typeof(CallConvSwift) })]
    public static void Control(SwiftSelf* x) { }

    [UnmanagedCallersOnly(CallConvs = new[] { typeof(CallConvSwift) }, EntryPoint = "0123456789")]
    public static void Associated(SwiftSelf* x) { }

    static int Compile(string name)
    {
        try { RuntimeHelpers.PrepareMethod(typeof(Program).GetMethod(name).MethodHandle); return 0; }
        catch (InvalidProgramException) { return 1; }
        catch (COMException) { return 2; }
    }

    public static int Main() => 10 * Compile("Control") + Compile("Associated");
}
"""

        let compiled =
            Roslyn.compileAssembly "AssociatedSourceType" OutputKind.ConsoleApplication [] [ guest ]

        // `FIELD STRING "EntryPoint" "0123456789"` and `FIELD TYPE "AssociatedSourceType" null`
        // are the same length.
        let bytesOf (parts : byte list list) : byte[] = parts |> List.concat |> Array.ofList

        let ascii (text : string) : byte list =
            Text.Encoding.ASCII.GetBytes text |> List.ofArray

        let entryPoint =
            bytesOf
                [
                    [ 0x53uy ; 0x0Euy ; 10uy ]
                    ascii "EntryPoint"
                    [ 10uy ]
                    ascii "0123456789"
                ]

        let associated =
            bytesOf [ [ 0x53uy ; 0x50uy ; 20uy ] ; ascii "AssociatedSourceType" ; [ 0xFFuy ] ]

        let at =
            [ 0 .. compiled.Length - entryPoint.Length ]
            |> List.filter (fun i -> compiled.AsSpan(i, entryPoint.Length).SequenceEqual (ReadOnlySpan entryPoint))
            |> List.exactlyOne

        let image = Array.copy compiled
        Array.blit associated 0 image at associated.Length

        let runtime = FrameworkUnderTest.runtime ()

        // Control compiles for Swift's convention on each runtime; Associated for it on .NET 11, and
        // not at all on .NET 10.
        let expected =
            match runtime with
            | EmulatedRuntime.Net10 -> 12
            | EmulatedRuntime.Net11 -> 11

        match RealRuntime.executeWithRealRuntime [||] image with
        | RealRuntimeResult.NormalExit code when code = expected -> ()
        | other -> failwith $"On %A{runtime}, the guest ends %A{other}, not exiting %d{expected}"

        let _, loggerFactory = LoggerFactory.makeTest ()

        let assembly =
            Assembly.read loggerFactory (Some "AssociatedSourceType.dll") (new MemoryStream (image))

        let readings =
            [ "Control" ; "Associated" ]
            |> List.map (fun name ->
                let method = assembly.Methods.[(methodNamed assembly "Program" name).Method.Get]

                match MethodInfo.unmanagedCallersOnly method with
                | None -> failwith $"%s{name} carries no UnmanagedCallersOnly"
                | Some application ->
                    UnmanagedCallingConvention.ofCallersOnly runtime application
                    |> UnmanagedCallingConvention.isSwift (hostTarget ())
            )

        let associatedIsSwift =
            match runtime with
            | EmulatedRuntime.Net10 -> false
            | EmulatedRuntime.Net11 -> true

        readings |> shouldEqual [ true ; associatedIsSwift ]

        let analysis = analysisOver [ assembly ] id

        let _, sources =
            EscapeAnalysis.unknownSources analysis (methodNamed assembly "Program" "Associated")

        let unreadable =
            sources
            |> Seq.map (renderSite analysis)
            |> Seq.filter (fun site -> site.StartsWith "Program::Associated entry")
            |> List.ofSeq

        let expectedSites =
            match runtime with
            | EmulatedRuntime.Net10 -> [ "Program::Associated entry UnreadableCallingConvention" ]
            | EmulatedRuntime.Net11 -> []

        unreadable |> shouldEqual expectedSites

    /// The bytes of a `SerString`: the null sentinel, or a `PackedLen` and the UTF-8 bytes.
    let private serString (text : string option) : byte list =
        match text with
        | None -> [ 0xFFuy ]
        | Some text ->
            let bytes = Text.Encoding.UTF8.GetBytes text |> List.ofArray

            let length =
                match bytes.Length with
                | n when n < 0x80 -> [ byte n ]
                | n when n < 0x4000 -> [ byte (0x80 ||| (n >>> 8)) ; byte (n &&& 0xFF) ]
                | n -> failwith $"No generated SerString is %d{n} bytes long"

            length @ bytes

    /// The little-endian bytes of a 32-bit count.
    let private count32 (count : int) : byte list =
        BitConverter.GetBytes count |> List.ofArray

    /// .NET 11's VM refuses a named argument its attribute declares where the blob names it twice
    /// (`META_E_CA_REPEATED_ARG`, which the host's .NET 10 shows for `CallConvs` and `EntryPoint`),
    /// `AssociatedSourceType` among them. C# cannot spell that blob, and the in-process host cannot
    /// run .NET 11, so this holds the parse to the rule rather than to a measurement.
    [<Test>]
    let ``an UnmanagedCallersOnly blob naming AssociatedSourceType twice does not parse`` () : unit =
        let associated =
            [ 0x53uy ; 0x50uy ] @ serString (Some "AssociatedSourceType") @ serString None

        let blob (count : int) =
            [ 0x01uy ; 0x00uy ; byte count ; 0x00uy ]
            @ List.concat (List.replicate count associated)
            |> ImmutableArray.CreateRange

        let once = CustomAttribute.parseUnmanagedCallersOnly (blob 1)

        once
        |> shouldEqual (UnmanagedCallersOnlyApplication.Parsed (UnmanagedCallersOnlyCallConvs.Unnamed, true))

        match CustomAttribute.parseUnmanagedCallersOnly (blob 2) with
        | UnmanagedCallersOnlyApplication.Malformed _ -> ()
        | other -> failwith $"Naming AssociatedSourceType twice parses as %A{other}"

    /// Serialised type names an `[UnmanagedCallersOnly]` blob may list in its `CallConvs`, some of
    /// which CoreCLR recognises and some of which it does not.
    let private callConvNames : string option list =
        let ns = "System.Runtime.CompilerServices."

        let qualified =
            ", System.Runtime, Version=10.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a"

        [
            for name in
                [
                    "CallConvSwift"
                    "CallConvCdecl"
                    "CallConvStdcall"
                    "CallConvThiscall"
                    "CallConvFastcall"
                    "CallConvSuppressGCTransition"
                    "CallConvMemberFunction"
                ] do
                yield Some (ns + name)
                yield Some (ns + name + qualified)

            yield Some (ns + "CallConvSwift,Nowhere")
            yield Some (ns + "CallConvSwift ,Nowhere")
            yield Some (ns + "CallConvSwiftX")
            yield Some (ns + "CallConvSwif")
            yield Some (ns + "CallConvSwift+Nested")
            yield Some ("System.Runtime.InteropServices.CallConvSwift" + qualified)
            yield Some "CallConvSwift"
            yield Some ""
            yield None
        ]

    /// One named argument of a generated `[UnmanagedCallersOnly]` blob, as bytes: mostly the two
    /// the attribute declares on every runtime, sometimes `AssociatedSourceType`, which only .NET
    /// 11's declares, and sometimes one CoreCLR refuses.
    let private ucoNamedArgument : FsCheck.Gen<byte list> =
        let kind = FsCheck.FSharp.Gen.elements [ 0x53uy ; 0x54uy ]

        let callConvs =
            FsCheck.FSharp.Gen.map3
                (fun kind elements skew ->
                    [ kind ; 0x1Duy ; 0x50uy ]
                    @ serString (Some "CallConvs")
                    @ count32 (max 0 (List.length elements + skew))
                    @ List.collect serString elements
                )
                kind
                (FsCheck.FSharp.Gen.choose (0, 4)
                 |> FsCheck.FSharp.Gen.bind (fun n ->
                     FsCheck.FSharp.Gen.listOfLength n (FsCheck.FSharp.Gen.elements callConvNames)
                 ))
                (FsCheck.FSharp.Gen.frequency
                    [
                        8, FsCheck.FSharp.Gen.constant 0
                        1, FsCheck.FSharp.Gen.constant 1
                        1, FsCheck.FSharp.Gen.constant -1
                    ])

        let stringArgument (name : string option) =
            FsCheck.FSharp.Gen.map2
                (fun kind value -> [ kind ; 0x0Euy ] @ serString name @ serString value)
                kind
                (FsCheck.FSharp.Gen.elements [ Some "e" ; None ; Some "" ])

        FsCheck.FSharp.Gen.frequency
            [
                8, callConvs
                4, stringArgument (Some "EntryPoint")
                1, stringArgument (Some "EntryPoinx")
                1, stringArgument (Some "AssociatedSourceType")
                1,
                FsCheck.FSharp.Gen.map2
                    (fun kind value -> [ kind ; 0x50uy ] @ serString (Some "AssociatedSourceType") @ serString value)
                    kind
                    (FsCheck.FSharp.Gen.elements [ Some "System.Int32" ; None ])
                1, stringArgument (Some "CallConvs")
                1, stringArgument None
                1, stringArgument (Some "")
                1,
                kind
                |> FsCheck.FSharp.Gen.map (fun kind ->
                    [ kind ; 0x1Duy ; 0x0Euy ] @ serString (Some "CallConvs") @ count32 0
                )
                1,
                kind
                |> FsCheck.FSharp.Gen.map (fun kind ->
                    [ kind ; 0x1Duy ; 0x51uy ] @ serString (Some "CallConvs") @ count32 0
                )
                1,
                FsCheck.FSharp.Gen.constant ([ 0x52uy ; 0x0Euy ] @ serString (Some "EntryPoint") @ serString (Some "e"))
            ]

    /// A generated `[UnmanagedCallersOnly]` blob: a prolog, a count of named arguments, and the
    /// arguments, each sometimes wrong, and sometimes cut short. None holds four `0xFF` bytes in a
    /// row, so that no reading of it finds the null array's length, whose compilation kills the
    /// process.
    let private ucoBlob : FsCheck.Gen<byte[]> =
        let blob =
            FsCheck.FSharp.Gen.map4
                (fun prolog arguments countForm cut ->
                    let count = List.length arguments

                    let countBytes =
                        match countForm with
                        | 0 when count = 0 -> []
                        | 1 -> [ 0x00uy ; 0x80uy ]
                        | 2 -> [ byte (count + 1) ; 0x00uy ]
                        | _ -> [ byte count ; 0x00uy ]

                    let whole = prolog @ countBytes @ List.concat arguments
                    let kept = max 0 (List.length whole - cut)
                    List.truncate kept whole |> Array.ofList
                )
                (FsCheck.FSharp.Gen.frequency
                    [
                        12, FsCheck.FSharp.Gen.constant [ 0x01uy ; 0x00uy ]
                        1, FsCheck.FSharp.Gen.constant [ 0x02uy ; 0x00uy ]
                    ])
                (FsCheck.FSharp.Gen.choose (0, 3)
                 |> FsCheck.FSharp.Gen.bind (fun n -> FsCheck.FSharp.Gen.listOfLength n ucoNamedArgument))
                (FsCheck.FSharp.Gen.frequency
                    [
                        8, FsCheck.FSharp.Gen.constant 3
                        2, FsCheck.FSharp.Gen.constant 0
                        1, FsCheck.FSharp.Gen.constant 1
                        1, FsCheck.FSharp.Gen.constant 2
                    ])
                (FsCheck.FSharp.Gen.frequency
                    [ 8, FsCheck.FSharp.Gen.constant 0 ; 2, FsCheck.FSharp.Gen.choose (1, 12) ])

        let hasNullLength (bytes : byte[]) : bool =
            bytes |> Array.windowed 4 |> Array.exists (Array.forall (fun b -> b = 0xFFuy))

        blob |> FsCheck.FSharp.Gen.filter (hasNullLength >> not)

    /// The blobs of each application of `[UnmanagedCallersOnly]` to one method: mostly one, and
    /// sometimes two, of which CoreCLR reads the first.
    let private ucoApplications : FsCheck.Gen<byte[] list> =
        FsCheck.FSharp.Gen.frequency
            [
                8, FsCheck.FSharp.Gen.map List.singleton ucoBlob
                1, FsCheck.FSharp.Gen.listOfLength 2 ucoBlob
            ]

    /// An assembly `Client` whose type `Uco` has a static `[UnmanagedCallersOnly]` method `M<i>`
    /// for each list of blobs, applying the attribute once with each blob in order, and taking a
    /// pointer to `goneStruct`.
    let private ucoMethods (goneStruct : Type) (blobs : byte[] list list) : byte[] =
        let builder =
            Emit.PersistedAssemblyBuilder (AssemblyName "Client", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "Client"

        let uco =
            modul.DefineType ("Uco", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        let attribute =
            typeof<Runtime.InteropServices.UnmanagedCallersOnlyAttribute>.GetConstructor Type.EmptyTypes

        blobs
        |> List.iteri (fun i applications ->
            let method =
                uco.DefineMethod (
                    $"M%d{i}",
                    MethodAttributes.Public ||| MethodAttributes.Static,
                    typeof<Void>,
                    [| goneStruct.MakePointerType () |]
                )

            for blob in applications do
                method.SetCustomAttribute (attribute, blob)

            method.GetILGenerator().Emit Emit.OpCodes.Ret
        )

        uco.CreateType () |> ignore<Type>
        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    /// For generated `[UnmanagedCallersOnly]` blobs, well-formed and not, sometimes two on one method,
    /// of which CoreCLR reads the first, on a method taking a
    /// pointer to a type missing at run time, the escape analysis agrees with the host's JIT: a
    /// `TypeLoadException` exactly where CoreCLR reads Swift's convention from the blob, an unknown
    /// answer from the method's entry exactly where CoreCLR cannot read the blob (and raises
    /// the exception of the failure's `HRESULT`: `COMException` for most, `OverflowException` where a
    /// string runs past the blob's end, `ArgumentException` for a bad length), and nothing otherwise.
    [<Test>]
    let ``the escape analysis reads an UnmanagedCallersOnly blob's conventions as the host's JIT does`` () : unit =
        let complete = ucoProvider true
        let incomplete = ucoProvider false

        let context =
            Runtime.Loader.AssemblyLoadContext ("UnmanagedCallersOnlyBlobs", isCollectible = true)

        try
            let goneStruct =
                (context.LoadFromStream (new MemoryStream (complete))).GetType "Provider.GoneStruct"

            // What compiling a method raises where CoreCLR cannot parse its attribute's blob.
            let unreadable =
                Set.ofList
                    [
                        "System.Runtime.InteropServices.COMException"
                        "System.OverflowException"
                        "System.ArgumentException"
                    ]

            let property (blobs : byte[] list list) : unit =
                let image = ucoMethods goneStruct blobs
                let names = blobs |> List.mapi (fun i _ -> $"M%d{i}")
                let host = ucoCompilations image incomplete "Uco" names
                let analysed = ucoAnalysed image incomplete "Uco" names

                let failures =
                    [
                        for i, blob in List.indexed blobs do
                            let name = $"M%d{i}"
                            let outcome, sources = analysed.[name]
                            let entry = $"Uco::%s{name} entry "

                            // Making a `TypeLoadException` runs code whose answer is itself
                            // unknown, so only the method's own entry says whether the analysis
                            // read the blob.
                            let agrees =
                                match host.[name], outcome with
                                | UcoCompilation.Compiled, UcoCompilation.Compiled -> sources.IsEmpty
                                | UcoCompilation.Raises "System.TypeLoadException",
                                  UcoCompilation.Raises "System.TypeLoadException" ->
                                    not (sources |> Set.exists (fun site -> site.StartsWith entry))
                                | UcoCompilation.Raises raised, UcoCompilation.Raises "Unknown" when
                                    unreadable.Contains raised
                                    ->
                                    sources = Set.singleton (entry + "UnreadableCallingConvention")
                                | _ -> false

                            if not agrees then
                                let shown =
                                    blob
                                    |> List.map (Array.map (sprintf "%02X") >> String.concat " ")
                                    |> String.concat "] ["

                                yield
                                    $"[%s{shown}]: the host's JIT %A{host.[name]}, the analysis %A{outcome} unknown at %A{Set.toList sources}"
                    ]

                if not failures.IsEmpty then
                    failures |> String.concat Environment.NewLine |> failwith

            FsCheck.Check.One (
                FsCheck.Config.QuickThrowOnFailure.WithMaxTest 40,
                FsCheck.FSharp.Prop.forAll
                    (FsCheck.FSharp.Arb.fromGen (
                        FsCheck.FSharp.Gen.choose (1, 10)
                        |> FsCheck.FSharp.Gen.bind (fun n -> FsCheck.FSharp.Gen.listOfLength n ucoApplications)
                    ))
                    property
            )
        finally
            context.Unload ()

    /// What running a method of `Client.Calls` does, each of which makes one call inside a `catch`
    /// of every exception.
    [<RequireQualifiedAccess>]
    type private CallOutcome =
        /// Nothing fails to load.
        | Runs
        /// Compiling the method called fails with the exception of this name, which the caller's
        /// `catch` stops.
        | Caught of exceptionName : string
        /// Compiling the caller itself fails with the exception of this name, past its `catch`.
        | Escapes of exceptionName : string

    /// A client compiled against one version of a provider, run against another lacking a type
    /// that a method the client calls names in its signature, or derives from or holds. The JIT
    /// compiling a caller loads in full the return type of each method it calls, and a parameter's
    /// type only if it is a value type; a parameter of a class type loads where the callee is
    /// compiled, which the caller's `catch` stops. Each caller makes one call, of a method of its
    /// own assembly, of another assembly compiled against the complete provider, or through a
    /// function pointer, and names the type in no local and nowhere in its own signature. The
    /// host's CoreCLR, running each caller against the same assemblies, says which fail, and where.
    [<Test>]
    let ``a caller's JIT loads the return type of a method it calls, and a parameter's only where it is a value type``
        ()
        : unit
        =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let provider (complete : bool) : string =
            "namespace Provider; public class Kept { }"
            + (if complete then
                   " public class Gone { } public struct GoneStruct { }"
               else
                   "")

        let middle =
            """
namespace Middle;

public class DerivesFromGone : Provider.Gone { }

public static class Api
{
    public static void TakesGone(Provider.Gone x) { }
    public static void TakesDerivesFromGone(DerivesFromGone x) { }
    public static Provider.Gone ReturnsGone() => null;
    public static DerivesFromGone ReturnsDerivesFromGone() => null;
}

public class Instance
{
    public static Instance Make() => new Instance();
    public virtual DerivesFromGone ReturnsDerivesFromGone() => null;
    public virtual void TakesDerivesFromGone(DerivesFromGone x) { }
}
"""

        let client =
            """
using System.Collections.Generic;

namespace Client;

public class DerivesFromKept : Provider.Kept { }
public class DerivesFromGone : Provider.Gone { }
public struct HoldsGone { public Provider.GoneStruct G; }
public class ConstructedFromGone { public ConstructedFromGone(Provider.Gone x) { } }

public static unsafe class Callees
{
    public static void TakesGone(Provider.Gone x) { }
    public static void TakesDerivesFromGone(DerivesFromGone x) { }
    public static void TakesArrayOfHoldsGone(HoldsGone[] x) { }
    public static void TakesPointerToGoneStruct(Provider.GoneStruct* x) { }
    public static DerivesFromKept ReturnsDerivesFromKept() => null;
    public static Provider.Gone ReturnsGone() => null;
    public static DerivesFromGone ReturnsDerivesFromGone() => null;
    public static List<DerivesFromGone> ReturnsListOfDerivesFromGone() => null;
    public static Provider.GoneStruct* ReturnsPointerToGoneStruct() => null;
}

public static unsafe class Calls
{
    static bool never;

    public static string TakesGone()
    {
        try { Callees.TakesGone(null); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string TakesDerivesFromGone()
    {
        try { Callees.TakesDerivesFromGone(null); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string TakesArrayOfHoldsGone()
    {
        try { Callees.TakesArrayOfHoldsGone(null); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string TakesPointerToGoneStruct()
    {
        try { Callees.TakesPointerToGoneStruct(null); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string ConstructsFromGone()
    {
        try { new ConstructedFromGone(null); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string ReturnsDerivesFromKept()
    {
        try { Callees.ReturnsDerivesFromKept(); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string ReturnsGone()
    {
        try { Callees.ReturnsGone(); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string ReturnsDerivesFromGone()
    {
        try { Callees.ReturnsDerivesFromGone(); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string ReturnsListOfDerivesFromGone()
    {
        try { Callees.ReturnsListOfDerivesFromGone(); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string ReturnsPointerToGoneStruct()
    {
        try { Callees.ReturnsPointerToGoneStruct(); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string ElsewhereTakesGone()
    {
        try { Middle.Api.TakesGone(null); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string ElsewhereTakesDerivesFromGone()
    {
        try { Middle.Api.TakesDerivesFromGone(null); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string ElsewhereReturnsGone()
    {
        try { Middle.Api.ReturnsGone(); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string ElsewhereReturnsDerivesFromGone()
    {
        try { Middle.Api.ReturnsDerivesFromGone(); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string ElsewhereVirtuallyReturnsDerivesFromGone()
    {
        try { Middle.Instance.Make().ReturnsDerivesFromGone(); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string ElsewhereVirtuallyTakesDerivesFromGone()
    {
        try { Middle.Instance.Make().TakesDerivesFromGone(null); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string IndirectlyTakesGone()
    {
        try { delegate*<Provider.Gone, void> f = null; if (never) f(null); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string IndirectlyReturnsDerivesFromGone()
    {
        try { delegate*<DerivesFromGone> f = null; if (never) f(); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
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

        let completeProvider = compile "Provider" [] (provider true)
        let incompleteProvider = compile "Provider" [] (provider false)
        let middleImage = compile "Middle" [ completeProvider ] middle
        let clientImage = compile "Client" [ completeProvider ; middleImage ] client

        let typeLoad = "System.TypeLoadException"
        let missingMethod = "System.MissingMethodException"

        // Each caller, the name of a type the signature of the method it calls spells, and what
        // running it does.
        let methods =
            [
                "TakesGone", "Gone", CallOutcome.Caught typeLoad
                "TakesDerivesFromGone", "DerivesFromGone", CallOutcome.Caught typeLoad
                "TakesArrayOfHoldsGone", "HoldsGone", CallOutcome.Caught typeLoad
                "TakesPointerToGoneStruct", "GoneStruct", CallOutcome.Runs
                "ConstructsFromGone", "Gone", CallOutcome.Caught typeLoad
                "ReturnsDerivesFromKept", "DerivesFromKept", CallOutcome.Runs
                "ReturnsGone", "Gone", CallOutcome.Escapes typeLoad
                "ReturnsDerivesFromGone", "DerivesFromGone", CallOutcome.Escapes typeLoad
                "ReturnsListOfDerivesFromGone", "DerivesFromGone", CallOutcome.Escapes typeLoad
                "ReturnsPointerToGoneStruct", "GoneStruct", CallOutcome.Escapes typeLoad
                "ElsewhereTakesGone", "Gone", CallOutcome.Escapes missingMethod
                "ElsewhereTakesDerivesFromGone", "DerivesFromGone", CallOutcome.Caught typeLoad
                "ElsewhereReturnsGone", "Gone", CallOutcome.Escapes missingMethod
                "ElsewhereReturnsDerivesFromGone", "DerivesFromGone", CallOutcome.Escapes typeLoad
                "ElsewhereVirtuallyReturnsDerivesFromGone", "DerivesFromGone", CallOutcome.Escapes typeLoad
                "ElsewhereVirtuallyTakesDerivesFromGone", "DerivesFromGone", CallOutcome.Caught typeLoad
                "IndirectlyTakesGone", "Gone", CallOutcome.Runs
                "IndirectlyReturnsDerivesFromGone", "DerivesFromGone", CallOutcome.Escapes typeLoad
            ]

        // What running each caller does on the host's CoreCLR.
        let hostOutcomes : Map<string, CallOutcome> =
            let context =
                Runtime.Loader.AssemblyLoadContext ("CalleeSignatureTypeLoads", isCollectible = true)

            context.add_Resolving (
                Func<_, _, _> (fun (context : Runtime.Loader.AssemblyLoadContext) (name : AssemblyName) ->
                    match name.Name with
                    | "Provider" -> context.LoadFromStream (new MemoryStream (incompleteProvider))
                    | "Middle" -> context.LoadFromStream (new MemoryStream (middleImage))
                    | _ -> null
                )
            )

            try
                let calls =
                    context.LoadFromStream(new MemoryStream (clientImage)).GetType "Client.Calls"

                methods
                |> List.map (fun (name, _, _) ->
                    let outcome =
                        try
                            match calls.GetMethod(name).Invoke ((null : obj), [||]) with
                            | null -> CallOutcome.Runs
                            | caught -> CallOutcome.Caught (unbox<string> caught)
                        with :? TargetInvocationException as e ->
                            CallOutcome.Escapes (e.InnerException.GetType().FullName)

                    name, outcome
                )
                |> Map.ofList
            finally
                context.Unload ()

        let read (name : string) (image : byte[]) : DumpedAssembly =
            Assembly.read loggerFactory (Some $"%s{name}.dll") (new MemoryStream (image))

        let clientAssembly = read "Client" clientImage
        let middleAssembly = read "Middle" middleImage
        let providerAssembly = read "Provider" incompleteProvider

        let referenceTo (from : DumpedAssembly) (name : string) =
            from.AssemblyReferences.Values |> Seq.find (fun r -> r.Name.Name = name)

        let mutable analysis =
            analysisOver
                [ clientAssembly ; middleAssembly ; providerAssembly ]
                (fun loaded ->
                    let loaded, _ =
                        loaded.WithBoundReference (referenceTo clientAssembly "Provider") providerAssembly

                    let loaded, _ =
                        loaded.WithBoundReference (referenceTo clientAssembly "Middle") middleAssembly

                    fst (loaded.WithBoundReference (referenceTo middleAssembly "Provider") providerAssembly)
                )

        let named = namedTypes clientAssembly

        // The runtime makes each exception by running one of its constructors, and what that raises
        // escapes in its place.
        let madeBy : Map<string, Set<string>> =
            let next, madeTypeLoad = EscapeAnalysis.escapes analysis (typeLoadConstructor ())
            analysis <- next
            let madeTypeLoad = render analysis madeTypeLoad
            let next, madeMissingMethod = making analysis missingMethod
            analysis <- next
            Map.ofList [ typeLoad, madeTypeLoad ; missingMethod, madeMissingMethod ]

        let failures =
            [
                for name, calleeType, expected in methods do
                    let key = methodNamed clientAssembly "Client.Calls" name
                    let method = clientAssembly.Methods.[key.Method.Get]

                    // Only the method called names the type: nothing the JIT loads of the caller's
                    // own signature or of its locals does.
                    let ownLoads =
                        [
                            match method.Signature.ReturnType with
                            | MethodReturnType.Void -> ()
                            | MethodReturnType.Returns returned -> yield JitTypePosition.OwnReturn, returned
                            for parameter in method.Signature.ParameterTypes do
                                yield JitTypePosition.OwnParameter, parameter
                            match method.Body with
                            | MethodBody.Il body ->
                                for local in Option.defaultValue ImmutableArray.Empty body.LocalVars do
                                    yield JitTypePosition.Local, local
                            | _ -> ()
                        ]
                        |> List.choose (fun (position, spelling) -> JitTypeLoads.load position spelling)

                    if
                        ownLoads
                        |> List.exists (fun load ->
                            match load with
                            | JitTypeLoad.InFull loaded -> List.contains calleeType (named loaded)
                            | other -> failwith $"%s{name}: the JIT loads %O{other} of its own signature or a local"
                        )
                    then
                        yield $"%s{name}: the JIT loads %s{calleeType} for its own signature or a local: %A{ownLoads}"

                    if hostOutcomes.[name] <> expected then
                        yield $"%s{name}: the host's outcome is %A{hostOutcomes.[name]}, not %A{expected}"

                    let next, escapes = EscapeAnalysis.escapes analysis key

                    analysis <- next
                    let shown = render analysis escapes

                    let escaping =
                        [ typeLoad ; missingMethod ; "System.IO.FileNotFoundException" ]
                        |> List.filter (fun exceptionName -> shown.Contains $"=%s{exceptionName}")

                    // What escapes is the exception the caller's compilation raises, and whatever
                    // making that one raises.
                    let unexplained =
                        match expected with
                        | CallOutcome.Escapes exceptionName ->
                            if not (List.contains exceptionName escaping) then
                                escaping @ [ $"no %s{exceptionName}" ]
                            else
                                escaping
                                |> List.filter (fun escaped ->
                                    escaped <> exceptionName
                                    && not (madeBy.[exceptionName].Contains $"=%s{escaped}")
                                )
                        | CallOutcome.Caught _
                        | CallOutcome.Runs -> escaping

                    if not unexplained.IsEmpty then
                        yield $"%s{name}: %A{unexplained} in %A{Set.toList shown}, unknown %b{escapes.Unknown}"
            ]

        if not failures.IsEmpty then
            failures |> String.concat Environment.NewLine |> failwith

    /// CoreCLR on Unix refuses a signature with a variable argument list, managed or native, as it
    /// converts one for the JIT (`ConvToJitSig`, `IDS_EE_VARARG_NOT_SUPPORTED`): compiling a method
    /// whose `call`, `callvirt`, `newobj`, `ldftn`, `ldvirtftn` or `calli` names such a method or
    /// call site fails with `InvalidProgramException`, past the method's handlers, as does compiling
    /// such a method itself; `ldtoken` converts no signature. Each caller makes its call in a branch
    /// never taken; the host's CoreCLR, at tier 0 and fully optimised, says which fail.
    [<Test>]
    let ``the JIT refuses a signature with a variable argument list wherever it reads one`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let provider (complete : bool) : string =
            "namespace Provider; public class Kept { }"
            + (if complete then " public struct GoneStruct { }" else "")

        let compileProvider (complete : bool) : byte[] =
            Roslyn.compileOptimizedAssembly "Provider" OutputKind.DynamicallyLinkedLibrary [ provider complete ]

        // The image with the `ldvirtftn` of `Target.Plain` in `Calls.VirtualAddressOfHoldsGone`
        // retargeted to `Target.Variadic`, whose address C# takes nowhere.
        let retargetAddress (image : byte[]) : byte[] =
            let patched = Array.copy image
            use pe = new PEReader (ImmutableArray.CreateRange image)
            let metadata = pe.GetMetadataReader ()

            let methodToken (typeName : string) (methodName : string) : int * MethodDefinition =
                metadata.TypeDefinitions
                |> Seq.map metadata.GetTypeDefinition
                |> Seq.filter (fun ty -> metadata.GetString ty.Name = typeName)
                |> Seq.exactlyOne
                |> fun ty -> ty.GetMethods ()
                |> Seq.filter (fun handle -> metadata.GetString (metadata.GetMethodDefinition handle).Name = methodName)
                |> Seq.exactlyOne
                |> fun handle ->
                    MetadataTokens.GetToken (MethodDefinitionHandle.op_Implicit handle : EntityHandle),
                    metadata.GetMethodDefinition handle

            let plain, _ = methodToken "Target" "Plain"
            let variadic, _ = methodToken "Target" "Variadic"
            let _, caller = methodToken "Calls" "VirtualAddressOfHoldsGone"
            let rva = caller.RelativeVirtualAddress

            let section =
                pe.PEHeaders.SectionHeaders
                |> Seq.find (fun s -> rva >= s.VirtualAddress && rva < s.VirtualAddress + s.VirtualSize)

            let start = rva - section.VirtualAddress + section.PointerToRawData
            let size = (pe.GetMethodBody rva).Size
            let ldvirtftn = Array.append [| 0xFEuy ; 0x07uy |] (BitConverter.GetBytes plain)

            match
                [ start .. start + size - ldvirtftn.Length ]
                |> List.filter (fun i -> patched.[i .. i + ldvirtftn.Length - 1] = ldvirtftn)
            with
            | [ at ] -> Array.blit (BitConverter.GetBytes variadic) 0 patched (at + 2) 4
            | found -> failwith $"VirtualAddressOfHoldsGone takes Plain's address %d{found.Length} times, not once"

            patched

        // C# spells a call of a method with a variable argument list, and of a P/Invoke with one.
        let source =
            """
using System.Runtime.InteropServices;

namespace Arglist;

public struct HoldsGone { public Provider.GoneStruct G; }

public class Target
{
    public Target(int a, __arglist) { }
    public virtual void Plain() { }
    public virtual void Variadic(HoldsGone h, __arglist) { }
    public Target() { }
    public static void Static(int a, __arglist) { }
    public virtual void Virtual(int a, __arglist) { }
    [DllImport("nowhere")]
    public static extern void Native(int a, __arglist);
}

public static class Calls
{
    static bool never;

    public static void Call() { if (never) Target.Static(1, __arglist(2)); }
    public static void CallCaught()
    {
        try { if (never) Target.Static(1, __arglist(2)); } catch (System.Exception) { }
    }
    public static void Callvirt() { if (never) new Target().Virtual(1, __arglist(2)); }
    public static void Newobj() { if (never) new Target(1, __arglist(2)); }
    public static void Native() { if (never) Target.Native(1, __arglist(2)); }
    // The JIT folds away a branch capability queries rule out, as no CPU has both of these, and
    // compiles no call in it.
    public static void Guarded()
    {
        if (System.Runtime.Intrinsics.X86.Sse.IsSupported && System.Runtime.Intrinsics.Arm.AdvSimd.IsSupported)
            Target.Static(1, __arglist(2));
    }
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)]
    static void Use(object o) { }
    // Retargeted to `Variadic` after compiling.
    public static void VirtualAddressOfHoldsGone()
    {
        if (never) Use(new System.Action(new Target().Plain));
    }
    public static void NativeCaught()
    {
        try { if (never) Target.Native(1, __arglist(2)); } catch (System.Exception) { }
    }
}
"""

        let arglistImage =
            Roslyn.compileOptimizedAssemblyWithReferences
                "Arglist"
                OutputKind.DynamicallyLinkedLibrary
                [
                    MetadataReference.CreateFromImage (ImmutableArray.CreateRange (compileProvider true))
                    :> MetadataReference
                ]
                [ source ]
            |> retargetAddress

        let incompleteProvider = compileProvider false

        // C# spells no `ldftn`, `ldvirtftn`, `calli` or `ldtoken` of one, so this emits them, each
        // in a branch never taken.
        let emittedImage =
            let builder =
                Emit.PersistedAssemblyBuilder (AssemblyName "Varargs", typeof<obj>.Assembly)

            let modul = builder.DefineDynamicModule "Varargs"

            let targets =
                modul.DefineType ("Varargs.Targets", TypeAttributes.Public ||| TypeAttributes.Class)

            let never =
                targets.DefineField ("Never", typeof<bool>, FieldAttributes.Public ||| FieldAttributes.Static)

            let staticTarget =
                targets.DefineMethod (
                    "Static",
                    MethodAttributes.Public ||| MethodAttributes.Static,
                    CallingConventions.VarArgs,
                    typeof<Void>,
                    [| typeof<int> |]
                )

            staticTarget.GetILGenerator().Emit Emit.OpCodes.Ret

            let virtualTarget =
                targets.DefineMethod (
                    "Virtual",
                    MethodAttributes.Public
                    ||| MethodAttributes.Virtual
                    ||| MethodAttributes.NewSlot,
                    CallingConventions.VarArgs ||| CallingConventions.HasThis,
                    typeof<Void>,
                    [| typeof<int> |]
                )

            virtualTarget.GetILGenerator().Emit Emit.OpCodes.Ret

            let constructor = targets.DefineDefaultConstructor MethodAttributes.Public

            let caller (name : string) (caught : bool) (body : Emit.ILGenerator -> unit) : unit =
                let method =
                    targets.DefineMethod (
                        name,
                        MethodAttributes.Public ||| MethodAttributes.Static,
                        typeof<Void>,
                        Type.EmptyTypes
                    )

                let il = method.GetILGenerator ()
                let skip = il.DefineLabel ()
                il.Emit (Emit.OpCodes.Ldsfld, never)
                il.Emit (Emit.OpCodes.Brfalse, skip)

                if caught then
                    il.BeginExceptionBlock () |> ignore<Emit.Label>

                body il

                if caught then
                    il.BeginCatchBlock typeof<Exception>
                    il.Emit Emit.OpCodes.Pop
                    il.EndExceptionBlock ()

                il.MarkLabel skip
                il.Emit Emit.OpCodes.Ret

            let functionPointer (il : Emit.ILGenerator) : unit =
                il.Emit Emit.OpCodes.Ldc_I4_1
                il.Emit Emit.OpCodes.Ldc_I4_0
                il.Emit Emit.OpCodes.Conv_I

                il.EmitCalli (Emit.OpCodes.Calli, CallingConventions.VarArgs, typeof<Void>, [| typeof<int> |], null)

            caller
                "Address"
                false
                (fun il ->
                    il.Emit (Emit.OpCodes.Ldftn, staticTarget)
                    il.Emit Emit.OpCodes.Pop
                )

            caller
                "VirtualAddress"
                false
                (fun il ->
                    il.Emit (Emit.OpCodes.Newobj, constructor)
                    il.Emit (Emit.OpCodes.Ldvirtftn, virtualTarget)
                    il.Emit Emit.OpCodes.Pop
                )

            caller "Indirect" false functionPointer
            caller "IndirectCaught" true functionPointer

            let abstractType =
                modul.DefineType (
                    "Varargs.Abstract",
                    TypeAttributes.Public ||| TypeAttributes.Class ||| TypeAttributes.Abstract
                )

            let abstractTarget =
                abstractType.DefineMethod (
                    "Target",
                    MethodAttributes.Public
                    ||| MethodAttributes.Virtual
                    ||| MethodAttributes.NewSlot
                    ||| MethodAttributes.Abstract,
                    CallingConventions.VarArgs ||| CallingConventions.HasThis,
                    typeof<Void>,
                    [| typeof<int> |]
                )

            abstractType.CreateType () |> ignore<Type>

            caller
                "AbstractAddress"
                false
                (fun il ->
                    il.Emit (Emit.OpCodes.Ldftn, abstractTarget)
                    il.Emit Emit.OpCodes.Pop
                )

            caller
                "AbstractVirtualAddress"
                false
                (fun il ->
                    il.Emit Emit.OpCodes.Ldnull
                    il.Emit (Emit.OpCodes.Ldvirtftn, abstractTarget)
                    il.Emit Emit.OpCodes.Pop
                )

            caller
                "Token"
                false
                (fun il ->
                    il.Emit (Emit.OpCodes.Ldtoken, staticTarget)
                    il.Emit Emit.OpCodes.Pop
                )

            targets.CreateType () |> ignore<Type>
            use stream = new MemoryStream ()
            builder.Save stream
            stream.ToArray ()

        let invalidProgram = "System.InvalidProgramException"

        // Each method, by its assembly's name and type, and its name; whether the host runs it as a
        // caller or prepares it as a method compiled by itself; and the exception compiling it fails
        // with, if any.
        let methods =
            [
                "Arglist", "Arglist.Calls", "Call", false, Some invalidProgram
                "Arglist", "Arglist.Calls", "CallCaught", false, Some invalidProgram
                "Arglist", "Arglist.Calls", "Callvirt", false, Some invalidProgram
                "Arglist", "Arglist.Calls", "Newobj", false, Some invalidProgram
                "Arglist", "Arglist.Calls", "Native", false, Some invalidProgram
                "Arglist", "Arglist.Calls", "NativeCaught", false, Some invalidProgram
                "Arglist", "Arglist.Calls", "Guarded", false, None
                // CoreCLR prepares the method as a function pointer, loading the value types of its
                // signature, before it converts the signature.
                "Arglist", "Arglist.Calls", "VirtualAddressOfHoldsGone", false, Some "System.TypeLoadException"
                "Arglist", "Arglist.Target", "Static", true, Some invalidProgram
                "Varargs", "Varargs.Targets", "Address", false, Some invalidProgram
                "Varargs", "Varargs.Targets", "VirtualAddress", false, Some invalidProgram
                "Varargs", "Varargs.Targets", "Indirect", false, Some invalidProgram
                "Varargs", "Varargs.Targets", "IndirectCaught", false, Some invalidProgram
                "Varargs", "Varargs.Targets", "Token", false, None
                // Resolving a call of an abstract method itself, CoreCLR refuses it first.
                "Varargs", "Varargs.Targets", "AbstractAddress", false, Some "System.BadImageFormatException"
                "Varargs", "Varargs.Targets", "AbstractVirtualAddress", false, Some invalidProgram
                "Varargs", "Varargs.Targets", "Static", true, Some invalidProgram
            ]

        let images = Map.ofList [ "Arglist", arglistImage ; "Varargs", emittedImage ]

        // What compiling each method does on the host's CoreCLR, at tier 0 and fully optimised:
        // the name of the exception, if it fails.
        let hostOutcomes (collectible : bool) : Map<string * string, string option> =
            let context =
                Runtime.Loader.AssemblyLoadContext ($"Varargs%b{collectible}", isCollectible = collectible)

            context.add_Resolving (
                Func<_, _, _> (fun (context : Runtime.Loader.AssemblyLoadContext) (name : AssemblyName) ->
                    match name.Name with
                    | "Provider" -> context.LoadFromStream (new MemoryStream (incompleteProvider))
                    | _ -> null
                )
            )

            try
                let loaded =
                    images
                    |> Map.map (fun _ image -> context.LoadFromStream (new MemoryStream (image)))

                methods
                |> List.map (fun (assemblyName, typeName, name, prepared, _) ->
                    let method = loaded.[assemblyName].GetType(typeName).GetMethod name

                    let outcome =
                        if prepared then
                            try
                                Runtime.CompilerServices.RuntimeHelpers.PrepareMethod method.MethodHandle
                                None
                            with e ->
                                Some (e.GetType().FullName)
                        else
                            try
                                method.Invoke ((null : obj), [||]) |> ignore<obj>
                                None
                            with :? TargetInvocationException as e ->
                                Some (e.InnerException.GetType().FullName)

                    (typeName, name), outcome
                )
                |> Map.ofList
            finally
                if collectible then
                    context.Unload ()

        let host = [ hostOutcomes false ; hostOutcomes true ]

        let read (name : string) (image : byte[]) : DumpedAssembly =
            Assembly.read loggerFactory (Some $"%s{name}.dll") (new MemoryStream (image))

        let assemblies = images |> Map.map read

        let providerAssembly = read "Provider" incompleteProvider

        let reference =
            assemblies.["Arglist"].AssemblyReferences.Values
            |> Seq.find (fun r -> r.Name.Name = "Provider")

        let mutable analysis =
            analysisOver
                (providerAssembly :: (assemblies |> Map.toList |> List.map snd))
                (fun loaded -> fst (loaded.WithBoundReference reference providerAssembly))

        // The runtime makes each exception by running one of its constructors, and what that raises
        // escapes in its place.
        let madeBy : Map<string, Set<string>> =
            let next, madeTypeLoad = EscapeAnalysis.escapes analysis (typeLoadConstructor ())
            analysis <- next
            let madeTypeLoad = render analysis madeTypeLoad
            let next, madeInvalidProgram = making analysis invalidProgram
            analysis <- next
            let next, madeBadImage = making analysis "System.BadImageFormatException"
            analysis <- next

            Map.ofList
                [
                    "System.TypeLoadException", madeTypeLoad
                    invalidProgram, madeInvalidProgram
                    "System.BadImageFormatException", madeBadImage
                ]

        let failures =
            [
                for assemblyName, typeName, name, _, expected in methods do

                    for outcomes in host do
                        if outcomes.[typeName, name] <> expected then
                            yield
                                $"%s{typeName}.%s{name}: the host's outcome is %A{outcomes.[typeName, name]}, not %A{expected}"

                    let next, escapes =
                        EscapeAnalysis.escapes analysis (methodNamed assemblies.[assemblyName] typeName name)

                    analysis <- next
                    let shown = render analysis escapes

                    let escaping =
                        [
                            invalidProgram
                            "System.BadImageFormatException"
                            "System.TypeLoadException"
                        ]
                        |> List.filter (fun exceptionName -> shown.Contains $"=%s{exceptionName}")

                    let unexplained =
                        match expected with
                        | None -> escaping
                        | Some expected when not (List.contains expected escaping) -> escaping @ [ $"no %s{expected}" ]
                        | Some expected ->
                            escaping
                            |> List.filter (fun escaped ->
                                escaped <> expected && not (madeBy.[expected].Contains $"=%s{escaped}")
                            )

                    if not unexplained.IsEmpty then
                        yield
                            $"%s{typeName}.%s{name}: %A{unexplained} in the analysis, in %A{Set.toList shown}, unknown %b{escapes.Unknown}"

                    // Nothing of an instruction CoreCLR refuses runs, so nothing it would call is
                    // unknown.
                    if expected = Some invalidProgram && escapes.Unknown then
                        yield
                            $"%s{typeName}.%s{name}: unknown in the analysis, from %A{snd (EscapeAnalysis.unknownSources analysis (methodNamed assemblies.[assemblyName] typeName name))}"
            ]

        if not failures.IsEmpty then
            failures |> String.concat Environment.NewLine |> failwith

    /// A client compiled against one version of a provider, run against another lacking two types
    /// and without the assembly `Optional`, each of whose callers calls unmanaged code (a P/Invoke,
    /// or `calli` by an unmanaged calling convention) whose signature names a type that fails to
    /// load. The JIT loads the return type and each value-type parameter, as of any call. Where it
    /// makes the transition to unmanaged code in the caller's own code, CoreCLR's walk for whether
    /// the call needs marshalling loads each instantiation the walk reaches, and Swift's convention
    /// loads each pointer's target; a class, a byref's target or, by any other convention, a
    /// pointer's target, it does not load at all. Whether the JIT makes the transition there the
    /// analysis does not decide, so it takes those loads to be ones the JIT may make
    /// (`JitTypeLoad.Possibly`), which do not stop it making the loads after them. Each call is in
    /// a branch never taken, so nothing runs the stub that CoreCLR otherwise makes for the call,
    /// which loads the rest of the signature. The host's CoreCLR says which callers fail to compile.
    [<Test>]
    let ``a call of unmanaged code loads what marshalling and Swift's convention read of its signature`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let provider (complete : bool) : string =
            "namespace Provider; public class KeptGeneric<T> { }"
            + (if complete then
                   " public class Gone { } public struct GoneStruct { }"
               else
                   "")

        let optional = "namespace Optional; public class Thing { }"

        let client =
            """
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace Client;

public static unsafe class Natives
{
    [DllImport("nowhere")]
    public static extern void TakesPointerToGoneStruct(Provider.GoneStruct* x);
    [DllImport("nowhere")]
    public static extern void TakesGone(Provider.Gone x);
    [DllImport("nowhere")]
    public static extern void TakesGenericOfGone(Provider.KeptGeneric<Provider.Gone> x);
    [DllImport("nowhere")]
    public static extern void TakesPointerThenGenericOfMissing(Provider.GoneStruct* x, Provider.KeptGeneric<Optional.Thing> y);
    [DllImport("nowhere")]
    public static extern Provider.Gone ReturnsGone();
    [DllImport("nowhere"), UnmanagedCallConv(CallConvs = new[] { typeof(CallConvSwift) })]
    public static extern void SwiftTakesPointerToGoneStruct(Provider.GoneStruct* x);
    [DllImport("nowhere"), UnmanagedCallConv(CallConvs = new[] { typeof(CallConvSwift) })]
    public static extern void SwiftTakesByrefToGoneStruct(ref Provider.GoneStruct x);
    [DllImport("nowhere", CallingConvention = CallingConvention.Cdecl), UnmanagedCallConv(CallConvs = new[] { typeof(CallConvSwift) })]
    public static extern void CdeclTakesPointerToGoneStruct(Provider.GoneStruct* x);
    // Its import's calling-convention bits are cleared after compiling.
    [DllImport("nowhere", CallingConvention = CallingConvention.Cdecl), UnmanagedCallConv(CallConvs = new[] { typeof(CallConvSwift) })]
    public static extern void UnspecifiedTakesPointerToGoneStruct(Provider.GoneStruct* x);
}

public static unsafe class Calls
{
    static bool never;
    static void* pointer;

    public static void PointerToGoneStruct() { if (never) Natives.TakesPointerToGoneStruct(null); }
    public static void Gone() { if (never) Natives.TakesGone(null); }
    public static void GenericOfGone() { if (never) Natives.TakesGenericOfGone(null); }
    public static void PointerThenGenericOfMissing() { if (never) Natives.TakesPointerThenGenericOfMissing(null, null); }
    public static void ReturnsGone() { if (never) Natives.ReturnsGone(); }
    public static void SwiftPointerToGoneStruct() { if (never) Natives.SwiftTakesPointerToGoneStruct(null); }
    public static void SwiftByrefToGoneStruct() { if (never) Natives.SwiftTakesByrefToGoneStruct(ref *(Provider.GoneStruct*)null); }
    public static void CdeclPointerToGoneStruct() { if (never) Natives.CdeclTakesPointerToGoneStruct(null); }
    public static void UnspecifiedPointerToGoneStruct() { if (never) Natives.UnspecifiedTakesPointerToGoneStruct(null); }
    public static void CaughtGenericOfGone()
    {
        try { if (never) Natives.TakesGenericOfGone(null); } catch (System.Exception) { }
    }
    public static void IndirectPointerToGoneStruct()
    {
        if (never) ((delegate* unmanaged<Provider.GoneStruct*, void>)pointer)(null);
    }
    public static void IndirectGenericOfGone()
    {
        if (never) ((delegate* unmanaged<Provider.KeptGeneric<Provider.Gone>, void>)pointer)(null);
    }
    public static void SwiftIndirectPointerToGoneStruct()
    {
        if (never) ((delegate* unmanaged[Swift]<Provider.GoneStruct*, void>)pointer)(null);
    }
}
"""

        // Optimised: the JIT never makes the transition in the caller's code where the assembly's
        // `DebuggableAttribute` disables its optimisation.
        let compile (name : string) (references : byte[] list) (text : string) : byte[] =
            Roslyn.compileOptimizedAssemblyWithReferences
                name
                OutputKind.DynamicallyLinkedLibrary
                (references
                 |> List.map (fun image ->
                     MetadataReference.CreateFromImage (ImmutableArray.CreateRange image) :> MetadataReference
                 ))
                [ text ]

        let completeProvider = compile "Provider" [] (provider true)
        let incompleteProvider = compile "Provider" [] (provider false)
        let optionalImage = compile "Optional" [] optional
        // The image with the calling-convention bits of the P/Invoke named `methodName`'s import
        // cleared, which C# never emits and CoreCLR reads as `WinApi`.
        let clearImportConvention (methodName : string) (image : byte[]) : byte[] =
            let patched = Array.copy image
            use pe = new PEReader (ImmutableArray.CreateRange image)
            let metadata = pe.GetMetadataReader ()

            let method =
                metadata.MethodDefinitions
                |> Seq.filter (fun handle -> metadata.GetString (metadata.GetMethodDefinition handle).Name = methodName)
                |> Seq.exactlyOne

            // A small image's `MemberForwarded` is a two-byte coded index whose tag 1 is a method.
            let memberForwarded =
                (MetadataTokens.GetRowNumber (MethodDefinitionHandle.op_Implicit method : EntityHandle)
                 <<< 1)
                ||| 1

            let rowSize = metadata.GetTableRowSize TableIndex.ImplMap

            let tableStart =
                pe.PEHeaders.MetadataStartOffset
                + metadata.GetTableMetadataOffset TableIndex.ImplMap

            let row =
                [ 0 .. metadata.GetTableRowCount TableIndex.ImplMap - 1 ]
                |> List.filter (fun row ->
                    int (BitConverter.ToUInt16 (patched, tableStart + row * rowSize + 2)) = memberForwarded
                )
                |> List.exactlyOne

            let offset = tableStart + row * rowSize
            let flags = BitConverter.ToUInt16 (patched, offset)
            let cleared = flags &&& ~~~(uint16 MethodImportAttributes.CallingConventionMask)
            Array.blit (BitConverter.GetBytes cleared) 0 patched offset 2
            patched

        let clientImage =
            compile "Client" [ completeProvider ; optionalImage ] client
            |> clearImportConvention "UnspecifiedTakesPointerToGoneStruct"

        let typeLoad = "System.TypeLoadException"
        let fileNotFound = "System.IO.FileNotFoundException"

        // Swift's convention is compiled as such on Arm64 and on x64 other than Windows'.
        let compilesSwift =
            not (
                OperatingSystem.IsWindows ()
                && Runtime.InteropServices.RuntimeInformation.ProcessArchitecture = Runtime.InteropServices.Architecture.X64
            )

        let swiftEscapes =
            if compilesSwift then
                CallOutcome.Escapes typeLoad
            else
                CallOutcome.Runs

        // Each caller, what running it does, and what the analysis lets escape it.
        let methods =
            [
                "PointerToGoneStruct", CallOutcome.Runs, []
                "Gone", CallOutcome.Runs, []
                "GenericOfGone", CallOutcome.Escapes typeLoad, [ typeLoad ]
                // The walk loads the instantiation; nothing loads what the pointer points to.
                "PointerThenGenericOfMissing", CallOutcome.Escapes fileNotFound, [ fileNotFound ]
                "ReturnsGone", CallOutcome.Escapes typeLoad, [ typeLoad ]
                "SwiftPointerToGoneStruct", swiftEscapes, [ typeLoad ]
                // A byref needs marshalling, so the JIT leaves the transition to the stub.
                "SwiftByrefToGoneStruct", CallOutcome.Runs, []
                // The import's convention is not `WinApi`, so CoreCLR reads no `UnmanagedCallConv`.
                "CdeclPointerToGoneStruct", CallOutcome.Runs, []
                // An import that names no calling convention CoreCLR reads as naming `WinApi`.
                "UnspecifiedPointerToGoneStruct", swiftEscapes, [ typeLoad ]
                // Inside a `try` with a `catch`, the JIT leaves the transition to the stub, which the
                // analysis does not decide.
                "CaughtGenericOfGone", CallOutcome.Runs, [ typeLoad ]
                "IndirectPointerToGoneStruct", CallOutcome.Runs, []
                "IndirectGenericOfGone", CallOutcome.Escapes typeLoad, [ typeLoad ]
                "SwiftIndirectPointerToGoneStruct", swiftEscapes, [ typeLoad ]
            ]

        // What running each caller does on the host's CoreCLR.
        let hostOutcomes : Map<string, CallOutcome> =
            let context =
                Runtime.Loader.AssemblyLoadContext ("UnmanagedCallTypeLoads", isCollectible = true)

            context.add_Resolving (
                Func<_, _, _> (fun (context : Runtime.Loader.AssemblyLoadContext) (name : AssemblyName) ->
                    match name.Name with
                    | "Provider" -> context.LoadFromStream (new MemoryStream (incompleteProvider))
                    | _ -> null
                )
            )

            try
                let calls =
                    context.LoadFromStream(new MemoryStream (clientImage)).GetType "Client.Calls"

                methods
                |> List.map (fun (name, _, _) ->
                    let outcome =
                        try
                            calls.GetMethod(name).Invoke ((null : obj), [||]) |> ignore
                            CallOutcome.Runs
                        with :? TargetInvocationException as e ->
                            CallOutcome.Escapes (e.InnerException.GetType().FullName)

                    name, outcome
                )
                |> Map.ofList
            finally
                context.Unload ()

        let read (name : string) (image : byte[]) : DumpedAssembly =
            Assembly.read loggerFactory (Some $"%s{name}.dll") (new MemoryStream (image))

        let clientAssembly = read "Client" clientImage
        let providerAssembly = read "Provider" incompleteProvider

        let reference =
            clientAssembly.AssemblyReferences.Values
            |> Seq.find (fun r -> r.Name.Name = "Provider")

        let mutable analysis =
            analysisOver
                [ clientAssembly ; providerAssembly ]
                (fun loaded -> fst (loaded.WithBoundReference reference providerAssembly))

        // The runtime makes each exception by running one of its constructors, and what that raises
        // escapes in its place.
        let made : Set<string> =
            let next, madeTypeLoad = EscapeAnalysis.escapes analysis (typeLoadConstructor ())
            analysis <- next
            let next, madeFileNotFound = making analysis fileNotFound
            analysis <- next
            Set.union (render analysis madeTypeLoad) madeFileNotFound

        let failures =
            [
                for name, expected, expectedEscaping in methods do
                    if hostOutcomes.[name] <> expected then
                        yield $"%s{name}: the host's outcome is %A{hostOutcomes.[name]}, not %A{expected}"

                    let next, escapes =
                        EscapeAnalysis.escapes analysis (methodNamed clientAssembly "Client.Calls" name)

                    analysis <- next
                    let shown = render analysis escapes

                    let escaping =
                        [ typeLoad ; fileNotFound ]
                        |> List.filter (fun exceptionName -> shown.Contains $"=%s{exceptionName}")

                    let unexplained =
                        (expectedEscaping
                         |> List.filter (fun expected -> not (List.contains expected escaping))
                         |> List.map (fun missing -> $"no %s{missing}"))
                        @ (escaping
                           |> List.filter (fun escaped ->
                               not (List.contains escaped expectedEscaping)
                               && (expectedEscaping.IsEmpty || not (made.Contains $"=%s{escaped}"))
                           ))

                    if not unexplained.IsEmpty then
                        yield $"%s{name}: %A{unexplained} in %A{Set.toList shown}, unknown %b{escapes.Unknown}"
            ]

        if not failures.IsEmpty then
            failures |> String.concat Environment.NewLine |> failwith

    /// A client compiled against one version of a provider, run against another lacking two types,
    /// taking the address of a method whose signature names a type that fails to load. The JIT
    /// loads the return type of the method whose address it takes in full; CoreCLR prepares the
    /// method as a function pointer, loading each value type of its signature whose definition is
    /// there, before that for an `ldvirtftn` or a method specification, and otherwise only where the
    /// JIT asks for the address itself, which it does compiling whatever method it inlines the
    /// `ldftn` into; a delegate's target it does not prepare. Each caller takes one address inside a
    /// `catch` of every exception, and names the type in no local and nowhere in its own signature.
    /// The host's CoreCLR, running each caller against the same assemblies, says which fail, and
    /// where. Where the JIT may ask for the address while compiling some caller of the method taking
    /// it, the analysis takes that caller, and each of its callers in turn, to fail past its
    /// handlers, which the host's outcome does not decide.
    [<Test>]
    let ``taking a method's address loads its return type, and its value types where CoreCLR prepares it`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let provider (complete : bool) : string =
            "namespace Provider; public class Kept { }"
            + (if complete then
                   " public class Gone { } public struct GoneStruct { }"
               else
                   "")

        let client =
            """
using System.Runtime.CompilerServices;

namespace Client;

public class DerivesFromGone : Provider.Gone { }
public struct HoldsGone { public Provider.GoneStruct G; }
public delegate void TakesHoldsGoneHandler(HoldsGone x);
public delegate void TakesGoneStructHandler(Provider.GoneStruct x);
public interface IStatic { static abstract void Target<U>(HoldsGone x); }
public struct StaticStruct : IStatic { public static void Target<U>(HoldsGone x) { } }

public static class Targets
{
    public static void TakesHoldsGone(HoldsGone x) { }
    public static void TakesGoneStruct(Provider.GoneStruct x) { }
    public static void TakesGone(Provider.Gone x) { }
    public static DerivesFromGone ReturnsDerivesFromGone() => null;
    public static void GenericTakesHoldsGone<T>(HoldsGone x) { }
}

public class Instance
{
    public void TakesHoldsGone(HoldsGone x) { }
    public virtual void VirtualTakesHoldsGone(HoldsGone x) { }
}

public static unsafe class Takes
{
    [MethodImpl(MethodImplOptions.NoInlining)]
    public static void Use(void* p) { }
    [MethodImpl(MethodImplOptions.NoInlining)]
    public static void UseDelegate(TakesHoldsGoneHandler d) { }
    [MethodImpl(MethodImplOptions.NoInlining)]
    public static void UseStructDelegate(TakesGoneStructHandler d) { }

    public static void* ConstrainedAddress<T>() where T : IStatic => (void*)(delegate*<HoldsGone, void>)&T.Target<int>;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void* InlinedAddress() => (void*)(delegate*<HoldsGone, void>)&Targets.TakesHoldsGone;
    [MethodImpl(MethodImplOptions.NoInlining)]
    public static void* NotInlinedAddress() => (void*)(delegate*<HoldsGone, void>)&Targets.TakesHoldsGone;
}

public static class SharedTarget<T>
{
    public static void TakesHoldsGone(HoldsGone x) { }
}

public static unsafe class Shared<T>
{
    public static void* Address() => (void*)(delegate*<HoldsGone, void>)&SharedTarget<T>.TakesHoldsGone;
}

public static unsafe class Addresses
{
    static void Use(void* p) { }

    public static string PointerToTakesHoldsGone()
    {
        try { Takes.Use((void*)(delegate*<HoldsGone, void>)&Targets.TakesHoldsGone); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string PointerToTakesGoneStruct()
    {
        try { Takes.Use((void*)(delegate*<Provider.GoneStruct, void>)&Targets.TakesGoneStruct); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string PointerToTakesGone()
    {
        try { Takes.Use((void*)(delegate*<Provider.Gone, void>)&Targets.TakesGone); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string PointerToReturnsDerivesFromGone()
    {
        try { Takes.Use((void*)(delegate*<DerivesFromGone>)&Targets.ReturnsDerivesFromGone); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string PointerToGenericTakesHoldsGone()
    {
        try { Takes.Use((void*)(delegate*<HoldsGone, void>)&Targets.GenericTakesHoldsGone<int>); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string DelegateToVirtualTakesHoldsGone()
    {
        try { Takes.UseDelegate(new Instance().VirtualTakesHoldsGone); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string DelegateToStaticTakesGoneStruct()
    {
        try { Takes.UseStructDelegate(Targets.TakesGoneStruct); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string DelegateToTakesHoldsGone()
    {
        try { Takes.UseDelegate(new Instance().TakesHoldsGone); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string CallsInlinedAddress()
    {
        try { Takes.Use(Takes.InlinedAddress()); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string CallsSharedAddress()
    {
        try { Takes.Use(Shared<object>.Address()); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string CallsNotInlinedAddress()
    {
        try { Takes.Use(Takes.NotInlinedAddress()); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string DiscardedPointerToTakesHoldsGone()
    {
        try { Use((void*)(delegate*<HoldsGone, void>)&Targets.TakesHoldsGone); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string CallsConstrainedAddress()
    {
        Takes.Use(Takes.ConstrainedAddress<StaticStruct>());
        return null;
    }
}
"""

        // Optimised, so that the JIT inlines: a Debug build disables its optimisation.
        let compile (name : string) (references : byte[] list) (text : string) : byte[] =
            Roslyn.compileOptimizedAssemblyWithReferences
                name
                OutputKind.DynamicallyLinkedLibrary
                (references
                 |> List.map (fun image ->
                     MetadataReference.CreateFromImage (ImmutableArray.CreateRange image) :> MetadataReference
                 ))
                [ text ]

        let completeProvider = compile "Provider" [] (provider true)
        let incompleteProvider = compile "Provider" [] (provider false)

        let clientImage =
            compile "Client" [ completeProvider ] client
            |> TestJitTypeLoads.discardAddresses "Addresses"

        let typeLoad = "System.TypeLoadException"

        // Each caller, the name of a type the signature of the method whose address it takes
        // spells, what running it does, and what the analysis lets escape it.
        let methods =
            [
                "PointerToTakesHoldsGone", "HoldsGone", CallOutcome.Escapes typeLoad, [ typeLoad ]
                "PointerToTakesGoneStruct", "GoneStruct", CallOutcome.Runs, []
                "PointerToTakesGone", "Gone", CallOutcome.Runs, []
                "PointerToReturnsDerivesFromGone", "DerivesFromGone", CallOutcome.Escapes typeLoad, [ typeLoad ]
                "PointerToGenericTakesHoldsGone", "HoldsGone", CallOutcome.Escapes typeLoad, [ typeLoad ]
                "DelegateToVirtualTakesHoldsGone", "HoldsGone", CallOutcome.Escapes typeLoad, [ typeLoad ]
                // A delegate closed over its target's receiver needs no thunk to shuffle the
                // arguments, which would load the value types of the signature.
                "DelegateToTakesHoldsGone", "HoldsGone", CallOutcome.Runs, []
                "DelegateToStaticTakesGoneStruct", "GoneStruct", CallOutcome.Escapes typeLoad, [ typeLoad ]
                // The caller's JIT, inlining the method taking the address, asks for it.
                "CallsInlinedAddress", "HoldsGone", CallOutcome.Escapes typeLoad, [ typeLoad ]
                // In shared code a generic dictionary supplies the address; the caller's JIT, inlining
                // the method for one instantiation, asks for it.
                "CallsSharedAddress", "HoldsGone", CallOutcome.Escapes typeLoad, [ typeLoad ]
                // The analysis does not tell which methods a JIT inlines.
                "CallsNotInlinedAddress", "HoldsGone", CallOutcome.Caught typeLoad, [ typeLoad ]
                // `Use`'s call replaced by a `pop`: nothing asks for the address.
                "DiscardedPointerToTakesHoldsGone", "HoldsGone", CallOutcome.Runs, []
                // CoreCLR prepares the method resolving the `constrained.` prefix, compiling the
                // method taking its address, which this caller does not catch.
                "CallsConstrainedAddress", "HoldsGone", CallOutcome.Escapes typeLoad, [ typeLoad ]
            ]

        // What running each caller does on the host's CoreCLR.
        let hostOutcomes : Map<string, CallOutcome> =
            let context =
                Runtime.Loader.AssemblyLoadContext ("MethodAddressTypeLoads", isCollectible = true)

            context.add_Resolving (
                Func<_, _, _> (fun (context : Runtime.Loader.AssemblyLoadContext) (name : AssemblyName) ->
                    match name.Name with
                    | "Provider" -> context.LoadFromStream (new MemoryStream (incompleteProvider))
                    | _ -> null
                )
            )

            try
                let addresses =
                    context.LoadFromStream(new MemoryStream (clientImage)).GetType "Client.Addresses"

                methods
                |> List.map (fun (name, _, _, _) ->
                    let outcome =
                        try
                            match addresses.GetMethod(name).Invoke ((null : obj), [||]) with
                            | null -> CallOutcome.Runs
                            | caught -> CallOutcome.Caught (unbox<string> caught)
                        with :? TargetInvocationException as e ->
                            CallOutcome.Escapes (e.InnerException.GetType().FullName)

                    name, outcome
                )
                |> Map.ofList
            finally
                context.Unload ()

        let read (name : string) (image : byte[]) : DumpedAssembly =
            Assembly.read loggerFactory (Some $"%s{name}.dll") (new MemoryStream (image))

        let clientAssembly = read "Client" clientImage
        let providerAssembly = read "Provider" incompleteProvider

        let reference =
            clientAssembly.AssemblyReferences.Values
            |> Seq.find (fun r -> r.Name.Name = "Provider")

        let mutable analysis =
            analysisOver
                [ clientAssembly ; providerAssembly ]
                (fun loaded -> fst (loaded.WithBoundReference reference providerAssembly))

        let named = namedTypes clientAssembly

        // The runtime makes the exception by running one of its constructors, and what that raises
        // escapes in its place.
        let madeTypeLoad =
            let next, made = EscapeAnalysis.escapes analysis (typeLoadConstructor ())
            analysis <- next
            render analysis made

        let failures =
            [
                for name, calleeType, expected, expectedEscaping in methods do
                    let key = methodNamed clientAssembly "Client.Addresses" name
                    let method = clientAssembly.Methods.[key.Method.Get]

                    // Only the method whose address is taken names the type: nothing the JIT loads
                    // of the caller's own signature or of its locals does.
                    let ownLoads =
                        [
                            match method.Signature.ReturnType with
                            | MethodReturnType.Void -> ()
                            | MethodReturnType.Returns returned -> yield JitTypePosition.OwnReturn, returned
                            for parameter in method.Signature.ParameterTypes do
                                yield JitTypePosition.OwnParameter, parameter
                            match method.Body with
                            | MethodBody.Il body ->
                                for local in Option.defaultValue ImmutableArray.Empty body.LocalVars do
                                    yield JitTypePosition.Local, local
                            | _ -> ()
                        ]
                        |> List.choose (fun (position, spelling) -> JitTypeLoads.load position spelling)

                    if
                        ownLoads
                        |> List.exists (fun load ->
                            match load with
                            | JitTypeLoad.InFull loaded -> List.contains calleeType (named loaded)
                            | other -> failwith $"%s{name}: the JIT loads %O{other} of its own signature or a local"
                        )
                    then
                        yield $"%s{name}: the JIT loads %s{calleeType} for its own signature or a local: %A{ownLoads}"

                    if hostOutcomes.[name] <> expected then
                        yield $"%s{name}: the host's outcome is %A{hostOutcomes.[name]}, not %A{expected}"

                    let next, escapes = EscapeAnalysis.escapes analysis key

                    analysis <- next
                    let shown = render analysis escapes

                    let escaping =
                        [
                            typeLoad
                            "System.MissingMethodException"
                            "System.IO.FileNotFoundException"
                        ]
                        |> List.filter (fun exceptionName -> shown.Contains $"=%s{exceptionName}")

                    // What escapes is the exception the caller's compilation raises, and whatever
                    // making that one raises.
                    let unexplained =
                        match expectedEscaping with
                        | [] -> escaping
                        | expectedEscaping ->
                            (expectedEscaping
                             |> List.filter (fun expected -> not (List.contains expected escaping))
                             |> List.map (fun missing -> $"no %s{missing}"))
                            @ (escaping
                               |> List.filter (fun escaped ->
                                   not (List.contains escaped expectedEscaping)
                                   && not (madeTypeLoad.Contains $"=%s{escaped}")
                               ))

                    if not unexplained.IsEmpty then
                        yield $"%s{name}: %A{unexplained} in %A{Set.toList shown}, unknown %b{escapes.Unknown}"
            ]

        if not failures.IsEmpty then
            failures |> String.concat Environment.NewLine |> failwith

    /// A client compiled against one version of a provider, run against another lacking two types,
    /// accessing fields and calling methods. The JIT loads in full the owner of each member a token
    /// names, before it resolves the member: a method or field definition's declaring type, a
    /// member reference's parent. It loads in full, too, the type of each field an instruction
    /// accesses, as the field's definition spells it. Each caller names the member inside a
    /// `catch` of every exception, of its own assembly or of another compiled against the complete
    /// provider, and names neither the owner nor the field's type in a local or in its own
    /// signature. The host's CoreCLR, running each caller against the same assemblies, says which
    /// fail, and where.
    [<Test>]
    let ``the JIT loads the owner of each member a token names, and the type of each field an instruction accesses``
        ()
        : unit
        =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let provider (complete : bool) : string =
            "namespace Provider; public class Kept { }"
            + (if complete then
                   " public class Gone { } public struct GoneStruct { }"
               else
                   "")

        let middle =
            """
namespace Middle;

public class DerivesFromGone : Provider.Gone
{
    public static int Count;
    public static Provider.Gone GoneField;
    public static void M() { }
}

public static class Fields
{
    public static Provider.Gone StaticGone;
    public static int StaticInt;
}
"""

        let client =
            """
using System.Runtime.CompilerServices;

namespace Client;

public class DerivesFromGone : Provider.Gone
{
    public static int Count;
    public static void M() { }
}
public class VarArgsOnGone : Provider.Gone
{
    public static void M(__arglist) { }
}
public struct HoldsGone { public Provider.GoneStruct G; }
public static class Box<T>
{
    public static int Count;
    public static void M() { }
}

public class Fields
{
    public static Provider.Gone StaticGone;
    public static DerivesFromGone StaticDerivesFromGone;
    public static int StaticInt;
    public Provider.Gone InstanceGone;
    public static Fields Make() => new Fields();
}

public static class StructFields
{
    public static HoldsGone StaticHoldsGone;
}

public static class Takes
{
    [MethodImpl(MethodImplOptions.NoInlining)]
    public static void Use(object o) { }
}

public static class Members
{
    public static string ReadsStaticGone()
    {
        try { Takes.Use(Fields.StaticGone); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string WritesStaticGone()
    {
        try { Fields.StaticGone = null; return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string ReadsStaticDerivesFromGone()
    {
        try { Takes.Use(Fields.StaticDerivesFromGone); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string ReadsStaticInt()
    {
        try { Takes.Use(Fields.StaticInt); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string ReadsInstanceGone()
    {
        try { Takes.Use(Fields.Make().InstanceGone); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string ReadsStaticHoldsGone()
    {
        try { Takes.Use(StructFields.StaticHoldsGone.G); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string ReadsDerivesFromGoneCount()
    {
        try { Takes.Use(DerivesFromGone.Count); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string CallsDerivesFromGoneM()
    {
        try { DerivesFromGone.M(); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string CallsVarArgsOnGone()
    {
        try { VarArgsOnGone.M(__arglist(1)); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string ReadsBoxOfHoldsGoneCount()
    {
        try { Takes.Use(Box<HoldsGone>.Count); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string CallsBoxOfHoldsGoneM()
    {
        try { Box<HoldsGone>.M(); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string ElsewhereReadsStaticGone()
    {
        try { Takes.Use(Middle.Fields.StaticGone); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string ElsewhereReadsStaticInt()
    {
        try { Takes.Use(Middle.Fields.StaticInt); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string ElsewhereReadsDerivesFromGoneCount()
    {
        try { Takes.Use(Middle.DerivesFromGone.Count); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string ElsewhereCallsDerivesFromGoneM()
    {
        try { Middle.DerivesFromGone.M(); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string ElsewhereReadsDerivesFromGoneGoneField()
    {
        try { Takes.Use(Middle.DerivesFromGone.GoneField); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
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

        let completeProvider = compile "Provider" [] (provider true)
        let incompleteProvider = compile "Provider" [] (provider false)
        let middleImage = compile "Middle" [ completeProvider ] middle
        let clientImage = compile "Client" [ completeProvider ; middleImage ] client

        let typeLoad = "System.TypeLoadException"

        // Each caller, the name of the type whose load fails, or would, and what running it does.
        let methods =
            [
                "ReadsStaticGone", "Gone", CallOutcome.Escapes typeLoad
                "WritesStaticGone", "Gone", CallOutcome.Escapes typeLoad
                "ReadsStaticDerivesFromGone", "DerivesFromGone", CallOutcome.Escapes typeLoad
                "ReadsStaticInt", "Fields", CallOutcome.Runs
                "ReadsInstanceGone", "Gone", CallOutcome.Escapes typeLoad
                "ReadsStaticHoldsGone", "HoldsGone", CallOutcome.Escapes typeLoad
                "ReadsDerivesFromGoneCount", "DerivesFromGone", CallOutcome.Escapes typeLoad
                "CallsDerivesFromGoneM", "DerivesFromGone", CallOutcome.Escapes typeLoad
                // A vararg call site names the method by a member reference whose parent is its
                // definition.
                "CallsVarArgsOnGone", "VarArgsOnGone", CallOutcome.Escapes typeLoad
                "ReadsBoxOfHoldsGoneCount", "HoldsGone", CallOutcome.Escapes typeLoad
                "CallsBoxOfHoldsGoneM", "HoldsGone", CallOutcome.Escapes typeLoad
                // CoreCLR matches the reference to the field by its signature, which names a type that
                // is not there, before it loads the type.
                "ElsewhereReadsStaticGone", "Gone", CallOutcome.Escapes "System.MissingFieldException"
                "ElsewhereReadsStaticInt", "Fields", CallOutcome.Runs
                "ElsewhereReadsDerivesFromGoneCount", "DerivesFromGone", CallOutcome.Escapes typeLoad
                "ElsewhereCallsDerivesFromGoneM", "DerivesFromGone", CallOutcome.Escapes typeLoad
                // CoreCLR loads the owner before it matches the reference to the field.
                "ElsewhereReadsDerivesFromGoneGoneField", "DerivesFromGone", CallOutcome.Escapes typeLoad
            ]

        // What running each caller does on the host's CoreCLR.
        let hostOutcomes : Map<string, CallOutcome> =
            let context =
                Runtime.Loader.AssemblyLoadContext ("MemberTypeLoads", isCollectible = true)

            context.add_Resolving (
                Func<_, _, _> (fun (context : Runtime.Loader.AssemblyLoadContext) (name : AssemblyName) ->
                    match name.Name with
                    | "Provider" -> context.LoadFromStream (new MemoryStream (incompleteProvider))
                    | "Middle" -> context.LoadFromStream (new MemoryStream (middleImage))
                    | _ -> null
                )
            )

            try
                let members =
                    context.LoadFromStream(new MemoryStream (clientImage)).GetType "Client.Members"

                methods
                |> List.map (fun (name, _, _) ->
                    let outcome =
                        try
                            match members.GetMethod(name).Invoke ((null : obj), [||]) with
                            | null -> CallOutcome.Runs
                            | caught -> CallOutcome.Caught (unbox<string> caught)
                        with :? TargetInvocationException as e ->
                            CallOutcome.Escapes (e.InnerException.GetType().FullName)

                    name, outcome
                )
                |> Map.ofList
            finally
                context.Unload ()

        let read (name : string) (image : byte[]) : DumpedAssembly =
            Assembly.read loggerFactory (Some $"%s{name}.dll") (new MemoryStream (image))

        let clientAssembly = read "Client" clientImage
        let middleAssembly = read "Middle" middleImage
        let providerAssembly = read "Provider" incompleteProvider

        let referenceTo (from : DumpedAssembly) (name : string) =
            from.AssemblyReferences.Values |> Seq.find (fun r -> r.Name.Name = name)

        let mutable analysis =
            analysisOver
                [ clientAssembly ; middleAssembly ; providerAssembly ]
                (fun loaded ->
                    let loaded, _ =
                        loaded.WithBoundReference (referenceTo clientAssembly "Provider") providerAssembly

                    let loaded, _ =
                        loaded.WithBoundReference (referenceTo clientAssembly "Middle") middleAssembly

                    fst (loaded.WithBoundReference (referenceTo middleAssembly "Provider") providerAssembly)
                )

        let named = namedTypes clientAssembly

        // The runtime makes the exception by running one of its constructors, and what that raises
        // escapes in its place.
        let madeTypeLoad =
            let next, made = EscapeAnalysis.escapes analysis (typeLoadConstructor ())
            analysis <- next
            render analysis made

        let failures =
            [
                for name, failingType, expected in methods do
                    let key = methodNamed clientAssembly "Client.Members" name
                    let method = clientAssembly.Methods.[key.Method.Get]

                    // Only the member's owner or the field's type names the type: nothing the JIT
                    // loads of the caller's own signature or of its locals does.
                    let ownLoads =
                        [
                            match method.Signature.ReturnType with
                            | MethodReturnType.Void -> ()
                            | MethodReturnType.Returns returned -> yield JitTypePosition.OwnReturn, returned
                            for parameter in method.Signature.ParameterTypes do
                                yield JitTypePosition.OwnParameter, parameter
                            match method.Body with
                            | MethodBody.Il body ->
                                for local in Option.defaultValue ImmutableArray.Empty body.LocalVars do
                                    yield JitTypePosition.Local, local
                            | _ -> ()
                        ]
                        |> List.choose (fun (position, spelling) -> JitTypeLoads.load position spelling)

                    if
                        ownLoads
                        |> List.exists (fun load ->
                            match load with
                            | JitTypeLoad.InFull loaded -> List.contains failingType (named loaded)
                            | other -> failwith $"%s{name}: the JIT loads %O{other} of its own signature or a local"
                        )
                    then
                        yield $"%s{name}: the JIT loads %s{failingType} for its own signature or a local: %A{ownLoads}"

                    if hostOutcomes.[name] <> expected then
                        yield $"%s{name}: the host's outcome is %A{hostOutcomes.[name]}, not %A{expected}"

                    let next, escapes = EscapeAnalysis.escapes analysis key

                    analysis <- next
                    let shown = render analysis escapes

                    let escaping =
                        [
                            typeLoad
                            "System.MissingFieldException"
                            "System.MissingMethodException"
                            "System.IO.FileNotFoundException"
                        ]
                        |> List.filter (fun exceptionName -> shown.Contains $"=%s{exceptionName}")

                    // What escapes is the exception the caller's compilation raises, and whatever
                    // making that one raises.
                    let unexplained =
                        match expected with
                        | CallOutcome.Escapes exceptionName ->
                            if not (List.contains exceptionName escaping) then
                                escaping @ [ $"no %s{exceptionName}" ]
                            else
                                escaping
                                |> List.filter (fun escaped ->
                                    escaped <> exceptionName && not (madeTypeLoad.Contains $"=%s{escaped}")
                                )
                        | CallOutcome.Caught _
                        | CallOutcome.Runs -> escaping

                    if not unexplained.IsEmpty then
                        yield $"%s{name}: %A{unexplained} in %A{Set.toList shown}, unknown %b{escapes.Unknown}"
            ]

        if not failures.IsEmpty then
            failures |> String.concat Environment.NewLine |> failwith

    /// A client compiled against one version of a provider, run against another lacking two types,
    /// making delegates. Making a delegate open over its target, as one over a static method is
    /// unless it closes over the method's first argument, needs a thunk to shuffle the arguments,
    /// and CoreCLR, making the thunk, loads approximately each value type of the delegate's `Invoke`
    /// signature and then of the target's (`COMDelegate::GetDelegateCtor`, `SetupShuffleThunk`).
    /// Making one closed over its target's receiver or first argument loads nothing of either
    /// signature. CoreCLR makes the thunk while the JIT compiles the method making the delegate,
    /// unless the target is a static method that code shared between instantiations names through
    /// its type variables, whose thunk it makes as the delegate is made (`Delegate_Construct`).
    /// Each caller makes one delegate inside a `catch` of every exception, or calls a method that
    /// does, and names none of the types in a local or in its own signature. The host's CoreCLR,
    /// running each caller against the same assemblies, says which fail, and where.
    [<Test>]
    let ``making an open delegate loads approximately the value types of its signature`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let provider (complete : bool) : string =
            "namespace Provider; public class Kept { }"
            + (if complete then
                   " public class Gone { } public struct GoneStruct { }"
               else
                   "")

        let client =
            """
using System.Runtime.CompilerServices;

namespace Client;

public struct HoldsGone { public Provider.GoneStruct G; }
public struct Wraps<T> { public T Value; }
public interface IHolds<T> { }
public struct ViaInterface : IHolds<Provider.Gone> { }

public delegate void TakesHoldsGone(HoldsGone h);
public delegate void TakesWrapsGone(Wraps<Provider.Gone> w);
public delegate void TakesViaInterface(ViaInterface v);
public delegate void TakesGone(Provider.Gone g);
public delegate void TakesGoneStruct(Provider.GoneStruct s);
public delegate void TakesInt(int i);

public class Target
{
    public void TakesHoldsGone(HoldsGone h) { }
    public virtual void VirtualTakesHoldsGone(HoldsGone h) { }
}

public static class Targets
{
    public static void TakesHoldsGone(HoldsGone h) { }
    public static void TakesWrapsGone(Wraps<Provider.Gone> w) { }
    public static void TakesViaInterface(ViaInterface v) { }
    public static void TakesGone(Provider.Gone g) { }
    public static void ExtensionTakesHoldsGone(this Target t, HoldsGone h) { }
    public static void TakesInt(int i) { }
    public static void TakesGoneStruct(Provider.GoneStruct s) { }
    public static void GenericTakesGoneStruct<U>(Provider.GoneStruct s) { }
}

public static class Generic<T>
{
    public static void TakesHoldsGone(HoldsGone h) { }
}

public static class Takes
{
    [MethodImpl(MethodImplOptions.NoInlining)]
    public static void Use(object o) { }
}

public static class Shared
{
    public static string OverTypeArgument<T>()
    {
        try { Takes.Use(new TakesHoldsGone(Generic<T>.TakesHoldsGone)); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string OverMethodArgument<T>()
    {
        try { Takes.Use(new TakesGoneStruct(Targets.GenericTakesGoneStruct<T>)); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
}

public static class Members
{
    public static string OpenStaticHoldsGone()
    {
        try { Takes.Use(new TakesHoldsGone(Targets.TakesHoldsGone)); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string OpenStaticWrapsGone()
    {
        try { Takes.Use(new TakesWrapsGone(Targets.TakesWrapsGone)); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string OpenStaticViaInterface()
    {
        try { Takes.Use(new TakesViaInterface(Targets.TakesViaInterface)); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string OpenStaticGoneStruct()
    {
        try { Takes.Use(new TakesGoneStruct(Targets.TakesGoneStruct)); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string OpenStaticGone()
    {
        try { Takes.Use(new TakesGone(Targets.TakesGone)); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string ClosedInstanceHoldsGone()
    {
        try { Takes.Use(new TakesHoldsGone(new Target().TakesHoldsGone)); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string ClosedVirtualHoldsGone()
    {
        try { Takes.Use(new TakesHoldsGone(new Target().VirtualTakesHoldsGone)); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string ClosedStaticHoldsGone()
    {
        try { Takes.Use(new TakesHoldsGone(new Target().ExtensionTakesHoldsGone)); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string MismatchedTarget()
    {
        try { Takes.Use(new TakesInt(Targets.TakesInt)); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string MismatchedInvoke()
    {
        try { Takes.Use(new TakesHoldsGone(Targets.TakesHoldsGone)); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string UnsharedOverTypeArgument() => Shared.OverTypeArgument<int>();
    public static string SharedOverTypeArgument() => Shared.OverTypeArgument<string>();
    public static string UnsharedOverMethodArgument() => Shared.OverMethodArgument<int>();
    public static string SharedOverMethodArgument() => Shared.OverMethodArgument<string>();
}
"""

        let compile (name : string) (references : byte[] list) (text : string) : byte[] =
            Roslyn.compileAssembly
                name
                OutputKind.DynamicallyLinkedLibrary
                (references
                 |> List.map (fun image -> MetadataReference.CreateFromImage (ImmutableArray.CreateRange image)))
                [ text ]

        // The image with the `ldftn` of `Targets.{from}` in `Members.{caller}` naming
        // `Targets.{onto}` instead, so that the delegate made over it has a signature other than
        // its target's, which C# has no way to say.
        let retarget (caller : string) (from : string) (onto : string) (image : byte[]) : byte[] =
            let patched = Array.copy image
            use pe = new PortableExecutable.PEReader (ImmutableArray.CreateRange image)
            let metadata = pe.GetMetadataReader ()

            let methodsOf (typeName : string) =
                metadata.TypeDefinitions
                |> Seq.map metadata.GetTypeDefinition
                |> Seq.filter (fun ty -> metadata.GetString ty.Name = typeName)
                |> Seq.exactlyOne
                |> fun ty -> ty.GetMethods ()
                |> Seq.map (fun handle -> handle, metadata.GetMethodDefinition handle)
                |> List.ofSeq

            let tokenOf (name : string) : int =
                methodsOf "Targets"
                |> List.filter (fun (_, m) -> metadata.GetString m.Name = name)
                |> List.exactlyOne
                |> fst
                |> fun handle ->
                    Ecma335.MetadataTokens.GetToken (MethodDefinitionHandle.op_Implicit handle : EntityHandle)

            let ldftn (name : string) : byte[] =
                Array.append [| 0xFEuy ; 0x06uy |] (BitConverter.GetBytes (tokenOf name))

            let body =
                methodsOf "Members"
                |> List.filter (fun (_, m) -> metadata.GetString m.Name = caller)
                |> List.exactlyOne
                |> snd

            let rva = body.RelativeVirtualAddress

            let section =
                pe.PEHeaders.SectionHeaders
                |> Seq.find (fun s -> rva >= s.VirtualAddress && rva < s.VirtualAddress + s.VirtualSize)

            let start = rva - section.VirtualAddress + section.PointerToRawData
            let size = (pe.GetMethodBody rva).Size
            let original = ldftn from

            match
                [ start .. start + size - original.Length ]
                |> List.filter (fun i -> patched.[i .. i + original.Length - 1] = original)
            with
            | [ at ] -> Array.blit (ldftn onto) 0 patched at original.Length
            | found -> failwith $"%s{caller} takes the address of %s{from} %d{found.Length} times, not once"

            patched

        let completeProvider = compile "Provider" [] (provider true)
        let incompleteProvider = compile "Provider" [] (provider false)

        let clientImage =
            compile "Client" [ completeProvider ] client
            |> retarget "MismatchedTarget" "TakesInt" "TakesHoldsGone"
            |> retarget "MismatchedInvoke" "TakesHoldsGone" "TakesInt"

        let typeLoad = "System.TypeLoadException"

        // Each caller, the name of the type whose load fails, or would, and what running it does.
        let methods =
            [
                "OpenStaticHoldsGone", "HoldsGone", CallOutcome.Escapes typeLoad
                // A delegate taking an `int` over a method taking a `HoldsGone`: the target's
                // signature is walked too.
                "MismatchedTarget", "HoldsGone", CallOutcome.Escapes typeLoad
                // A delegate taking a `HoldsGone` over a method taking an `int`: the `Invoke`
                // method's signature is walked too.
                "MismatchedInvoke", "HoldsGone", CallOutcome.Escapes typeLoad
                // The approximate load of an instantiation leaves out a class argument.
                "OpenStaticWrapsGone", "Gone", CallOutcome.Runs
                // The approximate load of a type loads only the generic definition of an interface.
                "OpenStaticViaInterface", "Gone", CallOutcome.Runs
                "OpenStaticGone", "Gone", CallOutcome.Runs
                "OpenStaticGoneStruct", "GoneStruct", CallOutcome.Escapes typeLoad
                "ClosedInstanceHoldsGone", "HoldsGone", CallOutcome.Runs
                // `ldvirtftn` prepares its method as a function pointer, which loads the value types of
                // its signature, whatever becomes of the address.
                "ClosedVirtualHoldsGone", "HoldsGone", CallOutcome.Escapes typeLoad
                "ClosedStaticHoldsGone", "HoldsGone", CallOutcome.Runs
                "UnsharedOverTypeArgument", "HoldsGone", CallOutcome.Escapes typeLoad
                "SharedOverTypeArgument", "HoldsGone", CallOutcome.Caught typeLoad
                // An `ldftn` of a method a method specification instantiates prepares it as a
                // function pointer too, but that passes over a reference to a type its assembly lacks.
                "UnsharedOverMethodArgument", "GoneStruct", CallOutcome.Escapes typeLoad
                "SharedOverMethodArgument", "GoneStruct", CallOutcome.Caught typeLoad
            ]

        // The analysis takes a failure making a delegate's thunk to fail the caller's compilation
        // wherever CoreCLR makes it, though where it makes it as the delegate is made, the caller's
        // `catch` stops the failure: an over-approximation.
        let overReported = set [ "SharedOverTypeArgument" ; "SharedOverMethodArgument" ]

        // What running each caller does on the host's CoreCLR.
        let hostOutcomes : Map<string, CallOutcome> =
            let context =
                Runtime.Loader.AssemblyLoadContext ("DelegateTypeLoads", isCollectible = true)

            context.add_Resolving (
                Func<_, _, _> (fun (context : Runtime.Loader.AssemblyLoadContext) (name : AssemblyName) ->
                    match name.Name with
                    | "Provider" -> context.LoadFromStream (new MemoryStream (incompleteProvider))
                    | _ -> null
                )
            )

            try
                let members =
                    context.LoadFromStream(new MemoryStream (clientImage)).GetType "Client.Members"

                methods
                |> List.map (fun (name, _, _) ->
                    let outcome =
                        try
                            match members.GetMethod(name).Invoke ((null : obj), [||]) with
                            | null -> CallOutcome.Runs
                            | caught -> CallOutcome.Caught (unbox<string> caught)
                        with :? TargetInvocationException as e ->
                            CallOutcome.Escapes (e.InnerException.GetType().FullName)

                    name, outcome
                )
                |> Map.ofList
            finally
                context.Unload ()

        let read (name : string) (image : byte[]) : DumpedAssembly =
            Assembly.read loggerFactory (Some $"%s{name}.dll") (new MemoryStream (image))

        let clientAssembly = read "Client" clientImage
        let providerAssembly = read "Provider" incompleteProvider

        let referenceTo (from : DumpedAssembly) (name : string) =
            from.AssemblyReferences.Values |> Seq.find (fun r -> r.Name.Name = name)

        let mutable analysis =
            analysisOver
                [ clientAssembly ; providerAssembly ]
                (fun loaded -> fst (loaded.WithBoundReference (referenceTo clientAssembly "Provider") providerAssembly))

        let named = namedTypes clientAssembly

        // The runtime makes the exception by running one of its constructors, and what that raises
        // escapes in its place.
        let madeTypeLoad =
            let next, made = EscapeAnalysis.escapes analysis (typeLoadConstructor ())
            analysis <- next
            render analysis made

        let failures =
            [
                for name, failingType, expected in methods do
                    let key = methodNamed clientAssembly "Client.Members" name
                    let method = clientAssembly.Methods.[key.Method.Get]

                    // Only the delegate's signature or its target's names the type: nothing the JIT
                    // loads of the caller's own signature or of its locals does.
                    let ownLoads =
                        [
                            match method.Signature.ReturnType with
                            | MethodReturnType.Void -> ()
                            | MethodReturnType.Returns returned -> yield JitTypePosition.OwnReturn, returned
                            for parameter in method.Signature.ParameterTypes do
                                yield JitTypePosition.OwnParameter, parameter
                            match method.Body with
                            | MethodBody.Il body ->
                                for local in Option.defaultValue ImmutableArray.Empty body.LocalVars do
                                    yield JitTypePosition.Local, local
                            | _ -> ()
                        ]
                        |> List.choose (fun (position, spelling) -> JitTypeLoads.load position spelling)

                    if
                        ownLoads
                        |> List.exists (fun load ->
                            match load with
                            | JitTypeLoad.InFull loaded -> List.contains failingType (named loaded)
                            | other -> failwith $"%s{name}: the JIT loads %O{other} of its own signature or a local"
                        )
                    then
                        yield $"%s{name}: the JIT loads %s{failingType} for its own signature or a local: %A{ownLoads}"

                    if hostOutcomes.[name] <> expected then
                        yield $"%s{name}: the host's outcome is %A{hostOutcomes.[name]}, not %A{expected}"

                    let analysed =
                        if overReported.Contains name then
                            CallOutcome.Escapes typeLoad
                        else
                            expected

                    let next, escapes = EscapeAnalysis.escapes analysis key

                    analysis <- next
                    let shown = render analysis escapes

                    let escaping =
                        [ typeLoad ; "System.IO.FileNotFoundException" ]
                        |> List.filter (fun exceptionName -> shown.Contains $"=%s{exceptionName}")

                    // What escapes is the exception the caller's compilation raises, and whatever
                    // making that one raises.
                    let unexplained =
                        match analysed with
                        | CallOutcome.Escapes exceptionName ->
                            if not (List.contains exceptionName escaping) then
                                escaping @ [ $"no %s{exceptionName}" ]
                            else
                                escaping
                                |> List.filter (fun escaped ->
                                    escaped <> exceptionName && not (madeTypeLoad.Contains $"=%s{escaped}")
                                )
                        | CallOutcome.Caught _
                        | CallOutcome.Runs -> escaping

                    if not unexplained.IsEmpty then
                        yield $"%s{name}: %A{unexplained} in %A{Set.toList shown}, unknown %b{escapes.Unknown}"
            ]

        if not failures.IsEmpty then
            failures |> String.concat Environment.NewLine |> failwith

    /// A client compiled against one version of a provider, run against another lacking two types
    /// and without the assembly `Optional`, naming types whose type parameters' constraints, or whose
    /// generic methods' constraints, name those types or that assembly. CoreCLR's full load of a type
    /// loads both (`MethodTable::DoFullyLoad`), so the JIT, loading a type a method names, fails to
    /// compile it, past the method's own `catch`, even in code shared between instantiations. The
    /// host's CoreCLR, running each method against the same assemblies, says which fail.
    [<Test>]
    let ``loading a type loads its type parameters' and its generic methods' constraints`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let provider (complete : bool) : string =
            "namespace Provider; public class Kept { } public interface IKeptGeneric<T> { }"
            + (if complete then
                   " public class Gone { } public interface IGone { }"
               else
                   "")

        let optional = "namespace Optional; public class Thing { }"

        let client =
            """
namespace Client;

public class ConstrainedToGone<T> where T : Provider.Gone { }
public class ConstrainedToInterfaceOfGone<T> where T : Provider.IKeptGeneric<Provider.Gone> { }
public class ConstrainedToMissing<T> where T : Optional.Thing { }
public class ConstrainedToKept<T> where T : Provider.Kept { }
public class HasGoneMethod { public static int Fine() => 1; public static void M<U>() where U : Provider.Gone { } }
public class HasGoneMethodGeneric<T> { public static void M<U>() where U : Provider.IGone { } }
public class HasMissingMethod { public static int Fine() => 1; public static void M<U>() where U : Optional.Thing { } }
public class HasKeptMethod { public static int Fine() => 1; public static void M<U>() where U : Provider.Kept { } }

public static class Shared
{
    public static string OverTypeArgument<T>()
    {
        try { return typeof(HasGoneMethodGeneric<T>).Name; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
}

public static class Members
{
    public static string TokenOfConstrainedToGone()
    {
        try { return typeof(ConstrainedToGone<>).Name; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string TokenOfConstrainedToInterfaceOfGone()
    {
        try { return typeof(ConstrainedToInterfaceOfGone<>).Name; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string TokenOfConstrainedToMissing()
    {
        try { return typeof(ConstrainedToMissing<>).Name; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string TokenOfConstrainedToKept()
    {
        try { typeof(ConstrainedToKept<>).ToString(); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string CallOnHasGoneMethod()
    {
        try { HasGoneMethod.Fine(); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string CallOnHasMissingMethod()
    {
        try { HasMissingMethod.Fine(); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string CallOnHasKeptMethod()
    {
        try { HasKeptMethod.Fine(); return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
    public static string SharedOverTypeArgument() => Shared.OverTypeArgument<string>();
}
"""

        let compile (name : string) (references : byte[] list) (text : string) : byte[] =
            Roslyn.compileAssembly
                name
                OutputKind.DynamicallyLinkedLibrary
                (references
                 |> List.map (fun image -> MetadataReference.CreateFromImage (ImmutableArray.CreateRange image)))
                [ text ]

        let completeProvider = compile "Provider" [] (provider true)
        let incompleteProvider = compile "Provider" [] (provider false)
        let optionalImage = compile "Optional" [] optional

        let clientImage = compile "Client" [ completeProvider ; optionalImage ] client

        let typeLoad = "System.TypeLoadException"
        let fileNotFound = "System.IO.FileNotFoundException"

        // Each method, and what running it does.
        let methods =
            [
                "TokenOfConstrainedToGone", CallOutcome.Escapes typeLoad
                // An interface constraint over a type: CoreCLR loads each type its signature names.
                "TokenOfConstrainedToInterfaceOfGone", CallOutcome.Escapes typeLoad
                "TokenOfConstrainedToMissing", CallOutcome.Escapes fileNotFound
                "TokenOfConstrainedToKept", CallOutcome.Runs
                // The owner of a method a call names, loaded in full, with its generic methods'
                // constraints.
                "CallOnHasGoneMethod", CallOutcome.Escapes typeLoad
                "CallOnHasMissingMethod", CallOutcome.Escapes fileNotFound
                "CallOnHasKeptMethod", CallOutcome.Runs
                // Code shared between instantiations loads a type a token names over its type
                // variables, and so that type's generic definition, when the JIT compiles it.
                "SharedOverTypeArgument", CallOutcome.Escapes typeLoad
            ]

        // What running each method does on the host's CoreCLR.
        let hostOutcomes : Map<string, CallOutcome> =
            let context =
                Runtime.Loader.AssemblyLoadContext ("ConstraintTypeLoads", isCollectible = true)

            context.add_Resolving (
                Func<_, _, _> (fun (context : Runtime.Loader.AssemblyLoadContext) (name : AssemblyName) ->
                    match name.Name with
                    | "Provider" -> context.LoadFromStream (new MemoryStream (incompleteProvider))
                    | _ -> null
                )
            )

            try
                let members =
                    context.LoadFromStream(new MemoryStream (clientImage)).GetType "Client.Members"

                methods
                |> List.map (fun (name, _) ->
                    let outcome =
                        try
                            match members.GetMethod(name).Invoke ((null : obj), [||]) with
                            | null -> CallOutcome.Runs
                            | :? string as returned when returned.StartsWith "System." -> CallOutcome.Caught returned
                            | _ -> CallOutcome.Runs
                        with :? TargetInvocationException as e ->
                            CallOutcome.Escapes (e.InnerException.GetType().FullName)

                    name, outcome
                )
                |> Map.ofList
            finally
                context.Unload ()

        let read (name : string) (image : byte[]) : DumpedAssembly =
            Assembly.read loggerFactory (Some $"%s{name}.dll") (new MemoryStream (image))

        let clientAssembly = read "Client" clientImage
        let providerAssembly = read "Provider" incompleteProvider

        let referenceTo (from : DumpedAssembly) (name : string) =
            from.AssemblyReferences.Values |> Seq.find (fun r -> r.Name.Name = name)

        let mutable analysis =
            analysisOver
                [ clientAssembly ; providerAssembly ]
                (fun loaded -> fst (loaded.WithBoundReference (referenceTo clientAssembly "Provider") providerAssembly))

        // The runtime makes the exception by running one of its constructors, and what that raises
        // escapes in its place.
        let madeTypeLoad =
            let next, made = EscapeAnalysis.escapes analysis (typeLoadConstructor ())
            analysis <- next
            render analysis made

        let failures =
            [
                for name, expected in methods do
                    if hostOutcomes.[name] <> expected then
                        yield $"%s{name}: the host's outcome is %A{hostOutcomes.[name]}, not %A{expected}"

                    let next, escapes =
                        EscapeAnalysis.escapes analysis (methodNamed clientAssembly "Client.Members" name)

                    analysis <- next
                    let shown = render analysis escapes

                    let escaping =
                        [ typeLoad ; fileNotFound ]
                        |> List.filter (fun exceptionName -> shown.Contains $"=%s{exceptionName}")

                    // What escapes is the exception the method's compilation raises, and whatever
                    // making that one raises.
                    let unexplained =
                        match expected with
                        | CallOutcome.Escapes exceptionName ->
                            if not (List.contains exceptionName escaping) then
                                escaping @ [ $"no %s{exceptionName}" ]
                            else
                                escaping
                                |> List.filter (fun escaped ->
                                    escaped <> exceptionName && not (madeTypeLoad.Contains $"=%s{escaped}")
                                )
                        | CallOutcome.Caught _
                        | CallOutcome.Runs -> escaping

                    if not unexplained.IsEmpty then
                        yield $"%s{name}: %A{unexplained} in %A{Set.toList shown}, unknown %b{escapes.Unknown}"
            ]

        if not failures.IsEmpty then
            failures |> String.concat Environment.NewLine |> failwith

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

public class GenericChild<T> : System.ComponentModel.Component { }

public static class Spells
{
    public static bool Check(object x) => x is Child;
    public static Child Cast(object x) => (Child)x;
    public static object Make() => new Child();
    public static bool CheckGeneric(object x) => x is GenericChild<int>;
    public static GenericChild<int> CastGeneric(object x) => (GenericChild<int>)x;
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
        for name in [ "Check" ; "Cast" ; "Make" ; "CheckGeneric" ; "CastGeneric" ] do
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
    // Of no class the analysis can tell, being public, so that what its `ToString` raises is
    // unknown.
    public static object s_value;

    [System.Runtime.CompilerServices.ModuleInitializer]
    internal static void Run()
    {
        s_value?.ToString();
        throw new System.InvalidOperationException();
    }
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

        // What of a failed module initializer the method's answer has: the wrapper, and what the
        // initializer raises, which escapes in its place should making the wrapper fail.
        let reportsInitialisation (assembly : DumpedAssembly) (typeName : string) (methodName : string) : bool * bool =
            let next, escapes =
                EscapeAnalysis.escapes analysis (methodNamed assembly typeName methodName)

            analysis <- next
            let shown = render analysis escapes

            shown.Contains "=System.TypeInitializationException", shown.Contains "=System.InvalidOperationException"

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

            // Binding happens before the body runs, so `CaughtCallM`'s handler catches neither.
            if reportsInitialisation clientAssembly "Client.Uses" methodName <> (true, true) then
                failwith $"%s{methodName} binds into Provider, whose module initializer throws"

        // What the module initializer cannot see through is where answers binding into it are
        // unknown from.
        let next, sources =
            EscapeAnalysis.unknownSources analysis (methodNamed clientAssembly "Client.Uses" "CallM")

        analysis <- next

        sources
        |> Seq.map (renderSite analysis)
        |> Set.ofSeq
        |> shouldEqual (Set.singleton "Provider.Init::Run IL VirtualCall System.Object::ToString")

        escapesOnRealRuntime "Local" |> shouldEqual false

        reportsInitialisation clientAssembly "Client.Uses" "Local"
        |> shouldEqual (false, false)
        // A module's own code runs only once its initializer has.
        reportsInitialisation providerAssembly "Provider.P" "CallsM"
        |> shouldEqual (false, false)

    /// Types whose initializers fail, and the ways a body can reach them, which CoreCLR's
    /// `CEEInfo::initClass` decides between: a static field's access runs its type's initializer,
    /// and so does calling a method of a type not marked `beforefieldinit` that is static, a
    /// constructor, or an instance method of a value type.
    let private initializerSource : string =
        """
using System;

namespace Inits;

public class Precise
{
    static Precise() { throw new InvalidTimeZoneException(); }
    public static void Run() { }
    public void Instance() { }
}

public struct PreciseValue
{
    static PreciseValue() { throw new InvalidTimeZoneException(); }
    public int Field;
    public void Instance() { }
}

// No static constructor, so `beforefieldinit`: only touching a static field runs the initializer.
public static class Lazy
{
    public static readonly int Value = Fail();
    static int Fail() => throw new InvalidTimeZoneException();
    public static void Run() { }
}

public sealed class Boom : Exception { }

public class Thrower
{
    public static readonly int Value;
    static Thrower() { throw new Boom(); }
    public static void Run() { }
}

// Its initializer can fail only by running out of memory, whose exception is made in advance, so
// that nothing it runs reaches the preservation of a stack trace: only the runtime's throwing that
// exception again does.
public class Allocates
{
    public static readonly object Value;
    static Allocates() { Value = new object(); }
    public static void Run() { }
}

public interface IMake { static abstract int Make(); }
public sealed class MakesFormat : IMake { public static int Make() => throw new FormatException(); }
public sealed class MakesNothing : IMake { public static int Make() => 0; }

// Each instantiation has an initializer of its own, which raises what its argument's `Make` does.
public static class Made<T> where T : IMake
{
    public static readonly int Value = T.Make();
}

public static class Runners
{
    public static void CallsStatic() => Precise.Run();
    public static void Constructs() => new Precise();
    public static void CallsValueInstance() => default(PreciseValue).Instance();
    public static int ReadsValueField() => default(PreciseValue).Field;
    public static void WritesValueField() { PreciseValue v = default; v.Field = 1; }
    public static void CallsReferenceInstance() { Precise p = null; p.Instance(); }
    public static void CallsLazy() => Lazy.Run();
    public static int ReadsLazy() => Lazy.Value;
    public static void CatchesStatic()
    {
        try { Precise.Run(); }
        catch (TypeInitializationException) { }
    }
    public static int ReadsMadeFormat() => Made<MakesFormat>.Value;
    public static int ReadsMadeNothing() => Made<MakesNothing>.Value;
    public static void CallsThrower() => Thrower.Run();
    public static int ReadsThrower() => Thrower.Value;
    public static void CallsAllocates() => Allocates.Run();
    public static object ReadsAllocates() => Allocates.Value;
}
"""

    [<Test>]
    let ``what runs a type's initializer, and what that initializer raises, are in the answer`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let image =
            Roslyn.compileAssembly "Inits" OutputKind.DynamicallyLinkedLibrary [] [ initializerSource ]

        let fixture =
            Assembly.read loggerFactory (Some "Inits.dll") (new MemoryStream (image))

        // The full name of what escapes the runner on the real runtime, if anything does, in a
        // context of its own, since a type whose initializer failed fails every later touch.
        let onRealRuntime (runner : string) : string option =
            let context =
                System.Runtime.Loader.AssemblyLoadContext ("Inits", isCollectible = true)

            try
                let runners =
                    context.LoadFromStream(new MemoryStream (image)).GetType "Inits.Runners"

                try
                    runners.GetMethod(runner).Invoke ((null : obj), Array.empty<obj>) |> ignore<obj>

                    None
                with :? TargetInvocationException as e ->
                    Some (e.InnerException.GetType().FullName)
            finally
                context.Unload ()

        let initialization = "System.TypeInitializationException"
        let own = "System.InvalidTimeZoneException"
        let format = "System.FormatException"

        // Each runner, what escapes it on the real runtime, what its answer must name besides, and
        // what its answer must not. A runner that faults makes the fault's exception, which looks up
        // its message, and the lookup runs initializers of its own, so a `TypeInitializationException`
        // in its answer says nothing of `Precise`'s.
        let cases : (string * string option * string list * string list) list =
            [
                "CallsStatic", Some initialization, [ own ], []
                "Constructs", Some initialization, [ own ], []
                "CallsValueInstance", Some initialization, [ own ], []
                "ReadsValueField", None, [], [ own ]
                "WritesValueField", None, [], [ own ]
                "CallsReferenceInstance", Some "System.NullReferenceException", [], [ own ]
                "CallsLazy", None, [], [ initialization ; own ]
                "ReadsLazy", Some initialization, [ own ], []
                // What the initializer raises when making the wrapper fails is no wrapper.
                "CatchesStatic", None, [ own ], [ initialization ]
                "ReadsMadeFormat", Some initialization, [ format ], []
                "ReadsMadeNothing", None, [ initialization ], [ format ]
                "CallsThrower", Some initialization, [ "Inits.Boom" ], []
                "ReadsThrower", Some initialization, [ "Inits.Boom" ], []
                "CallsAllocates", None, [ initialization ], []
                "ReadsAllocates", None, [ initialization ], []
            ]

        let mutable analysis = analysisOver [ fixture ] id

        let failures =
            [
                for runner, raised, named, excluded in cases do
                    onRealRuntime runner |> shouldEqual raised

                    let next, escapes =
                        EscapeAnalysis.escapes analysis (methodNamed fixture "Inits.Runners" runner)

                    analysis <- next
                    let shown = render analysis escapes
                    let describe = $"%s{runner}: %A{Set.toList shown}, unknown %b{escapes.Unknown}"

                    if escapes.Unknown then
                        yield $"%s{describe}, expected known"

                    for name in Option.toList raised @ named do
                        if not (shown.Contains ("=" + name)) then
                            yield $"%s{describe} lacks %s{name}"

                    for name in excluded do
                        if shown.Contains ("=" + name) then
                            yield $"%s{describe} has %s{name}"
            ]

        match failures with
        | [] -> ()
        | failures -> failures |> String.concat Environment.NewLine |> failwith

    [<Test>]
    let ``a failing type initializer is unknown only for preserving a stack trace, unless that is assumed too``
        ()
        : unit
        =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let image =
            Roslyn.compileAssembly "Inits" OutputKind.DynamicallyLinkedLibrary [] [ initializerSource ]

        let fixture =
            Assembly.read loggerFactory (Some "Inits.dll") (new MemoryStream (image))

        let analysisAllowed (assumptions : Set<Assumption>) : EscapeAnalysisState =
            analysisOf
                (hostCoreLib ())
                (FrameworkUnderTest.runtimeDirs ())
                (hostTarget ())
                HardwareIntrinsicsProfile.ScalarOnly
                assumptions
                [ fixture ]
                id

        let mutable unassuming =
            analysisAllowed (Set.remove Assumption.StackTracePreserved Assumption.all)

        let next, preserving = preservingStackTrace unassuming (hostCoreLib ())
        unassuming <- next
        let mutable assuming = analysisAllowed Assumption.all

        // `Precise`'s initializer makes a CoreLib exception, whose message's lookup may throw an
        // initializer's exception again by itself. Nothing `Allocates`' runs does, so only the site
        // running it does: a call, and a field's access.
        for runner in [ "CallsStatic" ; "CallsAllocates" ; "ReadsAllocates" ] do
            let key = methodNamed fixture "Inits.Runners" runner
            let next, escapes = EscapeAnalysis.escapes unassuming key
            escapes.Unknown |> shouldEqual true
            escapes.Assumes.Contains Assumption.StackTracePreserved |> shouldEqual false
            let next, sources = EscapeAnalysis.unknownSources next key
            unassuming <- next

            sources
            |> Seq.map (renderSite unassuming)
            |> Set.ofSeq
            |> shouldEqual preserving

            // Allowed it too, the answer is known, and relies on it.
            let next, escapes = EscapeAnalysis.escapes assuming key
            assuming <- next
            escapes.Unknown |> shouldEqual false
            escapes.Assumes.Contains Assumption.StackTracePreserved |> shouldEqual true

    /// `int32[,]`'s constructor taking lower bounds and lengths raises `ArgumentOutOfRangeException`
    /// for bounds whose upper end overflows; the one taking lengths alone does not. Each, and
    /// `newarr`, raises what making each of its exceptions runs. C# spells neither constructor, so
    /// the calls are emitted directly.
    [<Test>]
    let ``a multidimensional array constructor taking lower bounds can reject them`` () : unit =
        Net11Frontier.park
            "On .NET 11's CoreLib, `EscapeAnalysis.resourceMessageLookup` finds no `Environment.GetResourceStringLocal(string)`, the method its resource-lookup assumption (`ResourceLookup`) names, so the analysis stops: \"expected System.Private.CoreLib, Version=11.0.0.0 ... to define one Environment.GetResourceStringLocal(string), found 0\". Un-park when the assumption names each runtime's resource lookup."

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

        let int32Ref =
            metadata.AddTypeReference (
                (AssemblyReferenceHandle.op_Implicit corelibRef : EntityHandle),
                metadata.GetOrAddString "System",
                metadata.GetOrAddString "Int32"
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

        let vector =
            let code = InstructionEncoder (BlobBuilder ())
            code.LoadConstantI4 1
            code.OpCode ILOpCode.Newarr
            code.Token (TypeReferenceHandle.op_Implicit int32Ref : EntityHandle)
            code.OpCode ILOpCode.Pop
            code.OpCode ILOpCode.Ret
            bodies.AddMethodBody code

        metadata.AddMethodDefinition (
            MethodAttributes.Public ||| MethodAttributes.Static,
            MethodImplAttributes.IL,
            metadata.GetOrAddString "Vector",
            staticVoid,
            vector,
            MetadataTokens.ParameterHandle 1
        )
        |> ignore<MethodDefinitionHandle>

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

        let made (fullName : string) =
            let next, escapes = making analysis fullName
            analysis <- next
            Set.add ("=" + fullName) escapes

        // The runtime looks up the message of the `ArgumentOutOfRangeException` by its resource name
        // once its constructor has run.
        let lookup =
            let next, escapes =
                EscapeAnalysis.escapes
                    analysis
                    (methodNamed (hostCoreLib ()) "System.Environment" "GetResourceStringLocal")

            analysis <- next
            render analysis escapes

        let allocating =
            Set.union (made "System.OverflowException") (made "System.OutOfMemoryException")

        withBounds
        |> shouldEqual (
            allocating
            |> Set.union (made "System.ArgumentOutOfRangeException")
            |> Set.union lookup
        )

        withoutBounds |> shouldEqual allocating
        answer "Vector" |> shouldEqual allocating

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
            let key = methodNamed assembly "W.Binds" name
            let _, escapes = EscapeAnalysis.escapes analysis key

            escapes.Unknown |> shouldEqual true

            // The catch-all absorbs what the instruction may raise when it runs, but not the
            // failure to bind it, before the method runs.
            let _, sources = EscapeAnalysis.unknownSources analysis key

            sources
            |> shouldEqual (
                Set.singleton
                    {
                        Method = key
                        Offset = None
                        Reason = Opacity.DependsOnInstantiation
                        Names = None
                    }
            )

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
    /// `MakeException`, `Throw` and `MakeWrapper`, and `ThrowObject(object)` and
    /// `ThrowException(Exception)`, each of which throws its argument, carrying one
    /// `RuntimeCompatibilityAttribute` per blob in `attributes`, in order.
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
        // `Throw`, `MakeWrapper`, `ThrowObject`, `ThrowException`, then the cases in the order of
        // `raises` and `clauses`.
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

        // `ldarg.0; throw`, taking an argument of this type.
        let addThrowArgument (name : string) (argument : SignatureTypeEncoder -> unit) : unit =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .MethodSignature()
                .Parameters (
                    1,
                    (fun returnType -> returnType.Void ()),
                    (fun parameters -> argument (parameters.AddParameter().Type ()))
                )

            let code = InstructionEncoder (BlobBuilder ())
            code.LoadArgument 0
            code.OpCode ILOpCode.Throw

            metadata.AddMethodDefinition (
                MethodAttributes.Public ||| MethodAttributes.Static,
                MethodImplAttributes.IL,
                metadata.GetOrAddString name,
                metadata.GetOrAddBlob blob,
                bodies.AddMethodBody code,
                MetadataTokens.ParameterHandle 1
            )
            |> ignore<MethodDefinitionHandle>

        addThrowArgument "ThrowObject" (fun ty -> ty.Object ())
        addThrowArgument "ThrowException" (fun ty -> ty.Type (exceptionRef, false))

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

    [<Test>]
    let ``a thrown object that may not be an exception is wrapped, and what wrapping it raises escapes in its place``
        ()
        : unit
        =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let assembly =
            Assembly.read loggerFactory (Some "Throws.dll") (new MemoryStream (emitThrows []))

        let analysis = analysisOver [ assembly ] id

        // CoreCLR allocates a `RuntimeWrappedException` and runs its `(object)` constructor on it
        // (`WrapNonCompliantException`), and throws whatever either raises instead.
        let wrapperConstructor =
            let corelib = hostCoreLib ()

            match corelib.TryGetTopLevelTypeDef "System.Runtime.CompilerServices" "RuntimeWrappedException" with
            | None -> failwith "CoreLib defines no System.Runtime.CompilerServices.RuntimeWrappedException"
            | Some ty ->
                ty.Methods
                |> Seq.filter (fun m ->
                    m.Name = ".ctor"
                    && not m.IsStatic
                    && m.Signature.ParameterTypes = [ TypeDefn.PrimitiveType PrimitiveType.Object ]
                )
                |> Seq.exactlyOne
                |> fun m -> MethodKey.make corelib m.TryMetadata.Value.Handle

        let analysis, wrapping = EscapeAnalysis.escapes analysis wrapperConstructor

        let analysis, wrappingSources =
            EscapeAnalysis.unknownSources analysis wrapperConstructor

        let wrapped = Set.add "=System.OutOfMemoryException" (render analysis wrapping)

        // `object`, which may be an exception or not; `object` exactly, which is not; and an object
        // the analysis cannot name.
        let analysis =
            (analysis, [ "ThrowObject" ; "Throw" ; "Untyped" ])
            ||> List.fold (fun analysis methodName ->
                let key = methodNamed assembly "W.Throws" methodName
                let analysis, reached = EscapeAnalysis.reachable analysis key

                if not (reached.Contains wrapperConstructor) then
                    failwith $"%s{methodName} does not reach RuntimeWrappedException(object)"

                let analysis, escapes = EscapeAnalysis.escapes analysis key
                let analysis, sources = EscapeAnalysis.unknownSources analysis key

                Set.difference wrapped (render analysis escapes) |> shouldEqual Set.empty
                Set.isSubset wrapping.Assumes escapes.Assumes |> shouldEqual true

                if wrapping.Unknown then
                    escapes.Unknown |> shouldEqual true

                Set.isSubset wrappingSources sources |> shouldEqual true
                analysis
            )

        // An exception is thrown as itself, and null raises `NullReferenceException`, which the
        // runtime makes by its parameterless constructor.
        let analysis, reached =
            EscapeAnalysis.reachable analysis (methodNamed assembly "W.Throws" "ThrowException")

        reached.Contains wrapperConstructor |> shouldEqual false

        let analysis, throwsException =
            EscapeAnalysis.escapes analysis (methodNamed assembly "W.Throws" "ThrowException")

        let analysis, nullThrown = making analysis "System.NullReferenceException"

        render analysis throwsException
        |> shouldEqual (Set.union nullThrown (Set.ofList [ "<:System.Exception" ; "=System.NullReferenceException" ]))

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
        // `analysisOver` reads the host's CoreLib.
        let coreLibRuntime = EmulatedRuntime.ofCoreLib (hostCoreLib ())
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

                    let next, net11Frontier =
                        unknownOnlyThroughNet11MemoryQCalls
                            coreLibRuntime
                            analysis
                            (methodNamed fixture (ns + ".Runners") name)

                    analysis <- next

                    match claim with
                    | DispatchClaim.Precise ->
                        if (escapes.Unknown && not net11Frontier) || shownArithmetic <> reported then
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
                // Its dividend is 1, which no divisor overflows.
                yield "Hash_HashDivides", Set.singleton dividesByZero, DispatchClaim.Precise
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
            ((analysis, failures), [ "ClosedInside", arithmetic ; "HashInside", Set.singleton dividesByZero ])
            ||> List.fold (fun (analysis, failures) (name, expected) ->
                let analysis, escapes =
                    EscapeAnalysis.escapes analysis (methodNamed fixture "Dispatch.Shapes" name)

                let shown = render analysis escapes

                if
                    escapes.Unknown
                    || arithmetic |> Set.filter (fun ty -> shown.Contains ("=" + ty)) <> expected
                then
                    analysis,
                    failures
                    @ [
                        $"Shapes.%s{name}, uninstantiated: %A{Set.toList shown}, unknown %b{escapes.Unknown}; expected exactly %A{Set.toList expected} of the arithmetic exceptions, and nothing unknown"
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
                // The caller passes exactly an OpenDivides, whatever classes derive from it.
                "OpenParameter", both, DispatchClaim.Precise
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

    /// Callers of `Probe` through `callvirt` on what a call returns: the class of the object is
    /// what the callee's `ret`s return, which may be narrower than its declared return type.
    let private returnSource : string =
        """
namespace Returns;

public class Base { public virtual int Probe(int a, int b) => unchecked(a + b); }
public class Divides : Base { public override int Probe(int a, int b) => a / b; }
public sealed class SealedAdds : Base { public override int Probe(int a, int b) => checked(a + b); }
public class OpenDivides : Base { public override int Probe(int a, int b) => a / b; }

// As `ArrayPool<T>.Shared` is: a getter of the pool type returning a field of a sealed subclass.
public abstract class Pool<T>
{
    private static readonly SealedPool<T> s_shared = new SealedPool<T>();
    public static Pool<T> Shared => s_shared;
    public abstract int Probe(int a, int b);
}

public sealed class SealedPool<T> : Pool<T> { public override int Probe(int a, int b) => a / b; }

public static class Sources
{
    private static readonly SealedAdds s_adds = new SealedAdds();
    public static Base Field => s_adds;
    public static Base Fresh() => new Divides();
    public static Base Either(bool c) => c ? new Divides() : new SealedAdds();
    public static Base Relay() => Fresh();
    public static Base Passed(Base b) => b;
    public static Base Recurse(int n) => n <= 0 ? new Divides() : Recurse(n - 1);
    public static T Identity<T>(T x) => x;
}

public static class Runners
{
    public static int ThroughField(int a, int b) => Sources.Field.Probe(a, b);
    public static int ThroughFresh(int a, int b) => Sources.Fresh().Probe(a, b);
    public static int ThroughEither(int a, int b) => Sources.Either(b == 0).Probe(a, b);
    public static int ThroughRelay(int a, int b) => Sources.Relay().Probe(a, b);
    public static int ThroughShared(int a, int b) => Pool<int>.Shared.Probe(a, b);
    public static int ThroughIdentitySealed(int a, int b) => Sources.Identity(new SealedAdds()).Probe(a, b);
    public static int ThroughPassed(int a, int b) => Sources.Passed(new OpenDivides()).Probe(a, b);
    public static int ThroughRecursion(int a, int b) => Sources.Recurse(1).Probe(a, b);
}
"""

    [<Test>]
    let ``a callvirt on what a call returns runs the override of the class the callee returns`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let image =
            Roslyn.compileAssembly "Returns" OutputKind.DynamicallyLinkedLibrary [] [ returnSource ]

        let fixture =
            Assembly.read loggerFactory (Some "Returns.dll") (new MemoryStream (image))

        let both = Set.ofList [ dividesByZero ; overflows ]

        let cases =
            [
                "ThroughField", Set.singleton overflows, DispatchClaim.Precise
                "ThroughFresh", both, DispatchClaim.Precise
                "ThroughEither", both, DispatchClaim.Precise
                "ThroughRelay", both, DispatchClaim.Precise
                "ThroughShared", both, DispatchClaim.Precise
                "ThroughIdentitySealed", Set.singleton overflows, DispatchClaim.Precise
                // The callee returns what it is given, which the caller makes exactly an OpenDivides.
                "ThroughPassed", both, DispatchClaim.Precise
                // A return that depends on itself is its declared type.
                "ThroughRecursion", both, DispatchClaim.SoundOnly
            ]

        let runtime =
            cases
            |> List.map (fun (name, _, _) -> name)
            |> dispatchOnRealRuntime "Returns" image

        let _, failures =
            dispatchFailures fixture "Returns" runtime cases (analysisOver [ fixture ] id)

        match failures with
        | [] -> ()
        | failures -> failures |> String.concat Environment.NewLine |> failwith

    /// An image with two returns the analysis cannot follow, which no C# spells:
    ///   - `Runners.Walk`: `newobj SealedNode; L: callvirt Node::Next; dup; brtrue L; pop`, a loop
    ///     carrying a call's result back round, on the stack, as that same call's receiver;
    ///   - `Runners.Jumps`: `jmp Factory`, which returns what `Factory` does (a `Divides`), with no
    ///     `ret` of its own; `Runners.UseJump` calls `Probe` on what it returns.
    let private fabricateLoops () : byte[] =
        let builder =
            Emit.PersistedAssemblyBuilder (AssemblyName "Loops", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "Loops"

        let virtualAttributes =
            MethodAttributes.Public
            ||| MethodAttributes.Virtual
            ||| MethodAttributes.HideBySig
            ||| MethodAttributes.NewSlot

        let overrideAttributes =
            MethodAttributes.Public
            ||| MethodAttributes.Virtual
            ||| MethodAttributes.HideBySig

        let node =
            modul.DefineType ("Loops.Node", TypeAttributes.Public ||| TypeAttributes.Class)

        node.DefineDefaultConstructor MethodAttributes.Public
        |> ignore<Emit.ConstructorBuilder>

        let next = node.DefineMethod ("Next", virtualAttributes, node, [||])
        let il = next.GetILGenerator ()
        il.Emit Emit.OpCodes.Ldnull
        il.Emit Emit.OpCodes.Ret
        node.CreateType () |> ignore<Type>

        let sealedNode =
            modul.DefineType (
                "Loops.SealedNode",
                TypeAttributes.Public ||| TypeAttributes.Class ||| TypeAttributes.Sealed,
                node
            )

        let sealedNodeConstructor =
            sealedNode.DefineDefaultConstructor MethodAttributes.Public

        let sealedNext = sealedNode.DefineMethod ("Next", overrideAttributes, node, [||])
        let il = sealedNext.GetILGenerator ()
        il.Emit Emit.OpCodes.Ldnull
        il.Emit Emit.OpCodes.Ret
        sealedNode.CreateType () |> ignore<Type>

        let probeBase =
            modul.DefineType ("Loops.Base", TypeAttributes.Public ||| TypeAttributes.Class)

        probeBase.DefineDefaultConstructor MethodAttributes.Public
        |> ignore<Emit.ConstructorBuilder>

        let probe =
            probeBase.DefineMethod ("Probe", virtualAttributes, typeof<int>, [| typeof<int> ; typeof<int> |])

        let il = probe.GetILGenerator ()
        il.Emit Emit.OpCodes.Ldarg_1
        il.Emit Emit.OpCodes.Ldarg_2
        il.Emit Emit.OpCodes.Add
        il.Emit Emit.OpCodes.Ret
        probeBase.CreateType () |> ignore<Type>

        let divides =
            modul.DefineType ("Loops.Divides", TypeAttributes.Public ||| TypeAttributes.Class, probeBase)

        let dividesConstructor = divides.DefineDefaultConstructor MethodAttributes.Public

        let dividesProbe =
            divides.DefineMethod ("Probe", overrideAttributes, typeof<int>, [| typeof<int> ; typeof<int> |])

        let il = dividesProbe.GetILGenerator ()
        il.Emit Emit.OpCodes.Ldarg_1
        il.Emit Emit.OpCodes.Ldarg_2
        il.Emit Emit.OpCodes.Div
        il.Emit Emit.OpCodes.Ret
        divides.CreateType () |> ignore<Type>

        let runners =
            modul.DefineType (
                "Loops.Runners",
                TypeAttributes.Public
                ||| TypeAttributes.Abstract
                ||| TypeAttributes.Sealed
                ||| TypeAttributes.Class
            )

        let staticAttributes =
            MethodAttributes.Public
            ||| MethodAttributes.Static
            ||| MethodAttributes.HideBySig

        let walk =
            runners.DefineMethod ("Walk", staticAttributes, typeof<int>, [| typeof<int> ; typeof<int> |])

        let il = walk.GetILGenerator ()
        let loop = il.DefineLabel ()
        il.Emit (Emit.OpCodes.Newobj, sealedNodeConstructor)
        il.MarkLabel loop
        il.Emit (Emit.OpCodes.Callvirt, next)
        il.Emit Emit.OpCodes.Dup
        il.Emit (Emit.OpCodes.Brtrue, loop)
        il.Emit Emit.OpCodes.Pop
        il.Emit Emit.OpCodes.Ldc_I4_0
        il.Emit Emit.OpCodes.Ret

        let factory = runners.DefineMethod ("Factory", staticAttributes, probeBase, [||])
        let il = factory.GetILGenerator ()
        il.Emit (Emit.OpCodes.Newobj, dividesConstructor)
        il.Emit Emit.OpCodes.Ret

        let jumps = runners.DefineMethod ("Jumps", staticAttributes, probeBase, [||])
        let il = jumps.GetILGenerator ()
        il.Emit (Emit.OpCodes.Jmp, factory)

        let useJump =
            runners.DefineMethod ("UseJump", staticAttributes, typeof<int>, [| typeof<int> ; typeof<int> |])

        let il = useJump.GetILGenerator ()
        il.Emit (Emit.OpCodes.Call, jumps)
        il.Emit Emit.OpCodes.Ldarg_0
        il.Emit Emit.OpCodes.Ldarg_1
        il.Emit (Emit.OpCodes.Callvirt, probe)
        il.Emit Emit.OpCodes.Ret

        runners.CreateType () |> ignore<Type>

        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    [<Test>]
    let ``a return the analysis cannot follow is only its declared type`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let image = fabricateLoops ()

        let fixture =
            Assembly.read loggerFactory (Some "Loops.dll") (new MemoryStream (image))

        let cases =
            [
                // Answering at all is the main claim: it is a summary rather than a crash.
                "Walk", Set.empty, DispatchClaim.SoundOnly
                "UseJump", Set.ofList [ dividesByZero ; overflows ], DispatchClaim.SoundOnly
            ]

        let runtime =
            cases
            |> List.map (fun (name, _, _) -> name)
            |> dispatchOnRealRuntime "Loops" image

        let _, failures =
            dispatchFailures fixture "Loops" runtime cases (analysisOver [ fixture ] id)

        match failures with
        | [] -> ()
        | failures -> failures |> String.concat Environment.NewLine |> failwith

    /// An image whose branches test a value type that `newobj` made, which no C# spells, so that
    /// the branch tests the value rather than whether an object is null:
    ///   - `Runners.ZeroPointer`: `ldc.i4.0; newobj IntPtr(int); brtrue L; a / b`;
    ///   - `Runners.PassesZeroPointer`: passes that `IntPtr` to `Runners.Divides`, which branches
    ///     on it the same way.
    let private fabricateValueBranches () : byte[] =
        let builder =
            Emit.PersistedAssemblyBuilder (AssemblyName "Branches", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "Branches"

        let runners =
            modul.DefineType (
                "Branches.Runners",
                TypeAttributes.Public
                ||| TypeAttributes.Class
                ||| TypeAttributes.Abstract
                ||| TypeAttributes.Sealed
            )

        let staticAttributes = MethodAttributes.Public ||| MethodAttributes.Static
        let pointer = typeof<nativeint>.GetConstructor [| typeof<int> |]

        // `brtrue` over the `IntPtr` on the stack, then `a / b` from arguments `a` and `b`.
        let divideUnless (il : Emit.ILGenerator) (a : Emit.OpCode) (b : Emit.OpCode) =
            let skip = il.DefineLabel ()
            il.Emit (Emit.OpCodes.Brtrue, skip)
            il.Emit a
            il.Emit b
            il.Emit Emit.OpCodes.Div
            il.Emit Emit.OpCodes.Ret
            il.MarkLabel skip
            il.Emit Emit.OpCodes.Ldc_I4_0
            il.Emit Emit.OpCodes.Ret

        let zeroPointer =
            runners.DefineMethod ("ZeroPointer", staticAttributes, typeof<int>, [| typeof<int> ; typeof<int> |])

        let il = zeroPointer.GetILGenerator ()
        il.Emit Emit.OpCodes.Ldc_I4_0
        il.Emit (Emit.OpCodes.Newobj, pointer)
        divideUnless il Emit.OpCodes.Ldarg_0 Emit.OpCodes.Ldarg_1

        let divides =
            runners.DefineMethod (
                "Divides",
                staticAttributes,
                typeof<int>,
                [| typeof<nativeint> ; typeof<int> ; typeof<int> |]
            )

        let il = divides.GetILGenerator ()
        il.Emit Emit.OpCodes.Ldarg_0
        divideUnless il Emit.OpCodes.Ldarg_1 Emit.OpCodes.Ldarg_2

        let passesZeroPointer =
            runners.DefineMethod ("PassesZeroPointer", staticAttributes, typeof<int>, [| typeof<int> ; typeof<int> |])

        let il = passesZeroPointer.GetILGenerator ()
        il.Emit Emit.OpCodes.Ldc_I4_0
        il.Emit (Emit.OpCodes.Newobj, pointer)
        il.Emit Emit.OpCodes.Ldarg_0
        il.Emit Emit.OpCodes.Ldarg_1
        il.Emit (Emit.OpCodes.Call, divides)
        il.Emit Emit.OpCodes.Ret

        runners.CreateType () |> ignore<Type>

        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    [<Test>]
    let ``a branch on a value type that newobj made is not a test of whether it is null`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let image = fabricateValueBranches ()

        let fixture =
            Assembly.read loggerFactory (Some "Branches.dll") (new MemoryStream (image))

        let cases =
            [
                "ZeroPointer", Set.ofList [ dividesByZero ; overflows ], DispatchClaim.Precise
                "PassesZeroPointer", Set.ofList [ dividesByZero ; overflows ], DispatchClaim.Precise
            ]

        let runtime =
            cases
            |> List.map (fun (name, _, _) -> name)
            |> dispatchOnRealRuntime "Branches" image

        let _, failures =
            dispatchFailures fixture "Branches" runtime cases (analysisOver [ fixture ] id)

        match failures with
        | [] -> ()
        | failures -> failures |> String.concat Environment.NewLine |> failwith

    let private invalidProgram = "System.InvalidProgramException"
    let private typeLoad = "System.TypeLoadException"
    let private badImageFormat = "System.BadImageFormatException"

    /// An image of instructions the JIT refuses to compile a method over, for what their tokens
    /// name, which no C# spells: `box` and `newarr` of `Span<int>`, and `call` and `ldftn` of the
    /// abstract `Stream.Flush`. The JIT folds a `box` of a byref-like type away where the
    /// instructions after it in its block test or unbox it (`impBoxPatternMatch`), and refuses
    /// every other. Each method of `Refusals.Runners` but the generic ones takes a `bool` it may
    /// branch on; the generic ones, whose type parameter allows a byref-like type but for `BoxU`'s:
    ///   - `BoxT<T>(T)`, `box !!0; pop`, and `BoxTBranch<T>(T)`, `box !!0; brtrue`;
    ///   - `NewarrT<T>()`, `ldc.i4.0; newarr !!0; pop`;
    ///   - `BoxU<U>(U)`, `box !!0; pop`.
    ///
    /// The image is compiled against a `RefusalsDep` declaring `RefusalsDep.Gone`, which the one
    /// `refusalsDependency` makes does not.
    let private fabricateRefusals () : byte[] =
        // F# refuses `typeof` of a byref-like type (FS0412).
        let ofInt (definition : string) : Type =
            typeof<obj>.Assembly.GetType(definition, true).MakeGenericType [| typeof<int> |]

        let span = ofInt "System.Span`1"
        let readOnlySpan = ofInt "System.ReadOnlySpan`1"
        let empty = span.GetProperty("Empty").GetMethod
        let flush = typeof<Stream>.GetMethod ("Flush", Type.EmptyTypes)

        let gone =
            Emit
                .PersistedAssemblyBuilder(AssemblyName "RefusalsDep", typeof<obj>.Assembly)
                .DefineDynamicModule("RefusalsDep")
                .DefineType(
                    "RefusalsDep.Gone",
                    TypeAttributes.Public
                    ||| TypeAttributes.Sealed
                    ||| TypeAttributes.SequentialLayout,
                    typeof<ValueType>
                )
                .CreateType ()

        let builder =
            Emit.PersistedAssemblyBuilder (AssemblyName "Refusals", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "Refusals"

        let runners =
            modul.DefineType (
                "Refusals.Runners",
                TypeAttributes.Public
                ||| TypeAttributes.Class
                ||| TypeAttributes.Abstract
                ||| TypeAttributes.Sealed
            )

        let staticAttributes = MethodAttributes.Public ||| MethodAttributes.Static

        let byRefLike (name : string) : Emit.TypeBuilder =
            let ty =
                modul.DefineType (
                    name,
                    TypeAttributes.Public
                    ||| TypeAttributes.Sealed
                    ||| TypeAttributes.SequentialLayout,
                    typeof<ValueType>
                )

            ty.SetCustomAttribute (
                Emit.CustomAttributeBuilder (
                    typeof<Runtime.CompilerServices.IsByRefLikeAttribute>.GetConstructor Type.EmptyTypes,
                    [||]
                )
            )

            ty

        // A byref-like type that a type argument instantiates, so that code over `Holder<string>`
        // is shared.
        let holder =
            let ty = byRefLike "Refusals.Holder"

            ty.DefineGenericParameters [| "T" |]
            |> ignore<Emit.GenericTypeParameterBuilder[]>

            ty.CreateType ()

        // A byref-like type implementing `IDynamicInterfaceCastable`, which no value type is to the
        // runtime, so that the JIT decides a cast from it.
        let castable =
            let ty = byRefLike "Refusals.Castable"
            ty.AddInterfaceImplementation typeof<Runtime.InteropServices.IDynamicInterfaceCastable>

            let implementation (name : string) (returns : Type) (parameters : Type[]) (emit : Emit.OpCode) =
                let il =
                    ty
                        .DefineMethod(
                            name,
                            MethodAttributes.Public
                            ||| MethodAttributes.Virtual
                            ||| MethodAttributes.Final
                            ||| MethodAttributes.NewSlot
                            ||| MethodAttributes.HideBySig,
                            returns,
                            parameters
                        )
                        .GetILGenerator ()

                il.Emit emit
                il.Emit Emit.OpCodes.Ret

            implementation
                "GetInterfaceImplementation"
                typeof<RuntimeTypeHandle>
                [| typeof<RuntimeTypeHandle> |]
                Emit.OpCodes.Ldarg_1

            implementation
                "IsInterfaceImplemented"
                typeof<bool>
                [| typeof<RuntimeTypeHandle> ; typeof<bool> |]
                Emit.OpCodes.Ldc_I4_0

            ty.CreateType ()

        let generic
            (name : string)
            (allowsByRefLike : bool)
            (takesArgument : bool)
            (emit : Emit.ILGenerator -> Type -> unit)
            : Emit.MethodBuilder
            =
            let method = runners.DefineMethod (name, staticAttributes)
            let parameter = method.DefineGenericParameters([| "T" |]).[0]

            if allowsByRefLike then
                parameter.SetGenericParameterAttributes GenericParameterAttributes.AllowByRefLike

            method.SetReturnType typeof<int>
            method.SetParameters (if takesArgument then [| parameter :> Type |] else [||])
            let il = method.GetILGenerator ()
            emit il parameter
            il.Emit Emit.OpCodes.Ldc_I4_1
            il.Emit Emit.OpCodes.Ret
            method

        let boxT =
            generic
                "BoxT"
                true
                true
                (fun il parameter ->
                    il.Emit Emit.OpCodes.Ldarg_0
                    il.Emit (Emit.OpCodes.Box, parameter)
                    il.Emit Emit.OpCodes.Pop
                )

        let boxTBranch =
            generic
                "BoxTBranch"
                true
                true
                (fun il parameter ->
                    let next = il.DefineLabel ()
                    il.Emit Emit.OpCodes.Ldarg_0
                    il.Emit (Emit.OpCodes.Box, parameter)
                    il.Emit (Emit.OpCodes.Brtrue, next)
                    il.MarkLabel next
                )

        let boxTCast =
            generic
                "BoxTCast"
                true
                true
                (fun il parameter ->
                    let next = il.DefineLabel ()
                    il.Emit Emit.OpCodes.Ldarg_0
                    il.Emit (Emit.OpCodes.Box, parameter)
                    il.Emit (Emit.OpCodes.Isinst, typeof<obj>)
                    il.Emit (Emit.OpCodes.Brtrue, next)
                    il.MarkLabel next
                )

        // `box !!0; isinst target; brtrue`.
        let boxTCastTo (name : string) (target : Type) =
            generic
                name
                true
                true
                (fun il parameter ->
                    let next = il.DefineLabel ()
                    il.Emit Emit.OpCodes.Ldarg_0
                    il.Emit (Emit.OpCodes.Box, parameter)
                    il.Emit (Emit.OpCodes.Isinst, target)
                    il.Emit (Emit.OpCodes.Brtrue, next)
                    il.MarkLabel next
                )

        let boxTCastDisposable = boxTCastTo "BoxTCastDisposable" typeof<IDisposable>
        let boxTCastEquatable = boxTCastTo "BoxTCastEquatable" typeof<IEquatable<int>>

        // `box !!0; unbox.any !!0; pop`, with these implementation flags.
        let boxTUnboxWith (name : string) (flags : MethodImplAttributes) =
            let method =
                generic
                    name
                    true
                    true
                    (fun il parameter ->
                        il.Emit Emit.OpCodes.Ldarg_0
                        il.Emit (Emit.OpCodes.Box, parameter)
                        il.Emit (Emit.OpCodes.Unbox_Any, parameter)
                        il.Emit Emit.OpCodes.Pop
                    )

            method.SetImplementationFlags flags
            method

        let boxTUnbox = boxTUnboxWith "BoxTUnbox" MethodImplAttributes.IL

        let boxTUnboxNoInlining =
            boxTUnboxWith "BoxTUnboxNoInlining" MethodImplAttributes.NoInlining

        let boxTUnboxNoOptimization =
            boxTUnboxWith "BoxTUnboxNoOptimization" MethodImplAttributes.NoOptimization

        let newarrT =
            generic
                "NewarrT"
                true
                false
                (fun il parameter ->
                    il.Emit Emit.OpCodes.Ldc_I4_0
                    il.Emit (Emit.OpCodes.Newarr, parameter)
                    il.Emit Emit.OpCodes.Pop
                )

        let boxU =
            generic
                "BoxU"
                false
                true
                (fun il parameter ->
                    il.Emit Emit.OpCodes.Ldarg_0
                    il.Emit (Emit.OpCodes.Box, parameter)
                    il.Emit Emit.OpCodes.Pop
                )

        let define (name : string) (emit : Emit.ILGenerator -> unit) =
            runners.DefineMethod(name, staticAttributes, typeof<int>, [| typeof<bool> |]).GetILGenerator ()
            |> emit

        let returns (il : Emit.ILGenerator) (value : int) =
            il.Emit (Emit.OpCodes.Ldc_I4, value)
            il.Emit Emit.OpCodes.Ret

        let boxed (il : Emit.ILGenerator) =
            il.Emit (Emit.OpCodes.Call, empty)
            il.Emit (Emit.OpCodes.Box, span)

        // `op` to a return of 2, past a return of 1.
        let branches (op : Emit.OpCode) (il : Emit.ILGenerator) =
            let taken = il.DefineLabel ()
            il.Emit (op, taken)
            returns il 1
            il.MarkLabel taken
            returns il 2

        // `emit` only where the argument is true.
        let onlyIf (emit : Emit.ILGenerator -> unit) (il : Emit.ILGenerator) =
            let skip = il.DefineLabel ()
            il.Emit Emit.OpCodes.Ldarg_0
            il.Emit (Emit.OpCodes.Brfalse, skip)
            emit il
            il.MarkLabel skip
            returns il 0

        // `before`, then `branches`, where a branch from above also lands on the branch.
        let landedOn (before : Emit.ILGenerator -> unit) (il : Emit.ILGenerator) =
            let pushes = il.DefineLabel ()
            let tests = il.DefineLabel ()
            il.Emit Emit.OpCodes.Ldarg_0
            il.Emit (Emit.OpCodes.Brfalse, pushes)
            il.Emit Emit.OpCodes.Ldnull
            il.Emit (Emit.OpCodes.Br, tests)
            il.MarkLabel pushes
            before il
            il.MarkLabel tests
            branches Emit.OpCodes.Brtrue il

        let popped (emit : Emit.ILGenerator -> unit) (il : Emit.ILGenerator) =
            emit il
            il.Emit Emit.OpCodes.Pop

        let newarr (il : Emit.ILGenerator) =
            il.Emit Emit.OpCodes.Ldc_I4_0
            il.Emit (Emit.OpCodes.Newarr, span)

        let callsFlush (il : Emit.ILGenerator) =
            il.Emit Emit.OpCodes.Ldnull
            il.Emit (Emit.OpCodes.Call, flush)

        let calls (callee : MethodInfo) (pushes : Emit.ILGenerator -> unit) (il : Emit.ILGenerator) =
            pushes il
            il.Emit (Emit.OpCodes.Call, callee)
            il.Emit Emit.OpCodes.Ret

        let pushesSpan (il : Emit.ILGenerator) = il.Emit (Emit.OpCodes.Call, empty)
        let pushesInt (il : Emit.ILGenerator) = il.Emit Emit.OpCodes.Ldc_I4_0

        define
            "BoxPop"
            (fun il ->
                popped boxed il
                returns il 0
            )

        define
            "BoxBrtrue"
            (fun il ->
                boxed il
                branches Emit.OpCodes.Brtrue il
            )

        define
            "BoxBrfalse"
            (fun il ->
                boxed il
                branches Emit.OpCodes.Brfalse il
            )

        define
            "BoxIsinstObjectBrtrue"
            (fun il ->
                boxed il
                il.Emit (Emit.OpCodes.Isinst, typeof<obj>)
                branches Emit.OpCodes.Brtrue il
            )

        define
            "BoxIsinstDisposableBrfalse"
            (fun il ->
                boxed il
                il.Emit (Emit.OpCodes.Isinst, typeof<IDisposable>)
                branches Emit.OpCodes.Brfalse il
            )

        define
            "BoxIsinstLdnullCgtun"
            (fun il ->
                boxed il
                il.Emit (Emit.OpCodes.Isinst, typeof<obj>)
                il.Emit Emit.OpCodes.Ldnull
                il.Emit Emit.OpCodes.Cgt_Un
                il.Emit Emit.OpCodes.Ret
            )

        define
            "BoxIsinstPop"
            (fun il ->
                boxed il
                il.Emit (Emit.OpCodes.Isinst, typeof<obj>)
                il.Emit Emit.OpCodes.Pop
                returns il 0
            )

        for name, unboxed in [ "BoxUnboxSame", span ; "BoxUnboxOther", readOnlySpan ] do
            define
                name
                (fun il ->
                    boxed il
                    il.Emit (Emit.OpCodes.Unbox_Any, unboxed)
                    il.Emit Emit.OpCodes.Pop
                    returns il 0
                )

        for name, cast in [ "BoxIsinstSameUnboxSame", span ; "BoxIsinstObjectUnboxSame", typeof<obj> ] do
            define
                name
                (fun il ->
                    boxed il
                    il.Emit (Emit.OpCodes.Isinst, cast)
                    il.Emit (Emit.OpCodes.Unbox_Any, span)
                    il.Emit Emit.OpCodes.Pop
                    returns il 0
                )

        define "BoxBranchLandedOn" (landedOn boxed)

        define
            "BoxIsinstBranchLandedOn"
            (landedOn (fun il ->
                boxed il
                il.Emit (Emit.OpCodes.Isinst, typeof<obj>)
            ))

        define
            "BoxInTry"
            (fun il ->
                il.BeginExceptionBlock () |> ignore<Emit.Label>
                popped boxed il
                il.BeginCatchBlock typeof<exn>
                il.Emit Emit.OpCodes.Pop
                il.EndExceptionBlock ()
                returns il 0
            )

        // A protected block starting between the box and the branch starts a block there too. The
        // IL is invalid, the stack not being empty where the protected block starts, but the JIT
        // refuses it over the box.
        define
            "BoxBranchInTry"
            (fun il ->
                let result = il.DeclareLocal typeof<int>
                boxed il
                il.BeginExceptionBlock () |> ignore<Emit.Label>
                let next = il.DefineLabel ()
                il.Emit (Emit.OpCodes.Brtrue, next)
                il.MarkLabel next
                il.Emit Emit.OpCodes.Ldc_I4_2
                il.Emit (Emit.OpCodes.Stloc, result)
                il.BeginCatchBlock typeof<exn>
                il.Emit Emit.OpCodes.Pop
                il.EndExceptionBlock ()
                il.Emit (Emit.OpCodes.Ldloc, result)
                il.Emit Emit.OpCodes.Ret
            )

        // The JIT resolves the token of what follows the box as it matches the pattern, and raises
        // there where it does not bind, rather than refusing the box.
        define
            "BoxUnboxGone"
            (fun il ->
                boxed il
                il.Emit (Emit.OpCodes.Unbox_Any, gone)
                il.Emit Emit.OpCodes.Pop
                returns il 0
            )

        define
            "BoxIsinstGoneBrtrue"
            (fun il ->
                boxed il
                il.Emit (Emit.OpCodes.Isinst, gone)
                branches Emit.OpCodes.Brtrue il
            )

        for name, cast in [ "BoxIsinstSameUnboxGone", span ; "BoxIsinstObjectUnboxGone", typeof<obj> ] do
            define
                name
                (fun il ->
                    boxed il
                    il.Emit (Emit.OpCodes.Isinst, cast)
                    il.Emit (Emit.OpCodes.Unbox_Any, gone)
                    il.Emit Emit.OpCodes.Pop
                    returns il 0
                )

        define "BoxIfArg" (onlyIf (popped boxed))

        define
            "BoxDead"
            (fun il ->
                let past = il.DefineLabel ()
                il.Emit (Emit.OpCodes.Br, past)
                popped boxed il
                il.MarkLabel past
                returns il 0
            )

        define
            "BoxPastCapabilityBranch"
            (fun il ->
                let skip = il.DefineLabel ()

                il.Emit (
                    Emit.OpCodes.Call,
                    typeof<Runtime.Intrinsics.Vector128>.GetProperty("IsHardwareAccelerated").GetMethod
                )

                il.Emit (Emit.OpCodes.Brtrue, skip)
                popped boxed il
                il.MarkLabel skip
                returns il 0
            )

        define
            "BoxPastNewobjBranch"
            (fun il ->
                let skip = il.DefineLabel ()
                il.Emit (Emit.OpCodes.Newobj, typeof<obj>.GetConstructor Type.EmptyTypes)
                il.Emit (Emit.OpCodes.Brtrue, skip)
                popped boxed il
                il.MarkLabel skip
                returns il 0
            )

        define
            "NewarrPop"
            (fun il ->
                popped newarr il
                returns il 0
            )

        define "NewarrIfArg" (onlyIf (popped newarr))

        define
            "CallAbstract"
            (fun il ->
                callsFlush il
                returns il 0
            )

        define "CallAbstractIfArg" (onlyIf callsFlush)

        define
            "LdftnAbstract"
            (fun il ->
                il.Emit (Emit.OpCodes.Ldftn, flush)
                il.Emit Emit.OpCodes.Pop
                returns il 0
            )

        define "CallsBoxTSpan" (calls (boxT.MakeGenericMethod [| span |]) pushesSpan)
        define "CallsBoxTInt" (calls (boxT.MakeGenericMethod [| typeof<int> |]) pushesInt)
        define "CallsBoxTBranchSpan" (calls (boxTBranch.MakeGenericMethod [| span |]) pushesSpan)
        define "CallsNewarrTSpan" (calls (newarrT.MakeGenericMethod [| span |]) ignore)
        define "CallsNewarrTInt" (calls (newarrT.MakeGenericMethod [| typeof<int> |]) ignore)
        define "CallsBoxUInt" (calls (boxU.MakeGenericMethod [| typeof<int> |]) pushesInt)

        // A default value of `ty`, through a local.
        let pushesDefault (ty : Type) (il : Emit.ILGenerator) =
            let local = il.DeclareLocal ty
            il.Emit (Emit.OpCodes.Ldloca, local)
            il.Emit (Emit.OpCodes.Initobj, ty)
            il.Emit (Emit.OpCodes.Ldloc, local)

        let holderOf (argument : Type) : Type = holder.MakeGenericType [| argument |]

        define
            "BoxIsinstNullableBrtrue"
            (fun il ->
                boxed il
                il.Emit (Emit.OpCodes.Isinst, typeof<Nullable<int>>)
                branches Emit.OpCodes.Brtrue il
            )

        define
            "BoxCastableIsinstBrtrue"
            (fun il ->
                pushesDefault castable il
                il.Emit (Emit.OpCodes.Box, castable)
                il.Emit (Emit.OpCodes.Isinst, typeof<obj>)
                branches Emit.OpCodes.Brtrue il
            )

        for name, argument in [ "Shared", typeof<string> ; "Unshared", typeof<int> ] do
            let held = holderOf argument
            define $"CallsBoxTCast%s{name}" (calls (boxTCast.MakeGenericMethod [| held |]) (pushesDefault held))
            define $"CallsBoxTUnbox%s{name}" (calls (boxTUnbox.MakeGenericMethod [| held |]) (pushesDefault held))

        let shared = holderOf typeof<string>

        for name, callee in
            [
                "CallsBoxTCastDisposableShared", boxTCastDisposable
                "CallsBoxTCastEquatableShared", boxTCastEquatable
                "CallsBoxTUnboxNoInliningShared", boxTUnboxNoInlining
                "CallsBoxTUnboxNoOptimizationShared", boxTUnboxNoOptimization
            ] do
            define name (calls (callee.MakeGenericMethod [| shared |]) (pushesDefault shared))

        runners.CreateType () |> ignore<Type>

        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    /// The `RefusalsDep` that `fabricateRefusals`' image runs against, which declares no
    /// `RefusalsDep.Gone`.
    let private refusalsDependency () : byte[] =
        let builder =
            Emit.PersistedAssemblyBuilder (AssemblyName "RefusalsDep", typeof<obj>.Assembly)

        builder.DefineDynamicModule "RefusalsDep" |> ignore<Emit.ModuleBuilder>
        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    /// What the JIT does compiling a runner of `fabricateRefusals`, or the method it calls,
    /// whatever its argument.
    [<RequireQualifiedAccess>]
    type private Refusal =
        | Compiles
        /// It refuses, raising this.
        | Raises of string
        /// It refuses the callee's shared code, raising this, but not where it inlines the callee,
        /// which it may: there it sees the callee's types as they are. The analysis reports it.
        | UnlessInlined of string
        /// It refuses, raising `raised`. The analysis reports `bound` too, which binding a token
        /// the JIT never resolves would raise: it binds every token the JIT compiles (`bind`).
        | RaisesBesideBinding of raised : string * bound : string

    /// Each runner of `fabricateRefusals`, and what the JIT does compiling it.
    let private refusals : (string * Refusal) list =
        [
            "BoxPop", Refusal.Raises invalidProgram
            "BoxBrtrue", Refusal.Compiles
            "BoxBrfalse", Refusal.Compiles
            "BoxIsinstObjectBrtrue", Refusal.Compiles
            "BoxIsinstDisposableBrfalse", Refusal.Compiles
            // The JIT folds `isinst; ldnull; cgt.un` after a box of any other type.
            "BoxIsinstLdnullCgtun", Refusal.Raises invalidProgram
            "BoxIsinstPop", Refusal.Raises invalidProgram
            "BoxUnboxSame", Refusal.Compiles
            "BoxUnboxOther", Refusal.Raises invalidProgram
            "BoxIsinstSameUnboxSame", Refusal.Compiles
            "BoxIsinstObjectUnboxSame", Refusal.Raises invalidProgram
            // A branch landing on the instruction after the box starts a block there, and the JIT
            // matches a pattern only within the box's block.
            "BoxBranchLandedOn", Refusal.Raises invalidProgram
            "BoxIsinstBranchLandedOn", Refusal.Raises invalidProgram
            // The method's own handlers never see what compiling it raises.
            "BoxInTry", Refusal.Raises invalidProgram
            // Nor does it matter whether a run reaches the instruction, so long as the JIT does.
            "BoxIfArg", Refusal.Raises invalidProgram
            "BoxDead", Refusal.Compiles
            // Nor where the JIT folds a branch on a capability query past it.
            "BoxPastCapabilityBranch", Refusal.Compiles
            // No run reaches the box, a branch on an object `newobj` made always being taken, but
            // the JIT compiles it.
            "BoxPastNewobjBranch", Refusal.Raises invalidProgram
            "NewarrPop", Refusal.Raises typeLoad
            "NewarrIfArg", Refusal.Raises typeLoad
            "CallAbstract", Refusal.Raises badImageFormat
            "CallAbstractIfArg", Refusal.Raises badImageFormat
            "LdftnAbstract", Refusal.Raises badImageFormat
            "CallsBoxTSpan", Refusal.Raises invalidProgram
            "CallsBoxTInt", Refusal.Compiles
            "CallsBoxTBranchSpan", Refusal.Compiles
            "CallsNewarrTSpan", Refusal.Raises typeLoad
            "CallsNewarrTInt", Refusal.Compiles
            "CallsBoxUInt", Refusal.Compiles
            // The JIT decides no cast to a `Nullable<T>`.
            "BoxIsinstNullableBrtrue", Refusal.Raises invalidProgram
            "BoxCastableIsinstBrtrue", Refusal.Compiles
            // Code over `Holder<string>` is shared, and sees `Holder<__Canon>`, which it does not
            // decide is any type in particular.
            "CallsBoxTCastShared", Refusal.UnlessInlined invalidProgram
            "CallsBoxTUnboxShared", Refusal.UnlessInlined invalidProgram
            "CallsBoxTCastUnshared", Refusal.Compiles
            "CallsBoxTUnboxUnshared", Refusal.Compiles
            // It decides a cast from a canonical form to an interface with no type arguments all
            // the same.
            "CallsBoxTCastDisposableShared", Refusal.Compiles
            // But to one with them only where the canonical form casts to it, which PawPrint cannot
            // tell; this one does not.
            "CallsBoxTCastEquatableShared", Refusal.UnlessInlined invalidProgram
            // It never inlines a method that forbids it, nor one marked `NoOptimization`.
            "CallsBoxTUnboxNoInliningShared", Refusal.Raises invalidProgram
            "CallsBoxTUnboxNoOptimizationShared", Refusal.Raises invalidProgram
            // A protected block's bounds start blocks.
            "BoxBranchInTry", Refusal.Raises invalidProgram
            // The token of what follows the box does not bind, which the JIT raises instead.
            "BoxUnboxGone", Refusal.Raises typeLoad
            "BoxIsinstGoneBrtrue", Refusal.Raises typeLoad
            "BoxIsinstSameUnboxGone", Refusal.Raises typeLoad
            // It resolves the `unbox.any`'s token only once it proves the `isinst`'s type is the
            // boxed type.
            "BoxIsinstObjectUnboxGone", Refusal.RaisesBesideBinding (invalidProgram, typeLoad)
        ]

    [<Test>]
    let ``the JIT refuses a method whose instruction names what it cannot compile, wherever it is`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let image = fabricateRefusals ()
        let dependencyImage = refusalsDependency ()

        let fixture =
            Assembly.read loggerFactory (Some "Refusals.dll") (new MemoryStream (image))

        let dependency =
            Assembly.read loggerFactory (Some "RefusalsDep.dll") (new MemoryStream (dependencyImage))

        // The fixture exercises what it claims to: the real runtime raises exactly that exception,
        // for either argument, or none.
        let context =
            System.Runtime.Loader.AssemblyLoadContext ("Refusals", isCollectible = true)

        context.add_Resolving (
            Func<_, _, _> (fun (context : Runtime.Loader.AssemblyLoadContext) (name : AssemblyName) ->
                match name.Name with
                | "RefusalsDep" -> context.LoadFromStream (new MemoryStream (dependencyImage))
                | _ -> null
            )
        )

        let runtimeFailures =
            try
                let ty = context.LoadFromStream(new MemoryStream (image)).GetType "Refusals.Runners"

                [
                    for name, refused in refusals do
                        let raised =
                            [ false ; true ]
                            |> List.choose (fun argument ->
                                try
                                    ty.GetMethod(name).Invoke ((null : obj), [| box argument |]) |> ignore<obj>
                                    None
                                with :? TargetInvocationException as e ->
                                    Some (e.InnerException.GetType().FullName)
                            )

                        let expected =
                            match refused with
                            | Refusal.Compiles -> [ [] ]
                            | Refusal.Raises refusal -> [ List.replicate 2 refusal ]
                            | Refusal.UnlessInlined refusal -> [ [] ; List.replicate 2 refusal ]
                            | Refusal.RaisesBesideBinding (raised, _) -> [ List.replicate 2 raised ]

                        if not (List.contains raised expected) then
                            yield $"%s{name}: the real runtime raised %A{raised}, not one of %A{expected}"
                ]
            finally
                context.Unload ()

        let refusalTypes = Set.ofList [ invalidProgram ; typeLoad ; badImageFormat ]

        // Every CPU .NET supports accelerates `Vector128`, as this profile says.
        let profile =
            { HardwareIntrinsicsProfile.ScalarOnly with
                IsHardwareAccelerated =
                    Set.singleton
                        {
                            Namespace = "System.Runtime.Intrinsics"
                            Path = [ "Vector128" ]
                        }
            }

        let mutable analysis = analysisUnder profile [ fixture ; dependency ] id

        let shownRefusals (shown : Set<string>) : Set<string> =
            refusalTypes |> Set.filter (fun ty -> shown.Contains ("=" + ty))

        // What the runtime runs to make the exception of a refusal, which escapes in its place
        // (`ExceptionMaking`): what that raises, and where it is opaque.
        let made (refusal : string) : Set<string> * Set<string> =
            let maker =
                if refusal = typeLoad then
                    typeLoadConstructor ()
                else
                    let dot = refusal.LastIndexOf '.'
                    parameterlessConstructor (hostCoreLib ()) (refusal.Substring (0, dot)) (refusal.Substring (dot + 1))

            let next, escapes = EscapeAnalysis.escapes analysis maker
            let next, sources = EscapeAnalysis.unknownSources next maker
            analysis <- next
            render analysis escapes, Set.map (renderSite analysis) sources

        let analysisFailures =
            [
                for name, refused in refusals do
                    let key = methodNamed fixture "Refusals.Runners" name
                    let next, escapes = EscapeAnalysis.escapes analysis key
                    let next, sources = EscapeAnalysis.unknownSources next key
                    analysis <- next
                    let shown = render analysis escapes
                    let opaque = Set.map (renderSite analysis) sources

                    // Nothing is opaque, and no other refusal is reported, but what making the
                    // exception is and reports.
                    let reported =
                        match refused with
                        | Refusal.Compiles -> []
                        | Refusal.Raises refusal
                        | Refusal.UnlessInlined refusal -> [ refusal ]
                        | Refusal.RaisesBesideBinding (raised, bound) -> [ raised ; bound ]

                    let madeShown, madeOpaque =
                        let made = List.map made reported
                        Set.unionMany (List.map fst made), Set.unionMany (List.map snd made)

                    let expected = Set.union (Set.ofList reported) (shownRefusals madeShown)

                    if not (Set.isSubset opaque madeOpaque) || shownRefusals shown <> expected then
                        yield
                            $"%s{name}: %A{Set.toList shown}, opaque at %A{Set.toList opaque}; expected %A{Set.toList expected} of the refusals, and nothing opaque beyond making it"
            ]

        match runtimeFailures @ analysisFailures with
        | [] -> ()
        | failures -> failures |> String.concat Environment.NewLine |> failwith

    /// The type tokens a runner of `modifiedOperands` names.
    type private ModifiedOperands =
        {
            /// `Span<int>`
            Span : EntityHandle
            /// `Span<int modopt(IsConst)>`
            ModifiedSpan : EntityHandle
            /// `ValueTuple<int>`
            Tuple : EntityHandle
            /// `ValueTuple<int modopt(IsConst)>`
            ModifiedTuple : EntityHandle
            /// `List<int modopt(IsConst)>`
            ModifiedList : EntityHandle
            /// `Span<System.Gone modopt(IsConst)>`, which CoreLib declares no `System.Gone` for
            ModifiedGoneSpan : EntityHandle
            /// `Probe.Marked<int modopt(IsConst)>`, of a class carrying `IsByRefLikeAttribute`, which
            /// makes no class byref-like
            ModifiedMarked : EntityHandle
        }

    /// An image whose static `Probe.Runners` has a parameterless `void` method of each name in
    /// `runners`, each with locals `Span<int>` and `ValueTuple<int>` and the body `runners` writes
    /// for it. It emits the image with System.Reflection.Metadata, because neither C# nor
    /// `Reflection.Emit` spells a generic argument carrying a custom modifier.
    let private modifiedOperands (runners : (string * (ModifiedOperands -> InstructionEncoder -> unit)) list) : byte[] =
        let metadata = MetadataBuilder ()
        let ilStream = BlobBuilder ()
        let bodies = MethodBodyStreamEncoder ilStream

        metadata.AddModule (
            0,
            metadata.GetOrAddString "Probe.dll",
            metadata.GetOrAddGuid (Guid "0f7b3c52-8e1d-4a96-b2c4-7d5e6a183f90"),
            Unchecked.defaultof<GuidHandle>,
            Unchecked.defaultof<GuidHandle>
        )
        |> ignore<ModuleDefinitionHandle>

        metadata.AddAssembly (
            metadata.GetOrAddString "Probe",
            Version (1, 0, 0, 0),
            Unchecked.defaultof<StringHandle>,
            Unchecked.defaultof<BlobHandle>,
            Unchecked.defaultof<AssemblyFlags>,
            AssemblyHashAlgorithm.None
        )
        |> ignore<AssemblyDefinitionHandle>

        let coreName = typeof<obj>.Assembly.GetName ()

        let core =
            metadata.AddAssemblyReference (
                metadata.GetOrAddString coreName.Name,
                coreName.Version,
                Unchecked.defaultof<StringHandle>,
                metadata.GetOrAddBlob (coreName.GetPublicKeyToken ()),
                Unchecked.defaultof<AssemblyFlags>,
                Unchecked.defaultof<BlobHandle>
            )

        let typeRef (ns : string) (name : string) : EntityHandle =
            metadata.AddTypeReference (
                (AssemblyReferenceHandle.op_Implicit core : EntityHandle),
                metadata.GetOrAddString ns,
                metadata.GetOrAddString name
            )
            |> TypeReferenceHandle.op_Implicit

        let objectRef = typeRef "System" "Object"
        let spanRef = typeRef "System" "Span`1"
        let tupleRef = typeRef "System" "ValueTuple`1"
        let listRef = typeRef "System.Collections.Generic" "List`1"
        let isConstRef = typeRef "System.Runtime.CompilerServices" "IsConst"
        let goneRef = typeRef "System" "Gone"
        let byRefLikeRef = typeRef "System.Runtime.CompilerServices" "IsByRefLikeAttribute"
        // `<Module>`, `Probe.Runners`, then this.
        let marked = MetadataTokens.TypeDefinitionHandle 3

        let typeSpec (encode : SignatureTypeEncoder -> unit) : EntityHandle =
            let blob = BlobBuilder ()
            encode (BlobEncoder(blob).TypeSpecificationSignature ())

            metadata.AddTypeSpecification (metadata.GetOrAddBlob blob)
            |> TypeSpecificationHandle.op_Implicit

        let instance
            (root : EntityHandle)
            (valueType : bool)
            (modified : bool)
            (spellArgument : SignatureTypeEncoder -> unit)
            (encoder : SignatureTypeEncoder)
            : unit
            =
            let argument = encoder.GenericInstantiation(root, 1, valueType).AddArgument ()

            if modified then
                argument.CustomModifiers().AddModifier (isConstRef, true)
                |> ignore<CustomModifiersEncoder>

            spellArgument argument

        let int32 (encoder : SignatureTypeEncoder) : unit = encoder.Int32 ()

        let operands =
            {
                Span = typeSpec (instance spanRef true false int32)
                ModifiedSpan = typeSpec (instance spanRef true true int32)
                Tuple = typeSpec (instance tupleRef true false int32)
                ModifiedTuple = typeSpec (instance tupleRef true true int32)
                ModifiedList = typeSpec (instance listRef false true int32)
                ModifiedGoneSpan =
                    typeSpec (instance spanRef true true (fun argument -> argument.Type (goneRef, false)))
                ModifiedMarked = typeSpec (instance (TypeDefinitionHandle.op_Implicit marked) false true int32)
            }

        let locals =
            let blob = BlobBuilder ()
            let variables = BlobEncoder(blob).LocalVariableSignature 2
            instance spanRef true false int32 (variables.AddVariable().Type ())
            instance tupleRef true false int32 (variables.AddVariable().Type ())
            metadata.AddStandaloneSignature (metadata.GetOrAddBlob blob)

        let signature =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .MethodSignature()
                .Parameters (0, (fun returns -> returns.Void ()), ignore<ParametersEncoder>)

            metadata.GetOrAddBlob blob

        for name, write in runners do
            let code = InstructionEncoder (BlobBuilder (), ControlFlowBuilder ())
            write operands code

            let body =
                bodies.AddMethodBody (
                    code,
                    maxStack = 2,
                    localVariablesSignature = locals,
                    attributes = MethodBodyAttributes.InitLocals
                )

            metadata.AddMethodDefinition (
                MethodAttributes.Public ||| MethodAttributes.Static,
                MethodImplAttributes.IL,
                metadata.GetOrAddString name,
                signature,
                body,
                MetadataTokens.ParameterHandle 1
            )
            |> ignore<MethodDefinitionHandle>

        metadata.AddTypeDefinition (
            Unchecked.defaultof<TypeAttributes>,
            Unchecked.defaultof<StringHandle>,
            metadata.GetOrAddString "<Module>",
            Unchecked.defaultof<EntityHandle>,
            MetadataTokens.FieldDefinitionHandle 1,
            MetadataTokens.MethodDefinitionHandle 1
        )
        |> ignore<TypeDefinitionHandle>

        metadata.AddTypeDefinition (
            TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed,
            metadata.GetOrAddString "Probe",
            metadata.GetOrAddString "Runners",
            objectRef,
            MetadataTokens.FieldDefinitionHandle 1,
            MetadataTokens.MethodDefinitionHandle 1
        )
        |> ignore<TypeDefinitionHandle>

        metadata.AddTypeDefinition (
            TypeAttributes.Public,
            metadata.GetOrAddString "Probe",
            metadata.GetOrAddString "Marked`1",
            objectRef,
            MetadataTokens.FieldDefinitionHandle 1,
            MetadataTokens.MethodDefinitionHandle (List.length runners + 1)
        )
        |> fun handle ->
            if handle <> marked then
                failwith $"BUG: Probe.Marked`1 is %O{handle}"

        metadata.AddGenericParameter (
            TypeDefinitionHandle.op_Implicit marked,
            GenericParameterAttributes.None,
            metadata.GetOrAddString "T",
            0
        )
        |> ignore<GenericParameterHandle>

        let byRefLikeConstructor =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .MethodSignature(isInstanceMethod = true)
                .Parameters (0, (fun returns -> returns.Void ()), ignore<ParametersEncoder>)

            metadata.AddMemberReference (byRefLikeRef, metadata.GetOrAddString ".ctor", metadata.GetOrAddBlob blob)

        metadata.AddCustomAttribute (
            TypeDefinitionHandle.op_Implicit marked,
            MemberReferenceHandle.op_Implicit byRefLikeConstructor,
            // The prolog, and no named arguments.
            metadata.GetOrAddBlob [| 1uy ; 0uy ; 0uy ; 0uy |]
        )
        |> ignore<CustomAttributeHandle>

        let peBuilder =
            ManagedPEBuilder (
                PEHeaderBuilder (imageCharacteristics = Characteristics.Dll),
                MetadataRootBuilder metadata,
                ilStream
            )

        let blob = BlobBuilder ()
        peBuilder.Serialize blob |> ignore<BlobContentId>
        blob.ToArray ()

    /// Each runner of `modifiedOperands` the test below emits, and what the JIT does compiling it.
    let private modifiedRefusals : (string * (ModifiedOperands -> InstructionEncoder -> unit) * Refusal) list =
        // Boxes the local of type `Span<int>` (`local` 0) or `ValueTuple<int>` (1), naming the type
        // `pick` chooses.
        let boxed
            (pick : ModifiedOperands -> EntityHandle)
            (local : int)
            (operands : ModifiedOperands)
            (code : InstructionEncoder)
            : unit
            =
            code.LoadLocalAddress local
            code.OpCode ILOpCode.Initobj
            code.Token (if local = 0 then operands.Span else operands.Tuple)
            code.LoadLocal local
            code.OpCode ILOpCode.Box
            code.Token (pick operands)

        let popped
            (pick : ModifiedOperands -> EntityHandle)
            (local : int)
            (operands : ModifiedOperands)
            (code : InstructionEncoder)
            : unit
            =
            boxed pick local operands code
            code.OpCode ILOpCode.Pop
            code.OpCode ILOpCode.Ret

        let unboxed
            (pick : ModifiedOperands -> EntityHandle)
            (local : int)
            (unbox : ModifiedOperands -> EntityHandle)
            (operands : ModifiedOperands)
            (code : InstructionEncoder)
            : unit
            =
            boxed pick local operands code
            code.OpCode ILOpCode.Unbox_any
            code.Token (unbox operands)
            code.OpCode ILOpCode.Pop
            code.OpCode ILOpCode.Ret

        let branched
            (pick : ModifiedOperands -> EntityHandle)
            (local : int)
            (operands : ModifiedOperands)
            (code : InstructionEncoder)
            : unit
            =
            let label = code.DefineLabel ()
            boxed pick local operands code
            code.Branch (ILOpCode.Brtrue_s, label)
            code.MarkLabel label
            code.OpCode ILOpCode.Ret

        let newarr (pick : ModifiedOperands -> EntityHandle) (operands : ModifiedOperands) (code : InstructionEncoder) =
            code.LoadConstantI4 1
            code.OpCode ILOpCode.Newarr
            code.Token (pick operands)
            code.OpCode ILOpCode.Pop
            code.OpCode ILOpCode.Ret

        [
            "BoxModifiedSpanPop", popped _.ModifiedSpan 0, Refusal.Raises invalidProgram
            "BoxModifiedSpanUnboxTuple", unboxed _.ModifiedSpan 0 _.Tuple, Refusal.Raises invalidProgram
            // The JIT folds a box that a branch follows, of any type.
            "BoxModifiedSpanBrtrue", branched _.ModifiedSpan 0, Refusal.Compiles
            "NewarrModifiedSpan", newarr _.ModifiedSpan, Refusal.Raises typeLoad
            // `ValueTuple<int>` is no byref-like type, however its argument is modified.
            "BoxModifiedTuplePop", popped _.ModifiedTuple 1, Refusal.Compiles
            "NewarrModifiedTuple", newarr _.ModifiedTuple, Refusal.Compiles
            "NewarrModifiedMarked", newarr _.ModifiedMarked, Refusal.Compiles
            // A type that does not load raises what binding the token does, before the JIT can
            // refuse the box.
            "BoxModifiedGoneSpanPop", popped _.ModifiedGoneSpan 0, Refusal.Raises typeLoad
            "NewarrModifiedGoneSpan", newarr _.ModifiedGoneSpan, Refusal.Raises typeLoad
        ]

    /// PawPrint instantiates no generic type whose argument carries a custom modifier, so the
    /// analysis cannot instantiate the type these runners box or allocate an array of. That is not a
    /// type that fails to load, and the JIT still refuses where the type is byref-like.
    [<Test>]
    let ``the JIT's refusal of a box or newarr is reported where the analysis cannot instantiate the type it names``
        ()
        : unit
        =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let image =
            modifiedOperands (modifiedRefusals |> List.map (fun (name, write, _) -> name, write))

        let context =
            System.Runtime.Loader.AssemblyLoadContext ("ModifiedOperands", isCollectible = true)

        let runtimeFailures =
            try
                let ty = context.LoadFromStream(new MemoryStream (image)).GetType "Probe.Runners"

                [
                    for name, _, refused in modifiedRefusals do
                        let raised =
                            try
                                ty.GetMethod(name).Invoke ((null : obj), [||]) |> ignore<obj>
                                None
                            with :? TargetInvocationException as e ->
                                Some (e.InnerException.GetType().FullName)

                        let expected =
                            match refused with
                            | Refusal.Compiles -> None
                            | Refusal.Raises refusal -> Some refusal
                            | Refusal.UnlessInlined _
                            | Refusal.RaisesBesideBinding _ -> failwith $"BUG: %s{name} expects %A{refused}"

                        if raised <> expected then
                            yield $"%s{name}: the real runtime raised %A{raised}, not %A{expected}"
                ]
            finally
                context.Unload ()

        let fixture =
            Assembly.read loggerFactory (Some "Probe.dll") (new MemoryStream (image))

        let mutable analysis = analysisOver [ fixture ] id

        let analysisFailures =
            [
                for name, _, refused in modifiedRefusals do
                    let next, escapes =
                        EscapeAnalysis.escapes analysis (methodNamed fixture "Probe.Runners" name)

                    analysis <- next
                    let shown = render analysis escapes

                    let reported =
                        [ invalidProgram ; typeLoad ]
                        |> List.filter (fun refusal -> shown.Contains ("=" + refusal))

                    let expected =
                        match refused with
                        | Refusal.Raises refusal -> [ refusal ]
                        | _ -> []

                    if reported <> expected then
                        yield $"%s{name}: reported %A{reported} of the refusals, not %A{expected}"
            ]

        match runtimeFailures @ analysisFailures with
        | [] -> ()
        | failures -> failures |> String.concat Environment.NewLine |> failwith

    let private missingMethod = "System.MissingMethodException"

    /// The tokens a runner of `operandRefusals` names. Each of `VoidRef`, `VoidSpec` and
    /// `ValueTypeVoidSpec` names CoreLib's `System.Void`: by a TypeRef, by a TypeSpec spelling
    /// `ELEMENT_TYPE_VOID`, and by one spelling `valuetype System.Void`.
    type private RefusedOperands =
        {
            Int32 : EntityHandle
            /// `int32&`
            Byref : EntityHandle
            VoidRef : EntityHandle
            VoidSpec : EntityHandle
            ValueTypeVoidSpec : EntityHandle
            /// `class System.Gone&`, which CoreLib declares no `System.Gone` for
            GoneByref : EntityHandle
            /// `int32&` under `modopt(IsConst)`
            ModifiedByref : EntityHandle
            /// `pinned int32&`
            PinnedByref : EntityHandle
            /// `pinned void`
            PinnedVoid : EntityHandle
            Exception : EntityHandle
            /// `Object::GetHashCode`
            ObjectGetHashCode : EntityHandle
            /// A reference to the instance method of this name, with the signature `Object`'s has
            /// (`GetType`'s for `GetType`), through this parent.
            Member : EntityHandle -> string -> EntityHandle
        }

    /// An image whose static `Probe.Runners` has a parameterless `void` method of each name in
    /// `runners`, each with an `int32` local and the body `runners` writes for it. It emits the
    /// image with System.Reflection.Metadata, because no compiler names a member through a type
    /// that does not declare it, let alone through `System.Void`.
    let private operandRefusals (runners : (string * (RefusedOperands -> InstructionEncoder -> unit)) list) : byte[] =
        let metadata = MetadataBuilder ()
        let ilStream = BlobBuilder ()
        let bodies = MethodBodyStreamEncoder ilStream

        metadata.AddModule (
            0,
            metadata.GetOrAddString "Probe.dll",
            metadata.GetOrAddGuid (Guid "5c2a9e41-7b3d-4f08-a6e1-93d4b0c7e215"),
            Unchecked.defaultof<GuidHandle>,
            Unchecked.defaultof<GuidHandle>
        )
        |> ignore<ModuleDefinitionHandle>

        metadata.AddAssembly (
            metadata.GetOrAddString "Probe",
            Version (1, 0, 0, 0),
            Unchecked.defaultof<StringHandle>,
            Unchecked.defaultof<BlobHandle>,
            Unchecked.defaultof<AssemblyFlags>,
            AssemblyHashAlgorithm.None
        )
        |> ignore<AssemblyDefinitionHandle>

        let coreName = typeof<obj>.Assembly.GetName ()

        let core =
            metadata.AddAssemblyReference (
                metadata.GetOrAddString coreName.Name,
                coreName.Version,
                Unchecked.defaultof<StringHandle>,
                metadata.GetOrAddBlob (coreName.GetPublicKeyToken ()),
                Unchecked.defaultof<AssemblyFlags>,
                Unchecked.defaultof<BlobHandle>
            )

        let typeRef (ns : string) (name : string) : EntityHandle =
            metadata.AddTypeReference (
                (AssemblyReferenceHandle.op_Implicit core : EntityHandle),
                metadata.GetOrAddString ns,
                metadata.GetOrAddString name
            )
            |> TypeReferenceHandle.op_Implicit

        let objectRef = typeRef "System" "Object"
        let int32Ref = typeRef "System" "Int32"
        let voidRef = typeRef "System" "Void"
        let typeRef' = typeRef "System" "Type"
        let goneRef = typeRef "System" "Gone"
        let isConstRef = typeRef "System.Runtime.CompilerServices" "IsConst"

        let typeSpec (encode : BlobBuilder -> unit) : EntityHandle =
            let blob = BlobBuilder ()
            encode blob

            metadata.AddTypeSpecification (metadata.GetOrAddBlob blob)
            |> TypeSpecificationHandle.op_Implicit

        // ECMA-335 II.23.1.16: ELEMENT_TYPE_BYREF is 0x10, ELEMENT_TYPE_VOID 0x01.
        let byrefOf (encodeElement : SignatureTypeEncoder -> unit) (blob : BlobBuilder) : unit =
            blob.WriteByte 0x10uy
            encodeElement (SignatureTypeEncoder blob)

        let instanceSignature
            (returns : ReturnTypeEncoder -> unit)
            (parameters : int)
            (encodeParameters : ParametersEncoder -> unit)
            : BlobHandle
            =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .MethodSignature(isInstanceMethod = true)
                .Parameters (parameters, returns, encodeParameters)

            metadata.GetOrAddBlob blob

        let signatureOf (name : string) : BlobHandle =
            match name with
            | "ToString" -> instanceSignature (fun returns -> returns.Type().String ()) 0 ignore
            | "GetHashCode" -> instanceSignature (fun returns -> returns.Type().Int32 ()) 0 ignore
            | "Equals" ->
                instanceSignature
                    (fun returns -> returns.Type().Boolean ())
                    1
                    (fun parameters -> parameters.AddParameter().Type().Object ())
            | "Finalize" -> instanceSignature (fun returns -> returns.Void ()) 0 ignore
            | "GetType" -> instanceSignature (fun returns -> returns.Type().Type (typeRef', false)) 0 ignore
            | other -> failwith $"BUG: no signature for %s{other}"

        // One row for each parent and name, which the runners share.
        let members =
            System.Collections.Generic.Dictionary<struct (EntityHandle * string), EntityHandle> ()

        let memberOf (parent : EntityHandle) (name : string) : EntityHandle =
            match members.TryGetValue struct (parent, name) with
            | true, existing -> existing
            | false, _ ->
                let reference =
                    (MemberReferenceHandle.op_Implicit (
                        metadata.AddMemberReference (parent, metadata.GetOrAddString name, signatureOf name)
                    )
                    : EntityHandle)

                members.[struct (parent, name)] <- reference
                reference

        let operands =
            {
                Int32 = int32Ref
                Byref = typeSpec (byrefOf _.Int32())
                VoidRef = voidRef
                VoidSpec = typeSpec (fun blob -> blob.WriteByte 0x01uy)
                ValueTypeVoidSpec = typeSpec (fun blob -> SignatureTypeEncoder(blob).Type (voidRef, true))
                GoneByref = typeSpec (byrefOf (fun element -> element.Type (goneRef, false)))
                // ECMA-335 II.23.2.7: ELEMENT_TYPE_CMOD_OPT is 0x20, then the modifier's coded index.
                // ECMA-335 II.23.1.16: ELEMENT_TYPE_PINNED is 0x45.
                PinnedByref =
                    typeSpec (fun blob ->
                        blob.WriteByte 0x45uy
                        byrefOf _.Int32() blob
                    )
                PinnedVoid =
                    typeSpec (fun blob ->
                        blob.WriteByte 0x45uy
                        blob.WriteByte 0x01uy
                    )
                ModifiedByref =
                    typeSpec (fun blob ->
                        blob.WriteByte 0x20uy
                        blob.WriteCompressedInteger (CodedIndex.TypeDefOrRefOrSpec isConstRef)
                        byrefOf _.Int32() blob
                    )
                Exception = typeRef "System" "Exception"
                ObjectGetHashCode = memberOf objectRef "GetHashCode"
                Member = memberOf
            }

        let locals =
            let blob = BlobBuilder ()
            BlobEncoder(blob).LocalVariableSignature(1).AddVariable().Type().Int32 ()
            metadata.AddStandaloneSignature (metadata.GetOrAddBlob blob)

        let signature =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .MethodSignature()
                .Parameters (0, (fun returns -> returns.Void ()), ignore<ParametersEncoder>)

            metadata.GetOrAddBlob blob

        for name, write in runners do
            let code = InstructionEncoder (BlobBuilder (), ControlFlowBuilder ())
            write operands code

            let body =
                bodies.AddMethodBody (
                    code,
                    maxStack = 3,
                    localVariablesSignature = locals,
                    attributes = MethodBodyAttributes.InitLocals
                )

            metadata.AddMethodDefinition (
                MethodAttributes.Public ||| MethodAttributes.Static,
                MethodImplAttributes.IL,
                metadata.GetOrAddString name,
                signature,
                body,
                MetadataTokens.ParameterHandle 1
            )
            |> ignore<MethodDefinitionHandle>

        metadata.AddTypeDefinition (
            Unchecked.defaultof<TypeAttributes>,
            Unchecked.defaultof<StringHandle>,
            metadata.GetOrAddString "<Module>",
            Unchecked.defaultof<EntityHandle>,
            MetadataTokens.FieldDefinitionHandle 1,
            MetadataTokens.MethodDefinitionHandle 1
        )
        |> ignore<TypeDefinitionHandle>

        metadata.AddTypeDefinition (
            TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed,
            metadata.GetOrAddString "Probe",
            metadata.GetOrAddString "Runners",
            objectRef,
            MetadataTokens.FieldDefinitionHandle 1,
            MetadataTokens.MethodDefinitionHandle 1
        )
        |> ignore<TypeDefinitionHandle>

        let peBuilder =
            ManagedPEBuilder (
                PEHeaderBuilder (imageCharacteristics = Characteristics.Dll),
                MetadataRootBuilder metadata,
                ilStream
            )

        let blob = BlobBuilder ()
        peBuilder.Serialize blob |> ignore<BlobContentId>
        blob.ToArray ()

    /// Each runner of `operandRefusals` the test below emits, and what the JIT does compiling it.
    /// CoreCLR's `CEEInfo::resolveToken` refuses a token whose type is a byref or `System.Void`,
    /// for every kind of token but `ldtoken`'s, once it has loaded the type and found any member;
    /// for a member's token, the type is the parent its reference names.
    let private operandRefusalCases : (string * (RefusedOperands -> InstructionEncoder -> unit) * Refusal) list =
        let token (op : ILOpCode) (handle : EntityHandle) (code : InstructionEncoder) : unit =
            code.OpCode op
            code.Token handle

        let boxedInt (operands : RefusedOperands) (code : InstructionEncoder) : unit =
            code.LoadConstantI4 7
            token ILOpCode.Box operands.Int32 code

        let intArray (operands : RefusedOperands) (code : InstructionEncoder) : unit =
            code.LoadConstantI4 1
            token ILOpCode.Newarr operands.Int32 code

        // Each instruction that consumes a type, over the operand it is given, leaving the stack
        // empty.
        let typeShapes : (string * (RefusedOperands -> EntityHandle -> InstructionEncoder -> unit)) list =
            [
                "Isinst",
                fun _ operand code ->
                    code.OpCode ILOpCode.Ldnull
                    token ILOpCode.Isinst operand code
                    code.OpCode ILOpCode.Pop
                "Castclass",
                fun _ operand code ->
                    code.OpCode ILOpCode.Ldnull
                    token ILOpCode.Castclass operand code
                    code.OpCode ILOpCode.Pop
                "Box",
                fun _ operand code ->
                    code.LoadConstantI4 0
                    token ILOpCode.Box operand code
                    code.OpCode ILOpCode.Pop
                "Unbox",
                fun operands operand code ->
                    boxedInt operands code
                    token ILOpCode.Unbox operand code
                    code.OpCode ILOpCode.Pop
                "UnboxAny",
                fun operands operand code ->
                    boxedInt operands code
                    token ILOpCode.Unbox_any operand code
                    code.OpCode ILOpCode.Pop
                "Newarr",
                fun _ operand code ->
                    code.LoadConstantI4 0
                    token ILOpCode.Newarr operand code
                    code.OpCode ILOpCode.Pop
                "Sizeof",
                fun _ operand code ->
                    token ILOpCode.Sizeof operand code
                    code.OpCode ILOpCode.Pop
                "Initobj",
                fun _ operand code ->
                    code.LoadLocalAddress 0
                    token ILOpCode.Initobj operand code
                "Ldobj",
                fun _ operand code ->
                    code.LoadLocalAddress 0
                    token ILOpCode.Ldobj operand code
                    code.OpCode ILOpCode.Pop
                "Stobj",
                fun _ operand code ->
                    code.LoadLocalAddress 0
                    code.LoadConstantI4 0
                    token ILOpCode.Stobj operand code
                "Cpobj",
                fun _ operand code ->
                    code.LoadLocalAddress 0
                    code.LoadLocalAddress 0
                    token ILOpCode.Cpobj operand code
                "Ldelema",
                fun operands operand code ->
                    intArray operands code
                    code.LoadConstantI4 0
                    token ILOpCode.Ldelema operand code
                    code.OpCode ILOpCode.Pop
                "ReadonlyLdelema",
                fun operands operand code ->
                    intArray operands code
                    code.LoadConstantI4 0
                    code.OpCode ILOpCode.Readonly
                    token ILOpCode.Ldelema operand code
                    code.OpCode ILOpCode.Pop
                "Ldelem",
                fun operands operand code ->
                    intArray operands code
                    code.LoadConstantI4 0
                    token ILOpCode.Ldelem operand code
                    code.OpCode ILOpCode.Pop
                "Stelem",
                fun operands operand code ->
                    intArray operands code
                    code.LoadConstantI4 0
                    code.LoadConstantI4 0
                    token ILOpCode.Stelem operand code
                "Mkrefany",
                fun _ operand code ->
                    code.LoadLocalAddress 0
                    token ILOpCode.Mkrefany operand code
                    code.OpCode ILOpCode.Pop
                "Refanyval",
                fun operands operand code ->
                    code.LoadLocalAddress 0
                    token ILOpCode.Mkrefany operands.Int32 code
                    token ILOpCode.Refanyval operand code
                    code.OpCode ILOpCode.Pop
                "Constrained",
                fun operands operand code ->
                    code.LoadLocalAddress 0
                    token ILOpCode.Constrained operand code
                    token ILOpCode.Callvirt operands.ObjectGetHashCode code
                    code.OpCode ILOpCode.Pop
                "Ldtoken",
                fun _ operand code ->
                    token ILOpCode.Ldtoken operand code
                    code.OpCode ILOpCode.Pop
            ]

        let refusedOperands : (string * (RefusedOperands -> EntityHandle)) list =
            [
                "Byref", _.Byref
                "VoidRef", _.VoidRef
                "VoidSpec", _.VoidSpec
                "ValueTypeVoidSpec", _.ValueTypeVoidSpec
            ]

        // Each instruction that names a method, through the parent it is given, leaving the stack
        // empty. Looking a method up through a value type finds only the virtual methods it
        // inherits, so each names one of `Object`'s.
        let memberShapes : (string * (RefusedOperands -> EntityHandle -> InstructionEncoder -> unit)) list =
            [
                "Call",
                fun operands parent code ->
                    code.LoadLocalAddress 0
                    token ILOpCode.Call (operands.Member parent "GetHashCode") code
                    code.OpCode ILOpCode.Pop
                "Callvirt",
                fun operands parent code ->
                    boxedInt operands code
                    code.OpCode ILOpCode.Ldnull
                    token ILOpCode.Callvirt (operands.Member parent "Equals") code
                    code.OpCode ILOpCode.Pop
                "Ldftn",
                fun operands parent code ->
                    token ILOpCode.Ldftn (operands.Member parent "ToString") code
                    code.OpCode ILOpCode.Pop
                "Ldvirtftn",
                fun operands parent code ->
                    boxedInt operands code
                    token ILOpCode.Ldvirtftn (operands.Member parent "ToString") code
                    code.OpCode ILOpCode.Pop
                "Ldtoken",
                fun operands parent code ->
                    token ILOpCode.Ldtoken (operands.Member parent "Finalize") code
                    code.OpCode ILOpCode.Pop
            ]

        let returning
            (body : RefusedOperands -> InstructionEncoder -> unit)
            (operands : RefusedOperands)
            (code : InstructionEncoder)
            : unit
            =
            body operands code
            code.OpCode ILOpCode.Ret

        // `try { } catch (type) { pop }`, naming the type `pick` chooses.
        let catches (pick : RefusedOperands -> EntityHandle) (operands : RefusedOperands) (code : InstructionEncoder) =
            let tryStart = code.DefineLabel ()
            let handlerStart = code.DefineLabel ()
            let handlerEnd = code.DefineLabel ()
            code.MarkLabel tryStart
            code.Branch (ILOpCode.Leave, handlerEnd)
            code.MarkLabel handlerStart
            code.OpCode ILOpCode.Pop
            code.Branch (ILOpCode.Leave, handlerEnd)
            code.MarkLabel handlerEnd
            code.OpCode ILOpCode.Ret
            code.ControlFlowBuilder.AddCatchRegion (tryStart, handlerStart, handlerStart, handlerEnd, pick operands)

        let castclass = typeShapes |> List.find (fun (name, _) -> name = "Castclass") |> snd

        [
            for name, shape in typeShapes do
                yield $"%s{name}_Int32", returning (fun operands -> shape operands operands.Int32), Refusal.Compiles

                for operandName, pick in refusedOperands do
                    let refused =
                        if name = "Ldtoken" then
                            Refusal.Compiles
                        else
                            Refusal.Raises invalidProgram

                    yield
                        $"%s{name}_%s{operandName}", returning (fun operands -> shape operands (pick operands)), refused

            for name, shape in memberShapes do
                yield
                    $"%s{name}Through_Int32",
                    returning (fun operands -> shape operands operands.Int32),
                    Refusal.Compiles

                for parentName, pick in List.tail refusedOperands do
                    let refused =
                        if name = "Ldtoken" then
                            Refusal.Compiles
                        else
                            Refusal.Raises invalidProgram

                    yield
                        $"%s{name}Through_%s{parentName}",
                        returning (fun operands -> shape operands (pick operands)),
                        refused

            // `resolveToken` loads the type before it refuses it, and a load that fails raises
            // instead.
            yield
                "Castclass_GoneByref",
                returning (fun operands -> castclass operands operands.GoneByref),
                Refusal.Raises typeLoad
            // A custom modifier does not change the type a signature names.
            yield
                "Castclass_ModifiedByref",
                returning (fun operands -> castclass operands operands.ModifiedByref),
                Refusal.Raises invalidProgram
            // Nor does `pinned`, which no TypeSpec should spell.
            yield
                "Castclass_PinnedByref",
                returning (fun operands -> castclass operands operands.PinnedByref),
                Refusal.Raises invalidProgram
            yield
                "Castclass_PinnedVoid",
                returning (fun operands -> castclass operands operands.PinnedVoid),
                Refusal.Raises invalidProgram
            // And it finds the member before it refuses its parent: looking up through
            // `System.Void` finds no non-virtual method it inherits.
            yield
                "CallThrough_VoidRef_GetType",
                returning (fun operands code ->
                    code.LoadLocalAddress 0
                    token ILOpCode.Call (operands.Member operands.VoidRef "GetType") code
                    code.OpCode ILOpCode.Pop
                ),
                Refusal.Raises missingMethod
            yield "Catch_Exception", catches _.Exception, Refusal.Compiles
            yield "Catch_Byref", catches _.Byref, Refusal.Raises invalidProgram
            yield "Catch_VoidRef", catches _.VoidRef, Refusal.Raises invalidProgram
        ]

    [<Test>]
    let ``the JIT's refusal of a byref or System.Void as an instruction's type is reported, as a type operand or a member's parent``
        ()
        : unit
        =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let image =
            operandRefusals (operandRefusalCases |> List.map (fun (name, write, _) -> name, write))

        let context =
            System.Runtime.Loader.AssemblyLoadContext ("OperandRefusals", isCollectible = true)

        let runtimeFailures =
            try
                let ty = context.LoadFromStream(new MemoryStream (image)).GetType "Probe.Runners"

                [
                    for name, _, refused in operandRefusalCases do
                        let raised =
                            try
                                ty.GetMethod(name).Invoke ((null : obj), [||]) |> ignore<obj>
                                None
                            with :? TargetInvocationException as e ->
                                Some (e.InnerException.GetType().FullName)

                        let expected =
                            match refused with
                            | Refusal.Compiles -> None
                            | Refusal.Raises refusal -> Some refusal
                            | Refusal.UnlessInlined _
                            | Refusal.RaisesBesideBinding _ -> failwith $"BUG: %s{name} expects %A{refused}"

                        if raised <> expected then
                            yield $"%s{name}: the real runtime raised %A{raised}, not %A{expected}"
                ]
            finally
                context.Unload ()

        let fixture =
            Assembly.read loggerFactory (Some "Probe.dll") (new MemoryStream (image))

        let mutable analysis = analysisOver [ fixture ] id

        let refusals =
            Set.ofList [ invalidProgram ; typeLoad ; missingMethod ; badImageFormat ]

        let analysisFailures =
            [
                for name, _, refused in operandRefusalCases do
                    let next, escapes =
                        EscapeAnalysis.escapes analysis (methodNamed fixture "Probe.Runners" name)

                    analysis <- next
                    let shown = render analysis escapes

                    let refusalsIn (shown : Set<string>) : Set<string> =
                        refusals |> Set.filter (fun refusal -> shown.Contains ("=" + refusal))

                    // What making the refusal's exception raises escapes beside it.
                    let expected =
                        match refused with
                        | Refusal.Raises refusal when refusal = typeLoad ->
                            let next, escapes = EscapeAnalysis.escapes analysis (typeLoadConstructor ())
                            analysis <- next
                            Set.add refusal (refusalsIn (render analysis escapes))
                        | Refusal.Raises refusal ->
                            let next, made = making analysis refusal
                            analysis <- next
                            Set.add refusal (refusalsIn made)
                        | _ -> Set.empty

                    let reported = refusalsIn shown

                    if reported <> expected then
                        yield $"%s{name}: reported %A{Set.toList reported} of the refusals, not %A{Set.toList expected}"
            ]

        match runtimeFailures @ analysisFailures with
        | [] -> ()
        | failures -> failures |> String.concat Environment.NewLine |> failwith

    /// `Probe.Runners.Modified()`: `box Span<int>; unbox.any List<int modopt(IsConst)>; pop`, whose
    /// unboxed type PawPrint does not instantiate, a generic argument carrying a custom modifier.
    [<Test>]
    let ``the JIT's refusal of a box is reported where the analysis cannot instantiate what follows it`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let image =
            modifiedOperands
                [
                    "Modified",
                    fun operands code ->
                        code.LoadLocalAddress 0
                        code.OpCode ILOpCode.Initobj
                        code.Token operands.Span
                        code.LoadLocal 0
                        code.OpCode ILOpCode.Box
                        code.Token operands.Span
                        code.OpCode ILOpCode.Unbox_any
                        code.Token operands.ModifiedList
                        code.OpCode ILOpCode.Pop
                        code.OpCode ILOpCode.Ret
                ]

        // The real runtime refuses the box: `List<int>` is not the boxed type.
        let context =
            System.Runtime.Loader.AssemblyLoadContext ("ModifiedFollower", isCollectible = true)

        let raised =
            try
                try
                    context
                        .LoadFromStream(new MemoryStream (image))
                        .GetType("Probe.Runners")
                        .GetMethod("Modified")
                        .Invoke ((null : obj), [||])
                    |> ignore<obj>

                    None
                with :? TargetInvocationException as e ->
                    Some (e.InnerException.GetType().FullName)
            finally
                context.Unload ()

        raised |> shouldEqual (Some invalidProgram)

        let fixture =
            Assembly.read loggerFactory (Some "Probe.dll") (new MemoryStream (image))

        let analysis = analysisOver [ fixture ] id

        let analysis, escapes =
            EscapeAnalysis.escapes analysis (methodNamed fixture "Probe.Runners" "Modified")

        render analysis escapes
        |> Set.contains ("=" + invalidProgram)
        |> shouldEqual true

    [<Test>]
    let ``only an instance decides whether the JIT refuses a box or newarr of a type variable that allows a byref-like type``
        ()
        : unit
        =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let fixture =
            Assembly.read loggerFactory (Some "Refusals.dll") (new MemoryStream (fabricateRefusals ()))

        let mutable analysis = analysisOver [ fixture ] id
        // `analysisOver` reads the CoreLib of the runtime under test.
        let runtime = EmulatedRuntime.ofCoreLib (hostCoreLib ())

        let failures =
            [
                // Whether the type is byref-like decides these, and the open definition does not
                // say. A box of a type variable may call `CastHelpers.Box`, which on .NET 11
                // reaches a memory QCall the contract table has no row for.
                for name, dependsOnInstantiation in
                    [ "BoxT", true ; "NewarrT", true ; "BoxTBranch", false ; "BoxU", false ] do
                    let key = methodNamed fixture "Refusals.Runners" name
                    let next, escapes = EscapeAnalysis.escapes analysis key
                    let next, sources = EscapeAnalysis.unknownSources next key
                    analysis <- next

                    let reasons =
                        sources
                        |> Seq.filter (isNet11MemoryQCall runtime analysis >> not)
                        |> Seq.map _.Reason
                        |> Set.ofSeq

                    let expected =
                        if dependsOnInstantiation then
                            Set.singleton Opacity.DependsOnInstantiation
                        else
                            Set.empty

                    if reasons <> expected then
                        yield
                            $"%s{name}: unknown %b{escapes.Unknown} through %A{Set.toList reasons}, beyond .NET 11's memory QCalls; expected %A{Set.toList expected}"

                    // Where an instance decides it, the definition's answer is opaque instead.
                    let shown = render analysis escapes

                    for refusal in [ invalidProgram ; typeLoad ] do
                        if shown.Contains ("=" + refusal) then
                            yield $"%s{name}: %A{Set.toList shown}, which the open definition does not decide"
            ]

        match failures with
        | [] -> ()
        | failures -> failures |> String.concat Environment.NewLine |> failwith

    /// The integer types narrower than 64 bits that a parameter may have, each with whether a
    /// constant `int32` passed as one is zero once the parameter holds it.
    let private narrowParameters : (Type * (int32 -> bool)) list =
        [
            typeof<bool>, (fun value -> byte value = 0uy)
            typeof<sbyte>, (fun value -> sbyte value = 0y)
            typeof<byte>, (fun value -> byte value = 0uy)
            typeof<int16>, (fun value -> int16 value = 0s)
            typeof<uint16>, (fun value -> uint16 value = 0us)
            typeof<char>, (fun value -> char value = '\000')
            typeof<int32>, (fun value -> value = 0)
            typeof<uint32>, (fun value -> uint32 value = 0u)
        ]

    let private narrowConstants : int32 list =
        [ 0 ; 1 ; -1 ; 0x100 ; 0x10000 ; Int32.MinValue ]

    let private wideParameters : Type list = [ typeof<int64> ; typeof<uint64> ]

    let private wideConstants : int64 list = [ 0L ; 1L ; 0x100000000L ; Int64.MinValue ]

    /// An image whose runners pass integer constants to methods that branch on the parameter they
    /// are passed in, which C# compiled for debugging does not do; each runner takes `(int a, int b)`
    /// and computes `a / b` only where the branch falls through:
    ///   - `Runners.Pass_<T>_<i>`: `ldc <constant i>; ldarg.0; ldarg.1; call Unless_<T>`, where
    ///     `Unless_<T>(T v, int a, int b)` is `ldarg.0; brtrue L; a / b`;
    ///   - `Runners.ViaInt32_<T>_<i>` and `Runners.ViaBoolean_<T>_<i>`: the same, through a method
    ///     that takes the constant as an `int` or a `bool` and passes it on to `Unless_<T>`;
    ///   - `Runners.Inline_<i>`: `ldc.i4 <constant i>; brtrue L; a / b`.
    let private fabricateIntegerBranches () : byte[] =
        let builder =
            Emit.PersistedAssemblyBuilder (AssemblyName "Integers", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "Integers"

        let runners =
            modul.DefineType (
                "Integers.Runners",
                TypeAttributes.Public
                ||| TypeAttributes.Class
                ||| TypeAttributes.Abstract
                ||| TypeAttributes.Sealed
            )

        let staticAttributes = MethodAttributes.Public ||| MethodAttributes.Static

        // `brtrue` over the value on the stack, then `a / b` from arguments `a` and `b`.
        let divideUnless (il : Emit.ILGenerator) (a : Emit.OpCode) (b : Emit.OpCode) =
            let skip = il.DefineLabel ()
            il.Emit (Emit.OpCodes.Brtrue, skip)
            il.Emit a
            il.Emit b
            il.Emit Emit.OpCodes.Div
            il.Emit Emit.OpCodes.Ret
            il.MarkLabel skip
            il.Emit Emit.OpCodes.Ldc_I4_0
            il.Emit Emit.OpCodes.Ret

        let operands = [| typeof<int> ; typeof<int> |]

        let unless =
            [
                for parameter in List.map fst narrowParameters @ wideParameters do
                    let callee =
                        runners.DefineMethod (
                            $"Unless_%s{parameter.Name}",
                            staticAttributes,
                            typeof<int>,
                            [| parameter ; typeof<int> ; typeof<int> |]
                        )

                    let il = callee.GetILGenerator ()
                    il.Emit Emit.OpCodes.Ldarg_0
                    divideUnless il Emit.OpCodes.Ldarg_1 Emit.OpCodes.Ldarg_2
                    parameter, callee
            ]
            |> dict

        // A runner that pushes a constant with `push`, then calls `callee` with it and its operands.
        let runner (name : string) (push : Emit.ILGenerator -> unit) (callee : Emit.MethodBuilder) =
            let method = runners.DefineMethod (name, staticAttributes, typeof<int>, operands)
            let il = method.GetILGenerator ()
            push il
            il.Emit Emit.OpCodes.Ldarg_0
            il.Emit Emit.OpCodes.Ldarg_1
            il.Emit (Emit.OpCodes.Call, callee)
            il.Emit Emit.OpCodes.Ret

        // A method that takes a value as a `through`, and passes it on to `Unless_<parameter>`.
        let forwarder (through : Type) (parameter : Type) =
            let method =
                runners.DefineMethod (
                    $"Forward_%s{through.Name}_%s{parameter.Name}",
                    staticAttributes,
                    typeof<int>,
                    [| through ; typeof<int> ; typeof<int> |]
                )

            let il = method.GetILGenerator ()
            il.Emit Emit.OpCodes.Ldarg_0
            il.Emit Emit.OpCodes.Ldarg_1
            il.Emit Emit.OpCodes.Ldarg_2
            il.Emit (Emit.OpCodes.Call, unless.[parameter])
            il.Emit Emit.OpCodes.Ret
            method

        for parameter, _ in narrowParameters do
            let viaInt32 = forwarder typeof<int32> parameter
            let viaBoolean = forwarder typeof<bool> parameter

            for i, constant in List.indexed narrowConstants do
                let push (il : Emit.ILGenerator) = il.Emit (Emit.OpCodes.Ldc_I4, constant)
                runner $"Pass_%s{parameter.Name}_%d{i}" push unless.[parameter]
                runner $"ViaInt32_%s{parameter.Name}_%d{i}" push viaInt32
                runner $"ViaBoolean_%s{parameter.Name}_%d{i}" push viaBoolean

        for parameter in wideParameters do
            for i, constant in List.indexed wideConstants do
                runner
                    $"Pass_%s{parameter.Name}_%d{i}"
                    (fun il -> il.Emit (Emit.OpCodes.Ldc_I8, constant))
                    unless.[parameter]

        for i, constant in List.indexed narrowConstants do
            let method =
                runners.DefineMethod ($"Inline_%d{i}", staticAttributes, typeof<int>, operands)

            let il = method.GetILGenerator ()
            il.Emit (Emit.OpCodes.Ldc_I4, constant)
            divideUnless il Emit.OpCodes.Ldarg_0 Emit.OpCodes.Ldarg_1

        runners.CreateType () |> ignore<Type>

        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    [<Test>]
    let ``a branch on an integer a caller passes as a constant goes the way the parameter's value decides`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let image = fabricateIntegerBranches ()

        let fixture =
            Assembly.read loggerFactory (Some "Integers.dll") (new MemoryStream (image))

        let divides (zero : bool) : Set<string> =
            if zero then
                Set.ofList [ dividesByZero ; overflows ]
            else
                Set.empty

        let cases =
            [
                for parameter, isZero in narrowParameters do
                    for i, constant in List.indexed narrowConstants do
                        yield $"Pass_%s{parameter.Name}_%d{i}", divides (isZero constant), DispatchClaim.Precise
                        yield $"ViaInt32_%s{parameter.Name}_%d{i}", divides (isZero constant), DispatchClaim.Precise

                        // A `bool` holds the constant's low byte, which the parameter then holds.
                        yield
                            $"ViaBoolean_%s{parameter.Name}_%d{i}", divides (byte constant = 0uy), DispatchClaim.Precise
                for parameter in wideParameters do
                    for i, constant in List.indexed wideConstants do
                        yield $"Pass_%s{parameter.Name}_%d{i}", divides (constant = 0L), DispatchClaim.Precise
                for i, constant in List.indexed narrowConstants do
                    yield $"Inline_%d{i}", divides (constant = 0), DispatchClaim.Precise
            ]

        let runtime =
            cases
            |> List.map (fun (name, _, _) -> name)
            |> dispatchOnRealRuntime "Integers" image

        let _, failures =
            dispatchFailures fixture "Integers" runtime cases (analysisOver [ fixture ] id)

        match failures with
        | [] -> ()
        | failures -> failures |> String.concat Environment.NewLine |> failwith

    /// The `ldind` that loads a location of each integer type.
    let private loadOf : (Type * Emit.OpCode) list =
        [
            typeof<bool>, Emit.OpCodes.Ldind_U1
            typeof<sbyte>, Emit.OpCodes.Ldind_I1
            typeof<byte>, Emit.OpCodes.Ldind_U1
            typeof<int16>, Emit.OpCodes.Ldind_I2
            typeof<uint16>, Emit.OpCodes.Ldind_U2
            typeof<char>, Emit.OpCodes.Ldind_U2
            typeof<int32>, Emit.OpCodes.Ldind_I4
            typeof<uint32>, Emit.OpCodes.Ldind_U4
            typeof<int64>, Emit.OpCodes.Ldind_I8
            typeof<uint64>, Emit.OpCodes.Ldind_I8
        ]

    /// An image whose runners store to a local and pass its address to a method that branches on
    /// what it loads through the address; each runner takes `(int a, int b)`, and the method computes
    /// `a / b` only where the branch falls through:
    ///   - `Runners.Local_<T>_<i>`: `ldc <constant i>; stloc.0; ldloca.s 0`, with local 0 a `T`,
    ///     then `ldarg.0; ldarg.1; call Unless_<T>`, where `Unless_<T>(ref T p, int a, int b)` is
    ///     `ldarg.0; ldind; brtrue L; a / b`;
    ///   - `Runners.Narrowed_<i>`: the same with an `int` local, passed to `Unless_Byte`, which
    ///     loads its first byte: the low byte, on a little-endian CPU;
    ///   - `Runners.Stored` and `Runners.Cleared`: local 0, a `bool` holding 1, is set to 0
    ///     through the address the call passes before the call, by `stind.i1` or by `Clear`;
    ///   - `Runners.JoinedNonZero`, `Runners.JoinedMixed` and `Runners.JoinedMixedSwapped`: local 0
    ///     holds 1, 1 or 0 where `a` is 1, and otherwise 2, 0 or 1;
    ///   - `Runners.ClearedInFinally`: local 0, a `bool` holding 1, is set to 0 by the `finally`
    ///     handler the `leave` before the `ldloca` runs;
    ///   - `Runners.ClearsFirst`, `Runners.CallsFirst`, `Runners.MayClearFirst` and
    ///     `Runners.Repointed`: local 0, a `bool`, holds 1, and the method called, before it loads
    ///     through its argument, stores 0 through it, passes it to `Clear`, stores 0 through it
    ///     on one path of two, or points the argument at a local of its own holding 0;
    ///   - `Runners.Leaked` and `Runners.LeakedInFinally`: the address of local 0, a `bool`, is
    ///     passed to `Keep_Writer` (or, in a `finally` handler a `leave` runs, `Keep_FinallyWriter`),
    ///     which keeps it as a pointer; then local 0 is set to 1, and the address is passed to
    ///     `Writer.Unless` (or `FinallyWriter.Unless`), whose type's initializer, which the first
    ///     call runs first, stores 0 through the pointer.
    let private fabricatePointees () : byte[] =
        let builder =
            Emit.PersistedAssemblyBuilder (AssemblyName "Pointees", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "Pointees"

        let runners =
            modul.DefineType (
                "Pointees.Runners",
                TypeAttributes.Public
                ||| TypeAttributes.Class
                ||| TypeAttributes.Abstract
                ||| TypeAttributes.Sealed
            )

        let staticAttributes = MethodAttributes.Public ||| MethodAttributes.Static
        let operands = [| typeof<int> ; typeof<int> |]

        // `brtrue` over the value on the stack, then `a / b` from arguments 1 and 2.
        let divideUnless (il : Emit.ILGenerator) =
            let skip = il.DefineLabel ()
            il.Emit (Emit.OpCodes.Brtrue, skip)
            il.Emit Emit.OpCodes.Ldarg_1
            il.Emit Emit.OpCodes.Ldarg_2
            il.Emit Emit.OpCodes.Div
            il.Emit Emit.OpCodes.Ret
            il.MarkLabel skip
            il.Emit Emit.OpCodes.Ldc_I4_0
            il.Emit Emit.OpCodes.Ret

        // A method taking `(ref <pointee> p, int a, int b)`, whose body starts with `prepare`, then
        // loads through `p` with `load` and branches on what it loads.
        let callee (name : string) (pointee : Type) (load : Emit.OpCode) (prepare : Emit.ILGenerator -> unit) =
            let method =
                runners.DefineMethod (
                    name,
                    staticAttributes,
                    typeof<int>,
                    [| pointee.MakeByRefType () ; typeof<int> ; typeof<int> |]
                )

            let il = method.GetILGenerator ()
            prepare il
            il.Emit Emit.OpCodes.Ldarg_0
            il.Emit load
            divideUnless il
            method

        let unless =
            [
                for pointee, load in loadOf do
                    pointee, callee $"Unless_%s{pointee.Name}" pointee load ignore
            ]
            |> dict

        let clear =
            let method =
                runners.DefineMethod ("Clear", staticAttributes, typeof<Void>, [| typeof<bool>.MakeByRefType () |])

            let il = method.GetILGenerator ()
            il.Emit Emit.OpCodes.Ldarg_0
            il.Emit Emit.OpCodes.Ldc_I4_0
            il.Emit Emit.OpCodes.Stind_I1
            il.Emit Emit.OpCodes.Ret
            method

        // A runner with one local, of type `local`, whose body starts with `prepare`, which leaves
        // an address on the stack, then passes that and its operands to `target`.
        let runner (name : string) (local : Type) (prepare : Emit.ILGenerator -> unit) (target : Emit.MethodBuilder) =
            let method = runners.DefineMethod (name, staticAttributes, typeof<int>, operands)
            let il = method.GetILGenerator ()
            il.DeclareLocal local |> ignore<Emit.LocalBuilder>
            prepare il
            il.Emit Emit.OpCodes.Ldarg_0
            il.Emit Emit.OpCodes.Ldarg_1
            il.Emit (Emit.OpCodes.Call, target)
            il.Emit Emit.OpCodes.Ret

        let storeThenAddress (push : Emit.ILGenerator -> unit) (il : Emit.ILGenerator) =
            push il
            il.Emit Emit.OpCodes.Stloc_0
            il.Emit (Emit.OpCodes.Ldloca_S, 0uy)

        let holdsOne = storeThenAddress (fun il -> il.Emit Emit.OpCodes.Ldc_I4_1)

        for parameter, _ in narrowParameters do
            for i, constant in List.indexed narrowConstants do
                runner
                    $"Local_%s{parameter.Name}_%d{i}"
                    parameter
                    (storeThenAddress (fun il -> il.Emit (Emit.OpCodes.Ldc_I4, constant)))
                    unless.[parameter]

        for parameter in wideParameters do
            for i, constant in List.indexed wideConstants do
                runner
                    $"Local_%s{parameter.Name}_%d{i}"
                    parameter
                    (storeThenAddress (fun il -> il.Emit (Emit.OpCodes.Ldc_I8, constant)))
                    unless.[parameter]

        for i, constant in List.indexed narrowConstants do
            runner
                $"Narrowed_%d{i}"
                typeof<int>
                (storeThenAddress (fun il -> il.Emit (Emit.OpCodes.Ldc_I4, constant)))
                unless.[typeof<byte>]

        runner
            "Stored"
            typeof<bool>
            (fun il ->
                holdsOne il
                il.Emit Emit.OpCodes.Dup
                il.Emit Emit.OpCodes.Ldc_I4_0
                il.Emit Emit.OpCodes.Stind_I1
            )
            unless.[typeof<bool>]

        runner
            "Cleared"
            typeof<bool>
            (fun il ->
                holdsOne il
                il.Emit Emit.OpCodes.Dup
                il.Emit (Emit.OpCodes.Call, clear)
            )
            unless.[typeof<bool>]

        let joined (name : string) (whenOne : Emit.OpCode) (otherwise : Emit.OpCode) =
            runner
                name
                typeof<bool>
                (fun il ->
                    let one = il.DefineLabel ()
                    let join = il.DefineLabel ()
                    il.Emit Emit.OpCodes.Ldarg_0
                    il.Emit Emit.OpCodes.Ldc_I4_1
                    il.Emit (Emit.OpCodes.Beq, one)
                    il.Emit otherwise
                    il.Emit Emit.OpCodes.Stloc_0
                    il.Emit (Emit.OpCodes.Br, join)
                    il.MarkLabel one
                    il.Emit whenOne
                    il.Emit Emit.OpCodes.Stloc_0
                    il.MarkLabel join
                    il.Emit (Emit.OpCodes.Ldloca_S, 0uy)
                )
                unless.[typeof<bool>]

        joined "JoinedNonZero" Emit.OpCodes.Ldc_I4_1 Emit.OpCodes.Ldc_I4_2
        joined "JoinedMixed" Emit.OpCodes.Ldc_I4_1 Emit.OpCodes.Ldc_I4_0
        joined "JoinedMixedSwapped" Emit.OpCodes.Ldc_I4_0 Emit.OpCodes.Ldc_I4_1

        runner
            "ClearedInFinally"
            typeof<bool>
            (fun il ->
                il.Emit Emit.OpCodes.Ldc_I4_1
                il.Emit Emit.OpCodes.Stloc_0
                il.BeginExceptionBlock () |> ignore<Emit.Label>
                il.BeginFinallyBlock ()
                il.Emit Emit.OpCodes.Ldc_I4_0
                il.Emit Emit.OpCodes.Stloc_0
                il.EndExceptionBlock ()
                il.Emit (Emit.OpCodes.Ldloca_S, 0uy)
            )
            unless.[typeof<bool>]

        let clearsFirst =
            callee
                "ClearsFirstCallee"
                typeof<bool>
                Emit.OpCodes.Ldind_U1
                (fun il ->
                    il.Emit Emit.OpCodes.Ldarg_0
                    il.Emit Emit.OpCodes.Ldc_I4_0
                    il.Emit Emit.OpCodes.Stind_I1
                )

        let callsFirst =
            callee
                "CallsFirstCallee"
                typeof<bool>
                Emit.OpCodes.Ldind_U1
                (fun il ->
                    il.Emit Emit.OpCodes.Ldarg_0
                    il.Emit (Emit.OpCodes.Call, clear)
                )

        // Every operand `a` is nonzero, so the store runs.
        let mayClearFirst =
            callee
                "MayClearFirstCallee"
                typeof<bool>
                Emit.OpCodes.Ldind_U1
                (fun il ->
                    let load = il.DefineLabel ()
                    il.Emit Emit.OpCodes.Ldarg_1
                    il.Emit (Emit.OpCodes.Brfalse, load)
                    il.Emit Emit.OpCodes.Ldarg_0
                    il.Emit Emit.OpCodes.Ldc_I4_0
                    il.Emit Emit.OpCodes.Stind_I1
                    il.MarkLabel load
                )

        let repointed =
            callee
                "RepointedCallee"
                typeof<bool>
                Emit.OpCodes.Ldind_U1
                (fun il ->
                    il.DeclareLocal typeof<bool> |> ignore<Emit.LocalBuilder>
                    il.Emit Emit.OpCodes.Ldc_I4_0
                    il.Emit Emit.OpCodes.Stloc_0
                    il.Emit (Emit.OpCodes.Ldloca_S, 0uy)
                    il.Emit (Emit.OpCodes.Starg_S, 0uy)
                )

        runner "ClearsFirst" typeof<bool> holdsOne clearsFirst
        runner "CallsFirst" typeof<bool> holdsOne callsFirst
        runner "MayClearFirst" typeof<bool> holdsOne mayClearFirst
        runner "Repointed" typeof<bool> holdsOne repointed

        // `Keep_<name>`, which keeps the address it is passed as a pointer, and `<name>.Unless`, the
        // first call of which runs `<name>`'s initializer first, storing 0 through that pointer.
        let keeper (name : string) =
            let kept =
                runners.DefineField (
                    $"Kept_%s{name}",
                    typeof<bool>.MakePointerType (),
                    FieldAttributes.Public ||| FieldAttributes.Static
                )

            let keep =
                let method =
                    runners.DefineMethod (
                        $"Keep_%s{name}",
                        staticAttributes,
                        typeof<Void>,
                        [| typeof<bool>.MakeByRefType () |]
                    )

                let il = method.GetILGenerator ()
                il.Emit Emit.OpCodes.Ldarg_0
                il.Emit Emit.OpCodes.Conv_U
                il.Emit (Emit.OpCodes.Stsfld, kept)
                il.Emit Emit.OpCodes.Ret
                method

            // Not `beforefieldinit`, so its initializer runs before the first call of its method.
            let writer =
                modul.DefineType (
                    $"Pointees.%s{name}",
                    TypeAttributes.Public
                    ||| TypeAttributes.Class
                    ||| TypeAttributes.Abstract
                    ||| TypeAttributes.Sealed
                )

            let initializer = writer.DefineTypeInitializer().GetILGenerator ()
            initializer.Emit (Emit.OpCodes.Ldsfld, kept)
            initializer.Emit Emit.OpCodes.Ldc_I4_0
            initializer.Emit Emit.OpCodes.Stind_I1
            initializer.Emit Emit.OpCodes.Ret

            let unless =
                let method =
                    writer.DefineMethod (
                        "Unless",
                        staticAttributes,
                        typeof<int>,
                        [| typeof<bool>.MakeByRefType () ; typeof<int> ; typeof<int> |]
                    )

                let il = method.GetILGenerator ()
                il.Emit Emit.OpCodes.Ldarg_0
                il.Emit Emit.OpCodes.Ldind_U1
                divideUnless il
                method

            writer, keep, unless

        let leakedWriter, keep, writerUnless = keeper "Writer"

        runner
            "Leaked"
            typeof<bool>
            (fun il ->
                il.Emit (Emit.OpCodes.Ldloca_S, 0uy)
                il.Emit Emit.OpCodes.Dup
                il.Emit (Emit.OpCodes.Call, keep)
                il.Emit Emit.OpCodes.Ldc_I4_1
                il.Emit Emit.OpCodes.Stloc_0
            )
            writerUnless

        let finallyWriter, keepInFinally, finallyWriterUnless = keeper "FinallyWriter"

        runner
            "LeakedInFinally"
            typeof<bool>
            (fun il ->
                il.BeginExceptionBlock () |> ignore<Emit.Label>
                il.BeginFinallyBlock ()
                il.Emit (Emit.OpCodes.Ldloca_S, 0uy)
                il.Emit (Emit.OpCodes.Call, keepInFinally)
                il.EndExceptionBlock ()
                il.Emit Emit.OpCodes.Ldc_I4_1
                il.Emit Emit.OpCodes.Stloc_0
                il.Emit (Emit.OpCodes.Ldloca_S, 0uy)
            )
            finallyWriterUnless

        runners.CreateType () |> ignore<Type>
        leakedWriter.CreateType () |> ignore<Type>
        finallyWriter.CreateType () |> ignore<Type>

        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    [<Test>]
    let ``a branch on what a method loads through the address of its caller's local goes the way the local decides``
        ()
        : unit
        =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let image = fabricatePointees ()

        let fixture =
            Assembly.read loggerFactory (Some "Pointees.dll") (new MemoryStream (image))

        let divides (zero : bool) : Set<string> =
            if zero then
                Set.ofList [ dividesByZero ; overflows ]
            else
                Set.empty

        let cases =
            [
                for parameter, isZero in narrowParameters do
                    for i, constant in List.indexed narrowConstants do
                        yield $"Local_%s{parameter.Name}_%d{i}", divides (isZero constant), DispatchClaim.Precise
                for parameter in wideParameters do
                    for i, constant in List.indexed wideConstants do
                        yield $"Local_%s{parameter.Name}_%d{i}", divides (constant = 0L), DispatchClaim.Precise
                for i, constant in List.indexed narrowConstants do
                    yield $"Narrowed_%d{i}", divides (byte constant = 0uy), DispatchClaim.Precise
                yield "Stored", divides true, DispatchClaim.Precise
                yield "Cleared", divides true, DispatchClaim.Precise
                yield "JoinedNonZero", divides false, DispatchClaim.Precise
                yield "JoinedMixed", divides true, DispatchClaim.Precise
                yield "JoinedMixedSwapped", divides true, DispatchClaim.Precise
                yield "ClearedInFinally", divides true, DispatchClaim.Precise
                yield "ClearsFirst", divides true, DispatchClaim.Precise
                yield "CallsFirst", divides true, DispatchClaim.Precise
                yield "MayClearFirst", divides true, DispatchClaim.Precise
                yield "Repointed", divides true, DispatchClaim.Precise
                yield "Leaked", divides true, DispatchClaim.Precise
                yield "LeakedInFinally", divides true, DispatchClaim.Precise
            ]

        let runtime =
            cases
            |> List.map (fun (name, _, _) -> name)
            |> dispatchOnRealRuntime "Pointees" image

        let _, failures =
            dispatchFailures fixture "Pointees" runtime cases (analysisOver [ fixture ] id)

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

                    // Making the exception runs its parameterless constructor.
                    if expected then
                        let next, made = making analysis ambiguity
                        analysis <- next

                        if not (Set.isSubset made shown) then
                            yield
                                $"%s{name}: %A{Set.toList shown}, lacking some of what making the ambiguity raises, %A{Set.toList made}"
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

                    // A `constrained.` call on a value type that lands in a default body boxes
                    // its receiver, which .NET 11's `CastHelpers.Box` copies through a QCall the
                    // table has no row for (`Net11Frontier.spanHelpersMemoryQCalls`).
                    let next, net11Frontier =
                        unknownOnlyThroughNet11MemoryQCalls
                            (FrameworkUnderTest.runtime ())
                            analysis
                            (methodNamed clientAssembly "Client.Runners" methodName)

                    analysis <- next

                    if
                        (summary.Unknown && not net11Frontier)
                        || shown.Contains ("=" + initialization) <> escapes
                    then
                        yield
                            $"%s{methodName}: %A{Set.toList shown}, unknown %b{summary.Unknown}; expected %s{initialization} to escape: %b{escapes}, and nothing unknown"

                    // Should making the wrapper fail, the initializer's own exception escapes in its
                    // place, past a handler for the wrapper.
                    if not (shown.Contains "=System.InvalidOperationException") then
                        yield $"%s{methodName}: %A{Set.toList shown} lacks what the module initializer raises"
            ]

        match failures with
        | [] -> ()
        | failures -> failures |> String.concat Environment.NewLine |> failwith

    [<Test>]
    let ``a module initializer's failure is thrown again where it runs, through binding or dispatch`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let contracts =
            """
namespace Contracts;

public interface IStatic { static abstract int Probe(int a, int b); }
"""

        // Its initializer can fail only by running out of memory, whose exception is made in
        // advance, so that nothing it runs reaches the preservation of a stack trace: only the
        // runtime's throwing that exception again does.
        let allocating =
            """
namespace Allocating;

static class Init
{
    static object s_value;

    [System.Runtime.CompilerServices.ModuleInitializer]
    internal static void Run() { s_value = new object(); }
}

public static class P
{
    public static void M() { }
}

public interface IStaticDefault : Contracts.IStatic { static int Contracts.IStatic.Probe(int a, int b) => a; }
"""

        // `Binds` binds a token into Allocating; `Lands` names nothing of it, and only dispatch
        // reaches it.
        let client =
            """
namespace Client;

public struct StaticUser : Allocating.IStaticDefault { }

public static class Shapes
{
    public static int Static<T>(int a, int b) where T : Contracts.IStatic => T.Probe(a, b);
}

public static class Runners
{
    public static void Binds() => Allocating.P.M();
    public static int Lands() => Shapes.Static<StaticUser>(1, 2);
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
        let allocatingImage = compile "Allocating" [ contractsImage ] allocating
        let clientImage = compile "Client" [ contractsImage ; allocatingImage ] client

        let read (name : string) (image : byte[]) : DumpedAssembly =
            Assembly.read loggerFactory (Some $"%s{name}.dll") (new MemoryStream (image))

        let assemblies =
            [
                read "Contracts" contractsImage
                read "Allocating" allocatingImage
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

        let analysisAllowed (assumptions : Set<Assumption>) : EscapeAnalysisState =
            analysisOf
                (hostCoreLib ())
                (FrameworkUnderTest.runtimeDirs ())
                (hostTarget ())
                HardwareIntrinsicsProfile.ScalarOnly
                assumptions
                assemblies
                bind

        let mutable unassuming =
            analysisAllowed (Set.remove Assumption.StackTracePreserved Assumption.all)

        let next, preserving = preservingStackTrace unassuming (hostCoreLib ())
        unassuming <- next
        let mutable assuming = analysisAllowed Assumption.all

        for runner in [ "Binds" ; "Lands" ] do
            let key = methodNamed clientAssembly "Client.Runners" runner
            let next, escapes = EscapeAnalysis.escapes unassuming key
            escapes.Unknown |> shouldEqual true
            let next, sources = EscapeAnalysis.unknownSources next key
            unassuming <- next

            sources
            |> Seq.map (renderSite unassuming)
            |> Set.ofSeq
            |> shouldEqual preserving

            let next, escapes = EscapeAnalysis.escapes assuming key
            assuming <- next
            escapes.Unknown |> shouldEqual false

            render assuming escapes
            |> Set.contains "=System.TypeInitializationException"
            |> shouldEqual true

            escapes.Assumes.Contains Assumption.StackTracePreserved |> shouldEqual true

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

        let analysis, made =
            making analysis "System.Runtime.AmbiguousImplementationException"

        // The call boxes its receiver, which .NET 11's `CastHelpers.Box` copies through a QCall
        // the table has no row for (`Net11Frontier.spanHelpersMemoryQCalls`).
        let _, net11Frontier =
            unknownOnlyThroughNet11MemoryQCalls
                (FrameworkUnderTest.runtime ())
                analysis
                (methodNamed assembly "Run" "Call")

        if
            (escapes.Unknown && not net11Frontier)
            || not (shown.Contains "=System.Runtime.AmbiguousImplementationException")
            || not (Set.isSubset made shown)
        then
            failwith
                $"Run.Call: %A{Set.toList shown}, unknown %b{escapes.Unknown}; expected AmbiguousImplementationException, what making it raises (%A{Set.toList made}), and nothing unknown"

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
                let analysis, made = making analysis "System.EntryPointNotFoundException"

                // A `constrained.` call on a value type whose dispatch fails boxes its receiver,
                // which .NET 11's `CastHelpers.Box` copies through a QCall the table has no row for
                // (`Net11Frontier.spanHelpersMemoryQCalls`).
                let analysis, net11Frontier =
                    unknownOnlyThroughNet11MemoryQCalls
                        (FrameworkUnderTest.runtime ())
                        analysis
                        (methodNamed entry "ReabstractionEntry.Cases" methodName)

                if
                    (escapes.Unknown && not net11Frontier)
                    || not (shown.Contains "=System.EntryPointNotFoundException")
                    || not (Set.isSubset made shown)
                then
                    $"Cases.%s{methodName}: %A{Set.toList shown}, unknown %b{escapes.Unknown}; expected EntryPointNotFoundException, what making it raises (%A{Set.toList made}), and nothing unknown"
                    :: failures,
                    analysis
                else
                    failures, analysis
            )
            |> fst

        if not failures.IsEmpty then
            failwith (String.concat "\n" (List.rev failures))

    /// `Run.Check(double)` is `ldarg.0; ckfinite; ret`, and `Run.Caught(double)` is the same inside
    /// a `catch (OverflowException)` that returns zero. C# has no `ckfinite`, so the IL is emitted
    /// directly.
    let private emitCkfinite () : byte[] =
        let builder =
            System.Reflection.Emit.PersistedAssemblyBuilder (AssemblyName "Finite", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "Finite"

        let run =
            modul.DefineType ("Run", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        let define (name : string) : System.Reflection.Emit.ILGenerator =
            run
                .DefineMethod(
                    name,
                    MethodAttributes.Public ||| MethodAttributes.Static,
                    typeof<double>,
                    [| typeof<double> |]
                )
                .GetILGenerator ()

        do
            let il = define "Check"
            il.Emit System.Reflection.Emit.OpCodes.Ldarg_0
            il.Emit System.Reflection.Emit.OpCodes.Ckfinite
            il.Emit System.Reflection.Emit.OpCodes.Ret

        do
            let il = define "Caught"
            let result = il.DeclareLocal typeof<double>
            il.BeginExceptionBlock () |> ignore<System.Reflection.Emit.Label>
            il.Emit System.Reflection.Emit.OpCodes.Ldarg_0
            il.Emit System.Reflection.Emit.OpCodes.Ckfinite
            il.Emit (System.Reflection.Emit.OpCodes.Stloc, result)
            il.BeginCatchBlock typeof<OverflowException>
            il.Emit System.Reflection.Emit.OpCodes.Pop
            il.Emit (System.Reflection.Emit.OpCodes.Ldc_R8, 0.0)
            il.Emit (System.Reflection.Emit.OpCodes.Stloc, result)
            il.EndExceptionBlock ()
            il.Emit (System.Reflection.Emit.OpCodes.Ldloc, result)
            il.Emit System.Reflection.Emit.OpCodes.Ret

        run.CreateType () |> ignore<Type>

        use stream = new MemoryStream ()
        builder.Save stream
        stream.ToArray ()

    /// CoreCLR's `ckfinite` raises `OverflowException`, not the `ArithmeticException` ECMA-335
    /// III.3.19 names, so a `catch (OverflowException)` absorbs it. The real runtime is asked first,
    /// so that the emitted IL is known to mean what the analysis is held to.
    [<Test>]
    let ``ckfinite raises OverflowException, which a catch of it absorbs`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let image = emitCkfinite ()

        let assembly =
            Assembly.read loggerFactory (Some "Finite.dll") (new MemoryStream (image))

        let onRealRuntime =
            let context =
                System.Runtime.Loader.AssemblyLoadContext ("Finite", isCollectible = true)

            try
                let run = context.LoadFromStream(new MemoryStream (image)).GetType "Run"

                let invoke (name : string) : string =
                    try
                        $"%O{run.GetMethod(name).Invoke ((null : obj), [| box nan |])}"
                    with :? TargetInvocationException as e ->
                        e.InnerException.GetType().FullName

                invoke "Check", invoke "Caught"
            finally
                context.Unload ()

        onRealRuntime |> shouldEqual ("System.OverflowException", "0")

        let analysis, check =
            EscapeAnalysis.escapes (analysisOver [ assembly ] id) (methodNamed assembly "Run" "Check")

        let analysis, caught =
            EscapeAnalysis.escapes analysis (methodNamed assembly "Run" "Caught")

        let check = render analysis check, check.Unknown
        let caught = render analysis caught, caught.Unknown

        let arithmetic =
            Set.ofList [ "=System.OverflowException" ; "=System.ArithmeticException" ]

        (Set.intersect (fst check) arithmetic, snd check)
        |> shouldEqual (Set.singleton "=System.OverflowException", false)

        (Set.intersect (fst caught) arithmetic, snd caught)
        |> shouldEqual (Set.empty, false)

    /// Divisions by divisors, and of dividends, that the body says.
    let private divisionSource =
        """
namespace Division;

public static class Cases
{
    // A divisor known to be neither zero nor -1 raises neither of division's faults.
    public static int Halve(int x) => x / 2;
    public static int Remainder(int x) => x % 3;
    public static uint HalveUnsigned(uint x) => x / 2u;
    public static uint RemainderUnsigned(uint x) => x % 7u;
    public static long Wide(long x) => x / 0x100000000L;

    // Each divisor that reaches the division is one of those.
    public static int ByEither(int x, bool b) => x / (b ? 2 : 3);

    // -1 is not zero, but overflows the least dividend.
    public static int ByMinusOne(int x) => x / -1;

    // A divisor that may be zero, and may not be -1.
    public static int ByEitherOrZero(int x, bool b) => x / (b ? 2 : 0);

    // A dividend known not to be the least overflows under no divisor.
    public static int Into(int d) => 1000 / d;
    public static int IntoLeast(int d) => int.MinValue / d;

    public static int By(int x, int d) => x / d;
}
"""

    let private divideByZero = "=System.DivideByZeroException"
    let private overflow = "=System.OverflowException"

    /// What each method of `divisionSource` lets escape.
    let private divisionExpectations : Expectation list =
        // The method raises nothing, and so constructs no exception.
        let neither (name : string) : Expectation =
            { expect "Division.Cases" name with
                Excludes = [ divideByZero ; overflow ]
                Unknown = Some false
                Assumes = Some []
            }

        [
            neither "Halve"
            neither "Remainder"
            neither "HalveUnsigned"
            neither "RemainderUnsigned"
            neither "Wide"
            neither "ByEither"
            { expect "Division.Cases" "ByMinusOne" with
                Contains = [ overflow ]
                Excludes = [ divideByZero ]
                Unknown = Some false
            }
            { expect "Division.Cases" "ByEitherOrZero" with
                Contains = [ divideByZero ]
                Excludes = [ overflow ]
                Unknown = Some false
            }
            { expect "Division.Cases" "Into" with
                Contains = [ divideByZero ]
                Excludes = [ overflow ]
                Unknown = Some false
            }
            { expect "Division.Cases" "IntoLeast" with
                Contains = [ divideByZero ; overflow ]
                Unknown = Some false
            }
            { expect "Division.Cases" "By" with
                Contains = [ divideByZero ; overflow ]
                Unknown = Some false
            }
        ]

    [<TestCase false>]
    [<TestCase true>]
    let ``a division raises only the faults its known operands allow`` (optimised : bool) : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let image =
            if optimised then
                Roslyn.compileOptimizedAssembly "Division" OutputKind.DynamicallyLinkedLibrary [ divisionSource ]
            else
                Roslyn.compileAssembly "Division" OutputKind.DynamicallyLinkedLibrary [] [ divisionSource ]

        let fixture =
            Assembly.read loggerFactory (Some "Division.dll") (new MemoryStream (image))

        match unmet (analysisOver [ fixture ] id) fixture divisionExpectations with
        | _, [] -> ()
        | _, failures -> failures |> String.concat Environment.NewLine |> failwith

    /// A method of `Run` in `emitDivisions`'s image: its body, of one argument of type
    /// `parameter` that it returns the type of; what the real runtime does when it is passed
    /// `argument`, the value it returns or the full name of what it raises; and which of division's
    /// faults must escape it, and which must not.
    type private EmittedDivision =
        {
            Name : string
            Parameter : Type
            Body : Emit.ILGenerator -> unit
            Argument : obj
            OnRealRuntime : string
            Contains : string list
            Excludes : string list
        }

    /// Divisions C# does not write: by a constant zero, and of one constant by another.
    let private emittedDivisions : EmittedDivision list =
        let ops (ops : Emit.OpCode list) (il : Emit.ILGenerator) : unit =
            for op in ops do
                il.Emit op

        let ldcI4 (value : int32) (il : Emit.ILGenerator) : unit = il.Emit (Emit.OpCodes.Ldc_I4, value)
        let ldcI8 (value : int64) (il : Emit.ILGenerator) : unit = il.Emit (Emit.OpCodes.Ldc_I8, value)

        let body (parts : (Emit.ILGenerator -> unit) list) (il : Emit.ILGenerator) : unit =
            for part in parts do
                part il

        let case name parameter parts argument onRealRuntime contains excludes =
            {
                Name = name
                Parameter = parameter
                Body = body parts
                Argument = argument
                OnRealRuntime = onRealRuntime
                Contains = contains
                Excludes = excludes
            }

        let arg = ops [ Emit.OpCodes.Ldarg_0 ]
        let dbz = "System.DivideByZeroException"
        let ovf = "System.OverflowException"

        [
            case
                "ByZero"
                typeof<int>
                [ arg ; ldcI4 0 ; ops [ Emit.OpCodes.Div ] ]
                (box 7)
                dbz
                [ divideByZero ]
                [ overflow ]
            case
                "UnsignedByZero"
                typeof<int>
                [ arg ; ldcI4 0 ; ops [ Emit.OpCodes.Div_Un ] ]
                (box 7)
                dbz
                [ divideByZero ]
                []
            case
                "RemainderByZero"
                typeof<int>
                [ arg ; ldcI4 0 ; ops [ Emit.OpCodes.Rem ] ]
                (box 7)
                dbz
                [ divideByZero ]
                [ overflow ]
            case
                "UnsignedRemainderByZero"
                typeof<int>
                [ arg ; ldcI4 0 ; ops [ Emit.OpCodes.Rem_Un ] ]
                (box 7)
                dbz
                [ divideByZero ]
                []
            case
                "LeastByMinusOne"
                typeof<int>
                [ ldcI4 Int32.MinValue ; ldcI4 -1 ; ops [ Emit.OpCodes.Div ] ]
                (box 7)
                ovf
                [ overflow ]
                [ divideByZero ]
            case
                "LeastRemainderMinusOne"
                typeof<int>
                [ ldcI4 Int32.MinValue ; ldcI4 -1 ; ops [ Emit.OpCodes.Rem ] ]
                (box 7)
                ovf
                [ overflow ]
                [ divideByZero ]
            case
                "FiveByMinusOne"
                typeof<int>
                [ ldcI4 5 ; ldcI4 -1 ; ops [ Emit.OpCodes.Div ] ]
                (box 7)
                "-5"
                []
                [ divideByZero ; overflow ]
            case
                "UnsignedByMinusOne"
                typeof<int>
                [ arg ; ldcI4 -1 ; ops [ Emit.OpCodes.Div_Un ] ]
                (box 7)
                "0"
                []
                [ divideByZero ; overflow ]
            case
                "ByTwo"
                typeof<int>
                [ arg ; ldcI4 2 ; ops [ Emit.OpCodes.Div ] ]
                (box 7)
                "3"
                []
                [ divideByZero ; overflow ]
            case
                "LongByZero"
                typeof<int64>
                [ arg ; ldcI8 0L ; ops [ Emit.OpCodes.Div ] ]
                (box 7L)
                dbz
                [ divideByZero ]
                [ overflow ]
            case
                "LongByMinusOne"
                typeof<int64>
                [ arg ; ldcI8 -1L ; ops [ Emit.OpCodes.Div ] ]
                (box Int64.MinValue)
                ovf
                [ overflow ]
                [ divideByZero ]
            case
                "LongLeastRemainderMinusOne"
                typeof<int64>
                [ ldcI8 Int64.MinValue ; ldcI8 -1L ; ops [ Emit.OpCodes.Rem ] ]
                (box 7L)
                ovf
                [ overflow ]
                [ divideByZero ]
            // The least int32 is not the least int64.
            case
                "LongOfInt32LeastByMinusOne"
                typeof<int64>
                [ ldcI8 (int64 Int32.MinValue) ; ldcI8 -1L ; ops [ Emit.OpCodes.Div ] ]
                (box 7L)
                "2147483648"
                []
                [ divideByZero ; overflow ]
            // An int32 divisor of a native int is sign-extended to the native int -1.
            case
                "NativeByMinusOne"
                typeof<nativeint>
                [ arg ; ldcI4 -1 ; ops [ Emit.OpCodes.Div ] ]
                (box (
                    if IntPtr.Size = 8 then
                        nativeint Int64.MinValue
                    else
                        nativeint Int32.MinValue
                ))
                ovf
                [ overflow ]
                [ divideByZero ]
            // An int32 and a native int join, but a literal false decides the branch, which the
            // importer folds even where it does not optimise, so only the int32 reaches the join:
            // the least int32 by -1 overflows, though as a native int it would not.
            case
                "FoldedJoinLeastByMinusOne"
                typeof<int>
                [
                    fun il ->
                        let native = il.DefineLabel ()
                        let joined = il.DefineLabel ()
                        il.Emit Emit.OpCodes.Ldc_I4_0
                        il.Emit (Emit.OpCodes.Brtrue, native)
                        il.Emit (Emit.OpCodes.Ldc_I4, Int32.MinValue)
                        il.Emit (Emit.OpCodes.Br, joined)
                        il.MarkLabel native
                        il.Emit Emit.OpCodes.Ldc_I4_0
                        il.Emit Emit.OpCodes.Conv_I
                        il.MarkLabel joined
                        il.Emit Emit.OpCodes.Ldc_I4_M1
                        il.Emit Emit.OpCodes.Div
                        il.Emit Emit.OpCodes.Conv_I4
                ]
                (box 7)
                ovf
                [ overflow ]
                [ divideByZero ]
        ]

    let private emitDivisions () : byte[] =
        let builder =
            Emit.PersistedAssemblyBuilder (AssemblyName "Divisions", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "Divisions"

        let run =
            modul.DefineType ("Run", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        for case in emittedDivisions do
            let il =
                run
                    .DefineMethod(
                        case.Name,
                        MethodAttributes.Public ||| MethodAttributes.Static,
                        case.Parameter,
                        [| case.Parameter |]
                    )
                    .GetILGenerator ()

            case.Body il
            il.Emit Emit.OpCodes.Ret

        run.CreateType () |> ignore<Type>

        use stream = new MemoryStream ()
        builder.Save stream
        stream.ToArray ()

    /// The real runtime is asked first, so that the emitted IL is known to raise what the analysis
    /// must find.
    [<Test>]
    let ``a division in IL C# does not write raises only the faults its known operands allow`` () : unit =
        let image = emitDivisions ()

        let onRealRuntime =
            let context =
                System.Runtime.Loader.AssemblyLoadContext ("Divisions", isCollectible = true)

            try
                let run = context.LoadFromStream(new MemoryStream (image)).GetType "Run"

                emittedDivisions
                |> List.map (fun case ->
                    let outcome =
                        try
                            $"%O{run.GetMethod(case.Name).Invoke ((null : obj), [| case.Argument |])}"
                        with :? TargetInvocationException as e ->
                            e.InnerException.GetType().FullName

                    case.Name, outcome
                )
            finally
                context.Unload ()

        onRealRuntime
        |> shouldEqual (emittedDivisions |> List.map (fun case -> case.Name, case.OnRealRuntime))

        let _, loggerFactory = LoggerFactory.makeTest ()

        let assembly =
            Assembly.read loggerFactory (Some "Divisions.dll") (new MemoryStream (image))

        let expectations =
            emittedDivisions
            |> List.map (fun case ->
                { expect "Run" case.Name with
                    Contains = case.Contains
                    Excludes = case.Excludes
                    Unknown = Some false
                    // A method that raises neither fault constructs no exception.
                    Assumes = if case.Contains.IsEmpty then Some [] else None
                }
            )

        match unmet (analysisOver [ assembly ] id) assembly expectations with
        | _, [] -> ()
        | _, failures -> failures |> String.concat Environment.NewLine |> failwith

    /// The stack type of an integer (ECMA-335 III.1.1).
    [<RequireQualifiedAccess>]
    type private IntegerWidth =
        | Int32
        | Int64
        | Native

    /// A `conv` of an integer that does not check for overflow, and the stack type it pushes
    /// (ECMA-335 III.3.27).
    type private Conversion =
        {
            Op : Emit.OpCode
            Pushes : IntegerWidth
        }

    let private conversions : Conversion list =
        [
            Emit.OpCodes.Conv_I1, IntegerWidth.Int32
            Emit.OpCodes.Conv_I2, IntegerWidth.Int32
            Emit.OpCodes.Conv_I4, IntegerWidth.Int32
            Emit.OpCodes.Conv_I8, IntegerWidth.Int64
            Emit.OpCodes.Conv_U1, IntegerWidth.Int32
            Emit.OpCodes.Conv_U2, IntegerWidth.Int32
            Emit.OpCodes.Conv_U4, IntegerWidth.Int32
            Emit.OpCodes.Conv_U8, IntegerWidth.Int64
            Emit.OpCodes.Conv_I, IntegerWidth.Native
            Emit.OpCodes.Conv_U, IntegerWidth.Native
        ]
        |> List.map (fun (op, pushes) ->
            {
                Op = op
                Pushes = pushes
            }
        )

    /// What a body does to the integer on top of the stack.
    [<RequireQualifiedAccess>]
    type private IntegerStep =
        | Convert of Conversion
        /// `neg`, which keeps the stack type.
        | Negate

        override this.ToString () : string =
            match this with
            | IntegerStep.Convert conversion -> conversion.Op.Name
            | IntegerStep.Negate -> "neg"

    /// An integer that a body makes of constants and divides its argument by, or branches on. It
    /// pushes `Constant` (`ldc.i8` where `Wide`, `ldc.i4` of its low 32 bits otherwise), then takes
    /// each of `Steps`. Where `JoinedWith` is `Some`, it pushes that int32 constant instead where
    /// its Boolean argument is false, and takes `Constant` by `conv.i` where it is true, so that the
    /// two paths meet with an int32 and a native int, which CoreCLR merges as a native int
    /// (ECMA-335 III.1.8.1.3) unless the JIT folds the branch; then it takes `Steps`.
    type private ConvertedDivision =
        {
            Constant : int64
            Wide : bool
            JoinedWith : int32 option
            Steps : IntegerStep list
            Division : Emit.OpCode
        }

        /// The stack type of the divisor, and so of the dividend, which the argument is.
        member this.Width : IntegerWidth =
            let start =
                match this.JoinedWith with
                | Some _ -> IntegerWidth.Native
                | None -> if this.Wide then IntegerWidth.Int64 else IntegerWidth.Int32

            (start, this.Steps)
            ||> List.fold (fun width step ->
                match step with
                | IntegerStep.Convert conversion -> conversion.Pushes
                | IntegerStep.Negate -> width
            )

    let private convertedDivisionGen : FsCheck.Gen<ConvertedDivision> =
        // Most constants that conversions can make zero or -1 have all their low 8, 16 or 32 bits
        // clear, or all set.
        let constant =
            FsCheck.FSharp.Gen.oneof
                [
                    FsCheck.FSharp.Gen.choose (-3, 3) |> FsCheck.FSharp.Gen.map int64
                    FsCheck.FSharp.Gen.map3
                        (fun (high : int) (bits : int) (allSet : bool) ->
                            let shifted = int64 high <<< bits
                            if allSet then shifted ||| ((1L <<< bits) - 1L) else shifted
                        )
                        (FsCheck.FSharp.Gen.choose (-4, 4))
                        (FsCheck.FSharp.Gen.elements [ 8 ; 16 ; 32 ])
                        (FsCheck.FSharp.Gen.elements [ false ; true ])
                    FsCheck.FSharp.Gen.elements
                        [
                            Int64.MinValue
                            Int64.MaxValue
                            int64 Int32.MinValue
                            int64 Int32.MaxValue
                        ]
                    FsCheck.FSharp.ArbMap.defaults |> FsCheck.FSharp.ArbMap.generate<int64>
                ]

        let step =
            FsCheck.FSharp.Gen.frequency
                [
                    4,
                    FsCheck.FSharp.Gen.elements conversions
                    |> FsCheck.FSharp.Gen.map IntegerStep.Convert
                    1, FsCheck.FSharp.Gen.constant IntegerStep.Negate
                ]

        // An int32 joined with a native int differs from the int32 alone only where it is
        // negative, as conversions then extend it.
        let joined =
            FsCheck.FSharp.Gen.frequency
                [
                    2, FsCheck.FSharp.Gen.constant None
                    1,
                    FsCheck.FSharp.Gen.elements [ -1 ; 0 ; 1 ; Int32.MinValue ]
                    |> FsCheck.FSharp.Gen.map Some
                    1, constant |> FsCheck.FSharp.Gen.map (int32 >> Some)
                ]

        FsCheck.FSharp.Gen.map5
            (fun constant wide joinedWith steps division ->
                {
                    Constant = constant
                    Wide = wide
                    JoinedWith = joinedWith
                    Steps = steps
                    Division = division
                }
            )
            constant
            (FsCheck.FSharp.Gen.elements [ false ; true ])
            joined
            (FsCheck.FSharp.Gen.choose (0, 3)
             |> FsCheck.FSharp.Gen.bind (fun count -> FsCheck.FSharp.Gen.listOfLength count step))
            (FsCheck.FSharp.Gen.elements
                [
                    Emit.OpCodes.Div
                    Emit.OpCodes.Rem
                    Emit.OpCodes.Div_Un
                    Emit.OpCodes.Rem_Un
                ])

    /// An image whose type `Run` has, for the `i`th of `cases`, a method `Case<i>` of a dividend and
    /// a Boolean that divides as the case says, and a method `Branch<i>` of a Boolean that makes
    /// the case's integer as it says, then throws an `ArgumentException` where that is not zero, and
    /// a `FormatException` where it is.
    let private emitConvertedDivisions (cases : ConvertedDivision list) : byte[] =
        let builder =
            Emit.PersistedAssemblyBuilder (AssemblyName "ConvertedDivisions", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "ConvertedDivisions"

        let run =
            modul.DefineType ("Run", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        // Pushes the case's integer, reading the Boolean as the argument at `flag`.
        let integer (il : Emit.ILGenerator) (flag : int16) (case : ConvertedDivision) : unit =
            let pushConstant () =
                if case.Wide then
                    il.Emit (Emit.OpCodes.Ldc_I8, case.Constant)
                else
                    il.Emit (Emit.OpCodes.Ldc_I4, int32 case.Constant)

            match case.JoinedWith with
            | None -> pushConstant ()
            | Some joinedWith ->
                let native = il.DefineLabel ()
                let joined = il.DefineLabel ()
                il.Emit (Emit.OpCodes.Ldarg, flag)
                il.Emit (Emit.OpCodes.Brtrue, native)
                il.Emit (Emit.OpCodes.Ldc_I4, joinedWith)
                il.Emit (Emit.OpCodes.Br, joined)
                il.MarkLabel native
                pushConstant ()
                il.Emit Emit.OpCodes.Conv_I
                il.MarkLabel joined
                // The analysis follows both ways a `brtrue` that a branch lands on.
                il.Emit Emit.OpCodes.Nop

            for step in case.Steps do
                match step with
                | IntegerStep.Convert conversion -> il.Emit conversion.Op
                | IntegerStep.Negate -> il.Emit Emit.OpCodes.Neg

        cases
        |> List.iteri (fun index case ->
            let parameter =
                match case.Width with
                | IntegerWidth.Int32 -> typeof<int>
                | IntegerWidth.Int64 -> typeof<int64>
                | IntegerWidth.Native -> typeof<nativeint>

            let il =
                run
                    .DefineMethod(
                        $"Case%d{index}",
                        MethodAttributes.Public ||| MethodAttributes.Static,
                        parameter,
                        [| parameter ; typeof<bool> |]
                    )
                    .GetILGenerator ()

            il.Emit Emit.OpCodes.Ldarg_0
            integer il 1s case
            il.Emit case.Division
            il.Emit Emit.OpCodes.Ret

            let il =
                run
                    .DefineMethod(
                        $"Branch%d{index}",
                        MethodAttributes.Public ||| MethodAttributes.Static,
                        typeof<Void>,
                        [| typeof<bool> |]
                    )
                    .GetILGenerator ()

            let nonZero = il.DefineLabel ()
            integer il 0s case
            il.Emit (Emit.OpCodes.Brtrue, nonZero)
            il.Emit (Emit.OpCodes.Newobj, typeof<FormatException>.GetConstructor Type.EmptyTypes)
            il.Emit Emit.OpCodes.Throw
            il.MarkLabel nonZero
            il.Emit (Emit.OpCodes.Newobj, typeof<ArgumentException>.GetConstructor Type.EmptyTypes)
            il.Emit Emit.OpCodes.Throw
        )

        run.CreateType () |> ignore<Type>

        use stream = new MemoryStream ()
        builder.Save stream
        stream.ToArray ()

    /// Every division of a batch is asked of the real runtime with a dividend of 7, which faults
    /// only on a zero divisor, and with the least dividend of its width, which a signed division by
    /// -1 overflows, each way its Boolean can decide the divisor; the analysis must find exactly
    /// the faults those raise, as the divisor is one of the constants. It must find exactly the
    /// exceptions each branch throws, too. Where an int32 joins a native int, whose width the JIT
    /// decides, it must find at least those.
    [<Test>]
    let ``a converted constant decides a division's faults and a branch as on the real runtime`` () : unit =
        let property (cases : ConvertedDivision list) : unit =
            let image = emitConvertedDivisions cases

            let onRealRuntime : (Set<string> * Set<string>) list =
                let context =
                    System.Runtime.Loader.AssemblyLoadContext ("ConvertedDivisions", isCollectible = true)

                try
                    let run = context.LoadFromStream(new MemoryStream (image)).GetType "Run"

                    cases
                    |> List.mapi (fun index case ->
                        let dividends : obj list =
                            match case.Width with
                            | IntegerWidth.Int32 -> [ box 7 ; box Int32.MinValue ]
                            | IntegerWidth.Int64 -> [ box 7L ; box Int64.MinValue ]
                            | IntegerWidth.Native -> [ box 7n ; box (nativeint Int64.MinValue) ]

                        let flags : obj list =
                            match case.JoinedWith with
                            | Some _ -> [ box false ; box true ]
                            | None -> [ box false ]

                        let raised (name : string) (argument : obj[]) : string option =
                            try
                                run.GetMethod(name).Invoke ((null : obj), argument) |> ignore<obj>
                                None
                            with :? TargetInvocationException as e ->
                                Some ("=" + e.InnerException.GetType().FullName)

                        let divided =
                            [
                                for dividend in dividends do
                                    for flag in flags do
                                        yield! Option.toList (raised $"Case%d{index}" [| dividend ; flag |])
                            ]
                            |> Set.ofList

                        let branched =
                            flags
                            |> List.map (fun flag ->
                                match raised $"Branch%d{index}" [| flag |] with
                                | Some raised -> raised
                                | None -> failwith $"Branch%d{index} returned on the real runtime"
                            )
                            |> Set.ofList

                        divided, branched
                    )
                finally
                    context.Unload ()

            let _, loggerFactory = LoggerFactory.makeTest ()

            let assembly =
                Assembly.read loggerFactory (Some "ConvertedDivisions.dll") (new MemoryStream (image))

            let arithmetic = Set.ofList [ divideByZero ; overflow ]

            let thrown = Set.ofList [ "=System.ArgumentException" ; "=System.FormatException" ]

            let _, failures =
                ((analysisOver [ assembly ] id, []), List.indexed (List.zip cases onRealRuntime))
                ||> List.fold (fun (analysis, failures) (index, (case, (divided, branched))) ->
                    let analysis, escapes =
                        EscapeAnalysis.escapes analysis (methodNamed assembly "Run" $"Case%d{index}")

                    let found = Set.intersect arithmetic (render analysis escapes)

                    // Whether what the analysis finds is what the real runtime raises.
                    let agrees (found : Set<string>) (raised : Set<string>) : bool =
                        match case.JoinedWith with
                        | Some _ -> Set.isSubset raised found
                        | None -> found = raised

                    let failures =
                        if escapes.Unknown || not (agrees found divided) then
                            $"%A{case}: the division finds %A{Set.toList found}, unknown %b{escapes.Unknown}; the real runtime raised %A{Set.toList divided}"
                            :: failures
                        else
                            failures

                    let analysis, escapes =
                        EscapeAnalysis.escapes analysis (methodNamed assembly "Run" $"Branch%d{index}")

                    let found = Set.intersect thrown (render analysis escapes)

                    let failures =
                        if escapes.Unknown || not (agrees found branched) then
                            $"%A{case}: the branch finds %A{Set.toList found}, unknown %b{escapes.Unknown}; the real runtime raised %A{Set.toList branched}"
                            :: failures
                        else
                            failures

                    analysis, failures
                )

            match failures with
            | [] -> ()
            | failures -> failures |> String.concat Environment.NewLine |> failwith

        let conversion (op : Emit.OpCode) : IntegerStep =
            conversions
            |> List.find (fun conversion -> conversion.Op = op)
            |> IntegerStep.Convert

        // Cases a random batch rarely holds.
        property
            [
                // -1, which overflows the least dividend.
                {
                    Constant = 1L
                    Wide = false
                    JoinedWith = None
                    Steps = [ IntegerStep.Negate ; conversion Emit.OpCodes.Conv_I8 ]
                    Division = Emit.OpCodes.Div
                }
                // An int32 -1 joined with a native int is a native int -1, which conv.u8 keeps.
                {
                    Constant = 1L
                    Wide = false
                    JoinedWith = Some -1
                    Steps = [ conversion Emit.OpCodes.Conv_U8 ]
                    Division = Emit.OpCodes.Div
                }
                // The least int32 negates to itself, whose low byte is zero.
                {
                    Constant = int64 Int32.MinValue
                    Wide = false
                    JoinedWith = None
                    Steps = [ IntegerStep.Negate ; conversion Emit.OpCodes.Conv_U1 ]
                    Division = Emit.OpCodes.Div
                }
            ]

        FsCheck.Check.One (
            FsCheck.Config.QuickThrowOnFailure.WithMaxTest 20,
            FsCheck.FSharp.Prop.forAll
                (FsCheck.FSharp.Arb.fromGen (FsCheck.FSharp.Gen.nonEmptyListOf convertedDivisionGen))
                property
        )

    /// What the escape analysis says of each `ReabstractionEntry.Cases` or `AmbiguousEntry.Cases`
    /// method named, which takes a pointer to `ISFoo.SFrob` with a `constrained. ldftn`: what
    /// escapes it, and the places that make it unknown. First, what making `failure`, the
    /// dispatch failure's exception, raises: the runtime runs its parameterless constructor at the
    /// failure.
    let private staticPointerAnswers
        (assemblies : CrossAssemblySpec list)
        (libraryName : string)
        (entryName : string)
        (typeName : string)
        (failure : string)
        (methodNames : string list)
        : Set<string> * (string * Set<string> * Set<string>) list
        =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let compiled = CrossAssemblyHarness.compileAssemblies assemblies

        let read (name : string) : DumpedAssembly =
            Assembly.read loggerFactory (Some $"%s{name}.dll") (new MemoryStream (compiled.[name]))

        let library = read libraryName
        let entry = read entryName

        let analysis, answers =
            ((analysisOver [ library ; entry ] id, []), methodNames)
            ||> List.fold (fun (analysis, answers) methodName ->
                let key = methodNamed entry typeName methodName
                let analysis, escapes = EscapeAnalysis.escapes analysis key
                let analysis, sources = EscapeAnalysis.unknownSources analysis key
                let sources = sources |> Seq.map (renderSite analysis) |> Set.ofSeq
                analysis, (methodName, render analysis escapes, sources) :: answers
            )

        snd (making analysis failure), List.rev answers

    /// `TestCrossAssemblyReabstraction` pins that the real runtime throws `EntryPointNotFoundException`
    /// at the `ldftn` where CoreCLR compiles the frame for its exact instantiation (`SS`), and from
    /// the delegate where it shares the frame's code (`SC`), unless the JIT inlined the frame into
    /// exact code. Either way, the `ldftn` may raise it.
    [<Test>]
    let ``a constrained ldftn landing on a reabstraction may raise EntryPointNotFoundException`` () : unit =
        let made, answers =
            staticPointerAnswers
                [
                    TestCrossAssemblyReabstraction.libraryBefore
                    TestCrossAssemblyReabstraction.entryAssembly
                    TestCrossAssemblyReabstraction.libraryAfter
                ]
                TestCrossAssemblyReabstraction.libraryName
                TestCrossAssemblyReabstraction.entryName
                "ReabstractionEntry.Cases"
                "System.EntryPointNotFoundException"
                [
                    "StaticPointerOnValueType"
                    "StaticPointerOnClass"
                    "StaticPointerResolved"
                    "StaticPointer"
                    "FunctionPointerOnValueType"
                    "FunctionPointerResolved"
                    "StaticFunctionPointer"
                    "FunctionPointerEnabled"
                    "FunctionPointerDisabled"
                ]

        let failures =
            answers
            |> List.choose (fun (methodName, shown, sources) ->
                let raises = shown.Contains "=System.EntryPointNotFoundException"

                let ldftnOpaque =
                    sources
                    |> Set.exists (fun source -> source.EndsWith " Reabstraction.ISFoo::SFrob")

                let ok =
                    match methodName with
                    | "StaticPointerResolved" -> not raises && not ldftnOpaque
                    // No delegate constructor, whose native body is unknown, hides the `ldftn`.
                    | "FunctionPointerResolved" -> not raises && sources.IsEmpty
                    // The `ldftn` is in a branch the `false` its caller passes rules out.
                    | "FunctionPointerDisabled" -> not raises && sources.IsEmpty
                    // Every instantiation at once: the type variable does not decide what the
                    // `ldftn` resolves to.
                    | "StaticPointer"
                    | "StaticFunctionPointer" -> ldftnOpaque
                    // Making the exception runs its parameterless constructor.
                    | _ -> raises && Set.isSubset made shown

                if ok then
                    None
                else
                    Some
                        $"Cases.%s{methodName}: %A{Set.toList shown}, unknown at %A{Set.toList sources}; making the exception raises %A{Set.toList made}"
            )

        if not failures.IsEmpty then
            failwith (String.concat "\n" failures)

    /// In shared code a `constrained. ldftn` whose dispatch fails may make its pointer, which
    /// raises when called, and the analysis attributes that raise to the `ldftn`. That is sound only
    /// while calling the pointer is opaque: a handler around the `ldftn` that the call is outside
    /// would otherwise be credited with catching it. Before making a delegate's `Invoke` or `calli`
    /// transparent, revisit how `callsOf` attributes `LocalFacts.ConstrainedPointers`.
    [<Test>]
    let ``calling a pointer a constrained ldftn loads stays opaque`` () : unit =
        let _, answers =
            staticPointerAnswers
                [
                    TestCrossAssemblyReabstraction.libraryBefore
                    TestCrossAssemblyReabstraction.entryAssembly
                    TestCrossAssemblyReabstraction.libraryAfter
                ]
                TestCrossAssemblyReabstraction.libraryName
                TestCrossAssemblyReabstraction.entryName
                "ReabstractionEntry.Cases"
                "System.EntryPointNotFoundException"
                [ "InvokeStaticPointerOnValueType" ; "CallFunctionPointerOnValueType" ]

        let expected =
            Map.ofList
                [
                    "InvokeStaticPointerOnValueType", "System.Func`1::Invoke entry NativeBody"
                    "CallFunctionPointerOnValueType",
                    "ReabstractionEntry.Cases::CallFunctionPointerOnValueType IL IndirectCall"
                ]

        let failures =
            answers
            |> List.choose (fun (methodName, _, sources) ->
                if sources.Contains expected.[methodName] then
                    None
                else
                    Some
                        $"Cases.%s{methodName}: unknown at %A{Set.toList sources}, lacking %s{expected.[methodName]}; revisit how callsOf attributes ConstrainedPointers before making the call transparent"
            )

        if not failures.IsEmpty then
            failwith (String.concat "\n" failures)

    /// `TestCrossAssemblyAmbiguousDefault` pins that the real runtime throws
    /// `AmbiguousImplementationException` from these.
    [<Test>]
    let ``a constrained ldftn whose most specific body is ambiguous may raise AmbiguousImplementationException``
        ()
        : unit
        =
        let made, answers =
            staticPointerAnswers
                [
                    TestCrossAssemblyAmbiguousDefault.libraryBefore
                    TestCrossAssemblyAmbiguousDefault.entryAssembly
                    TestCrossAssemblyAmbiguousDefault.libraryAfter
                ]
                TestCrossAssemblyAmbiguousDefault.libraryName
                TestCrossAssemblyAmbiguousDefault.entryName
                "AmbiguousEntry.Cases"
                "System.Runtime.AmbiguousImplementationException"
                [ "StaticPointerOnValueType" ; "StaticPointerOnClass" ]

        let failures =
            answers
            |> List.choose (fun (methodName, shown, sources) ->
                // Making the exception runs its parameterless constructor.
                if
                    shown.Contains "=System.Runtime.AmbiguousImplementationException"
                    && Set.isSubset made shown
                then
                    None
                else
                    Some
                        $"Cases.%s{methodName}: %A{Set.toList shown}, unknown at %A{Set.toList sources}; making the exception raises %A{Set.toList made}"
            )

        if not failures.IsEmpty then
            failwith (String.concat "\n" failures)

    /// A statement of a body whose locals each hold an object of one of `LocalFlow`'s classes,
    /// every one of which overrides `Hit` to throw an exception of its own.
    [<RequireQualifiedAccess>]
    type private LocalStatement =
        /// `local = new C<made>()`.
        | Make of local : int * made : int
        /// `local = other`.
        | Copy of local : int * other : int
        /// `if (argument) { ... } else { ... }`, on an argument no other `If` of the body tests.
        | If of argument : int * thenBranch : LocalStatement list * elseBranch : LocalStatement list
        /// `try { ... } finally { ... }`.
        | TryFinally of body : LocalStatement list * cleanup : LocalStatement list
        /// `try { ...; throw new Marker(); } catch (Marker) { ... }`.
        | TryCatch of body : LocalStatement list * handler : LocalStatement list
        /// `ldloca local; pop`: the body takes the local's address.
        | Address of local : int
        /// `ldloca local; newobj C<made>; stind.ref`: a store through the local's address.
        | StoreThrough of local : int * made : int
        /// The statements, run twice by a loop on a counter of its own.
        | Twice of LocalStatement list

    /// A body of `LocalFlow`'s `Run(bool, bool, bool)`: it stores a new `C0` in every local, runs
    /// `Statements`, then calls `Hit` on the local `Final`.
    type private LocalFlowCase =
        {
            Statements : LocalStatement list
            Final : int
        }

    let private localFlowLocals : int = 3
    let private localFlowClasses : int = 4
    let private localFlowArguments : int = 3

    /// Whether the analysis must say exactly what the real runtime raises: where the body has no
    /// handler and takes no local's address, every path through it is a run for some arguments,
    /// since each `If` tests an argument of its own.
    let rec private exactlyDecided (statements : LocalStatement list) : bool =
        statements
        |> List.forall (fun statement ->
            match statement with
            | LocalStatement.Make _
            | LocalStatement.Copy _ -> true
            | LocalStatement.If (_, thenBranch, elseBranch) -> exactlyDecided thenBranch && exactlyDecided elseBranch
            | LocalStatement.TryFinally _
            | LocalStatement.TryCatch _
            | LocalStatement.Address _
            | LocalStatement.StoreThrough _
            | LocalStatement.Twice _ -> false
        )

    let private localFlowGen : FsCheck.Gen<LocalFlowCase> =
        let local = FsCheck.FSharp.Gen.choose (0, localFlowLocals - 1)

        let rec statements (size : int) : FsCheck.Gen<LocalStatement list> =
            FsCheck.FSharp.Gen.choose (0, 3)
            |> FsCheck.FSharp.Gen.bind (fun count -> FsCheck.FSharp.Gen.listOfLength count (statement size))

        and statement (size : int) : FsCheck.Gen<LocalStatement> =
            let leaves =
                [
                    4,
                    FsCheck.FSharp.Gen.map2
                        (fun l c -> LocalStatement.Make (l, c))
                        local
                        (FsCheck.FSharp.Gen.choose (0, localFlowClasses - 1))
                    2, FsCheck.FSharp.Gen.map2 (fun l o -> LocalStatement.Copy (l, o)) local local
                    1, FsCheck.FSharp.Gen.map LocalStatement.Address local
                    1,
                    FsCheck.FSharp.Gen.map2
                        (fun l c -> LocalStatement.StoreThrough (l, c))
                        local
                        (FsCheck.FSharp.Gen.choose (0, localFlowClasses - 1))
                ]

            if size <= 0 then
                FsCheck.FSharp.Gen.frequency leaves
            else
                let nested = statements (size - 1)

                FsCheck.FSharp.Gen.frequency (
                    leaves
                    @ [
                        3, FsCheck.FSharp.Gen.map2 (fun t e -> LocalStatement.If (0, t, e)) nested nested
                        1, FsCheck.FSharp.Gen.map2 (fun b c -> LocalStatement.TryFinally (b, c)) nested nested
                        1, FsCheck.FSharp.Gen.map2 (fun b h -> LocalStatement.TryCatch (b, h)) nested nested
                        1, FsCheck.FSharp.Gen.map LocalStatement.Twice nested
                    ]
                )

        // Give each `If`, in order, an argument of its own, keeping only the `then` branch of one
        // past the last argument.
        let rec numbered (next : int) (statements : LocalStatement list) : int * LocalStatement list =
            ((next, []), statements)
            ||> List.fold (fun (next, done') statement ->
                match statement with
                | LocalStatement.If (_, thenBranch, elseBranch) when next < localFlowArguments ->
                    let next', thenBranch = numbered (next + 1) thenBranch
                    let next', elseBranch = numbered next' elseBranch
                    next', LocalStatement.If (next, thenBranch, elseBranch) :: done'
                | LocalStatement.If (_, thenBranch, _) ->
                    let next, thenBranch = numbered next thenBranch
                    next, List.rev thenBranch @ done'
                | LocalStatement.TryFinally (body, cleanup) ->
                    let next, body = numbered next body
                    let next, cleanup = numbered next cleanup
                    next, LocalStatement.TryFinally (body, cleanup) :: done'
                | LocalStatement.TryCatch (body, handler) ->
                    let next, body = numbered next body
                    let next, handler = numbered next handler
                    next, LocalStatement.TryCatch (body, handler) :: done'
                | LocalStatement.Twice body ->
                    let next, body = numbered next body
                    next, LocalStatement.Twice body :: done'
                | LocalStatement.Make _
                | LocalStatement.Copy _
                | LocalStatement.Address _
                | LocalStatement.StoreThrough _ -> next, statement :: done'
            )
            |> fun (next, done') -> next, List.rev done'

        FsCheck.FSharp.Gen.map2
            (fun statements final ->
                {
                    Statements = snd (numbered 0 statements)
                    Final = final
                }
            )
            (statements 3)
            local

    /// The image of `LocalFlow`, an assembly named `name`, whose `Cases.Run` is `case`'s body.
    let private localFlowImage (name : string) (case : LocalFlowCase) : byte[] =
        let builder =
            Emit.PersistedAssemblyBuilder (AssemblyName name, typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule name

        let exceptionType (typeName : string) : Emit.TypeBuilder * Emit.ConstructorBuilder =
            let defined =
                modul.DefineType (typeName, TypeAttributes.Public ||| TypeAttributes.Sealed, typeof<Exception>)

            defined, defined.DefineDefaultConstructor MethodAttributes.Public

        let marker, markerConstructor = exceptionType "LocalFlow.Marker"

        let baseType =
            modul.DefineType ("LocalFlow.Base", TypeAttributes.Public ||| TypeAttributes.Abstract)

        let baseConstructor = baseType.DefineDefaultConstructor MethodAttributes.Family

        let hit =
            baseType.DefineMethod (
                "Hit",
                MethodAttributes.Public
                ||| MethodAttributes.Virtual
                ||| MethodAttributes.Abstract
                ||| MethodAttributes.HideBySig
                ||| MethodAttributes.NewSlot,
                typeof<Void>,
                [||]
            )

        let classes =
            [
                for index in 0 .. localFlowClasses - 1 do
                    let thrown, thrownConstructor = exceptionType $"LocalFlow.E%d{index}"

                    let made =
                        modul.DefineType (
                            $"LocalFlow.C%d{index}",
                            TypeAttributes.Public ||| TypeAttributes.Sealed,
                            baseType
                        )

                    let constructor =
                        made.DefineConstructor (MethodAttributes.Public, CallingConventions.Standard, [||])

                    let il = constructor.GetILGenerator ()
                    il.Emit Emit.OpCodes.Ldarg_0
                    il.Emit (Emit.OpCodes.Call, baseConstructor)
                    il.Emit Emit.OpCodes.Ret

                    let overriding =
                        made.DefineMethod (
                            "Hit",
                            MethodAttributes.Public
                            ||| MethodAttributes.Virtual
                            ||| MethodAttributes.HideBySig,
                            typeof<Void>,
                            [||]
                        )

                    let il = overriding.GetILGenerator ()
                    il.Emit (Emit.OpCodes.Newobj, thrownConstructor)
                    il.Emit Emit.OpCodes.Throw
                    made.DefineMethodOverride (overriding, hit)
                    yield made, constructor, thrown
            ]

        let cases =
            modul.DefineType (
                "LocalFlow.Cases",
                TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed
            )

        let run =
            cases.DefineMethod (
                "Run",
                MethodAttributes.Public ||| MethodAttributes.Static,
                typeof<Void>,
                Array.replicate localFlowArguments typeof<bool>
            )

        let il = run.GetILGenerator ()

        let locals = [ for _ in 1..localFlowLocals -> il.DeclareLocal baseType ]

        let constructorOf (index : int) =
            let _, constructor, _ = classes.[index]
            constructor

        let rec emit (statement : LocalStatement) : unit =
            match statement with
            | LocalStatement.Make (local, made) ->
                il.Emit (Emit.OpCodes.Newobj, constructorOf made)
                il.Emit (Emit.OpCodes.Stloc, locals.[local])
            | LocalStatement.Copy (local, other) ->
                il.Emit (Emit.OpCodes.Ldloc, locals.[other])
                il.Emit (Emit.OpCodes.Stloc, locals.[local])
            | LocalStatement.If (argument, thenBranch, elseBranch) ->
                let otherwise = il.DefineLabel ()
                let after = il.DefineLabel ()
                il.Emit (Emit.OpCodes.Ldarg, int16 argument)
                il.Emit (Emit.OpCodes.Brfalse, otherwise)
                List.iter emit thenBranch
                il.Emit (Emit.OpCodes.Br, after)
                il.MarkLabel otherwise
                List.iter emit elseBranch
                il.MarkLabel after
            | LocalStatement.TryFinally (body, cleanup) ->
                il.BeginExceptionBlock () |> ignore<Emit.Label>
                List.iter emit body
                il.BeginFinallyBlock ()
                List.iter emit cleanup
                il.EndExceptionBlock ()
            | LocalStatement.TryCatch (body, handler) ->
                il.BeginExceptionBlock () |> ignore<Emit.Label>
                List.iter emit body
                il.Emit (Emit.OpCodes.Newobj, markerConstructor)
                il.Emit Emit.OpCodes.Throw
                il.BeginCatchBlock marker
                il.Emit Emit.OpCodes.Pop
                List.iter emit handler
                il.EndExceptionBlock ()
            | LocalStatement.Address local ->
                il.Emit (Emit.OpCodes.Ldloca, locals.[local])
                il.Emit Emit.OpCodes.Pop
            | LocalStatement.StoreThrough (local, made) ->
                il.Emit (Emit.OpCodes.Ldloca, locals.[local])
                il.Emit (Emit.OpCodes.Newobj, constructorOf made)
                il.Emit Emit.OpCodes.Stind_Ref
            | LocalStatement.Twice body ->
                let counter = il.DeclareLocal typeof<int>
                let head = il.DefineLabel ()
                il.Emit Emit.OpCodes.Ldc_I4_2
                il.Emit (Emit.OpCodes.Stloc, counter)
                il.MarkLabel head
                List.iter emit body
                il.Emit (Emit.OpCodes.Ldloc, counter)
                il.Emit Emit.OpCodes.Ldc_I4_1
                il.Emit Emit.OpCodes.Sub
                il.Emit Emit.OpCodes.Dup
                il.Emit (Emit.OpCodes.Stloc, counter)
                il.Emit (Emit.OpCodes.Brtrue, head)

        for local in locals do
            il.Emit (Emit.OpCodes.Newobj, constructorOf 0)
            il.Emit (Emit.OpCodes.Stloc, local)

        List.iter emit case.Statements
        il.Emit (Emit.OpCodes.Ldloc, locals.[case.Final])
        il.Emit (Emit.OpCodes.Callvirt, hit)
        il.Emit Emit.OpCodes.Ret

        marker.CreateType () |> ignore<Type>
        baseType.CreateType () |> ignore<Type>

        for made, _, thrown in classes do
            thrown.CreateType () |> ignore<Type>
            made.CreateType () |> ignore<Type>

        cases.CreateType () |> ignore<Type>

        use stream = new MemoryStream ()
        builder.Save stream
        stream.ToArray ()

    /// The exceptions of `LocalFlow`'s classes that the real runtime raises from `Cases.Run` in
    /// `image`, over every combination of its arguments, as the expectations spell them.
    let private localFlowRaised (image : byte[]) : Set<string> =
        let context =
            System.Runtime.Loader.AssemblyLoadContext ("LocalFlow", isCollectible = true)

        try
            let run =
                context.LoadFromStream(new MemoryStream (image)).GetType("LocalFlow.Cases").GetMethod ("Run")

            let raisedWith (combination : int) : string =
                let arguments =
                    Array.init localFlowArguments (fun index -> box ((combination >>> index) &&& 1 = 1))

                try
                    run.Invoke ((null : obj), arguments) |> ignore<obj>
                    failwith<string> "LocalFlow.Cases.Run returned, though every class's Hit throws"
                with :? TargetInvocationException as e ->
                    "=" + e.InnerException.GetType().FullName

            [ 0 .. (1 <<< localFlowArguments) - 1 ] |> List.map raisedWith |> Set.ofList
        finally
            context.Unload ()

    [<Test>]
    let ``a virtual call on a local dispatches on what the body stored there, as the real runtime does`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let mutable count = 0

        let property (case : LocalFlowCase) : unit =
            count <- count + 1
            let name = $"LocalFlow%d{count}"
            let image = localFlowImage name case
            let raised = localFlowRaised image

            let fixture =
                Assembly.read loggerFactory (Some $"%s{name}.dll") (new MemoryStream (image))

            let _, escapes =
                EscapeAnalysis.escapes (analysisOver [ fixture ] id) (methodNamed fixture "LocalFlow.Cases" "Run")

            let ours =
                render (analysisOver [ fixture ] id) escapes
                |> Set.filter (fun shown -> shown.StartsWith "=LocalFlow.E")

            if not escapes.Unknown && not (Set.isSubset raised ours) then
                failwith
                    $"%A{case}: the real runtime raises %A{Set.toList raised}, the analysis says only %A{Set.toList ours}"

            if exactlyDecided case.Statements && (escapes.Unknown || ours <> raised) then
                failwith
                    $"%A{case}: the real runtime raises %A{Set.toList raised}, the analysis says %A{Set.toList ours}, unknown %b{escapes.Unknown}"

        // A store into a local that the body then reads.
        property
            {
                Statements = [ LocalStatement.Make (0, 1) ]
                Final = 0
            }

        // Two paths storing different classes meet.
        property
            {
                Statements =
                    [
                        LocalStatement.If (0, [ LocalStatement.Make (0, 1) ], [ LocalStatement.Make (0, 2) ])
                    ]
                Final = 0
            }

        // A copy holds what the local copied from held at the copy.
        property
            {
                Statements = [ LocalStatement.Copy (1, 0) ; LocalStatement.Make (0, 3) ]
                Final = 1
            }

        // A `finally` runs on the way out of its `try`, so what it stores is what the local holds.
        property
            {
                Statements =
                    [
                        LocalStatement.TryFinally ([ LocalStatement.Make (0, 1) ], [ LocalStatement.Make (0, 2) ])
                    ]
                Final = 0
            }

        // A store through a local's address.
        property
            {
                Statements = [ LocalStatement.StoreThrough (0, 2) ]
                Final = 0
            }

        // A loop's second run copies what its first run stored.
        property
            {
                Statements =
                    [
                        LocalStatement.Twice [ LocalStatement.Copy (1, 0) ; LocalStatement.Make (0, 2) ]
                    ]
                Final = 1
            }

        // A handler runs after any of its `try`'s stores.
        property
            {
                Statements =
                    [
                        LocalStatement.TryCatch ([ LocalStatement.Make (0, 1) ], [ LocalStatement.Copy (1, 0) ])
                    ]
                Final = 1
            }

        FsCheck.Check.One (
            FsCheck.Config.QuickThrowOnFailure.WithMaxTest 60,
            FsCheck.FSharp.Prop.forAll (FsCheck.FSharp.Arb.fromGen localFlowGen) property
        )

    /// A local of a sealed type holds an object of exactly that type, so a call on it dispatches
    /// there, though the body stored in it a value the analysis cannot name: an array's element.
    [<Test>]
    let ``a virtual call on a local of a sealed type dispatches there, whatever was stored in it`` () : unit =
        let image =
            let builder =
                Emit.PersistedAssemblyBuilder (AssemblyName "SealedLocal", typeof<obj>.Assembly)

            let modul = builder.DefineDynamicModule "SealedLocal"

            let thrown =
                modul.DefineType (
                    "SealedLocal.Raised",
                    TypeAttributes.Public ||| TypeAttributes.Sealed,
                    typeof<Exception>
                )

            let thrownConstructor = thrown.DefineDefaultConstructor MethodAttributes.Public

            let baseType =
                modul.DefineType ("SealedLocal.Base", TypeAttributes.Public ||| TypeAttributes.Abstract)

            let baseConstructor = baseType.DefineDefaultConstructor MethodAttributes.Family

            let hit =
                baseType.DefineMethod (
                    "Hit",
                    MethodAttributes.Public
                    ||| MethodAttributes.Virtual
                    ||| MethodAttributes.Abstract
                    ||| MethodAttributes.HideBySig
                    ||| MethodAttributes.NewSlot,
                    typeof<Void>,
                    [||]
                )

            let made =
                modul.DefineType ("SealedLocal.Made", TypeAttributes.Public ||| TypeAttributes.Sealed, baseType)

            let constructor =
                made.DefineConstructor (MethodAttributes.Public, CallingConventions.Standard, [||])

            let il = constructor.GetILGenerator ()
            il.Emit Emit.OpCodes.Ldarg_0
            il.Emit (Emit.OpCodes.Call, baseConstructor)
            il.Emit Emit.OpCodes.Ret

            let overriding =
                made.DefineMethod (
                    "Hit",
                    MethodAttributes.Public
                    ||| MethodAttributes.Virtual
                    ||| MethodAttributes.HideBySig,
                    typeof<Void>,
                    [||]
                )

            let il = overriding.GetILGenerator ()
            il.Emit (Emit.OpCodes.Newobj, thrownConstructor)
            il.Emit Emit.OpCodes.Throw
            made.DefineMethodOverride (overriding, hit)

            let cases =
                modul.DefineType (
                    "SealedLocal.Cases",
                    TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed
                )

            let run =
                cases.DefineMethod (
                    "FirstOf",
                    MethodAttributes.Public ||| MethodAttributes.Static,
                    typeof<Void>,
                    [| made.MakeArrayType () |]
                )

            let il = run.GetILGenerator ()
            let local = il.DeclareLocal made
            il.Emit Emit.OpCodes.Ldarg_0
            il.Emit Emit.OpCodes.Ldc_I4_0
            il.Emit Emit.OpCodes.Ldelem_Ref
            il.Emit (Emit.OpCodes.Stloc, local)
            il.Emit (Emit.OpCodes.Ldloc, local)
            il.Emit (Emit.OpCodes.Callvirt, hit)
            il.Emit Emit.OpCodes.Ret

            thrown.CreateType () |> ignore<Type>
            baseType.CreateType () |> ignore<Type>
            made.CreateType () |> ignore<Type>
            cases.CreateType () |> ignore<Type>

            use stream = new MemoryStream ()
            builder.Save stream
            stream.ToArray ()

        let _, loggerFactory = LoggerFactory.makeTest ()

        let fixture =
            Assembly.read loggerFactory (Some "SealedLocal.dll") (new MemoryStream (image))

        let analysis = analysisOver [ fixture ] id

        let analysis, escapes =
            EscapeAnalysis.escapes analysis (methodNamed fixture "SealedLocal.Cases" "FirstOf")

        escapes.Unknown |> shouldEqual false
        render analysis escapes |> shouldContain "=SealedLocal.Raised"

    /// A store of an integer constant into a local of an integer type, then a division by the
    /// local: `ldc; stloc; ldc 1; ldloc; div`, at the widths the local's type loads as.
    type private StoredDivisor =
        {
            /// The local's type.
            Local : Type
            /// The constant stored.
            Stored : int64
            /// Whether the constant is pushed as a native int, rather than at the width the local's
            /// type loads as.
            AsNative : bool
            /// Whether the local is of an enum whose underlying type is `Local`, rather than of
            /// `Local` itself.
            InEnum : bool
            /// Whether `Run` takes the constant as an argument of the type it is pushed as, which
            /// `Caller` passes it, rather than pushing it itself.
            ThroughArgument : bool
            /// Whether `Run` tests the local, dividing by zero where it is zero, rather than
            /// dividing by it.
            ByBranch : bool
        }

    /// The width a local of `local` loads as: 4, 8, or 0 for a native int.
    let private loadedWidth (local : Type) : int =
        if local = typeof<int64> || local = typeof<uint64> then
            8
        elif local = typeof<nativeint> || local = typeof<unativeint> then
            0
        else
            4

    /// Emit into `modul` the case of `divisor` at `index` (`storedDivisorImage`).
    let private emitStoredDivisor (modul : Emit.ModuleBuilder) (index : int) (divisor : StoredDivisor) : unit =
        let cases =
            modul.DefineType (
                $"StoredDivisor.Cases%d{index}",
                TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed
            )

        let localType : Type =
            if divisor.InEnum then
                let enum =
                    modul.DefineEnum ($"StoredDivisor.Narrow%d{index}", TypeAttributes.Public, divisor.Local)

                enum.CreateType ()
            else
                divisor.Local

        let width = loadedWidth divisor.Local
        let pushedWidth = if divisor.AsNative then 0 else width

        let pushedType : Type =
            match pushedWidth with
            | 8 -> typeof<int64>
            | 4 -> typeof<int32>
            | _ -> typeof<nativeint>

        let run =
            cases.DefineMethod (
                "Run",
                MethodAttributes.Public ||| MethodAttributes.Static,
                typeof<Void>,
                (if divisor.ThroughArgument then [| pushedType |] else [||])
            )

        let il = run.GetILGenerator ()
        let local = il.DeclareLocal localType

        // Push `value` at `width`: 4 and 8 as constants of those widths, 0 as a native int.
        let push (il : Emit.ILGenerator) (width : int) (value : int64) : unit =
            match width with
            | 8 -> il.Emit (Emit.OpCodes.Ldc_I8, value)
            | 4 -> il.Emit (Emit.OpCodes.Ldc_I4, int32 value)
            | _ ->
                il.Emit (Emit.OpCodes.Ldc_I8, value)
                il.Emit Emit.OpCodes.Conv_I

        if divisor.ThroughArgument then
            il.Emit Emit.OpCodes.Ldarg_0
        else
            push il pushedWidth divisor.Stored

        il.Emit (Emit.OpCodes.Stloc, local)

        if divisor.ByBranch then
            let skip = il.DefineLabel ()
            il.Emit (Emit.OpCodes.Ldloc, local)
            il.Emit (Emit.OpCodes.Brtrue, skip)
            il.Emit Emit.OpCodes.Ldc_I4_1
            il.Emit Emit.OpCodes.Ldc_I4_0
            il.Emit Emit.OpCodes.Div
            il.Emit Emit.OpCodes.Pop
            il.MarkLabel skip
        else
            push il width 1L
            il.Emit (Emit.OpCodes.Ldloc, local)
            il.Emit Emit.OpCodes.Div
            il.Emit Emit.OpCodes.Pop

        il.Emit Emit.OpCodes.Ret

        let caller =
            cases.DefineMethod ("Caller", MethodAttributes.Public ||| MethodAttributes.Static, typeof<Void>, [||])

        let il = caller.GetILGenerator ()

        if divisor.ThroughArgument then
            push il pushedWidth divisor.Stored

        il.Emit (Emit.OpCodes.Call, run)
        il.Emit Emit.OpCodes.Ret
        cases.CreateType () |> ignore<Type>


    /// The image of `StoredDivisor`: for the divisor at each index `i`, `StoredDivisor.Cases<i>.Run`, a
    /// body as the divisor says, and `Caller`, which calls it.
    let private storedDivisorImage (divisors : StoredDivisor list) : byte[] =
        let builder =
            Emit.PersistedAssemblyBuilder (AssemblyName "StoredDivisor", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "StoredDivisor"

        for index, divisor in List.indexed divisors do
            emitStoredDivisor modul index divisor

        use stream = new MemoryStream ()
        builder.Save stream
        stream.ToArray ()

    [<Test>]
    let ``a division by a local divides by what the store left there, as the real runtime does`` () : unit =

        let locals =
            [
                typeof<bool>
                typeof<char>
                typeof<sbyte>
                typeof<byte>
                typeof<int16>
                typeof<uint16>
                typeof<int32>
                typeof<uint32>
                typeof<int64>
                typeof<uint64>
                typeof<nativeint>
                typeof<unativeint>
            ]

        let constants =
            [
                0L
                1L
                -1L
                2L
                255L
                256L
                65535L
                65536L
                0x1_0000_0000L
                Int64.MinValue
                Int64.MaxValue
            ]

        // An enum's underlying type is an integer type other than bool, char and the native ints.
        let enumerable (local : Type) : bool =
            local <> typeof<bool>
            && local <> typeof<char>
            && local <> typeof<nativeint>
            && local <> typeof<unativeint>

        // Every local type, with each constant at the width it loads as, and as a native int where
        // the store converts it; in a local of the type, and of an enum of it; pushed by the body,
        // and passed by its caller; divided by, and tested.
        let divisors =
            [
                for local in locals do
                    for stored in constants do
                        for asNative in
                            (if loadedWidth local = 4 then
                                 [ false ; true ]
                             else
                                 [ false ]) do
                            for inEnum in (if enumerable local then [ false ; true ] else [ false ]) do
                                for throughArgument in [ false ; true ] do
                                    for byBranch in [ false ; true ] do
                                        yield
                                            {
                                                Local = local
                                                Stored = stored
                                                AsNative = asNative
                                                InEnum = inEnum
                                                ThroughArgument = throughArgument
                                                ByBranch = byBranch
                                            }
            ]

        let image = storedDivisorImage divisors

        let raised =
            let context =
                System.Runtime.Loader.AssemblyLoadContext ("StoredDivisor", isCollectible = true)

            try
                let loaded = context.LoadFromStream (new MemoryStream (image))

                divisors
                |> List.mapi (fun index divisor ->
                    let caller = loaded.GetType($"StoredDivisor.Cases%d{index}").GetMethod ("Caller")

                    try
                        caller.Invoke ((null : obj), [||]) |> ignore<obj>
                        false
                    with :? TargetInvocationException as e ->
                        match e.InnerException with
                        | :? DivideByZeroException -> true
                        | other -> failwith $"%A{divisor}: the real runtime raised %O{other}"
                )
            finally
                context.Unload ()

        let _, loggerFactory = LoggerFactory.makeTest ()

        let fixture =
            Assembly.read loggerFactory (Some "StoredDivisor.dll") (new MemoryStream (image))

        let failures =
            ((analysisOver [ fixture ] id, []), List.indexed (List.zip divisors raised))
            ||> List.fold (fun (analysis, failures) (index, (divisor, raises)) ->
                let analysis, escapes =
                    EscapeAnalysis.escapes analysis (methodNamed fixture $"StoredDivisor.Cases%d{index}" "Caller")

                let says = (render analysis escapes).Contains "=System.DivideByZeroException"

                let failure =
                    if raises && not says && not escapes.Unknown then
                        Some $"%A{divisor}: the real runtime divides by zero, which the analysis rules out"
                    // A constant the store keeps at the width the local loads as is what the local
                    // holds.
                    elif
                        not divisor.AsNative
                        && not divisor.InEnum
                        && not divisor.ThroughArgument
                        && says <> raises
                    then
                        Some $"%A{divisor}: the real runtime divides by zero %b{raises}, the analysis says %b{says}"
                    else
                        None

                analysis, Option.toList failure @ failures
            )
            |> snd

        match failures with
        | [] -> ()
        | failures -> failures |> List.rev |> String.concat Environment.NewLine |> failwith


    /// What one writer of `FieldFlow`'s field stores in it.
    [<RequireQualifiedAccess>]
    type private FieldWrite =
        /// `field = new C<made>()`.
        | Make of made : int
        /// `field = null`.
        | Null
        /// `field = argument`, which `Run` calls the writer with as `new C<made>()`.
        | Argument of made : int
        /// `ldflda field; newobj C<made>; stind.ref`: a store through the field's address.
        | ThroughAddress of made : int
        /// `field = new[] { new C<made>() }[0]`: an array's element, which the analysis does not
        /// follow.
        | Element of made : int

    /// The accessibility of `FieldFlow`'s field.
    [<RequireQualifiedAccess>]
    type private FieldAccess =
        | Private
        | Internal
        | Public

    /// A holder of one field of `FieldFlow.Base`'s type, written by `Writers`, one method each, and
    /// read by `Read`, which calls `Hit` on what the field holds. `Run(bool, bool, bool)` makes a
    /// holder, calls the writer at each index whose argument is true, then calls `Read`.
    type private FieldFlowCase =
        {
            Static : bool
            Access : FieldAccess
            Writers : FieldWrite list
        }

    let private fieldFlowClasses : int = 4
    let private fieldFlowWriters : int = 3

    /// Whether the analysis, allowed `FieldsWrittenVisibly`, must say exactly what the real runtime
    /// raises: where only the field's own assembly can name it, and every writer stores an object it
    /// makes or null in it by `stfld` or `stsfld`. Each writer runs alone for some arguments.
    let private fieldExactlyDecided (case : FieldFlowCase) : bool =
        case.Access <> FieldAccess.Public
        && case.Writers
           |> List.forall (fun write ->
               match write with
               | FieldWrite.Make _
               | FieldWrite.Null -> true
               | FieldWrite.Argument _
               | FieldWrite.ThroughAddress _
               | FieldWrite.Element _ -> false
           )

    let private fieldFlowGen : FsCheck.Gen<FieldFlowCase> =
        let made = FsCheck.FSharp.Gen.choose (0, fieldFlowClasses - 1)

        let write =
            FsCheck.FSharp.Gen.frequency
                [
                    4, FsCheck.FSharp.Gen.map FieldWrite.Make made
                    1, FsCheck.FSharp.Gen.constant FieldWrite.Null
                    1, FsCheck.FSharp.Gen.map FieldWrite.Argument made
                    1, FsCheck.FSharp.Gen.map FieldWrite.ThroughAddress made
                    1, FsCheck.FSharp.Gen.map FieldWrite.Element made
                ]

        FsCheck.FSharp.Gen.map3
            (fun isStatic access writers ->
                {
                    Static = isStatic
                    Access = access
                    Writers = writers
                }
            )
            (FsCheck.FSharp.Gen.elements [ false ; true ])
            (FsCheck.FSharp.Gen.frequency
                [
                    2, FsCheck.FSharp.Gen.constant FieldAccess.Private
                    2, FsCheck.FSharp.Gen.constant FieldAccess.Internal
                    1, FsCheck.FSharp.Gen.constant FieldAccess.Public
                ])
            (FsCheck.FSharp.Gen.choose (1, fieldFlowWriters)
             |> FsCheck.FSharp.Gen.bind (fun count -> FsCheck.FSharp.Gen.listOfLength count write))

    /// The image of `FieldFlow`: the classes every case shares, then each case at index `i` as
    /// `FieldFlow<i>.Holder` and `FieldFlow<i>.Cases.Run`.
    let private fieldFlowImage (cases : FieldFlowCase list) : byte[] =
        let builder =
            Emit.PersistedAssemblyBuilder (AssemblyName "FieldFlow", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "FieldFlow"

        let baseType =
            modul.DefineType ("FieldFlow.Base", TypeAttributes.Public ||| TypeAttributes.Abstract)

        let baseConstructor = baseType.DefineDefaultConstructor MethodAttributes.Family

        let hit =
            baseType.DefineMethod (
                "Hit",
                MethodAttributes.Public
                ||| MethodAttributes.Virtual
                ||| MethodAttributes.Abstract
                ||| MethodAttributes.HideBySig
                ||| MethodAttributes.NewSlot,
                typeof<Void>,
                [||]
            )

        let classes =
            [
                for index in 0 .. fieldFlowClasses - 1 do
                    let thrown =
                        modul.DefineType (
                            $"FieldFlow.E%d{index}",
                            TypeAttributes.Public ||| TypeAttributes.Sealed,
                            typeof<Exception>
                        )

                    let thrownConstructor = thrown.DefineDefaultConstructor MethodAttributes.Public

                    let made =
                        modul.DefineType (
                            $"FieldFlow.C%d{index}",
                            TypeAttributes.Public ||| TypeAttributes.Sealed,
                            baseType
                        )

                    let constructor =
                        made.DefineConstructor (MethodAttributes.Public, CallingConventions.Standard, [||])

                    let il = constructor.GetILGenerator ()
                    il.Emit Emit.OpCodes.Ldarg_0
                    il.Emit (Emit.OpCodes.Call, baseConstructor)
                    il.Emit Emit.OpCodes.Ret

                    let overriding =
                        made.DefineMethod (
                            "Hit",
                            MethodAttributes.Public
                            ||| MethodAttributes.Virtual
                            ||| MethodAttributes.HideBySig,
                            typeof<Void>,
                            [||]
                        )

                    let il = overriding.GetILGenerator ()
                    il.Emit (Emit.OpCodes.Newobj, thrownConstructor)
                    il.Emit Emit.OpCodes.Throw
                    made.DefineMethodOverride (overriding, hit)
                    yield thrown, made, constructor
            ]

        let constructorOf (index : int) =
            let _, _, constructor = classes.[index]
            constructor

        // `Check` throws `ENull` for null: what it is passed decides its branch, so a field taken
        // never to be null would rule that out.
        let nullThrown =
            modul.DefineType ("FieldFlow.ENull", TypeAttributes.Public ||| TypeAttributes.Sealed, typeof<Exception>)

        let nullThrownConstructor =
            nullThrown.DefineDefaultConstructor MethodAttributes.Public

        let checks =
            modul.DefineType (
                "FieldFlow.Checks",
                TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed
            )

        let check =
            checks.DefineMethod (
                "Check",
                MethodAttributes.Public ||| MethodAttributes.Static,
                typeof<Void>,
                [| baseType :> Type |]
            )

        let il = check.GetILGenerator ()
        let present = il.DefineLabel ()
        il.Emit Emit.OpCodes.Ldarg_0
        il.Emit (Emit.OpCodes.Brtrue, present)
        il.Emit (Emit.OpCodes.Newobj, nullThrownConstructor)
        il.Emit Emit.OpCodes.Throw
        il.MarkLabel present
        il.Emit Emit.OpCodes.Ret

        let caseTypes =
            [
                for index, case in List.indexed cases do
                    let holder =
                        modul.DefineType ($"FieldFlow%d{index}.Holder", TypeAttributes.Public ||| TypeAttributes.Sealed)

                    let holderConstructor = holder.DefineDefaultConstructor MethodAttributes.Public

                    let access =
                        match case.Access with
                        | FieldAccess.Private -> FieldAttributes.Private
                        | FieldAccess.Internal -> FieldAttributes.Assembly
                        | FieldAccess.Public -> FieldAttributes.Public

                    let field =
                        holder.DefineField (
                            "held",
                            baseType,
                            (if case.Static then
                                 access ||| FieldAttributes.Static
                             else
                                 access)
                        )

                    let instanceMethod = MethodAttributes.Public ||| MethodAttributes.HideBySig

                    let writers =
                        [
                            for writerIndex, write in List.indexed case.Writers do
                                let parameters =
                                    match write with
                                    | FieldWrite.Argument _ -> [| baseType :> Type |]
                                    | FieldWrite.Make _
                                    | FieldWrite.Null
                                    | FieldWrite.ThroughAddress _
                                    | FieldWrite.Element _ -> [||]

                                let writer =
                                    holder.DefineMethod (
                                        $"Write%d{writerIndex}",
                                        instanceMethod,
                                        typeof<Void>,
                                        parameters
                                    )

                                let il = writer.GetILGenerator ()

                                if not case.Static then
                                    il.Emit Emit.OpCodes.Ldarg_0

                                match write with
                                | FieldWrite.Make made -> il.Emit (Emit.OpCodes.Newobj, constructorOf made)
                                | FieldWrite.Null -> il.Emit Emit.OpCodes.Ldnull
                                | FieldWrite.Argument _ -> il.Emit Emit.OpCodes.Ldarg_1
                                | FieldWrite.ThroughAddress made ->
                                    if case.Static then
                                        il.Emit (Emit.OpCodes.Ldsflda, field)
                                    else
                                        il.Emit (Emit.OpCodes.Ldflda, field)

                                    il.Emit (Emit.OpCodes.Newobj, constructorOf made)
                                | FieldWrite.Element made ->
                                    il.Emit Emit.OpCodes.Ldc_I4_1
                                    il.Emit (Emit.OpCodes.Newarr, baseType)
                                    il.Emit Emit.OpCodes.Dup
                                    il.Emit Emit.OpCodes.Ldc_I4_0
                                    il.Emit (Emit.OpCodes.Newobj, constructorOf made)
                                    il.Emit Emit.OpCodes.Stelem_Ref
                                    il.Emit Emit.OpCodes.Ldc_I4_0
                                    il.Emit Emit.OpCodes.Ldelem_Ref

                                match write, case.Static with
                                | FieldWrite.ThroughAddress _, _ -> il.Emit Emit.OpCodes.Stind_Ref
                                | _, true -> il.Emit (Emit.OpCodes.Stsfld, field)
                                | _, false -> il.Emit (Emit.OpCodes.Stfld, field)

                                // A store through the address of an instance field leaves `this`
                                // beneath it.
                                match write, case.Static with
                                | FieldWrite.ThroughAddress _, false -> il.Emit Emit.OpCodes.Pop
                                | _ -> ()

                                il.Emit Emit.OpCodes.Ret
                                yield writer, write
                        ]

                    let read = holder.DefineMethod ("Read", instanceMethod, typeof<Void>, [||])
                    let il = read.GetILGenerator ()

                    if case.Static then
                        il.Emit (Emit.OpCodes.Ldsfld, field)
                    else
                        il.Emit Emit.OpCodes.Ldarg_0
                        il.Emit (Emit.OpCodes.Ldfld, field)

                    il.Emit Emit.OpCodes.Dup
                    il.Emit (Emit.OpCodes.Call, check)
                    il.Emit (Emit.OpCodes.Callvirt, hit)
                    il.Emit Emit.OpCodes.Ret

                    let cases =
                        modul.DefineType (
                            $"FieldFlow%d{index}.Cases",
                            TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed
                        )

                    let run =
                        cases.DefineMethod (
                            "Run",
                            MethodAttributes.Public ||| MethodAttributes.Static,
                            typeof<Void>,
                            Array.replicate fieldFlowWriters typeof<bool>
                        )

                    let il = run.GetILGenerator ()
                    let made = il.DeclareLocal holder
                    il.Emit (Emit.OpCodes.Newobj, holderConstructor)
                    il.Emit (Emit.OpCodes.Stloc, made)

                    for writerIndex, (writer, write) in List.indexed writers do
                        let skip = il.DefineLabel ()
                        il.Emit (Emit.OpCodes.Ldarg, int16 writerIndex)
                        il.Emit (Emit.OpCodes.Brfalse, skip)
                        il.Emit (Emit.OpCodes.Ldloc, made)

                        match write with
                        | FieldWrite.Argument passed -> il.Emit (Emit.OpCodes.Newobj, constructorOf passed)
                        | FieldWrite.Make _
                        | FieldWrite.Null
                        | FieldWrite.ThroughAddress _
                        | FieldWrite.Element _ -> ()

                        il.Emit (Emit.OpCodes.Call, writer)
                        il.MarkLabel skip

                    il.Emit (Emit.OpCodes.Ldloc, made)
                    il.Emit (Emit.OpCodes.Call, read)
                    il.Emit Emit.OpCodes.Ret
                    yield holder, cases
            ]

        baseType.CreateType () |> ignore<Type>
        nullThrown.CreateType () |> ignore<Type>
        checks.CreateType () |> ignore<Type>

        for thrown, made, _ in classes do
            thrown.CreateType () |> ignore<Type>
            made.CreateType () |> ignore<Type>

        for holder, cases in caseTypes do
            holder.CreateType () |> ignore<Type>
            cases.CreateType () |> ignore<Type>

        use stream = new MemoryStream ()
        builder.Save stream
        stream.ToArray ()

    /// What the real runtime raises from each case's `Run` in `image`, over every combination of its
    /// arguments, as the expectations spell them.
    let private fieldFlowRaised (image : byte[]) (count : int) : Set<string> list =
        let context =
            System.Runtime.Loader.AssemblyLoadContext ("FieldFlow", isCollectible = true)

        try
            let loaded = context.LoadFromStream (new MemoryStream (image))

            [
                for index in 0 .. count - 1 do
                    let run = loaded.GetType($"FieldFlow%d{index}.Cases").GetMethod ("Run")

                    let raisedWith (combination : int) : string =
                        let arguments =
                            Array.init fieldFlowWriters (fun bit -> box ((combination >>> bit) &&& 1 = 1))

                        try
                            run.Invoke ((null : obj), arguments) |> ignore<obj>
                            failwith<string> "FieldFlow's Run returned, though every class's Hit throws"
                        with :? TargetInvocationException as e ->
                            "=" + e.InnerException.GetType().FullName

                    yield [ 0 .. (1 <<< fieldFlowWriters) - 1 ] |> List.map raisedWith |> Set.ofList
            ]
        finally
            context.Unload ()

    [<Test>]
    let ``a virtual call on a field dispatches on what its writers store there, as the real runtime does`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let property (cases : FieldFlowCase list) : unit =
            let image = fieldFlowImage cases
            let raised = fieldFlowRaised image cases.Length

            let fixture =
                Assembly.read loggerFactory (Some "FieldFlow.dll") (new MemoryStream (image))

            let analysisAllowing (assumptions : Set<Assumption>) =
                analysisOf
                    (hostCoreLib ())
                    (FrameworkUnderTest.runtimeDirs ())
                    (hostTarget ())
                    HardwareIntrinsicsProfile.ScalarOnly
                    assumptions
                    [ fixture ]
                    id

            // What the analysis says of each case's `Run`: the exceptions of `FieldFlow`'s classes
            // and `Check`'s, whether it is unknown, and whether it relied on `FieldsWrittenVisibly`.
            let answers (assumptions : Set<Assumption>) : (Set<string> * bool * bool) list =
                ((analysisAllowing assumptions, []), List.indexed cases)
                ||> List.fold (fun (analysis, answers) (index, _) ->
                    let analysis, escapes =
                        EscapeAnalysis.escapes analysis (methodNamed fixture $"FieldFlow%d{index}.Cases" "Run")

                    let ours =
                        render analysis escapes
                        |> Set.filter (fun shown -> shown.StartsWith "=FieldFlow.E")

                    analysis,
                    (ours, escapes.Unknown, escapes.Assumes.Contains Assumption.FieldsWrittenVisibly)
                    :: answers
                )
                |> snd
                |> List.rev

            let allowed = answers Assumption.all
            let withheld = answers (Set.remove Assumption.FieldsWrittenVisibly Assumption.all)

            let failures =
                List.zip3 cases raised (List.zip allowed withheld)
                |> List.choose (fun (case, raised, ((ours, unknown, assumes), (_, unknownWithheld, _))) ->
                    if not unknown && not (Set.isSubset raised ours) then
                        Some
                            $"%A{case}: the real runtime raises %A{Set.toList raised}, the analysis says only %A{Set.toList ours}"
                    elif fieldExactlyDecided case && (unknown || ours <> raised || not assumes) then
                        Some
                            $"%A{case}: the real runtime raises %A{Set.toList raised}, the analysis says %A{Set.toList ours}, unknown %b{unknown}, assuming %b{assumes}"
                    // Where another assembly may write the field, or a writer stores something the
                    // analysis does not follow, nothing says what the field holds beyond its type.
                    elif not (fieldExactlyDecided case) && not unknown then
                        Some $"%A{case}: the analysis decides the call, though it cannot know what the field holds"
                    // Without the assumption, nothing says what the field holds beyond its type, on
                    // which `Hit` is abstract.
                    elif not unknownWithheld then
                        Some $"%A{case}: without FieldsWrittenVisibly the analysis still decides the call"
                    else
                        None
                )

            match failures with
            | [] -> ()
            | failures -> failures |> String.concat Environment.NewLine |> failwith

        // A private field each writer makes an object for, or nulls.
        property
            [
                {
                    Static = false
                    Access = FieldAccess.Private
                    Writers = [ FieldWrite.Make 1 ; FieldWrite.Make 2 ]
                }
                {
                    Static = true
                    Access = FieldAccess.Internal
                    Writers = [ FieldWrite.Make 3 ; FieldWrite.Null ]
                }
                // A store through the field's address may store anything.
                {
                    Static = true
                    Access = FieldAccess.Private
                    Writers = [ FieldWrite.Make 1 ; FieldWrite.ThroughAddress 2 ]
                }
                // Another assembly may write a public field.
                {
                    Static = false
                    Access = FieldAccess.Public
                    Writers = [ FieldWrite.Make 0 ]
                }
                // What a writer is passed, its callers decide.
                {
                    Static = false
                    Access = FieldAccess.Private
                    Writers = [ FieldWrite.Argument 3 ]
                }
            ]

        FsCheck.Check.One (
            FsCheck.Config.QuickThrowOnFailure.WithMaxTest 10,
            FsCheck.FSharp.Prop.forAll
                (FsCheck.FSharp.Arb.fromGen (FsCheck.FSharp.Gen.nonEmptyListOf fieldFlowGen))
                property
        )

    /// An image named `name` with an abstract `Fields.Base`, whose `Hit` `Fields.C1` overrides to
    /// throw `Fields.E1`, and whatever `emit` adds, given the module, `Base`, `Hit` and `C1`'s
    /// constructor; with `InternalsVisibleTo` each of `friends`.
    let private fieldFixture
        (name : string)
        (friends : string list)
        (emit : Emit.ModuleBuilder -> Emit.TypeBuilder -> Emit.MethodBuilder -> Emit.ConstructorBuilder -> unit)
        : byte[]
        =
        let builder =
            Emit.PersistedAssemblyBuilder (AssemblyName name, typeof<obj>.Assembly)

        for friend in friends do
            builder.SetCustomAttribute (
                Emit.CustomAttributeBuilder (
                    typeof<System.Runtime.CompilerServices.InternalsVisibleToAttribute>.GetConstructor
                        [| typeof<string> |],
                    [| box friend |]
                )
            )

        let modul = builder.DefineDynamicModule name

        let baseType =
            modul.DefineType ("Fields.Base", TypeAttributes.Public ||| TypeAttributes.Abstract)

        let baseConstructor = baseType.DefineDefaultConstructor MethodAttributes.Family

        let hit =
            baseType.DefineMethod (
                "Hit",
                MethodAttributes.Public
                ||| MethodAttributes.Virtual
                ||| MethodAttributes.Abstract
                ||| MethodAttributes.HideBySig
                ||| MethodAttributes.NewSlot,
                typeof<Void>,
                [||]
            )

        let thrown =
            modul.DefineType ("Fields.E1", TypeAttributes.Public ||| TypeAttributes.Sealed, typeof<Exception>)

        let thrownConstructor = thrown.DefineDefaultConstructor MethodAttributes.Public

        let made =
            modul.DefineType ("Fields.C1", TypeAttributes.Public ||| TypeAttributes.Sealed, baseType)

        let constructor =
            made.DefineConstructor (MethodAttributes.Public, CallingConventions.Standard, [||])

        let il = constructor.GetILGenerator ()
        il.Emit Emit.OpCodes.Ldarg_0
        il.Emit (Emit.OpCodes.Call, baseConstructor)
        il.Emit Emit.OpCodes.Ret

        let overriding =
            made.DefineMethod (
                "Hit",
                MethodAttributes.Public
                ||| MethodAttributes.Virtual
                ||| MethodAttributes.HideBySig,
                typeof<Void>,
                [||]
            )

        let il = overriding.GetILGenerator ()
        il.Emit (Emit.OpCodes.Newobj, thrownConstructor)
        il.Emit Emit.OpCodes.Throw
        made.DefineMethodOverride (overriding, hit)

        emit modul baseType hit constructor

        baseType.CreateType () |> ignore<Type>
        thrown.CreateType () |> ignore<Type>
        made.CreateType () |> ignore<Type>

        use stream = new MemoryStream ()
        builder.Save stream
        stream.ToArray ()

    /// What the real runtime raises from `Fields.Cases.Run` in `image`, as the expectations spell it.
    let private fieldFixtureRaises (name : string) (image : byte[]) : string =
        let context = System.Runtime.Loader.AssemblyLoadContext (name, isCollectible = true)

        try
            let run =
                context.LoadFromStream(new MemoryStream (image)).GetType("Fields.Cases").GetMethod ("Run")

            try
                run.Invoke ((null : obj), [||]) |> ignore<obj>
                failwith<string> "Fields.Cases.Run returned, though it must throw"
            with :? TargetInvocationException as e ->
                "=" + e.InnerException.GetType().FullName
        finally
            context.Unload ()

    /// What the analysis, allowed every assumption, says of `Fields.Cases.Run` in `image`, and what
    /// the real runtime raises from it, as the expectations spell them.
    let private fieldFixtureAnswers (name : string) (image : byte[]) : Set<string> * bool * string =
        let raised = fieldFixtureRaises name image
        let _, loggerFactory = LoggerFactory.makeTest ()

        let fixture =
            Assembly.read loggerFactory (Some $"%s{name}.dll") (new MemoryStream (image))

        let analysis = analysisOver [ fixture ] id

        let analysis, escapes =
            EscapeAnalysis.escapes analysis (methodNamed fixture "Fields.Cases" "Run")

        render analysis escapes, escapes.Unknown, raised

    [<Test>]
    let ``a field another field of an explicit layout overlaps holds what is stored in either`` () : unit =
        // `b` lies over `a`, so storing in `a` stores in `b`, which no instruction stores to.
        let image =
            fieldFixture
                "OverlappingFields"
                []
                (fun modul baseType hit made ->
                    let overlap =
                        modul.DefineType (
                            "Fields.Overlap",
                            TypeAttributes.Public
                            ||| TypeAttributes.Sealed
                            ||| TypeAttributes.ExplicitLayout
                        )

                    let overlapConstructor = overlap.DefineDefaultConstructor MethodAttributes.Public
                    let a = overlap.DefineField ("a", baseType, FieldAttributes.Private)
                    a.SetOffset 0
                    let b = overlap.DefineField ("b", baseType, FieldAttributes.Private)
                    b.SetOffset 0

                    let run =
                        overlap.DefineMethod (
                            "Run",
                            MethodAttributes.Public ||| MethodAttributes.Static,
                            typeof<Void>,
                            [||]
                        )

                    let il = run.GetILGenerator ()
                    il.Emit (Emit.OpCodes.Newobj, overlapConstructor)
                    il.Emit Emit.OpCodes.Dup
                    il.Emit (Emit.OpCodes.Newobj, made)
                    il.Emit (Emit.OpCodes.Stfld, a)
                    il.Emit (Emit.OpCodes.Ldfld, b)
                    il.Emit (Emit.OpCodes.Callvirt, hit)
                    il.Emit Emit.OpCodes.Ret

                    let cases =
                        modul.DefineType (
                            "Fields.Cases",
                            TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed
                        )

                    let caseRun =
                        cases.DefineMethod (
                            "Run",
                            MethodAttributes.Public ||| MethodAttributes.Static,
                            typeof<Void>,
                            [||]
                        )

                    let il = caseRun.GetILGenerator ()
                    il.Emit (Emit.OpCodes.Call, run)
                    il.Emit Emit.OpCodes.Ret
                    overlap.CreateType () |> ignore<Type>
                    cases.CreateType () |> ignore<Type>
                )

        let shown, unknown, raised = fieldFixtureAnswers "OverlappingFields" image
        raised |> shouldEqual "=Fields.E1"

        if not unknown && not (shown.Contains raised) then
            failwith $"The real runtime raises %s{raised}, which the analysis rules out: %A{Set.toList shown}"

    [<Test>]
    let ``an internal field of an assembly with a friend holds what the friend may store`` () : unit =
        let image =
            fieldFixture
                "FriendlyFields"
                [ "Friend" ]
                (fun modul baseType hit made ->
                    let cases =
                        modul.DefineType (
                            "Fields.Cases",
                            TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed
                        )

                    let held =
                        cases.DefineField ("held", baseType, FieldAttributes.Assembly ||| FieldAttributes.Static)

                    let run =
                        cases.DefineMethod (
                            "Run",
                            MethodAttributes.Public ||| MethodAttributes.Static,
                            typeof<Void>,
                            [||]
                        )

                    let il = run.GetILGenerator ()
                    il.Emit (Emit.OpCodes.Newobj, made)
                    il.Emit (Emit.OpCodes.Stsfld, held)
                    il.Emit (Emit.OpCodes.Ldsfld, held)
                    il.Emit (Emit.OpCodes.Callvirt, hit)
                    il.Emit Emit.OpCodes.Ret
                    cases.CreateType () |> ignore<Type>
                )

        // `Friend` may store any `Base` in it, so nothing says which class `Hit` dispatches on.
        let _, unknown, raised = fieldFixtureAnswers "FriendlyFields" image
        raised |> shouldEqual "=Fields.E1"
        unknown |> shouldEqual true

    [<Test>]
    let ``no field CoreCLR's native code binds by name is taken to hold only what IL stores`` () : unit =
        let corelib = hostCoreLib ()

        let typeName (handle : TypeDefinitionHandle) : string =
            TypeInfo.fullName (fun handle -> corelib.TypeDefs.[handle]) corelib.TypeDefs.[handle]

        let fields =
            corelib.Fields
            |> Seq.map (fun (KeyValue (handle, field)) ->
                (typeName field.DeclaringType.Identity.TypeDefinition.Get, field.Name), handle
            )
            |> List.ofSeq

        let mutable analysis = analysisOver [] id

        let follows (handle : FieldDefinitionHandle) : bool =
            let next, follows =
                EscapeAnalysis.followsField analysis (FieldKey.make corelib.DefinitionFullName handle)

            analysis <- next
            follows

        let followedBound =
            fields
            |> List.filter (fun (named, _) -> (VmBoundField.all.Force ()).Contains named)
            |> List.filter (snd >> follows)
            |> List.map fst

        followedBound |> shouldEqual []

        // The analysis does follow some of CoreLib's fields, or the check above says nothing.
        fields |> List.exists (snd >> follows) |> shouldEqual true

    /// The fields of the host CoreLib, by declaring type's full name and own name, with each one's
    /// attributes and handle.
    let private coreLibFields
        (corelib : DumpedAssembly)
        : ((string * string) * FieldAttributes * FieldDefinitionHandle) list
        =
        let typeName (handle : TypeDefinitionHandle) : string =
            TypeInfo.fullName (fun handle -> corelib.TypeDefs.[handle]) corelib.TypeDefs.[handle]

        corelib.Fields
        |> Seq.map (fun (KeyValue (handle, field)) ->
            (typeName field.DeclaringType.Identity.TypeDefinition.Get, field.Name), field.Attributes, handle
        )
        |> List.ofSeq

    /// `MethodDesc::AllocateStubMethodInfo` (method.cpp) writes `RuntimeMethodInfoStub.m_keepalive`
    /// through the C++ layout of the stub's class (`ReflectMethodObject::SetKeepAlive`, object.h), and
    /// `FieldDesc::AllocateStubFieldInfo` (field.cpp) writes `RuntimeFieldInfoStub.m_keepalive` through
    /// `ReflectFieldObject::SetKeepAlive`, where no instruction names either field and no row of
    /// corelib.h binds it. (On .NET 10 the field stub also has sequential layout, which the analysis
    /// refuses on its own; the method stub has automatic layout on every supported runtime.)
    [<TestCase("System.RuntimeMethodInfoStub")>]
    [<TestCase("System.RuntimeFieldInfoStub")>]
    let ``a field CoreCLR writes through its class's native layout is not taken to hold only what IL stores``
        (stub : string)
        : unit
        =
        let corelib = hostCoreLib ()

        let keepAlive =
            coreLibFields corelib
            |> List.pick (fun (named, _, handle) -> if named = (stub, "m_keepalive") then Some handle else None)

        (VmBoundField.all.Force ()).Contains (stub, "m_keepalive") |> shouldEqual false

        let _, follows =
            EscapeAnalysis.followsField (analysisOver [] id) (FieldKey.make corelib.DefinitionFullName keepAlive)

        follows |> shouldEqual false

    /// A static field is not part of any object, so no C++ layout of its class reaches it: the VM
    /// finds a static only by name (`VmBoundField`), and `ExecutionContext.Default` is bound by none.
    [<Test>]
    let ``a static field of a class CoreCLR lays out natively holds only what IL stores`` () : unit =
        let corelib = hostCoreLib ()
        let named = "System.Threading.ExecutionContext", "Default"
        VmLaidOutClass.isLaidOut (fst named) |> shouldEqual true
        (VmBoundField.all.Force ()).Contains named |> shouldEqual false

        let _, attributes, handle =
            coreLibFields corelib |> List.find (fun (candidate, _, _) -> candidate = named)

        attributes.HasFlag FieldAttributes.Static |> shouldEqual true

        let _, follows =
            EscapeAnalysis.followsField (analysisOver [] id) (FieldKey.make corelib.DefinitionFullName handle)

        follows |> shouldEqual true

    [<Test>]
    let ``no instance field of a class CoreCLR lays out natively is taken to hold only what IL stores`` () : unit =
        let corelib = hostCoreLib ()
        let fields = coreLibFields corelib
        let mutable analysis = analysisOver [] id

        let follows (handle : FieldDefinitionHandle) : bool =
            let next, follows =
                EscapeAnalysis.followsField analysis (FieldKey.make corelib.DefinitionFullName handle)

            analysis <- next
            follows

        let ofLaidOutClass =
            fields
            |> List.filter (fun ((typeName, _), attributes, _) ->
                VmLaidOutClass.isLaidOut typeName
                && not (attributes.HasFlag FieldAttributes.Static)
            )

        // The check says something only if CoreLib declares instance fields of such classes.
        ofLaidOutClass.IsEmpty |> shouldEqual false

        let followed =
            ofLaidOutClass
            |> List.filter (fun (_, _, handle) -> follows handle)
            |> List.map (fun ((typeName, fieldName), _, _) -> $"%s{typeName}\t%s{fieldName}")

        if not followed.IsEmpty then
            let followed = String.concat "\n" followed

            failwith $"The analysis follows these instance fields of classes CoreCLR lays out natively:\n%s{followed}"

    /// A store into a field through a member reference naming it, which neither C# nor
    /// Reflection.Emit writes for a field of a type that is not generic, so the IL is built directly:
    /// `Write` stores a string in `M.Holder.held` through a member reference whose parent is
    /// `Holder` itself, and `Read` loads the field by its definition and calls `ToString` on it.
    [<Test>]
    let ``a field a member reference may store to holds what that store does`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let metadata = MetadataBuilder ()
        let ilStream = BlobBuilder ()
        let bodies = MethodBodyStreamEncoder ilStream

        metadata.AddModule (
            0,
            metadata.GetOrAddString "FieldReferences.dll",
            metadata.GetOrAddGuid (Guid "6d1e2f3a-4b5c-4d6e-8f90-a1b2c3d4e5f6"),
            Unchecked.defaultof<GuidHandle>,
            Unchecked.defaultof<GuidHandle>
        )
        |> ignore<ModuleDefinitionHandle>

        metadata.AddAssembly (
            metadata.GetOrAddString "FieldReferences",
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

        let toString =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .MethodSignature(isInstanceMethod = true)
                .Parameters (0, (fun returnType -> returnType.Type().String ()), ignore<ParametersEncoder>)

            metadata.AddMemberReference (
                (TypeReferenceHandle.op_Implicit objectRef : EntityHandle),
                metadata.GetOrAddString "ToString",
                metadata.GetOrAddBlob blob
            )

        let objectField =
            let blob = BlobBuilder ()
            BlobEncoder(blob).Field().Type().Object ()
            metadata.GetOrAddBlob blob

        let held =
            metadata.AddFieldDefinition (
                FieldAttributes.Private ||| FieldAttributes.Static,
                metadata.GetOrAddString "held",
                objectField
            )

        // The holder is the second type definition, after `<Module>`.
        let holder = MetadataTokens.TypeDefinitionHandle 2

        let heldByReference =
            metadata.AddMemberReference (
                (TypeDefinitionHandle.op_Implicit holder : EntityHandle),
                metadata.GetOrAddString "held",
                objectField
            )

        let staticVoid =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .MethodSignature()
                .Parameters (0, (fun returnType -> returnType.Void ()), ignore<ParametersEncoder>)

            metadata.GetOrAddBlob blob

        let write =
            let code = InstructionEncoder (BlobBuilder ())
            code.LoadString (metadata.GetOrAddUserString "stored")
            code.OpCode ILOpCode.Stsfld
            code.Token (MemberReferenceHandle.op_Implicit heldByReference : EntityHandle)
            code.OpCode ILOpCode.Ret
            bodies.AddMethodBody code

        let read =
            let code = InstructionEncoder (BlobBuilder ())
            code.OpCode ILOpCode.Ldsfld
            code.Token (FieldDefinitionHandle.op_Implicit held : EntityHandle)
            code.OpCode ILOpCode.Callvirt
            code.Token (MemberReferenceHandle.op_Implicit toString : EntityHandle)
            code.OpCode ILOpCode.Pop
            code.OpCode ILOpCode.Ret
            bodies.AddMethodBody code

        let writeMethod =
            metadata.AddMethodDefinition (
                MethodAttributes.Public ||| MethodAttributes.Static,
                MethodImplAttributes.IL,
                metadata.GetOrAddString "Write",
                staticVoid,
                write,
                MetadataTokens.ParameterHandle 1
            )

        metadata.AddMethodDefinition (
            MethodAttributes.Public ||| MethodAttributes.Static,
            MethodImplAttributes.IL,
            metadata.GetOrAddString "Read",
            staticVoid,
            read,
            MetadataTokens.ParameterHandle 1
        )
        |> ignore<MethodDefinitionHandle>

        metadata.AddTypeDefinition (
            TypeAttributes.Class,
            Unchecked.defaultof<StringHandle>,
            metadata.GetOrAddString "<Module>",
            Unchecked.defaultof<EntityHandle>,
            held,
            writeMethod
        )
        |> ignore<TypeDefinitionHandle>

        metadata.AddTypeDefinition (
            TypeAttributes.Public
            ||| TypeAttributes.Class
            ||| TypeAttributes.Abstract
            ||| TypeAttributes.Sealed,
            metadata.GetOrAddString "M",
            metadata.GetOrAddString "Holder",
            (TypeReferenceHandle.op_Implicit objectRef : EntityHandle),
            held,
            writeMethod
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
            Assembly.read loggerFactory (Some "FieldReferences.dll") (new MemoryStream (image.ToArray ()))

        let analysis = analysisOver [ assembly ] id

        let _, escapes =
            EscapeAnalysis.escapes analysis (methodNamed assembly "M.Holder" "Read")

        // `Write` may have stored any object, whose `ToString` may raise anything.
        escapes.Unknown |> shouldEqual true

    /// A field one of whose writers the analysis takes a contract for, in place of its body: the
    /// contract says nothing of what the writer stores. CoreLib's type initializers, under
    /// `CoreLibTypeInitializers`, are such writers; `Encoding`'s stores its default encoding.
    [<Test>]
    let ``a field a writer under a contract stores to holds anything of its type`` () : unit =
        let source =
            """
namespace ContractWriters;

public static class Cases
{
    public static int Count() => System.Text.Encoding.Default.GetByteCount((string)null);
}
"""

        let image =
            Roslyn.compileOptimizedAssembly "ContractWriters" OutputKind.DynamicallyLinkedLibrary [ source ]

        let raised =
            let context =
                System.Runtime.Loader.AssemblyLoadContext ("ContractWriters", isCollectible = true)

            try
                let count =
                    context
                        .LoadFromStream(new MemoryStream (image))
                        .GetType("ContractWriters.Cases")
                        .GetMethod ("Count")

                try
                    count.Invoke ((null : obj), [||]) |> ignore<obj>
                    "nothing"
                with :? TargetInvocationException as e ->
                    "=" + e.InnerException.GetType().FullName
            finally
                context.Unload ()

        raised |> shouldEqual "=System.ArgumentNullException"

        let _, loggerFactory = LoggerFactory.makeTest ()

        let fixture =
            Assembly.read loggerFactory (Some "ContractWriters.dll") (new MemoryStream (image))

        let analysis = analysisOver [ fixture ] id

        let analysis, escapes =
            EscapeAnalysis.escapes analysis (methodNamed fixture "ContractWriters.Cases" "Count")

        if not escapes.Unknown && not ((render analysis escapes).Contains raised) then
            failwith
                $"The real runtime raises %s{raised}, which the analysis rules out: %A{Set.toList (render analysis escapes)}"

    /// A method that dispatches on what a field holds relies on `FieldsWrittenVisibly` wherever it
    /// came by the value: inside a handler that catches every exception, or from a callee that
    /// loaded the field. Were the assumption to fail, the call would dispatch elsewhere, outside
    /// every handler.
    [<Test>]
    let ``what a field holds is relied on wherever the value goes`` () : unit =
        let source =
            """
namespace Relying;

public abstract class Base { public abstract void Hit(); }

// Its `Hit` runs nothing, so that nothing else the call reaches relies on the assumption.
public sealed class Made : Base { public override void Hit() { } }

public static class Cases
{
    private static Base held;

    public static void Write() { held = new Made(); }

    public static Base Get() => held;

    public static void CaughtLoad()
    {
        Base local;
        try { local = held; }
        catch (System.Exception) { return; }
        local.Hit();
    }

    public static void CaughtCall()
    {
        Base local;
        try { local = Get(); }
        catch (System.Exception) { return; }
        local.Hit();
    }
}
"""

        let _, loggerFactory = LoggerFactory.makeTest ()

        let fixture =
            Roslyn.compileOptimizedAssembly "Relying" OutputKind.DynamicallyLinkedLibrary [ source ]
            |> fun image -> Assembly.read loggerFactory (Some "Relying.dll") (new MemoryStream (image))

        let mutable analysis = analysisOver [ fixture ] id

        for name in [ "CaughtLoad" ; "CaughtCall" ] do
            let next, escapes =
                EscapeAnalysis.escapes analysis (methodNamed fixture "Relying.Cases" name)

            analysis <- next

            // The call dispatches on `Made`, which only the assumption says, or on null.
            if
                escapes.Unknown
                || not (escapes.Assumes.Contains Assumption.FieldsWrittenVisibly)
            then
                failwith
                    $"%s{name}: unknown %b{escapes.Unknown}, assuming %A{Set.toList escapes.Assumes}, which must hold FieldsWrittenVisibly"

    /// A field CoreLib's type initializer writes, where the analysis takes `CoreLibTypeInitializers`'s
    /// contract in place of the initializer: the contract says what the initializer raises, and its
    /// IL still says what it stores. `Random()` dispatches on what its type's initializer stores.
    [<Test>]
    let ``what a type initializer under a contract stores in a field is what its IL stores`` () : unit =
        let corelib = hostCoreLib ()

        let constructor =
            corelib.Methods
            |> Seq.find (fun (KeyValue (_, method)) ->
                let declaring = corelib.TypeDefs.[method.RequiredDeclaringType.Definition.Get]

                declaring.Namespace = "System"
                && declaring.Name = "Random"
                && not declaring.IsNested
                && method.Name = ".ctor"
                && method.Signature.ParameterTypes.IsEmpty
            )
            |> fun (KeyValue (handle, _)) -> MethodKey.make corelib handle

        let _, escapes = EscapeAnalysis.escapes (analysisOver [] id) constructor
        escapes.Unknown |> shouldEqual false
        escapes.Assumes |> shouldContain Assumption.FieldsWrittenVisibly

    /// A field of a struct an explicit layout embeds: another field of the container may lie over it,
    /// so that a store in that one is a store in the struct's, though the struct's own layout is
    /// sequential.
    [<Test>]
    let ``a struct's field another field of an explicit layout overlaps holds what is stored in either`` () : unit =
        let image =
            fieldFixture
                "OverlappingStructFields"
                []
                (fun modul baseType hit made ->
                    let payload =
                        modul.DefineType (
                            "Fields.Payload",
                            TypeAttributes.Public
                            ||| TypeAttributes.Sealed
                            ||| TypeAttributes.SequentialLayout,
                            typeof<ValueType>
                        )

                    let held = payload.DefineField ("held", baseType, FieldAttributes.Private)

                    let read =
                        payload.DefineMethod (
                            "Read",
                            MethodAttributes.Public ||| MethodAttributes.HideBySig,
                            typeof<Void>,
                            [||]
                        )

                    let il = read.GetILGenerator ()
                    il.Emit Emit.OpCodes.Ldarg_0
                    il.Emit (Emit.OpCodes.Ldfld, held)
                    il.Emit (Emit.OpCodes.Callvirt, hit)
                    il.Emit Emit.OpCodes.Ret
                    payload.CreateType () |> ignore<Type>

                    // `alias` lies over `payload.held`.
                    let container =
                        modul.DefineType (
                            "Fields.Container",
                            TypeAttributes.Public
                            ||| TypeAttributes.Sealed
                            ||| TypeAttributes.ExplicitLayout
                        )

                    let containerConstructor =
                        container.DefineDefaultConstructor MethodAttributes.Public

                    let inner = container.DefineField ("payload", payload, FieldAttributes.Public)
                    inner.SetOffset 0
                    let alias = container.DefineField ("alias", baseType, FieldAttributes.Public)
                    alias.SetOffset 0
                    container.CreateType () |> ignore<Type>

                    let cases =
                        modul.DefineType (
                            "Fields.Cases",
                            TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed
                        )

                    let run =
                        cases.DefineMethod (
                            "Run",
                            MethodAttributes.Public ||| MethodAttributes.Static,
                            typeof<Void>,
                            [||]
                        )

                    let il = run.GetILGenerator ()
                    il.Emit (Emit.OpCodes.Newobj, containerConstructor)
                    il.Emit Emit.OpCodes.Dup
                    il.Emit (Emit.OpCodes.Newobj, made)
                    il.Emit (Emit.OpCodes.Stfld, alias)
                    il.Emit (Emit.OpCodes.Ldflda, inner)
                    il.Emit (Emit.OpCodes.Call, read)
                    il.Emit Emit.OpCodes.Ret
                    cases.CreateType () |> ignore<Type>
                )

        let shown, unknown, raised = fieldFixtureAnswers "OverlappingStructFields" image
        raised |> shouldEqual "=Fields.E1"

        if not unknown && not (shown.Contains raised) then
            failwith $"The real runtime raises %s{raised}, which the analysis rules out: %A{Set.toList shown}"

    /// A field of a class with sequential layout, which the interop marshaller writes as a
    /// formatted class: `Marshal.PtrToStructure` fills it from unmanaged memory, as does a P/Invoke
    /// that takes the class as an `[Out]` parameter, where no instruction names the field.
    [<Test>]
    let ``a field of a class with sequential layout holds what the marshaller writes in it`` () : unit =
        let marshal (name : string) (parameters : Type[]) : MethodInfo =
            typeof<System.Runtime.InteropServices.Marshal>.GetMethod (name, parameters)

        let image =
            fieldFixture
                "FormattedFields"
                []
                (fun modul _ _ _ ->
                    let formatted =
                        modul.DefineType (
                            "Fields.Formatted",
                            TypeAttributes.Public
                            ||| TypeAttributes.Sealed
                            ||| TypeAttributes.SequentialLayout
                        )

                    let formattedConstructor =
                        formatted.DefineDefaultConstructor MethodAttributes.Public

                    // No instruction of the image stores to `text`.
                    let text = formatted.DefineField ("text", typeof<string>, FieldAttributes.Private)

                    let run =
                        formatted.DefineMethod (
                            "Run",
                            MethodAttributes.Public ||| MethodAttributes.Static,
                            typeof<Void>,
                            [||]
                        )

                    let il = run.GetILGenerator ()
                    let memory = il.DeclareLocal typeof<IntPtr>
                    let holder = il.DeclareLocal formatted
                    // Unmanaged memory the size of the class, holding a pointer to an ANSI string.
                    il.Emit (Emit.OpCodes.Ldtoken, formatted)

                    il.Emit (
                        Emit.OpCodes.Call,
                        typeof<Type>.GetMethod ("GetTypeFromHandle", [| typeof<RuntimeTypeHandle> |])
                    )

                    il.Emit (Emit.OpCodes.Call, marshal "SizeOf" [| typeof<Type> |])
                    il.Emit (Emit.OpCodes.Call, marshal "AllocHGlobal" [| typeof<int> |])
                    il.Emit (Emit.OpCodes.Stloc, memory)
                    il.Emit (Emit.OpCodes.Ldloc, memory)
                    il.Emit (Emit.OpCodes.Ldc_I4_0)
                    il.Emit (Emit.OpCodes.Ldstr, "written by the marshaller")
                    il.Emit (Emit.OpCodes.Call, marshal "StringToHGlobalAnsi" [| typeof<string> |])

                    il.Emit (
                        Emit.OpCodes.Call,
                        marshal "WriteIntPtr" [| typeof<IntPtr> ; typeof<int> ; typeof<IntPtr> |]
                    )
                    // The marshaller fills a fresh `Formatted` from it.
                    il.Emit (Emit.OpCodes.Newobj, formattedConstructor)
                    il.Emit (Emit.OpCodes.Stloc, holder)
                    il.Emit (Emit.OpCodes.Ldloc, memory)
                    il.Emit (Emit.OpCodes.Ldloc, holder)
                    il.Emit (Emit.OpCodes.Call, marshal "PtrToStructure" [| typeof<IntPtr> ; typeof<obj> |])
                    // `text.Substring 1000` raises `ArgumentOutOfRangeException`, where a null `text`
                    // would raise `NullReferenceException`.
                    il.Emit (Emit.OpCodes.Ldloc, holder)
                    il.Emit (Emit.OpCodes.Ldfld, text)
                    il.Emit (Emit.OpCodes.Ldc_I4, 1000)
                    il.Emit (Emit.OpCodes.Callvirt, typeof<string>.GetMethod ("Substring", [| typeof<int> |]))
                    il.Emit Emit.OpCodes.Pop
                    il.Emit Emit.OpCodes.Ret

                    let cases =
                        modul.DefineType (
                            "Fields.Cases",
                            TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed
                        )

                    let caseRun =
                        cases.DefineMethod (
                            "Run",
                            MethodAttributes.Public ||| MethodAttributes.Static,
                            typeof<Void>,
                            [||]
                        )

                    let il = caseRun.GetILGenerator ()
                    il.Emit (Emit.OpCodes.Call, run)
                    il.Emit Emit.OpCodes.Ret
                    formatted.CreateType () |> ignore<Type>
                    cases.CreateType () |> ignore<Type>
                )

        // The marshaller did write `text`: the real runtime gets past the load of it.
        fieldFixtureRaises "FormattedFields" image
        |> shouldEqual "=System.ArgumentOutOfRangeException"

        // The marshalling is opaque to the analysis, so its answer for `Run` says nothing here; what
        // it must not do is take `text` to hold only what IL stores in it.
        let _, loggerFactory = LoggerFactory.makeTest ()

        let fixture =
            Assembly.read loggerFactory (Some "FormattedFields.dll") (new MemoryStream (image))

        let text =
            fixture.Fields
            |> Seq.pick (fun (KeyValue (handle, field)) -> if field.Name = "text" then Some handle else None)

        let _, follows =
            EscapeAnalysis.followsField (analysisOver [ fixture ] id) (FieldKey.make fixture.DefinitionFullName text)

        follows |> shouldEqual false
