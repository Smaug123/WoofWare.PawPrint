namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Runtime.CompilerServices
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `throw` of an object that is not a `System.Exception` (ECMA-335 III.4.26 permits any object
/// reference), against the real runtime.
///
/// CoreCLR's `IL_Throw` wraps such an object in a `RuntimeWrappedException` the moment it is
/// thrown (`WrapNonCompliantException` in `vm/excep.cpp`), and dispatches the wrapper. Each
/// `catch` or `filter` clause then sees the wrapper only if its assembly carries
/// `[assembly: RuntimeCompatibility(WrapNonExceptionThrows = true)]`, and the wrapped object
/// otherwise (`PossiblyUnwrapThrowable`, and `ShouldTypedClauseCatchThisException` in the shared
/// managed `ExceptionHandling.cs` for a typed clause's match). Roslyn always emits the attribute;
/// `PersistedAssemblyBuilder` never does, so the fabricated assembly here is one that does not wrap
/// and the C# driver is one that does.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFabricatedNonExceptionThrow =

    /// `Thrower`, an assembly without `RuntimeCompatibility`:
    /// * `Throw()`: `ldstr "not an exception"; throw`.
    /// * `CatchObject() : object`: calls `Throw` inside `catch (object)` and returns what the
    ///   handler saw.
    /// * `CatchObjectAndRethrow()`: calls `Throw` inside `catch (object)`, whose handler stores
    ///   what it saw in the static field `Caught` and then `rethrow`s.
    /// * `CatchWrapper() : int`: calls `Throw` inside `catch (RuntimeWrappedException)`, returning 1
    ///   from the handler.
    /// * `FilterSeesString() : int`: calls `Throw` inside a `filter` that accepts exactly when what
    ///   it is shown is a `string`; returns 1 if the handler ran.
    /// * `CatchString(Action) : object`: invokes the action inside `catch (string)` and returns what
    ///   the handler saw.
    /// * `CatchComparable(Action) : int`, `CatchSerializable(Action) : int`: invoke the action inside
    ///   `catch (IComparable)` and `catch (ISerializable)` respectively, returning 1 from the handler.
    /// * `ThrowStrings()`: throws a `string[]`.
    /// * `CatchObjects() : int`: calls `ThrowStrings` inside `catch (object[])`, returning 1 from the
    ///   handler.
    /// * `FilterThenCatchString(Action) : int`: invokes the action inside a `try` whose first clause
    ///   is a `filter` accepting everything, returning 1 from its handler, and whose second is
    ///   `catch (string)`, returning 2.
    let private fabricate () : byte[] =
        let builder =
            PersistedAssemblyBuilder (AssemblyName "Thrower", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "Thrower"

        let thrower =
            modul.DefineType ("Thrower", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        let attributes = MethodAttributes.Public ||| MethodAttributes.Static

        let caught =
            thrower.DefineField ("Caught", typeof<obj>, FieldAttributes.Public ||| FieldAttributes.Static)

        let throwMethod = thrower.DefineMethod ("Throw", attributes, typeof<Void>, [||])

        do
            let il = throwMethod.GetILGenerator ()
            il.Emit (OpCodes.Ldstr, "not an exception")
            il.Emit OpCodes.Throw

        do
            let il =
                thrower.DefineMethod("CatchObject", attributes, typeof<obj>, [||]).GetILGenerator ()

            let seen = il.DeclareLocal typeof<obj>
            il.BeginExceptionBlock () |> ignore<Label>
            il.Emit (OpCodes.Call, throwMethod)
            il.BeginCatchBlock typeof<obj>
            il.Emit (OpCodes.Stloc, seen)
            il.EndExceptionBlock ()
            il.Emit (OpCodes.Ldloc, seen)
            il.Emit OpCodes.Ret

        do
            let il =
                thrower.DefineMethod("CatchObjectAndRethrow", attributes, typeof<Void>, [||]).GetILGenerator ()

            // ILGenerator does not count the exception object a handler starts with towards
            // `maxstack`, so without a push of its own this body would declare 0, and CoreCLR
            // rejects it as an invalid program.
            il.Emit OpCodes.Ldnull
            il.Emit OpCodes.Pop

            il.BeginExceptionBlock () |> ignore<Label>
            il.Emit (OpCodes.Call, throwMethod)
            il.BeginCatchBlock typeof<obj>
            il.Emit (OpCodes.Stsfld, caught)
            il.Emit OpCodes.Rethrow
            il.EndExceptionBlock ()
            il.Emit OpCodes.Ret

        do
            let il =
                thrower.DefineMethod("CatchWrapper", attributes, typeof<int>, [||]).GetILGenerator ()

            let result = il.DeclareLocal typeof<int>
            il.BeginExceptionBlock () |> ignore<Label>
            il.Emit (OpCodes.Call, throwMethod)
            il.BeginCatchBlock typeof<RuntimeWrappedException>
            il.Emit OpCodes.Pop
            il.Emit OpCodes.Ldc_I4_1
            il.Emit (OpCodes.Stloc, result)
            il.EndExceptionBlock ()
            il.Emit (OpCodes.Ldloc, result)
            il.Emit OpCodes.Ret

        do
            let il =
                thrower.DefineMethod("FilterSeesString", attributes, typeof<int>, [||]).GetILGenerator ()

            let result = il.DeclareLocal typeof<int>
            il.BeginExceptionBlock () |> ignore<Label>
            il.Emit (OpCodes.Call, throwMethod)
            il.BeginExceptFilterBlock ()
            il.Emit (OpCodes.Isinst, typeof<string>)
            il.Emit OpCodes.Ldnull
            il.Emit OpCodes.Cgt_Un
            il.BeginCatchBlock null
            il.Emit OpCodes.Pop
            il.Emit OpCodes.Ldc_I4_1
            il.Emit (OpCodes.Stloc, result)
            il.EndExceptionBlock ()
            il.Emit (OpCodes.Ldloc, result)
            il.Emit OpCodes.Ret

        do
            let invoke =
                typeof<Action>.GetMethod "Invoke"
                |> Option.ofObj
                |> Option.defaultWith (fun () -> failwith "Action has no Invoke")

            let il =
                thrower.DefineMethod("CatchString", attributes, typeof<obj>, [| typeof<Action> |]).GetILGenerator ()

            let seen = il.DeclareLocal typeof<obj>
            il.BeginExceptionBlock () |> ignore<Label>
            il.Emit OpCodes.Ldarg_0
            il.Emit (OpCodes.Callvirt, invoke)
            il.BeginCatchBlock typeof<string>
            il.Emit (OpCodes.Stloc, seen)
            il.EndExceptionBlock ()
            il.Emit (OpCodes.Ldloc, seen)
            il.Emit OpCodes.Ret

        let invoke =
            typeof<Action>.GetMethod "Invoke"
            |> Option.ofObj
            |> Option.defaultWith (fun () -> failwith "Action has no Invoke")

        let catchingInvoke (name : string) (clauseType : Type) =
            let il =
                thrower.DefineMethod(name, attributes, typeof<int>, [| typeof<Action> |]).GetILGenerator ()

            let result = il.DeclareLocal typeof<int>
            il.BeginExceptionBlock () |> ignore<Label>
            il.Emit OpCodes.Ldarg_0
            il.Emit (OpCodes.Callvirt, invoke)
            il.BeginCatchBlock clauseType
            il.Emit OpCodes.Pop
            il.Emit OpCodes.Ldc_I4_1
            il.Emit (OpCodes.Stloc, result)
            il.EndExceptionBlock ()
            il.Emit (OpCodes.Ldloc, result)
            il.Emit OpCodes.Ret

        catchingInvoke "CatchComparable" typeof<IComparable>
        catchingInvoke "CatchSerializable" typeof<System.Runtime.Serialization.ISerializable>

        let throwStrings =
            thrower.DefineMethod ("ThrowStrings", attributes, typeof<Void>, [||])

        do
            let il = throwStrings.GetILGenerator ()
            il.Emit OpCodes.Ldc_I4_0
            il.Emit (OpCodes.Newarr, typeof<string>)
            il.Emit OpCodes.Throw

        do
            let il =
                thrower.DefineMethod("CatchObjects", attributes, typeof<int>, [||]).GetILGenerator ()

            let result = il.DeclareLocal typeof<int>
            il.BeginExceptionBlock () |> ignore<Label>
            il.Emit (OpCodes.Call, throwStrings)
            il.BeginCatchBlock typeof<obj[]>
            il.Emit OpCodes.Pop
            il.Emit OpCodes.Ldc_I4_1
            il.Emit (OpCodes.Stloc, result)
            il.EndExceptionBlock ()
            il.Emit (OpCodes.Ldloc, result)
            il.Emit OpCodes.Ret

        do
            let il =
                thrower
                    .DefineMethod("FilterThenCatchString", attributes, typeof<int>, [| typeof<Action> |])
                    .GetILGenerator ()

            let result = il.DeclareLocal typeof<int>
            il.BeginExceptionBlock () |> ignore<Label>
            il.Emit OpCodes.Ldarg_0
            il.Emit (OpCodes.Callvirt, invoke)
            il.BeginExceptFilterBlock ()
            il.Emit OpCodes.Pop
            il.Emit OpCodes.Ldc_I4_1
            il.BeginCatchBlock null
            il.Emit OpCodes.Pop
            il.Emit OpCodes.Ldc_I4_1
            il.Emit (OpCodes.Stloc, result)
            il.BeginCatchBlock typeof<string>
            il.Emit OpCodes.Pop
            il.Emit OpCodes.Ldc_I4_2
            il.Emit (OpCodes.Stloc, result)
            il.EndExceptionBlock ()
            il.Emit (OpCodes.Ldloc, result)
            il.Emit OpCodes.Ret

        thrower.CreateType () |> ignore<Type>

        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    /// Each row returns its own nonzero code if the runtime disagrees with CoreCLR there, so a
    /// failure names the row.
    let private driverSource : string =
        """
using System;
using System.Runtime.CompilerServices;

public static class Driver
{
    static bool ContainsSubstring(string haystack, string needle)
    {
        for (int i = 0; i <= haystack.Length - needle.Length; i++)
        {
            bool matches = true;
            for (int j = 0; j < needle.Length; j++)
            {
                if (haystack[i + j] != needle[j])
                {
                    matches = false;
                    break;
                }
            }

            if (matches)
            {
                return true;
            }
        }

        return false;
    }

    public static int Main(string[] args)
    {
        // Before the runtime has wrapped anything, a wrapper the guest made and threw is matched
        // against a clause that does not wrap by what it wraps, but the handler is shown the
        // wrapper itself: only a wrap teaches the runtime which type to unwrap.
        RuntimeWrappedException made = new RuntimeWrappedException("made");
        object seenBeforeAnyWrap = Thrower.CatchString(() => { throw made; });
        if (!ReferenceEquals(seenBeforeAnyWrap, made)) return 20;

        // A clause in a wrapping assembly sees a thrown string as an exception.
        try
        {
            Thrower.Throw();
            return 1;
        }
        catch (Exception)
        {
        }

        // ... namely a RuntimeWrappedException around the very string thrown.
        try
        {
            Thrower.Throw();
            return 2;
        }
        catch (RuntimeWrappedException e)
        {
            string s = e.WrappedException as string;
            if (s == null || s != "not an exception") return 3;
            if (e.HResult != unchecked((int)0x8013153E)) return 21;
            if (e.Message != "An object that does not derive from System.Exception has been wrapped in a RuntimeWrappedException.") return 22;
        }

        // A filter in a wrapping assembly sees the wrapper too.
        try
        {
            Thrower.Throw();
            return 4;
        }
        catch (Exception e) when (e is RuntimeWrappedException)
        {
        }

        // A clause in an assembly that does not wrap sees the string itself.
        if (!(Thrower.CatchObject() is string)) return 5;

        // ... and so does a filter there.
        if (Thrower.FilterSeesString() != 1) return 12;

        // ... and a clause there for the wrapper does not catch it.
        try
        {
            Thrower.CatchWrapper();
            return 13;
        }
        catch (RuntimeWrappedException)
        {
        }

        // A rethrow from that clause rethrows the wrapper, around what the clause saw.
        try
        {
            Thrower.CatchObjectAndRethrow();
            return 6;
        }
        catch (RuntimeWrappedException e)
        {
            if (Thrower.Caught == null || !ReferenceEquals(e.WrappedException, Thrower.Caught)) return 7;
        }

        // A rethrow in a wrapping assembly rethrows the same wrapper.
        RuntimeWrappedException first = null;
        try
        {
            try
            {
                Thrower.Throw();
                return 8;
            }
            catch (RuntimeWrappedException e)
            {
                first = e;
                throw;
            }
        }
        catch (RuntimeWrappedException e)
        {
            if (first == null || !ReferenceEquals(e, first)) return 9;
        }

        // Each throw makes a wrapper of its own.
        RuntimeWrappedException second = null;
        try
        {
            Thrower.Throw();
            return 14;
        }
        catch (RuntimeWrappedException e)
        {
            second = e;
        }
        if (ReferenceEquals(first, second)) return 15;

        // The wrapper carries the trace of the throw.
        try
        {
            Thrower.Throw();
            return 10;
        }
        catch (RuntimeWrappedException e)
        {
            string trace = e.StackTrace;
            if (trace == null || !ContainsSubstring(trace, "Thrower.Throw")) return 11;
        }

        // Once the runtime has wrapped something, the handler of a clause that does not wrap is
        // shown what a wrapper the guest made wraps, too.
        object seenAfterAWrap = Thrower.CatchString(() => { throw made; });
        if (!ReferenceEquals(seenAfterAWrap, "made")) return 23;

        // A typed clause matches a class or one of its base classes, never an interface it
        // implements, whether the object is unwrapped ...
        try
        {
            Thrower.CatchComparable(() => { throw made; });
            return 30;
        }
        catch (RuntimeWrappedException)
        {
        }

        // ... or an exception.
        try
        {
            Thrower.CatchSerializable(() => { throw new InvalidOperationException(); });
            return 31;
        }
        catch (InvalidOperationException)
        {
        }

        // An array's base class is System.Array, so a thrown string[] is no object[] to a clause.
        try
        {
            Thrower.CatchObjects();
            return 32;
        }
        catch (RuntimeWrappedException)
        {
        }

        // Clauses are offered the exception in order, and the first to accept it takes it, so a
        // later typed clause never examines a wrapper around null.
        if (Thrower.FilterThenCatchString(() => { throw new RuntimeWrappedException(null); }) != 1) return 33;

        return 0;
    }
}
"""

    [<Test>]
    let ``a thrown non-exception is wrapped for clauses in wrapping assemblies alone, as the real runtime does``
        ()
        : unit
        =
        FabricatedGuest.run "Thrower" (fabricate ()) "NonExceptionThrowDriver" driverSource 0

    /// `TargetSite` and `Source` of the `RuntimeWrappedException` a `throw` of a non-exception
    /// raises. `IL_Throw` dispatches the wrapper from the `throw`, so the first frame of its trace is
    /// the method that threw, as for a `throw` of an exception. Returns the row's code if the runtime
    /// disagrees with CoreCLR there.
    let private targetSiteDriverSource : string =
        """
using System;
using System.Reflection;
using System.Runtime.CompilerServices;

public static class Driver
{
    public static int Main(string[] args)
    {
        try
        {
            Thrower.Throw();
            return 1;
        }
        catch (RuntimeWrappedException e)
        {
            MethodBase site = e.TargetSite;
            if (site == null) return 2;
            if (site.Name != "Throw") return 3;
            if (site.DeclaringType != typeof(Thrower)) return 4;
            if (e.Source != "Thrower") return 5;
        }

        // A handler that does not wrap sees the string, and its `rethrow` raises the wrapper
        // again, keeping its trace.
        try
        {
            Thrower.CatchObjectAndRethrow();
            return 6;
        }
        catch (RuntimeWrappedException e)
        {
            if (e.TargetSite.Name != "Throw") return 7;
            if (e.TargetSite.DeclaringType != typeof(Thrower)) return 8;
        }

        return 0;
    }
}
"""

    [<Test>]
    let ``TargetSite of a wrapped non-exception is the method that threw it, as the real runtime does`` () : unit =
        FabricatedGuest.run "Thrower" (fabricate ()) "NonExceptionTargetSiteDriver" targetSiteDriverSource 0

    /// Why the cases in which preserving a trace reads the culture are parked on .NET 11.
    let private traceRenderedWhenThrown : string =
        "PawPrint renders a thrown exception's trace when it is thrown, so preserving it never reads the culture and never raises: PawPrint catches the culture's first exception, where real .NET 11 raises and preserves the exceptions that reading the culture raises in turn (docs/divergences.md, \"A thrown exception's trace is rendered when it is thrown, not when it is read\"). Un-park when `Exception.StackTrace` renders through CoreLib's `StackTrace` on demand."

    /// Throws a string while the current UI culture's name, which the wrapper's constructor reads
    /// to look up its message, throws a `TimeZoneNotFoundException`. Returns 0 if that exception
    /// reached the caller's filter with the lock CoreLib's resource lookup takes already
    /// released, and the row's code otherwise.
    ///
    /// The lookup (`SR.InternalGetResourceString`) holds `SR._lock` in a `try` whose `catch`
    /// rethrows and whose `finally` releases it. The rethrow's first pass runs every filter it
    /// reaches before that `finally`, so a filter outside the `throw` finds the lock released only
    /// if the raise ended at the wrapper's constructor, where the runtime called it, and began
    /// again at the `throw`.
    let private throwingConstructorDriverSource : string =
        """
using System;
using System.Globalization;
using System.Reflection;
using System.Threading;

sealed class ThrowingCulture : CultureInfo
{
    public ThrowingCulture() : base("") { }
    public bool Armed;
    public override string Name => Armed ? throw new TimeZoneNotFoundException("a culture's name") : base.Name;
}

public static class Driver
{
    static object srLock;
    static bool lockHeldAtFilter;

    static bool Observe()
    {
        lockHeldAtFilter = Monitor.IsEntered(srLock);
        return true;
    }

    public static int Main(string[] args)
    {
        srLock = typeof(object).Assembly.GetType("System.SR", true).GetField("_lock", BindingFlags.NonPublic | BindingFlags.Static).GetValue(null);
        if (srLock == null) return 4;

        var culture = new ThrowingCulture();
        CultureInfo.CurrentUICulture = culture;
        culture.Armed = true;

        Exception caught = null;
        try
        {
            Thrower.Throw();
            return 1;
        }
        catch (Exception e) when (Observe())
        {
            caught = e;
        }

        culture.Armed = false;
        CultureInfo.CurrentUICulture = CultureInfo.InvariantCulture;

        if (!(caught is TimeZoneNotFoundException)) return 2;
        if (lockHeldAtFilter) return 3;
        return 0;
    }
}
"""

    [<Test>]
    let ``what wrapping a thrown non-exception raises is thrown in the wrapper's place, as the real runtime does``
        ()
        : unit
        =
        match FrameworkUnderTest.runtime () with
        | EmulatedRuntime.Net10 ->
            FabricatedGuest.run
                "Thrower"
                (fabricate ())
                "ThrowingWrapperConstructorDriver"
                throwingConstructorDriverSource
                0
        | EmulatedRuntime.Net11 ->
            // Real .NET 11 does not survive this culture, which throws on every read: preserving
            // the trace of each exception it raises raises another, until the stack overflows
            // (`ExceptionMaking.vmMakingRaise`).
            let onHost, onPawPrint =
                FabricatedGuest.runOnBoth
                    "Thrower"
                    (fabricate ())
                    "ThrowingWrapperConstructorDriver"
                    throwingConstructorDriverSource

            // The overflow aborts the process (SIGABRT), which the oracle reports as exit 134.
            onHost |> shouldEqual (RealRuntimeResult.NormalExit 134)

            ParkedOn.check [ EmulatedRuntime.Net11 ] traceRenderedWhenThrown

            // Once un-parked: PawPrint follows a bounded number of the re-raises, and then refuses.
            match onPawPrint with
            | FabricatedOutcome.Failed e ->
                e.ToString ()
                |> shouldContainText "Exception.InternalPreserveStackTrace raised more than 16 times running"
            | FabricatedOutcome.Exited code ->
                failwith $"PawPrint ran on past a raising wrapper constructor on .NET 11, exiting %d{code}"

    /// Throws a string while the current UI culture's name throws a `TimeZoneNotFoundException` on
    /// its first read alone, so that the wrapper's constructor raises and nothing after it does.
    /// Returns 0 if what the caller caught is that exception, with the trace and `Source` the
    /// runtime's handling gave it, and its filter and `finally` clauses ran in the runtime's order;
    /// the row's code otherwise.
    ///
    /// On .NET 11 the runtime catches the constructor's exception in
    /// `Exception.CreateRuntimeWrappedException` and `COMPlusThrow`s it, which first runs
    /// `Exception.InternalPreserveStackTrace` on it: that fixes `Source` at the faulting frame,
    /// moves the rendered trace (ending at the catching helper) into `_remoteStackTraceString` with
    /// a newline, and clears the trace, so the `throw`'s raise begins a fresh one after that prefix.
    let private oneShotCultureDriverSource : string =
        """
using System;
using System.Globalization;
using System.Reflection;

sealed class OneShotThrowingCulture : CultureInfo
{
    public OneShotThrowingCulture() : base("") { }
    public int Throws;
    public override string Name
    {
        get
        {
            if (Throws > 0)
            {
                Throws--;
                try
                {
                    throw new TimeZoneNotFoundException("a culture's name");
                }
                finally
                {
                    Driver.Order = Driver.Order + "name finally;";
                }
            }
            return base.Name;
        }
    }
}

public static class Driver
{
    public static string Order = "";

    static int IndexOf(string haystack, string needle)
    {
        for (int i = 0; i <= haystack.Length - needle.Length; i++)
        {
            bool matches = true;
            for (int j = 0; j < needle.Length; j++)
            {
                if (haystack[i + j] != needle[j])
                {
                    matches = false;
                    break;
                }
            }

            if (matches)
            {
                return i;
            }
        }

        return -1;
    }

    static bool IsPrefix(string prefix, string s)
    {
        if (prefix.Length > s.Length) return false;
        for (int i = 0; i < prefix.Length; i++)
        {
            if (prefix[i] != s[i]) return false;
        }
        return true;
    }

    static bool Observe()
    {
        Order = Order + "filter;";
        return true;
    }

    public static int Main(string[] args)
    {
        var culture = new OneShotThrowingCulture();
        CultureInfo.CurrentUICulture = culture;
        culture.Throws = 1;

        Exception caught = null;
        try
        {
            try
            {
                Thrower.Throw();
                return 1;
            }
            finally
            {
                Order = Order + "finally;";
            }
        }
        catch (Exception e) when (Observe())
        {
            Order = Order + "catch;";
            caught = e;
        }

        CultureInfo.CurrentUICulture = CultureInfo.InvariantCulture;

        if (culture.Throws != 0) return 5;
        if (!(caught is TimeZoneNotFoundException)) return 2;
        // The raise inside the constructor ends where the runtime called it, so its cleanup runs
        // before the first pass of the raise from the `throw` reaches any filter.
        if (Order != "name finally;filter;finally;catch;") return 6;

        string source = (string)typeof(Exception).GetField("_source", BindingFlags.NonPublic | BindingFlags.Instance).GetValue(caught);
        string remote = (string)typeof(Exception).GetField("_remoteStackTraceString", BindingFlags.NonPublic | BindingFlags.Instance).GetValue(caught);
        string trace = caught.StackTrace;
        if (Environment.Version.Major == 10)
        {
            // .NET 10 lets the constructor's exception escape the `throw` with its trace unbroken.
            if (remote != null) return 20;
            if (source != null) return 26;
            int name10 = IndexOf(trace, "OneShotThrowingCulture.get_Name");
            int ctor10 = IndexOf(trace, "RuntimeWrappedException..ctor");
            int throw10 = IndexOf(trace, "Thrower.Throw");
            if (name10 < 0) return 21;
            if (ctor10 < name10) return 22;
            if (throw10 < ctor10) return 23;
            if (IndexOf(trace, "Driver.Main") < throw10) return 24;
            if (IndexOf(trace, "CreateRuntimeWrappedException") >= 0) return 25;
            return 0;
        }

        // `Source` was read while the trace still began at the culture, whose assembly this is.
        if (source != "OneShotCultureWrapperConstructorDriver") return 7;

        if (remote == null) return 8;
        if (remote.Length == 0 || remote[remote.Length - 1] != '\n') return 9;

        // The preserved trace runs from the culture's getter to the helper that caught it, and
        // stops there.
        int name = IndexOf(remote, "OneShotThrowingCulture.get_Name");
        int ctor = IndexOf(remote, "RuntimeWrappedException..ctor");
        int helper = IndexOf(remote, "Exception.CreateRuntimeWrappedException");
        if (name < 0) return 10;
        if (ctor < name) return 11;
        if (helper < ctor) return 12;
        if (IndexOf(remote, "Thrower.Throw") >= 0) return 13;
        if (IndexOf(remote, "Driver.Main") >= 0) return 14;

        // The trace a reader sees is that prefix and then the `throw`'s raise, begun afresh.
        if (trace == null || !IsPrefix(remote, trace)) return 15;
        string fresh = trace.Substring(remote.Length);
        if (!IsPrefix("   at Thrower.Throw()", fresh)) return 16;
        if (IndexOf(fresh, "Driver.Main") < 0) return 17;
        if (IndexOf(fresh, "get_Name") >= 0) return 18;
        if (IndexOf(fresh, "CreateRuntimeWrappedException") >= 0) return 19;

        return 0;
    }
}
"""

    [<Test>]
    let ``what wrapping a thrown non-exception raises once is thrown from the throw with its trace preserved, as the real runtime does``
        ()
        : unit
        =
        FabricatedGuest.run
            "Thrower"
            (fabricate ())
            "OneShotCultureWrapperConstructorDriver"
            oneShotCultureDriverSource
            0

    /// Throws a string while the current UI culture's name throws a `CountingException` of index 0
    /// on its first read alone, so that the wrapper's constructor raises it, and while each
    /// `CountingException`'s `Source` throws one of the next index until `reraises` have been
    /// thrown. Returns 0 if what the caller caught, and the order its clauses ran in, are what the
    /// runtime's handling gives; the row's code otherwise.
    ///
    /// On .NET 11 the runtime preserves the trace of what the constructor raised
    /// (`Exception.InternalPreserveStackTrace`, which reads `Source`), and preserves in turn each
    /// exception that preserving raises, so that exactly `reraises` preserves raise and the one
    /// caught is the last, of index `reraises`. On .NET 10 the constructor's exception is the one
    /// caught, and nothing reads `Source`.
    let private countingSourceDriverSource (reraises : int) : string =
        """
using System;
using System.Globalization;
using System.Reflection;

sealed class CountingException : Exception
{
    public static int Budget;
    public readonly int Index;
    public CountingException(int index) : base("counting") { Index = index; }
    public override string Source
    {
        get
        {
            if (Budget > 0)
            {
                Budget--;
                try
                {
                    throw new CountingException(Index + 1);
                }
                finally
                {
                    Driver.Order = Driver.Order + "source finally;";
                }
            }
            return base.Source;
        }
    }
}

sealed class OneShotCountingCulture : CultureInfo
{
    public OneShotCountingCulture() : base("") { }
    public bool Armed;
    public override string Name
    {
        get
        {
            if (Armed)
            {
                Armed = false;
                try
                {
                    throw new CountingException(0);
                }
                finally
                {
                    Driver.Order = Driver.Order + "name finally;";
                }
            }
            return base.Name;
        }
    }
}

public static class Driver
{
    public static string Order = "";

    static int IndexOf(string haystack, string needle)
    {
        for (int i = 0; i <= haystack.Length - needle.Length; i++)
        {
            bool matches = true;
            for (int j = 0; j < needle.Length; j++)
            {
                if (haystack[i + j] != needle[j])
                {
                    matches = false;
                    break;
                }
            }

            if (matches)
            {
                return i;
            }
        }

        return -1;
    }

    static bool IsPrefix(string prefix, string s)
    {
        if (prefix.Length > s.Length) return false;
        for (int i = 0; i < prefix.Length; i++)
        {
            if (prefix[i] != s[i]) return false;
        }
        return true;
    }

    static bool Observe()
    {
        Order = Order + "filter;";
        return true;
    }

    public static int Main(string[] args)
    {
        int reraises = RERAISES;
        CountingException.Budget = reraises;
        var culture = new OneShotCountingCulture();
        CultureInfo.CurrentUICulture = culture;
        culture.Armed = true;

        Exception caught = null;
        try
        {
            try
            {
                Thrower.Throw();
                return 1;
            }
            finally
            {
                Order = Order + "finally;";
            }
        }
        catch (Exception e) when (Observe())
        {
            Order = Order + "catch;";
            caught = e;
        }

        CultureInfo.CurrentUICulture = CultureInfo.InvariantCulture;

        if (culture.Armed) return 2;
        CountingException counted = caught as CountingException;
        if (counted == null) return 3;

        string remote = (string)typeof(Exception).GetField("_remoteStackTraceString", BindingFlags.NonPublic | BindingFlags.Instance).GetValue(caught);
        string trace = caught.StackTrace;

        if (Environment.Version.Major == 10)
        {
            // .NET 10 lets the constructor's exception escape the `throw`, and preserves nothing.
            if (counted.Index != 0) return 20;
            if (CountingException.Budget != reraises) return 21;
            if (Order != "name finally;filter;finally;catch;") return 22;
            if (remote != null) return 23;
            if (caught.TargetSite == null || caught.TargetSite.Name != "get_Name") return 24;
            return 0;
        }

        if (counted.Index != reraises) return 4;
        if (CountingException.Budget != 0) return 5;

        string expectedOrder = "name finally;";
        for (int i = 0; i < reraises; i++)
        {
            expectedOrder = expectedOrder + "source finally;";
        }
        expectedOrder = expectedOrder + "filter;finally;catch;";
        if (Order != expectedOrder) return 6;

        // Preserving read `Source`, which read `TargetSite` while the trace still began where the
        // exception was thrown, and both keep what they read.
        string thrownIn = reraises == 0 ? "get_Name" : "get_Source";
        if (caught.TargetSite == null || caught.TargetSite.Name != thrownIn) return 7;
        if (caught.Source != "CountingSourceWrapperConstructorDriver") return 8;

        if (remote == null) return 9;
        if (remote.Length == 0 || remote[remote.Length - 1] != '\n') return 10;

        // The preserved trace runs from where the caught exception was thrown to the helper that
        // caught it, and stops there.
        if (reraises == 0)
        {
            int name = IndexOf(remote, "OneShotCountingCulture.get_Name");
            int ctor = IndexOf(remote, "RuntimeWrappedException..ctor");
            int helper = IndexOf(remote, "Exception.CreateRuntimeWrappedException");
            if (name < 0) return 11;
            if (ctor < name) return 12;
            if (helper < ctor) return 13;
        }
        else
        {
            int source = IndexOf(remote, "CountingException.get_Source");
            int preserve = IndexOf(remote, "Exception.InternalPreserveStackTrace");
            if (source < 0) return 14;
            if (preserve < source) return 15;
            if (IndexOf(remote, "RuntimeWrappedException..ctor") >= 0) return 16;
            if (IndexOf(remote, "CreateRuntimeWrappedException") >= 0) return 17;
        }
        if (IndexOf(remote, "Thrower.Throw") >= 0) return 18;
        if (IndexOf(remote, "Driver.Main") >= 0) return 19;

        // The trace a reader sees is that prefix and then the `throw`'s raise, begun afresh.
        if (trace == null || !IsPrefix(remote, trace)) return 30;
        string fresh = trace.Substring(remote.Length);
        if (!IsPrefix("   at Thrower.Throw()", fresh)) return 31;
        if (IndexOf(fresh, "Driver.Main") < 0) return 32;
        if (IndexOf(fresh, "InternalPreserveStackTrace") >= 0) return 33;
        if (IndexOf(fresh, "CreateRuntimeWrappedException") >= 0) return 34;

        return 0;
    }
}
"""
            .Replace ("RERAISES", string reraises)

    [<TestCase 0>]
    [<TestCase 1>]
    [<TestCase 2>]
    [<TestCase 16>]
    let ``what preserving the trace of what wrapping raised raises is preserved in turn, as the real runtime does``
        (reraises : int)
        : unit
        =
        FabricatedGuest.run
            "Thrower"
            (fabricate ())
            "CountingSourceWrapperConstructorDriver"
            (countingSourceDriverSource reraises)
            0

    /// One more re-raise than PawPrint follows: real .NET 11 follows it, and PawPrint refuses.
    [<Test>]
    let ``PawPrint refuses a seventeenth raise from preserving what wrapping raised`` () : unit =
        let onHost, onPawPrint =
            FabricatedGuest.runOnBoth
                "Thrower"
                (fabricate ())
                "CountingSourceWrapperConstructorDriver"
                (countingSourceDriverSource 17)

        onHost |> shouldEqual (RealRuntimeResult.NormalExit 0)

        match FrameworkUnderTest.runtime (), onPawPrint with
        | EmulatedRuntime.Net10, FabricatedOutcome.Exited code -> code |> shouldEqual 0
        | EmulatedRuntime.Net10, FabricatedOutcome.Failed e ->
            raise (Exception ("PawPrint did not run the guest to completion", e))
        | EmulatedRuntime.Net11, FabricatedOutcome.Failed e ->
            e.ToString ()
            |> shouldContainText "Exception.InternalPreserveStackTrace raised more than 16 times running"
        | EmulatedRuntime.Net11, FabricatedOutcome.Exited code ->
            failwith
                $"PawPrint followed more re-raises from preserving a trace than it bounds itself to, exiting %d{code}"

    /// Throws a string while the current UI culture's name throws a `TimeZoneNotFoundException` on
    /// each of its first two reads, the first of which is the wrapper's constructor's. Returns 0 if
    /// the exception the caller caught is the one the runtime settles on, and the row's code
    /// otherwise.
    ///
    /// On .NET 11 preserving the first exception's trace renders it, which reads the culture
    /// (`SR.Word_At`) and raises the second, which is preserved in turn and caught. On .NET 10
    /// nothing reads the culture after the constructor, and the first is caught.
    let private twiceThrowingCultureDriverSource : string =
        """
using System;
using System.Globalization;
using System.Reflection;

sealed class TwiceThrowingCulture : CultureInfo
{
    public TwiceThrowingCulture() : base("") { }
    public int Reads;
    public bool Armed;
    public override string Name
    {
        get
        {
            if (Armed)
            {
                Reads++;
                if (Reads <= 2) throw new TimeZoneNotFoundException("read " + Reads);
            }
            return base.Name;
        }
    }
}

public static class Driver
{
    static bool Contains(string haystack, string needle)
    {
        for (int i = 0; i <= haystack.Length - needle.Length; i++)
        {
            bool matches = true;
            for (int j = 0; j < needle.Length; j++)
            {
                if (haystack[i + j] != needle[j])
                {
                    matches = false;
                    break;
                }
            }

            if (matches) return true;
        }

        return false;
    }

    public static int Main(string[] args)
    {
        var culture = new TwiceThrowingCulture();
        CultureInfo.CurrentUICulture = culture;
        culture.Armed = true;

        Exception caught = null;
        try
        {
            Thrower.Throw();
            return 1;
        }
        catch (Exception e)
        {
            caught = e;
        }

        culture.Armed = false;
        CultureInfo.CurrentUICulture = CultureInfo.InvariantCulture;

        if (!(caught is TimeZoneNotFoundException)) return 2;
        string remote = (string)typeof(Exception).GetField("_remoteStackTraceString", BindingFlags.NonPublic | BindingFlags.Instance).GetValue(caught);

        if (Environment.Version.Major == 10)
        {
            if (caught.Message != "read 1") return 10;
            if (remote != null) return 11;
            return 0;
        }

        if (caught.Message != "read 2") return 3;
        if (remote == null) return 4;
        // The second was raised rendering the first's trace, inside the helper that preserves it.
        if (!Contains(remote, "TwiceThrowingCulture.get_Name")) return 5;
        if (!Contains(remote, "Exception.InternalPreserveStackTrace")) return 6;
        if (Contains(remote, "RuntimeWrappedException..ctor")) return 7;
        return 0;
    }
}
"""

    [<Test>]
    let ``what wrapping a thrown non-exception raises twice is the second, as the real runtime does`` () : unit =
        let onHost, onPawPrint =
            FabricatedGuest.runOnBoth
                "Thrower"
                (fabricate ())
                "TwiceThrowingCultureWrapperConstructorDriver"
                twiceThrowingCultureDriverSource

        onHost |> shouldEqual (RealRuntimeResult.NormalExit 0)

        ParkedOn.check [ EmulatedRuntime.Net11 ] traceRenderedWhenThrown

        match onPawPrint with
        | FabricatedOutcome.Exited code -> code |> shouldEqual 0
        | FabricatedOutcome.Failed e -> raise (Exception ("PawPrint did not run the guest to completion", e))

    /// Throws a string while the current UI culture's name throws a `SourceThrowingException` on
    /// its first read alone, so that the wrapper's constructor raises it, and while that
    /// exception's `Source` throws a string, from `Thrower.Throw`, on its first read alone. Returns
    /// 0 if what the caller caught is what the runtime's handling gives, with the trace it gave it,
    /// and the row's code otherwise.
    ///
    /// On .NET 11 preserving the constructor's exception reads `Source`, so the second string is
    /// wrapped inside `Exception.InternalPreserveStackTrace`, while the runtime's wrapping of the
    /// first is under way; that wrapper is what preserving raised, and is preserved and caught. On
    /// .NET 10 nothing reads `Source`, and the constructor's exception is caught.
    ///
    /// (Wrapping cannot nest inside the constructor itself: its resource lookup would look up its
    /// own message again, and real .NET fails fast on that recursion.)
    let private nestedWrapDriverSource : string =
        """
using System;
using System.Globalization;
using System.Reflection;
using System.Runtime.CompilerServices;

sealed class SourceThrowingException : Exception
{
    public static bool Fired;
    public SourceThrowingException() : base("source throws") { }
    public override string Source
    {
        get
        {
            if (!Fired)
            {
                Fired = true;
                Thrower.Throw();
            }
            return base.Source;
        }
    }
}

sealed class OneShotCulture : CultureInfo
{
    public OneShotCulture() : base("") { }
    public bool Armed;
    public override string Name
    {
        get
        {
            if (Armed)
            {
                Armed = false;
                throw new SourceThrowingException();
            }
            return base.Name;
        }
    }
}

public static class Driver
{
    static bool Contains(string haystack, string needle)
    {
        for (int i = 0; i <= haystack.Length - needle.Length; i++)
        {
            bool matches = true;
            for (int j = 0; j < needle.Length; j++)
            {
                if (haystack[i + j] != needle[j])
                {
                    matches = false;
                    break;
                }
            }

            if (matches) return true;
        }

        return false;
    }

    public static int Main(string[] args)
    {
        var culture = new OneShotCulture();
        CultureInfo.CurrentUICulture = culture;
        culture.Armed = true;

        Exception caught = null;
        try
        {
            Thrower.Throw();
            return 1;
        }
        catch (Exception e)
        {
            caught = e;
        }

        CultureInfo.CurrentUICulture = CultureInfo.InvariantCulture;

        if (culture.Armed) return 2;
        string remote = (string)typeof(Exception).GetField("_remoteStackTraceString", BindingFlags.NonPublic | BindingFlags.Instance).GetValue(caught);

        if (Environment.Version.Major == 10)
        {
            if (!(caught is SourceThrowingException)) return 10;
            if (SourceThrowingException.Fired) return 11;
            if (remote != null) return 12;
            return 0;
        }

        RuntimeWrappedException wrapper = caught as RuntimeWrappedException;
        if (wrapper == null) return 3;
        if (!ReferenceEquals(wrapper.WrappedException, "not an exception")) return 4;
        if (!SourceThrowingException.Fired) return 5;

        // The inner wrapper was raised from the `throw` in `Thrower.Throw`, which `Source` called
        // inside the helper that preserves, and caught there.
        if (remote == null) return 6;
        if (!Contains(remote, "Thrower.Throw")) return 7;
        if (!Contains(remote, "SourceThrowingException.get_Source")) return 8;
        if (!Contains(remote, "Exception.InternalPreserveStackTrace")) return 9;
        if (Contains(remote, "Driver.Main")) return 13;

        // The outer `throw`'s raise began afresh after that prefix.
        string trace = caught.StackTrace;
        if (trace == null || trace.Length <= remote.Length) return 14;
        string fresh = trace.Substring(remote.Length);
        if (!Contains(fresh, "Thrower.Throw")) return 15;
        if (!Contains(fresh, "Driver.Main")) return 16;
        if (Contains(fresh, "get_Source")) return 17;
        return 0;
    }
}
"""

    [<Test>]
    let ``a non-exception thrown while preserving what wrapping raised is wrapped in turn, as the real runtime does``
        ()
        : unit
        =
        FabricatedGuest.run "Thrower" (fabricate ()) "NestedWrapDriver" nestedWrapDriverSource 0

    /// Throws a string `WRAPS` times, each caught. When `PRESERVES` is true, each throw's wrapping
    /// raises too, from the culture's name, and preserving what it raised raises once more, from
    /// that exception's `Source`, so that on .NET 11 each throw runs both of the runtime's helpers
    /// and preserves twice. Returns 0 once every throw has been caught.
    let private repeatedWrapDriverSource (wraps : int) (preserves : bool) : string =
        """
using System;
using System.Globalization;
using System.Runtime.CompilerServices;

sealed class SourceOnceException : Exception
{
    public bool Fired;
    public SourceOnceException() : base("source once") { }
    public override string Source
    {
        get
        {
            if (!Fired)
            {
                Fired = true;
                throw new SourceOnceException { Fired = true };
            }
            return base.Source;
        }
    }
}

sealed class OneShotCulture : CultureInfo
{
    public OneShotCulture() : base("") { }
    public bool Armed;
    public override string Name
    {
        get
        {
            if (Armed)
            {
                Armed = false;
                throw new SourceOnceException();
            }
            return base.Name;
        }
    }
}

public static class Driver
{
    public static int Main(string[] args)
    {
        var culture = new OneShotCulture();
        CultureInfo.CurrentUICulture = culture;
        for (int i = 0; i < WRAPS; i++)
        {
            culture.Armed = PRESERVES;
            try
            {
                Thrower.Throw();
                return 1;
            }
            catch (Exception)
            {
            }
        }
        CultureInfo.CurrentUICulture = CultureInfo.InvariantCulture;
        return 0;
    }
}
"""
            .Replace("WRAPS", string wraps)
            .Replace ("PRESERVES", (if preserves then "true" else "false"))

    /// How many native blocks are still allocated once the guest has wrapped `wraps` throws.
    let private liveNativeBlocksAfter (wraps : int) (preserves : bool) : int =
        let state =
            FabricatedGuest.finalStateOnPawPrint
                "Thrower"
                (fabricate ())
                "RepeatedWrapDriver"
                (repeatedWrapDriverSource wraps preserves)

        state.LatchedExitCode |> shouldEqual 0
        IlMachineState.getNativeMemoryPool state |> NativeMemoryPool.liveBlockCount

    /// The native cells the runtime's wrapping passes its CoreLib helpers die with the wrap, as the
    /// VM's locals die when the native code holding them returns. Measured against one wrap rather
    /// than none, so that whatever the first wrap allocates once is counted on both sides.
    [<TestCase false>]
    [<TestCase true>]
    let ``wrapping a thrown non-exception leaves no native memory allocated`` (preserves : bool) : unit =
        liveNativeBlocksAfter 3 preserves
        |> shouldEqual (liveNativeBlocksAfter 1 preserves)
