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
            // Real .NET 11 does not survive this culture, which throws on every read
            // (`Net11Frontier.nonExceptionWrapping`), and PawPrint refuses any exception the
            // wrapper's constructor raises on .NET 11.
            let onHost, onPawPrint =
                FabricatedGuest.runOnBoth
                    "Thrower"
                    (fabricate ())
                    "ThrowingWrapperConstructorDriver"
                    throwingConstructorDriverSource

            match onHost with
            | RealRuntimeResult.NormalExit 0 -> failwith "real .NET 11 survived a raising wrapper constructor"
            | _ -> ()

            match onPawPrint with
            | FabricatedOutcome.Failed e -> e.ToString () |> shouldContainText "RuntimeWrappedException"
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
