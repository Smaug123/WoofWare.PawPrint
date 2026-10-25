namespace WoofWare.PawPrint.Test

open System
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PawPrint.Analysis

/// `ExceptionMaking` says how CoreCLR makes the exception of each fault it raises by itself. These
/// tests hold it to the real runtime, which a guest drives one fault at a time, in a process of its
/// own because a fault the CPU reports can end the process.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestExceptionMaking =

    /// Triggers the fault its first argument names, under a current UI culture whose name, as its
    /// second argument says, can always be read ("named"), can never be read ("nameless"), or can be
    /// read only as often as making the exception its third argument names by its parameterless
    /// constructor reads it ("after-constructor"). Looking up a CoreLib exception's message reads
    /// that name, so where the runtime looks one up, the culture's exception is raised in the fault's
    /// place. Exits 1 if the exception its third argument names escaped the fault, 2 if the culture's
    /// did, 3 if another did, and 0 if none did. Given "twice" instead, triggers the fault twice and
    /// exits 1 if the same exception escaped both times, 4 if two distinct ones did, 3 if another
    /// did, and 0 if none did. The "fault" `Wrap` throws a string, which the runtime wraps.
    let private guest : string =
        """
using System;
using System.Globalization;
using System.Reflection.Emit;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Runtime.Intrinsics;
using System.Threading;

sealed class CountingCulture : CultureInfo
{
    public CountingCulture() : base("") { }
    public int Reads;
    public int ThrowFrom = int.MaxValue;
    public override string Name => ++Reads >= ThrowFrom ? throw new TimeZoneNotFoundException("a culture's name") : base.Name;
}

sealed class Cell { public int Value; }

static class Program
{
    [MethodImpl(MethodImplOptions.NoInlining)]
    static int Field(Cell cell) => cell.Value;

    [MethodImpl(MethodImplOptions.NoInlining)]
    static int Element(int[] array, int index) => array[index];

    [MethodImpl(MethodImplOptions.NoInlining)]
    static void Store(object[] array) => array[0] = new object();

    [MethodImpl(MethodImplOptions.NoInlining)]
    static string Cast(object value) => (string)value;

    [MethodImpl(MethodImplOptions.NoInlining)]
    static int Sum(int x, int y) => checked(x + y);

    [MethodImpl(MethodImplOptions.NoInlining)]
    static int Quotient(int x, int y) => x / y;

    [MethodImpl(MethodImplOptions.NoInlining)]
    static long Exchange() => Interlocked.Exchange(ref Unsafe.NullRef<long>(), 1L);

    [MethodImpl(MethodImplOptions.NoInlining)]
    static unsafe Vector128<byte> Load()
    {
        if (RuntimeInformation.ProcessArchitecture == Architecture.Arm64)
            return System.Runtime.Intrinsics.Arm.AdvSimd.LoadVector128((byte*)0);
        return System.Runtime.Intrinsics.X86.Sse2.LoadVector128((byte*)0);
    }

    // `String.Ctor(char, int)` passes the count to `FastAllocateString`, which refuses it.
    [MethodImpl(MethodImplOptions.NoInlining)]
    static string Repeat(int count) => new string('a', count);

    [MethodImpl(MethodImplOptions.NoInlining)]
    static long[] Length(int length) => new long[length];

    [MethodImpl(MethodImplOptions.NoInlining)]
    static long[,] Extent(int length) => new long[length, length];

    [MethodImpl(MethodImplOptions.NoInlining)]
    static int[,] Negative(int length) => new int[1, length];

    // C# spells no constructor of a multidimensional array taking lower bounds, so this emits a call
    // to `int[,]`'s.
    static Func<int, int, int, int, object> bounds;

    // C# throws only exceptions, so this emits a throw of its argument.
    static Action<object> wrap;

    static void Variadic(__arglist) { }

    // CoreCLR on Unix refuses to compile a method calling one with a variable argument list.
    [MethodImpl(MethodImplOptions.NoInlining)]
    static void CallsVariadic() => Variadic(__arglist(1));

    [UnmanagedCallersOnly]
    static void UcoTarget(int x) { }

    // CoreCLR refuses to compile an `[UnmanagedCallersOnly]` method whose signature needs marshalling.
    [UnmanagedCallersOnly]
    static void InvalidUco(bool b) { }

    // C# makes no delegate over an `[UnmanagedCallersOnly]` method, so this emits one, which the
    // JIT refuses to compile.
    static Action ucoDelegate;

    static Action UcoDelegate()
    {
        var method = new DynamicMethod("UcoDelegate", typeof(void), Type.EmptyTypes, typeof(Program).Module);
        var il = method.GetILGenerator();
        il.Emit(OpCodes.Ldnull);
        il.Emit(OpCodes.Ldftn, typeof(Program).GetMethod("UcoTarget", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Static));
        il.Emit(OpCodes.Newobj, typeof(Action<int>).GetConstructor(new[] { typeof(object), typeof(IntPtr) }));
        il.Emit(OpCodes.Pop);
        il.Emit(OpCodes.Ret);
        return method.CreateDelegate<Action>();
    }

    static Action<object> Wrap()
    {
        var method = new DynamicMethod("Wrap", typeof(void), new[] { typeof(object) });
        var il = method.GetILGenerator();
        il.Emit(OpCodes.Ldarg_0);
        il.Emit(OpCodes.Throw);
        return method.CreateDelegate<Action<object>>();
    }

    static Func<int, int, int, int, object> Bounds()
    {
        var parameters = new[] { typeof(int), typeof(int), typeof(int), typeof(int) };
        var method = new DynamicMethod("Bounds", typeof(object), parameters);
        var il = method.GetILGenerator();
        il.Emit(OpCodes.Ldarg_0);
        il.Emit(OpCodes.Ldarg_1);
        il.Emit(OpCodes.Ldarg_2);
        il.Emit(OpCodes.Ldarg_3);
        il.Emit(OpCodes.Newobj, typeof(int[,]).GetConstructor(parameters));
        il.Emit(OpCodes.Ret);
        return method.CreateDelegate<Func<int, int, int, int, object>>();
    }

    static void Run(string fault)
    {
        switch (fault)
        {
            case "Field": Field(null); break;
            case "Element": Element(new int[1], 1); break;
            case "Store": Store(new string[1]); break;
            case "Cast": Cast(1); break;
            case "Sum": Sum(int.MaxValue, 1); break;
            case "Quotient": Quotient(1, 0); break;
            case "Exchange": Exchange(); break;
            case "Load": Load(); break;
            case "Repeat": Repeat(int.MaxValue); break;
            case "Length": Length(0x7FFFFFF0); break;
            case "Extent": Extent(0x10000); break;
            case "Negative": Negative(-1); break;
            case "Bounds": bounds(int.MaxValue, 2, 0, 1); break;
            case "Wrap": wrap("a string"); break;
            case "Vararg": CallsVariadic(); break;
            case "UcoDelegate": ucoDelegate(); break;
            case "UcoInvalid": RuntimeHelpers.PrepareMethod(typeof(Program).GetMethod("InvalidUco", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Static).MethodHandle); break;
            default: throw new ArgumentException(fault);
        }
    }

    static Exception Raise(string fault)
    {
        try
        {
            Run(fault);
            return null;
        }
        catch (Exception e)
        {
            return e;
        }
    }

    static int Main(string[] args)
    {
        string fault = args[0], mode = args[1], expected = args[2];
        bounds = Bounds();
        wrap = Wrap();
        ucoDelegate = UcoDelegate();

        if (mode == "twice")
        {
            Exception first = Raise(fault), second = Raise(fault);
            if (first == null || second == null) return 0;
            if (first.GetType().FullName != expected || second.GetType().FullName != expected) return 3;
            return ReferenceEquals(first, second) ? 1 : 4;
        }

        var culture = new CountingCulture();
        if (mode != "named")
        {
            CultureInfo.CurrentUICulture = culture;
            if (mode == "after-constructor")
            {
                // Each read once, so that what is left is what every making reads.
                Raise(fault);
                var exceptionType = typeof(object).Assembly.GetType(expected, true);
                for (int i = 0; i < 3; i++)
                {
                    culture.Reads = 0;
                    Activator.CreateInstance(exceptionType);
                }
                culture.ThrowFrom = culture.Reads + 1;
            }
            else
            {
                culture.ThrowFrom = 1;
            }
            culture.Reads = 0;
        }

        try
        {
            Run(fault);
            return 0;
        }
        catch (TimeZoneNotFoundException)
        {
            return 2;
        }
        catch (Exception e)
        {
            culture.ThrowFrom = int.MaxValue;
            return e.GetType().FullName == expected ? 1 : 3;
        }
    }
}
"""

    let private image : Lazy<byte[]> = lazy (Roslyn.compile [ guest ])

    /// How `ExceptionMaking` says the runtime makes `fault`'s exception at each instruction that
    /// raises it by itself in the method of this name in `image`.
    let private makingsIn (image : byte[]) (methodName : string) (fault : OpcodeFault) : ExceptionMaking list =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let assembly =
            Assembly.read loggerFactory (Some "Guest.dll") (new MemoryStream (image))

        let method =
            assembly.Methods.Values
            |> Seq.filter (fun m -> m.Name = methodName)
            |> Seq.exactlyOne

        match MethodInfo.tryIlBody method with
        | None -> failwith $"the guest's %s{methodName} has no IL body"
        | Some body ->
            body.Instructions
            |> List.choose (fun (op, _) ->
                match OpcodeFaults.ofIlOp op with
                | OpcodeFaults.Raises (faults, _) when List.contains fault faults ->
                    Some (ExceptionMaking.ofOpcodeFault op fault)
                | OpcodeFaults.Raises _
                | OpcodeFaults.Unmodelled -> None
            )

    /// How `ExceptionMaking` says the runtime makes `fault`'s exception where the guest's method of
    /// this name raises it: at the one instruction of the method that raises it by itself.
    let private atInstruction (methodName : string) (fault : OpcodeFault) : Lazy<ExceptionMaking> =
        lazy
            match makingsIn image.Value methodName fault with
            | [ making ] -> making
            | makings ->
                failwith $"expected one instruction of %s{methodName} to raise %A{fault}, found %d{makings.Length}"

    /// How `ExceptionMaking` says the runtime makes the exception of this full name, where a method
    /// it supplies on `arrayType` raises it.
    let private ofArrayMethod
        (arrayType : TypeDefn)
        (accessor : ArrayAccessor)
        (exceptionName : string)
        : Lazy<ExceptionMaking>
        =
        lazy
            match
                ExceptionMaking.ofArrayMethod arrayType accessor
                |> List.filter (fun (name, _) -> name = exceptionName)
            with
            | [ _, making ] -> making
            | found ->
                failwith $"expected %O{arrayType}'s %A{accessor} to raise %s{exceptionName} once, found %A{found}"

    /// A fault the guest triggers: how `ExceptionMaking` says the runtime makes its exception, the
    /// exception, and whether the CPU may report it on some target, so that a constructor that
    /// raises ends the process rather than raising in the fault's place.
    type private Case =
        {
            Making : Lazy<ExceptionMaking>
            Exception : string
            MayEndProcess : bool
        }

    let private cases : Map<string, Case> =
        Map.ofList
            [
                "Field",
                {
                    Making = atInstruction "Field" OpcodeFault.NullReference
                    Exception = "System.NullReferenceException"
                    MayEndProcess = true
                }
                "Element",
                {
                    Making = atInstruction "Element" OpcodeFault.IndexOutOfRange
                    Exception = "System.IndexOutOfRangeException"
                    MayEndProcess = false
                }
                "Store",
                {
                    Making = atInstruction "Store" OpcodeFault.ArrayTypeMismatch
                    Exception = "System.ArrayTypeMismatchException"
                    MayEndProcess = false
                }
                "Cast",
                {
                    Making = atInstruction "Cast" OpcodeFault.InvalidCast
                    Exception = "System.InvalidCastException"
                    MayEndProcess = false
                }
                "Sum",
                {
                    Making = atInstruction "Sum" OpcodeFault.Overflow
                    Exception = "System.OverflowException"
                    MayEndProcess = false
                }
                // x64 divides in hardware, which reports a zero divisor.
                "Quotient",
                {
                    Making = atInstruction "Quotient" OpcodeFault.DivideByZero
                    Exception = "System.DivideByZeroException"
                    MayEndProcess = true
                }
                "Exchange",
                {
                    Making = lazy ExceptionMaking.ofPrimitiveFault PrimitiveFault.NullReference
                    Exception = "System.NullReferenceException"
                    MayEndProcess = true
                }
                "Load",
                {
                    Making =
                        lazy
                            match ExceptionMaking.ofInstructionFault InstructionFault.NullAddress with
                            | Some making -> making
                            | None ->
                                failwith "ExceptionMaking says the runtime does not make a null address's exception"
                    Exception = "System.NullReferenceException"
                    MayEndProcess = true
                }
                "Length",
                {
                    Making = atInstruction "Length" OpcodeFault.OutOfMemory
                    Exception = "System.OutOfMemoryException"
                    MayEndProcess = false
                }
                "Extent",
                {
                    Making =
                        ofArrayMethod
                            (TypeDefn.Array (TypeDefn.PrimitiveType PrimitiveType.Int64, 2))
                            (ArrayAccessor.Constructor 2)
                            "System.OutOfMemoryException"
                    Exception = "System.OutOfMemoryException"
                    MayEndProcess = false
                }
                "Negative",
                {
                    Making =
                        ofArrayMethod
                            (TypeDefn.Array (TypeDefn.PrimitiveType PrimitiveType.Int32, 2))
                            (ArrayAccessor.Constructor 2)
                            "System.OverflowException"
                    Exception = "System.OverflowException"
                    MayEndProcess = false
                }
                // Compiling a method that calls one with a variable argument list.
                "Vararg",
                {
                    Making = lazy (snd (ExceptionMaking.ofSignatureRefusal SignatureRefusal.VariableArguments))
                    Exception = "System.InvalidProgramException"
                    MayEndProcess = false
                }
                "Bounds",
                {
                    Making =
                        ofArrayMethod
                            (TypeDefn.Array (TypeDefn.PrimitiveType PrimitiveType.Int32, 2))
                            (ArrayAccessor.Constructor 4)
                            "System.ArgumentOutOfRangeException"
                    Exception = "System.ArgumentOutOfRangeException"
                    MayEndProcess = false
                }
            ]

    let faults : string list = cases |> Map.keys |> List.ofSeq

    /// The contract table's row for `String.FastAllocateString`.
    let private fastAllocateString : NativeContractRow =
        NativeContractTable.rows.Force ()
        |> List.find (fun row -> row.Native = TabulatedNative.FCall ("System.String", "FastAllocateString"))

    [<Test>]
    let ``a native raises an exception its row names, made by running its parameterless constructor`` () : unit =
        Net11Frontier.park Net11Frontier.exceptionMaking

        // Which of the row's exceptions the native raises for a string too long to allocate depends
        // on the target's allocator.
        let raised =
            fastAllocateString.Contract.Raises
            |> List.filter (fun name ->
                RealRuntime.executeWithRealRuntime [| "Repeat" ; "named" ; name.FullName |] image.Value = RealRuntimeResult.NormalExit
                    1
            )

        match raised with
        | [ name ] ->
            let nameless =
                RealRuntime.executeWithRealRuntime [| "Repeat" ; "nameless" ; name.FullName |] image.Value

            match name.FullName with
            // Its constructor reads its message from the runtime's native resources, not the
            // culture's.
            | "System.OutOfMemoryException" -> nameless |> shouldEqual (RealRuntimeResult.NormalExit 1)
            | _ -> nameless |> shouldEqual (RealRuntimeResult.NormalExit 2)
        | raised ->
            failwith
                $"FastAllocateString of a string too long to allocate: expected exactly one of %A{fastAllocateString.Contract.Raises} to escape, got %A{raised}"

    [<TestCaseSource(nameof faults)>]
    let ``what the runtime runs to make a fault's exception is what ExceptionMaking says`` (fault : string) : unit =
        if List.contains fault [ "Bounds" ; "Cast" ; "Negative" ; "Vararg" ] then
            Net11Frontier.park Net11Frontier.exceptionMaking

        let case = cases.[fault]

        let run (mode : string) : RealRuntimeResult =
            RealRuntime.executeWithRealRuntime [| fault ; mode ; case.Exception |] image.Value

        let expect (mode : string) (what : string) (expected : RealRuntimeResult) : unit =
            match run mode with
            | actual when actual = expected -> ()
            // SIGABRT: the VM's handler for a fault the CPU reported met the culture's exception.
            | RealRuntimeResult.NormalExit 134 when case.MayEndProcess && mode <> "named" && mode <> "twice" -> ()
            | actual -> failwith $"%s{fault} (%s{mode}): expected %s{what}, %A{expected}; got %A{actual}"

        // The guest does raise this fault.
        expect "named" "its own exception" (RealRuntimeResult.NormalExit 1)

        match case.Making.Force () with
        | ExceptionMaking.ParameterlessConstructor ->
            expect "twice" "a new exception each time" (RealRuntimeResult.NormalExit 4)

            expect
                "after-constructor"
                "nothing but the constructor to read the culture"
                (RealRuntimeResult.NormalExit 1)

            match case.Exception with
            // Its constructor reads its message from the runtime's native resources, not the
            // culture's.
            | "System.OutOfMemoryException" ->
                expect "nameless" "a constructor that reads no culture" (RealRuntimeResult.NormalExit 1)
            | _ -> expect "nameless" "the culture's exception" (RealRuntimeResult.NormalExit 2)
        | ExceptionMaking.ResourceMessage ->
            expect "twice" "a new exception each time" (RealRuntimeResult.NormalExit 4)
            expect "after-constructor" "the message lookup to read the culture" (RealRuntimeResult.NormalExit 2)
            expect "nameless" "the culture's exception" (RealRuntimeResult.NormalExit 2)
        // Running no managed code, the runtime never reads the culture.
        | ExceptionMaking.Preallocated ->
            expect "twice" "the same exception each time" (RealRuntimeResult.NormalExit 1)
            expect "nameless" "the exception itself" (RealRuntimeResult.NormalExit 1)
        | ExceptionMaking.InitializerFailure -> failwith $"%s{fault}: no case here raises a TypeInitializationException"
        | ExceptionMaking.TypeLoadConstructor -> failwith $"%s{fault}: no case here raises a TypeLoadException"

    /// Where the JIT compiles the construction of a delegate over an `[UnmanagedCallersOnly]`
    /// method, .NET 10 refuses it with an exception it makes as `ExceptionMaking` says, and .NET 11
    /// makes the delegate (`UnmanagedCallersOnly.refusesDelegateTarget`). The runtime under test
    /// compiles the guest's dynamic method making one.
    [<Test>]
    let ``the JIT refuses a delegate over an UnmanagedCallersOnly method only where the runtime does, making the exception as ExceptionMaking says``
        ()
        : unit
        =
        let exceptionName, making = ExceptionMaking.unmanagedCallersOnlyDelegateTarget

        let run (mode : string) : RealRuntimeResult =
            RealRuntime.executeWithRealRuntime [| "UcoDelegate" ; mode ; exceptionName |] image.Value

        if UnmanagedCallersOnly.refusesDelegateTarget (FrameworkUnderTest.runtime ()) then
            making |> shouldEqual ExceptionMaking.ResourceMessage
            run "named" |> shouldEqual (RealRuntimeResult.NormalExit 1)
            run "twice" |> shouldEqual (RealRuntimeResult.NormalExit 4)
            run "after-constructor" |> shouldEqual (RealRuntimeResult.NormalExit 2)
            run "nameless" |> shouldEqual (RealRuntimeResult.NormalExit 2)
        else
            run "named" |> shouldEqual (RealRuntimeResult.NormalExit 0)

    /// CoreCLR refuses to compile an `[UnmanagedCallersOnly]` method whose signature needs
    /// marshalling with an exception it makes as `ExceptionMaking` says. The runtime under test
    /// compiles the guest's method taking a `bool`.
    [<Test>]
    let ``the JIT refuses an invalid UnmanagedCallersOnly method with an exception made as ExceptionMaking says``
        ()
        : unit
        =
        let exceptionName, making = ExceptionMaking.unmanagedCallersOnlyInvalid

        let run (mode : string) : RealRuntimeResult =
            RealRuntime.executeWithRealRuntime [| "UcoInvalid" ; mode ; exceptionName |] image.Value

        making |> shouldEqual ExceptionMaking.ResourceMessage
        run "named" |> shouldEqual (RealRuntimeResult.NormalExit 1)
        run "twice" |> shouldEqual (RealRuntimeResult.NormalExit 4)

        Net11Frontier.park Net11Frontier.exceptionMaking

        run "after-constructor" |> shouldEqual (RealRuntimeResult.NormalExit 2)
        run "nameless" |> shouldEqual (RealRuntimeResult.NormalExit 2)

    [<Test>]
    let ``a thrown object that is not an exception is wrapped by running RuntimeWrappedException's constructor``
        ()
        : unit
        =
        let run (mode : string) : RealRuntimeResult =
            RealRuntime.executeWithRealRuntime
                [| "Wrap" ; mode ; "System.Runtime.CompilerServices.RuntimeWrappedException" |]
                image.Value

        // The guest's `catch` is in a C# assembly, which sees the object wrapped.
        run "named" |> shouldEqual (RealRuntimeResult.NormalExit 1)
        run "twice" |> shouldEqual (RealRuntimeResult.NormalExit 4)

        Net11Frontier.park Net11Frontier.nonExceptionWrapping

        // The constructor looks up its message, and what that raises is thrown in the wrapper's place.
        run "nameless" |> shouldEqual (RealRuntimeResult.NormalExit 2)

    /// The provider a binding guest is compiled against: it declares a field and a method, an
    /// interface method one of two interfaces overrides, an interface method with a default body, and
    /// a class.
    let private providerBefore : CrossAssemblySpec =
        CrossAssemblySpec.library
            "Provider"
            []
            [
                """
namespace Provider;

public static class Parent
{
    public static int Value;
    public static void Gone() { }
}

public interface IProbe { int Probe() => 0; }
public interface IL : IProbe { int IProbe.Probe() => 1; }
public interface IR : IProbe { }

public interface IFoo { int Frob() => 2; }
public interface IBar : IFoo { }

public class Vanished { }
"""
            ]

    /// The provider a binding guest runs against: the field, method and class are gone, both
    /// interfaces override the method, and the default body is reabstracted.
    let private providerAfter : CrossAssemblySpec =
        CrossAssemblySpec.library
            "Provider"
            []
            [
                """
namespace Provider;

public static class Parent { }

public interface IProbe { int Probe() => 0; }
public interface IL : IProbe { int IProbe.Probe() => 1; }
public interface IR : IProbe { int IProbe.Probe() => 3; }

public interface IFoo { int Frob() => 2; }
public interface IBar : IFoo { abstract int IFoo.Frob(); }
"""
            ]

    /// Uses what `providerBefore` declares and `providerAfter` does not, or decides differently,
    /// as its first argument names, under a culture as `guest`'s, and exits as `guest` does.
    let private bindingGuest : CrossAssemblySpec =
        CrossAssemblySpec.entryPoint
            "Binding"
            [ "Provider" ]
            [
                """
using System;
using System.Globalization;
using System.Runtime.CompilerServices;
using Provider;

sealed class NamelessCulture : CultureInfo
{
    public NamelessCulture() : base("") { }
    public bool Nameless;
    public override string Name => Nameless ? throw new TimeZoneNotFoundException("a culture's name") : base.Name;
}

sealed class Both : IL, IR { }

sealed class Reabstracted : IBar { }

static class Program
{
    [MethodImpl(MethodImplOptions.NoInlining)]
    static int Read() => Parent.Value;

    [MethodImpl(MethodImplOptions.NoInlining)]
    static void Call() => Parent.Gone();

    [MethodImpl(MethodImplOptions.NoInlining)]
    static int Ambiguous(IProbe probe) => probe.Probe();

    [MethodImpl(MethodImplOptions.NoInlining)]
    static int Reabstraction(IFoo foo) => foo.Frob();

    [MethodImpl(MethodImplOptions.NoInlining)]
    static object Construct() => new Vanished();

    static void Run(string failure)
    {
        switch (failure)
        {
            case "Read": Read(); break;
            case "Call": Call(); break;
            case "Ambiguous": Ambiguous(new Both()); break;
            case "Reabstraction": Reabstraction(new Reabstracted()); break;
            case "Construct": Construct(); break;
            default: throw new ArgumentException(failure);
        }
    }

    static Exception Raise(string failure)
    {
        try
        {
            Run(failure);
            return null;
        }
        catch (Exception e)
        {
            return e;
        }
    }

    static int Main(string[] args)
    {
        if (args[1] == "twice")
        {
            Exception first = Raise(args[0]), second = Raise(args[0]);
            if (first == null || second == null) return 0;
            if (first.GetType().FullName != args[2] || second.GetType().FullName != args[2]) return 3;
            return ReferenceEquals(first, second) ? 1 : 4;
        }

        var culture = new NamelessCulture();
        if (args[1] == "nameless")
        {
            CultureInfo.CurrentUICulture = culture;
            culture.Nameless = true;
        }

        try
        {
            Run(args[0]);
            return 0;
        }
        catch (TimeZoneNotFoundException)
        {
            return 2;
        }
        catch (Exception e)
        {
            culture.Nameless = false;
            return e.GetType().FullName == args[2] ? 1 : 3;
        }
    }
}
"""
            ]

    /// How the binding guest, compiled against `providerBefore` and run against `providerAfter`,
    /// exits when it uses `failure` in each of `modes`, expecting the exception of this full name.
    let private runBinding (failure : string) (exceptionName : string) (modes : string list) : RealRuntimeResult list =
        let compiled =
            CrossAssemblyHarness.compileAssemblies [ providerBefore ; bindingGuest ]

        let after = CrossAssemblyHarness.compileAssemblies [ providerAfter ]
        let tempDir = Path.Combine (Path.GetTempPath (), Path.GetRandomFileName ())
        Directory.CreateDirectory tempDir |> ignore<DirectoryInfo>

        try
            CrossAssemblyHarness.writeAssemblies tempDir (Map.add "Provider" after.["Provider"] compiled)
            let entryPath = Path.Combine (tempDir, "Binding.dll")

            modes
            |> List.map (fun mode -> RealRuntime.executeAssemblyInPlace [| failure ; mode ; exceptionName |] entryPath)
        finally
            try
                if Directory.Exists tempDir then
                    Directory.Delete (tempDir, true)
            with
            | :? IOException
            | :? UnauthorizedAccessException -> ()

    /// What the binding guest uses that the provider it runs against lacks or decides differently,
    /// and the exception the runtime raises for it.
    let bindingFailures : obj[] list =
        [
            [| "Read" ; "System.MissingFieldException" |]
            [| "Call" ; "System.MissingMethodException" |]
            [| "Ambiguous" ; "System.Runtime.AmbiguousImplementationException" |]
            [| "Reabstraction" ; "System.EntryPointNotFoundException" |]
        ]

    /// The escape analysis takes the runtime to make each of these exceptions by running its
    /// parameterless constructor, which looks up its message.
    [<TestCaseSource(nameof bindingFailures)>]
    let ``a failure to bind or dispatch raises an exception made by running its parameterless constructor``
        (failure : string)
        (exceptionName : string)
        : unit
        =
        Net11Frontier.park Net11Frontier.exceptionMaking

        runBinding failure exceptionName [ "named" ; "twice" ; "nameless" ]
        |> shouldEqual
            [
                RealRuntimeResult.NormalExit 1
                RealRuntimeResult.NormalExit 4
                RealRuntimeResult.NormalExit 2
            ]

    /// The escape analysis takes the runtime to make the `TypeLoadException` of a type that fails to
    /// bind by running its `(string, string, string, int)` constructor (`ExceptionMaking.TypeLoadConstructor`),
    /// which formats its message from the runtime's native resources, reading no culture.
    [<Test>]
    let ``a type that fails to bind raises a TypeLoadException made by running a constructor that reads no culture``
        ()
        : unit
        =
        runBinding "Construct" "System.TypeLoadException" [ "named" ; "twice" ; "nameless" ]
        |> shouldEqual
            [
                RealRuntimeResult.NormalExit 1
                RealRuntimeResult.NormalExit 4
                RealRuntimeResult.NormalExit 1
            ]

    /// Touches a type whose initializer throws an exception whose construction looks nothing up,
    /// under a current UI culture whose name cannot be read at all when its argument is "nameless",
    /// can be read except the first time when it is "once", and can always be read when it is
    /// "named". Exits 1 if a `TypeInitializationException` escaped the touch, 4 if the initializer's
    /// own exception did, 2 if the culture's did, 3 if another did, and 0 if none did.
    let private initializerGuest : string =
        """
using System;
using System.Globalization;

sealed class Boom : Exception { }

static class Failing
{
    static Failing() { throw new Boom(); }
    public static void Touch() { }
}

sealed class FickleCulture : CultureInfo
{
    public FickleCulture() : base("") { }
    // How many more reads of the name throw; every one does while it is negative.
    public int Throws;
    public override string Name
    {
        get
        {
            if (Throws == 0) return base.Name;
            if (Throws > 0) Throws--;
            throw new TimeZoneNotFoundException("a culture's name");
        }
    }
}

static class Program
{
    static int Main(string[] args)
    {
        var culture = new FickleCulture();
        if (args[0] != "named")
        {
            CultureInfo.CurrentUICulture = culture;
            culture.Throws = args[0] == "once" ? 1 : -1;
        }

        try
        {
            Failing.Touch();
            return 0;
        }
        catch (TimeZoneNotFoundException)
        {
            return 2;
        }
        catch (TypeInitializationException)
        {
            return 1;
        }
        catch (Boom)
        {
            return 4;
        }
        catch (Exception)
        {
            return 3;
        }
    }
}
"""

    let private initializerImage : Lazy<byte[]> =
        lazy (Roslyn.compile [ initializerGuest ])

    [<Test>]
    let ``a type initializer's failure raises what ExceptionMaking says`` () : unit =
        Net11Frontier.park
            "Measured on real .NET 11: when looking up the message of the `TypeInitializationException` the runtime makes for a failed initializer raises, that wrapper is still raised (the guest exits 1), where on .NET 10 the initializer's own exception escapes in its place (4), as `ExceptionMaking.InitializerFailure` describes. Un-park when `ExceptionMaking` describes each runtime's making."

        let makings = makingsIn initializerImage.Value "Main" OpcodeFault.TypeInitialization

        makings |> shouldNotEqual []

        for making in makings do
            making |> shouldEqual ExceptionMaking.InitializerFailure

        let touch (culture : string) : RealRuntimeResult =
            RealRuntime.executeWithRealRuntime [| culture |] initializerImage.Value

        // The runtime wraps what the initializer raised.
        touch "named" |> shouldEqual (RealRuntimeResult.NormalExit 1)
        // Making the wrapper looks up its message, and fails; the initializer's own exception
        // escapes in its place.
        touch "once" |> shouldEqual (RealRuntimeResult.NormalExit 4)
        // So it does again, but preserving the stack trace of that exception before raising it
        // looks up resources too, and what that raises escapes instead.
        touch "nameless" |> shouldEqual (RealRuntimeResult.NormalExit 2)
