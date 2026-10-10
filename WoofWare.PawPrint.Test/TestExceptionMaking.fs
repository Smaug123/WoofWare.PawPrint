namespace WoofWare.PawPrint.Test

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

    /// Triggers the fault its first argument names, under a current UI culture whose name cannot be
    /// read when its second argument is "nameless". Looking up a CoreLib exception's message reads
    /// that name, so where the runtime runs the exception's constructor, the culture's exception is
    /// raised in the fault's place. Exits 1 if the exception its third argument names escaped the
    /// fault, 2 if the culture's did, 3 if another did, and 0 if none did.
    let private guest : string =
        """
using System;
using System.Globalization;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Runtime.Intrinsics;
using System.Threading;

sealed class NamelessCulture : CultureInfo
{
    public NamelessCulture() : base("") { }
    public bool Nameless;
    public override string Name => Nameless ? throw new TimeZoneNotFoundException("a culture's name") : base.Name;
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
            default: throw new ArgumentException(fault);
        }
    }

    static int Main(string[] args)
    {
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

    let private image : Lazy<byte[]> = lazy (Roslyn.compile [ guest ])

    /// A fault the guest triggers: how `ExceptionMaking` says the runtime makes its exception, the
    /// exception, and whether the CPU may report it on some target, so that a constructor that
    /// raises ends the process rather than raising in the fault's place.
    type private Case =
        {
            Making : ExceptionMaking
            Exception : string
            MayEndProcess : bool
        }

    let private cases : Map<string, Case> =
        Map.ofList
            [
                "Field",
                {
                    Making = ExceptionMaking.ofOpcodeFault OpcodeFault.NullReference
                    Exception = "System.NullReferenceException"
                    MayEndProcess = true
                }
                "Element",
                {
                    Making = ExceptionMaking.ofOpcodeFault OpcodeFault.IndexOutOfRange
                    Exception = "System.IndexOutOfRangeException"
                    MayEndProcess = false
                }
                "Store",
                {
                    Making = ExceptionMaking.ofOpcodeFault OpcodeFault.ArrayTypeMismatch
                    Exception = "System.ArrayTypeMismatchException"
                    MayEndProcess = false
                }
                "Cast",
                {
                    Making = ExceptionMaking.ofOpcodeFault OpcodeFault.InvalidCast
                    Exception = "System.InvalidCastException"
                    MayEndProcess = false
                }
                "Sum",
                {
                    Making = ExceptionMaking.ofOpcodeFault OpcodeFault.Overflow
                    Exception = "System.OverflowException"
                    MayEndProcess = false
                }
                // x64 divides in hardware, which reports a zero divisor.
                "Quotient",
                {
                    Making = ExceptionMaking.ofOpcodeFault OpcodeFault.DivideByZero
                    Exception = "System.DivideByZeroException"
                    MayEndProcess = true
                }
                "Exchange",
                {
                    Making = ExceptionMaking.ofPrimitiveFault PrimitiveFault.NullReference
                    Exception = "System.NullReferenceException"
                    MayEndProcess = true
                }
                "Load",
                {
                    Making =
                        match ExceptionMaking.ofInstructionFault InstructionFault.NullAddress with
                        | Some making -> making
                        | None -> failwith "ExceptionMaking says the runtime does not make a null address's exception"
                    Exception = "System.NullReferenceException"
                    MayEndProcess = true
                }
            ]

    let faults : string list = cases |> Map.keys |> List.ofSeq

    [<TestCaseSource(nameof faults)>]
    let ``what the runtime runs to make a fault's exception is what ExceptionMaking says`` (fault : string) : unit =
        let case = cases.[fault]

        // The guest does raise this fault.
        match RealRuntime.executeWithRealRuntime [| fault ; "named" ; case.Exception |] image.Value with
        | RealRuntimeResult.NormalExit 1 -> ()
        | other -> failwith $"%s{fault} with a well-behaved culture: expected its own exception, got %A{other}"

        let nameless =
            RealRuntime.executeWithRealRuntime [| fault ; "nameless" ; case.Exception |] image.Value

        match case.Making with
        | ExceptionMaking.ParameterlessConstructor ->
            match nameless with
            | RealRuntimeResult.NormalExit 2 -> ()
            // SIGABRT: the VM's handler for a fault the CPU reported met the constructor's exception.
            | RealRuntimeResult.NormalExit 134 when case.MayEndProcess -> ()
            | other ->
                failwith
                    $"%s{fault} under a culture whose name throws: expected the culture's exception in place of %s{case.Exception}, got %A{other}"
        // Running no managed code, the runtime never reads the culture.
        | ExceptionMaking.Preallocated ->
            match nameless with
            | RealRuntimeResult.NormalExit 1 -> ()
            | other ->
                failwith
                    $"%s{fault} under a culture whose name throws: expected %s{case.Exception} itself, got %A{other}"
        | ExceptionMaking.InitializerFailure -> failwith $"%s{fault}: no case here raises a TypeInitializationException"
