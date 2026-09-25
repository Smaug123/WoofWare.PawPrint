namespace WoofWare.PawPrint.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `IntrinsicPrimitive.contract` states what each primitive can do to its caller. These tests hold
/// the contracts to real .NET: a guest performs each primitive, with a null for every argument
/// whose null the contract says faults and with ordinary values otherwise, and the real runtime
/// must raise exactly the `NullReferenceException`s the contracts predict.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestIntrinsicContracts =

    /// For each primitive, a C# statement that performs it. An argument the contract names in a
    /// `FaultCondition.ArgumentNull` is null; `CoreLib`'s internal methods are reached by
    /// reflection, which reports a fault as the inner exception of a `TargetInvocationException`.
    let private calls : (IntrinsicPrimitive * string) list =
        let internalStatic (ty : string) (name : string) (generic : string option) (args : string) =
            let method =
                $"typeof(%s{ty}).GetMethod(\"%s{name}\", BindingFlags.NonPublic | BindingFlags.Static)"

            let method =
                match generic with
                | None -> method
                | Some t -> $"%s{method}.MakeGenericMethod(typeof(%s{t}))"

            $"%s{method}.Invoke(null, %s{args});"

        [
            IntrinsicPrimitive.FullBarrier, "Interlocked.MemoryBarrier();"
            IntrinsicPrimitive.ReadBarrier, "Volatile.ReadBarrier();"
            IntrinsicPrimitive.WriteBarrier, "Volatile.WriteBarrier();"
            IntrinsicPrimitive.GcPoll, internalStatic "Thread" "FastPollGC" None "null"
            IntrinsicPrimitive.AtomicCompareExchange AtomicOperand.UInt8,
            "Interlocked.CompareExchange(ref Unsafe.NullRef<byte>(), (byte)1, (byte)0);"
            IntrinsicPrimitive.AtomicCompareExchange AtomicOperand.UInt16,
            "Interlocked.CompareExchange(ref Unsafe.NullRef<ushort>(), (ushort)1, (ushort)0);"
            IntrinsicPrimitive.AtomicCompareExchange AtomicOperand.Int32,
            "Interlocked.CompareExchange(ref Unsafe.NullRef<int>(), 1, 0);"
            IntrinsicPrimitive.AtomicCompareExchange AtomicOperand.Int64,
            "Interlocked.CompareExchange(ref Unsafe.NullRef<long>(), 1L, 0L);"
            IntrinsicPrimitive.AtomicExchange AtomicOperand.UInt8,
            "Interlocked.Exchange(ref Unsafe.NullRef<byte>(), (byte)1);"
            IntrinsicPrimitive.AtomicExchange AtomicOperand.UInt16,
            "Interlocked.Exchange(ref Unsafe.NullRef<ushort>(), (ushort)1);"
            IntrinsicPrimitive.AtomicExchange AtomicOperand.Int32, "Interlocked.Exchange(ref Unsafe.NullRef<int>(), 1);"
            IntrinsicPrimitive.AtomicExchange AtomicOperand.Int64,
            "Interlocked.Exchange(ref Unsafe.NullRef<long>(), 1L);"
            // `Interlocked.Add` is `ExchangeAdd(ref location, value) + value`.
            IntrinsicPrimitive.AtomicAdd AtomicOperand.Int32, "Interlocked.Add(ref Unsafe.NullRef<int>(), 1);"
            IntrinsicPrimitive.AtomicAdd AtomicOperand.Int64, "Interlocked.Add(ref Unsafe.NullRef<long>(), 1L);"
            IntrinsicPrimitive.MethodTableOf,
            internalStatic "RuntimeHelpers" "GetMethodTable" None "new object[] { null }"
            IntrinsicPrimitive.ArrayDataReference, "MemoryMarshal.GetArrayDataReference((int[])null);"
            IntrinsicPrimitive.IsReferenceOrContainsReferences, "RuntimeHelpers.IsReferenceOrContainsReferences<int>();"
            IntrinsicPrimitive.IsBitwiseEquatable,
            internalStatic "RuntimeHelpers" "IsBitwiseEquatable" (Some "int") "null"
            IntrinsicPrimitive.ReciprocalEstimate FloatWidth.Double, "Math.ReciprocalEstimate(0.0);"
            IntrinsicPrimitive.ReciprocalEstimate FloatWidth.Single, "MathF.ReciprocalEstimate(float.NaN);"
            IntrinsicPrimitive.ReciprocalSqrtEstimate FloatWidth.Double, "Math.ReciprocalSqrtEstimate(-1.0);"
            IntrinsicPrimitive.ReciprocalSqrtEstimate FloatWidth.Single, "MathF.ReciprocalSqrtEstimate(0f);"
            IntrinsicPrimitive.MultiplyAddEstimate FloatWidth.Double,
            "double.MultiplyAddEstimate(double.MaxValue, 2.0, double.NegativeInfinity);"
            IntrinsicPrimitive.MultiplyAddEstimate FloatWidth.Single, "float.MultiplyAddEstimate(float.NaN, 0f, 1f);"
            IntrinsicPrimitive.ConvertToIntegerNative FloatWidth.Double,
            "double.ConvertToIntegerNative<int>(double.NaN);"
            IntrinsicPrimitive.ConvertToIntegerNative FloatWidth.Single, "float.ConvertToIntegerNative<long>(1e30f);"
        ]

    /// Primitives no statement here can perform with the argument its contract names null.
    /// `VolatileReadAsByref` takes a `ref nint`, and reflection passes a byref argument as a
    /// fresh box, never as a null byref.
    let private unexercised : IntrinsicPrimitive list =
        [ IntrinsicPrimitive.VolatileReadByref ]

    let private raisesOnNull (primitive : IntrinsicPrimitive) : bool =
        (IntrinsicPrimitive.contract primitive).Raises
        |> List.exists (fun (fault, _) -> fault = PrimitiveFault.NullReference)

    /// For each primitive whose contract says a misaligned location can fault, a C# statement
    /// that performs it on a location at an address one below a multiple of 16, which `at`
    /// holds, so that any access wider than a byte crosses a 16-byte boundary. Arm64 CPUs with
    /// FEAT_LSE2 fault on exactly that; those without it, on any misalignment; x64 never.
    let private misalignedCalls : (IntrinsicPrimitive * string) list =
        [
            IntrinsicPrimitive.AtomicCompareExchange AtomicOperand.UInt16,
            "Interlocked.CompareExchange(ref Unsafe.AsRef<ushort>(at), (ushort)1, (ushort)0);"
            IntrinsicPrimitive.AtomicCompareExchange AtomicOperand.Int32,
            "Interlocked.CompareExchange(ref Unsafe.AsRef<int>(at), 1, 0);"
            IntrinsicPrimitive.AtomicCompareExchange AtomicOperand.Int64,
            "Interlocked.CompareExchange(ref Unsafe.AsRef<long>(at), 1L, 0L);"
            IntrinsicPrimitive.AtomicExchange AtomicOperand.UInt16,
            "Interlocked.Exchange(ref Unsafe.AsRef<ushort>(at), (ushort)1);"
            IntrinsicPrimitive.AtomicExchange AtomicOperand.Int32, "Interlocked.Exchange(ref Unsafe.AsRef<int>(at), 1);"
            IntrinsicPrimitive.AtomicExchange AtomicOperand.Int64,
            "Interlocked.Exchange(ref Unsafe.AsRef<long>(at), 1L);"
            IntrinsicPrimitive.AtomicAdd AtomicOperand.Int32, "Interlocked.Add(ref Unsafe.AsRef<int>(at), 1);"
            IntrinsicPrimitive.AtomicAdd AtomicOperand.Int64, "Interlocked.Add(ref Unsafe.AsRef<long>(at), 1L);"
        ]

    [<Test>]
    let ``the misalignment cases are exactly the primitives whose contracts name misalignment`` () : unit =
        let namesMisalignment (primitive : IntrinsicPrimitive) =
            (IntrinsicPrimitive.contract primitive).Raises
            |> List.exists (fun (fault, _) -> fault = PrimitiveFault.DataMisaligned)

        let expected = calls |> List.map fst |> List.filter namesMisalignment |> Set.ofList

        misalignedCalls |> List.map fst |> Set.ofList |> shouldEqual expected

    [<Test>]
    let ``every primitive CoreLib performs is exercised`` () : unit =
        let exercised = calls |> List.map fst |> Set.ofList
        let unexercised = Set.ofList unexercised

        Set.intersect exercised unexercised |> shouldEqual Set.empty

        // The primitives the pinned linux-x64 CoreLib performs, as `TestIntrinsicBody` pins them.
        let corelib =
            match LinuxCoreLibFlavour.linuxFrameworkDir with
            | Some dir ->
                let _, loggerFactory = LoggerFactory.makeTest ()
                Assembly.readFile loggerFactory (LinuxCoreLibFlavour.corelibPath dir)
            | None ->
                let _, loggerFactory = LoggerFactory.makeTest ()
                Assembly.readFile loggerFactory typeof<obj>.Assembly.Location

        let performed =
            corelib.Methods.Keys
            |> Seq.choose (IntrinsicPrimitive.recognise corelib)
            |> Set.ofSeq

        Set.difference performed (Set.union exercised unexercised)
        |> shouldEqual Set.empty

    [<Test>]
    let ``real .NET raises exactly the faults each primitive's contract states`` () : unit =
        let cases =
            calls
            |> List.mapi (fun i (primitive, statement) ->
                let expectNull = if raisesOnNull primitive then "true" else "false"

                $"""
        try
        {{
            %s{statement}
            if (%s{expectNull}) return %d{2 * i + 1};
        }}
        catch (NullReferenceException)
        {{
            if (!%s{expectNull}) return %d{2 * i + 2};
        }}
        catch (TargetInvocationException e) when (e.InnerException is NullReferenceException)
        {{
            if (!%s{expectNull}) return %d{2 * i + 2};
        }}
"""
            )
            |> String.concat ""

        let misalignedBase = 2 * calls.Length

        let misaligned =
            misalignedCalls
            |> List.mapi (fun i (_, statement) ->
                $"""
        fixed (byte* p = buffer)
        {{
            void* at = p + (15 - (long)p %% 16 + 16) %% 16;
            try
            {{
                %s{statement}
                if (arm64) return %d{misalignedBase + 2 * i + 1};
            }}
            catch (DataMisalignedException)
            {{
                if (!arm64) return %d{misalignedBase + 2 * i + 2};
            }}
        }}
"""
            )
            |> String.concat ""

        let source =
            $"""
using System;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Threading;

unsafe class Program
{{
    static int Main(string[] args)
    {{
%s{cases}
        byte[] buffer = new byte[64];
        bool arm64 = RuntimeInformation.ProcessArchitecture == Architecture.Arm64;
%s{misaligned}
        return 0;
    }}
}}
"""

        match RealRuntime.executeWithRealRuntime [||] (Roslyn.compile [ source ]) with
        | RealRuntimeResult.NormalExit 0 -> ()
        | RealRuntimeResult.NormalExit code when code > misalignedBase ->
            let primitive, statement = misalignedCalls.[(code - misalignedBase - 1) / 2]

            let what =
                if code % 2 = 1 then
                    "did not raise DataMisalignedException on Arm64"
                else
                    "raised DataMisalignedException off Arm64"

            failwith $"%A{primitive}, performed as `%s{statement}` on a location crossing 16 bytes, %s{what}"
        | RealRuntimeResult.NormalExit code ->
            let primitive, statement = calls.[(code - 1) / 2]

            let what =
                if code % 2 = 1 then
                    "did not raise the NullReferenceException its contract states"
                else
                    "raised a NullReferenceException its contract does not state"

            failwith $"%A{primitive}, performed as `%s{statement}`, %s{what}"
        | other -> failwith $"the guest did not exit normally: %O{other}"
