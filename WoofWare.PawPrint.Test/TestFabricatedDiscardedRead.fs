namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open FsUnitTyped
open NUnit.Framework

/// An `ldind` whose value the next instruction pops, against the real runtime.
///
/// Roslyn opens each of its `<PrivateImplementationDetails>.InlineArray*` helpers with
/// `ldarg.0; ldind.u1; pop`, a null check of the buffer's address, and .NET 11's CoreLib and
/// System.Linq are compiled with those helpers. When the buffer holds object references its first
/// byte is a byte of one, which has no value; the JIT reads none of it, since it imports a popped
/// indirection as a null check.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFabricatedDiscardedRead =

    /// Each `ldind` the fixture discards, by the name of the method that discards it.
    let private widths : (string * OpCode) list =
        [
            "U1", OpCodes.Ldind_U1
            "I1", OpCodes.Ldind_I1
            "U2", OpCodes.Ldind_U2
            "I2", OpCodes.Ldind_I2
            "U4", OpCodes.Ldind_U4
            "I4", OpCodes.Ldind_I4
            "I8", OpCodes.Ldind_I8
            "I", OpCodes.Ldind_I
            "R4", OpCodes.Ldind_R4
            "R8", OpCodes.Ldind_R8
        ]

    let private unsafeAs : MethodInfo =
        typeof<Unsafe>.GetMethods ()
        |> Array.filter (fun m ->
            m.Name = "As"
            && m.GetGenericArguments().Length = 2
            && (m.GetParameters ()).Length = 1
        )
        |> Array.exactlyOne

    let private unsafeAddInt : MethodInfo =
        typeof<Unsafe>.GetMethods ()
        |> Array.filter (fun m ->
            let ps = m.GetParameters ()

            m.Name = "Add"
            && m.GetGenericArguments().Length = 1
            && ps.Length = 2
            && ps.[0].ParameterType.IsByRef
            && ps.[1].ParameterType = typeof<int>
        )
        |> Array.exactlyOne

    let private createSpan : MethodInfo = typeof<MemoryMarshal>.GetMethod "CreateSpan"

    /// `N`, a static class holding, for each of `widths`, `Discard<width><T>(ref T)`, which reads
    /// through its argument at that width and pops the value; `ElementRef<TBuffer, TElement>` and
    /// `AsSpan<TBuffer, TElement>`, transcribed from the helpers .NET 11's CoreLib carries; `Joined<T>(ref
    /// T, bool)`, whose `pop` a branch reaches as well as the discarded read, and `JoinedFloat(ref object,
    /// bool)`, at whose `pop` a float32 meets a double; and `Used<T>(ref T)`,
    /// which returns the byte it reads.
    let private fabricate () : byte[] =
        let builder =
            PersistedAssemblyBuilder (AssemblyName "DiscardedRead", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "DiscardedRead"

        let n =
            modul.DefineType ("N", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        let attributes = MethodAttributes.Public ||| MethodAttributes.Static

        for name, opcode in widths do
            let m = n.DefineMethod ($"Discard%s{name}", attributes)
            let t = (m.DefineGenericParameters [| "T" |]).[0]
            m.SetReturnType typeof<Void>
            m.SetParameters [| t.MakeByRefType () |]
            let il = m.GetILGenerator ()
            il.Emit OpCodes.Ldarg_0
            il.Emit opcode
            il.Emit OpCodes.Pop
            il.Emit OpCodes.Ret

        let elementRef = n.DefineMethod ("ElementRef", attributes)
        let ps = elementRef.DefineGenericParameters [| "TBuffer" ; "TElement" |]
        elementRef.SetReturnType (ps.[1].MakeByRefType ())
        elementRef.SetParameters [| ps.[0].MakeByRefType () ; typeof<int> |]
        let il = elementRef.GetILGenerator ()
        il.Emit OpCodes.Ldarg_0
        il.Emit OpCodes.Ldind_U1
        il.Emit OpCodes.Pop
        il.Emit OpCodes.Ldarg_0
        il.Emit (OpCodes.Call, unsafeAs.MakeGenericMethod (ps.[0], ps.[1]))
        il.Emit OpCodes.Ldarg_1
        il.Emit (OpCodes.Call, unsafeAddInt.MakeGenericMethod ps.[1])
        il.Emit OpCodes.Ret

        let asSpan = n.DefineMethod ("AsSpan", attributes)
        let ps = asSpan.DefineGenericParameters [| "TBuffer" ; "TElement" |]
        asSpan.SetReturnType (createSpan.ReturnType.GetGenericTypeDefinition().MakeGenericType ps.[1])
        asSpan.SetParameters [| ps.[0].MakeByRefType () ; typeof<int> |]
        let il = asSpan.GetILGenerator ()
        il.Emit OpCodes.Ldarg_0
        il.Emit OpCodes.Ldind_U1
        il.Emit OpCodes.Pop
        il.Emit OpCodes.Ldarg_0
        il.Emit (OpCodes.Call, unsafeAs.MakeGenericMethod (ps.[0], ps.[1]))
        il.Emit OpCodes.Ldarg_1
        il.Emit (OpCodes.Call, createSpan.MakeGenericMethod ps.[1])
        il.Emit OpCodes.Ret

        // Returns 0 when the discarded read reached the `pop`, and 1 when the branch did.
        let joined = n.DefineMethod ("Joined", attributes)
        let t = (joined.DefineGenericParameters [| "T" |]).[0]
        joined.SetReturnType typeof<int>
        joined.SetParameters [| t.MakeByRefType () ; typeof<bool> |]
        let il = joined.GetILGenerator ()
        let other = il.DefineLabel ()
        let join = il.DefineLabel ()
        il.Emit OpCodes.Ldarg_1
        il.Emit (OpCodes.Brtrue, other)
        il.Emit OpCodes.Ldarg_0
        il.Emit OpCodes.Ldind_U1
        il.MarkLabel join
        il.Emit OpCodes.Pop
        il.Emit OpCodes.Ldarg_1
        il.Emit OpCodes.Ret
        il.MarkLabel other
        il.Emit OpCodes.Ldc_I4_7
        il.Emit (OpCodes.Br, join)

        // A join at the `pop` where a float32 below the discarded value meets a double: CoreCLR
        // widens the float32 on arrival, so either path returns 2^24 + 1. Inlined into a caller
        // passing a constant, the branch would fold and the join with it.
        let joinedFloat = n.DefineMethod ("JoinedFloat", attributes)
        joinedFloat.SetImplementationFlags MethodImplAttributes.NoInlining
        joinedFloat.SetReturnType typeof<double>
        joinedFloat.SetParameters [| typeof<obj>.MakeByRefType () ; typeof<bool> |]
        let il = joinedFloat.GetILGenerator ()
        let other = il.DefineLabel ()
        let join = il.DefineLabel ()
        // Branching on a local rather than on the argument, which PawPrint's stack shape counts as
        // a branch the JIT may fold, leaving the join's width undecided.
        il.DeclareLocal typeof<bool> |> ignore<LocalBuilder>
        il.Emit OpCodes.Ldarg_1
        il.Emit OpCodes.Stloc_0
        il.Emit OpCodes.Ldloc_0
        il.Emit (OpCodes.Brtrue, other)
        il.Emit (OpCodes.Ldc_R4, 16777216.0f)
        il.Emit OpCodes.Ldarg_0
        il.Emit OpCodes.Ldind_U1
        il.MarkLabel join
        il.Emit OpCodes.Pop
        il.Emit (OpCodes.Ldc_R4, 1.0f)
        il.Emit OpCodes.Add
        il.Emit OpCodes.Ret
        il.MarkLabel other
        il.Emit (OpCodes.Ldc_R8, 16777216.0)
        il.Emit OpCodes.Ldc_I4_0
        il.Emit (OpCodes.Br, join)

        let used = n.DefineMethod ("Used", attributes)
        let t = (used.DefineGenericParameters [| "T" |]).[0]
        used.SetReturnType typeof<int>
        used.SetParameters [| t.MakeByRefType () |]
        let il = used.GetILGenerator ()
        il.Emit OpCodes.Ldarg_0
        il.Emit OpCodes.Ldind_U1
        il.Emit OpCodes.Ret

        n.CreateType () |> ignore<Type>

        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    /// The driver's buffers. Under CoreCLR's auto layout a struct's references come first, so the
    /// first byte of every one of these but `Ints` is a byte of an object reference.
    let private declarations : string =
        """
[InlineArray(2)]
public struct Refs { private object _e; }

public struct Pair { public int Tag; public object P; }

[InlineArray(3)]
public struct Pairs { private Pair _e; }

public struct Mixed { public byte B; public object P; public long L; }

[InlineArray(4)]
public struct Ints { private long _e; }
"""

    /// Each buffer the discarded reads go through: C# statements making `b`, its type, and a C#
    /// condition that holds of `b` as made.
    let private buffers : Map<string, string * string * string> =
        [
            "RefsEmpty", ("var b = new Refs();", "Refs", "b[0] == null && b[1] == null")
            "RefsFull",
            ("object o0 = new object(), o1 = new object(); var b = new Refs(); b[0] = o0; b[1] = o1;",
             "Refs",
             "ReferenceEquals(b[0], o0) && ReferenceEquals(b[1], o1)")
            "Pairs",
            ("object o = new object(); var b = new Pairs(); b[0] = new Pair { Tag = 3, P = o };",
             "Pairs",
             "b[0].Tag == 3 && ReferenceEquals(b[0].P, o) && b[1].P == null && b[2].Tag == 0")
            "Mixed",
            ("object o = new object(); var b = new Mixed { B = 5, P = o, L = 7 };",
             "Mixed",
             "b.B == 5 && ReferenceEquals(b.P, o) && b.L == 7")
            "Object", ("object o = new object(); object b = o;", "object", "ReferenceEquals(b, o)")
            "Ints",
            ("var b = new Ints(); b[0] = 0x0102030405060708L;", "Ints", "b[0] == 0x0102030405060708L && b[3] == 0")
        ]
        |> Map.ofList

    let discardCases : TestCaseData list =
        [
            for width, _ in widths do
                for buffer in buffers.Keys do
                    yield TestCaseData(width, buffer).SetName $"discarded ldind.%s{width} over %s{buffer}"
        ]

    let private driver (body : string) : string =
        $$"""
using System;
using System.Runtime.CompilerServices;

{{declarations}}

public static class Driver
{
    public static int Main(string[] args)
    {
{{body}}
    }
}
"""

    [<TestCaseSource(nameof discardCases)>]
    let ``a read whose value is popped completes, and leaves the buffer as it was``
        (width : string)
        (buffer : string)
        : unit
        =
        let make, ty, holds = buffers.[buffer]

        let body =
            $"""
        %s{make}
        if (!(%s{holds})) return 2;
        N.Discard%s{width}<%s{ty}>(ref b);
        return %s{holds} ? 0 : 1;
"""

        FabricatedGuest.run "DiscardedRead" (fabricate ()) $"Discard%s{width}%s{buffer}Driver" (driver body) 0

    let nullCases : string list = widths |> List.map fst

    [<TestCaseSource(nameof nullCases)>]
    let ``a read whose value is popped raises NullReferenceException through a null byref`` (width : string) : unit =
        let body =
            $"""
        try
        {{
            N.Discard%s{width}<Refs>(ref Unsafe.NullRef<Refs>());
        }}
        catch (NullReferenceException)
        {{
            return 0;
        }}

        return 1;
"""

        FabricatedGuest.run "DiscardedRead" (fabricate ()) $"DiscardNull%s{width}Driver" (driver body) 0

    [<Test>]
    let ``Roslyn's inline-array helpers index an inline array of references`` () : unit =
        let body =
            """
        object zero = new object(), one = new object(), zed = new object(), two = new object();
        var b = new Refs();
        N.ElementRef<Refs, object>(ref b, 0) = zero;
        N.ElementRef<Refs, object>(ref b, 1) = one;
        if (!ReferenceEquals(b[0], zero)) return 1;
        if (!ReferenceEquals(N.ElementRef<Refs, object>(ref b, 1), one)) return 2;
        var s = N.AsSpan<Refs, object>(ref b, 2);
        if (s.Length != 2 || !ReferenceEquals(s[1], one)) return 3;
        s[0] = zed;
        if (!ReferenceEquals(N.ElementRef<Refs, object>(ref b, 0), zed)) return 4;
        var p = new Pairs();
        N.ElementRef<Pairs, Pair>(ref p, 2).P = two;
        N.ElementRef<Pairs, Pair>(ref p, 2).Tag = 9;
        if (!ReferenceEquals(p[2].P, two) || p[2].Tag != 9 || p[0].P != null) return 5;
        try
        {
            N.ElementRef<Refs, object>(ref Unsafe.NullRef<Refs>(), 0);
            return 6;
        }
        catch (NullReferenceException)
        {
        }

        return 0;
"""

        FabricatedGuest.run "DiscardedRead" (fabricate ()) "InlineArrayHelpersDriver" (driver body) 0

    [<Test>]
    let ``a pop that a branch also reaches pops what each path pushed`` () : unit =
        let body =
            """
        var b = new Refs();
        b[0] = new object();
        if (N.Joined<Refs>(ref b, false) != 0) return 1;
        if (N.Joined<Refs>(ref b, true) != 1) return 2;
        return 0;
"""

        FabricatedGuest.run "DiscardedRead" (fabricate ()) "JoinedDriver" (driver body) 0

    [<Test>]
    let ``a pop at a join widens the float32 below the discarded value`` () : unit =
        let body =
            """
        object b = new object();
        if (N.JoinedFloat(ref b, false) != 16777217.0) return 1;
        if (N.JoinedFloat(ref b, true) != 16777217.0) return 2;
        return 0;
"""

        FabricatedGuest.run "DiscardedRead" (fabricate ()) "JoinedFloatDriver" (driver body) 0

    [<Test>]
    let ``a byte of an object reference that is used, not popped, is still refused`` () : unit =
        let body =
            """
        var b = new Refs();
        b[0] = new object();
        return N.Used<Refs>(ref b) == 1000 ? 1 : 0;
"""

        match
            FabricatedGuest.runOnBoth "DiscardedRead" (fabricate ()) "UsedDriver" (driver body)
            |> snd
        with
        | FabricatedOutcome.Failed e ->
            e.ToString ()
            |> shouldContainText "refusing byte view over value type containing object references"
        | FabricatedOutcome.Exited code ->
            failwith $"PawPrint gave a byte of an object reference a value, exiting %d{code}"

    /// Real .NET reads whatever follows the array and exits normally; PawPrint stops, since the
    /// address names no storage it holds.
    [<Test>]
    let ``a read whose value is popped still needs its address to name storage`` () : unit =
        let body =
            """
        var a = new object[2];
        a[1] = new object();
        N.DiscardU1<object>(ref Unsafe.Add(ref a[0], 2));
        return 0;
"""

        let onHost, onPawPrint =
            FabricatedGuest.runOnBoth "DiscardedRead" (fabricate ()) "PastEndDriver" (driver body)

        onHost |> shouldEqual (RealRuntimeResult.NormalExit 0)

        match onPawPrint with
        | FabricatedOutcome.Failed e -> e.ToString () |> shouldContainText "array index 2 >= length 2"
        | FabricatedOutcome.Exited code -> failwith $"PawPrint read past the end of an array, exiting %d{code}"
