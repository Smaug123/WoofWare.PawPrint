namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open FsUnitTyped
open NUnit.Framework

/// A `box` of a byref-like type, which the JIT refuses with `InvalidProgramException` unless the
/// instructions after it in its block are a pattern that `impBoxPatternMatch` (importer.cpp) folds
/// the box into: a `brtrue` or `brfalse`; `isinst` and then one of those, where it decides the cast;
/// `unbox.any` of the boxed type; or `isinst` and then `unbox.any`, both of the boxed type. Roslyn
/// emits the first for a null test of a value whose type parameter `allows ref struct`
/// (`RefStructNullTest.cs`); no C# spells the others.
///
/// CoreCLR refuses the method when it compiles it, PawPrint when it reaches the `box` (see
/// docs/divergences.md). Each fabricated method here reaches its `box` on every run, with nothing
/// observable before it and no handler of its own, so that difference cannot show.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFabricatedByRefLikeBox =

    /// The image:
    ///   - `interface IMarker`, and `ref struct Held : IMarker { int X; }` and `ref struct Plain`;
    ///   - `ref struct Holder<T>`, so that code over `Holder<string>` is shared;
    ///   - `static class Boxes`, whose methods are described inline. Each non-generic one takes
    ///     no argument but `BoxBranchLandedOn`'s and returns an `int`.
    let private fabricate () : byte[] =
        let builder =
            PersistedAssemblyBuilder (AssemblyName "ByRefLikeBox", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "ByRefLikeBox"

        let byRefLike (name : string) : TypeBuilder =
            let ty =
                modul.DefineType (
                    name,
                    TypeAttributes.Public
                    ||| TypeAttributes.Sealed
                    ||| TypeAttributes.SequentialLayout,
                    typeof<ValueType>
                )

            ty.SetCustomAttribute (
                CustomAttributeBuilder (
                    typeof<Runtime.CompilerServices.IsByRefLikeAttribute>.GetConstructor Type.EmptyTypes,
                    [||]
                )
            )

            ty

        let marker =
            modul.DefineType ("IMarker", TypeAttributes.Public ||| TypeAttributes.Interface ||| TypeAttributes.Abstract)

        marker.CreateType () |> ignore<Type>

        let heldBuilder = byRefLike "Held"
        heldBuilder.AddInterfaceImplementation marker
        let heldX = heldBuilder.DefineField ("X", typeof<int>, FieldAttributes.Public)
        let held = heldBuilder.CreateType ()

        let plain =
            let ty = byRefLike "Plain"

            ty.DefineField ("X", typeof<int>, FieldAttributes.Public)
            |> ignore<FieldBuilder>

            ty.CreateType ()

        let holder =
            let ty = byRefLike "Holder"
            ty.DefineGenericParameters [| "T" |] |> ignore<GenericTypeParameterBuilder[]>
            ty.CreateType ()

        // F# refuses `typeof` of a byref-like type (FS0412).
        let span =
            typeof<obj>.Assembly.GetType("System.Span`1", true).MakeGenericType [| typeof<int> |]

        let spanEmpty = span.GetProperty("Empty").GetMethod

        let boxes =
            modul.DefineType ("Boxes", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        let staticAttributes = MethodAttributes.Public ||| MethodAttributes.Static

        let returns (il : ILGenerator) (value : int) =
            il.Emit (OpCodes.Ldc_I4, value)
            il.Emit OpCodes.Ret

        // `op` to a return of 2, past a return of 1.
        let branches (op : OpCode) (il : ILGenerator) =
            let taken = il.DefineLabel ()
            il.Emit (op, taken)
            returns il 1
            il.MarkLabel taken
            returns il 2

        // A `Held` whose `X` is 7, through a local.
        let pushesHeld (il : ILGenerator) =
            let local = il.DeclareLocal held
            il.Emit (OpCodes.Ldloca, local)
            il.Emit (OpCodes.Initobj, held)
            il.Emit (OpCodes.Ldloca, local)
            il.Emit (OpCodes.Ldc_I4_7)
            il.Emit (OpCodes.Stfld, heldX)
            il.Emit (OpCodes.Ldloc, local)

        let boxesHeld (il : ILGenerator) =
            pushesHeld il
            il.Emit (OpCodes.Box, held)

        let boxesSpan (il : ILGenerator) =
            il.Emit (OpCodes.Call, spanEmpty)
            il.Emit (OpCodes.Box, span)

        let define (name : string) (emit : ILGenerator -> unit) : unit =
            boxes.DefineMethod(name, staticAttributes, typeof<int>, Type.EmptyTypes).GetILGenerator ()
            |> emit

        // `int name<T>(T)`, whose `T` allows a byref-like type, ending in a return of `value`
        // unless `emit` returns first.
        let generic (name : string) (value : int) (emit : ILGenerator -> Type -> unit) : MethodBuilder =
            let method = boxes.DefineMethod (name, staticAttributes)
            let parameter = method.DefineGenericParameters([| "T" |]).[0]
            parameter.SetGenericParameterAttributes GenericParameterAttributes.AllowByRefLike
            method.SetReturnType typeof<int>
            method.SetParameters [| parameter :> Type |]
            let il = method.GetILGenerator ()
            il.Emit OpCodes.Ldarg_0
            il.Emit (OpCodes.Box, parameter)
            emit il parameter
            returns il value
            method

        // Folded: a box is never null.
        define
            "BoxBrtrue"
            (fun il ->
                boxesHeld il
                branches OpCodes.Brtrue il
            )

        define
            "BoxBrfalse"
            (fun il ->
                boxesHeld il
                branches OpCodes.Brfalse il
            )

        define
            "BoxSpanBrtrue"
            (fun il ->
                boxesSpan il
                branches OpCodes.Brtrue il
            )

        // Folded: the JIT decides each cast.
        for name, boxed, cast, op in
            [
                "BoxIsinstObjectBrtrue", boxesHeld, typeof<obj>, OpCodes.Brtrue
                "BoxIsinstMarkerBrtrue", boxesHeld, (marker :> Type), OpCodes.Brtrue
                "BoxIsinstPlainBrtrue", boxesHeld, plain, OpCodes.Brtrue
                "BoxSpanIsinstMarkerBrfalse", boxesSpan, (marker :> Type), OpCodes.Brfalse
            ] do
            define
                name
                (fun il ->
                    boxed il
                    il.Emit (OpCodes.Isinst, cast)
                    branches op il
                )

        // Folded: the value comes back out as it went in.
        define
            "BoxUnboxSame"
            (fun il ->
                boxesHeld il
                il.Emit (OpCodes.Unbox_Any, held)
                il.Emit (OpCodes.Ldfld, heldX)
                il.Emit OpCodes.Ret
            )

        define
            "BoxIsinstSameUnboxSame"
            (fun il ->
                boxesHeld il
                il.Emit (OpCodes.Isinst, held)
                il.Emit (OpCodes.Unbox_Any, held)
                il.Emit (OpCodes.Ldfld, heldX)
                il.Emit OpCodes.Ret
            )

        // Refused: nothing consumes the box in a pattern the JIT folds.
        define
            "BoxPop"
            (fun il ->
                boxesHeld il
                il.Emit OpCodes.Pop
                returns il 0
            )

        define
            "BoxIsinstPop"
            (fun il ->
                boxesHeld il
                il.Emit (OpCodes.Isinst, typeof<obj>)
                il.Emit OpCodes.Pop
                returns il 0
            )

        // The JIT folds `isinst; ldnull; cgt.un` after a box of any other type.
        define
            "BoxIsinstLdnullCgtun"
            (fun il ->
                boxesHeld il
                il.Emit (OpCodes.Isinst, typeof<obj>)
                il.Emit OpCodes.Ldnull
                il.Emit OpCodes.Cgt_Un
                il.Emit OpCodes.Ret
            )

        define
            "BoxUnboxOther"
            (fun il ->
                boxesHeld il
                il.Emit (OpCodes.Unbox_Any, plain)
                il.Emit OpCodes.Pop
                returns il 0
            )

        define
            "BoxIsinstObjectUnboxSame"
            (fun il ->
                boxesHeld il
                il.Emit (OpCodes.Isinst, typeof<obj>)
                il.Emit (OpCodes.Unbox_Any, held)
                il.Emit OpCodes.Pop
                returns il 0
            )

        // The JIT decides no cast to a `Nullable<T>`.
        define
            "BoxIsinstNullableBrtrue"
            (fun il ->
                boxesHeld il
                il.Emit (OpCodes.Isinst, typeof<Nullable<int>>)
                branches OpCodes.Brtrue il
            )

        // A branch landing on the `brtrue` after the box starts a block there, and the JIT matches
        // a pattern only within the box's block. The driver passes `false`, so that the box runs.
        let landedOn =
            boxes.DefineMethod("BoxBranchLandedOn", staticAttributes, typeof<int>, [| typeof<bool> |]).GetILGenerator ()

        let pushes = landedOn.DefineLabel ()
        let tests = landedOn.DefineLabel ()
        landedOn.Emit OpCodes.Ldarg_0
        landedOn.Emit (OpCodes.Brfalse, pushes)
        landedOn.Emit OpCodes.Ldnull
        landedOn.Emit (OpCodes.Br, tests)
        landedOn.MarkLabel pushes
        boxesHeld landedOn
        landedOn.MarkLabel tests
        branches OpCodes.Brtrue landedOn

        // `System.Void` is no type `isinst` takes, which the JIT refuses as it resolves the token.
        define
            "BoxIsinstVoidBrtrue"
            (fun il ->
                boxesHeld il
                il.Emit (OpCodes.Isinst, typeof<Void>)
                branches OpCodes.Brtrue il
            )

        // A protected block starting between the box and the branch starts a block there too. The
        // IL is invalid, the stack not being empty where the protected block starts, but the JIT
        // refuses it over the box.
        define
            "BoxBranchInTry"
            (fun il ->
                let result = il.DeclareLocal typeof<int>
                boxesHeld il
                il.BeginExceptionBlock () |> ignore<Label>
                let next = il.DefineLabel ()
                il.Emit (OpCodes.Brtrue, next)
                il.MarkLabel next
                il.Emit OpCodes.Ldc_I4_2
                il.Emit (OpCodes.Stloc, result)
                il.BeginCatchBlock typeof<exn>
                il.Emit OpCodes.Pop
                il.EndExceptionBlock ()
                il.Emit (OpCodes.Ldloc, result)
                il.Emit OpCodes.Ret
            )

        // The generic shapes, which only an instance decides.
        let boxTPop = generic "BoxTPop" 3 (fun il _ -> il.Emit OpCodes.Pop)

        let boxTBranch =
            generic
                "BoxTBranch"
                2
                (fun il _ ->
                    let taken = il.DefineLabel ()
                    il.Emit (OpCodes.Brtrue, taken)
                    returns il 1
                    il.MarkLabel taken
                )

        let boxTCast =
            generic
                "BoxTCast"
                2
                (fun il _ ->
                    let taken = il.DefineLabel ()
                    il.Emit (OpCodes.Isinst, marker)
                    il.Emit (OpCodes.Brtrue, taken)
                    returns il 1
                    il.MarkLabel taken
                )

        // Casts the JIT decides from a canonical form only where the target is an interface, and to
        // a generic one only where the canonical form casts to it.
        let castingTo (name : string) (target : Type) =
            generic
                name
                2
                (fun il _ ->
                    let taken = il.DefineLabel ()
                    il.Emit (OpCodes.Isinst, target)
                    il.Emit (OpCodes.Brtrue, taken)
                    returns il 1
                    il.MarkLabel taken
                )

        let boxTCastObject = castingTo "BoxTCastObject" typeof<obj>
        let boxTCastEquatable = castingTo "BoxTCastEquatable" typeof<IEquatable<int>>

        // A box of `Held` itself, tested against a type variable, which shared code sees as
        // `__Canon`: the JIT decides no cast to a canonical form.
        let boxHeldCastT =
            let method = boxes.DefineMethod ("BoxHeldCastT", staticAttributes)
            let parameter = method.DefineGenericParameters([| "T" |]).[0]
            method.SetReturnType typeof<int>
            let il = method.GetILGenerator ()
            boxesHeld il
            il.Emit (OpCodes.Isinst, parameter)
            branches OpCodes.Brtrue il
            method

        let boxTUnbox =
            generic
                "BoxTUnbox"
                4
                (fun il parameter ->
                    il.Emit (OpCodes.Unbox_Any, parameter)
                    il.Emit OpCodes.Pop
                )

        // The JIT never inlines a method that forbids it, so in shared code it refuses this.
        let boxTUnboxNoInlining =
            let method =
                generic
                    "BoxTUnboxNoInlining"
                    4
                    (fun il parameter ->
                        il.Emit (OpCodes.Unbox_Any, parameter)
                        il.Emit OpCodes.Pop
                    )

            method.SetImplementationFlags MethodImplAttributes.NoInlining
            method

        // Nor does it resolve the `unbox.any` operand after an `isinst` unless it proves the
        // `isinst` operand is the boxed type, which in shared code it does not. This one names a
        // type of an assembly that is nowhere to be found where the guest runs.
        let absent =
            let absentBuilder =
                PersistedAssemblyBuilder (AssemblyName "ByRefLikeBoxAbsent", typeof<obj>.Assembly)

            let ty =
                absentBuilder
                    .DefineDynamicModule("ByRefLikeBoxAbsent")
                    .DefineType (
                        "Absent",
                        TypeAttributes.Public
                        ||| TypeAttributes.Sealed
                        ||| TypeAttributes.SequentialLayout,
                        typeof<ValueType>
                    )

            ty.CreateType ()

        let boxTCastUnboxAbsentNoInlining =
            let method =
                generic
                    "BoxTCastUnboxAbsentNoInlining"
                    4
                    (fun il parameter ->
                        il.Emit (OpCodes.Isinst, parameter)
                        il.Emit (OpCodes.Unbox_Any, absent)
                        il.Emit OpCodes.Pop
                    )

            method.SetImplementationFlags MethodImplAttributes.NoInlining
            method

        // A default value of `ty`, through a local.
        let pushesDefault (ty : Type) (il : ILGenerator) =
            let local = il.DeclareLocal ty
            il.Emit (OpCodes.Ldloca, local)
            il.Emit (OpCodes.Initobj, ty)
            il.Emit (OpCodes.Ldloc, local)

        let calls (name : string) (callee : MethodBuilder) (argument : Type) (pushes : ILGenerator -> unit) =
            define
                name
                (fun il ->
                    pushes il
                    il.Emit (OpCodes.Call, callee.MakeGenericMethod [| argument |])
                    il.Emit OpCodes.Ret
                )

        let sharedHolder = holder.MakeGenericType [| typeof<string> |]
        let unsharedHolder = holder.MakeGenericType [| typeof<int> |]

        calls "CallsBoxTPopHeld" boxTPop held pushesHeld
        calls "CallsBoxTPopInt" boxTPop typeof<int> (fun il -> il.Emit OpCodes.Ldc_I4_0)
        calls "CallsBoxTBranchHeld" boxTBranch held pushesHeld
        calls "CallsBoxTBranchShared" boxTBranch sharedHolder (pushesDefault sharedHolder)
        calls "CallsBoxTCastHeld" boxTCast held pushesHeld
        calls "CallsBoxTCastSpan" boxTCast span (fun il -> il.Emit (OpCodes.Call, spanEmpty))
        calls "CallsBoxTCastUnshared" boxTCast unsharedHolder (pushesDefault unsharedHolder)
        calls "CallsBoxTUnboxHeld" boxTUnbox held pushesHeld
        calls "CallsBoxTUnboxUnshared" boxTUnbox unsharedHolder (pushesDefault unsharedHolder)
        // Code over `Holder<string>` is shared, and sees `Holder<__Canon>`. The JIT decides its cast
        // to an interface with no type arguments all the same.
        calls "CallsBoxTCastShared" boxTCast sharedHolder (pushesDefault sharedHolder)
        // But not its cast to a class, nor its unbox, unless it inlines the callee into this caller,
        // which sees the type as it is; whether it does is up to the JIT. Nor its cast to a generic
        // interface, unless `Holder<__Canon>` implements it, which PawPrint cannot tell.
        calls "CallsBoxTCastObjectShared" boxTCastObject sharedHolder (pushesDefault sharedHolder)
        calls "CallsBoxTCastEquatableShared" boxTCastEquatable sharedHolder (pushesDefault sharedHolder)
        calls "CallsBoxTUnboxShared" boxTUnbox sharedHolder (pushesDefault sharedHolder)
        calls "CallsBoxHeldCastTUnshared" boxHeldCastT typeof<int> ignore
        calls "CallsBoxTUnboxNoInliningShared" boxTUnboxNoInlining sharedHolder (pushesDefault sharedHolder)

        calls "CallsBoxTCastUnboxAbsentShared" boxTCastUnboxAbsentNoInlining sharedHolder (pushesDefault sharedHolder)

        calls "CallsBoxTUnboxNoInliningUnshared" boxTUnboxNoInlining unsharedHolder (pushesDefault unsharedHolder)
        calls "CallsBoxHeldCastTShared" boxHeldCastT typeof<string> ignore

        boxes.CreateType () |> ignore<Type>

        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    /// Each scenario returns its own code on failure and 0 on success, so a disagreement names
    /// the scenario.
    let private driverSource : string =
        """
using System;

public static class Driver
{
    private static int Expect(Func<int> call, int expected, int ifWrong)
    {
        try
        {
            return call() == expected ? 0 : ifWrong;
        }
        catch (Exception)
        {
            return ifWrong + 1;
        }
    }

    // CoreCLR's message is InvalidProgramException's default.
    private static int ExpectRefused(Func<int> call, int ifReturned, int ifWrong)
    {
        try
        {
            call();
            return ifReturned;
        }
        catch (InvalidProgramException e)
        {
            return e.Message == new InvalidProgramException().Message ? 0 : ifWrong;
        }
    }

    private static int Run()
    {
        int r;

        if ((r = Expect(Boxes.BoxBrtrue, 2, 1)) != 0) return r;
        if ((r = Expect(Boxes.BoxBrfalse, 1, 3)) != 0) return r;
        if ((r = Expect(Boxes.BoxSpanBrtrue, 2, 5)) != 0) return r;
        if ((r = Expect(Boxes.BoxIsinstObjectBrtrue, 2, 7)) != 0) return r;
        if ((r = Expect(Boxes.BoxIsinstMarkerBrtrue, 2, 9)) != 0) return r;
        if ((r = Expect(Boxes.BoxIsinstPlainBrtrue, 1, 11)) != 0) return r;
        if ((r = Expect(Boxes.BoxSpanIsinstMarkerBrfalse, 2, 13)) != 0) return r;
        if ((r = Expect(Boxes.BoxUnboxSame, 7, 15)) != 0) return r;
        if ((r = Expect(Boxes.BoxIsinstSameUnboxSame, 7, 17)) != 0) return r;

        if ((r = ExpectRefused(Boxes.BoxPop, 19, 20)) != 0) return r;
        if ((r = ExpectRefused(Boxes.BoxIsinstPop, 21, 22)) != 0) return r;
        if ((r = ExpectRefused(Boxes.BoxIsinstLdnullCgtun, 23, 24)) != 0) return r;
        if ((r = ExpectRefused(Boxes.BoxUnboxOther, 25, 26)) != 0) return r;
        if ((r = ExpectRefused(Boxes.BoxIsinstObjectUnboxSame, 27, 28)) != 0) return r;
        if ((r = ExpectRefused(Boxes.BoxIsinstNullableBrtrue, 29, 30)) != 0) return r;
        if ((r = ExpectRefused(() => Boxes.BoxBranchLandedOn(false), 31, 32)) != 0) return r;
        if ((r = ExpectRefused(Boxes.BoxBranchInTry, 53, 54)) != 0) return r;

        if ((r = ExpectRefused(Boxes.CallsBoxTPopHeld, 33, 34)) != 0) return r;
        if ((r = Expect(Boxes.CallsBoxTPopInt, 3, 35)) != 0) return r;
        if ((r = Expect(Boxes.CallsBoxTBranchHeld, 2, 37)) != 0) return r;
        if ((r = Expect(Boxes.CallsBoxTBranchShared, 2, 39)) != 0) return r;
        if ((r = Expect(Boxes.CallsBoxTCastHeld, 2, 41)) != 0) return r;
        if ((r = Expect(Boxes.CallsBoxTCastSpan, 1, 43)) != 0) return r;
        if ((r = Expect(Boxes.CallsBoxTCastUnshared, 1, 45)) != 0) return r;
        if ((r = Expect(Boxes.CallsBoxTUnboxHeld, 4, 47)) != 0) return r;
        if ((r = Expect(Boxes.CallsBoxTUnboxUnshared, 4, 49)) != 0) return r;
        if ((r = Expect(Boxes.CallsBoxTCastShared, 1, 51)) != 0) return r;
        if ((r = Expect(Boxes.CallsBoxHeldCastTUnshared, 1, 55)) != 0) return r;
        if ((r = ExpectRefused(Boxes.CallsBoxTUnboxNoInliningShared, 57, 58)) != 0) return r;
        if ((r = Expect(Boxes.CallsBoxTUnboxNoInliningUnshared, 4, 59)) != 0) return r;
        if ((r = ExpectRefused(Boxes.CallsBoxTCastUnboxAbsentShared, 61, 62)) != 0) return r;

        return 0;
    }

    public static int Main(string[] args)
    {
        return Run();
    }
}
"""

    [<Test>]
    let ``a box of a byref-like type folds or is refused as on the real runtime`` () : unit =
        FabricatedGuest.run "ByRefLikeBox" (fabricate ()) "ByRefLikeBoxDriver" driverSource 0

    /// A driver that calls `callee`, exiting 0 where the JIT folds its box and 1 where it refuses it.
    let private refusalDriver (callee : string) (folded : int) : string =
        $$"""
using System;

public static class Driver
{
    public static int Main(string[] args)
    {
        try
        {
            return Boxes.{{callee}}() == {{folded}} ? 0 : 2;
        }
        catch (InvalidProgramException)
        {
            return 1;
        }
    }
}
"""

    /// Where whether the JIT folds a box turns on whether it inlines the method, on what it decides
    /// for a canonical form PawPrint does not build, or on its refusing a metadata operand, which
    /// PawPrint does not validate, PawPrint stops rather than guess. Under `inliningDecides`, the
    /// real runtime may fold the box or refuse it; otherwise it refuses it.
    [<TestCase("CallsBoxTCastObjectShared", 2, "inlines", true)>]
    [<TestCase("CallsBoxTUnboxShared", 4, "inlines", true)>]
    [<TestCase("CallsBoxHeldCastTShared", 1, "inlines", true)>]
    [<TestCase("CallsBoxTCastEquatableShared", 1, "canonical form", true)>]
    [<TestCase("BoxIsinstVoidBrtrue", 1, "System.Void", false)>]
    let ``PawPrint refuses to guess whether the JIT folds a box``
        (callee : string)
        (folded : int)
        (reason : string)
        (inliningDecides : bool)
        =
        let onHost, onPawPrint =
            FabricatedGuest.runOnBoth "ByRefLikeBox" (fabricate ()) $"%s{callee}Driver" (refusalDriver callee folded)

        match onHost with
        | RealRuntimeResult.NormalExit 0 when inliningDecides -> ()
        | RealRuntimeResult.NormalExit 1 -> ()
        | other -> failwith $"real runtime did not exit as expected: %O{other}"

        match onPawPrint with
        | FabricatedOutcome.Failed e ->
            let rec messages (e : exn) : string list =
                if isNull e then
                    []
                else
                    e.Message :: messages e.InnerException

            messages e
            |> List.exists (fun message -> message.Contains reason)
            |> shouldEqual true
        | FabricatedOutcome.Exited code -> failwith $"PawPrint ran the guest to exit code %d{code}"
