namespace WoofWare.PawPrint.Test

open System
open System.Reflection.Emit
open FsCheck
open FsCheck.FSharp
open NUnit.Framework
open WoofWare.PawPrint

/// An int32 on one side of a comparison and a native int on the other (ECMA-335 Table III.4),
/// measured against the host CLR through `DynamicMethod`s that emit the opcode itself.
///
/// The compare opcodes and the unsigned branches widen the int32 differently on CoreCLR, and the
/// unsigned branches widen a `ldc.i4` constant differently from any other int32, so each of the
/// three is its own oracle here: see `EvalStackValueComparisons.unsignedBranch`.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestMixedWidthComparisons =

    /// `ldarg.0; ldarg.1; op; ret` over `(nativeint, int32)`.
    let private hostCompare (op : OpCode) : Func<nativeint, int32, bool> =
        let dm =
            DynamicMethod ($"compare_%s{op.Name}", typeof<bool>, [| typeof<nativeint> ; typeof<int32> |])

        let il = dm.GetILGenerator ()
        il.Emit OpCodes.Ldarg_0
        il.Emit OpCodes.Ldarg_1
        il.Emit op
        il.Emit OpCodes.Ret
        dm.CreateDelegate typeof<Func<nativeint, int32, bool>> :?> Func<nativeint, int32, bool>

    /// `ldarg.0; ldarg.1; op; ret` over `(int32, nativeint)`.
    let private hostCompareReversed (op : OpCode) : Func<int32, nativeint, bool> =
        let dm =
            DynamicMethod ($"compareReversed_%s{op.Name}", typeof<bool>, [| typeof<int32> ; typeof<nativeint> |])

        let il = dm.GetILGenerator ()
        il.Emit OpCodes.Ldarg_0
        il.Emit OpCodes.Ldarg_1
        il.Emit op
        il.Emit OpCodes.Ret
        dm.CreateDelegate typeof<Func<int32, nativeint, bool>> :?> Func<int32, nativeint, bool>

    /// Returns whether the branch was taken, over operands that are both arguments, so the JIT
    /// sees no constant.
    let private hostBranch (op : OpCode) : Func<nativeint, int32, bool> =
        let dm =
            DynamicMethod ($"branch_%s{op.Name}", typeof<bool>, [| typeof<nativeint> ; typeof<int32> |])

        let il = dm.GetILGenerator ()
        let taken = il.DefineLabel ()
        il.Emit OpCodes.Ldarg_0
        il.Emit OpCodes.Ldarg_1
        il.Emit (op, taken)
        il.Emit OpCodes.Ldc_I4_0
        il.Emit OpCodes.Ret
        il.MarkLabel taken
        il.Emit OpCodes.Ldc_I4_1
        il.Emit OpCodes.Ret
        dm.CreateDelegate typeof<Func<nativeint, int32, bool>> :?> Func<nativeint, int32, bool>

    let private hostBranchReversed (op : OpCode) : Func<int32, nativeint, bool> =
        let dm =
            DynamicMethod ($"branchReversed_%s{op.Name}", typeof<bool>, [| typeof<int32> ; typeof<nativeint> |])

        let il = dm.GetILGenerator ()
        let taken = il.DefineLabel ()
        il.Emit OpCodes.Ldarg_0
        il.Emit OpCodes.Ldarg_1
        il.Emit (op, taken)
        il.Emit OpCodes.Ldc_I4_0
        il.Emit OpCodes.Ret
        il.MarkLabel taken
        il.Emit OpCodes.Ldc_I4_1
        il.Emit OpCodes.Ret
        dm.CreateDelegate typeof<Func<int32, nativeint, bool>> :?> Func<int32, nativeint, bool>

    /// As `hostBranch`, but the int32 is `ldc.i4 constant`, which the JIT widens differently from
    /// an argument.
    let private hostBranchConstant (op : OpCode) (constant : int32) : Func<nativeint, bool> =
        let dm =
            DynamicMethod ($"branchConstant_%s{op.Name}", typeof<bool>, [| typeof<nativeint> |])

        let il = dm.GetILGenerator ()
        let taken = il.DefineLabel ()
        il.Emit OpCodes.Ldarg_0
        il.Emit (OpCodes.Ldc_I4, constant)
        il.Emit (op, taken)
        il.Emit OpCodes.Ldc_I4_0
        il.Emit OpCodes.Ret
        il.MarkLabel taken
        il.Emit OpCodes.Ldc_I4_1
        il.Emit OpCodes.Ret
        dm.CreateDelegate typeof<Func<nativeint, bool>> :?> Func<nativeint, bool>

    let private hostBranchConstantReversed (op : OpCode) (constant : int32) : Func<nativeint, bool> =
        let dm =
            DynamicMethod ($"branchConstantReversed_%s{op.Name}", typeof<bool>, [| typeof<nativeint> |])

        let il = dm.GetILGenerator ()
        let taken = il.DefineLabel ()
        il.Emit (OpCodes.Ldc_I4, constant)
        il.Emit OpCodes.Ldarg_0
        il.Emit (op, taken)
        il.Emit OpCodes.Ldc_I4_0
        il.Emit OpCodes.Ret
        il.MarkLabel taken
        il.Emit OpCodes.Ldc_I4_1
        il.Emit OpCodes.Ret
        dm.CreateDelegate typeof<Func<nativeint, bool>> :?> Func<nativeint, bool>

    type private Operands =
        {
            Native : int64
            Int : int32
        }

    let private genInt32 : Gen<int32> =
        Gen.frequency
            [
                1, Gen.elements [ -1 ; 0 ; 1 ; -2 ; Int32.MinValue ; Int32.MaxValue ]
                2, Gen.choose (Int32.MinValue, Int32.MaxValue)
            ]

    let private genInt64 : Gen<int64> =
        gen {
            let! high = Gen.choose (Int32.MinValue, Int32.MaxValue)
            let! low = Gen.choose (Int32.MinValue, Int32.MaxValue)
            return (int64 high <<< 32) ||| int64 (uint32 low)
        }

    /// The native int is chosen around the int32's two widenings as often as at random, because
    /// that is where sign extension and zero extension tell apart.
    let private genOperands : Gen<Operands> =
        gen {
            let! i = genInt32
            let signExtended = int64 i
            let zeroExtended = int64 (uint32 i)

            let! native =
                Gen.frequency
                    [
                        3,
                        Gen.elements
                            [
                                signExtended
                                zeroExtended
                                signExtended - 1L
                                signExtended + 1L
                                zeroExtended - 1L
                                zeroExtended + 1L
                            ]
                        1,
                        Gen.elements
                            [
                                0L
                                -1L
                                1L
                                0x1_0000_0000L
                                0x7FFF_FFFFL
                                0x8000_0000L
                                Int64.MinValue
                                Int64.MaxValue
                            ]
                        2, genInt64
                    ]

            return
                {
                    Native = native
                    Int = i
                }
        }

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 400

    let private nativeOf (n : int64) : EvalStackValue =
        EvalStackValue.NativeInt (NativeIntSource.Verbatim n)

    let private int32Of (i : int32) : EvalStackValue =
        EvalStackValue.Int32 (Int32Source.Verbatim i)

    /// The compare opcodes and the signed branches, each with PawPrint's function for it.
    let private compareOpcodes : (OpCode * (EvalStackValue -> EvalStackValue -> bool)) list =
        [
            OpCodes.Ceq, EvalStackValueComparisons.ceq PointerHashState.empty
            OpCodes.Cgt, EvalStackValueComparisons.cgt
            OpCodes.Clt, EvalStackValueComparisons.clt
            OpCodes.Cgt_Un, EvalStackValueComparisons.cgtUn
            OpCodes.Clt_Un, EvalStackValueComparisons.cltUn
        ]

    let private signedBranches : (OpCode * (EvalStackValue -> EvalStackValue -> bool)) list =
        [
            OpCodes.Beq, EvalStackValueComparisons.ceq PointerHashState.empty
            OpCodes.Bgt, EvalStackValueComparisons.cgt
            OpCodes.Bge, EvalStackValueComparisons.cge
            OpCodes.Blt, EvalStackValueComparisons.clt
            OpCodes.Ble, EvalStackValueComparisons.cle
        ]

    let private bneUn (var1 : EvalStackValue) (var2 : EvalStackValue) : bool =
        match EvalStackValueComparisons.bneUnDeferred PointerHashState.empty var1 var2 with
        | CeqOutcome.Decided eq -> not eq
        | CeqOutcome.NeedsByteLocation (left, right, diagnostic) ->
            failwith $"bne.un deferred a non-byref pair: %O{left} vs %O{right} (%s{diagnostic})"

    let private unsignedBranches : (OpCode * (EvalStackValue -> EvalStackValue -> bool)) list =
        [
            OpCodes.Bne_Un, bneUn
            OpCodes.Bgt_Un, EvalStackValueComparisons.bgtUn
            OpCodes.Bge_Un, EvalStackValueComparisons.bgeUn
            OpCodes.Blt_Un, EvalStackValueComparisons.bltUn
            OpCodes.Ble_Un, EvalStackValueComparisons.bleUn
        ]

    let private opcodeNames (ops : (OpCode * _) list) : obj[] seq =
        ops |> Seq.map (fun (op, _) -> [| box op.Name |])

    let private lookup (ops : (OpCode * (EvalStackValue -> EvalStackValue -> bool)) list) (name : string) =
        ops |> List.find (fun (op, _) -> op.Name = name)

    let private compareOpcodeNames : obj[] seq = opcodeNames compareOpcodes
    let private signedBranchNames : obj[] seq = opcodeNames signedBranches
    let private unsignedBranchNames : obj[] seq = opcodeNames unsignedBranches

    [<TestCaseSource(nameof compareOpcodeNames)>]
    let ``compare opcodes agree with the host in both operand orders`` (name : string) : unit =
        let op, pawPrint = lookup compareOpcodes name
        let host = hostCompare op
        let hostReversed = hostCompareReversed op

        let property (operands : Operands) : bool =
            let native = operands.Native
            let i = operands.Int

            pawPrint (nativeOf native) (int32Of i) = host.Invoke (nativeint native, i)
            && pawPrint (int32Of i) (nativeOf native) = hostReversed.Invoke (i, nativeint native)

        Check.One (config, Prop.forAll (Arb.fromGen genOperands) property)

    [<TestCaseSource(nameof signedBranchNames)>]
    let ``signed branches agree with the host whether or not the int32 is a constant`` (name : string) : unit =
        let op, pawPrint = lookup signedBranches name
        let host = hostBranch op
        let hostReversed = hostBranchReversed op

        let property (operands : Operands) : bool =
            let native = operands.Native
            let i = operands.Int
            let expected = host.Invoke (nativeint native, i)
            let expectedReversed = hostReversed.Invoke (i, nativeint native)

            (hostBranchConstant op i).Invoke (nativeint native) = expected
            && (hostBranchConstantReversed op i).Invoke (nativeint native) = expectedReversed
            && pawPrint (nativeOf native) (int32Of i) = expected
            && pawPrint (int32Of i) (nativeOf native) = expectedReversed

        Check.One (config, Prop.forAll (Arb.fromGen genOperands) property)

    /// PawPrint's answer, or `None` for the refusal `unsignedBranch` documents.
    let private tryUnsignedBranch
        (pawPrint : EvalStackValue -> EvalStackValue -> bool)
        (var1 : EvalStackValue)
        (var2 : EvalStackValue)
        : bool option
        =
        try
            Some (pawPrint var1 var2)
        with e when e.Message.StartsWith ("refusing", StringComparison.Ordinal) ->
            None

    [<TestCaseSource(nameof unsignedBranchNames)>]
    let ``unsigned branches agree with the host, and refuse exactly when the host's answer depends on the int32 being a constant``
        (name : string)
        : unit
        =
        let op, pawPrint = lookup unsignedBranches name
        let host = hostBranch op
        let hostReversed = hostBranchReversed op

        let agrees (actual : bool option) (asArgument : bool) (asConstant : bool) : bool =
            match actual with
            | Some answer -> answer = asArgument && answer = asConstant
            | None -> asArgument <> asConstant

        let property (operands : Operands) : bool =
            let native = operands.Native
            let i = operands.Int

            agrees
                (tryUnsignedBranch pawPrint (nativeOf native) (int32Of i))
                (host.Invoke (nativeint native, i))
                ((hostBranchConstant op i).Invoke (nativeint native))
            && agrees
                (tryUnsignedBranch pawPrint (int32Of i) (nativeOf native))
                (hostReversed.Invoke (i, nativeint native))
                ((hostBranchConstantReversed op i).Invoke (nativeint native))

        Check.One (config, Prop.forAll (Arb.fromGen genOperands) property)

    [<Test>]
    let ``the host does widen an int32 argument and an int32 constant differently in an unsigned branch`` () : unit =
        // Without this the refusal branch of the property above could be vacuous: this is the
        // measurement the refusal exists for, so it is pinned here as a plain fact.
        let asArgument = (hostBranch OpCodes.Bgt_Un).Invoke (nativeint 0x1_0000_0000L, -1)

        let asConstant =
            (hostBranchConstant OpCodes.Bgt_Un -1).Invoke (nativeint 0x1_0000_0000L)

        Assert.That (asArgument, Is.True)
        Assert.That (asConstant, Is.False)

        match tryUnsignedBranch EvalStackValueComparisons.bgtUn (nativeOf 0x1_0000_0000L) (int32Of -1) with
        | None -> ()
        | Some answer -> failwith $"expected a refusal, got %b{answer}"
