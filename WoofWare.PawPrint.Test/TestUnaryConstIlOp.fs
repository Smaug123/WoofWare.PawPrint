namespace WoofWare.PawPrint.Test

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestUnaryConstIlOp =

    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private baseClassTypes : BaseClassTypes<DumpedAssembly> =
        Corelib.getBaseTypes corelib

    let private loadedAssemblies : LoadedAssemblies =
        LoadedAssemblies.ofAssemblies [ corelib ]

    let private concreteTypes : AllConcreteTypes =
        Corelib.concretizeAll loadedAssemblies baseClassTypes AllConcreteTypes.Empty

    type private SignedBranchKind =
        | Blt
        | Bgt

    type private ComparisonRelation =
        | Less
        | Equal
        | Greater

    type private SignedBranchCase =
        {
            Kind : SignedBranchKind
            Value1 : int32
            Value2 : int32
            Offset : int32
        }

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    let private initialState (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory) : IlMachineState =
        { IlMachineState.initial loggerFactory ImmutableArray.Empty corelib with
            ConcreteTypes = concreteTypes
        }

    let private methodWithUnaryConst
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (op : UnaryConstIlOp)
        (state : IlMachineState)
        : IlMachineState * MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        let objectToString =
            baseClassTypes.Object.Methods
            |> List.find (fun method -> method.Name = "ToString" && method.Parameters.IsEmpty)

        let state, signature =
            TypeMethodSignature.map
                state
                (fun state ty ->
                    IlMachineState.concretizeType
                        loggerFactory
                        baseClassTypes
                        state
                        corelib.Name
                        ImmutableArray.Empty
                        ImmutableArray.Empty
                        ty
                )
                objectToString.Signature

        let op = IlOp.UnaryConst op

        let instructions : MethodInstructions<ConcreteTypeHandle> =
            { MethodInstructions.onlyRet () with
                Instructions = [ op, 0 ]
                Locations = Map.empty |> Map.add 0 op
            }

        let method =
            objectToString
            |> MethodInfo.mapTypeGenerics (fun _ -> failwith "System.Object::ToString is not type-generic")
            |> MethodInfo.mapMethodGenerics (fun _ _ -> failwith "System.Object::ToString is not method-generic")
            |> MethodInfo.setMethodVars (MethodBody.Il instructions) signature

        state, method

    let private stateWithSignedBranch
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (op : UnaryConstIlOp)
        (value1 : int32)
        (value2 : int32)
        : IlMachineState * ThreadId
        =
        let state, method =
            initialState loggerFactory |> methodWithUnaryConst loggerFactory op

        let methodState =
            match
                MethodState.Empty
                    state.ConcreteTypes
                    baseClassTypes
                    state._LoadedAssemblies
                    corelib
                    method
                    ImmutableArray.Empty
                    (ImmutableArray.Create (CliType.ObjectRef None))
                    None
            with
            | Ok methodState -> methodState
            | Error missing ->
                failwith $"Unexpected missing assembly references creating signed-branch test frame: %O{missing}"

        let thread = ThreadId.ThreadId 0

        let state =
            { state with
                ThreadState =
                    Map.empty
                    |> Map.add thread (ThreadState.New (CpuId 0) (OsThreadId 1u) methodState)
            }
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 value1) thread
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 value2) thread

        state, thread

    let private genComparablePair : Gen<int32 * int32> =
        let generated =
            gen {
                let! pivot = Gen.choose (-1_000_000, 1_000_000)
                let! delta = Gen.choose (1, 1_000)

                let! relation =
                    Gen.elements
                        [
                            ComparisonRelation.Less
                            ComparisonRelation.Equal
                            ComparisonRelation.Greater
                        ]

                match relation with
                | ComparisonRelation.Less -> return int32 pivot, int32 (pivot + delta)
                | ComparisonRelation.Equal -> return int32 pivot, int32 pivot
                | ComparisonRelation.Greater -> return int32 (pivot + delta), int32 pivot
            }

        let edges =
            Gen.elements
                [
                    Int32.MinValue, Int32.MaxValue
                    Int32.MaxValue, Int32.MinValue
                    Int32.MinValue, Int32.MinValue
                    Int32.MaxValue, Int32.MaxValue
                    -1, 0
                    0, -1
                    0, 0
                ]

        Gen.frequency [ 8, generated ; 1, edges ]

    let private genNonZeroOffset : Gen<int32> =
        Gen.oneof [ Gen.choose (-20, -1) ; Gen.choose (1, 20) ] |> Gen.map int32

    let private genSignedBranchCase : Gen<SignedBranchCase> =
        gen {
            let! kind = Gen.elements [ SignedBranchKind.Blt ; SignedBranchKind.Bgt ]
            let! value1, value2 = genComparablePair
            let! offset = genNonZeroOffset

            return
                {
                    Kind = kind
                    Value1 = value1
                    Value2 = value2
                    Offset = offset
                }
        }

    let private opOfCase (case : SignedBranchCase) : UnaryConstIlOp =
        match case.Kind with
        | SignedBranchKind.Blt -> UnaryConstIlOp.Blt case.Offset
        | SignedBranchKind.Bgt -> UnaryConstIlOp.Bgt case.Offset

    let private isTaken (case : SignedBranchCase) : bool =
        match case.Kind with
        | SignedBranchKind.Blt -> case.Value1 < case.Value2
        | SignedBranchKind.Bgt -> case.Value1 > case.Value2

    [<Test>]
    let ``Blt and Bgt branch relative to next instruction iff signed comparison holds`` () : unit =
        let mutable bltTaken = 0
        let mutable bltNotTaken = 0
        let mutable bgtTaken = 0
        let mutable bgtNotTaken = 0

        let property (case : SignedBranchCase) : unit =
            let op = opOfCase case
            let taken = isTaken case

            match case.Kind, taken with
            | SignedBranchKind.Blt, true -> bltTaken <- bltTaken + 1
            | SignedBranchKind.Blt, false -> bltNotTaken <- bltNotTaken + 1
            | SignedBranchKind.Bgt, true -> bgtTaken <- bgtTaken + 1
            | SignedBranchKind.Bgt, false -> bgtNotTaken <- bgtNotTaken + 1

            let _, loggerFactory = LoggerFactory.makeTest ()
            use _loggerFactoryResource = loggerFactory

            let state, thread = stateWithSignedBranch loggerFactory op case.Value1 case.Value2

            let state, whatWeDid = UnaryConstIlOp.execute state thread op

            whatWeDid |> shouldEqual WhatWeDid.Executed

            let instructionSize = IlOp.NumberOfBytes (IlOp.UnaryConst op)
            let branchDelta = if taken then int case.Offset else 0
            let expectedPc = instructionSize + branchDelta

            let methodState = state.ThreadState.[thread].MethodState
            methodState.IlOpIndex |> shouldEqual expectedPc
            methodState.EvaluationStack.Values |> shouldEqual []

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genSignedBranchCase) property)

        if bltTaken = 0 || bltNotTaken = 0 || bgtTaken = 0 || bgtNotTaken = 0 then
            failwith
                $"generator missed required regimes: bltTaken=%d{bltTaken}, bltNotTaken=%d{bltNotTaken}, bgtTaken=%d{bgtTaken}, bgtNotTaken=%d{bgtNotTaken}"

    /// Push two float operands onto a fresh evaluation stack, queue `op` as the only instruction,
    /// then dispatch it. Returns the post-dispatch PC and the residual eval stack so callers can
    /// assert both branch direction (PC) and that the operands were consumed.
    let private runFloatBranch (op : UnaryConstIlOp) (value1 : float) (value2 : float) : int32 * EvalStackValue list =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let state, method =
            initialState loggerFactory |> methodWithUnaryConst loggerFactory op

        let methodState =
            match
                MethodState.Empty
                    state.ConcreteTypes
                    baseClassTypes
                    state._LoadedAssemblies
                    corelib
                    method
                    ImmutableArray.Empty
                    (ImmutableArray.Create (CliType.ObjectRef None))
                    None
            with
            | Ok methodState -> methodState
            | Error missing ->
                failwith $"Unexpected missing assembly references creating float-branch test frame: %O{missing}"

        let thread = ThreadId.ThreadId 0

        let state =
            { state with
                ThreadState =
                    Map.empty
                    |> Map.add thread (ThreadState.New (CpuId 0) (OsThreadId 1u) methodState)
            }
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Float value1) thread
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Float value2) thread

        let state, whatWeDid = UnaryConstIlOp.execute state thread op

        whatWeDid |> shouldEqual WhatWeDid.Executed

        let methodState = state.ThreadState.[thread].MethodState
        methodState.IlOpIndex, methodState.EvaluationStack.Values

    /// Expected PC after executing `op` (the only instruction in its method) given whether the
    /// branch is taken.
    let private expectedPcAfter (op : UnaryConstIlOp) (taken : bool) (offset : int32) : int32 =
        let instructionSize = IlOp.NumberOfBytes (IlOp.UnaryConst op)
        instructionSize + (if taken then offset else 0)

    // Ordered float branches: ECMA-335 specifies that NaN comparisons report "branch not taken",
    // matching .NET's default IEEE semantics for `<`/`<=`/`>`/`>=` (all return false for NaN).
    // ±0 compare equal under IEEE; ±Inf orders sit at the extremes of the real-valued reals.
    let private nanF : float = System.Double.NaN
    let private pInfF : float = System.Double.PositiveInfinity
    let private nInfF : float = System.Double.NegativeInfinity
    let private subnormalF : float = System.Double.Epsilon

    /// `(name, op, factory)` quartet: each branch op under test together with the helper that
    /// reconstructs it for a given short-form offset. Long-form Ble/Bge are exercised in their
    /// own test; the short-form (Blt_s/Ble_s/Bgt_s/Bge_s) versions are the ones that were
    /// `failwith "todo"` for Float × Float before this change.
    let private floatTakenCases (v1 : float) (v2 : float) : (string * bool) list =
        [ "blt", v1 < v2 ; "ble", v1 <= v2 ; "bgt", v1 > v2 ; "bge", v1 >= v2 ]

    [<Test>]
    let ``Blt_s/Ble_s/Bgt_s/Bge_s on Float × Float follow IEEE ordered semantics`` () : unit =
        let offset = 7

        let assertCase (name : string) (op : UnaryConstIlOp) (expectedTaken : bool) (v1 : float) (v2 : float) : unit =
            let pc, stack = runFloatBranch op v1 v2

            stack |> shouldEqual []

            let expectedPc = expectedPcAfter op expectedTaken offset

            if pc <> expectedPc then
                failwith
                    $"%s{name} (%f{v1}, %f{v2}): expected pc=%d{expectedPc} (taken=%b{expectedTaken}), got pc=%d{pc}"

        let cases : (float * float) list =
            [
                // Ordinary ordered finite reals.
                1.0, 2.0
                2.0, 1.0
                1.0, 1.0
                -3.5, -3.5
                -3.5, 3.5
                // ±0 equality.
                0.0, -0.0
                -0.0, 0.0
                // ±Inf at the extremes.
                pInfF, 1.0
                1.0, pInfF
                nInfF, 1.0
                1.0, nInfF
                pInfF, nInfF
                nInfF, pInfF
                pInfF, pInfF
                nInfF, nInfF
                // Subnormals are real, finite values; ordering is well-defined.
                subnormalF, 1.0
                1.0, subnormalF
                0.0, subnormalF
                // NaN: every ordered comparison must report "not taken" (false), in all
                // directions and against any other regime (including itself).
                nanF, 1.0
                1.0, nanF
                nanF, nanF
                nanF, pInfF
                nInfF, nanF
                nanF, 0.0
            ]

        for v1, v2 in cases do
            for name, taken in floatTakenCases v1 v2 do
                let op =
                    match name with
                    | "blt" -> UnaryConstIlOp.Blt_s (int8 offset)
                    | "ble" -> UnaryConstIlOp.Ble_s (int8 offset)
                    | "bgt" -> UnaryConstIlOp.Bgt_s (int8 offset)
                    | "bge" -> UnaryConstIlOp.Bge_s (int8 offset)
                    | other -> failwith $"unexpected float-branch case: %s{other}"

                assertCase name op taken v1 v2

    [<Test>]
    let ``Ble/Bge (long form) on Float × Float follow IEEE ordered semantics`` () : unit =
        // The long-form ops were also previously `failwith "todo"` on Float; cover them here
        // with a smaller but still NaN-inclusive battery. `Blt`/`Bgt` (long form) already
        // delegated to `clt`/`cgt` before this change, but we exercise them too for symmetry.
        let offset = 13

        let assertCase (name : string) (op : UnaryConstIlOp) (expectedTaken : bool) (v1 : float) (v2 : float) : unit =
            let pc, stack = runFloatBranch op v1 v2

            stack |> shouldEqual []

            let expectedPc = expectedPcAfter op expectedTaken offset

            if pc <> expectedPc then
                failwith
                    $"%s{name} (%f{v1}, %f{v2}): expected pc=%d{expectedPc} (taken=%b{expectedTaken}), got pc=%d{pc}"

        let cases : (float * float) list =
            [
                1.0, 2.0
                2.0, 1.0
                1.0, 1.0
                0.0, -0.0
                pInfF, nInfF
                nanF, 1.0
                1.0, nanF
                nanF, nanF
            ]

        for v1, v2 in cases do
            assertCase "blt" (UnaryConstIlOp.Blt offset) (v1 < v2) v1 v2
            assertCase "ble" (UnaryConstIlOp.Ble offset) (v1 <= v2) v1 v2
            assertCase "bgt" (UnaryConstIlOp.Bgt offset) (v1 > v2) v1 v2
            assertCase "bge" (UnaryConstIlOp.Bge offset) (v1 >= v2) v1 v2
