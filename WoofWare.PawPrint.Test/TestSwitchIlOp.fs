namespace WoofWare.PawPrint.Test

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
module TestSwitchIlOp =

    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private baseClassTypes : BaseClassTypes<DumpedAssembly> =
        Corelib.getBaseTypes corelib

    let private loadedAssemblies : ImmutableDictionary<string, DumpedAssembly> =
        ImmutableDictionary.CreateRange [ KeyValuePair (corelib.Name.FullName, corelib) ]

    let private concreteTypes : AllConcreteTypes =
        Corelib.concretizeAll loadedAssemblies baseClassTypes AllConcreteTypes.Empty

    type private SwitchCaseKind =
        | InRangeNonZero
        | InRangeZero
        | NegativeIndex
        | TooLargeIndex
        | EmptyTable

    type private SwitchCase =
        {
            Targets : int32 list
            Index : int32
            ExpectedDelta : int32 option
            Kind : SwitchCaseKind
        }

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    let private genTargetDelta : Gen<int32> = Gen.choose (-20, 20) |> Gen.map int32

    let private genNonZeroTargetDelta : Gen<int32> =
        Gen.oneof [ Gen.choose (-20, -1) ; Gen.choose (1, 20) ] |> Gen.map int32

    let private genTargetsWithSelected (length : int) (index : int) (selected : int32) : Gen<int32 list> =
        Gen.listOfLength length genTargetDelta
        |> Gen.map (List.mapi (fun i target -> if i = index then selected else target))

    let private genInRangeCase (kind : SwitchCaseKind) (selectedTarget : Gen<int32>) : Gen<SwitchCase> =
        gen {
            let! length = Gen.choose (1, 8)
            let! index = Gen.choose (0, length - 1)
            let! selected = selectedTarget
            let! targets = genTargetsWithSelected length index selected

            return
                {
                    Targets = targets
                    Index = int32 index
                    ExpectedDelta = Some selected
                    Kind = kind
                }
        }

    let private genNegativeIndexCase : Gen<SwitchCase> =
        gen {
            let! length = Gen.choose (1, 8)
            let! targets = Gen.listOfLength length genTargetDelta
            let! index = Gen.choose (-8, -1)

            return
                {
                    Targets = targets
                    Index = int32 index
                    ExpectedDelta = None
                    Kind = SwitchCaseKind.NegativeIndex
                }
        }

    let private genTooLargeIndexCase : Gen<SwitchCase> =
        gen {
            let! length = Gen.choose (1, 8)
            let! targets = Gen.listOfLength length genTargetDelta
            let! overshoot = Gen.choose (0, 8)

            return
                {
                    Targets = targets
                    Index = int32 (length + overshoot)
                    ExpectedDelta = None
                    Kind = SwitchCaseKind.TooLargeIndex
                }
        }

    let private genEmptyTableCase : Gen<SwitchCase> =
        gen {
            let! index = Gen.choose (-8, 8)

            return
                {
                    Targets = []
                    Index = int32 index
                    ExpectedDelta = None
                    Kind = SwitchCaseKind.EmptyTable
                }
        }

    let private genSwitchCase : Gen<SwitchCase> =
        Gen.oneof
            [
                genInRangeCase SwitchCaseKind.InRangeNonZero genNonZeroTargetDelta
                genInRangeCase SwitchCaseKind.InRangeZero (Gen.constant 0)
                genNegativeIndexCase
                genTooLargeIndexCase
                genEmptyTableCase
            ]

    let private initialState (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory) : IlMachineState =
        { IlMachineState.initial loggerFactory ImmutableArray.Empty corelib with
            ConcreteTypes = concreteTypes
        }

    let private methodWithSwitch
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (targets : ImmutableArray<int32>)
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
                    IlMachineTypeResolution.concretizeType
                        loggerFactory
                        baseClassTypes
                        state
                        corelib.Name
                        ImmutableArray.Empty
                        ImmutableArray.Empty
                        ty
                )
                objectToString.Signature

        let op = IlOp.Switch targets

        let instructions : MethodInstructions<ConcreteTypeHandle> =
            { MethodInstructions.onlyRet () with
                Instructions = [ op, 0 ]
                Locations = Map.empty |> Map.add 0 op
            }

        let method =
            objectToString
            |> MethodInfo.mapTypeGenerics (fun _ -> failwith "System.Object::ToString is not type-generic")
            |> MethodInfo.mapMethodGenerics (fun _ _ -> failwith "System.Object::ToString is not method-generic")
            |> MethodInfo.setMethodVars (Some instructions) signature

        state, method

    let private stateWithSwitch
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (targets : ImmutableArray<int32>)
        (index : int32)
        : IlMachineState * ThreadId
        =
        let state, method =
            initialState loggerFactory |> methodWithSwitch loggerFactory targets

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
                failwith $"Unexpected missing assembly references creating switch test frame: %O{missing}"

        let thread = ThreadId.ThreadId 0

        let state =
            { state with
                ThreadState = Map.empty |> Map.add thread (ThreadState.New methodState)
            }
            |> IlMachineThreadState.pushToEvalStack' (EvalStackValue.Int32 index) thread

        state, thread

    let private switchSize (targets : int32 list) : int = 1 + 4 + targets.Length * 4

    [<Test>]
    let ``switch jumps relative to the instruction after its target table`` () : unit =
        let mutable inRangeNonZeroCases = 0
        let mutable inRangeZeroCases = 0
        let mutable negativeIndexCases = 0
        let mutable tooLargeIndexCases = 0
        let mutable emptyTableCases = 0

        let property (case : SwitchCase) : bool =
            match case.Kind with
            | SwitchCaseKind.InRangeNonZero -> inRangeNonZeroCases <- inRangeNonZeroCases + 1
            | SwitchCaseKind.InRangeZero -> inRangeZeroCases <- inRangeZeroCases + 1
            | SwitchCaseKind.NegativeIndex -> negativeIndexCases <- negativeIndexCases + 1
            | SwitchCaseKind.TooLargeIndex -> tooLargeIndexCases <- tooLargeIndexCases + 1
            | SwitchCaseKind.EmptyTable -> emptyTableCases <- emptyTableCases + 1

            let targets = ImmutableArray.CreateRange case.Targets
            let _, loggerFactory = LoggerFactory.makeTest ()

            use _loggerFactoryResource = loggerFactory

            let state, thread = stateWithSwitch loggerFactory targets case.Index

            let state, whatWeDid = SwitchIlOp.execute state thread targets

            whatWeDid |> shouldEqual WhatWeDid.Executed

            let expectedPc =
                match case.ExpectedDelta with
                | None -> switchSize case.Targets
                | Some delta -> switchSize case.Targets + int delta

            state.ThreadState.[thread].MethodState.IlOpIndex |> shouldEqual expectedPc
            state.ThreadState.[thread].MethodState.EvaluationStack.Values |> shouldEqual []

            true

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genSwitchCase) property)

        if
            inRangeNonZeroCases = 0
            || inRangeZeroCases = 0
            || negativeIndexCases = 0
            || tooLargeIndexCases = 0
            || emptyTableCases = 0
        then
            failwith
                $"generator missed required regimes: inRangeNonZero=%d{inRangeNonZeroCases}, inRangeZero=%d{inRangeZeroCases}, negative=%d{negativeIndexCases}, tooLarge=%d{tooLargeIndexCases}, empty=%d{emptyTableCases}"
