namespace WoofWare.PawPrint.Test

open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
module TestFaultHandlers =

    // The factory is intentionally not disposed: the DumpedAssembly keeps the logger.
    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        use stream = File.OpenRead corelibPath
        Assembly.read loggerFactory (Some corelibPath) stream

    let private bct : BaseClassTypes<DumpedAssembly> = Corelib.getBaseTypes corelib

    let private loaded : ImmutableDictionary<string, DumpedAssembly> =
        ImmutableDictionary.CreateRange [ KeyValuePair (corelib.Name.FullName, corelib) ]

    let private concreteTypes : AllConcreteTypes =
        Corelib.concretizeAll loaded bct AllConcreteTypes.Empty

    let private initialState (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory) : IlMachineState =
        { IlMachineState.initial loggerFactory ImmutableArray.Empty corelib with
            ConcreteTypes = concreteTypes
        }

    let private methodWithRegions
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (regions : ExceptionRegion seq)
        (state : IlMachineState)
        : IlMachineState * WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        let objectToString =
            bct.Object.Methods
            |> List.find (fun method -> method.Name = "ToString" && method.Parameters.IsEmpty)

        let state, signature =
            TypeMethodSignature.map
                state
                (fun state ty ->
                    IlMachineState.concretizeType
                        loggerFactory
                        bct
                        state
                        corelib.Name
                        ImmutableArray.Empty
                        ImmutableArray.Empty
                        ty
                )
                objectToString.Signature

        let ops : (IlOp * int) list =
            [
                IlOp.Nullary NullaryIlOp.Nop, 0
                IlOp.Nullary NullaryIlOp.Nop, 1
                IlOp.Nullary NullaryIlOp.Endfinally, 10
                IlOp.Nullary NullaryIlOp.Ret, 11
            ]

        let instructions : MethodInstructions<ConcreteTypeHandle> =
            {
                Instructions = ops
                Locations = ops |> List.map (fun (op, offset) -> offset, op) |> Map.ofList
                LocalsInit = false
                LocalVars = None
                ExceptionRegions = regions |> ImmutableArray.CreateRange
            }

        let method =
            objectToString
            |> MethodInfo.mapTypeGenerics (fun _ -> failwith "System.Object::ToString is not type-generic")
            |> MethodInfo.mapMethodGenerics (fun _ _ -> failwith "System.Object::ToString is not method-generic")
            |> MethodInfo.setMethodVars (Some instructions) signature

        state, method

    let private stateWithMethod
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (regions : ExceptionRegion seq)
        : IlMachineState * ThreadId
        =
        let state, method =
            initialState loggerFactory |> methodWithRegions loggerFactory regions

        let methodState =
            match
                MethodState.Empty
                    state.ConcreteTypes
                    bct
                    state._LoadedAssemblies
                    corelib
                    method
                    ImmutableArray.Empty
                    (ImmutableArray.Create (CliType.ObjectRef None))
                    None
            with
            | Ok methodState -> methodState
            | Error missing ->
                failwith $"Unexpected missing assembly references creating fault-handler test frame: %O{missing}"

        let thread = ThreadId.ThreadId 0

        { state with
            ThreadState = Map.empty |> Map.add thread (ThreadState.New corelib.Name methodState)
        },
        thread

    let private faultOffset : ExceptionOffset =
        {
            TryOffset = 0
            TryLength = 4
            HandlerOffset = 10
            HandlerLength = 1
        }

    [<Test>]
    let ``Fault handler is entered as exceptional cleanup`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let state, thread =
            stateWithMethod loggerFactory [ ExceptionRegion.Fault faultOffset ]

        let exceptionObject = ManagedHeapAddress 42

        let objectHandle =
            AllConcreteTypes.getRequiredNonGenericHandle concreteTypes bct.Object

        let methodState = state.ThreadState.[thread].MethodState

        let state, handler =
            ExceptionDispatching.findExceptionHandler
                loggerFactory
                bct
                state
                corelib
                1
                objectHandle
                methodState.ExecutingMethod

        let handler =
            match handler with
            | Some (ExceptionRegion.Fault offset, isCleanup) ->
                isCleanup |> shouldEqual true
                ExceptionRegion.Fault offset
            | other -> failwith $"Expected fault handler, got %O{other}"

        let state =
            state |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 123) thread

        let methodState =
            { state.ThreadState.[thread].MethodState with
                PendingPrefix =
                    { PrefixState.empty with
                        Tail = true
                    }
            }

        let threadState =
            ThreadState.setFrame state.ThreadState.[thread].ActiveMethodState methodState state.ThreadState.[thread]

        let state =
            { state with
                ThreadState = state.ThreadState |> Map.add thread threadState
            }

        let cliException : CliException<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> =
            {
                ExceptionObject = exceptionObject
                StackTrace = []
            }

        let state =
            ExceptionDispatching.enterHandler thread methodState threadState state cliException handler

        let methodState = state.ThreadState.[thread].MethodState
        methodState.IlOpIndex |> shouldEqual faultOffset.HandlerOffset
        methodState.EvaluationStack.Values |> shouldEqual []
        methodState.PendingPrefix |> shouldEqual PrefixState.empty

        match methodState.ExceptionContinuation with
        | Some (ExceptionContinuation.PropagatingException actual) ->
            actual.ExceptionObject |> shouldEqual exceptionObject
            actual.StackTrace |> shouldEqual []
        | other -> failwith $"Expected propagating exception continuation, got %O{other}"

    [<Test>]
    let ``Leave schedules finally but not fault`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let finallyOffset : ExceptionOffset =
            {
                TryOffset = 0
                TryLength = 4
                HandlerOffset = 20
                HandlerLength = 1
            }

        let _state, method =
            initialState loggerFactory
            |> methodWithRegions
                loggerFactory
                [ ExceptionRegion.Fault faultOffset ; ExceptionRegion.Finally finallyOffset ]

        let blocks = ExceptionHandling.findFinallyBlocksToRun 1 8 method

        blocks |> shouldEqual [ finallyOffset ]
