namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open WoofWare.PawPrint

/// Threads for a test that drives the interpreter's thread-creation paths by
/// hand: a frame to start one on, and a guest `Thread` constructed and started.
[<RequireQualifiedAccess>]
module ThreadFixtures =

    let corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private baseClassTypes : BaseClassTypes<DumpedAssembly> =
        BaseClassTypes.ofCorelib corelib

    /// A machine on `corelib`, before `addThread` has given the leader its thread.
    let bare () : IlMachineState =
        let _, loggerFactory = LoggerFactory.makeTest ()
        IlMachineState.initial loggerFactory ImmutableArray.Empty corelib

    /// A frame on any concrete method: nothing reads its instructions, only that
    /// `addThread` or `startUnstartedThread` has something to start a thread on.
    let aFrame (state : IlMachineState) : IlMachineState * MethodState =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let objectToString =
            baseClassTypes.Object.Methods
            |> List.find (fun method -> method.Name = "ToString" && (MethodInfo.arity method = 0))

        let state, signature =
            IlMachineState.concretizeMethodSignature
                loggerFactory
                baseClassTypes
                state
                corelib.DefinitionFullName
                ImmutableArray.Empty
                ImmutableArray.Empty
                objectToString.Signature

        let method =
            objectToString
            |> MethodInfo.mapTypeGenerics (fun _ -> failwith "System.Object::ToString is not type-generic")
            |> MethodInfo.mapMethodGenerics (fun _ _ -> failwith "System.Object::ToString is not method-generic")
            |> MethodInfo.setMethodVars (MethodBody.Il (MethodInstructions.onlyRet ())) signature

        match
            MethodState.Empty
                state.TypeSystem.ConcreteTypes
                baseClassTypes
                state.TypeSystem._LoadedAssemblies
                corelib
                method
                ImmutableArray.Empty
                (ImmutableArray.Create (CliType.ObjectRef None))
                None
        with
        | Ok methodState -> state, methodState
        | Error missing -> failwith $"unexpected missing assembly references creating frame: %O{missing}"

    /// `starter` starts the constructed, unstarted `thread`, as `Thread.Start` does.
    let start (starter : ThreadId) (thread : ThreadId) (state : IlMachineState) : IlMachineState =
        let state, frame = aFrame state
        IlMachineState.startUnstartedThread starter thread frame state

    /// The guest constructs a `Thread` at `address`, and `starter` starts it.
    let constructAndStart
        (starter : ThreadId)
        (address : ManagedHeapAddress)
        (state : IlMachineState)
        : IlMachineState * ThreadId
        =
        let state, thread = IlMachineState.allocateUnstartedThread address state
        start starter thread state, thread
