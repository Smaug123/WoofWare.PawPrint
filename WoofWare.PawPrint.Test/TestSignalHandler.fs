namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Focused tests for the `SignalHandler` wrapper that `SignalState.Handler`
/// stores. The wrapper exists so a `MethodInfo` (whose naked structural
/// equality is unstable — its `ImmutableArray` fields and `MethodBody` payloads
/// compare by reference) can be embedded in a structurally-compared
/// `EmulatedKernel` without breaking the deterministic state-dedup contract.
/// These tests pin down:
///   * `MethodInfo.NominallyEqual` correctly identifies the same underlying
///     method even when the two `MethodInfo` records were constructed
///     independently (so `SignalState.setHandler` is idempotent in practice).
///   * Distinct methods compare unequal at both the `SignalHandler` and
///     enclosing `SignalState` layers.
///   * `GetHashCode` agrees with `Equals` for the equal-handler case.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSignalHandler =

    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private baseClassTypes : BaseClassTypes<DumpedAssembly> =
        Corelib.getBaseTypes corelib

    let private loadedAssemblies : ImmutableDictionary<string, DumpedAssembly> =
        ImmutableDictionary<string, DumpedAssembly>.Empty.Add (corelib.Name.FullName, corelib)

    let private concreteTypes : AllConcreteTypes =
        Corelib.concretizeAll loadedAssemblies baseClassTypes AllConcreteTypes.Empty

    let private baseState () : IlMachineState =
        let _, loggerFactory = LoggerFactory.makeTest ()

        { IlMachineState.initial loggerFactory ImmutableArray.Empty corelib with
            ConcreteTypes = concreteTypes
        }

    /// Look up a no-arg method by name on `System.Object` and concretize it.
    /// `GetHashCode` / `ToString` / `GetType` are the safe choices: each is
    /// uniquely named on `Object`, so the filter unambiguously picks one
    /// `MethodInfo`, and the declaring type carries no generics so
    /// concretization is a one-liner.
    let private concretizeObjectMethod
        (state : IlMachineState)
        (methodName : string)
        : IlMachineState * MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let objectType =
            corelib.TryGetTopLevelTypeDef "System" "Object"
            |> Option.defaultWith (fun () -> failwith "System.Object not found in corelib")

        let rawMethod =
            objectType.Methods
            |> List.filter (fun m -> m.Name = methodName)
            |> function
                | [ method ] -> method
                | [] -> failwith $"method %s{methodName} not found on System.Object"
                | methods ->
                    failwith $"method %s{methodName} on System.Object was ambiguous: %d{methods.Length} matches"

        let state, method, _declaringType =
            ExecutionConcretization.concretizeMethodWithTypeGenerics
                loggerFactory
                baseClassTypes
                ImmutableArray.Empty
                rawMethod
                None
                corelib.Name
                ImmutableArray.Empty
                state

        state, method

    [<Test>]
    let ``empty SignalState has no handler installed`` () : unit =
        SignalState.empty |> SignalState.handler |> shouldEqual None

    [<Test>]
    let ``setHandler then handler round-trips the installed handler`` () : unit =
        let state, method = concretizeObjectMethod (baseState ()) "GetHashCode"
        let handler = SignalHandler.ofMethodInfo method

        let installed =
            SignalState.empty |> SignalState.setHandler handler |> SignalState.handler

        match installed with
        | Some h -> h |> shouldEqual handler
        | None -> failwith "expected setHandler to install the handler"

    [<Test>]
    let ``SignalHandler equality identifies independently-constructed wrappers around the same method`` () : unit =
        // Two independent concretizations of `Object.GetHashCode` produce
        // `MethodInfo` records whose `ImmutableArray` fields differ by
        // reference but agree by nominal identity. The wrapper's
        // `MethodInfo.NominallyEqual` contract must collapse them.
        let state, methodA = concretizeObjectMethod (baseState ()) "GetHashCode"
        let _, methodB = concretizeObjectMethod state "GetHashCode"

        let handlerA = SignalHandler.ofMethodInfo methodA
        let handlerB = SignalHandler.ofMethodInfo methodB

        handlerA |> shouldEqual handlerB
        hash handlerA |> shouldEqual (hash handlerB)

    [<Test>]
    let ``SignalHandler equality distinguishes different methods`` () : unit =
        let state, getHashCode = concretizeObjectMethod (baseState ()) "GetHashCode"
        let _, toString = concretizeObjectMethod state "ToString"

        let handlerA = SignalHandler.ofMethodInfo getHashCode
        let handlerB = SignalHandler.ofMethodInfo toString

        handlerA |> shouldNotEqual handlerB

    [<Test>]
    let ``SignalHandler equals returns false against a non-SignalHandler value`` () : unit =
        let _, method = concretizeObjectMethod (baseState ()) "GetHashCode"
        let handler = SignalHandler.ofMethodInfo method

        handler.Equals (box "not a handler") |> shouldEqual false
        handler.Equals (box 42) |> shouldEqual false
        handler.Equals (null : obj) |> shouldEqual false

    [<Test>]
    let ``SignalState structural equality survives an installed handler`` () : unit =
        // Critical: `EmulatedKernel` (which embeds `SignalState`) is compared
        // structurally for deterministic state dedup. Two states built
        // independently with the same logical handler installed must compare
        // equal — otherwise dedup would split semantically-equivalent states.
        let state, methodA = concretizeObjectMethod (baseState ()) "GetHashCode"
        let _, methodB = concretizeObjectMethod state "GetHashCode"

        let stateA =
            SignalState.empty
            |> SignalState.markInitialized
            |> SignalState.setHandler (SignalHandler.ofMethodInfo methodA)

        let stateB =
            SignalState.empty
            |> SignalState.markInitialized
            |> SignalState.setHandler (SignalHandler.ofMethodInfo methodB)

        stateA |> shouldEqual stateB
        hash stateA |> shouldEqual (hash stateB)

    [<Test>]
    let ``SignalState distinguishes states whose handlers differ`` () : unit =
        let state, getHashCode = concretizeObjectMethod (baseState ()) "GetHashCode"
        let _, toString = concretizeObjectMethod state "ToString"

        let stateA =
            SignalState.empty
            |> SignalState.setHandler (SignalHandler.ofMethodInfo getHashCode)

        let stateB =
            SignalState.empty
            |> SignalState.setHandler (SignalHandler.ofMethodInfo toString)

        stateA |> shouldNotEqual stateB

    [<Test>]
    let ``setHandler is last-writer-wins`` () : unit =
        // CoreLib's `PosixSignalRegistration.Initialize` only ever calls
        // `SystemNative_SetPosixSignalHandler` once, but the underlying native
        // contract is "overwrite g_posixSignalHandler". Pin down that the
        // wrapper preserves that contract.
        let state, getHashCode = concretizeObjectMethod (baseState ()) "GetHashCode"
        let _, toString = concretizeObjectMethod state "ToString"

        let handlerA = SignalHandler.ofMethodInfo getHashCode
        let handlerB = SignalHandler.ofMethodInfo toString

        let installed =
            SignalState.empty
            |> SignalState.setHandler handlerA
            |> SignalState.setHandler handlerB
            |> SignalState.handler

        installed |> shouldEqual (Some handlerB)
