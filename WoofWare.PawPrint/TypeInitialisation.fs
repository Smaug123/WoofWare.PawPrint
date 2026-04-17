namespace WoofWare.PawPrint

open System.Collections.Immutable

/// Represents the state of a type's initialization in the CLI
type TypeInitState =
    | InProgress of ThreadId // Being initialized by this thread
    | Initialized
    /// The .cctor threw; stores the cached outer TypeInitializationException so that
    /// repeated accesses rethrow the *same* instance (matching CLR identity semantics).
    | Failed of tieAddress : ManagedHeapAddress * tieType : ConcreteTypeHandle

/// Tracks the initialization state of types across assemblies. The string in the key is the FullName of the AssemblyName where the type comes from.
// TODO: need a better solution than string here! AssemblyName didn't work, we had nonequal assembly names.
type TypeInitTable = ImmutableDictionary<ConcreteTypeHandle, TypeInitState>

[<RequireQualifiedAccess>]
module TypeInitTable =
    let tryGet (ty : ConcreteTypeHandle) (t : TypeInitTable) =
        match t.TryGetValue ty with
        | true, v -> Some v
        | false, _ -> None

    let beginInitialising (thread : ThreadId) (ty : ConcreteTypeHandle) (t : TypeInitTable) : TypeInitTable =
        match t.TryGetValue ty with
        | false, _ -> t.Add (ty, TypeInitState.InProgress thread)
        | true, v -> failwith $"Logic error: tried initialising a type which is already in state %A{v}"

    let markInitialised (thread : ThreadId) (ty : ConcreteTypeHandle) (t : TypeInitTable) : TypeInitTable =
        match t.TryGetValue ty with
        | false, _ -> failwith "Logic error: completing initialisation of a type which never started initialising"
        | true, TypeInitState.Initialized ->
            failwith "Logic error: completing initialisation of a type which has already finished initialising"
        | true, TypeInitState.Failed _ ->
            failwith "Logic error: completing initialisation of a type whose .cctor already failed"
        | true, TypeInitState.InProgress thread2 ->
            if thread <> thread2 then
                failwith
                    "Logic error: completed initialisation of a type on a different thread to the one which started it!"
            else
                t.SetItem (ty, TypeInitState.Initialized)

    let markFailed
        (thread : ThreadId)
        (ty : ConcreteTypeHandle)
        (tieAddress : ManagedHeapAddress)
        (tieType : ConcreteTypeHandle)
        (t : TypeInitTable)
        : TypeInitTable
        =
        match t.TryGetValue ty with
        | false, _ -> failwith "Logic error: failing initialisation of a type which never started initialising"
        | true, TypeInitState.Initialized ->
            failwith "Logic error: failing initialisation of a type which has already finished initialising"
        | true, TypeInitState.Failed _ ->
            failwith "Logic error: failing initialisation of a type whose .cctor already failed"
        | true, TypeInitState.InProgress thread2 ->
            if thread <> thread2 then
                failwith
                    "Logic error: failed initialisation of a type on a different thread to the one which started it!"
            else
                t.SetItem (ty, TypeInitState.Failed (tieAddress, tieType))
