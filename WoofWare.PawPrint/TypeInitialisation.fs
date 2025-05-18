namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata

/// Represents the state of a type's initialization in the CLI
type TypeInitState =
    | InProgress of ThreadId // Being initialized by this thread
    | Initialized

/// Tracks the initialization state of types across assemblies. The AssemblyName in the key is where the type comes from.
type TypeInitTable = ImmutableDictionary<TypeDefinitionHandle * AssemblyName, TypeInitState>

[<RequireQualifiedAccess>]
module TypeInitTable =
    let beginInitialising
        (thread : ThreadId)
        (typeDef : TypeDefinitionHandle * AssemblyName)
        (t : TypeInitTable)
        : TypeInitTable
        =
        match t.TryGetValue typeDef with
        | false, _ -> t.Add (typeDef, TypeInitState.InProgress thread)
        | true, v -> failwith "Logic error: tried initialising a type which has already started initialising"

    let markInitialised
        (thread : ThreadId)
        (typeDef : TypeDefinitionHandle * AssemblyName)
        (t : TypeInitTable)
        : TypeInitTable
        =
        match t.TryGetValue typeDef with
        | false, _ -> failwith "Logic error: completing initialisation of a type which never started initialising"
        | true, TypeInitState.Initialized ->
            failwith "Logic error: completing initialisation of a type which has already finished initialising"
        | true, TypeInitState.InProgress thread2 ->
            if thread <> thread2 then
                failwith
                    "Logic error: completed initialisation of a type on a different thread to the one which started it!"
            else
                t.SetItem (typeDef, TypeInitState.Initialized)
