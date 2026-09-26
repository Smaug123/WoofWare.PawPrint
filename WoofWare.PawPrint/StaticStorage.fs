namespace WoofWare.PawPrint

open System.Collections.Immutable

type StaticStorage =
    {
        /// Nested rather than keyed on a flat `(owner, type, field)` triple so that the
        /// per-owner partition is a value in its own right: a thread-static's slots are
        /// separate sub-dictionaries, not entries that merely happen to differ in one
        /// component of a compound key.
        Slots :
            ImmutableDictionary<
                StaticOwner,
                ImmutableDictionary<ConcreteTypeHandle, Map<ComparableFieldDefinitionHandle, CliType>>
             >
    }

[<RequireQualifiedAccess>]
module StaticStorage =
    let empty : StaticStorage =
        {
            Slots = ImmutableDictionary.Empty
        }

    let get
        (owner : StaticOwner)
        (ty : ConcreteTypeHandle)
        (field : ComparableFieldDefinitionHandle)
        (storage : StaticStorage)
        : CliType option
        =
        match storage.Slots.TryGetValue owner with
        | false, _ -> None
        | true, ownerSlots ->

        match ownerSlots.TryGetValue ty with
        | false, _ -> None
        | true, fields -> Map.tryFind field fields

    let set
        (owner : StaticOwner)
        (ty : ConcreteTypeHandle)
        (field : ComparableFieldDefinitionHandle)
        (value : CliType)
        (storage : StaticStorage)
        : StaticStorage
        =
        let ownerSlots =
            match storage.Slots.TryGetValue owner with
            | false, _ -> ImmutableDictionary.Empty
            | true, v -> v

        let ownerSlots =
            match ownerSlots.TryGetValue ty with
            | false, _ -> ownerSlots.Add (ty, Map.ofList [ field, value ])
            | true, v -> ownerSlots.SetItem (ty, Map.add field value v)

        {
            Slots = storage.Slots.SetItem (owner, ownerSlots)
        }

[<RequireQualifiedAccess>]
module StaticStorageObserver =
    let writtenSlots
        (storage : StaticStorage)
        : (StaticOwner * ConcreteTypeHandle * ComparableFieldDefinitionHandle * CliType) list
        =
        [
            for KeyValue (owner, ownerSlots) in storage.Slots do
                for KeyValue (ty, fields) in ownerSlots do
                    for KeyValue (field, value) in fields do
                        yield owner, ty, field, value
        ]
        // `ImmutableDictionary` enumerates in hash-code order, which for today's key types agrees
        // with `compare`; the order promised in the signature must not rest on that.
        |> List.sortBy (fun (owner, ty, field, _) -> owner, ty, field)
