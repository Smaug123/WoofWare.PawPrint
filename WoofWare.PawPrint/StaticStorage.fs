namespace WoofWare.PawPrint

open System.Collections.Immutable

type StaticStorage =
    {
        /// Nested rather than keyed on a flat `(owner, type, field)` triple so that the
        /// per-owner partition is a value in its own right: a thread-static's slots are
        /// separate sub-dictionaries, not entries that merely happen to differ in one
        /// component of a compound key.
        ///
        /// This is the shape the storage had while it was `IlMachineState._Statics`, kept
        /// verbatim through the move. Now that nothing outside this file can see it, it is
        /// free to change without touching a caller — which is the point of the signature
        /// file.
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
