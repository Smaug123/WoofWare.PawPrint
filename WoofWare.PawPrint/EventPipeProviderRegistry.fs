namespace WoofWare.PawPrint

type EventPipeProviderInfo =
    {
        Name : string
    }

type EventPipeEventInfo =
    {
        Provider : EventPipeProviderHandle
        EventID : uint32
        Keywords : int64
        EventVersion : uint32
        Level : uint32
    }

/// PawPrint deliberately does not store provider callback function pointers or contexts:
/// no PawPrint code path invokes the registered callback today, and not storing it makes
/// "we accidentally invoked a callback" a compile error rather than a silent no-op. When a
/// session-enablement model is added, callback storage and the invocation site need to land
/// together in the same change.
type EventPipeProviderRegistry =
    private
        {
            NextProviderId : int64
            NextEventId : int64
            /// Keyed by handle; insertion order is the natural order of the int64 ids.
            Providers : Map<EventPipeProviderHandle, EventPipeProviderInfo>
            Events : Map<EventPipeEventHandle, EventPipeEventInfo>
        }

[<RequireQualifiedAccess>]
module EventPipeProviderRegistry =
    let empty () : EventPipeProviderRegistry =
        {
            NextProviderId = 1L
            NextEventId = 1L
            Providers = Map.empty
            Events = Map.empty
        }

    let allocateProvider
        (name : string)
        (registry : EventPipeProviderRegistry)
        : EventPipeProviderHandle * EventPipeProviderRegistry
        =
        let handle = EventPipeProviderHandle.EventPipeProviderHandle registry.NextProviderId

        let info : EventPipeProviderInfo =
            {
                Name = name
            }

        let registry =
            { registry with
                NextProviderId = registry.NextProviderId + 1L
                Providers = registry.Providers |> Map.add handle info
            }

        handle, registry

    let lookupProvider
        (handle : EventPipeProviderHandle)
        (registry : EventPipeProviderRegistry)
        : EventPipeProviderInfo option
        =
        registry.Providers |> Map.tryFind handle

    /// Mirrors the native `config_find_provider_by_name` linear scan: returns the first
    /// provider with the given name in insertion order. Native EventPipe permits multiple
    /// providers with the same name and `GetProvider` always returns the earliest-registered.
    let findFirstByName (name : string) (registry : EventPipeProviderRegistry) : EventPipeProviderHandle option =
        registry.Providers
        |> Map.toSeq
        |> Seq.tryFind (fun (_, info) -> info.Name = name)
        |> Option.map fst

    let allocateEvent
        (info : EventPipeEventInfo)
        (registry : EventPipeProviderRegistry)
        : EventPipeEventHandle * EventPipeProviderRegistry
        =
        let handle = EventPipeEventHandle.EventPipeEventHandle registry.NextEventId

        let registry =
            { registry with
                NextEventId = registry.NextEventId + 1L
                Events = registry.Events |> Map.add handle info
            }

        handle, registry

    let lookupEvent (handle : EventPipeEventHandle) (registry : EventPipeProviderRegistry) : EventPipeEventInfo option =
        registry.Events |> Map.tryFind handle

    /// Removes a provider and all events that referenced it.
    let freeProvider
        (handle : EventPipeProviderHandle)
        (registry : EventPipeProviderRegistry)
        : EventPipeProviderRegistry
        =
        if not (registry.Providers |> Map.containsKey handle) then
            failwith $"Tried to free unknown EventPipe provider %O{handle}"

        let remainingEvents =
            registry.Events |> Map.filter (fun _ info -> info.Provider <> handle)

        { registry with
            Providers = registry.Providers |> Map.remove handle
            Events = remainingEvents
        }
