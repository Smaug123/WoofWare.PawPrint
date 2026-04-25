namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
type GcHandleKind =
    | Weak
    | WeakTrackResurrection
    | Normal
    | Pinned
    | Dependent

[<RequireQualifiedAccess>]
type GcHandleOwner =
    | TypeAssociated of ConcreteTypeHandle
    | GuestAllocated

type GcHandleCell =
    {
        Kind : GcHandleKind
        Owner : GcHandleOwner
        Target : ManagedHeapAddress option
    }

type GcHandleRegistry =
    private
        {
            NextHandle : int
            Handles : Map<GcHandleAddress, GcHandleCell>
        }

[<RequireQualifiedAccess>]
module GcHandleRegistry =
    let empty () : GcHandleRegistry =
        {
            NextHandle = 1
            Handles = Map.empty
        }

    let allocate
        (kind : GcHandleKind)
        (owner : GcHandleOwner)
        (target : ManagedHeapAddress option)
        (registry : GcHandleRegistry)
        : GcHandleAddress * GcHandleRegistry
        =
        let handle = GcHandleAddress.GcHandleAddress registry.NextHandle

        let cell =
            {
                Kind = kind
                Owner = owner
                Target = target
            }

        let registry =
            {
                NextHandle = registry.NextHandle + 1
                Handles = registry.Handles |> Map.add handle cell
            }

        handle, registry

    let get (handle : GcHandleAddress) (registry : GcHandleRegistry) : GcHandleCell =
        registry.Handles
        |> Map.tryFind handle
        |> Option.defaultWith (fun () -> failwith $"Unknown GC handle %O{handle}")

    let target (handle : GcHandleAddress) (registry : GcHandleRegistry) : ManagedHeapAddress option =
        (get handle registry).Target

    let setTarget
        (handle : GcHandleAddress)
        (target : ManagedHeapAddress option)
        (registry : GcHandleRegistry)
        : GcHandleRegistry
        =
        let cell = get handle registry

        { registry with
            Handles =
                registry.Handles
                |> Map.add
                    handle
                    { cell with
                        Target = target
                    }
        }

    /// Atomically replace the target if it currently equals the comparand, returning the previous target.
    let compareExchangeTarget
        (handle : GcHandleAddress)
        (value : ManagedHeapAddress option)
        (comparand : ManagedHeapAddress option)
        (registry : GcHandleRegistry)
        : ManagedHeapAddress option * GcHandleRegistry
        =
        let oldTarget = target handle registry

        let registry =
            if oldTarget = comparand then
                setTarget handle value registry
            else
                registry

        oldTarget, registry

    let free (handle : GcHandleAddress) (registry : GcHandleRegistry) : GcHandleRegistry =
        if registry.Handles |> Map.containsKey handle then
            { registry with
                Handles = registry.Handles |> Map.remove handle
            }
        else
            failwith $"Tried to free unknown GC handle %O{handle}"

    let strongRoots (registry : GcHandleRegistry) : ManagedHeapAddress list =
        registry.Handles
        |> Map.toList
        |> List.choose (fun (_handle, cell) ->
            match cell.Kind, cell.Target with
            | GcHandleKind.Normal, Some target
            | GcHandleKind.Pinned, Some target -> Some target
            | GcHandleKind.Weak, _
            | GcHandleKind.WeakTrackResurrection, _
            | GcHandleKind.Dependent, _
            | _, None -> None
        )
