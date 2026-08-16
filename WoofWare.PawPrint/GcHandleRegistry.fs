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
    | TypeAssociated of RuntimeTypeHandleTarget
    | GuestAllocated

type GcHandleCell =
    {
        Kind : GcHandleKind
        Owner : GcHandleOwner
        Target : ManagedHeapAddress option
        /// Secondary heap address for `GcHandleKind.Dependent` cells, recording the
        /// "dependent" object whose lifetime is tied to `Target`. `None` for every
        /// other handle kind; for `Dependent`, also `None` when the dependent slot
        /// has been explicitly cleared.
        Dependent : ManagedHeapAddress option
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
        match kind with
        | GcHandleKind.Dependent ->
            // Dependent handles must be created via `allocateDependent` so that the
            // secondary `Dependent` slot is supplied at allocation time.
            failwith "GcHandleRegistry.allocate: use allocateDependent for GcHandleKind.Dependent"
        | _ -> ()

        let handle = GcHandleAddress.GcHandleAddress registry.NextHandle

        let cell =
            {
                Kind = kind
                Owner = owner
                Target = target
                Dependent = None
            }

        let registry =
            {
                NextHandle = registry.NextHandle + 1
                Handles = registry.Handles |> Map.add handle cell
            }

        handle, registry

    /// Allocate a `GcHandleKind.Dependent` cell carrying both the weak `target` and the
    /// strong `dependent` object addresses. CoreCLR's DependentHandle keeps the dependent
    /// alive only while the target is reachable; PawPrint has no GC, so we store
    /// both slots and expose them via `target` / `dependent`.
    let allocateDependent
        (owner : GcHandleOwner)
        (target : ManagedHeapAddress option)
        (dependent : ManagedHeapAddress option)
        (registry : GcHandleRegistry)
        : GcHandleAddress * GcHandleRegistry
        =
        let handle = GcHandleAddress.GcHandleAddress registry.NextHandle

        let cell =
            {
                Kind = GcHandleKind.Dependent
                Owner = owner
                Target = target
                Dependent = dependent
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

    /// Read the `Dependent` slot of a `GcHandleKind.Dependent` cell. Fails for any
    /// other handle kind, since only DependentHandles have a meaningful dependent
    /// object.
    let dependent (handle : GcHandleAddress) (registry : GcHandleRegistry) : ManagedHeapAddress option =
        let cell = get handle registry

        match cell.Kind with
        | GcHandleKind.Dependent -> cell.Dependent
        | other -> failwith $"GcHandleRegistry.dependent: handle %O{handle} is %O{other}, not Dependent"

    /// Replace the `Dependent` slot of a `GcHandleKind.Dependent` cell. Fails for any
    /// other handle kind.
    let setDependent
        (handle : GcHandleAddress)
        (dependent : ManagedHeapAddress option)
        (registry : GcHandleRegistry)
        : GcHandleRegistry
        =
        let cell = get handle registry

        match cell.Kind with
        | GcHandleKind.Dependent ->
            { registry with
                Handles =
                    registry.Handles
                    |> Map.add
                        handle
                        { cell with
                            Dependent = dependent
                        }
            }
        | other -> failwith $"GcHandleRegistry.setDependent: handle %O{handle} is %O{other}, not Dependent"

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
