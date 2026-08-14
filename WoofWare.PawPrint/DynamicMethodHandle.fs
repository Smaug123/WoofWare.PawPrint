namespace WoofWare.PawPrint

/// The identity of a method minted at runtime by `Reflection.Emit` rather than read out of any
/// assembly's metadata: CoreCLR's `DynamicMethodDesc`, which is exactly what
/// `MethodDesc::IsNoMetadata()` answers `true` for.
///
/// The payload is deliberately nothing but the registry id. Two `DynamicMethod`s built with the
/// same name, the same signature and the same module are *different methods*, and CoreCLR's
/// identity for one is its `DynamicMethodDesc`'s address: distinct for any two methods that are
/// live at once, though `DynamicMethodTable::GetDynamicMethod` (dynamicmethod.cpp:218) recycles a
/// desc off its free list once the method it belonged to has died, so an address can be reused
/// across generations. PawPrint never frees, so a monotone counter is the faithful projection of
/// that. Either way nothing descriptive may participate in equality here, or two distinct dynamic
/// methods would collide in the registry's maps. What the method is called, and what its signature
/// says, live in `MethodHandleRegistry.DynamicMethods` keyed by this.
///
/// Lives in its own file, ahead of `NativeIntSource`, rather than beside the registry that mints
/// it: `FunctionPointerTarget.Dynamic` names one, and a function pointer is a far more primitive
/// notion than the handle registry.
type DynamicMethodHandle =
    private
        {
            RegistryId : int64
        }

    member this.GetRegistryId () : int64 = this.RegistryId

    override this.ToString () : string = $"dynamic method #%d{this.RegistryId}"

[<RequireQualifiedAccess>]
module DynamicMethodHandle =
    /// Mint the handle for the given registry id. Only `MethodHandleRegistry.mintDynamicMethod`
    /// should call this: the id must be one that registry has allocated, or the handle names
    /// nothing and every lookup through it returns `None`.
    let ofRegistryId (registryId : int64) : DynamicMethodHandle =
        {
            RegistryId = registryId
        }
