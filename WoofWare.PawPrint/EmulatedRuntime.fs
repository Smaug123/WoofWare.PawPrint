namespace WoofWare.PawPrint

/// Describes which .NET runtime PawPrint reproduces.
///
/// The managed BCL is loaded from the guest's own assemblies, so the only runtime-specific
/// behaviour PawPrint itself supplies is the native/extern boundary (see the `Native/` and
/// `ExternImplementations/` directories). This record records *which* runtime that native code
/// was written and validated against.
///
/// It is descriptive metadata, not the source of truth for guest-observable behaviour: what the
/// guest actually does is determined by PawPrint's own code, and hence by PawPrint's own package
/// version. Treat this as "the runtime we are aiming to reproduce", not "the thing that defines
/// our behaviour".
type EmulatedRuntime =
    {
        /// The target framework moniker whose semantics this runtime reproduces, e.g. "net10.0".
        TargetFramework : string
        /// The servicing version of the emulated runtime, e.g. 10.0.6.
        Version : System.Version
        /// The dotnet/runtime git tag the native implementations were validated against, e.g. "v10.0.6".
        SourceRef : string
        /// The full dotnet/runtime commit SHA corresponding to `SourceRef`; the ground-truth identifier
        /// for the upstream source that PawPrint's native code mirrors.
        SourceCommit : string
    }

[<RequireQualifiedAccess>]
module EmulatedRuntime =

    /// .NET 10. The only runtime PawPrint currently emulates.
    ///
    /// Keep these values in step with the runtime pinned by the Nix devshell and the
    /// `../dotnet-runtime` checkout; the `sync-dotnet-runtime` process establishes both.
    let net10 : EmulatedRuntime =
        {
            TargetFramework = "net10.0"
            Version = System.Version (10, 0, 6)
            SourceRef = "v10.0.6"
            SourceCommit = "7706f546bac1a99b3d891afe3591dc88c67f0cc4"
        }

    /// The runtime PawPrint emulates in this build.
    ///
    /// PawPrint currently implements exactly one runtime's native surface. When a second is added,
    /// this is the value to dispatch on when selecting the appropriate native handler set in
    /// `NativeDispatch`; nothing in today's structure forecloses that.
    let current : EmulatedRuntime = net10
