namespace WoofWare.PawPrint

open System.Collections.Immutable

/// A .NET runtime whose host contract PawPrint implements: the allowlist of supported runtimes.
///
/// The managed BCL is loaded from the guest's own assemblies, so the only runtime-specific
/// behaviour PawPrint itself supplies is the native/extern boundary (see the `Native/` and
/// `ExternImplementations/` directories). A case of this type names one version of that
/// boundary.
///
/// Which case a run uses is not configured: it is a function of the CoreLib the run actually
/// loaded, read by `EmulatedRuntime.ofCoreLib` from that image's `AssemblyVersion` major. The
/// CoreLib is the one `Program.beginStartup` resolves, which is `BaseClassTypes.Corelib`, so
/// anything holding the base class types can ask which runtime it is serving. A CoreLib whose
/// major is not in this allowlist is refused at resolution; see `UnsupportedRuntimeException`.
///
/// Admission is by major alone: any servicing or preview build of a supported major runs.
/// Which exact build our validation was measured against is `EmulatedRuntime.pin`, checked by
/// the `TestEmulatedRuntime` drift test rather than at load time.
[<RequireQualifiedAccess>]
type EmulatedRuntime =
    /// .NET 10: a CoreLib whose `AssemblyVersion` major is 10.
    | Net10

/// A runtime build's identity: its CoreLib's `AssemblyInformationalVersion` with the SemVer build
/// metadata (a `+` and everything after it) removed, e.g. `10.0.7-servicing.26217.108`.
///
/// Kept as the exact text, never parsed into a `System.Version`: a preview build's label (as in
/// `11.0.0-preview.7.25380.108`) is part of the identity, and `System.Version` cannot hold it. The
/// label is present even on a released servicing build, whose `FrameworkDescription` omits it.
type RuntimeBuild =
    private
    | RuntimeBuild of string

    /// The build identity as text, without build metadata.
    member this.Text : string =
        match this with
        | RuntimeBuild s -> s

    override this.ToString () : string = this.Text

    /// The build identity carried by an `AssemblyInformationalVersion` value: everything before
    /// the first `+`, which begins the SemVer build metadata (on .NET, an internal build commit).
    static member OfInformationalVersion (informationalVersion : string) : RuntimeBuild =
        let text =
            match informationalVersion.IndexOf '+' with
            | -1 -> informationalVersion
            | plus -> informationalVersion.Substring (0, plus)

        if text.Length = 0 then
            failwith $"informational version %s{informationalVersion} has no version before its build metadata"

        RuntimeBuild text

/// What a supported runtime's native surface was written and validated against.
///
/// Descriptive metadata about our validation, not a source of guest-observable behaviour: a guest
/// running on a different servicing build of the same major is admitted and behaves exactly as
/// it would on this one.
type RuntimePin =
    {
        /// The target framework moniker whose semantics this runtime reproduces, e.g. "net10.0".
        TargetFramework : string
        /// The build the Nix devshell pins, as the loaded CoreLib reports it.
        Build : RuntimeBuild
        /// The dotnet/runtime git tag the native implementations were validated against, e.g. "v10.0.7".
        SourceRef : string
        /// The full dotnet/runtime commit SHA that `SourceRef` resolves to: the public release-tag
        /// commit, i.e. the upstream source PawPrint's native code is read against and mirrors.
        ///
        /// This is NOT necessarily the commit the shipped binary was built from: `dotnet --info`
        /// (and the runtime pack's `.version`, and the build metadata of the CoreLib's
        /// informational version) can report an internal build commit that was never pushed to
        /// the public dotnet/runtime repo. We record the public, readable source commit.
        SourceCommit : string
    }

/// A CoreLib whose `AssemblyVersion` major is not one PawPrint supports.
type UnsupportedRuntime =
    {
        /// The CoreLib's definition identity, e.g. "System.Private.CoreLib, Version=11.0.0.0, ...".
        CoreLib : string
        /// Where the CoreLib was read from, if it was read from a file.
        CoreLibPath : string option
        /// The major version the CoreLib's `AssemblyVersion` states.
        FoundMajor : int
    }

[<RequireQualifiedAccess>]
module EmulatedRuntime =

    /// Every runtime PawPrint supports.
    let supported : EmulatedRuntime list = [ EmulatedRuntime.Net10 ]

    /// The CoreLib `AssemblyVersion` major that identifies `runtime`.
    let major (runtime : EmulatedRuntime) : int =
        match runtime with
        | EmulatedRuntime.Net10 -> 10

    /// The build `runtime`'s native surface was validated against.
    let pin (runtime : EmulatedRuntime) : RuntimePin =
        match runtime with
        | EmulatedRuntime.Net10 ->
            // Keep these values in step with the runtime pinned by the Nix devshell; the
            // `sync-dotnet-runtime` process establishes them.
            {
                TargetFramework = "net10.0"
                Build = RuntimeBuild.OfInformationalVersion "10.0.7-servicing.26217.108"
                SourceRef = "v10.0.7"
                SourceCommit = "7706f546bac1a99b3d891afe3591dc88c67f0cc4"
            }

    /// The supported runtime whose CoreLib states `coreLibMajor` as its `AssemblyVersion` major,
    /// or `None` if PawPrint supports no such runtime.
    let ofCoreLibMajor (coreLibMajor : int) : EmulatedRuntime option =
        supported |> List.tryFind (fun runtime -> major runtime = coreLibMajor)

    /// Which supported runtime `corelib` belongs to, keyed by its `AssemblyVersion` major, or why
    /// it is not supported.
    let classify (corelib : DumpedAssembly) : Result<EmulatedRuntime, UnsupportedRuntime> =
        let version =
            match corelib.Name.Version with
            | null -> failwith $"CoreLib %s{corelib.DefinitionFullName} has no AssemblyVersion"
            | version -> version

        match ofCoreLibMajor version.Major with
        | Some runtime -> Ok runtime
        | None ->
            Error
                {
                    CoreLib = corelib.DefinitionFullName
                    CoreLibPath = corelib.OriginalPath
                    FoundMajor = version.Major
                }

    /// The runtime a run whose CoreLib is `corelib` is serving.
    ///
    /// `Program.beginStartup` refuses an unsupported CoreLib before anything can ask this, so a
    /// failure here means the caller's CoreLib never passed through startup.
    let ofCoreLib (corelib : DumpedAssembly) : EmulatedRuntime =
        match classify corelib with
        | Ok runtime -> runtime
        | Error unsupported ->
            failwith
                $"logic error: CoreLib %s{unsupported.CoreLib} has AssemblyVersion major %d{unsupported.FoundMajor}, which no supported runtime has; Program.beginStartup refuses such a CoreLib, so this one did not come through startup"

    /// "10 (net10.0)" and so on, for messages naming what is supported.
    let describeSupported () : string =
        supported
        |> List.map (fun runtime -> $"%d{major runtime} (%s{(pin runtime).TargetFramework})")
        |> String.concat ", "

/// Raised by `Program.beginStartup` (and hence `Program.prepare`, `Program.run` and
/// `Program.runToFirstFork`) when the CoreLib the guest resolves has an `AssemblyVersion` major
/// that PawPrint does not support. No guest code has run when it is raised.
///
/// The CoreLib is the first `System.Private.CoreLib.dll` found along `DotnetRuntimeDirs`, so the
/// remedy is to put a supported framework's directory at the head of that list.
type UnsupportedRuntimeException (unsupported : UnsupportedRuntime, dotnetRuntimeDirs : ImmutableArray<string>) =
    inherit
        System.Exception (
            let from =
                match unsupported.CoreLibPath with
                | Some path -> $" (read from %s{path})"
                | None -> ""

            let dirs = dotnetRuntimeDirs |> String.concat ", "

            $"Unsupported runtime: the guest's CoreLib '%s{unsupported.CoreLib}'%s{from} is .NET major %d{unsupported.FoundMajor}, but PawPrint supports CoreLib majors: %s{EmulatedRuntime.describeSupported ()}. CoreLib binds from the first directory in DotnetRuntimeDirs that contains it: [%s{dirs}]."
        )

    /// The CoreLib that was refused, and the major it states.
    member _.Unsupported : UnsupportedRuntime = unsupported

    /// The runtime directories the CoreLib was resolved along.
    member _.DotnetRuntimeDirs : ImmutableArray<string> = dotnetRuntimeDirs
