namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open Microsoft.Extensions.Logging.Abstractions
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// Why the suite cannot run its guests on the framework it was asked for. Each case fails the
/// run; none of them falls back to another framework.
[<RequireQualifiedAccess>]
type FrameworkUnderTestError =
    /// `PAWPRINT_TEST_RUNTIME` is set, but not to the name of a supported runtime. The empty
    /// string is such a value: only an unset variable selects the default.
    | UnknownRuntime of value : string
    /// The runtime's framework is found only through an environment variable that the Nix devshell
    /// sets, and `variable` is unset.
    | Unlocated of runtime : EmulatedRuntime * variable : string
    /// A directory on the runtime-dir list does not exist.
    | MissingDirectory of runtime : EmulatedRuntime * directory : string
    /// No directory on the runtime-dir list holds a CoreLib, so a guest could bind none.
    | NoCoreLib of runtime : EmulatedRuntime * runtimeDirs : ImmutableArray<string>
    /// The CoreLib a run binds, or would bind, is not the selected runtime's: `found` is how
    /// `EmulatedRuntime.classify` classifies it.
    | CoreLibMismatch of
        expected : EmulatedRuntime *
        coreLib : string *
        coreLibPath : string option *
        found : Result<EmulatedRuntime, UnsupportedRuntime>

/// The framework the suite runs its guests on, checked to be the one that was selected: the first
/// CoreLib along `RuntimeDirs` classifies to `Runtime`. Only `FrameworkUnderTest.check` builds one.
type SelectedFramework =
    private
        {
            runtime : EmulatedRuntime
            runtimeDirs : ImmutableArray<string>
            coreLibPath : string
        }

    /// The runtime that was selected, and that the CoreLib along `RuntimeDirs` serves.
    member this.Runtime : EmulatedRuntime = this.runtime

    /// The `DotnetRuntimeDirs` a guest runs along.
    member this.RuntimeDirs : ImmutableArray<string> = this.runtimeDirs

    /// The CoreLib a guest binds along `RuntimeDirs`: the first directory holding one.
    member this.CoreLibPath : string = this.coreLibPath

    /// The shared framework directory (`<root>/shared/Microsoft.NETCore.App/<version>`) that holds
    /// `CoreLibPath`.
    member this.SharedFrameworkDirectory : string =
        match Path.GetDirectoryName this.coreLibPath with
        | null -> failwith $"logic error: CoreLib path %s{this.coreLibPath} has no directory"
        | dir -> dir

/// Which framework the suite's guests run on, under PawPrint and under the `RealRuntime` oracle.
///
/// `PAWPRINT_TEST_RUNTIME` names an `EmulatedRuntime` case (`Net10` or `Net11`); unset, it selects
/// `EmulatedRuntime.Net10`. A value naming no supported runtime, or a selected runtime whose
/// framework cannot be found or whose CoreLib is not that runtime's, fails every caller of
/// `runtimeDirs` and `sharedFrameworkDirectory` rather than falling back to the test host's own
/// framework.
///
/// This is the only place in the test project that locates a framework to run guests on
/// (`TestFrameworkLocatorRatchet` enforces it). Guests are still *compiled* against the test
/// host's framework, which is the oldest supported runtime, so that one image runs on every
/// supported runtime.
[<RequireQualifiedAccess>]
module FrameworkUnderTest =

    type private Marker = class end

    /// The environment variable naming the runtime under test.
    [<Literal>]
    let EnvironmentVariable = "PAWPRINT_TEST_RUNTIME"

    /// The runtime selected when `EnvironmentVariable` is unset. The test host runs on the oldest
    /// supported runtime, so this is the host's own.
    let defaultRuntime : EmulatedRuntime = EmulatedRuntime.Net10

    /// How `EnvironmentVariable` spells `runtime`.
    let name (runtime : EmulatedRuntime) : string =
        match runtime with
        | EmulatedRuntime.Net10 -> "Net10"
        | EmulatedRuntime.Net11 -> "Net11"

    /// The runtime `EnvironmentVariable`'s value selects: `None` is an unset variable. Matching is
    /// exact, so any value that is not some supported runtime's `name` is an error.
    let parse (value : string option) : Result<EmulatedRuntime, FrameworkUnderTestError> =
        match value with
        | None -> Ok defaultRuntime
        | Some value ->
            match EmulatedRuntime.supported |> List.tryFind (fun runtime -> name runtime = value) with
            | Some runtime -> Ok runtime
            | None -> Error (FrameworkUnderTestError.UnknownRuntime value)

    /// Whether `corelib` is `runtime`'s CoreLib, by `EmulatedRuntime.classify`.
    let checkServes (runtime : EmulatedRuntime) (corelib : DumpedAssembly) : Result<unit, FrameworkUnderTestError> =
        match EmulatedRuntime.classify corelib with
        | Ok found when found = runtime -> Ok ()
        | found ->
            FrameworkUnderTestError.CoreLibMismatch (runtime, corelib.DefinitionFullName, corelib.OriginalPath, found)
            |> Error

    /// `path` with every symbolic link along it resolved, so that two spellings of one directory
    /// compare equal. The devshell's `dotnet` links each framework directory to the one its own
    /// package installs, and the host is not consistent about which spelling it reports: CoreLib's
    /// location has the links resolved, another framework assembly's does not. Fails on a chain of
    /// more than 40 links, as `realpath` does (ELOOP).
    let resolveLinks (path : string) : string =
        let rec go (hops : int) (path : string) : string =
            let full = Path.TrimEndingDirectorySeparator (Path.GetFullPath path)

            match Path.GetDirectoryName full with
            | null -> full
            | parent ->
                let candidate = Path.Combine (go hops parent, Path.GetFileName full)

                match FileInfo(candidate).LinkTarget with
                | null -> candidate
                | target ->
                    if hops >= 40 then
                        failwith $"resolving %s{path} followed more than 40 symbolic links"

                    // A relative target is relative to the link's own directory.
                    match Path.GetDirectoryName candidate with
                    | null -> failwith $"logic error: %s{candidate} has no directory"
                    | linkDir -> go (hops + 1) (Path.Combine (linkDir, target))

        go 0 path

    /// `runtimeDirs` as a framework for `runtime`: every directory exists, and the CoreLib a guest
    /// binds along them (the first `System.Private.CoreLib.dll`) is `runtime`'s.
    let check
        (runtime : EmulatedRuntime)
        (runtimeDirs : ImmutableArray<string>)
        : Result<SelectedFramework, FrameworkUnderTestError>
        =
        // PawPrint's binder raises on a directory that does not exist rather than skipping it.
        match runtimeDirs |> Seq.tryFind (fun dir -> not (Directory.Exists dir)) with
        | Some missing -> Error (FrameworkUnderTestError.MissingDirectory (runtime, missing))
        | None ->

        let coreLibPath =
            runtimeDirs
            |> Seq.tryPick (fun dir ->
                let path = Path.Combine (dir, "System.Private.CoreLib.dll")
                if File.Exists path then Some path else None
            )

        match coreLibPath with
        | None -> Error (FrameworkUnderTestError.NoCoreLib (runtime, runtimeDirs))
        | Some coreLibPath ->
            Assembly.readFile NullLoggerFactory.Instance coreLibPath
            |> checkServes runtime
            |> Result.map (fun () ->
                {
                    runtime = runtime
                    runtimeDirs = runtimeDirs
                    coreLibPath = coreLibPath
                }
            )

    /// A human-readable account of `error`, naming what to change.
    let describe (error : FrameworkUnderTestError) : string =
        match error with
        | FrameworkUnderTestError.UnknownRuntime value ->
            let names = EmulatedRuntime.supported |> List.map name |> String.concat ", "

            $"%s{EnvironmentVariable} is %A{value}, which names no supported runtime. Set it to one of: %s{names}; or unset it to select %s{name defaultRuntime}."
        | FrameworkUnderTestError.Unlocated (runtime, variable) ->
            $"%s{name runtime}'s framework is named by %s{variable}, which is unset; run under `nix develop`."
        | FrameworkUnderTestError.MissingDirectory (runtime, directory) ->
            $"The runtime directories for %s{name runtime} include %s{directory}, which does not exist."
        | FrameworkUnderTestError.NoCoreLib (runtime, runtimeDirs) ->
            let dirs = runtimeDirs |> String.concat ", "
            $"No directory on %s{name runtime}'s runtime-dir list holds System.Private.CoreLib.dll: [%s{dirs}]."
        | FrameworkUnderTestError.CoreLibMismatch (expected, coreLib, coreLibPath, found) ->
            let from =
                match coreLibPath with
                | Some path -> $" (from %s{path})"
                | None -> ""

            let classified =
                match found with
                | Ok runtime -> $"is %s{name runtime}'s"
                | Error unsupported -> $"states major %d{unsupported.FoundMajor}, which no supported runtime has"

            $"The CoreLib %s{coreLib}%s{from} %s{classified}, but the framework under test is %s{name expected}."

    /// The environment variables in which the Nix devshell names `runtime`'s shared framework
    /// directories (`Microsoft.NETCore.App`, then `Microsoft.AspNetCore.App`), for a runtime the
    /// test host does not itself run on; `None` for the test host's own runtime.
    let frameworkVariables (runtime : EmulatedRuntime) : string list option =
        match runtime with
        | EmulatedRuntime.Net10 -> None
        | EmulatedRuntime.Net11 -> Some [ "DOTNET_FRAMEWORK_DIR_NET11" ; "DOTNET_ASPNETCORE_FRAMEWORK_DIR_NET11" ]

    /// Where `runtime`'s framework is looked for. Not checked; `check` does that.
    let private candidateRuntimeDirs
        (runtime : EmulatedRuntime)
        : Result<ImmutableArray<string>, FrameworkUnderTestError>
        =
        match frameworkVariables runtime with
        // The test host runs on this runtime, so its framework is the one the test assembly's
        // own runtimeconfig selects.
        | None ->
            DotnetRuntime.SelectForDll typeof<Marker>.Assembly.Location
            |> ImmutableArray.CreateRange
            |> Ok
        | Some variables ->
            let folder (dirs : Result<ImmutableArray<string>, FrameworkUnderTestError>) (variable : string) =
                dirs
                |> Result.bind (fun dirs ->
                    match Environment.GetEnvironmentVariable variable with
                    | null
                    | "" -> Error (FrameworkUnderTestError.Unlocated (runtime, variable))
                    | dir -> Ok (dirs.Add dir)
                )

            variables |> List.fold folder (Ok ImmutableArray.Empty)

    /// `runtime`'s framework, checked as `check` does, whether or not it is the one under test.
    let locate (runtime : EmulatedRuntime) : Result<SelectedFramework, FrameworkUnderTestError> =
        candidateRuntimeDirs runtime |> Result.bind (check runtime)

    let private selection : Lazy<Result<SelectedFramework, FrameworkUnderTestError>> =
        lazy
            (Environment.GetEnvironmentVariable EnvironmentVariable
             |> Option.ofObj
             |> parse
             |> Result.bind locate)

    /// The framework under test. Fails, with the reason, if the selection is invalid.
    let selected () : SelectedFramework =
        match selection.Force () with
        | Ok selected -> selected
        | Error error -> failwith $"The framework under test cannot be used: %s{describe error}"

    /// The runtime under test.
    let runtime () : EmulatedRuntime = (selected ()).Runtime

    /// One NUnit case per supported runtime, for a fixture that checks something against each
    /// runtime's own image through `requireCoreLibPathOf`.
    let everyRuntime : TestCaseData list =
        EmulatedRuntime.supported
        |> List.map (fun runtime -> TestCaseData(runtime).SetArgDisplayNames (name runtime))

    /// The environment variable in which the Nix devshell names `runtime`'s pinned dotnet/runtime
    /// source, a sparse checkout at the tag of the version `flake.nix` pins for it.
    let runtimeSourceVariable (runtime : EmulatedRuntime) : string =
        match runtime with
        | EmulatedRuntime.Net10 -> "DOTNET_RUNTIME_SRC_NET10"
        | EmulatedRuntime.Net11 -> "DOTNET_RUNTIME_SRC_NET11"

    /// The environment variable in which the Nix devshell names the managed assemblies of
    /// `runtime`'s linux-x64 runtime pack, at the version `flake.nix` pins for it.
    let linuxFrameworkVariable (runtime : EmulatedRuntime) : string =
        match runtime with
        | EmulatedRuntime.Net10 -> "DOTNET_LINUX_FRAMEWORK_DIR_NET10"
        | EmulatedRuntime.Net11 -> "DOTNET_LINUX_FRAMEWORK_DIR_NET11"

    let private devshellDirectory (variable : string) : string option =
        match Environment.GetEnvironmentVariable variable with
        | null
        | "" -> None
        | dir -> Some dir

    /// The value of `variable`, or else the calling test is ignored: these directories exist only
    /// inside the Nix devshell, so a plain `dotnet test` in a non-Nix checkout skips rather than
    /// fails.
    let private requireDevshellDirectory (variable : string) (purpose : string) : string =
        match devshellDirectory variable with
        | Some dir -> dir
        | None ->
            Assert.Ignore $"%s{variable} is unset; run under `nix develop` to %s{purpose}."
            failwith "unreachable: Assert.Ignore did not throw"

    /// The runtime under test's pinned dotnet/runtime source, or `None` outside the Nix devshell.
    let runtimeSource () : string option =
        devshellDirectory (runtimeSourceVariable (runtime ()))

    /// The runtime under test's pinned dotnet/runtime source; outside the Nix devshell, the
    /// calling test is ignored.
    let requireRuntimeSource () : string =
        requireDevshellDirectory (runtimeSourceVariable (runtime ())) "check against pinned upstream sources"

    /// The runtime under test's pinned linux-x64 framework, or `None` outside the Nix devshell.
    let linuxFrameworkDirectory () : string option =
        devshellDirectory (linuxFrameworkVariable (runtime ()))

    /// The runtime under test's pinned linux-x64 framework; outside the Nix devshell, the calling
    /// test is ignored. Nothing a test asserts about a foreign CoreLib flavour has a meaningful
    /// fallback.
    let requireLinuxFrameworkDirectory () : string =
        requireDevshellDirectory (linuxFrameworkVariable (runtime ())) "exercise the linux-x64 CoreLib"

    /// `runtime`'s CoreLib, whichever runtime is under test, for a fixture that checks a shape
    /// PawPrint must handle on every supported runtime against each runtime's own image. A runtime
    /// whose framework only the Nix devshell names ignores the calling test outside it.
    let requireCoreLibPathOf (runtime : EmulatedRuntime) : string =
        match locate runtime with
        | Ok selected -> selected.CoreLibPath
        | Error (FrameworkUnderTestError.Unlocated (_, variable)) ->
            Assert.Ignore $"%s{variable} is unset; run under `nix develop` to read %s{name runtime}'s CoreLib."
            failwith "unreachable: Assert.Ignore did not throw"
        | Error error -> failwith $"%s{name runtime}'s CoreLib cannot be read: %s{describe error}"

    /// `runtime`'s shared framework directory, whichever runtime is under test, for a fixture that
    /// measures one runtime's native libraries on this host. A runtime whose framework only the Nix
    /// devshell names ignores the calling test outside it.
    let requireSharedFrameworkDirectoryOf (runtime : EmulatedRuntime) : string =
        match locate runtime with
        | Ok selected -> selected.SharedFrameworkDirectory
        | Error (FrameworkUnderTestError.Unlocated (_, variable)) ->
            Assert.Ignore $"%s{variable} is unset; run under `nix develop` to load %s{name runtime}'s framework."
            failwith "unreachable: Assert.Ignore did not throw"
        | Error error -> failwith $"%s{name runtime}'s framework cannot be located: %s{describe error}"

    /// `runtime`'s pinned dotnet/runtime source, whichever runtime is under test; outside the Nix
    /// devshell, the calling test is ignored.
    let requireRuntimeSourceOf (runtime : EmulatedRuntime) : string =
        requireDevshellDirectory
            (runtimeSourceVariable runtime)
            $"check against %s{name runtime}'s pinned upstream sources"

    /// The `DotnetRuntimeDirs` to run a guest along. A caller may put directories ahead of these
    /// (the guest's own siblings, say); if one of them holds a CoreLib, the guest runs on that
    /// CoreLib instead, and `assertServes` is what checks it is still the runtime under test's.
    let runtimeDirs () : ImmutableArray<string> = (selected ()).RuntimeDirs

    /// The shared framework directory the `RealRuntime` oracle runs guests on.
    let sharedFrameworkDirectory () : string = (selected ()).SharedFrameworkDirectory

    /// The `Microsoft.AspNetCore.App` shared framework that a guest binds ASP.NET Core from: the
    /// first directory along `runtimeDirs` holding `Microsoft.AspNetCore.dll`. The `RealRuntime`
    /// oracle runs an ASP.NET Core guest on the same directory, so this fails unless it sits
    /// under the same `dotnet` root as `sharedFrameworkDirectory`, where the oracle's muxer
    /// looks; and it fails if no directory along `runtimeDirs` holds ASP.NET Core at all.
    let aspNetCoreFrameworkDirectory () : string =
        let selected = selected ()

        let found =
            selected.RuntimeDirs
            |> Seq.tryFind (fun dir -> File.Exists (Path.Combine (dir, "Microsoft.AspNetCore.dll")))

        match found with
        | None ->
            let dirs = selected.RuntimeDirs |> String.concat ", "

            failwith
                $"No directory on %s{name selected.Runtime}'s runtime-dir list holds Microsoft.AspNetCore.dll: [%s{dirs}]."
        | Some dir ->
            // <root>/shared/<framework name>/<version>
            let rootOf (frameworkDir : string) : string =
                frameworkDir
                |> Path.GetFullPath
                |> Path.TrimEndingDirectorySeparator
                |> Path.GetDirectoryName
                |> Path.GetDirectoryName
                |> Path.GetDirectoryName

            let frameworkName =
                dir
                |> Path.GetFullPath
                |> Path.TrimEndingDirectorySeparator
                |> Path.GetDirectoryName
                |> Path.GetFileName

            if frameworkName <> "Microsoft.AspNetCore.App" then
                failwith
                    $"The ASP.NET Core assemblies on %s{name selected.Runtime}'s runtime-dir list are in %s{dir}, which is not a Microsoft.AspNetCore.App shared framework directory."

            let expectedRoot = rootOf selected.SharedFrameworkDirectory

            if rootOf dir <> expectedRoot then
                failwith
                    $"The ASP.NET Core shared framework %s{dir} is not under the dotnet root %s{expectedRoot} that holds the framework under test, so the real-runtime oracle could not run a guest on it."

            dir

    /// The selected runtime's CoreLib, which a guest binds along `runtimeDirs ()`. A fixture that
    /// asserts a shape of the runtime's CoreLib reads this image, not `HostImage`'s.
    let coreLibPath () : string = (selected ()).CoreLibPath

    /// The `System.Private.CoreLib` that `state`'s run has loaded. Fails unless there is exactly
    /// one.
    let loadedCoreLib (state : IlMachineState) : DumpedAssembly =
        let corelibs =
            state.TypeSystem._LoadedAssemblies.DefinitionNames
            |> Seq.choose state.TypeSystem._LoadedAssemblies.TryByDefinitionName
            |> Seq.filter (fun loaded -> loaded.Name.Name = "System.Private.CoreLib")
            |> Seq.toList

        match corelibs with
        | [ corelib ] -> corelib
        | [] -> failwith "No System.Private.CoreLib was loaded"
        | many ->
            let names =
                many
                |> List.map (fun c -> $"%s{c.DefinitionFullName} (%A{c.OriginalPath})")
                |> String.concat ", "

            failwith $"Expected exactly one loaded System.Private.CoreLib, got %d{many.Length}: %s{names}"

    /// Fails unless the CoreLib `state`'s run loaded is the runtime under test's.
    let assertServes (state : IlMachineState) : unit =
        match checkServes (runtime ()) (loadedCoreLib state) with
        | Ok () -> ()
        | Error error -> failwith $"The run did not serve the framework under test: %s{describe error}"

    /// `assertServes` on the final state of `runEnd`.
    let assertOutcomeServes (runEnd : RunEnd) : unit = assertServes (RunEnd.state runEnd)
