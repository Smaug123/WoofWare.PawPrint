namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open Microsoft.Extensions.Logging.Abstractions
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// Why the suite cannot run its guests on the framework it was asked for. Each case fails the
/// run; none of them falls back to another framework.
[<RequireQualifiedAccess>]
type FrameworkUnderTestError =
    /// `PAWPRINT_TEST_RUNTIME` is set, but not to the name of a supported runtime. The empty
    /// string is such a value: only an unset variable selects the default.
    | UnknownRuntime of value : string
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
/// `PAWPRINT_TEST_RUNTIME` names an `EmulatedRuntime` case (`Net10`); unset, it selects
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

    /// Where each runtime's framework is looked for. Not checked; `check` does that.
    let private candidateRuntimeDirs (runtime : EmulatedRuntime) : ImmutableArray<string> =
        match runtime with
        // The test host runs on this runtime, so its framework is the one the test assembly's
        // own runtimeconfig selects.
        | EmulatedRuntime.Net10 ->
            DotnetRuntime.SelectForDll typeof<Marker>.Assembly.Location
            |> ImmutableArray.CreateRange

    let private selection : Lazy<Result<SelectedFramework, FrameworkUnderTestError>> =
        lazy
            (Environment.GetEnvironmentVariable EnvironmentVariable
             |> Option.ofObj
             |> parse
             |> Result.bind (fun runtime -> check runtime (candidateRuntimeDirs runtime)))

    /// The framework under test. Fails, with the reason, if the selection is invalid.
    let selected () : SelectedFramework =
        match selection.Force () with
        | Ok selected -> selected
        | Error error -> failwith $"The framework under test cannot be used: %s{describe error}"

    /// The runtime under test.
    let runtime () : EmulatedRuntime = (selected ()).Runtime

    /// The `DotnetRuntimeDirs` to run a guest along. A caller may put directories ahead of these
    /// (the guest's own siblings, say); if one of them holds a CoreLib, the guest runs on that
    /// CoreLib instead, and `assertServes` is what checks it is still the runtime under test's.
    let runtimeDirs () : ImmutableArray<string> = (selected ()).RuntimeDirs

    /// The shared framework directory the `RealRuntime` oracle runs guests on.
    let sharedFrameworkDirectory () : string = (selected ()).SharedFrameworkDirectory

    /// The `System.Private.CoreLib` that `state`'s run has loaded. Fails unless there is exactly
    /// one.
    let loadedCoreLib (state : IlMachineState) : DumpedAssembly =
        let corelibs =
            state._LoadedAssemblies.DefinitionNames
            |> Seq.choose state._LoadedAssemblies.TryByDefinitionName
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

    /// `assertServes` on the final state of `outcome`.
    let assertOutcomeServes (outcome : RunOutcome) : unit =
        match outcome with
        | RunOutcome.NormalExit (state, _)
        | RunOutcome.ProcessExit (state, _)
        | RunOutcome.Aborted (state, _, _)
        | RunOutcome.SignalTerminated (state, _)
        | RunOutcome.GuestUnhandledException (state, _, _) -> assertServes state
