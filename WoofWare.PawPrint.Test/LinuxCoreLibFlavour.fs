namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// Running a guest against the pinned linux-x64 framework (`$DOTNET_LINUX_FRAMEWORK_DIR`), for
/// the fixtures that need the Linux CoreLib flavour rather than the host's. See
/// `TestLinuxCoreLibFlavour` for why the two differ.
module LinuxCoreLibFlavour =

    let assy = typeof<RunResult>.Assembly

    let linuxFrameworkDir : string option =
        match Environment.GetEnvironmentVariable "DOTNET_LINUX_FRAMEWORK_DIR" with
        | null
        | "" -> None
        | dir -> Some dir

    /// The pinned framework only exists inside the Nix devshell, so a plain `dotnet test` in a
    /// non-Nix checkout skips rather than fails. Everything these tests assert is about a
    /// foreign CoreLib flavour, so there is nothing meaningful to fall back to.
    let requireLinuxFramework () : string =
        match linuxFrameworkDir with
        | Some dir -> dir
        | None ->
            Assert.Ignore
                "DOTNET_LINUX_FRAMEWORK_DIR is unset; run under `nix develop` to exercise the linux-x64 CoreLib."
            // Assert.Ignore throws, so this is unreachable; it exists to satisfy the type checker.
            failwith "unreachable: Assert.Ignore did not throw"

    let corelibPath (frameworkDir : string) : string =
        Path.Combine (frameworkDir, "System.Private.CoreLib.dll")

    /// Runtime dirs with the pinned linux-x64 framework first. Assembly binding takes the first
    /// directory that has a `<simple name>.dll`, so every framework assembly resolves from the
    /// pack; the host's dirs stay on the list only to bind anything the pack does not carry.
    let runtimeDirsPreferringLinux (frameworkDir : string) : ImmutableArray<string> =
        seq {
            yield frameworkDir
            yield! DotnetRuntime.SelectForDll assy.Location
        }
        |> ImmutableArray.CreateRange

    let runOnLinuxFramework (name : string) (frameworkDir : string) (source : string) : IlMachineState =
        let image = Roslyn.compile [ source ]
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        use peImage = new MemoryStream (image)

        let outcome =
            Program.run loggerFactory (Some name) peImage (HostConfig.Default (runtimeDirsPreferringLinux frameworkDir))

        match outcome with
        | RunOutcome.NormalExit (terminalState, _) -> terminalState
        | other -> failwith $"Expected the guest to exit normally on the linux-x64 CoreLib, got %O{other}"

    let loadedCorelibPath (terminalState : IlMachineState) : string =
        let corelibs =
            terminalState._LoadedAssemblies.DefinitionNames
            |> Seq.choose terminalState._LoadedAssemblies.TryByDefinitionName
            |> Seq.filter (fun loaded -> loaded.Name.Name = "System.Private.CoreLib")
            |> Seq.toList

        match corelibs with
        | [ corelib ] ->
            corelib.OriginalPath
            |> Option.defaultWith (fun () ->
                failwith "Loaded CoreLib has no OriginalPath; cannot tell where it came from"
            )
        | [] -> failwith "No System.Private.CoreLib was loaded"
        | many ->
            let paths = many |> List.map (fun c -> string<string option> c.OriginalPath)

            failwith
                $"""Expected exactly one loaded System.Private.CoreLib, got %d{many.Length}: %s{String.Join (", ", paths)}"""
