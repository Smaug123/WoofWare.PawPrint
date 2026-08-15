namespace WoofWare.PawPrint.Test

open System
open System.Diagnostics
open System.IO
open System.Reflection.Metadata
open System.Reflection.PortableExecutable
open System.Runtime.InteropServices
open System.Text
open WoofWare.PawPrint

/// How a program terminated when run on the real .NET runtime.
///
/// These cases mirror the terminal `WoofWare.PawPrint.RunOutcome` cases that a real process can
/// actually reach, because the whole point of the oracle is to be comparable against PawPrint.
type RealRuntimeResult =
    /// The program terminated with this exit code: by returning from `Main`, by falling off the end
    /// of a `void` entry point, or by calling `Environment.Exit`.
    ///
    /// This is also where a guest killed by a signal lands, which `RunOutcome` would call
    /// `SignalTerminated`. `Process.ExitCode` reports a signalled child as `128 + signo`, exactly as
    /// a shell would, and that is indistinguishable from a guest that returned the same number — so
    /// a segfaulting guest is reported here as `NormalExit 139` rather than as a fault. No guest in
    /// the suite does this; a case that needs the distinction cannot be a comparison test.
    | NormalExit of exitCode : int
    /// The runtime terminated the program because an exception escaped. The payload is the
    /// runtime's own stderr report, which names the exception type and carries its stack trace.
    | UnhandledException of report : string
    /// The program called `Environment.FailFast`. The payload is the runtime's stderr report.
    | FailFast of report : string

[<RequireQualifiedAccess>]
module RealRuntime =

    /// The runtime writes this to stderr immediately before aborting on an escaped exception. It is
    /// a hardcoded literal (`SZ_UNHANDLED_EXCEPTION`, coreclr/vm/excep.cpp), not a localisable
    /// resource, so matching on it does not depend on the machine's locale.
    [<Literal>]
    let private UnhandledExceptionBanner = "Unhandled exception."

    /// As `UnhandledExceptionBanner`, for `Environment.FailFast`: `PrintToStdErrA("Process
    /// terminated.\n")` in coreclr/vm/eepolicy.cpp.
    [<Literal>]
    let private FailFastBanner = "Process terminated."

    /// A guest that neither exits nor blocks is a bug in the guest, but without a bound it hangs CI
    /// with no diagnostic at all. This only has to be larger than any legitimate guest; the
    /// slowest `sourcesPure` case is orders of magnitude under it, so no passing test's outcome
    /// depends on wall-clock time.
    let private guestTimeout : TimeSpan = TimeSpan.FromSeconds 120.0

    /// The shared framework we are ourselves running on. `Roslyn` compiles guests against exactly
    /// this directory's reference assemblies, so running them against anything else would be
    /// comparing against a runtime the guest was never built for.
    let private frameworkDirectory : string =
        // A single-file or bundled host reports no location for CoreLib, and everything below is
        // derived from this path; fail with the reason rather than with a null-deref three
        // derivations later.
        match typeof<obj>.Assembly.Location with
        | null
        | "" ->
            failwith
                "CoreLib reports no on-disk location, so the shared framework directory cannot be determined. The out-of-process oracle needs a normal (non-single-file) test host."
        | location -> Path.GetDirectoryName location

    let private frameworkVersion : string = Path.GetFileName frameworkDirectory

    /// The `dotnet` muxer. Deriving it from the framework directory rather than from `PATH` keeps
    /// the guest on the same runtime as the test host even when several SDKs are installed.
    let private muxerPath : string =
        let exeName =
            if RuntimeInformation.IsOSPlatform OSPlatform.Windows then
                "dotnet.exe"
            else
                "dotnet"

        // <root>/shared/Microsoft.NETCore.App/<version>/System.Private.CoreLib.dll
        let derivedRoot =
            frameworkDirectory
            |> Path.GetDirectoryName
            |> Path.GetDirectoryName
            |> Path.GetDirectoryName

        let candidates =
            [
                yield Path.Combine (derivedRoot, exeName)
                match Environment.GetEnvironmentVariable "DOTNET_ROOT" with
                | null
                | "" -> ()
                | root -> yield Path.Combine (root, exeName)
            ]

        match candidates |> List.tryFind File.Exists with
        | Some found -> found
        | None ->
            let tried = String.Join (", ", candidates)

            failwith
                $"Could not locate the dotnet muxer needed to run guests out of process. Tried: %s{tried}. CoreLib is at %s{typeof<obj>.Assembly.Location}."

    /// A guest is a framework-dependent console app, so it needs a runtimeconfig naming the
    /// framework to run on. Generating a minimal one rather than copying the test host's keeps the
    /// guest off the test host's own config properties (the GC heap cap, the ASP.NET Core
    /// framework reference), which a real guest process would not have. Roll-forward is disabled
    /// because the version is by construction the one the guest was compiled against, so any
    /// roll-forward would silently be a mismatch rather than a convenience.
    let private runtimeConfig : string =
        // Derived rather than hardcoded so this does not quietly start lying when the projects move
        // to a later target framework.
        let tfm =
            "net" + frameworkVersion.Substring (0, frameworkVersion.IndexOf '.') + ".0"

        $"""{{
  "runtimeOptions": {{
    "tfm": "%s{tfm}",
    "rollForward": "Disable",
    "framework": {{
      "name": "Microsoft.NETCore.App",
      "version": "%s{frameworkVersion}"
    }}
  }}
}}
"""

    /// The host locates `<name>.runtimeconfig.json` from the assembly's *file* name, so the two
    /// have to agree; use the image's own assembly name so the guest also sees the name it was
    /// compiled with.
    let private assemblyNameOf (assemblyBytes : byte array) : string =
        use stream = new MemoryStream (assemblyBytes)
        use peReader = new PEReader (stream)
        let metadata = peReader.GetMetadataReader ()
        metadata.GetString (metadata.GetAssemblyDefinition().Name)

    /// Run `exePath` to completion and classify how it terminated.
    ///
    /// We do it in a separate process so that e.g. calling `Environment.Exit` does not terminate the host.
    let private runToCompletion
        (timeout : TimeSpan)
        (exePath : string)
        (arguments : string list)
        (workingDirectory : string)
        (description : string)
        : RealRuntimeResult
        =
        let startInfo = ProcessStartInfo exePath

        for arg in arguments do
            startInfo.ArgumentList.Add arg

        // A published app runs from its own directory; match that rather than leaking the test
        // host's working directory, which varies by how the suite was invoked.
        startInfo.WorkingDirectory <- workingDirectory
        startInfo.UseShellExecute <- false
        startInfo.RedirectStandardInput <- true
        startInfo.RedirectStandardOutput <- true
        startInfo.RedirectStandardError <- true

        use proc = new Process ()
        proc.StartInfo <- startInfo

        let stdout = StringBuilder ()
        let stderr = StringBuilder ()
        // Reading asynchronously is not optional: a guest that writes more than a pipe buffer would
        // block forever if we waited for exit before draining. The two readers are separate threads,
        // and on the timeout path below we snapshot the buffers while they may still be appending,
        // so guard both rather than reason about when each reader has finished.
        let sync = obj ()

        let append (buffer : StringBuilder) (line : string) : unit =
            lock sync (fun () -> buffer.AppendLine line |> ignore)

        let snapshot (buffer : StringBuilder) : string =
            lock sync (fun () -> buffer.ToString ())

        proc.OutputDataReceived.Add (fun e ->
            if not (isNull e.Data) then
                append stdout e.Data
        )

        proc.ErrorDataReceived.Add (fun e ->
            if not (isNull e.Data) then
                append stderr e.Data
        )

        proc.Start () |> ignore
        proc.BeginOutputReadLine ()
        proc.BeginErrorReadLine ()
        // No guest reads stdin; closing it turns a guest that tries into a prompt EOF rather
        // than a hang.
        proc.StandardInput.Close ()

        if not (proc.WaitForExit (int timeout.TotalMilliseconds)) then
            try
                proc.Kill true
            with _ ->
                ()

            // Give the readers a bounded moment to drain what the guest managed to emit; that
            // output is the only clue to why it hung. Proceed regardless if the kill did not take.
            proc.WaitForExit 5_000 |> ignore

            failwith
                $"Guest %s{description} did not terminate within %g{timeout.TotalSeconds}s under the real runtime, and was killed.\nPartial stdout:\n%s{snapshot stdout}\nPartial stderr:\n%s{snapshot stderr}"

        // The parameterless overload additionally waits for the async readers to finish, so the
        // buffers are complete before we read them.
        proc.WaitForExit ()

        let exitCode = proc.ExitCode
        let outputText = snapshot stdout
        let errorText = snapshot stderr

        // Exit code alone cannot classify this: an escaped exception, a `FailFast`, and a guest
        // that merely returns 134 all exit 134 on Unix (128 + SIGABRT). Requiring a nonzero
        // exit *and* the runtime's banner keeps both a plain `return 134` and a guest that
        // prints the banner itself on the normal path. A guest that does both at once would be
        // misclassified, but it's either impossible or extremely hard to repair that.
        let result =
            if exitCode <> 0 && errorText.Contains UnhandledExceptionBanner then
                RealRuntimeResult.UnhandledException (errorText.Trim ())
            elif exitCode <> 0 && errorText.Contains FailFastBanner then
                RealRuntimeResult.FailFast (errorText.Trim ())
            else
                RealRuntimeResult.NormalExit exitCode

        // Emit the text on the calling thread so that NUnit attributes it to the correct test.
        if outputText.Length > 0 then
            Console.Out.Write outputText

        // Standard error only when it is not already the payload above. Cases that are *expected*
        // to crash are passing tests, and echoing their stack traces would fill a green run with
        // alarming output; when such a case genuinely fails, the caller prints the payload instead.
        match result with
        | RealRuntimeResult.NormalExit _ ->
            if errorText.Length > 0 then
                Console.Error.Write errorText
        | RealRuntimeResult.UnhandledException _
        | RealRuntimeResult.FailFast _ -> ()

        result

    /// Run a single-file guest image as its own process on the real .NET runtime, and report how it
    /// terminated. The image must be self-contained in the sense of referencing nothing but the
    /// shared framework, which is what `Roslyn.compile` produces; an assembly with its own
    /// dependencies needs them laid out beside it, so use `executePublishedApp` instead.
    /// Reject a seed the real filesystem cannot faithfully stand in for.
    ///
    /// Every rule here is a way the two sides would silently answer questions
    /// about *different* filesystems, which is far worse than a test that
    /// refuses to run: the comparison would still be made, and would still look
    /// like evidence.
    ///
    /// Deliberately narrow rather than clever. An earlier version tried to
    /// decide, by walking the seed, whether a multi-component target could
    /// escape the scratch directory; three rounds of review found three
    /// different holes in that analysis (a `..` after a symlink, a `..` that
    /// left the walk pointing at the wrong directory, a target naming a file
    /// the host has and PawPrint does not). The analysis was the wrong altitude:
    /// getting it right means reimplementing host path resolution, including
    /// the *unclamped* `..` that PawPrint deliberately does not model. So the
    /// oracle accepts only targets it can check by inspection — a single
    /// component, naming something in the same directory or nothing at all —
    /// and refuses the rest with an explanation. Every seed the differential
    /// tests use is of that shape; when one genuinely needs more, this is the
    /// place to do the analysis properly, once, with the case in hand.
    ///
    /// `reserved` are the names the oracle must itself write at the top level
    /// (the guest image and its runtimeconfig).
    let validateSeedForOracle (reserved : string list) (seed : Map<FileName, SeedEntry>) : unit =
        // A stock macOS filesystem folds case *and* Unicode normalisation, so
        // two names PawPrint keeps apart can become one file there. Reproducing
        // that folding faithfully is beyond what this can do: APFS applies full
        // Unicode case folding, under which "ss" aliases the sharp s, and
        // `ToLowerInvariant` does not. So seed names are restricted to an
        // alphabet whose folding is unambiguous, and anything else is refused
        // rather than approximated — every seed the differential tests use is
        // ASCII, and PawPrint's own unit tests, which need no host directory,
        // are unaffected.
        let unambiguous (c : char) : bool =
            (c >= 'a' && c <= 'z')
            || (c >= 'A' && c <= 'Z')
            || (c >= '0' && c <= '9')
            || c = '.'
            || c = '-'
            || c = '_'

        // On that alphabet, case is the only folding left, and ASCII case
        // folding really is `ToLowerInvariant`. Applied whatever *this*
        // machine's filesystem does, so a seed cannot pass on a case-sensitive
        // dev box and compare the wrong thing in CI.
        let fold (name : string) : string = name.ToLowerInvariant ()

        /// Every string `fold` is applied to must be in that alphabet, or the
        /// folding is an approximation of the host's rather than a match for
        /// it. That includes the names the *oracle* contributes: they are
        /// derived from the guest assembly's name, which is ASCII today, but
        /// nothing else here would notice if that changed.
        let requireFoldable (what : string) (candidate : string) : unit =
            match candidate |> Seq.tryFind (unambiguous >> not) with
            | Some c ->
                failwith
                    $"%s{what} \"%s{candidate}\" contains '%c{c}'. The oracle accepts only names whose case folding is unambiguous (ASCII letters, digits, '.', '-' and '_'), because a stock macOS filesystem folds case and Unicode normalisation in ways this cannot reproduce — it aliases \"ss\" with the sharp s, for instance — so two names it folds together would silently become one file on the host while PawPrint kept both."
            | None -> ()

        // An unseeded run materialises nothing, so there is nothing for a
        // reserved name to collide with and nothing for the host to fold it
        // against. Checking it anyway would refuse a perfectly good guest whose
        // assembly happened to be named "Paw Print" — a regression against
        // every oracle run that predates seeding.
        if not (Map.isEmpty seed) then
            for name in reserved do
                requireFoldable "The oracle's own reserved file" name

        let rec go (prefix : string) (depth : int) (entries : Map<FileName, SeedEntry>) : unit =
            let names =
                entries |> Map.toList |> List.map (fun (name, _) -> FileName.toString name)

            for name in names do
                requireFoldable $"The filesystem seed's entry %s{prefix}/%s{name}" name

            names
            |> List.groupBy fold
            |> List.iter (fun (_, group) ->
                if List.length group > 1 then
                    let clashing = String.Join (" and ", group)

                    failwith
                        $"The filesystem seed declares %s{clashing} in %s{prefix}/, which differ only by case or Unicode normalisation. PawPrint's filesystem distinguishes them by their exact bytes; a stock macOS one folds both, so the oracle cannot stand in for one of them."
            )

            // What the host will resolve in this directory: the seed's own
            // names, plus — at the top level — the two files the oracle has to
            // put there to run the guest at all.
            let hostNames = if depth = 0 then names @ reserved else names

            let foldsOntoHost (candidate : string) : string option =
                hostNames |> List.tryFind (fun existing -> fold existing = fold candidate)

            for KeyValue (name, entry) in entries do
                let name = FileName.toString name

                if depth = 0 then
                    match reserved |> List.tryFind (fun r -> fold r = fold name) with
                    | Some clash ->
                        failwith
                            $"The filesystem seed declares \"%s{name}\" at its root, which is also what this oracle must write there to run the guest at all (\"%s{clash}\"). Rename the seeded entry: silently overwriting the guest image would turn the test into a much more confusing failure."
                    | None -> ()

                match entry with
                | SeedEntry.File _ -> ()
                | SeedEntry.Directory children -> go (prefix + "/" + name) (depth + 1) children
                | SeedEntry.Symlink target ->
                    let raw = SymlinkTarget.toString target

                    let refuse (why : string) : unit =
                        failwith
                            $"The filesystem seed declares the symlink %s{prefix}/%s{name} with the target \"%s{raw}\", which %s{why}. The oracle only accepts a target naming a single entry in the link's own directory, because that is the only shape it can check by inspection rather than by reimplementing the host's path resolution."

                    let asPath = SymlinkTarget.toUnixPath target

                    if UnixPath.isRooted asPath then
                        // PawPrint would resolve this against its own root; the
                        // host against the real one.
                        refuse "is absolute"

                    match UnixPath.components asPath with
                    | [ PathComponent.Name only ] ->
                        // Fine if it names a sibling exactly, and fine if it
                        // names nothing at all (a dangling link resolves the
                        // same way on both sides). Not fine in between: if the
                        // host would fold it onto a sibling — or onto the guest
                        // image — that PawPrint does not match exactly, one
                        // side has a target and the other does not.
                        let only = FileName.toString only

                        // `foldsOntoHost` below folds this, so it is subject to
                        // the same alphabet: a target of "ss" beside a sibling
                        // named with the sharp s aliases on APFS and not here.
                        requireFoldable $"The symlink %s{prefix}/%s{name}'s target" only

                        // An exact match on one of the *seed's* names is the
                        // good case. Anything else the host would nonetheless
                        // resolve is not: the reserved files exist only on the
                        // host, and a folded match resolves there while
                        // PawPrint's exact-bytes lookup misses.
                        if not (List.contains only names) then
                            match foldsOntoHost only with
                            | Some existing ->
                                refuse
                                    $"names nothing in that directory that PawPrint would find, but a case-insensitive host would resolve it to \"%s{existing}\""
                            | None -> ()
                    // Everything else, including "." and "..": a rooted target
                    // was refused above, and an empty one is unrepresentable,
                    // so no case here is unreachable.
                    | _ -> refuse "is not a single path component"

        go "" 0 seed

    /// Write a filesystem seed into a real directory, so that the guest running
    /// on the real runtime sees the same tree PawPrint realises into its
    /// `VirtualFileSystem`.
    ///
    /// One description, two interpreters: the differential claim is only worth
    /// anything if both sides are configured from the *same value*, rather than
    /// from a host tree and a seed that someone kept in step by hand.
    let rec private materialiseSeed (directory : string) (entries : Map<FileName, SeedEntry>) : unit =
        for KeyValue (name, entry) in entries do
            let path = Path.Combine (directory, FileName.toString name)

            match entry with
            | SeedEntry.File contents -> File.WriteAllBytes (path, Seq.toArray contents)
            | SeedEntry.Directory children ->
                Directory.CreateDirectory path |> ignore<DirectoryInfo>
                materialiseSeed path children
            // Verbatim, and deliberately not checked for existence: a seeded
            // symlink may dangle, and `File.CreateSymbolicLink` is happy to
            // create one that does.
            | SeedEntry.Symlink target ->
                File.CreateSymbolicLink (path, SymlinkTarget.toString target)
                |> ignore<FileSystemInfo>

    /// Run a single-file guest image as its own process on the real .NET
    /// runtime, with `seed` materialised into its working directory, and report
    /// how it terminated.
    ///
    /// The guest's working directory is the scratch directory this owns, so a
    /// guest should probe **relative** paths: PawPrint's side of the comparison
    /// puts the same seed at the root of an otherwise-empty filesystem with `/`
    /// as the current directory, and the two agree on relative names but not on
    /// what the absolute ones are.
    ///
    /// Note the scratch directory necessarily also holds the guest image and
    /// its `runtimeconfig.json`, which PawPrint's filesystem does not contain.
    /// A guest that enumerated its working directory would therefore diverge
    /// for a reason that has nothing to do with the code under test; probe named
    /// paths only. A seed that collides with either name is refused outright.
    let executeWithTimeoutAndSeed
        (timeout : TimeSpan)
        (seed : Map<FileName, SeedEntry>)
        (args : string[])
        (assemblyBytes : byte array)
        : RealRuntimeResult
        =
        let tempDir =
            Path.Combine (Path.GetTempPath (), "pawprint-oracle-" + Path.GetRandomFileName ())

        Directory.CreateDirectory tempDir |> ignore

        try
            let assemblyName = assemblyNameOf assemblyBytes
            let dllPath = Path.Combine (tempDir, assemblyName + ".dll")

            validateSeedForOracle [ assemblyName + ".dll" ; assemblyName + ".runtimeconfig.json" ] seed

            File.WriteAllBytes (dllPath, assemblyBytes)
            File.WriteAllText (Path.Combine (tempDir, assemblyName + ".runtimeconfig.json"), runtimeConfig)
            materialiseSeed tempDir seed

            runToCompletion timeout muxerPath (dllPath :: List.ofArray args) tempDir assemblyName
        finally
            try
                Directory.Delete (tempDir, true)
            with
            | :? IOException
            | :? UnauthorizedAccessException -> ()

    let executeWithTimeout (timeout : TimeSpan) (args : string[]) (assemblyBytes : byte array) : RealRuntimeResult =
        executeWithTimeoutAndSeed timeout FileSystemSeed.empty args assemblyBytes

    /// As `executeWithTimeoutAndSeed`, with the standard guest time limit.
    let executeWithSeed
        (seed : Map<FileName, SeedEntry>)
        (args : string[])
        (assemblyBytes : byte array)
        : RealRuntimeResult
        =
        executeWithTimeoutAndSeed guestTimeout seed args assemblyBytes

    /// As `executeWithTimeout`, with the standard guest time limit.
    let executeWithRealRuntime (args : string[]) (assemblyBytes : byte array) : RealRuntimeResult =
        executeWithTimeout guestTimeout args assemblyBytes

    /// Run a managed assembly that already sits on disk beside the assemblies it depends on.
    ///
    /// `executeWithTimeout` owns a scratch directory holding exactly one image, so it cannot run a
    /// guest that references anything but the shared framework. This instead runs the assembly
    /// where it lies, which is what makes its siblings resolve: an app with no `deps.json` gets
    /// every `.dll` in its own directory placed on the trusted platform assemblies list, so a
    /// directory of co-compiled assemblies binds by simple name with no probing logic of ours.
    ///
    /// This writes `<name>.runtimeconfig.json` beside the assembly, overwriting any existing one.
    /// An app that ships its own configuration is a published app; use `executePublishedApp`.
    let executeAssemblyInPlace (args : string[]) (dllPath : string) : RealRuntimeResult =
        if not (File.Exists dllPath) then
            failwith $"Cannot run %s{dllPath} under the real runtime: the assembly does not exist."

        let directory = Path.GetDirectoryName dllPath
        // The host derives the config's name from the assembly's *file* name, not from its
        // assembly name, and the file is already on disk under a name the caller chose.
        let name = Path.GetFileNameWithoutExtension dllPath
        File.WriteAllText (Path.Combine (directory, name + ".runtimeconfig.json"), runtimeConfig)

        runToCompletion guestTimeout muxerPath (dllPath :: List.ofArray args) directory name

    /// Run an already-published application in place, by executing its apphost.
    ///
    /// A published app carries its own dependencies, `runtimeconfig.json` and `deps.json` in its
    /// output directory, so it must run from there rather than being copied to a scratch directory
    /// as a bare image. Executing the apphost rather than passing the managed dll to the muxer is
    /// how such an app is actually launched, and is the only supported launch path for a
    /// self-contained publish.
    let executePublishedApp (args : string[]) (executablePath : string) : RealRuntimeResult =
        if not (File.Exists executablePath) then
            failwith
                $"Cannot run published app %s{executablePath} under the real runtime: the apphost does not exist. Has the project been published?"

        runToCompletion
            guestTimeout
            executablePath
            (List.ofArray args)
            (Path.GetDirectoryName executablePath)
            (Path.GetFileName executablePath)
