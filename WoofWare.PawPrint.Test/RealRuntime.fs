namespace WoofWare.PawPrint.Test

open System
open System.Diagnostics
open System.IO
open System.Reflection.Metadata
open System.Reflection.PortableExecutable
open System.Runtime.InteropServices
open System.Text
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// What the real runtime's stderr reveals about *which* fatal error killed the guest.
///
/// Strictly less than PawPrint's own <c>FatalErrorCode</c>, and deliberately a different type so
/// the two are not mistaken for the same fact. PawPrint knows which fatal error it raised because
/// it raised it; the oracle can only read what the process printed, and CoreCLR derives the banner
/// from the <c>COR_E_*</c> code with a single equality test — <c>exitCode == COR_E_FAILFAST</c>
/// (eepolicy.cpp:374-383) — so stderr separates <c>COR_E_FAILFAST</c> from everything else and
/// nothing finer. The exit status adds nothing either: on Unix the process aborts to 134 whatever
/// the code was.
[<RequireQualifiedAccess>]
type ObservedFatalError =
    /// Banner <c>Process terminated.</c>, so the code was <c>COR_E_FAILFAST</c>: the guest called
    /// <c>Environment.FailFast</c>.
    | FailFast
    /// Banner <c>Fatal error.</c>, so the code was one of the others — <c>COR_E_EXECUTIONENGINE</c>,
    /// <c>COR_E_STACKOVERFLOW</c>, and so on. Which is not recoverable from what the process
    /// printed; a test that needs to know reads the report, whose message is the runtime's own and
    /// does identify the situation even though it does not identify the code.
    | Other

/// How a program terminated when run on the real .NET runtime.
///
/// These cases mirror the terminal `WoofWare.PawPrint.RunOutcome` cases that a real process can
/// actually reach, so that the oracle is comparable against PawPrint.
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
    /// A fatal error tore the program down: `Environment.FailFast`, or a refusal the runtime
    /// itself raised. The payload is the runtime's stderr report, banner included.
    | Aborted of observed : ObservedFatalError * report : string

[<RequireQualifiedAccess>]
module RealRuntime =

    /// The runtime writes this to stderr immediately before aborting on an escaped exception. It is
    /// a hardcoded literal (`SZ_UNHANDLED_EXCEPTION`, coreclr/vm/excep.cpp), not a localisable
    /// resource, so matching on it does not depend on the machine's locale.
    [<Literal>]
    let private UnhandledExceptionBanner = "Unhandled exception."

    /// As `UnhandledExceptionBanner`, for a fatal error whose code is `COR_E_FAILFAST` --
    /// `Environment.FailFast`, and nothing else. `PrintToStdErrA("Process terminated.\n")` in
    /// coreclr/vm/eepolicy.cpp:378.
    [<Literal>]
    let private FailFastBanner = "Process terminated."

    /// The banner for every *other* fatal error, chosen by the same `if` that chooses the one
    /// above (eepolicy.cpp:374-383) -- so a guest whose runtime refused to continue prints this
    /// one and exits 134, exactly as `Environment.FailFast` does. Without this the two are
    /// indistinguishable from the exit code alone, and a runtime-raised abort would be classified
    /// as an ordinary `NormalExit 134`. It says only "not `COR_E_FAILFAST`": see
    /// `ObservedFatalError.Other`.
    [<Literal>]
    let private FatalErrorBanner = "Fatal error."

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

        // Exit code alone cannot classify this: an escaped exception, either flavour of fatal
        // error, and a guest that merely returns 134 all exit 134 on Unix (128 + SIGABRT).
        // Requiring a nonzero exit *and* the runtime's banner keeps both a plain `return 134` and
        // a guest that prints the banner itself on the normal path. A guest that does both at once
        // would be misclassified, but it's either impossible or extremely hard to repair that.
        //
        // The two fatal-error banners are the two sides of the single equality test CoreCLR makes
        // on the `COR_E_*` code (eepolicy.cpp:374-383), so that is exactly what they distinguish
        // and `ObservedFatalError` says no more than that. They are disjoint strings, so the order
        // between them is for readability rather than correctness.
        let result =
            if exitCode <> 0 && errorText.Contains UnhandledExceptionBanner then
                RealRuntimeResult.UnhandledException (errorText.Trim ())
            elif exitCode <> 0 && errorText.Contains FailFastBanner then
                RealRuntimeResult.Aborted (ObservedFatalError.FailFast, errorText.Trim ())
            elif exitCode <> 0 && errorText.Contains FatalErrorBanner then
                RealRuntimeResult.Aborted (ObservedFatalError.Other, errorText.Trim ())
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
        | RealRuntimeResult.Aborted _ -> ()

        result

    /// Reject a seed the real filesystem cannot faithfully stand in for.
    ///
    /// Every rule here blocks a case where the two sides would silently answer
    /// questions about *different* filesystems while the comparison still
    /// looked like evidence.
    ///
    /// Deliberately narrow: deciding whether a multi-component symlink target
    /// escapes the scratch directory means reimplementing host path
    /// resolution, including the *unclamped* `..` that PawPrint deliberately
    /// does not model. So the oracle accepts only targets it can check by
    /// inspection — a single component, naming something in the same directory
    /// or nothing at all — and refuses the rest with an explanation. Every
    /// seed the differential tests use is of that shape.
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
        // against; checking it anyway would refuse a perfectly good guest
        // whose assembly happened to be named "Paw Print".
        if not (Map.isEmpty seed) then
            for name in reserved do
                requireFoldable "The oracle's own reserved file" name

        /// The set-user-ID, set-group-ID and sticky bits are refused for a seed
        /// the oracle will materialise, because the host does not reliably
        /// reproduce what the seed asked for. Linux's `chmod` silently drops
        /// `S_ISGID` when the caller is not a member of the file's group, and
        /// several filesystems refuse `S_ISUID` outright — so PawPrint would
        /// report the seeded bit while the host reported nothing, and the
        /// differential test would be asserting a difference in the *harness*
        /// rather than in the runtime.
        ///
        /// Only for oracle-visible seeds: a `sourcesImpure` case, which nothing
        /// materialises, may set them freely.
        let requireOraclePermissions (what : string) (isDirectory : bool) (permissions : PermissionBits) : unit =
            // Windows has no Unix modes at all, and `File.SetUnixFileMode`
            // throws there — so the host tree simply cannot be made to match a
            // seed that asks for one. Refusing loudly beats materialising the
            // shape and silently comparing PawPrint's stored bits against
            // whatever Windows synthesises. The *default* modes are permitted,
            // since a case that never mentions a mode is not asking about one.
            if RuntimeInformation.IsOSPlatform OSPlatform.Windows then
                let expected =
                    if isDirectory then
                        PermissionBits.defaultForDirectory
                    else
                        PermissionBits.defaultForRegularFile

                if permissions <> expected then
                    failwith
                        $"The filesystem seed gives %s{what} the mode %o{PermissionBits.toInt permissions}, and this oracle is running on Windows, which has no Unix permission bits to give a real file. Run mode-bearing differential cases on Unix."

            let special = PermissionBits.toInt permissions &&& 0o7000

            if special <> 0 then
                failwith
                    $"The filesystem seed gives %s{what} the mode %o{PermissionBits.toInt permissions}, whose set-user-ID/set-group-ID/sticky bits (%o{special}) this oracle refuses. A host `chmod` may silently drop them — Linux drops S_ISGID for a caller outside the file's group — so the two runtimes would disagree about the harness rather than about themselves. Move the case to sourcesImpure, which materialises nothing."

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
                | SeedEntry.File (_, permissions) -> requireOraclePermissions $"%s{prefix}/%s{name}" false permissions
                | SeedEntry.Directory (children, permissions) ->
                    requireOraclePermissions $"%s{prefix}/%s{name}" true permissions
                    go (prefix + "/" + name) (depth + 1) children
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

    /// Give every directory under `root` (and `root` itself) owner rwx, so that
    /// the tree can be enumerated and deleted.
    ///
    /// Symlinks are not followed — `Directory.GetDirectories`
    /// reports them, but chmod through one would change the mode of whatever
    /// it points at, which may be outside the scratch tree entirely.
    ///
    /// A whole-body no-op on Windows, which has no Unix modes to restore and
    /// where `File.SetUnixFileMode` throws `PlatformNotSupportedException`.
    /// `deleteScratchTree` runs in the `finally` of *every* oracle run, seeded
    /// or not, and the caller catches only
    /// `IOException`/`UnauthorizedAccessException` — so an unguarded call here
    /// would turn every successful differential test on Windows into a cleanup
    /// failure.
    ///
    /// Not private: the seed for `FileModeSeeded.cs` really does contain a
    /// directory its owner cannot write, so this runs on every differential run
    /// and its failure mode — a silently leaked scratch tree, since `Delete`'s
    /// exception is swallowed — is invisible. `TestRealRuntimeCleanup` fires it
    /// directly.
    let rec makeTreeDeletable (root : string) : unit =
        if not (RuntimeInformation.IsOSPlatform OSPlatform.Windows) then
            // Top-down: a directory must be made searchable before its own children can be listed.
            File.SetUnixFileMode (root, UnixFileMode.UserRead ||| UnixFileMode.UserWrite ||| UnixFileMode.UserExecute)

            for child in Directory.GetDirectories root do
                if not (File.GetAttributes(child).HasFlag FileAttributes.ReparsePoint) then
                    makeTreeDeletable child

    /// Remove a scratch tree, including one a seed left with directories their
    /// owner cannot write.
    let deleteScratchTree (root : string) : unit =
        // The chmod lives *inside* this function rather than beside its call site
        // deliberately: the caller swallows the failure (a leaked temp directory
        // must not fail a test), so a call site that forgot to restore the modes
        // first would be silent. Here, `TestRealRuntimeCleanup` covers both halves
        // as one unit.
        makeTreeDeletable root
        Directory.Delete (root, true)

    /// Whether a real directory on *this* host can stand in for `seed` at all.
    ///
    /// False only on Windows, and only for a seed naming a mode that
    /// `SeedEntry.file`/`SeedEntry.directory` would not have produced: Windows
    /// has no Unix permission bits to give a real file, so the host tree cannot
    /// be made to match.
    ///
    /// Distinct from `validateSeedForOracle`, which *refuses* such a seed
    /// loudly, and deliberately so: a seed that reaches the oracle with modes
    /// it cannot honour is a bug worth a failure, but a *test case* that simply
    /// cannot run here wants to be skipped. One predicate, so the two answers
    /// cannot drift apart.
    let rec canMaterialise (seed : Map<FileName, SeedEntry>) : bool =
        if not (RuntimeInformation.IsOSPlatform OSPlatform.Windows) then
            true
        else

        seed
        |> Map.forall (fun _ entry ->
            match entry with
            | SeedEntry.Symlink _ -> true
            | SeedEntry.File (_, permissions) -> permissions = PermissionBits.defaultForRegularFile
            | SeedEntry.Directory (children, permissions) ->
                permissions = PermissionBits.defaultForDirectory && canMaterialise children
        )

    /// The seed's permission bits, as the mode `chmod(2)` takes. `PermissionBits`
    /// is exactly `st_mode & 0o7777`, which is exactly `UnixFileMode`'s domain,
    /// so this is a re-encoding rather than a translation.
    let private toUnixFileMode (permissions : PermissionBits) : UnixFileMode =
        enum<UnixFileMode> (PermissionBits.toInt permissions)

    /// Apply a seeded mode to a real path, and check it took.
    ///
    /// No test here can fire the failure path: every filesystem the suite runs
    /// on stores Unix modes, and there is no way to fake one that does not.
    ///
    /// The read-back guards against the *filesystem* underneath `TMPDIR`, not
    /// `chmod(2)`: a mount whose modes are synthesised from mount options
    /// rather than stored — vfat, some CIFS configurations — accepts the call
    /// and reports something else afterwards. The oracle would then be
    /// comparing PawPrint's stored bits against the mount's invented ones and
    /// calling the difference a runtime bug.
    let private applyMode (path : string) (permissions : PermissionBits) : unit =
        let desired = toUnixFileMode permissions
        File.SetUnixFileMode (path, desired)
        let actual = File.GetUnixFileMode path

        if actual <> desired then
            failwith
                $"The oracle set the mode of \"%s{path}\" to %o{PermissionBits.toInt permissions}, and the filesystem reported %o{int actual} back. Some mounts synthesise modes from mount options rather than storing them (vfat, some CIFS); the differential comparison would be about that mount rather than about the two runtimes. Point TMPDIR at a filesystem that stores Unix modes."

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
            | SeedEntry.File (contents, permissions) ->
                File.WriteAllBytes (path, Seq.toArray contents)
                // After writing, not before: `File.WriteAllBytes` creates the
                // file under the host's umask, so the mode it lands with is a
                // property of the machine rather than of the seed.
                if not (RuntimeInformation.IsOSPlatform OSPlatform.Windows) then
                    applyMode path permissions
            | SeedEntry.Directory (children, permissions) ->
                Directory.CreateDirectory path |> ignore<DirectoryInfo>
                // Children first, *then* the mode: a directory seeded without
                // owner-write or owner-search could not have its own children
                // created through it once the mode was in place.
                materialiseSeed path children

                if not (RuntimeInformation.IsOSPlatform OSPlatform.Windows) then
                    applyMode path permissions
            // Verbatim, and deliberately not checked for existence: a seeded
            // symlink may dangle, and `File.CreateSymbolicLink` is happy to
            // create one that does.
            | SeedEntry.Symlink target ->
                File.CreateSymbolicLink (path, SymlinkTarget.toString target)
                |> ignore<FileSystemInfo>

    /// Run a single-file guest image as its own process on the real .NET
    /// runtime, with `seed` materialised into its working directory, and report
    /// how it terminated. The image must be self-contained in the sense of
    /// referencing nothing but the shared framework, which is what
    /// `Roslyn.compile` produces; an assembly with its own dependencies needs
    /// them laid out beside it, so use `executePublishedApp` instead.
    ///
    /// The guest's working directory is the scratch directory this owns, so a
    /// guest should probe **relative** paths: PawPrint's side of the comparison
    /// puts the same seed at the root of an otherwise-empty filesystem with `/`
    /// as the current directory, and the two agree on relative names but not on
    /// what the absolute ones are.
    ///
    /// The scratch directory necessarily also holds the guest image and
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
            // The scratch directory *is* the guest's "/" on this side of the
            // comparison, and `VirtualFileSystem.empty` gives PawPrint's root
            // `PermissionBits.defaultForDirectory`. `Directory.CreateDirectory`
            // creates 0777 less the umask, so the two agree only while the
            // umask is 022 — and the seed, being a map of *entries*, has no way
            // to name the root and fix it.
            //
            // Measured: `nix develop` pins the umask to 0022, so no run through
            // the devshell (CI included) can diverge here, and
            // `FileModeSeeded.cs`'s check on "." cannot fail without this line.
            // The guard is kept anyway, because the suite is runnable outside
            // the devshell and one chmod is cheaper than the confusing failure
            // it would otherwise produce there.
            if not (RuntimeInformation.IsOSPlatform OSPlatform.Windows) then
                applyMode tempDir PermissionBits.defaultForDirectory

            materialiseSeed tempDir seed

            runToCompletion timeout muxerPath (dllPath :: List.ofArray args) tempDir assemblyName
        finally
            try
                // A seed may deliberately have left a directory unreadable or
                // unsearchable, which is a mode `Directory.Delete` cannot
                // recurse through — and the `with` below would swallow the
                // failure, leaking the scratch tree rather than reporting it.
                // Restore owner rwx from the top down first; each directory is
                // made traversable before it is enumerated.
                deleteScratchTree tempDir
            with
            | :? IOException
            | :? UnauthorizedAccessException -> ()

    /// As `executeWithTimeoutAndSeed`, with an empty filesystem seed.
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
