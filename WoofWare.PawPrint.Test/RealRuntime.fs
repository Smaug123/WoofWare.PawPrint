namespace WoofWare.PawPrint.Test

open System
open System.Diagnostics
open System.IO
open System.Reflection.Metadata
open System.Reflection.PortableExecutable
open System.Runtime.InteropServices
open System.Text

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
    let executeWithTimeout (timeout : TimeSpan) (args : string[]) (assemblyBytes : byte array) : RealRuntimeResult =
        let tempDir =
            Path.Combine (Path.GetTempPath (), "pawprint-oracle-" + Path.GetRandomFileName ())

        Directory.CreateDirectory tempDir |> ignore

        try
            let assemblyName = assemblyNameOf assemblyBytes
            let dllPath = Path.Combine (tempDir, assemblyName + ".dll")
            File.WriteAllBytes (dllPath, assemblyBytes)
            File.WriteAllText (Path.Combine (tempDir, assemblyName + ".runtimeconfig.json"), runtimeConfig)

            runToCompletion timeout muxerPath (dllPath :: List.ofArray args) tempDir assemblyName
        finally
            try
                Directory.Delete (tempDir, true)
            with
            | :? IOException
            | :? UnauthorizedAccessException -> ()

    /// As `executeWithTimeout`, with the standard guest time limit.
    let executeWithRealRuntime (args : string[]) (assemblyBytes : byte array) : RealRuntimeResult =
        executeWithTimeout guestTimeout args assemblyBytes

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
