namespace WoofWare.Pawprint.Test

open System
open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint
open WoofWare.PawPrint.Test

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestImpureCases =
    let assy = typeof<RunResult>.Assembly

    /// Build one registration of `CurrentDirectoryConfigured.cs`. The guest
    /// echoes the directory it observed to stdout, so the assertion is simply
    /// that the bytes it printed are the UTF-8 of the path we configured —
    /// which pins the whole chain (`KernelConfig.CurrentDirectory` ->
    /// `withCurrentDirectory` -> `SystemNative_GetCwd` -> CoreLib's buffer
    /// dance -> `Marshal.PtrToStringUTF8`) to an exact value, not a shape.
    let private currentDirectoryCase (dir : string) : EndToEndTestCase =
        {
            FileName = "CurrentDirectoryConfigured.cs"
            ExpectedReturnCode = 0
            KernelConfig =
                { KernelConfig.Default with
                    CurrentDirectory = AbsoluteUnixPath.parseOrFail "test current directory" dir
                }
            ExpectsUnhandledException = false
            AssertTerminalState =
                Some (fun state ->
                    OutputLogEntry.bytesFor FileDescriptorRole.StandardOutput state.Kernel.OutputLog
                    |> Seq.toArray
                    |> shouldEqual (Text.Encoding.UTF8.GetBytes dir)
                )
        }

    /// A directory whose UTF-8 encoding exceeds the 256 bytes CoreLib's
    /// `Interop.Sys.GetCwd()` stackallocs, so that the first `SystemNative_GetCwd`
    /// must fail with ERANGE and the guest must take its ArrayPool
    /// grow-and-retry branch. Several segments rather than one long name, so
    /// that the separators have to survive the retry too.
    let private longCurrentDirectory : string =
        List.replicate 20 "0123456789abcdef"
        |> List.fold (fun acc seg -> acc + "/" + seg) ""

    /// A directory of only 121 UTF-16 characters but 264 UTF-8 bytes: under the
    /// 256-byte stackalloc if you measure it in `string` length, over it if you
    /// measure the bytes the kernel actually writes. ERANGE is a *byte* rule,
    /// so this must still take the grow-and-retry branch; an implementation
    /// that compared `bufferSize` against the character count would silently
    /// overrun here rather than retry. `TestCurrentDirectoryEncodingSizes`
    /// asserts those two counts, so this comment cannot rot into a lie.
    let private multiByteCurrentDirectory : string =
        // Per segment (including its leading separator): é×5 at 2 UTF-8 bytes,
        // 中×3 at 3, 🐶×1 at 4 (and a surrogate pair, so 2 UTF-16 chars) = 24
        // bytes and 11 chars. A mix, so the test cannot accidentally pass under
        // a wrong-but-constant bytes-per-character assumption.
        List.replicate 11 "é中éé中🐶ééé中" |> List.fold (fun acc seg -> acc + "/" + seg) ""

    /// The two size claims the cases above rest on. Asserted rather than
    /// trusted: if a future edit to either literal quietly drops one of them
    /// under the 256-byte stackalloc, the corresponding case stops exercising
    /// the grow-and-retry branch and would still pass, silently.
    [<Test>]
    let ``The long current-directory cases really do overflow CoreLib's stackalloc`` () : unit =
        // `Interop.Sys.GetCwd()` stackallocs exactly this much before retrying.
        let stackallocBytes = 256

        Text.Encoding.UTF8.GetByteCount longCurrentDirectory
        |> shouldBeGreaterThan stackallocBytes

        Text.Encoding.UTF8.GetByteCount multiByteCurrentDirectory
        |> shouldBeGreaterThan stackallocBytes

        // ...and the multi-byte one must be *under* the limit by character
        // count, or it is not testing anything the ASCII case doesn't.
        multiByteCurrentDirectory.Length |> shouldBeSmallerThan stackallocBytes

    let unimplemented : EndToEndTestCase list =
        [
            // Both of these have a current directory whose UTF-8 encoding
            // overflows the 256 bytes `Interop.Sys.GetCwd()` stackallocs, so
            // `SystemNative_GetCwd` correctly returns NULL with errno=ERANGE
            // and the guest takes its ArrayPool grow-and-retry branch. It then
            // stops one call later, at `Interop.Sys.GetLastErrorInfo()`: that
            // converts the raw errno to the `Interop.Error` PAL enum through
            // `SystemNative_ConvertErrorPlatformToPal`, which has no handler
            // registered in `Native/NativeDispatch.fs`, so this reaches
            // `NativeCall.failUnimplemented`.
            //
            // That entry point is the runtime's shared errno<->PAL translation
            // (an 84-case table in `src/native/libs/Common/pal_error_common.h`)
            // used by every `SystemNative_*` shim rather than anything specific
            // to getcwd, so it wants its own change. Un-park both when it lands
            // — nothing else here is missing, and the short-path siblings in
            // `cases` below already prove the success path end to end.
            currentDirectoryCase longCurrentDirectory
            currentDirectoryCase multiByteCurrentDirectory

            // A *short* non-ASCII directory, parked for an unrelated reason:
            // it fits the stackalloc, so `SystemNative_GetCwd` succeeds and
            // ERANGE never enters into it, but decoding the bytes back with
            // `Marshal.PtrToStringUTF8` takes CoreLib's non-ASCII UTF-8 path,
            // which stops at the unreviewed JIT intrinsic
            // `System.Numerics.BitOperations.TrailingZeroCount(uint32)`
            // (`IlMachineStateExecution.fs`, "TODO: implement JIT intrinsic").
            // The ASCII siblings in `cases` cover the same handler; what is
            // missing is an intrinsic, not anything about the current
            // directory. `TestAbsoluteUnixPath` covers the UTF-8 encoding of
            // such a path directly in the meantime.
            currentDirectoryCase "/héllo/中文/🐶"
        ]

    let cases : EndToEndTestCase list =
        [
            // The default current directory is part of PawPrint's replay
            // contract: a guest that resolves a relative path must get the same
            // answer on every machine, so the default has to be a fixed value
            // rather than the host's. Registered explicitly (rather than relying
            // on the other cases) so that a change to `defaultCurrentDirectory`
            // fails a test that says so.
            currentDirectoryCase "/"
            currentDirectoryCase "/home/pawprint/work"
            {
                // `SystemNative_GetCwd` must classify its error returns before
                // resolving the caller's buffer to storage, because the C
                // decides them without dereferencing it. Impure because the
                // guest passes a pointer that addresses nothing: safe under
                // PawPrint by construction, but not something to hand the
                // in-process real runtime in the differential harness.
                FileName = "GetCwdNoDereferenceErrors.cs"
                ExpectedReturnCode = 0
                KernelConfig = KernelConfig.Default
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // `Console.WriteLine("Hello, world!")` exercises the full
                // BCL stdio stack end-to-end: `Console::get_Out` descends
                // `ConsolePal::OpenStandardOutput → Interop.Sys.Dup`, then
                // the `StreamWriter` flush descends `Interop.Sys.Write`.
                // Both shims are intercepted by PawPrint's
                // FileDescriptorRegistry / EmulatedKernel. We assert on
                // the bytes the guest actually appended to the stdout
                // log, not just the exit code — a regression in the
                // encoder, the StreamWriter buffer, or the SystemNative
                // pointer decode would not change the exit code (the
                // `return 1;` runs unconditionally) but would corrupt
                // these bytes.
                FileName = "WriteLine.cs"
                ExpectedReturnCode = 1
                KernelConfig = KernelConfig.Default
                ExpectsUnhandledException = false
                AssertTerminalState =
                    Some (fun state ->
                        OutputLogEntry.bytesFor FileDescriptorRole.StandardOutput state.Kernel.OutputLog
                        |> Seq.toArray
                        |> shouldEqual (System.Text.Encoding.UTF8.GetBytes "Hello, world!\n")

                        OutputLogEntry.bytesFor FileDescriptorRole.StandardError state.Kernel.OutputLog
                        |> Seq.length
                        |> shouldEqual 0
                    )
            }
            {
                // A host-configured `KernelConfig.ProcessorCount` must actually
                // reach the guest, and must do so before the entry type's
                // `.cctor` runs — CoreLib latches `Environment.ProcessorCount`
                // into a static on first read, so applying the configuration any
                // later than `Program.prepare` does would leave a guest that
                // reads it during static initialisation observing the default.
                // 4 rather than 1 so that a regression to "always the default"
                // is a failure rather than a coincidence.
                FileName = "ProcessorCountConfigured.cs"
                ExpectedReturnCode = 0
                KernelConfig =
                    { KernelConfig.Default with
                        ProcessorCount = 4
                    }
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // The wall clock the guest observes through `DateTime.UtcNow`
                // boots at the Unix epoch by default. That is a replay-contract
                // value rather than an implementation detail, and the pure test
                // cannot pin it: pure cases are cross-checked against the real
                // runtime, which reports today's date.
                FileName = "DateTimeUtcNowEpochDefault.cs"
                ExpectedReturnCode = 0
                KernelConfig = KernelConfig.Default
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // PawPrint places guest threads round-robin over the simulated
                // cores, so with four of them the entry thread and its workers
                // observe distinct `Thread.GetCurrentProcessorId()` values. The
                // pure `ThreadGetCurrentProcessorId.cs` cannot pin any of this:
                // it is cross-checked against the real runtime, where the value
                // comes from the host's `sched_getcpu` (or, on macOS, from a
                // managed-thread-id fallback that is not bounded by the core
                // count at all). 4 rather than 1 so that a regression to
                // "always core 0" is a failure rather than a coincidence.
                // `TestCpuPlacement` covers the placement policy itself.
                FileName = "SchedGetCpuPlacement.cs"
                ExpectedReturnCode = 0
                KernelConfig =
                    { KernelConfig.Default with
                        ProcessorCount = 4
                    }
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // The monotonic clock the guest observes through `Stopwatch`
                // boots at zero and moves in whole milliseconds, and is the same
                // clock `Environment.TickCount64` reads. Those are
                // replay-contract facts the pure `StopwatchElapsed.cs` cannot
                // pin: it is cross-checked against the real runtime, whose
                // CLOCK_MONOTONIC counts from an unspecified origin at
                // nanosecond resolution. `TestMonotonicTimestamp` covers the
                // scaling arithmetic itself; this covers the chain from
                // `SystemNative_GetTimestamp` out to guest-visible `Stopwatch`.
                FileName = "StopwatchTimestampGranularity.cs"
                ExpectedReturnCode = 0
                KernelConfig = KernelConfig.Default
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // Same guest observation, but with the host moving the boot
                // instant to 2023-11-14T00:00:00Z. Covers the whole chain
                // (`KernelConfig.WallClockEpochMs` -> `withWallClockEpochMs` ->
                // `systemTimeAsTicks` -> `SystemNative_GetSystemTimeAsTicks`),
                // where `TestSystemTimeAsTicks` covers the tick arithmetic
                // itself.
                FileName = "DateTimeUtcNowEpochConfigured.cs"
                ExpectedReturnCode = 0
                KernelConfig =
                    { KernelConfig.Default with
                        WallClockEpochMs = 1_699_920_000_000L
                    }
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // Same guest, reached the other way: the count comes from the
                // guest-visible `DOTNET_PROCESSOR_COUNT` knob rather than from
                // `KernelConfig.ProcessorCount`, which stays at its default.
                // Covers the whole chain (env overlay -> kernel table ->
                // `effectiveProcessorCount` -> the native handler), where
                // `TestEffectiveProcessorCount` covers the resolution rules
                // themselves.
                FileName = "ProcessorCountConfigured.cs"
                ExpectedReturnCode = 0
                KernelConfig =
                    { KernelConfig.Default with
                        Environment = Map.ofList [ "DOTNET_PROCESSOR_COUNT", "4" ]
                    }
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // `Environment.Exit` from the entry thread. Exercises the same
                // `ProcessExit` path as `ExitFromWorker.cs` below, but with the
                // caller being the thread whose return would otherwise have
                // supplied the exit code: `Main` goes on to `return 100`, so a
                // regression that let the guest keep running past `_Exit` would
                // surface as exit code 100 instead of 1.
                FileName = "InstaQuit.cs"
                ExpectedReturnCode = 1
                KernelConfig = KernelConfig.Default
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // Exercises Environment.Exit called from a worker thread: the whole process
                // must terminate with the worker's exit code, not just that worker thread.
                FileName = "ExitFromWorker.cs"
                ExpectedReturnCode = 7
                KernelConfig = KernelConfig.Default
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // Exercises the SystemNative_Write success path: a guest that
                // DllImports SystemNative_Write directly and pushes a few
                // bytes at stdout. The pure-source test only covers the
                // error paths (negative size, bad fd, zero size); the
                // success path is impure because it appends to the
                // interpreter's `OutputLog` and we want to assert directly
                // on those bytes rather than try to capture the test
                // runner's real stdout. The guest returns 0 on success
                // (positive return from `Write`), so a regression in the
                // handler's return value or pointer decoding also surfaces
                // as a wrong exit code.
                FileName = "SystemNativeWriteSuccess.cs"
                ExpectedReturnCode = 0
                KernelConfig = KernelConfig.Default
                ExpectsUnhandledException = false
                AssertTerminalState =
                    Some (fun state ->
                        // The guest writes the literal "hi\n" (3 bytes) to
                        // fd 1. If the handler decoded the pointer wrong,
                        // we'd see garbage or fewer bytes here.
                        OutputLogEntry.bytesFor FileDescriptorRole.StandardOutput state.Kernel.OutputLog
                        |> Seq.toArray
                        |> shouldEqual [| 0x68uy ; 0x69uy ; 0x0Auy |]

                        OutputLogEntry.bytesFor FileDescriptorRole.StandardError state.Kernel.OutputLog
                        |> fun bytes -> bytes.Length
                        |> shouldEqual 0
                    )
            }
            {
                // Exercises the SystemNative_Close / SystemNative_Dup handler
                // pair end-to-end against the PawPrint FileDescriptorRegistry:
                // close of an invalid fd, close of a freshly-duped fd, the
                // double-close EBADF path, and the lowest-free gap-fill after
                // a close. This used to live in sourcesPure for cross-runtime
                // validation, but the real CLR's multi-threaded fd activity
                // races our close + dup window in the NUnit test process, so
                // it now runs as an impure (PawPrint-only) test where the
                // interpreter's deterministic single-threaded fd table makes
                // the assertions stable. The registry-level invariants are
                // still independently covered by TestFileDescriptorRegistry's
                // property tests; this test verifies the wiring from the
                // P/Invoke handler through to the registry.
                FileName = "SystemNativeClose.cs"
                ExpectedReturnCode = 0
                KernelConfig = KernelConfig.Default
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // Exercises the SystemNative_IsATty PawPrint handler against
                // standard fds, a freshly-duped fd, and a closed fd. Lives in
                // sourcesImpure because the real CLR's IsATty answer depends
                // on whether the test process happens to have a TTY attached
                // to its standard streams, which races with how a developer
                // happens to run NUnit; PawPrint's headless-process model
                // makes the answer stable by construction.
                FileName = "SystemNativeIsATty.cs"
                ExpectedReturnCode = 0
                KernelConfig = KernelConfig.Default
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // PawPrint reports every GCMemoryInfo field as zero, for every GCKind,
                // because the interpreter never collects. That is emphatically not a
                // property of the real runtime, so it cannot be asserted in a
                // sourcesPure case (which is diffed against the real runtime's exit
                // code); it belongs here, where the expected code is PawPrint's alone.
                // sourcesPure/GCGetMemoryInfo.cs carries the cross-runtime half.
                FileName = "GCMemoryInfoAllZero.cs"
                ExpectedReturnCode = 42
                KernelConfig = KernelConfig.Default
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // A byref to a `[ThreadStatic]` field taken on thread A still addresses A's
                // slot when dereferenced on thread B, because `ldsflda` bakes the owning thread
                // into the pointer rather than re-resolving it at each access. That is a real
                // CLI fact, but it cannot be a differential case: the only way to move a byref
                // across a thread boundary in C# is through a raw pointer, and a .NET 9+
                // thread-static lives in a movable GC-heap block, so on the real runtime the
                // program is undefined behaviour - and it really does misbehave in-process
                // under the suite's allocation pressure. PawPrint's byrefs are symbolic and
                // never move. See the file's own comment, plus the unit property in
                // `TestThreadStatics.fs`; `sourcesPure/ThreadStaticIsolation.cs` carries the
                // cross-runtime half of the thread-static contract.
                FileName = "ThreadStaticByrefAcrossThreads.cs"
                ExpectedReturnCode = 0
                KernelConfig = KernelConfig.Default
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // `Assembly.Location` is empty for every assembly, because under
                // PawPrint no assembly has a file the guest could reach — the
                // same state CoreCLR reports for a byte-array load or a
                // single-file-published app. Deliberately not a differential
                // case: the real runtime is launched from a real .dll and
                // reports its path, so there is no cross-runtime fact here.
                // Recorded in docs/divergences.md.
                FileName = "AssemblyLocationEmpty.cs"
                ExpectedReturnCode = 0
                KernelConfig = KernelConfig.Default
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
        ]

    let runTest (case : EndToEndTestCase) : unit =
        let source = Assembly.getEmbeddedResourceAsString case.FileName assy
        let image = Roslyn.compile [ source ]

        let messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", case.FileName ]

        use _loggerFactoryResource = loggerFactory

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        try
            let terminalState, terminatingThread =
                match
                    Program.run
                        loggerFactory
                        (Some case.FileName)
                        peImage
                        { HostConfig.Default dotnetRuntimes with
                            Kernel = case.KernelConfig
                        }
                with
                | RunOutcome.GuestUnhandledException (_, _, exn) ->
                    failwith $"Guest threw unhandled exception: %O{exn.ExceptionObject}"
                | RunOutcome.FailFast (_, _, message) ->
                    let m = message |> Option.defaultValue "<no message>"
                    failwith $"Guest called Environment.FailFast: %s{m}"
                | RunOutcome.SignalTerminated (_, signal) -> failwith $"Guest was terminated by POSIX signal %O{signal}"
                | RunOutcome.NormalExit (state, thread) -> state, thread
                | RunOutcome.ProcessExit (state, thread) -> state, thread

            let exitCode =
                match terminalState.ThreadState.[terminatingThread].MethodState.EvaluationStack.Values with
                | [] -> failwith "expected program to return a value, but it returned void"
                | head :: _ ->
                    match head with
                    | EvalStackValue.Int32 (Int32Source.Verbatim i) -> i
                    | ret -> failwith $"expected program to return an int, but it returned %O{ret}"

            exitCode |> shouldEqual case.ExpectedReturnCode

            match case.AssertTerminalState with
            | None -> ()
            | Some assertion -> assertion terminalState
        with _ ->
            for message in messages () do
                System.Console.Error.WriteLine $"{message}"

            reraise ()

    [<TestCaseSource(nameof unimplemented)>]
    [<Explicit>]
    let ``Can evaluate C# files, unimplemented`` (case : EndToEndTestCase) = runTest case

    [<TestCaseSource(nameof cases)>]
    let ``Can evaluate C# files`` (case : EndToEndTestCase) = runTest case
