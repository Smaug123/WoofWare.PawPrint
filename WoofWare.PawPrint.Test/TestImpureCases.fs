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

    /// Build one registration of `EffectiveUserIdConfigured.cs`. The guest
    /// echoes the effective uid it observed to stdout as four little-endian
    /// bytes, so the assertion is that those bytes are the identity we
    /// configured — which lets one source file pin `SystemNative_GetEUid` at
    /// several distinct identities.
    ///
    /// Through stdout rather than through the exit code, because an exit code
    /// is eight bits and a uid is a `uint32`. Every identity below 2^16 leaves
    /// a truncating handler indistinguishable from a correct one, and every
    /// identity below 2^31 leaves a sign-confusing one indistinguishable too —
    /// so the registrations include `nobody`, which is neither.
    ///
    /// `gid` is always different from `uid`, so a handler reading `GroupId`
    /// fails; the registrations below also swap the pair, so it fails in both
    /// directions. None of them is `EmulatedKernel.defaultUserId`, so a handler
    /// answering with a constant fails too.
    let private effectiveUserIdCase (uid : uint32) (gid : uint32) : EndToEndTestCase =
        {
            FileName = "EffectiveUserIdConfigured.cs"
            ExpectedReturnCode = 0
            KernelConfig =
                { KernelConfig.Default with
                    UserId = uid
                    GroupId = gid
                    // One file, for the guest's `st_uid == GetEUid()` check.
                    FileSystem =
                        Map.ofList
                            [
                                FileName.parseOrFail "test seed" "f",
                                SeedEntry.File (Text.Encoding.UTF8.GetBytes "hello" |> ImmutableArray.CreateRange)
                            ]
                }
            AppContext = AppContextProperties.empty
            ExpectsUnhandledException = false
            AssertTerminalState =
                Some (fun state ->
                    // Spelled out rather than taken from `BitConverter`, which
                    // would make this expectation and the guest's own
                    // byte-shifting agree only because the host is
                    // little-endian.
                    OutputLogEntry.bytesFor FileDescriptorRole.StandardOutput state.Kernel.OutputLog
                    |> Seq.toArray
                    |> shouldEqual
                        [|
                            byte (uid &&& 0xFFu)
                            byte ((uid >>> 8) &&& 0xFFu)
                            byte ((uid >>> 16) &&& 0xFFu)
                            byte ((uid >>> 24) &&& 0xFFu)
                        |]
                )
        }

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
            AppContext = AppContextProperties.empty
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

    /// Is this the concrete handle for `System.Runtime.ExceptionServices.ExceptionDispatchInfo`?
    let private isExceptionDispatchInfo (state : IlMachineState) (handle : ConcreteTypeHandle) : bool =
        match AllConcreteTypes.lookup handle state.ConcreteTypes with
        | None -> false
        | Some ct ->
            ct.Assembly.Name = "System.Private.CoreLib"
            && ct.Namespace = "System.Runtime.ExceptionServices"
            && ct.Name = "ExceptionDispatchInfo"
            && ct.Generics.IsEmpty

    /// The contract `ExceptionNative_GetFrozenStackTrace` has to satisfy, stated where it can
    /// actually be observed. See `sourcesImpure/ExceptionDispatchInfoCaptureState.cs` for why
    /// the differential test cannot carry this.
    ///
    /// Walks `ExceptionDispatchInfo._dispatchState.StackTrace` — the field the QCall writes
    /// through its second `ObjectHandleOnStack` — and requires that it holds a token registered
    /// in `IlMachineState.FrozenStackTraces` whose frames are the guest's real ones. A handler
    /// that wrote null, or that minted a fresh object instead of returning `_stackTrace`, fails
    /// here.
    let private assertCapturedFrozenStackTrace (state : IlMachineState) : unit =
        let ediObjects =
            HeapObserver.nonArrayObjects state.ManagedHeap
            |> List.filter (fun (_, object) -> isExceptionDispatchInfo state object.ConcreteType)

        let _ediAddr, ediObject =
            match ediObjects with
            | [ single ] -> single
            | other ->
                failwith
                    $"expected exactly one ExceptionDispatchInfo on the heap, got %d{other.Length}; the guest parks exactly one in a static"

        let dispatchStateField =
            IlMachineState.requiredOwnInstanceFieldId state ediObject.ConcreteType "_dispatchState"

        let dispatchState =
            match AllocatedNonArrayObject.DereferenceFieldById dispatchStateField ediObject with
            | CliType.ValueType vt -> vt
            | other -> failwith $"expected ExceptionDispatchInfo._dispatchState to be a value type, got %O{other}"

        let stackTraceField =
            IlMachineState.requiredOwnInstanceFieldId state dispatchState.Declared "StackTrace"

        let token =
            match CliValueType.DereferenceFieldById stackTraceField dispatchState with
            | CliType.ObjectRef (Some token) -> token
            | CliType.ObjectRef None ->
                failwith
                    "DispatchState.StackTrace is null after capturing a thrown exception: GetFrozenStackTrace did not return the exception's frozen trace"
            | other -> failwith $"expected DispatchState.StackTrace to be an ObjectRef, got %O{other}"

        let frames =
            match state.FrozenStackTraces |> Map.tryFind token with
            | Some frames -> frames
            | None ->
                failwith
                    $"DispatchState.StackTrace holds @ %O{token}, which is not a token PawPrint minted; GetFrozenStackTrace must return the exception's own _stackTrace, not a fresh object"

        // The frames must be the guest's, not an empty placeholder: the throwing method and the
        // method that caught it both appear in the trace PawPrint built during unwind.
        let methodNames = frames |> List.map (fun frame -> frame.Method.Name)

        methodNames |> List.contains "Thrower" |> shouldEqual true
        methodNames |> List.contains "Main" |> shouldEqual true

    let cases : EndToEndTestCase list =
        [
            // Both of these have a current directory whose UTF-8 encoding
            // overflows the 256 bytes `Interop.Sys.GetCwd()` stackallocs, so
            // `SystemNative_GetCwd` returns NULL with errno=ERANGE and the
            // guest takes its ArrayPool grow-and-retry branch. That branch runs
            // through `Interop.Sys.GetLastErrorInfo()`, which converts the raw
            // errno with `SystemNative_ConvertErrorPlatformToPal` and compares
            // the result against `Interop.Error.ERANGE` to decide whether to
            // retry or throw — so these are the cases that exercise that
            // handler against *real* CoreLib, including its `Interop.Error`
            // enum return type, rather than a hand-rolled P/Invoke declaration.
            // They were parked until that entry point existed.
            //
            // Note the short non-ASCII sibling still in `unimplemented` below
            // is parked for a genuinely different reason and was re-checked
            // when these two were promoted: it still fails, on the
            // TrailingZeroCount intrinsic.
            currentDirectoryCase longCurrentDirectory
            currentDirectoryCase multiByteCurrentDirectory
            {
                // Pins the PawPrint-side contract of `ExceptionNative_GetFrozenStackTrace`.
                // Impure because the claim is about interpreter state (the token and the frame
                // table behind it), which the real runtime has no analogue of — its equivalent
                // is a native `StackTraceArray` of `MethodDesc*`.
                FileName = "ExceptionDispatchInfoCaptureState.cs"
                ExpectedReturnCode = 0
                KernelConfig = KernelConfig.Default
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = Some assertCapturedFrozenStackTrace
            }
            // The default current directory is part of PawPrint's replay
            // contract: a guest that resolves a relative path must get the same
            // answer on every machine, so the default has to be a fixed value
            // rather than the host's. Registered explicitly (rather than relying
            // on the other cases) so that a change to `defaultCurrentDirectory`
            // fails a test that says so.
            currentDirectoryCase "/"
            currentDirectoryCase "/home/pawprint/work"
            // Root, which is the identity `defaultUserId` deliberately avoids:
            // `Environment.IsPrivilegedProcess` is exactly `GetEUid() == 0`, so
            // this is the only case in the suite that observes a guest taking
            // its privileged branch.
            effectiveUserIdCase 0u 200u
            // An ordinary unprivileged identity, and the same pair swapped, so
            // that reporting the gid fails whichever way round it is.
            effectiveUserIdCase 37u 200u
            effectiveUserIdCase 200u 37u
            // `nobody` on Linux, and the `nogroup` beside it. Both have their
            // high bit set and neither fits in sixteen bits, which is what
            // makes a truncating or sign-confusing handler visible at all.
            effectiveUserIdCase 4294967294u 4294967293u
            {
                // Reads every field `SystemNative_Stat`/`LStat` write, through a
                // hand-rolled P/Invoke. Impure because most of those fields
                // *cannot* agree with a real filesystem: a real file's owner is
                // whoever ran the suite, and its timestamps are "just now",
                // whereas the emulated kernel's are its boot instant. The
                // cross-runtime half of the story — which paths exist, and what
                // kind of thing lives at each — is `sourcesPure/FileExistsSeeded.cs`.
                FileName = "StatFieldsSeeded.cs"
                ExpectedReturnCode = 0
                KernelConfig =
                    { KernelConfig.Default with
                        // Deliberately *not* the defaults. A boot clock of 0
                        // would make "the seed recorded the configured instant"
                        // indistinguishable from "the seed left a zero in
                        // place", and equal uid and gid would let the two be
                        // swapped without any test noticing. The awkward
                        // millisecond count also forces the seconds/nanoseconds
                        // split to be done rather than guessed.
                        WallClockEpochMs = 1_700_000_123L
                        UserId = 1000u
                        GroupId = 2000u
                        FileSystem =
                            let name (s : string) = FileName.parseOrFail "test seed" s
                            let target (s : string) = SymlinkTarget.parseOrFail "test seed" s

                            Map.ofList
                                [
                                    name "f",
                                    SeedEntry.File (Text.Encoding.UTF8.GetBytes "hello" |> ImmutableArray.CreateRange)
                                    name "d", SeedEntry.Directory Map.empty
                                    name "lf", SeedEntry.Symlink (target "f")
                                    name "dang", SeedEntry.Symlink (target "nx")
                                ]
                    }
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // The three parts of `SystemNative_ReadLink`'s contract the
                // differential oracle cannot be asked about; the guest's own
                // header says why each one is here rather than in the pure
                // sibling `SystemNativeReadLink.cs`.
                FileName = "ReadLinkRawSeeded.cs"
                ExpectedReturnCode = 0
                KernelConfig =
                    { KernelConfig.Default with
                        FileSystem =
                            let name (s : string) = FileName.parseOrFail "test seed" s
                            let target (s : string) = SymlinkTarget.parseOrFail "test seed" s

                            Map.ofList
                                [
                                    name "f",
                                    SeedEntry.File (Text.Encoding.UTF8.GetBytes "hello" |> ImmutableArray.CreateRange)
                                    name "lf", SeedEntry.Symlink (target "f")
                                    // U+00DF then 'x': three UTF-8 bytes,
                                    // C3 9F 78, so that a one- or two-byte
                                    // truncation lands *inside* the first
                                    // character. That is the whole point of
                                    // the seed — a handler measuring .NET
                                    // characters rather than bytes agrees with
                                    // a correct one on every ASCII target, and
                                    // ASCII is all the oracle's seed validator
                                    // permits.
                                    name "mb", SeedEntry.Symlink (target "ßx")
                                ]
                    }
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // Pins the emulated kernel's MAXSYMLINKS end to end, which is
                // the one part of `pathLimits` that unit tests cannot reach:
                // they call the resolver directly, so a `resolveGuestPath` that
                // hardcoded a platform would satisfy every one of them.
                //
                // Impure because its subject is a 33-link chain — precisely the
                // length Linux resolves and macOS refuses — so it is not a
                // cross-runtime fact and must not be handed to the oracle.
                FileName = "SymlinkLimitSeeded.cs"
                ExpectedReturnCode = 0
                KernelConfig =
                    { KernelConfig.Default with
                        FileSystem =
                            let name (s : string) = FileName.parseOrFail "test seed" s
                            let target (s : string) = SymlinkTarget.parseOrFail "test seed" s

                            /// A chain of `length` links under `prefix`, ending
                            /// at a regular file, so resolving its head performs
                            /// exactly `length` traversals.
                            let chain (prefix : string) (length : int) =
                                [
                                    for i in 1..length do
                                        let next =
                                            if i = length then
                                                $"%s{prefix}target"
                                            else
                                                $"%s{prefix}%d{i + 1}"

                                        yield name $"%s{prefix}%d{i}", SeedEntry.Symlink (target next)

                                    yield name $"%s{prefix}target", SeedEntry.File ImmutableArray<byte>.Empty
                                ]

                            // 32 is below every platform's limit, 41 above every
                            // platform's limit, and 33 is the disputed band.
                            // Written as literals rather than derived from
                            // `pathLimits`, so that this test disagrees with a
                            // wrong `pathLimits` instead of agreeing with it.
                            [ chain "a" 32 ; chain "b" 33 ; chain "c" 41 ] |> List.concat |> Map.ofList
                    }
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // Pins Darwin's symlink-splice length re-check end to end. The
                // unit tests call the resolver directly, so a `resolveGuestPath`
                // passing hardcoded limits would satisfy all of them; only a
                // guest sees that the configured platform reaches the syscall
                // boundary.
                //
                // Configured as **macOS**, unusually for these tests, because
                // Linux performs no such check at any length — on the default
                // kernel every path in this guest would simply resolve. That
                // also makes the raw errno Darwin's 63.
                FileName = "SpliceLengthSeeded.cs"
                ExpectedReturnCode = 0
                KernelConfig =
                    { KernelConfig.Default with
                        UnixPlatform = SimulatedUnixPlatform.macOsArm64
                        FileSystem =
                            /// An absolute path of exactly `bytes` bytes naming
                            /// nothing, in components of 200 so that NAME_MAX
                            /// cannot be what refuses it.
                            let dangling (bytes : int) : SymlinkTarget =
                                let component_ = "/" + String.replicate 200 "z"

                                String.replicate (bytes / component_.Length + 1) component_
                                |> fun s -> s.Substring (0, bytes)
                                |> SymlinkTarget.parseOrFail "test seed"

                            // Written as literals rather than derived from
                            // `pathLimits`, so that this test disagrees with a
                            // wrong PATH_MAX instead of agreeing with it.
                            [
                                FileName.parseOrFail "test seed" "atMax", SeedEntry.Symlink (dangling 1021)
                                FileName.parseOrFail "test seed" "overMax", SeedEntry.Symlink (dangling 1022)
                            ]
                            |> Map.ofList
                    }
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // Pins PATH_MAX and NAME_MAX end to end. Needs no seed: every
                // path it passes is refused before anything is looked up, and
                // the controls are ENOENT in an empty filesystem.
                //
                // Impure because the raw errno it reads is the *Linux* one, and
                // ENAMETOOLONG is numbered differently on Darwin (63) — so this
                // is a claim about the kernel PawPrint is configured to be, not
                // a cross-runtime fact.
                FileName = "PathLengthLimitsSeeded.cs"
                ExpectedReturnCode = 0
                KernelConfig = KernelConfig.Default
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // The motivating case for host-seeded AppContext: a BCL feature switch,
                // declared in `runtimeconfig.json` and latched by `EventSource` on first
                // read. Impure for the same reason as the case below.
                FileName = "EventSourceDisabled.cs"
                ExpectedReturnCode = 0
                KernelConfig = KernelConfig.Default
                AppContext =
                    AppContextProperties.ofMap (
                        Map.ofList [ "System.Diagnostics.Tracing.EventSource.IsSupported", "false" ]
                    )
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // Host-seeded AppContext properties, as `hostpolicy` installs them from
                // `runtimeconfig.json`. Impure because the differential oracle loads the
                // guest in-process on the host runtime, whose AppContext was seeded before
                // this test process started and cannot be reseeded; "what the host put in
                // AppContext" is therefore a PawPrint-only fact.
                FileName = "AppContextConfigProperties.cs"
                ExpectedReturnCode = 0
                KernelConfig = KernelConfig.Default
                AppContext =
                    AppContextProperties.ofMap (
                        Map.ofList
                            [
                                "Test.String", "hello world"
                                "Test.Astral", "p\U0001F436w"
                                "Test.Empty", ""
                                "Test.True", "true"
                                "Test.False", "false"
                            ]
                    )
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // Seeding must precede the entry type's `.cctor` pump, not merely precede
                // Main: BCL feature switches latch into `static readonly` fields on first
                // read. The guest latches a seeded property the same way, so this fails if
                // the seed ever moves later in `Program.prepare`.
                FileName = "AppContextSeededBeforeCctor.cs"
                ExpectedReturnCode = 0
                KernelConfig = KernelConfig.Default
                AppContext = AppContextProperties.ofMap (Map.ofList [ "Test.Latched", "latched" ])
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // PawPrint declares that it does not support dynamic code, which is a fact
                // about this runtime rather than about any guest: it has no JIT and no
                // Reflection.Emit. The BCL routes around Emit when the switch is off, so
                // this turns a class of "unimplemented native primitive" crashes into the
                // documented `PlatformNotSupportedException` a real host raises in the same
                // configuration. Impure because the differential oracle runs on the host
                // runtime, which does support dynamic code.
                //
                // Note this case declares *no* AppContext properties: the baseline is
                // supplied by the library itself, so a host that expresses no preference
                // still gets it. That is what makes it a default rather than a convention
                // every host has to remember.
                FileName = "DynamicCodeUnsupportedByDefault.cs"
                ExpectedReturnCode = 0
                KernelConfig = KernelConfig.Default
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // The other half of the same contract: the baseline sits *beneath* the
                // host's properties, so a guest whose `runtimeconfig.json` declares the
                // switch true observes true. Pins the precedence direction, which is the
                // part a future edit could silently reverse.
                FileName = "DynamicCodeSupportedOverride.cs"
                ExpectedReturnCode = 0
                KernelConfig = KernelConfig.Default
                AppContext =
                    AppContextProperties.ofMap (
                        Map.ofList
                            [
                                "System.Runtime.CompilerServices.RuntimeFeature.IsDynamicCodeSupported", "true"
                            ]
                    )
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // `ModuleHandle_GetDynamicMethod`, the QCall behind
                // `DynamicMethod.GetMethodDescriptor()`. Registered with the switch overridden to
                // true, which is the only way to ask PawPrint to exercise a dynamic-code path --
                // exactly the escape hatch `DynamicCodeSupportedOverride.cs` pins the existence of.
                // The guest's comment explains why the QCall's effect is observable without
                // executing the dynamic method, and what each non-zero exit code means.
                FileName = "DynamicMethodStubFromModule.cs"
                ExpectedReturnCode = 0
                KernelConfig = KernelConfig.Default
                AppContext =
                    AppContextProperties.ofMap (
                        Map.ofList
                            [
                                "System.Runtime.CompilerServices.RuntimeFeature.IsDynamicCodeSupported", "true"
                            ]
                    )
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // What a `Reflection.Emit` method's frame looks like in a rendered stack trace:
                // no qualifying type name, because it has no declaring type. The guest-visible
                // consequence of #988's representation choice, and the one thing that would have
                // caught a fabricated owner.
                FileName = "DynamicMethodStackTrace.cs"
                ExpectedReturnCode = 0
                KernelConfig = KernelConfig.Default
                AppContext =
                    AppContextProperties.ofMap (
                        Map.ofList
                            [
                                "System.Runtime.CompilerServices.RuntimeFeature.IsDynamicCodeSupported", "true"
                            ]
                    )
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // Executing the body of a `Reflection.Emit` method: the first slice that runs the
                // IL rather than only minting, describing or binding it. Registered with the
                // dynamic-code switch overridden, like its siblings.
                FileName = "DynamicMethodInvoke.cs"
                ExpectedReturnCode = 0
                KernelConfig = KernelConfig.Default
                AppContext =
                    AppContextProperties.ofMap (
                        Map.ofList
                            [
                                "System.Runtime.CompilerServices.RuntimeFeature.IsDynamicCodeSupported", "true"
                            ]
                    )
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // When `DynamicMethod.InitLocals` is read (after minting) and when it stops being
                // read (after the first execution). Registered with the dynamic-code switch
                // overridden, like its siblings.
                FileName = "DynamicMethodInitLocals.cs"
                ExpectedReturnCode = 0
                KernelConfig = KernelConfig.Default
                AppContext =
                    AppContextProperties.ofMap (
                        Map.ofList
                            [
                                "System.Runtime.CompilerServices.RuntimeFeature.IsDynamicCodeSupported", "true"
                            ]
                    )
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // `ldstr` whose operand names a `DynamicScope` entry rather than a UserString row,
                // and the object identity that comes with it: interning by value, with the
                // emitting guest's own string as the candidate on a miss, decided at first
                // execution rather than at mint. Registered with the dynamic-code switch
                // overridden, like its siblings. Every expectation was measured against the host's
                // real .NET, which returns 0 for this program.
                FileName = "DynamicMethodStringLiteral.cs"
                ExpectedReturnCode = 0
                KernelConfig = KernelConfig.Default
                AppContext =
                    AppContextProperties.ofMap (
                        Map.ofList
                            [
                                "System.Runtime.CompilerServices.RuntimeFeature.IsDynamicCodeSupported", "true"
                            ]
                    )
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // Type-shaped operands resolved against a `DynamicScope` rather than against
                // metadata: `newarr`, `sizeof`, `isinst`, `castclass`, `box`/`unbox`/`unbox.any`,
                // `initobj`, `ldobj`/`stobj` and `ldelema`, plus the `InvalidProgramException` an
                // operand that does not name a closed type produces. Registered with the
                // dynamic-code switch overridden, like its siblings. Every expectation was measured
                // against the host's real .NET, which returns 0 for this program.
                FileName = "DynamicMethodTypeToken.cs"
                ExpectedReturnCode = 0
                KernelConfig = KernelConfig.Default
                AppContext =
                    AppContextProperties.ofMap (
                        Map.ofList
                            [
                                "System.Runtime.CompilerServices.RuntimeFeature.IsDynamicCodeSupported", "true"
                            ]
                    )
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // `Delegate_BindToMethodInfo`, the QCall behind `DynamicMethod.CreateDelegate`.
                // Registered with the dynamic-code switch overridden to true, like its
                // `ModuleHandle_GetDynamicMethod` sibling above. The guest walks every binding
                // shape a dynamic method can produce and names what each non-zero exit code means.
                FileName = "DynamicMethodDelegateBinding.cs"
                ExpectedReturnCode = 0
                KernelConfig = KernelConfig.Default
                AppContext =
                    AppContextProperties.ofMap (
                        Map.ofList
                            [
                                "System.Runtime.CompilerServices.RuntimeFeature.IsDynamicCodeSupported", "true"
                            ]
                    )
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // The other half of the same QCall: that the `_methodPtr` it writes is the *bound
                // method's* identity rather than a constant or a per-binding one. Separate from
                // the case above because nothing there compares two delegates, so nothing there
                // can observe `_methodPtr` at all.
                FileName = "DynamicMethodDelegateIdentity.cs"
                ExpectedReturnCode = 0
                KernelConfig = KernelConfig.Default
                AppContext =
                    AppContextProperties.ofMap (
                        Map.ofList
                            [
                                "System.Runtime.CompilerServices.RuntimeFeature.IsDynamicCodeSupported", "true"
                            ]
                    )
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
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
                AppContext = AppContextProperties.empty
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
                AppContext = AppContextProperties.empty
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
                AppContext = AppContextProperties.empty
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
                AppContext = AppContextProperties.empty
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
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // The monotonic clock the guest observes through `Stopwatch`
                // boots at zero, and is the same clock `Environment.TickCount64`
                // reads. It moves in whole milliseconds at the current
                // instruction cost — a property of the rate rather than of the
                // clock's 100 ns unit; see the guest for what that means for
                // these assertions. Those are
                // replay-contract facts the pure `StopwatchElapsed.cs` cannot
                // pin: it is cross-checked against the real runtime, whose
                // CLOCK_MONOTONIC counts from an unspecified origin at
                // nanosecond resolution. `TestMonotonicTimestamp` covers the
                // scaling arithmetic itself; this covers the chain from
                // `SystemNative_GetTimestamp` out to guest-visible `Stopwatch`.
                FileName = "StopwatchTimestampGranularity.cs"
                ExpectedReturnCode = 0
                KernelConfig = KernelConfig.Default
                AppContext = AppContextProperties.empty
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
                AppContext = AppContextProperties.empty
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
                AppContext = AppContextProperties.empty
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
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // Exercises Environment.Exit called from a worker thread: the whole process
                // must terminate with the worker's exit code, not just that worker thread.
                FileName = "ExitFromWorker.cs"
                ExpectedReturnCode = 7
                KernelConfig = KernelConfig.Default
                AppContext = AppContextProperties.empty
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
                AppContext = AppContextProperties.empty
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
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // Writes through descriptors produced by `dup(2)` and asserts
                // the bytes land under the role of the *shared* open file
                // description, not under some default. A wiring regression
                // that lost the role on the dup path — routing every duped
                // descriptor to stdout, say — is invisible to the registry's
                // own property tests, which never reach the Write handler.
                //
                // PawPrint-only because OutputLog has no real-runtime
                // counterpart; the cross-runtime half of this contract is
                // sourcesPure/SystemNativeDupWrite.cs, which asserts the same
                // routing through return values without emitting bytes.
                FileName = "SystemNativeDupWriteRole.cs"
                ExpectedReturnCode = 0
                KernelConfig = KernelConfig.Default
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState =
                    Some (fun state ->
                        OutputLogEntry.bytesFor FileDescriptorRole.StandardOutput state.Kernel.OutputLog
                        |> Seq.toArray
                        |> shouldEqual [| 0x61uy ; 0x62uy |]

                        OutputLogEntry.bytesFor FileDescriptorRole.StandardError state.Kernel.OutputLog
                        |> Seq.toArray
                        |> shouldEqual [| 0x7Auy |]
                    )
            }
            {
                // Exercises SystemNative_ConvertErrorPlatformToPal, the point
                // at which PawPrint's raw errno vocabulary becomes the
                // platform-independent `Interop.Error` CoreLib branches on.
                // Impure by necessity rather than convenience: the PAL values
                // are platform-independent but the *mapping* is not, because
                // the real shim is compiled against one platform's <errno.h>
                // (raw 39 is ENOTEMPTY on Linux, EDESTADDRREQ on Darwin). A
                // cross-runtime oracle would therefore be asserting a
                // host-specific fact. Covers both arms of the handler's
                // return-type match: the enum CoreLib declares and the plain
                // `int` a hand-rolled P/Invoke would use.
                FileName = "SystemNativeConvertErrorPlatformToPal.cs"
                ExpectedReturnCode = 0
                KernelConfig = KernelConfig.Default
                AppContext = AppContextProperties.empty
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
                AppContext = AppContextProperties.empty
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
                AppContext = AppContextProperties.empty
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
                AppContext = AppContextProperties.empty
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
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // Which message a negative-length `newarr` reports. CoreCLR has two answers,
                // picked by the allocation helper the JIT emitted for the element type, and
                // for `string[]` on a 64-bit target it picks the one PawPrint does *not*
                // reproduce — so there is no cross-runtime fact here, only PawPrint's own
                // choice of the `AllocateSzArray` message. The exception *type*, which both
                // runtimes agree on, is asserted differentially in
                // `sourcesPure/NewarrLengthValidation.cs`. Recorded in docs/divergences.md.
                FileName = "NewarrNegativeLengthMessage.cs"
                ExpectedReturnCode = 0
                KernelConfig = KernelConfig.Default
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // `Unsafe.ByteOffset` between byrefs more than 2^31 bytes apart, which is the
                // guest-visible face of the projection walk's byte-offset accumulation.
                //
                // Impure not because the two runtimes disagree — they agree exactly, and the
                // expected values were measured on real .NET — but because displacing a byref
                // that far past a stack local is undefined behaviour, so the *oracle* is
                // non-deterministic: measured, this guest died with an AccessViolationException
                // roughly one run in ten on real .NET while never returning a different answer.
                // A differential registration would be flaky for a reason unrelated to the code
                // under test. The guest's own comment carries the measurement.
                FileName = "UnsafeByteOffsetInt32Overflow.cs"
                ExpectedReturnCode = 0
                KernelConfig = KernelConfig.Default
                AppContext = AppContextProperties.empty
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
                    BoundedRun.run
                        loggerFactory
                        case.FileName
                        (Some case.FileName)
                        peImage
                        { HostConfig.Default dotnetRuntimes with
                            Guest =
                                { GuestConfig.Default dotnetRuntimes with
                                    Kernel = case.KernelConfig
                                    AppContext = case.AppContext
                                }
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
