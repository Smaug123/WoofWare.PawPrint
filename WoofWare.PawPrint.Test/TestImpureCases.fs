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
                                SeedEntry.file (Text.Encoding.UTF8.GetBytes "hello" |> ImmutableArray.CreateRange)
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

    /// A seed holding exactly the chain of directories `path` names, so that a
    /// process really can be started there. The kernel resolves its current
    /// directory when it is built, so a case configuring one must seed it —
    /// no process is ever started in a directory that does not exist.
    let private directoryChain (path : string) : Map<FileName, SeedEntry> =
        path.Split '/'
        |> Array.filter (fun segment -> segment <> "")
        |> Array.rev
        |> Array.fold
            (fun contents segment ->
                Map.ofList
                    [
                        FileName.parseOrFail "test current directory" segment, SeedEntry.directory contents
                    ]
            )
            FileSystemSeed.empty

    /// Build one registration of `CurrentDirectoryConfigured.cs`. The guest
    /// echoes the directory it observed to stdout, so the assertion is
    /// that the bytes it printed are the UTF-8 of the path we configured —
    /// which pins the whole chain (`KernelConfig.CurrentDirectory` ->
    /// `withFileSystemAndCurrentDirectory` -> `SystemNative_GetCwd` -> CoreLib's
    /// buffer dance -> `Marshal.PtrToStringUTF8`) to an exact value, not a shape.
    let private currentDirectoryCase (dir : string) : EndToEndTestCase =
        {
            FileName = "CurrentDirectoryConfigured.cs"
            ExpectedReturnCode = 0
            KernelConfig =
                { KernelConfig.Default with
                    CurrentDirectory = AbsoluteUnixPath.parseOrFail "test current directory" dir
                    FileSystem = directoryChain dir
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

    /// Build one registration of `ProcessPathConfigured.cs`. The guest echoes
    /// the executable path it observed to stdout, so the assertion is that the
    /// bytes it printed are the UTF-8 of the path we configured — which pins the
    /// whole chain (`KernelConfig.ProcessPath` -> `withProcessPath` ->
    /// `SystemNative_GetProcessPath` -> CoreLib's `Utf8StringMarshaller` ->
    /// `Environment.ProcessPath`) to an exact value rather than a shape.
    ///
    /// Registered under more than one path below, so that a handler answering a
    /// constant instead of reading `Kernel.ProcessPath` cannot satisfy them all.
    let private processPathCase (path : string) : EndToEndTestCase =
        {
            FileName = "ProcessPathConfigured.cs"
            ExpectedReturnCode = 0
            KernelConfig =
                { KernelConfig.Default with
                    ProcessPath = Some (AbsoluteUnixPath.parseOrFail "test process path" path)
                }
            AppContext = AppContextProperties.empty
            ExpectsUnhandledException = false
            AssertTerminalState =
                Some (fun state ->
                    OutputLogEntry.bytesFor FileDescriptorRole.StandardOutput state.Kernel.OutputLog
                    |> Seq.toArray
                    |> shouldEqual (Text.Encoding.UTF8.GetBytes path)
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
    /// overrun here rather than retry. ``The long current-directory cases
    /// really do overflow CoreLib's stackalloc`` below asserts those two
    /// counts.
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
            AssemblyDefinitionName.isNamed "System.Private.CoreLib" ct.AssemblyFullName
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

    /// The seed both write-wiring guests read: one file per mode shape, each
    /// holding the same five bytes, so a row's answer turns on its mode alone.
    let private writeModeSeed : Map<FileName, SeedEntry> =
        let hello =
            Text.Encoding.UTF8.GetBytes "hello"
            |> System.Collections.Immutable.ImmutableArray.CreateRange

        let entry (name : string) (mode : int) : FileName * SeedEntry =
            FileName.parseOrFail "test seed" name, SeedEntry.File (hello, PermissionBits.parseOrFail "test seed" mode)

        Map.ofList
            [
                entry "suid" 0o4755
                entry "sgid" 0o2755
                // Set-group-ID without group-execute: the row the two flavours
                // answer differently.
                entry "sgnox" 0o2644
                // Both set-ID bits, still without group-execute. The two flavour
                // rules and "strip nothing" each answer this one differently, so
                // no two of them can be confused on it.
                entry "both" 0o6644
                entry "sticky" 0o1755
                entry "plain" 0o0644
                // Written with a zero-length write, which is not a write at all
                // and must strip nothing.
                entry "zerolen" 0o4755
            ]

    /// The seed both truncation-wiring guests read: one file per mode shape, each
    /// holding the same five bytes, so a row's answer turns on its mode alone.
    let private truncationModeSeed : Map<FileName, SeedEntry> =
        let hello =
            Text.Encoding.UTF8.GetBytes "hello"
            |> System.Collections.Immutable.ImmutableArray.CreateRange

        let entry (name : string) (mode : int) : FileName * SeedEntry =
            FileName.parseOrFail "test seed" name, SeedEntry.File (hello, PermissionBits.parseOrFail "test seed" mode)

        Map.ofList
            [
                entry "suid" 0o4755
                entry "sgid" 0o2755
                // Set-group-ID without group-execute: the row that separates the
                // real rule from "clear both bits whenever either is set".
                entry "sgnox" 0o2644
                entry "sticky" 0o1755
                // Truncated to the length it already has.
                entry "noop" 0o4755
                // Not writable by its owner, which is what makes `O_TRUNC`'s
                // extra permission demand observable.
                entry "readonly" 0o444
                entry "otrunc" 0o4755
            ]

    /// The seed both `GetFileSystemType` flavour guests read: one file and one
    /// directory, which is all the mount has to hold for descriptors of both
    /// kinds to exist.
    let private getFileSystemTypeSeed : Map<FileName, SeedEntry> =
        let name (s : string) = FileName.parseOrFail "test seed" s

        Map.ofList
            [
                name "f", SeedEntry.file (Text.Encoding.UTF8.GetBytes "hello" |> ImmutableArray.CreateRange)
                name "d", SeedEntry.directory Map.empty
            ]

    /// Shared by the two `mkdir` wiring guests, so that the only thing that
    /// differs between them is the flavour.
    let private mkDirWiringSeed : Map<FileName, SeedEntry> =
        let name (s : string) = FileName.parseOrFail "test seed" s
        let target (s : string) = SymlinkTarget.parseOrFail "test seed" s

        let mode (raw : int) =
            PermissionBits.parseOrFail "test seed" raw

        Map.ofList
            [
                name "f", SeedEntry.file (Text.Encoding.UTF8.GetBytes "hello" |> ImmutableArray.CreateRange)
                name "d", SeedEntry.directory Map.empty
                name "lf", SeedEntry.Symlink (target "f")
                name "ld", SeedEntry.Symlink (target "d")
                // The link whose target Darwin creates and Linux does not.
                // "nx" is deliberately absent.
                name "dang", SeedEntry.Symlink (target "nx")
                name "cyc", SeedEntry.Symlink (target "cyc")
                // A parent carrying S_ISGID, which only Linux passes on. Seeded
                // rather than chmod'ed into place: there is no `SystemNative_ChMod`,
                // and on a real host a non-root `chmod` would drop the bit anyway.
                name "sg", SeedEntry.Directory (Map.empty, mode 0o2777)
                // Searchable but not writable, and holding a child: the child
                // answers EEXIST while a free name beside it answers EACCES,
                // which is what puts the write check *below* the EEXIST arm.
                name "nowrite",
                SeedEntry.Directory (Map.ofList [ name "kid", SeedEntry.directory Map.empty ], mode 0o555)
                // Unsearchable, and holding a child: looking the final name up
                // needs the search bit, so this answers EACCES where "nowrite"
                // — which can be searched but not written — answers EEXIST for
                // the same shape.
                name "nosearch",
                SeedEntry.Directory (Map.ofList [ name "kid", SeedEntry.directory Map.empty ], mode 0o666)
            ]

    /// Shared by the two `unlink` wiring guests, so that the only thing that
    /// differs between them is the configured flavour and the constants each
    /// expects.
    let private unlinkWiringSeed : Map<FileName, SeedEntry> =
        let name (s : string) = FileName.parseOrFail "test seed" s
        let target (s : string) = SymlinkTarget.parseOrFail "test seed" s

        let mode (raw : int) =
            PermissionBits.parseOrFail "test seed" raw

        Map.ofList
            [
                name "f", SeedEntry.file (Text.Encoding.UTF8.GetBytes "hello" |> ImmutableArray.CreateRange)
                name "d", SeedEntry.directory Map.empty
                name "ld", SeedEntry.Symlink (target "d")
                name "dang", SeedEntry.Symlink (target "nx")
                name "cyc", SeedEntry.Symlink (target "cyc")
                // The row that separates the two walks: with a trailing
                // separator, Darwin follows this to the root and answers EISDIR
                // where Linux never looks and answers ENOTDIR.
                name "lroot", SeedEntry.Symlink (target "/")
                // Searchable but not writable, holding one of each kind: the
                // directory is EACCES on Linux and EPERM on Darwin, which is the
                // pair of orderings the two guests exist to tell apart.
                name "nowrite",
                SeedEntry.Directory (
                    Map.ofList
                        [
                            name "kdir", SeedEntry.directory Map.empty
                            name "kid",
                            SeedEntry.file (Text.Encoding.UTF8.GetBytes "inside" |> ImmutableArray.CreateRange)
                        ],
                    mode 0o555
                )
            ]

    /// The two files `UnlinkReapSeeded.cs` opens, one of which it closes.
    let private unlinkReapSeed : Map<FileName, SeedEntry> =
        let name (s : string) = FileName.parseOrFail "test seed" s

        let file (contents : string) =
            SeedEntry.file (Text.Encoding.UTF8.GetBytes contents |> ImmutableArray.CreateRange)

        Map.ofList
            [
                name "held", file "payload"
                name "kept", file "kept-bytes"
                // Never opened, so the `unlink` itself has to free it.
                name "plain", file "never opened"
            ]

    /// `UnlinkReapSeeded.cs` has removed the last name of three inodes: one it
    /// never opened, one it opened and closed, and one it still holds. The first
    /// two must be gone — freed by the `unlink` and by the `close` respectively,
    /// which are the two halves of the rule — and the third must not be, excused
    /// its unreachability by being pinned.
    ///
    /// This is the only place either half of `EmulatedKernel.forgetIfUnheld` is
    /// visible: freeing an inode is not something a guest can observe, and
    /// failing to free one shows up only as a filesystem that grows without
    /// bound.
    let private assertUnlinkReapedExactlyOne (state : IlMachineState) : unit =
        let kernel = state.Kernel
        let filesystem = kernel.FileSystem
        let pinned = EmulatedKernel.pinnedInodes kernel

        let survivors =
            VirtualFileSystem.inodes filesystem
            |> Map.toList
            |> List.map fst
            |> List.filter (fun inode -> inode <> VirtualFileSystem.root filesystem)

        match survivors with
        | [ kept ] ->
            // Held open, so unreachable and legitimately so.
            VirtualFileSystem.bindingCount kept filesystem |> shouldEqual 0
            Set.contains kept pinned |> shouldEqual true

            VirtualFileSystem.checkInvariants pinned filesystem |> shouldEqual []

            // ...and the pin is what excuses it, rather than the rule having
            // gone quiet: without it, this is a defect.
            VirtualFileSystem.checkInvariants Set.empty filesystem
            |> shouldEqual [ VirtualFileSystemDefect.UnreachableFromRoot kept ]

            EmulatedKernel.checkInvariants kernel |> shouldEqual []
        | other ->
            failwith
                $"expected exactly one inode besides the root to survive the run -- the one the guest still holds open -- but %d{other.Length} did: %A{other}. The guest unlinks three files: one it never opened (so the unlink must free it), one it opened and closed (so the close must), and one it still holds. A survivor beyond the third means one of those two halves did not reap; none means something reaped an inode a descriptor still names."

    /// Shared by the two search-permission wiring guests, so that the only thing
    /// that differs between them is the uid.
    let private searchPermissionSeed : Map<FileName, SeedEntry> =
        let name (s : string) = FileName.parseOrFail "test seed" s
        let target (s : string) = SymlinkTarget.parseOrFail "test seed" s

        let mode (raw : int) =
            PermissionBits.parseOrFail "test seed" raw

        let file (contents : string) =
            SeedEntry.file (Text.Encoding.UTF8.GetBytes contents |> ImmutableArray.CreateRange)

        Map.ofList
            [
                // Readable but not searchable, and holding things worth reaching:
                // a directory, a file, and (by absence) a name that is not there.
                name "ns",
                SeedEntry.Directory (
                    Map.ofList [ name "kid", SeedEntry.directory Map.empty ; name "f", file "hello" ],
                    mode 0o666
                )
                // The control: the same shape, searchable.
                name "open", SeedEntry.Directory (Map.ofList [ name "kid", SeedEntry.directory Map.empty ], mode 0o755)
                // A component spliced in from a symlink target is looked up like
                // any other, so this earns the same answer "ns/kid" does.
                name "lns", SeedEntry.Symlink (target "ns")
            ]

    /// An unsearchable directory holding the current directory, for the guest
    /// that pins how a relative path is resolved.
    let private searchPermissionCwdSeed : Map<FileName, SeedEntry> =
        let name (s : string) = FileName.parseOrFail "test seed" s

        let mode (raw : int) =
            PermissionBits.parseOrFail "test seed" raw

        let file (contents : string) =
            SeedEntry.file (Text.Encoding.UTF8.GetBytes contents |> ImmutableArray.CreateRange)

        Map.ofList
            [
                name "outer",
                SeedEntry.Directory (
                    Map.ofList
                        [
                            name "inner",
                            SeedEntry.Directory (
                                Map.ofList [ name "target", file "hello" ; name "sub", SeedEntry.directory Map.empty ],
                                mode 0o755
                            )
                        ],
                    mode 0o666
                )
            ]

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
            // Two distinct executable paths, so a handler that answered a
            // constant rather than reading `Kernel.ProcessPath` fails one of
            // them. An apphost-shaped path and a muxer-shaped one, which are the
            // two things a real `Environment.ProcessPath` actually reports.
            processPathCase "/home/pawprint/work/Guest"
            processPathCase "/usr/share/dotnet/dotnet"
            // A path whose UTF-8 encoding is longer than its character count, so
            // that a handler measuring the wrong one truncates visibly.
            processPathCase "/héllo/中文/🐶/Guest"
            {
                // PawPrint's *default*: a process with no executable path at
                // all, which the entry point reports as NULL with errno ENOENT.
                // That default is part of the replay contract, so it is
                // registered explicitly rather than left implicit in the cases
                // above — a change to `defaultProcessPath` must fail a test that
                // says so.
                FileName = "ProcessPathAbsent.cs"
                ExpectedReturnCode = 0
                KernelConfig = KernelConfig.Default
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
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
                                    SeedEntry.file (Text.Encoding.UTF8.GetBytes "hello" |> ImmutableArray.CreateRange)
                                    name "d", SeedEntry.directory Map.empty
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
                                    SeedEntry.file (Text.Encoding.UTF8.GetBytes "hello" |> ImmutableArray.CreateRange)
                                    name "lf", SeedEntry.Symlink (target "f")
                                    // U+00DF then 'x': three UTF-8 bytes,
                                    // C3 9F 78, so that a one- or two-byte
                                    // truncation lands *inside* the first
                                    // character. A handler measuring .NET
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
                // `SystemNative_GetFileSystemType` for every kind of descriptor
                // the table holds, under each flavour in turn. Not differential:
                // the number a *file* reports is a property of whichever mount
                // the oracle's scratch directory is on, where PawPrint's is
                // `KernelConfig.FileSystemType`. The portable half — that the
                // reported filesystem is one CoreCLR will lock, so
                // `File.WriteAllBytes` works — is in `WriteSeeded.cs` and
                // `FlockContentionSeeded.cs`.
                FileName = "GetFileSystemTypeLinux.cs"
                ExpectedReturnCode = 0
                KernelConfig =
                    { KernelConfig.Default with
                        FileSystem = getFileSystemTypeSeed
                    }
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // The same checks in the same order under the Darwin flavour,
                // where `fstatfs(2)` refuses every object that is not on a
                // filesystem. This case is what pins the Darwin column even when
                // CI runs on Linux: the flavour is the kernel's configuration
                // rather than the host's.
                FileName = "GetFileSystemTypeDarwin.cs"
                ExpectedReturnCode = 0
                KernelConfig =
                    { KernelConfig.Default with
                        UnixPlatform = SimulatedUnixPlatform.macOsArm64
                        FileSystem = getFileSystemTypeSeed
                    }
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // The one filesystem whose *behaviour* differs: CoreCLR will not
                // take a shared lock under write access on NFS. Pairs with
                // `FlockContentionSeeded.cs` check 10, which is the same two
                // opens under the default filesystem, where they do contend.
                FileName = "FileSystemTypeNfs.cs"
                ExpectedReturnCode = 0
                KernelConfig =
                    { KernelConfig.Default with
                        FileSystemType = Some EmulatedFileSystemType.Nfs
                        FileSystem =
                            let name (s : string) = FileName.parseOrFail "test seed" s

                            Map.ofList
                                [
                                    name "f",
                                    SeedEntry.file (Text.Encoding.UTF8.GetBytes "hello" |> ImmutableArray.CreateRange)
                                ]
                    }
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // `SystemNative_FLock`'s contract on the points where Linux and
                // Darwin disagree — operation validation, `flock` on a
                // pipe, and the raw number of `EWOULDBLOCK`. PawPrint simulates
                // Linux, so a *pure* case would assert whichever machine ran it;
                // the guest's header carries the measured table for both.
                // `FlockContentionSeeded.cs` is the cross-runtime half.
                FileName = "FlockRawSeeded.cs"
                ExpectedReturnCode = 0
                KernelConfig =
                    { KernelConfig.Default with
                        FileSystem =
                            let name (s : string) = FileName.parseOrFail "test seed" s

                            Map.ofList
                                [
                                    name "f",
                                    SeedEntry.file (Text.Encoding.UTF8.GetBytes "hello" |> ImmutableArray.CreateRange)
                                ]
                    }
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // `SystemNative_PRead`'s contract at the syscall boundary,
                // including the *order* its checks run in. Impure because that
                // order is Linux's — on a two-fault input such as a negative
                // offset with a bad fd, Linux answers EINVAL and Darwin EBADF —
                // and because several arms are about PawPrint's own simulated
                // fd table and address space. The single-fault rows all agree
                // across platforms; `ReadAllBytesSeeded.cs` is the differential
                // half.
                FileName = "PReadRawSeeded.cs"
                ExpectedReturnCode = 0
                KernelConfig =
                    { KernelConfig.Default with
                        FileSystem =
                            let name (s : string) = FileName.parseOrFail "test seed" s

                            Map.ofList
                                [
                                    name "f",
                                    SeedEntry.file (Text.Encoding.UTF8.GetBytes "hello" |> ImmutableArray.CreateRange)
                                    name "d", SeedEntry.directory Map.empty
                                ]
                    }
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // The write path's three unarbitrable parts: the descriptor whose
                // errno the platforms disagree about (fd 0, a pipe's read end,
                // where Linux lets unseekability win and Darwin unwritability);
                // everything that turns on a file's mode, since a privileged
                // process bypasses those rules and the oracle's uid is not this
                // suite's to choose, where PawPrint's is configuration; and the
                // timestamps a write does and does not move, which need
                // PawPrint's deterministic clock to state without racing a real
                // filesystem's granularity.
                // `WriteSeeded.cs` is the differential half.
                FileName = "PWriteRawSeeded.cs"
                ExpectedReturnCode = 0
                KernelConfig =
                    { KernelConfig.Default with
                        FileSystem =
                            let name (s : string) = FileName.parseOrFail "test seed" s

                            let bytes (s : string) =
                                Text.Encoding.UTF8.GetBytes s |> ImmutableArray.CreateRange

                            let mode (m : int) =
                                PermissionBits.parseOrFail "test seed" m

                            Map.ofList
                                [
                                    name "f", SeedEntry.file (bytes "hello")
                                    name "ro", SeedEntry.File (bytes "hello", mode 0o444)
                                    name "wo", SeedEntry.File (bytes "hello", mode 0o200)
                                    // Set-user-ID; set-group-ID on a
                                    // group-executable file, which is the shape
                                    // whose bit a write strips; and sticky, which
                                    // it must not.
                                    name "suid", SeedEntry.File (bytes "hello", mode 0o4755)
                                    name "sgid", SeedEntry.File (bytes "hello", mode 0o2755)
                                    name "sticky", SeedEntry.File (bytes "hello", mode 0o1755)
                                ]
                    }
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // The write path's Darwin arms, which the case above cannot reach
                // because it runs on the default Linux flavour: which of
                // unwritability and unseekability wins for a pipe, and whether the
                // access mode is settled before a negative offset (it is for
                // `pread` on Darwin and not for `pwrite`, where Linux checks the
                // offset first for both).
                //
                // Configured as macOS for the same reason `SpliceLengthSeeded.cs`
                // is: on the default kernel these rows have different answers, so
                // there is no flavour that exercises both files.
                FileName = "WriteDarwinSeeded.cs"
                ExpectedReturnCode = 0
                KernelConfig =
                    { KernelConfig.Default with
                        UnixPlatform = SimulatedUnixPlatform.macOsArm64
                        FileSystem =
                            Map.ofList
                                [
                                    FileName.parseOrFail "test seed" "f",
                                    SeedEntry.file (Text.Encoding.UTF8.GetBytes "hello" |> ImmutableArray.CreateRange)
                                ]
                    }
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // A socket event port as a descriptor: that
                // `SystemNative_CreateSocketEventPort` allocates one like any
                // other fd, that `dup`/`close` treat it like any other, and what
                // the ordinary file operations answer for one under the Linux
                // flavour.
                //
                // Not differential: the descriptor numbers are unpredictable
                // under the oracle (OpenFdNumbering.cs gives the reason), and
                // every errno row but pread/pwrite is flavour-dependent.
                FileName = "SocketEventPortLinux.cs"
                ExpectedReturnCode = 0
                KernelConfig = KernelConfig.Default
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // The `bind(2)` rows the flavours answer differently: which
                // addresses are local, which declared lengths are accepted, and
                // -- the subtle one -- which of two simultaneous faults gets
                // reported, since the two check in different orders.
                FileName = "SocketBindLinux.cs"
                ExpectedReturnCode = 0
                KernelConfig = KernelConfig.Default
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // The same rows under Darwin, where every one of them inverts.
                FileName = "SocketBindDarwin.cs"
                ExpectedReturnCode = 0
                KernelConfig =
                    { KernelConfig.Default with
                        UnixPlatform = SimulatedUnixPlatform.macOsArm64
                    }
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // The raw errno numbers `accept(2)`'s failures leave, under the
                // Linux flavour. The classification is flavour-independent
                // (SocketAccept.cs carries it differentially); the numbers are
                // not -- EAGAIN 11/35, ENOTSOCK 88/38, EOPNOTSUPP 95/102.
                FileName = "SocketAcceptLinux.cs"
                ExpectedReturnCode = 0
                KernelConfig = KernelConfig.Default
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // The same rows under Darwin's numbering.
                FileName = "SocketAcceptDarwin.cs"
                ExpectedReturnCode = 0
                KernelConfig =
                    { KernelConfig.Default with
                        UnixPlatform = SimulatedUnixPlatform.macOsArm64
                    }
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // The epoll-only rows of
                // `SystemNative_TryChangeSocketEventRegistration`: EEXIST, the
                // (fd, description) registration key, EPERM for a file target,
                // EINVAL for a non-epoll port, and the check orderings —
                // everything kqueue answers differently, measured with a C
                // probe on Linux 6.18.5 and the guest itself on real Linux
                // .NET. The flavour-agreement rows live differentially in
                // sourcesPure/SocketEventRegistration.cs. No Darwin twin: the
                // Darwin arm is a refusal, which no exit-code guest can
                // observe.
                FileName = "SocketEventRegistrationLinux.cs"
                ExpectedReturnCode = 0
                KernelConfig = KernelConfig.Default
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // The bytes `SocketAddressPal` lays a `sockaddr_in` and
                // `sockaddr_in6` out in, under the Linux flavour. Not
                // differential: the family field is two bytes at offset 0 here
                // and one byte at offset 1 on Darwin, so no claim about these
                // bytes holds on both. `SocketAddressRoundTrip.cs` carries what
                // does.
                FileName = "SocketAddressLinuxBytes.cs"
                ExpectedReturnCode = 0
                KernelConfig = KernelConfig.Default
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // The same rows under Darwin, where `sa_len` occupies byte 0 and
                // `AF_INET6` is 30. Configured as macOS for the same reason
                // `SocketCreateDarwin.cs` is; the pair is what makes the flavour
                // split load-bearing in both directions rather than only pinning
                // the default.
                FileName = "SocketAddressDarwinBytes.cs"
                ExpectedReturnCode = 0
                KernelConfig =
                    { KernelConfig.Default with
                        UnixPlatform = SimulatedUnixPlatform.macOsArm64
                    }
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // The `SystemNative_Socket` rows true only under the Linux
                // flavour — the Unix-domain sockets Darwin refuses — together
                // with the two descriptor-level rows a differential guest cannot
                // reach: `flock` on a socket, and `lseek`'s screen order.
                FileName = "SocketCreateLinux.cs"
                ExpectedReturnCode = 0
                KernelConfig = KernelConfig.Default
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // The two address families the shim's own `#ifdef`s compile out
                // on Darwin, so that the wrapper refuses them before any kernel
                // sees them. Configured as macOS for the same reason
                // `SocketEventPortDarwin.cs` is.
                FileName = "SocketCreateDarwin.cs"
                ExpectedReturnCode = 0
                KernelConfig =
                    { KernelConfig.Default with
                        UnixPlatform = SimulatedUnixPlatform.macOsArm64
                    }
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // The port rows Darwin answers differently: ENXIO rather than
                // EINVAL for read/write, and ESPIPE rather than a successful 0
                // for `lseek` — a kqueue is not seekable at all, where an epoll
                // descriptor is seekable-but-inert. Configured as macOS for the
                // same reason `WriteDarwinSeeded.cs` is.
                FileName = "SocketEventPortDarwin.cs"
                ExpectedReturnCode = 0
                KernelConfig =
                    { KernelConfig.Default with
                        UnixPlatform = SimulatedUnixPlatform.macOsArm64
                    }
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // `SystemNative_WaitForSocketEvents` under the Linux flavour:
                // `epoll_wait`'s four screens, the order they are applied in, and
                // the `*count = 0` sentinel each error row writes.
                //
                // Not differential: five of the eight rows of the entry point's
                // contract differ between the two kernels, so a differential guest
                // would have to agree with whichever kernel the test host is. The
                // rows both kernels agree on are in
                // `sourcesPure/SocketEventsWaitScreening.cs`.
                FileName = "SocketEventsWaitLinux.cs"
                ExpectedReturnCode = 0
                KernelConfig = KernelConfig.Default
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // The wait rows Darwin answers differently: `*count = -1` as the
                // error sentinel, EBADF rather than EINVAL for a live non-port
                // descriptor, and a `*count == 0` that succeeds immediately instead
                // of being rejected. Configured as macOS for the same reason
                // `SocketEventPortDarwin.cs` is.
                FileName = "SocketEventsWaitDarwin.cs"
                ExpectedReturnCode = 0
                KernelConfig =
                    { KernelConfig.Default with
                        UnixPlatform = SimulatedUnixPlatform.macOsArm64
                    }
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // The event-buffer stride under the epoll backend, seen through the
                // count at which PawPrint can no longer address the block. Not
                // differential: a real libc succeeds at every count here.
                FileName = "SocketEventBufferLinux.cs"
                ExpectedReturnCode = 0
                KernelConfig = KernelConfig.Default
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // The same rows under kqueue, whose 32-byte stride halves the
                // largest representable count. The last row of each file is the pair
                // that disagrees.
                FileName = "SocketEventBufferDarwin.cs"
                ExpectedReturnCode = 0
                KernelConfig =
                    { KernelConfig.Default with
                        UnixPlatform = SimulatedUnixPlatform.macOsArm64
                    }
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // The `SystemNative_LSeek` rows on which Linux and Darwin
                // disagree: the order `whence` validity and seekability are
                // checked in, and the errno for an offset that leaves `int64`.
                // PawPrint answers Linux's. Everything portable — which is most
                // of the syscall, and all of `SystemNative_Read` — is in
                // `ReadSeekSeeded.cs`, the differential half.
                FileName = "LSeekRawSeeded.cs"
                ExpectedReturnCode = 0
                KernelConfig =
                    { KernelConfig.Default with
                        FileSystem =
                            let name (s : string) = FileName.parseOrFail "test seed" s

                            Map.ofList
                                [
                                    name "f",
                                    SeedEntry.file (Text.Encoding.UTF8.GetBytes "hello" |> ImmutableArray.CreateRange)
                                ]
                    }
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // Pins `open(2)`'s lowest-free-descriptor rule against the
                // emulated kernel's own table. Impure because the *numbers*
                // are not cross-runtime: the oracle's process holds the
                // runtime's own descriptors, so its first open is not 3, and a
                // differential guest could assert only ">= 0" — which no wrong
                // allocator fails.
                FileName = "OpenFdNumbering.cs"
                ExpectedReturnCode = 0
                KernelConfig =
                    { KernelConfig.Default with
                        FileSystem =
                            let name (s : string) = FileName.parseOrFail "test seed" s

                            let file (contents : string) =
                                SeedEntry.file (Text.Encoding.UTF8.GetBytes contents |> ImmutableArray.CreateRange)

                            Map.ofList [ name "f", file "one" ; name "g", file "two" ; name "h", file "three" ]
                    }
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // `mkdir`'s flavour-dependent facts under a **Linux** kernel:
                // what a trailing separator costs, which mode bits survive, and
                // set-group-ID inheritance. Paired with the Darwin case below,
                // and neither alone can catch a handler that hardcodes one
                // platform's answers.
                //
                // The umask and the uid are set away from `KernelConfig`'s
                // defaults deliberately: a unit test hands the rules in by hand,
                // so only a guest can see that the handler reads
                // `Kernel.Umask` and `Kernel.UserId` rather than a constant.
                FileName = "MkDirWiringLinuxSeeded.cs"
                ExpectedReturnCode = 0
                KernelConfig =
                    { KernelConfig.Default with
                        Umask = PermissionBits.parseOrFail "test" 0o027
                        UserId = 1000u
                        FileSystem = mkDirWiringSeed
                    }
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // The same checks in the same order under the Darwin flavour,
                // where a trailing separator reaches past the final component --
                // including the row that creates a dangling link's target.
                FileName = "MkDirWiringDarwinSeeded.cs"
                ExpectedReturnCode = 0
                KernelConfig =
                    { KernelConfig.Default with
                        UnixPlatform = SimulatedUnixPlatform.macOsArm64
                        Umask = PermissionBits.parseOrFail "test" 0o027
                        UserId = 1000u
                        FileSystem = mkDirWiringSeed
                    }
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // `unlink`'s flavour-dependent facts under a **Linux** kernel:
                // which errno each refusal carries, and whether a trailing
                // separator reaches past a final symlink. Paired with the Darwin
                // case below, and neither alone can catch a handler that
                // hardcodes one platform's answers.
                //
                // The uid is set away from privileged deliberately: the two
                // orderings only differ on a directory whose parent the caller
                // may not write, so a privileged guest could not tell them apart.
                FileName = "UnlinkWiringLinuxSeeded.cs"
                ExpectedReturnCode = 0
                KernelConfig =
                    { KernelConfig.Default with
                        UserId = 1000u
                        FileSystem = unlinkWiringSeed
                    }
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // The same checks in the same order under the Darwin flavour,
                // where a trailing separator resolves the final symlink --
                // including the row that follows a link to the root and answers
                // EISDIR where Linux says ENOTDIR.
                FileName = "UnlinkWiringDarwinSeeded.cs"
                ExpectedReturnCode = 0
                KernelConfig =
                    { KernelConfig.Default with
                        UnixPlatform = SimulatedUnixPlatform.macOsArm64
                        UserId = 1000u
                        FileSystem = unlinkWiringSeed
                    }
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // What becomes of an inode whose last name has gone: freed if
                // nothing holds it, kept if something does. Not a fact any guest
                // can read, so the assertion is on the terminal state.
                FileName = "UnlinkReapSeeded.cs"
                ExpectedReturnCode = 0
                KernelConfig =
                    { KernelConfig.Default with
                        FileSystem = unlinkReapSeed
                    }
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = Some assertUnlinkReapedExactlyOne
            }
            {
                // The directory search bit at an unprivileged uid, and its uid-0
                // twin below. Between them the handler's privilege argument is
                // falsifiable in both directions; a unit test hands the walk its
                // privilege directly and so cannot see the wiring at all.
                FileName = "SearchPermissionSeeded.cs"
                ExpectedReturnCode = 0
                KernelConfig =
                    { KernelConfig.Default with
                        UserId = 1000u
                        FileSystem = searchPermissionSeed
                    }
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                FileName = "SearchPermissionRootSeeded.cs"
                ExpectedReturnCode = 0
                KernelConfig =
                    { KernelConfig.Default with
                        UserId = 0u
                        FileSystem = searchPermissionSeed
                    }
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // How a *relative* path is resolved: from the cwd's inode, not
                // by re-walking the cwd's own path under the guest's privilege.
                // Only a guest can see this; remove the exemption and the
                // interpreter fails loudly rather than answering differently.
                FileName = "SearchPermissionCwdSeeded.cs"
                ExpectedReturnCode = 0
                KernelConfig =
                    { KernelConfig.Default with
                        UserId = 1000u
                        CurrentDirectory = AbsoluteUnixPath.parseOrFail "test" "/outer/inner"
                        FileSystem = searchPermissionCwdSeed
                    }
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // `mkdir`'s permission rule from the other side: uid 0, where
                // the 0o555 directory the two wiring guests are refused by lets
                // root bind a name. Between them the three cases make the
                // `privileged` argument falsifiable in both directions.
                FileName = "MkDirPrivilegedSeeded.cs"
                ExpectedReturnCode = 0
                KernelConfig =
                    { KernelConfig.Default with
                        UserId = 0u
                        FileSystem = mkDirWiringSeed
                    }
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // The twelve-bit modes the differential oracle refuses, because
                // a host `chmod` may drop them. Pins that the model itself
                // carries them.
                FileName = "SpecialModeBitsSeeded.cs"
                ExpectedReturnCode = 0
                KernelConfig =
                    { KernelConfig.Default with
                        FileSystem =
                            let name (s : string) = FileName.parseOrFail "test seed" s

                            let mode (raw : int) =
                                PermissionBits.parseOrFail "test seed" raw

                            let bytes (contents : string) =
                                Text.Encoding.UTF8.GetBytes contents |> ImmutableArray.CreateRange

                            Map.ofList
                                [
                                    name "setuid", SeedEntry.File (bytes "x", mode 0o4755)
                                    name "setgid", SeedEntry.File (bytes "x", mode 0o2755)
                                    name "sticky", SeedEntry.Directory (Map.empty, mode 0o1777)
                                    name "plain", SeedEntry.file (bytes "x")
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

                                    yield name $"%s{prefix}target", SeedEntry.file ImmutableArray<byte>.Empty
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
                // Truncation's set-ID rule, one guest per flavour. The pair is
                // what closes the wiring: a handler that hardcoded either answer
                // instead of reading
                // `SimulatedUnixPlatform.setIdBitsOnTruncation` would still
                // satisfy every unit test (they pass the rule in by hand), the
                // host oracle (it compares the pure function) *and* one of these
                // two.
                FileName = "TruncateWiringLinuxSeeded.cs"
                ExpectedReturnCode = 0
                KernelConfig =
                    { KernelConfig.Default with
                        // The default already, stated explicitly because this
                        // case's whole subject is which flavour is configured.
                        UnixPlatform = SimulatedUnixPlatform.linuxX64
                        FileSystem = truncationModeSeed
                    }
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                FileName = "TruncateWiringDarwinSeeded.cs"
                ExpectedReturnCode = 0
                KernelConfig =
                    { KernelConfig.Default with
                        UnixPlatform = SimulatedUnixPlatform.macOsArm64
                        FileSystem = truncationModeSeed
                    }
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // A content-changing write's set-ID rule, one guest per flavour,
                // and the same pairing argument the truncation guests above make:
                // a `commitFileWrite` that hardcoded either answer instead of
                // reading `SimulatedUnixPlatform.setGroupIdOnWrite` would satisfy
                // every unit test (they pass the rule in by hand), the host
                // oracle (it compares the pure function) *and* one of these two.
                FileName = "WriteModeWiringLinuxSeeded.cs"
                ExpectedReturnCode = 0
                KernelConfig =
                    { KernelConfig.Default with
                        // The default already, stated explicitly because this
                        // case's whole subject is which flavour is configured.
                        UnixPlatform = SimulatedUnixPlatform.linuxX64
                        FileSystem = writeModeSeed
                    }
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                FileName = "WriteModeWiringDarwinSeeded.cs"
                ExpectedReturnCode = 0
                KernelConfig =
                    { KernelConfig.Default with
                        UnixPlatform = SimulatedUnixPlatform.macOsArm64
                        FileSystem = writeModeSeed
                    }
                AppContext = AppContextProperties.empty
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }
            {
                // Pins the *wiring* of the creating open: that the handler reads
                // `Kernel.Umask` and `Kernel.UnixPlatform` rather than
                // hardcoding the defaults. The unit tests pass
                // `CreatingOpenRules` in by hand, so only a guest can see this.
                //
                // Both knobs are set away from their defaults on purpose: macOS
                // because Linux gives the opposite answer for every directory
                // row, and umask 0o077 because 0o022 is what
                // `PermissionBits.defaultForRegularFile` already bakes in.
                FileName = "CreateWiringSeeded.cs"
                ExpectedReturnCode = 0
                KernelConfig =
                    { KernelConfig.Default with
                        UnixPlatform = SimulatedUnixPlatform.macOsArm64
                        Umask = PermissionBits.parseOrFail "test seed" 0o077
                        FileSystem =
                            [
                                FileName.parseOrFail "test seed" "f",
                                SeedEntry.file (
                                    Text.Encoding.UTF8.GetBytes "hello"
                                    |> System.Collections.Immutable.ImmutableArray.CreateRange
                                )
                                FileName.parseOrFail "test seed" "d", SeedEntry.directory Map.empty
                            ]
                            |> Map.ofList
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
                // This case declares *no* AppContext properties: the baseline is
                // supplied by the library itself, so a host that expresses no preference
                // still gets it.
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
                // Exception-handling regions in a dynamic method's body: the clauses come off the
                // resolver's `m_exceptions`, and each `catch` clause's type is a `DynamicScope`
                // index resolved when the method is first prepared. Every clause kind is here, and
                // each cleanup kind is exercised on both the normal and the exceptional path --
                // "decoded the regions and then unwound straight past the frame" passes the normal
                // half of both.
                FileName = "DynamicMethodExceptionRegions.cs"
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
                // The other side of clause resolution: a clause naming a type that cannot be one.
                // `BeginCatchBlock` accepts any `RuntimeType`, so this is emittable, and real .NET
                // refuses it when it compiles the method -- at the first invocation, and whether or
                // not anything ever throws. That last part is what rules out resolving clause types
                // lazily during dispatch, which would let the quiet case run.
                FileName = "DynamicMethodInvalidCatchClause.cs"
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
                // `ldtoken` with a type-shaped operand resolved against a `DynamicScope`. Unlike
                // every other type-shaped opcode, `ldtoken` accepts a type of any shape, so the
                // guest pins an open generic definition, a bare generic parameter and `System.Void`
                // — all three of which `DynamicScopeOperand.closedType` refuses on behalf of the
                // opcodes that do demand a closed type. Registered with the dynamic-code switch
                // overridden, like its siblings. Every expectation was measured against the host's
                // real .NET, which returns 0 for this program.
                FileName = "DynamicMethodLdtoken.cs"
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
                // Field-shaped operands resolved against a `DynamicScope` rather than against
                // metadata: all six of `ldfld`/`ldflda`/`stfld`/`ldsfld`/`ldsflda`/`stsfld`, over
                // static and instance fields, a corelib field, a closed generic instantiation, and
                // the two `InvalidProgramException`s an operand that names an open generic
                // definition or the wrong staticness produces. Registered with the dynamic-code
                // switch overridden, like its siblings. Every expectation was measured against the
                // host's real .NET, which returns 0 for this program.
                FileName = "DynamicMethodFieldToken.cs"
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
                // Method-shaped operands resolved against a `DynamicScope` rather than against
                // metadata: `call` naming a scope entry that is itself a `DynamicMethod`, including
                // a method naming *itself*. Registered with the dynamic-code switch overridden, like
                // its siblings. Every expectation was measured against the host's real .NET, which
                // returns 0 for this program.
                FileName = "DynamicMethodMethodToken.cs"
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
                // A `call` naming a `DynamicMethod` the guest never minted itself, which real .NET
                // mints from inside `ResolveToken` by running the guest's `GetMethodDescriptor`.
                // Registered with the dynamic-code switch overridden, like its siblings. Every
                // expectation was measured against the host's real .NET, which returns 0 for this
                // program.
                FileName = "DynamicMethodMintOnDemand.cs"
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
                // `Delegate.Method` on a delegate bound to a dynamic method. It is the one route
                // to `Delegate_FindMethodHandle` that does *not* reach that QCall: the MethodInfo
                // is cached in `_methodBase` by `DynamicMethod.CreateDelegate`, so
                // `Delegate.GetMethodImpl` answers from there. Registered so that the handler's
                // `FunctionPointerTarget.Dynamic` refusal stays unreachable — the short-circuit
                // lives in interpreted CoreLib IL, which nothing in PawPrint enforces.
                FileName = "DelegateMethodOnDynamicMethod.cs"
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
                // a close. Impure because the real CLR's multi-threaded fd
                // activity races the close + dup window in the NUnit test
                // process; the interpreter's deterministic single-threaded fd
                // table makes the assertions stable. The registry-level
                // invariants are independently covered by
                // TestFileDescriptorRegistry's property tests; this test
                // verifies the wiring from the P/Invoke handler through to the
                // registry.
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
                // Impure because the PAL values
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
