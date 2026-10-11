namespace WoofWare.PosixKernel.Test

open System
open System.Collections.Immutable
open System.IO
open System.Runtime.InteropServices
open System.Text
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `UnixInotify` and the calls that reach an inotify instance, against a real
/// Linux kernel: random sequences of `inotify_init1`, `inotify_add_watch`,
/// `inotify_rm_watch`, `read`, `poll`, `FIONREAD` and `close`, over a real
/// temporary directory and over the model holding the same files, must answer
/// alike, and the bytes each read returns must be equal.
///
/// Nothing in a sequence changes the filesystem: this kernel reports no change
/// to a watch yet, so the only records are the `IN_IGNORED` of a removed watch.
/// Linux only: CI's x86-64 is what checks the record layout on the other
/// architecture the library models. Elsewhere the tests are ignored.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestInotifyAgainstHost =

    [<Struct ; StructLayout(LayoutKind.Sequential)>]
    type private PollFd =
        val mutable Fd : int
        val mutable Events : int16
        val mutable Revents : int16

    [<DllImport("libc", EntryPoint = "inotify_init1", SetLastError = true)>]
    extern int private hostInit1(int flags)

    [<DllImport("libc", EntryPoint = "inotify_add_watch", SetLastError = true)>]
    extern int private hostAddWatch(int fd, byte[] path, uint32 mask)

    [<DllImport("libc", EntryPoint = "inotify_rm_watch", SetLastError = true)>]
    extern int private hostRmWatch(int fd, int wd)

    [<DllImport("libc", EntryPoint = "read", SetLastError = true)>]
    extern nativeint private hostRead(int fd, byte[] buffer, unativeint count)

    [<DllImport("libc", EntryPoint = "read", SetLastError = true)>]
    extern nativeint private hostReadAt(int fd, nativeint buffer, unativeint count)

    [<DllImport("libc", EntryPoint = "poll", SetLastError = true)>]
    extern int private hostPoll([<In ; Out>] PollFd[] fds, unativeint count, int timeout)

    [<DllImport("libc", EntryPoint = "close")>]
    extern int private hostClose(int fd)

    [<DllImport("libc", EntryPoint = "geteuid")>]
    extern uint32 private hostGetEUid()

    [<DllImport("libc", EntryPoint = "getegid")>]
    extern uint32 private hostGetEGid()

    // `ioctl(2)` is variadic, so FIONREAD goes through .NET's own fixed-arity
    // wrapper in its `System.Native` library.
    [<DllImport("libSystem.Native", EntryPoint = "SystemNative_GetBytesAvailable")>]
    extern int private hostBytesAvailable(nativeint fd, int& available)

    let private context : string = "TestInotifyAgainstHost"

    let private onLinux (action : SimulatedUnixPlatform -> unit) : unit =
        HostPlatform.onUnixHostKernel (fun platform ->
            match SimulatedUnixPlatform.flavour platform with
            | SimulatedUnixFlavour.Darwin -> Assert.Ignore "inotify is Linux's"
            | SimulatedUnixFlavour.Linux -> action platform
        )

    /// What the host answered: the count, or the model's name for the errno.
    let private hostAnswer (numbering : RawErrnoNumbering) (result : nativeint) : Result<int64, UnixError> =
        if result >= 0n then
            Ok (int64 result)
        else
            let errno = Marshal.GetLastPInvokeError ()

            match UnixError.ofRawErrnoUnder numbering errno with
            | Some error -> Error error
            | None -> failwith $"the host answered errno %d{errno}, which the model has no name for"

    let private modelAnswer (answer : SyscallAnswer) : Result<int64, UnixError> =
        match answer with
        | SyscallAnswer.Completed value -> Ok value
        | SyscallAnswer.Failed error -> Error error

    /// The paths a sequence watches, relative to the directory both sides hold
    /// the same files in: `d1`, `d2` and `nr` directories (`nr` with no read
    /// permission), a file `f`, a symbolic link `l` to `d1`, and names that
    /// walk to nothing.
    let private paths : string list =
        [ "d1" ; "d2" ; "f" ; "l" ; "d1/." ; "nr" ; "missing" ; "f/x" ; "" ]

    /// The mask bits a sequence may ask for: the events this kernel raises, the
    /// flags but `IN_ONESHOT`, the bits Linux adds to a record, and two bits
    /// `<sys/inotify.h>` does not define.
    let private maskBits : uint32 list =
        [
            0x2u
            0x4u
            0x40u
            0x80u
            0x100u
            0x200u
            0x400u
            0x800u
            0x01000000u
            0x02000000u
            0x04000000u
            0x10000000u
            0x20000000u
            0x4000u
            0x8000u
            0x40000000u
            0x1000u
            0x08000000u
        ]

    [<RequireQualifiedAccess>]
    type private Op =
        | Init of cloexec : bool
        | AddWatch of slot : int * path : int * mask : uint32
        | RmWatch of slot : int * wd : int
        | Read of slot : int * count : int * mapped : bool
        | Poll of slot : int * events : int16
        | BytesAvailable of slot : int
        | Close of slot : int

    let private opGen : Gen<Op> =
        let slot = Gen.choose (0, 3)

        let mask =
            Gen.frequency
                [
                    8,
                    Gen.subListOf maskBits
                    |> Gen.map (List.fold (|||) 0u)
                    |> Gen.map (fun mask ->
                        // Mostly without the undefined bits, so that most
                        // adds get past the screen.
                        mask &&& ~~~(0x1000u ||| 0x08000000u)
                    )
                    1, Gen.subListOf maskBits |> Gen.map (List.fold (|||) 0u)
                    1, Gen.constant 0u
                ]

        Gen.frequency
            [
                2, Gen.elements [ false ; true ] |> Gen.map Op.Init
                6, Gen.map3 (fun s p m -> Op.AddWatch (s, p, m)) slot (Gen.choose (0, paths.Length - 1)) mask
                3, Gen.map2 (fun s w -> Op.RmWatch (s, w)) slot (Gen.choose (-1, 6))
                3,
                Gen.map3
                    (fun s c m -> Op.Read (s, c, m))
                    slot
                    (Gen.elements [ 0 ; 15 ; 16 ; 17 ; 32 ; 48 ; 4096 ])
                    (Gen.frequency [ 5, Gen.constant true ; 1, Gen.constant false ])
                2,
                Gen.map2
                    (fun s e -> Op.Poll (s, e))
                    slot
                    (Gen.elements [ 0x1s ; 0x4s ; 0x1s ||| 0x4s ||| 0x2s ||| 0x2000s ])
                1, slot |> Gen.map Op.BytesAvailable
                1, slot |> Gen.map Op.Close
            ]

    [<Test>]
    let ``a real inotify and the model answer every call alike, byte for byte`` () : unit =
        onLinux (fun platform ->
            let numbering = SimulatedUnixPlatform.rawErrnoNumbering platform
            let uid = hostGetEUid ()
            let gid = hostGetEGid ()

            let owner =
                Credentials.ofIds (UserId.parseOrFail context uid) (GroupId.parseOrFail context gid) []

            let unique = Guid.NewGuid().ToString "N"
            let root = Path.Combine (Path.GetTempPath (), $"inotify-host-%s{unique}")
            Directory.CreateDirectory (Path.Combine (root, "d1")) |> ignore<DirectoryInfo>
            Directory.CreateDirectory (Path.Combine (root, "d2")) |> ignore<DirectoryInfo>
            Directory.CreateDirectory (Path.Combine (root, "nr")) |> ignore<DirectoryInfo>
            File.WriteAllText (Path.Combine (root, "f"), "x")

            File.CreateSymbolicLink (Path.Combine (root, "l"), "d1")
            |> ignore<FileSystemInfo>

            File.SetUnixFileMode (Path.Combine (root, "nr"), UnixFileMode.UserWrite ||| UnixFileMode.UserExecute)

            try
                let name (text : string) =
                    DirectoryEntryName.parseOrFail context text

                let bits (mode : int) = PermissionBits.parseOrFail context mode

                let seed =
                    Map.ofList
                        [
                            name "d1", SeedEntry.Directory (Map.empty, bits 0o755, None)
                            name "d2", SeedEntry.Directory (Map.empty, bits 0o755, None)
                            name "nr", SeedEntry.Directory (Map.empty, bits 0o300, None)
                            name "f", SeedEntry.File (ImmutableArray.CreateRange "x"B, bits 0o644, None)
                            name "l", SeedEntry.Symlink (SymlinkTarget.parseOrFail context "d1", None)
                        ]

                let initial : UnixSystem<int, string> =
                    UnixSystem.initial platform
                    |> Launched.fileSystem
                        (UnixTimestamp.ofMillisecondsSinceEpoch 0L)
                        {
                            User = UserId.parseOrFail context uid
                            Group = GroupId.parseOrFail context gid
                        }
                        seed
                    |> Launched.bootWith (Launched.credentials owner) UnixSystem.pipedStandardStreams 0 (CpuId 0)

                let property (ops : Op list) : unit =
                    // Each slot: the host's descriptor and the model's, while open.
                    let slots = Array.create 4 None
                    let mutable system = initial
                    let nonBlock = 0x800
                    let cloexec = 0x80000

                    let compare (op : Op) (host : Result<int64, UnixError>) (model : Result<int64, UnixError>) =
                        if host <> model then
                            failwith $"%A{op}: the host answered %A{host}, the model %A{model}"

                    try
                        for op in ops do
                            match op with
                            | Op.Init cloexecFlag ->
                                match Array.tryFindIndex Option.isNone slots with
                                | None -> ()
                                | Some free ->
                                    let flags = nonBlock ||| (if cloexecFlag then cloexec else 0)
                                    let host = hostAnswer numbering (nativeint (hostInit1 flags))

                                    let model =
                                        match UnixInotify.inotifyInit1 flags system with
                                        | Ok (Ok (fd, after)) ->
                                            system <- after
                                            Ok (int64 fd)
                                        | Ok (Error error) -> Error error
                                        | Error refusal -> failwith $"%A{refusal}"

                                    match host, model with
                                    | Ok hostFd, Ok modelFd -> slots.[free] <- Some (int hostFd, int modelFd)
                                    | _ -> compare op host model
                            | Op.AddWatch (slot, path, mask) ->
                                let relative = paths.[path]
                                let hostPath = if relative = "" then "" else Path.Combine (root, relative)
                                let hostBytes = Array.append (Encoding.UTF8.GetBytes hostPath) [| 0uy |]
                                let hostFd, modelFd = slots.[slot] |> Option.defaultValue (999, 999)
                                let host = hostAnswer numbering (nativeint (hostAddWatch (hostFd, hostBytes, mask)))

                                let model =
                                    match
                                        UnixInotify.inotifyAddWatch modelFd (PathArg.ofText relative) mask system
                                    with
                                    | Ok (answer, after) ->
                                        system <- after
                                        modelAnswer answer
                                    | Error refusal ->
                                        failwith $"%A{op}: refused: %s{InotifyAddWatchRefusal.describe refusal}"

                                compare op host model
                            | Op.RmWatch (slot, wd) ->
                                let hostFd, modelFd = slots.[slot] |> Option.defaultValue (999, 999)
                                let host = hostAnswer numbering (nativeint (hostRmWatch (hostFd, wd)))

                                let model =
                                    match UnixInotify.inotifyRmWatch modelFd wd system with
                                    | Ok (answer, after) ->
                                        system <- after
                                        modelAnswer answer
                                    | Error refusal -> failwith $"%A{refusal}"

                                compare op host model
                            | Op.Read (slot, count, mapped) ->
                                let hostFd, modelFd = slots.[slot] |> Option.defaultValue (999, 999)
                                let buffer = Array.zeroCreate<byte> (max count 1)

                                let host =
                                    if mapped then
                                        hostAnswer numbering (hostRead (hostFd, buffer, unativeint count))
                                    else
                                        hostAnswer numbering (hostReadAt (hostFd, 0n, unativeint count))

                                let model =
                                    match
                                        UnixReadWrite.read
                                            0
                                            modelFd
                                            (if mapped then
                                                 UserBuffer.Mapped
                                             else
                                                 UserBuffer.Unmapped 0UL)
                                            (uint64 count)
                                            system
                                    with
                                    | Ok (ReadOutcome.Answered (ReadAnswer.Completed bytes), after) ->
                                        system <- after

                                        match host with
                                        | Ok n ->
                                            let hostBytes = buffer.[0 .. int n - 1]

                                            if hostBytes <> bytes.AsSpan().ToArray () then
                                                failwith
                                                    $"%A{op}: the host read %A{hostBytes}, the model %A{bytes.AsSpan().ToArray ()}"
                                        | Error _ -> ()

                                        Ok (int64 bytes.Length)
                                    | Ok (ReadOutcome.Answered (ReadAnswer.Failed error), after) ->
                                        system <- after
                                        Error error
                                    | other -> failwith $"%A{op}: %A{other}"

                                compare op host model
                            | Op.Poll (slot, events) ->
                                let hostFd, modelFd = slots.[slot] |> Option.defaultValue (999, 999)
                                let fds = [| PollFd (Fd = hostFd, Events = events, Revents = 0s) |]
                                let hostCount = hostPoll (fds, 1un, 0)

                                match
                                    UnixPoll.poll
                                        0
                                        [
                                            {
                                                Fd = modelFd
                                                Events = events
                                            }
                                        ]
                                        0
                                        system
                                with
                                | Ok (PollOutcome.Answered ([ revents ], count), _) ->
                                    (hostCount, fds.[0].Revents) |> shouldEqual (count, revents)
                                | other -> failwith $"%A{op}: %A{other}"
                            | Op.BytesAvailable slot ->
                                match slots.[slot] with
                                | None -> ()
                                | Some (hostFd, modelFd) ->
                                    let mutable available = 0
                                    hostBytesAvailable (nativeint hostFd, &available) |> shouldEqual 0

                                    match UnixDescriptor.bytesAvailable modelFd UserBuffer.Mapped system with
                                    | Ok (BytesAvailableAnswer.Reported count) -> count |> shouldEqual available
                                    | other -> failwith $"%A{op}: %A{other}"
                            | Op.Close slot ->
                                match slots.[slot] with
                                | None -> ()
                                | Some (hostFd, modelFd) ->
                                    hostClose hostFd |> shouldEqual 0

                                    match UnixDescriptor.close modelFd system with
                                    | Ok (SyscallAnswer.Completed 0L, after) -> system <- after
                                    | other -> failwith $"%A{op}: %A{other}"

                                    slots.[slot] <- None

                        UnixSystem.checkInvariants system |> shouldEqual []
                    finally
                        for slot in slots do
                            match slot with
                            | Some (hostFd, _) -> hostClose hostFd |> ignore<int>
                            | None -> ()

                let gen =
                    gen {
                        // Begin with an instance, so that most calls reach one.
                        let! rest = Gen.listOf opGen |> Gen.resize 30
                        return Op.Init false :: rest
                    }

                Check.One (Config.QuickThrowOnFailure.WithMaxTest 300, Prop.forAll (Arb.fromGen gen) property)
            finally
                File.SetUnixFileMode (
                    Path.Combine (root, "nr"),
                    UnixFileMode.UserRead ||| UnixFileMode.UserWrite ||| UnixFileMode.UserExecute
                )

                Directory.Delete (root, true)
        )

    [<Test>]
    let ``inotify_init1's flag screen is this host's`` () : unit =
        onLinux (fun platform ->
            let numbering = SimulatedUnixPlatform.rawErrnoNumbering platform

            let system : UnixSystem<int, string> =
                UnixSystem.initial platform
                |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

            let flags =
                [
                    yield 0
                    yield -1
                    yield Int32.MinValue
                    for bit in 0..31 do
                        yield 1 <<< bit
                        yield 0x80000 ||| (1 <<< bit)
                ]

            for flags in flags do
                let fd = hostInit1 flags
                let host = hostAnswer numbering (nativeint fd)

                if fd >= 0 then
                    hostClose fd |> ignore<int>

                let model =
                    match UnixInotify.inotifyInit1 flags system with
                    | Ok (Ok _) -> true
                    | Ok (Error UnixError.EINVAL) -> false
                    | other -> failwith $"0x%x{flags}: %A{other}"

                if host.IsOk <> model then
                    failwith $"flags 0x%x{flags}: the host answered %A{host}, the model created %b{model}"
        )
