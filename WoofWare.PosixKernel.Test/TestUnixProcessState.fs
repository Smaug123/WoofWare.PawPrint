namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// The process record, exercised directly rather than through a client.
///
/// Both type parameters are `SignalState`'s, and these rows instantiate them at
/// `int` and `string` — which is the point: naming a scheduling entity and
/// naming a signal handler are the client's business, and nothing here knows
/// what PawPrint's `ThreadId` or `SignalHandler` is.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestUnixProcessState =

    let private rootInode : InodeNumber = InodeNumber 1L

    let private context : string = "TestUnixProcessState"

    /// A registration watching `EPOLLIN` alone, edge-triggered, as the kernel
    /// stores it: with `EPOLLERR` and `EPOLLHUP` added.
    let private readInterest : EpollRegistration =
        {
            Events =
                EpollEvents.In
                ||| EpollEvents.EdgeTriggered
                ||| EpollEvents.Err
                ||| EpollEvents.Hup
            Data = 0UL
            RegisteredAt = 0L
        }

    /// A process holding nothing but its current directory: the least a client
    /// has to supply for any of these operations to mean something.
    let private empty : UnixProcessState<int, string> =
        {
            FileDescriptors = FileDescriptorRegistry.initial
            OutputLog = ImmutableArray<OutputLogEntry>.Empty
            Environment = []
            CurrentDirectoryInode = rootInode
            ProcessPath = None
            DirectoryStreams = Map.empty
            NextDirectoryStreamId = DirectoryStreamId 0L
            Credentials = Credentials.ofIds (UserId.parseOrFail context 1000u) (GroupId.parseOrFail context 1000u) []
            Umask = PermissionBits.parseOrFail context 0o022
            ProcessId = ProcessId.parseOrFail context 4242
            Signals = SignalState.initial SignalNumbering.Linux
        }

    [<Test>]
    let ``the signal state is keyed by whatever the client names tasks`` () : unit =
        // The claim the two type parameters exist to make. `int` names a task and
        // `string` is a handler; a record that had kept PawPrint's `ThreadId` and
        // `SignalHandler` would not compile here at all.
        let proc =
            { empty with
                Signals =
                    empty.Signals
                    |> SignalState.setHandler "sigaction"
                    |> SignalState.block 7 Signal.SIGTERM
            }

        SignalState.handler proc.Signals |> shouldEqual (Some "sigaction")
        SignalState.isBlocked 7 Signal.SIGTERM proc.Signals |> shouldEqual true
        SignalState.isBlocked 8 Signal.SIGTERM proc.Signals |> shouldEqual false

    /// An environment entry: arbitrary non-NUL bytes, drawn from a small pool
    /// often enough that duplicates turn up, and including the shapes a
    /// `NAME=VALUE` reading would treat specially (no `=`, a leading `=`, empty).
    let private genEntry : Gen<UnixByteString> =
        let ofBytes (bytes : byte array) : UnixByteString =
            match UnixByteString.ofBytes (ImmutableArray.Create<byte> bytes) with
            | Ok s -> s
            | Error defect -> failwith $"generator produced a NUL: %s{UnixByteString.describe defect}"

        Gen.frequency
            [
                2,
                Gen.elements [ "A=1" ; "A=2" ; "A" ; "=A" ; "" ; "B==" ]
                |> Gen.map (fun s -> ofBytes (System.Text.Encoding.ASCII.GetBytes s))
                3,
                ArbMap.defaults
                |> ArbMap.generate<byte>
                |> Gen.filter (fun b -> b <> 0uy)
                |> Gen.listOf
                |> Gen.map (List.toArray >> ofBytes)
            ]

    [<Test>]
    let ``the environment is exactly the entries it was set to, in order`` () : unit =
        // Replacement, not an overlay: whatever the process held before is gone,
        // and nothing is merged, sorted or de-duplicated.
        let mutable withDuplicates = 0

        let property (before : UnixByteString list, after : UnixByteString list) : unit =
            let proc =
                empty
                |> UnixProcessState.withEnvironment context before
                |> UnixProcessState.withEnvironment context after

            proc.Environment |> shouldEqual after

            if List.length (List.distinct after) < List.length after then
                withDuplicates <- withDuplicates + 1

        let gen = Gen.zip (Gen.listOf genEntry) (Gen.listOf genEntry)
        Check.One (Config.QuickThrowOnFailure.WithMaxTest 500, Prop.forAll (Arb.fromGen gen) property)

        // Duplicates are the case a map would silently collapse.
        withDuplicates > 20 |> shouldEqual true

    [<Test>]
    let ``a forged entry is refused under the caller's name for it`` () : unit =
        // The context string is the client's, not this library's: a host that has
        // to fix one of these knows the table by whatever its own configuration
        // calls it.
        let exn =
            Assert.Throws<exn> (fun () ->
                UnixProcessState.withEnvironment
                    "whatever the client calls it"
                    [ UnixByteString.empty ; Unchecked.defaultof<UnixByteString> ]
                    empty
                |> ignore<UnixProcessState<int, string>>
            )

        exn.Message |> shouldContainText "whatever the client calls it"

    [<Test>]
    let ``a forged path is refused under the caller's name`` () : unit =
        // `AbsoluteUnixPath` hides its case, so the only invalid value a client
        // can produce is a defaulted one; this setter is where it stops.
        //
        // `withUmask`'s guard has no such row, and cannot have one: a defaulted
        // `PermissionBits` is 0o000, which is `umask 000` and perfectly legal
        // (`PermissionBits.assertValid` says so itself). The only value it
        // refuses is an out-of-range word, and the case being private means no
        // caller outside this assembly can build one.
        let exn =
            Assert.Throws<exn> (fun () ->
                UnixProcessState.withProcessPath
                    "the client's name for the path"
                    (Some Unchecked.defaultof<AbsoluteUnixPath>)
                    empty
                |> ignore<UnixProcessState<int, string>>
            )

        exn.Message |> shouldContainText "the client's name for the path"

    [<Test>]
    let ``no path is an answer rather than a request for a default`` () : unit =
        let proc =
            { empty with
                ProcessPath = Some (AbsoluteUnixPath.parseOrFail context "/bin/guest")
            }
            |> UnixProcessState.withProcessPath context None

        proc.ProcessPath |> shouldEqual None

    [<Test>]
    let ``the process's privilege is its credentials' privilege`` () : unit =
        let property (credentials : Credentials) : unit =
            UnixProcessState.callerPrivilege
                { empty with
                    Credentials = credentials
                }
            |> shouldEqual (Credentials.privilege credentials)

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 200,
            Prop.forAll (Arb.fromGen CredentialsGen.credentials) property
        )

    [<Test>]
    let ``every reference this record holds keeps its inode alive`` () : unit =
        // One of each kind that can hold one, and one of each that cannot, so a
        // rule that answered "every description" or "no description" fails.
        let fileInode = InodeNumber 7L
        let streamInode = InodeNumber 9L

        let _fd, withFile =
            FileDescriptorRegistry.openFile fileInode FileAccessMode.ReadOnly FileDescriptorRegistry.initial

        let _sock, withSocket = FileDescriptorRegistry.createSocket (SocketId 1L) withFile

        let streamFd, registry = FileDescriptorRegistry.createSocketEventPort withSocket

        let proc =
            { empty with
                FileDescriptors = registry
                DirectoryStreams =
                    Map.ofList
                        [
                            DirectoryStreamId 0L,
                            {
                                Fd = streamFd
                                Inode = streamInode
                                Cursor = DirectoryCursor.Start
                            }
                        ]
            }

        UnixProcessState.heldInodes proc
        |> shouldEqual (Set.ofList [ rootInode ; fileInode ; streamInode ])

    [<Test>]
    let ``a socket is named by exactly the descriptions that name it`` () : unit =
        let watched = SocketId 1L
        let other = SocketId 2L

        let watchedFd, registry =
            FileDescriptorRegistry.createSocket watched FileDescriptorRegistry.initial

        let _otherFd, registry = FileDescriptorRegistry.createSocket other registry
        let portFd, registry = FileDescriptorRegistry.createSocketEventPort registry

        let proc =
            { empty with
                FileDescriptors = registry
            }

        UnixProcessState.descriptionsNamingSocket watched proc
        |> Set.count
        |> shouldEqual 1

        UnixProcessState.descriptionsNamingSocket (SocketId 3L) proc
        |> shouldEqual Set.empty

    [<Test>]
    let ``a state-change wake queues every registration of the socket`` () : unit =
        let watched = SocketId 1L

        let watchedFd, registry =
            FileDescriptorRegistry.createSocket watched FileDescriptorRegistry.initial

        let portFd, registry = FileDescriptorRegistry.createSocketEventPort registry

        let idOf (fd : int) : OpenFileDescriptionId =
            match FileDescriptorRegistry.tryFindId fd registry with
            | Some id -> id
            | None -> failwith $"fd %d{fd} is not live"

        let registry =
            FileDescriptorRegistry.addEpollRegistration (idOf portFd) (watchedFd, idOf watchedFd) readInterest registry

        let ready (proc : UnixProcessState<int, string>) : (int * OpenFileDescriptionId) list =
            FileDescriptorRegistry.descriptions proc.FileDescriptors
            |> Map.toSeq
            |> Seq.collect (fun (_, description) ->
                match description.Target with
                | OpenFileTarget.SocketEventPort portState -> portState.Ready
                | _ -> []
            )
            |> List.ofSeq

        let proc =
            { empty with
                FileDescriptors = registry
            }

        ready proc |> shouldEqual []

        let woken = UnixProcessState.signalSocketStateChange watched proc
        ready woken |> List.length |> shouldEqual 1

        // A socket nothing watches wakes nothing.
        ready (UnixProcessState.signalSocketStateChange (SocketId 2L) proc)
        |> shouldEqual []
