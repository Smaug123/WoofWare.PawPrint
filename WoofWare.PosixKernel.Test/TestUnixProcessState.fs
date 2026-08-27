namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
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

    /// A process holding nothing but its current directory: the least a client
    /// has to supply for any of these operations to mean something.
    let private empty : UnixProcessState<int, string> =
        {
            FileDescriptors = FileDescriptorRegistry.initial
            OutputLog = ImmutableArray<OutputLogEntry>.Empty
            Environment = Map.empty
            CurrentDirectory = AbsoluteUnixPath.parseOrFail context "/"
            CurrentDirectoryInode = rootInode
            ProcessPath = None
            DirectoryStreams = Map.empty
            NextDirectoryStreamId = DirectoryStreamId 0L
            UserId = 1000u
            GroupId = 1000u
            Umask = PermissionBits.parseOrFail context 0o022
            Signals = SignalState.empty
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

    [<Test>]
    let ``an overlay wins over what the process already holds`` () : unit =
        let proc =
            empty
            |> UnixProcessState.withEnvironment context (Map.ofList [ "KEEP", "1" ; "REPLACE", "old" ])
            |> UnixProcessState.withEnvironment context (Map.ofList [ "REPLACE", "new" ; "ADD", "2" ])

        proc.Environment
        |> shouldEqual (Map.ofList [ "KEEP", "1" ; "REPLACE", "new" ; "ADD", "2" ])

    [<Test>]
    let ``an entry no environ could hold is refused, under the caller's name for it`` () : unit =
        // The context string is the client's, not this library's: a host that has
        // to fix one of these knows the table by whatever its own configuration
        // calls it. Asserting the *caller's* string comes back is what stops the
        // parameter being quietly ignored in favour of a hard-coded prefix.
        for name, value in [ "", "v" ; "A=B", "v" ; "A\000B", "v" ; "A", "v\000w" ] do
            let exn =
                Assert.Throws<exn> (fun () ->
                    UnixProcessState.withEnvironment "whatever the client calls it" (Map.ofList [ name, value ]) empty
                    |> ignore<UnixProcessState<int, string>>
                )

            exn.Message |> shouldContainText "whatever the client calls it"
            exn.Message |> shouldContainText "refusing to install"

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
    let ``only uid 0 is privileged`` () : unit =
        UnixProcessState.callerPrivilege empty
        |> shouldEqual CallerPrivilege.Unprivileged

        UnixProcessState.callerPrivilege
            { empty with
                UserId = 0u
            }
        |> shouldEqual CallerPrivilege.Privileged

        // Group 0 is not root: `IsPrivilegedProcess` reads the *user* id.
        UnixProcessState.callerPrivilege
            { empty with
                GroupId = 0u
            }
        |> shouldEqual CallerPrivilege.Unprivileged

    [<Test>]
    let ``the ids a setter writes are the ids it was given`` () : unit =
        let proc = empty |> UnixProcessState.withUserAndGroupId 3u 5u
        proc.UserId |> shouldEqual 3u
        proc.GroupId |> shouldEqual 5u

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

        // Nothing is registered yet, so no port could deliver the difference.
        UnixProcessState.socketIsRegisteredWithAnyEventPort watched proc
        |> shouldEqual false

        let registered =
            FileDescriptorRegistry.changeSocketEventRegistration
                portFd
                watchedFd
                0L
                (SocketEventRegistrationChange.Add (SocketEventInterest.ofBits context 0x01, 0UL))
                registry
            |> function
                | Ok registry -> registry
                | Error error -> failwith $"could not register: %O{error}"

        let proc =
            { proc with
                FileDescriptors = registered
            }

        UnixProcessState.socketIsRegisteredWithAnyEventPort watched proc
        |> shouldEqual true
        // The registration is of one socket, not of the port's whole table.
        UnixProcessState.socketIsRegisteredWithAnyEventPort other proc
        |> shouldEqual false

    [<Test>]
    let ``a state-change wake queues every registration of the socket`` () : unit =
        let watched = SocketId 1L

        let watchedFd, registry =
            FileDescriptorRegistry.createSocket watched FileDescriptorRegistry.initial

        let portFd, registry = FileDescriptorRegistry.createSocketEventPort registry

        let registry =
            FileDescriptorRegistry.changeSocketEventRegistration
                portFd
                watchedFd
                0L
                (SocketEventRegistrationChange.Add (SocketEventInterest.ofBits context 0x01, 0UL))
                registry
            |> function
                | Ok registry -> registry
                | Error error -> failwith $"could not register: %O{error}"

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
