namespace WoofWare.PosixKernel.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// A kqueue belongs to the process that created it (`KqueueState.Owner`): its
/// registrations name descriptors in that process's table, so a close purges
/// only the closing process's kqueues; and each is attached to the socket its
/// descriptor named (`KqueueRegistration.Socket`), so an event on the socket
/// reaches it whichever process's view the event happens in.
///
/// Another process's kqueue here is one whose recorded owner has been
/// rewritten; `TestCrossProcess` drives two real processes.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestKqueueOwner =

    let private other : ProcessId = ProcessId.parseOrFail "test" 9

    let private registration (socket : SocketId) : KqueueRegistration =
        {
            Clear = true
            Receipt = false
            UserData = 0UL
            RegisteredAt = 0L
            Socket = socket
        }

    let private socketOf (fd : int) (system : UnixSystem<int, string>) : SocketId =
        match FileDescriptorRegistry.tryFindTarget fd (UnixSystemState.fileDescriptors system) with
        | Some (OpenFileTarget.Socket socket) -> socket
        | other -> failwith $"fd %d{fd} names %A{other}"

    /// A Darwin system with a listener, and a kqueue that registers the
    /// listener's `EVFILT_READ` through the listener's descriptor and is owned by
    /// `owner`: the listener's descriptor, the kqueue's, and the system.
    let private world (owner : ProcessId option) : int * int * UnixSystem<int, string> =
        let listener, system = KeventWorld.listenerAt 8080us KeventWorld.darwin
        let kqueue, system = KeventWorld.kqueue system
        let id = KeventWorld.idOf kqueue system

        let state =
            {
                Owner = owner |> Option.defaultValue system.Process.ProcessId
                Drained = false
                Registrations = Map.ofList [ (listener, KqueueFilter.Read), registration (socketOf listener system) ]
                Active = []
            }

        // The registration's ordinal, 0, is one the machine has minted.
        let system =
            UnixSystemState.mapOpenFiles (OpenFileTable.setKqueueState id state) system
            |> fun system ->
                { system with
                    Machine =
                        { system.Machine with
                            NextEventRegistrationOrdinal = 1L
                        }
                }

        listener, kqueue, system

    let private registrationsOf (kqueue : int) (system : UnixSystem<int, string>) =
        match FileDescriptorRegistry.tryFindTarget kqueue (UnixSystemState.fileDescriptors system) with
        | Some (OpenFileTarget.Kqueue state) -> state.Registrations
        | other -> failwith $"fd %d{kqueue} names %A{other}"

    [<Test>]
    let ``kqueue records the calling process as its owner`` () : unit =
        let pid = ProcessId.parseOrFail "test" 77

        let kqueue, system =
            KeventWorld.darwinWith (Launched.processId pid) |> KeventWorld.kqueue

        match FileDescriptorRegistry.tryFindTarget kqueue (UnixSystemState.fileDescriptors system) with
        | Some (OpenFileTarget.Kqueue state) -> state.Owner |> shouldEqual pid
        | other -> failwith $"fd %d{kqueue} names %A{other}"

    [<Test>]
    let ``a close purges registrations made through the descriptor from the closing process's kqueues alone``
        ()
        : unit
        =
        for owner, purged in [ None, true ; Some other, false ] do
            let listener, kqueue, system = world owner

            let registry =
                match
                    FileDescriptorRegistry.dropDescriptor
                        system.Process.ProcessId
                        listener
                        (UnixSystemState.fileDescriptors system)
                with
                | Ok (registry, _) -> registry
                | Error error -> failwith $"close: %O{error}"

            let after = UnixSystemState.withFileDescriptors registry system
            (registrationsOf kqueue after).IsEmpty |> shouldEqual purged

    [<Test>]
    let ``an event reaches every registration attached to its socket, whichever process owns the kqueue`` () : unit =
        for owner in [ None ; Some other ] do
            let listener, kqueue, system = world owner
            let socket = socketOf listener system

            // A queued connection makes the listener's READ ready, so the event
            // activates the registration. (The connect raises the same event
            // itself, so it is undone first.)
            let client, system = KeventWorld.stream true system
            let _, connected = KeventWorld.connect client 8080us system
            let id = KeventWorld.idOf kqueue connected

            let withActive (active : (int * KqueueFilter) list) (system : UnixSystem<int, string>) =
                match OpenFileTable.tryFind id system.Machine.OpenFiles with
                | Some {
                           Target = OpenFileTarget.Kqueue state
                       } ->
                    UnixSystemState.mapOpenFiles
                        (OpenFileTable.setKqueueState
                            id
                            { state with
                                Active = active
                            })
                        system
                | other -> failwith $"%O{id} is %A{other}"

            let idle = withActive [] connected
            let activated = KqueueQueue.activate socket [ KqueueFilter.Read ] idle.Machine

            match OpenFileTable.tryFind id activated.OpenFiles with
            | Some {
                       Target = OpenFileTarget.Kqueue state
                   } -> state.Active |> shouldEqual [ listener, KqueueFilter.Read ]
            | other -> failwith $"%O{id} is %A{other}"

            // An event on the client's socket reaches nothing: the
            // registration is attached to the listener's.
            KqueueQueue.activate (socketOf client connected) [ KqueueFilter.Read ] idle.Machine
            |> shouldEqual idle.Machine

    [<Test>]
    let ``a descriptor naming another process's kqueue is a defect`` () : unit =
        let _, kqueue, own = world None
        UnixSystem.checkInvariants own |> shouldEqual []

        let _, kqueue', foreign = world (Some other)
        kqueue' |> shouldEqual kqueue

        // `other` is no process on the machine, which is a defect of its own.
        UnixSystem.checkInvariants foreign
        |> shouldEqual
            [
                UnixSystemDefect.KqueueOwnerNotLive (KeventWorld.idOf kqueue foreign, other)
                UnixSystemDefect.KqueueOfAnotherProcess (kqueue, KeventWorld.idOf kqueue foreign, other)
            ]
