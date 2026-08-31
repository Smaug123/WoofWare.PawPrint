namespace WoofWare.PosixKernel

/// Which of `struct sockaddr_in`'s fields a caller's declared length reaches,
/// for a syscall whose sockaddr the kernel is about to copy in.
///
/// The kernel copies the caller's whole declared length whatever this says; what
/// it names is which fields that copy actually *contains*, and so which of them
/// the caller can supply. A shorter length is not an error -- `bind(2)` and
/// `connect(2)` both have measured answers for a sockaddr whose family they
/// never saw -- so this is an instruction to the caller rather than a verdict.
[<RequireQualifiedAccess>]
type SockaddrCopyFields =
    /// The copy reaches no field this kernel reads. Pass no family and no
    /// endpoint.
    | Nothing
    /// It reaches `sa_family` and no further. Pass the family alone.
    | Family
    /// It reaches `sa_family`, `sin_addr` and `sin_port`. Pass both.
    | FamilyAndEndpoint

[<RequireQualifiedAccess>]
module SockaddrCopyFields =
    /// Refuse a caller that supplied a different set of fields from the one the
    /// copy reaches.
    ///
    /// Not defensiveness. This kernel's answer for a field it *could not read*
    /// is measured and different from its answer for a field nobody bothered to
    /// read, so conflating the two would be a silent wrong answer rather than a
    /// crash. `operation` is the caller's own name for itself, since the mistake
    /// is the caller's.
    let checkSupplied
        (operation : string)
        (fields : SockaddrCopyFields)
        (family : int option)
        (endpoint : InternetEndpoint option)
        : unit
        =
        let expected =
            match fields with
            | SockaddrCopyFields.Nothing -> false, false
            | SockaddrCopyFields.Family -> true, false
            | SockaddrCopyFields.FamilyAndEndpoint -> true, true

        if (Option.isSome family, Option.isSome endpoint) <> expected then
            failwith
                $"%s{operation}: the copy of this sockaddr reaches %O{fields}, but the caller supplied family=%b{Option.isSome family} endpoint=%b{Option.isSome endpoint}. A field this kernel could not read and a field the caller did not read have different measured answers, so they must not be conflated (this is a bug in the caller)."

/// Whether a syscall taking a `struct sockaddr` reaches the point at which the
/// kernel copies it in.
///
/// The question exists for the reason `WriteAdmission`'s does: a caller may not
/// be able to produce the bytes without failing, so every answer available
/// *without* reading the sockaddr comes first. It matters more here than for a
/// write, because whether the copy happens at all is a measured per-flavour rule
/// rather than a length test -- Darwin's `getsockaddr` reads nothing at a length
/// too short to reach `sa_family`, and Linux's `move_addr_to_kernel` reads at
/// any positive length.
///
/// Shared by `bind(2)` and `connect(2)`, whose screens up to this point are the
/// same ones in the same order -- which is measurement rather than convenience:
/// `SimulatedUnixPlatform.bindAddressLength` is named for the first and used by
/// the second because the two were measured to agree exactly.
[<RequireQualifiedAccess>]
type SockaddrCopyAdmission =
    /// Answered without the sockaddr being read at all -- a bad descriptor, a
    /// non-socket, a length the copy helper rejects outright, or a faulting
    /// address.
    ///
    /// Always a failure: every screen that precedes the copy is one that can
    /// only refuse, which is why this carries an errno rather than an outcome.
    | Answered of error : UnixError
    /// The copy is reached: it takes exactly `length` bytes from the caller's
    /// buffer, of which `fields` says which are worth decoding.
    | Transfer of length : int * fields : SockaddrCopyFields

/// Why this kernel will not answer a syscall that copies a `struct sockaddr` in.
///
/// Distinct from an errno: an errno is an answer, and these are the inputs for
/// which this library has measured what real kernels do and found no single
/// answer to give.
[<RequireQualifiedAccess>]
type SockaddrCopyRefusal =
    /// The descriptor is a socket in a domain whose addresses this kernel does
    /// not model, so there is no address to read out of the caller's sockaddr
    /// even if the call would otherwise succeed.
    | UnmodelledDomain of socket : SocketId * domain : SocketDomain
    /// The kernel copies the sockaddr in, and the buffer has no answer at that
    /// step.
    ///
    /// A `BufferRefusal` rather than a case of its own, unlike `accept`'s
    /// copy-*out* fault: nothing has been consumed by the time this copy
    /// happens, so an unmapped buffer is an ordinary EFAULT and only the two
    /// classifications a client cannot represent are left over.
    | Buffer of BufferRefusal

[<RequireQualifiedAccess>]
module SockaddrCopyRefusal =
    /// What this kernel knows about why it cannot answer. The client supplies
    /// its own half -- which entry point, which argument, and how a caller could
    /// have come by such a socket or such a buffer.
    let describe (refusal : SockaddrCopyRefusal) : string =
        match refusal with
        | SockaddrCopyRefusal.Buffer refusal -> BufferRefusal.describe refusal
        | SockaddrCopyRefusal.UnmodelledDomain (socket, domain) ->
            $"the descriptor is socket %O{socket}, whose domain is %O{domain}. This kernel models a transport address only for IPv4: an IPv6 socket's is sixteen bytes of address plus a scope id, and a Unix-domain socket's is a *path* in the filesystem rather than a transport endpoint. Neither is a wider version of what is modelled here, so there is nothing to truncate or widen into an answer."

/// What a `bind(2)` answered.
[<RequireQualifiedAccess>]
type BindAnswer =
    /// The socket is bound. `endpoint` is where -- which is not necessarily what
    /// the caller asked for: a request for port 0 asks for any free port, and
    /// this is the one allocated.
    | Bound of endpoint : InternetEndpoint
    /// The call failed with this errno.
    ///
    /// The system still comes back, and may have changed: the `SO_REUSEADDR` a
    /// caller folds into this call is applied before every answer here, the
    /// address fault included. Measured -- after a bind that answered EFAULT the
    /// option still reads back set.
    | Failed of error : UnixError

/// Why this kernel will not answer a `bind`.
[<RequireQualifiedAccess>]
type BindRefusal =
    /// The screens every sockaddr-taking call shares had no answer.
    | Copy of SockaddrCopyRefusal
    /// The caller asked to bind a broadcast or multicast address.
    ///
    /// Refused rather than answered, and refused *late*: a fault this platform
    /// ranks ahead of the address is one this kernel does know the answer to,
    /// and reporting it is better than refusing. Only when the address itself is
    /// what the platform would rule on does the gap bite.
    ///
    /// Multicast is not modelled -- there is no group membership and no
    /// interface to receive on -- and the real rule is not one rule: measured,
    /// Linux takes such an address on a stream socket, Darwin answers
    /// `EAFNOSUPPORT` there, and Darwin's answer depends on the socket's kind
    /// besides.
    | UnmodelledMulticast of socket : SocketId * address : uint32
    /// The bind asked for any free port and every port in the ephemeral range is
    /// taken.
    ///
    /// A real kernel reports `EADDRINUSE` here, but that has not been measured
    /// under this kernel's own allocator and inventing it would be a guess.
    | EphemeralPortsExhausted of range : uint16 * uint16

[<RequireQualifiedAccess>]
module BindRefusal =
    /// What this kernel knows about why it cannot bind. The client supplies its
    /// own half -- which entry point asked, and which descriptor.
    let describe (refusal : BindRefusal) : string =
        match refusal with
        | BindRefusal.Copy refusal -> SockaddrCopyRefusal.describe refusal
        | BindRefusal.UnmodelledMulticast (socket, address) ->
            $"socket %O{socket} asked to bind %s{InternetEndpoint.toString (InternetEndpoint.ofParts address 0us)}, a broadcast or multicast address. This kernel models no multicast -- there is no group membership and no interface to receive on -- and the real rule is not one rule: measured, Linux takes such an address on a stream socket, Darwin answers EAFNOSUPPORT there, and Darwin's answer depends on the socket's kind besides. Model multicast before binding one."
        | BindRefusal.EphemeralPortsExhausted (low, high) ->
            $"every port in the ephemeral range %d{low}-%d{high} is taken, so this bind of port 0 has no answer. A real kernel reports EADDRINUSE, but that has not been measured under this allocator and inventing it would be a guess. Widen the range, or measure the real answer."

/// What a `listen(2)` answered.
[<RequireQualifiedAccess>]
type ListenAnswer =
    /// The socket is listening. `endpoint` is where it is bound, which for a
    /// socket that had no address is the one this call gave it.
    | Listening of endpoint : InternetEndpoint
    /// The call failed with this errno, and nothing changed.
    | Failed of error : UnixError

/// Why this kernel will not answer a `listen`.
[<RequireQualifiedAccess>]
type ListenRefusal =
    /// The descriptor is a socket in a domain whose addresses this kernel does
    /// not model, so the implicit bind below has no address to give it.
    | UnmodelledDomain of socket : SocketId * domain : SocketDomain
    /// The descriptor is a socket of a kind whose `listen(2)` answer is
    /// unmeasured: `SOCK_SEQPACKET` does accept connections and `SOCK_RAW`
    /// plausibly answers `EOPNOTSUPP`, but neither has been measured.
    | UnmeasuredKind of socket : SocketId * kind : SocketKind
    /// The descriptor is a stream socket in a phase whose `listen(2)` answer is
    /// unmeasured -- plausibly `EISCONN` for a connected one.
    | UnmeasuredPhase of socket : SocketId * phase : SocketPhase
    /// The socket had no address, so this call binds it, and every port in the
    /// ephemeral range is taken.
    | EphemeralPortsExhausted of range : uint16 * uint16

[<RequireQualifiedAccess>]
module ListenRefusal =
    /// What this kernel knows about why it will not listen. The client supplies
    /// its own half -- which entry point asked, and which descriptor.
    let describe (refusal : ListenRefusal) : string =
        match refusal with
        | ListenRefusal.UnmodelledDomain (socket, domain) ->
            $"the descriptor is socket %O{socket}, whose domain is %O{domain}. A `listen` on an unbound socket binds it, and this kernel models a local address only for IPv4: an IPv6 socket's is sixteen bytes of address plus a scope id, and a Unix-domain socket's is a *path* in the filesystem rather than a transport endpoint."
        | ListenRefusal.UnmeasuredKind (socket, kind) ->
            $"the descriptor is socket %O{socket}, which is a %O{kind} socket, and what `listen(2)` answers for one is unmeasured. Measure it rather than guessing: SOCK_SEQPACKET does accept connections, so a guess of EOPNOTSUPP there would be a wrong answer rather than an approximate one."
        | ListenRefusal.UnmeasuredPhase (socket, phase) ->
            $"the descriptor is socket %O{socket}, a stream socket in %A{phase}, and what `listen(2)` answers for one is unmeasured -- plausibly EISCONN for a connected socket. Measure it rather than guessing."
        | ListenRefusal.EphemeralPortsExhausted (low, high) ->
            $"this socket has no address, so `listen(2)` binds it, and every port in the ephemeral range %d{low}-%d{high} is taken. Widen the range, or measure what a real kernel says here."

/// What a change to a descriptor's `O_NONBLOCK` answered.
///
/// Store and answer are separate because on one flavour they disagree: an event
/// port's bit toggles and the call still reports a failure.
[<RequireQualifiedAccess>]
type SetNonBlockingAnswer =
    /// The flag is now what the caller asked for, and the call succeeded.
    | Set
    /// The call failed with this errno.
    ///
    /// The system still comes back, and the flag may have changed with it: see
    /// `SimulatedUnixPlatform.eventPortSetStatusFlagsError`, where the bit
    /// toggles and the answer is a failure anyway.
    | Failed of error : UnixError

/// Why this kernel will not set a descriptor's `O_NONBLOCK`.
[<RequireQualifiedAccess>]
type SetNonBlockingRefusal =
    /// The descriptor is a standard stream and the caller asked to *set* the
    /// flag.
    ///
    /// A real pipe honours `O_NONBLOCK` -- an empty read becomes `EAGAIN` -- and
    /// no modelled stream transfer consults it, so storing the flag would keep
    /// blocking semantics silently. Clearing it is fine, and answered: `false`
    /// is what a stream already reads back.
    | UnmodelledOnStandardStream of role : FileDescriptorRole

[<RequireQualifiedAccess>]
module SetNonBlockingRefusal =
    /// What this kernel knows about why it will not set the flag. The client
    /// supplies its own half -- which entry point asked, and which descriptor.
    let describe (refusal : SetNonBlockingRefusal) : string =
        match refusal with
        | SetNonBlockingRefusal.UnmodelledOnStandardStream role ->
            $"the descriptor is the standard stream %O{role}, which this kernel models as a pipe, and no modelled stream transfer consults `O_NONBLOCK`; storing it would silently keep blocking semantics. Decide what a non-blocking stream read does before accepting this."

/// What `getsockname(2)` reports about a socket's own address.
[<RequireQualifiedAccess>]
type GetSockNameAnswer =
    /// The address the socket is bound to, and the length the call reports.
    ///
    /// The endpoint rather than the bytes: a `struct sockaddr_in` is the
    /// client's to lay out, and the client that decoded one for `bind(2)` is the
    /// one that encodes this. `reportedLength` is that structure's *untruncated*
    /// size, which the caller's declared length does not bound -- see the
    /// entry point.
    | Reported of endpoint : InternetEndpoint * reportedLength : int
    /// The entry point returns -1, the caller stores `error` wherever its libc
    /// keeps errno, and `lengthOverwritten` is what the kernel had already put
    /// in the caller's length cell before it discovered the fault.
    ///
    /// `None` on a flavour that had stored nothing yet, and on every failure
    /// that precedes the copy on either. See `GetSockNameFaultLength`, which is
    /// where the divergence and its measurement are written down.
    | Failed of error : UnixError * lengthOverwritten : int option

/// Why this kernel will not answer a `getsockname`.
[<RequireQualifiedAccess>]
type GetSockNameRefusal =
    /// The destination has no answer at the step the call reached.
    | Buffer of BufferRefusal
    /// A socket in an address family whose local address this kernel does not
    /// model. Not an errno: a real kernel in this family answers, and every
    /// value this one could report would be invented.
    | UnmodelledDomain of socket : SocketId * domain : SocketDomain

[<RequireQualifiedAccess>]
module GetSockNameRefusal =
    /// What this kernel knows about why it cannot answer. The client supplies
    /// its own half -- which entry point, which descriptor, and how a caller
    /// could have come by such a socket.
    let describe (refusal : GetSockNameRefusal) : string =
        match refusal with
        | GetSockNameRefusal.Buffer refusal -> BufferRefusal.describe refusal
        | GetSockNameRefusal.UnmodelledDomain (socket, domain) ->
            $"the descriptor is socket %O{socket}, whose domain is %O{domain}. This kernel models a local address only for IPv4: an IPv6 socket's is sixteen bytes of address plus a scope id, and a Unix-domain socket's is a *path* in the filesystem rather than a transport endpoint. Neither is a wider version of what is modelled here, so there is nothing to truncate or widen into an answer."

[<RequireQualifiedAccess>]
module UnixSocket =

    /// The shortest copy that contains both transport fields: `sin_addr` is the
    /// further of the two, so its end is the extent. The same on both flavours --
    /// Darwin's `sa_len` byte displaces `sa_family` into byte 1 and leaves the
    /// transport fields where they are.
    let private internetEndpointExtent : int =
        InternetSockaddr.address.Offset + InternetSockaddr.address.Width

    /// Everything `connect(2)` decides before the kernel copies the caller's
    /// sockaddr in, which is where a client that cannot always produce those
    /// bytes needs to be let off. See `SockaddrCopyAdmission`.
    ///
    /// `declaredLength` must not be negative: a caller that casts it to
    /// `socklen_t` makes the copy enormous rather than negative, so a kernel is
    /// never asked one, and a caller that has not screened it is asking a
    /// question this library has no answer for.
    ///
    /// Changes nothing: everything a connect does before the copy is a question.
    let admitSockaddrCopy<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (destination : UserBuffer)
        (declaredLength : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SockaddrCopyAdmission, SockaddrCopyRefusal>
        =
        if declaredLength < 0 then
            failwith
                $"UnixSocket.admitSockaddrCopy: declared length %d{declaredLength} is negative, which no kernel is ever asked -- a caller that casts it to `socklen_t` makes the copy SIZE_MAX bytes rather than passing it on. Screen this in the client (this is a bug in the caller)."

        let answered (error : UnixError) : Result<SockaddrCopyAdmission, SockaddrCopyRefusal> =
            Ok (SockaddrCopyAdmission.Answered error)

        // The descriptor is classified first, before the length and before the
        // buffer: measured on both flavours, a closed descriptor answers EBADF
        // and a non-socket ENOTSOCK at every length and through every buffer.
        match FileDescriptorRegistry.tryFind fd system.Process.FileDescriptors with
        | None -> answered (UnixError.EBADF)
        | Some description ->

        match description.Target with
        | OpenFileTarget.File _
        | OpenFileTarget.StandardStream _
        | OpenFileTarget.SocketEventPort _ -> answered (UnixError.ENOTSOCK)
        | OpenFileTarget.Socket socketId ->

        let socket = UnixMachineState.socket socketId system.Machine

        match socket.Domain with
        | SocketDomain.InterNetworkV6
        | SocketDomain.Unix -> Error (SockaddrCopyRefusal.UnmodelledDomain (socketId, socket.Domain))
        | SocketDomain.InterNetwork ->

        let platform = system.Machine.UnixPlatform
        let exactSize = (SimulatedUnixPlatform.socketAddressSizes platform).InterNetwork

        // The oversized-length rejection happens in the copy helper before any
        // byte moves, so it precedes every buffer answer below.
        // `connectSocket` documents why `bind(2)`'s verdict function is the
        // right one: the measured lengths agree exactly.
        match SimulatedUnixPlatform.bindAddressLength platform exactSize declaredLength with
        | BindLengthVerdict.RejectedBeforeCopy error -> answered (error)
        | BindLengthVerdict.Accepted
        | BindLengthVerdict.Invalid ->

        let familyField = SimulatedUnixPlatform.sockaddrFamilyField platform
        let reachesFamily = SockaddrFamilyField.reachedBy familyField declaredLength

        // Whether the kernel touches the caller's buffer at all. Linux's
        // `move_addr_to_kernel` copies at any positive length; Darwin's
        // `getsockaddr` reads nothing at a length that does not reach
        // `sa_family`, which is why a stray pointer is answerable there and not
        // here.
        let copies =
            declaredLength > 0
            && match SimulatedUnixPlatform.flavour platform with
               | SimulatedUnixFlavour.Linux -> true
               | SimulatedUnixFlavour.Darwin -> reachesFamily

        if not copies then
            Ok (SockaddrCopyAdmission.Transfer (0, SockaddrCopyFields.Nothing))
        else

        match destination with
        | UserBuffer.Unmapped _ -> answered (UnixError.EFAULT)
        | UserBuffer.Opaque -> Error (SockaddrCopyRefusal.Buffer BufferRefusal.OpaqueAtTransfer)
        | UserBuffer.Addressless -> Error (SockaddrCopyRefusal.Buffer BufferRefusal.AddresslessAtTransfer)
        | UserBuffer.Mapped ->

        let fields =
            if declaredLength >= internetEndpointExtent then
                SockaddrCopyFields.FamilyAndEndpoint
            elif reachesFamily then
                SockaddrCopyFields.Family
            else
                SockaddrCopyFields.Nothing

        Ok (SockaddrCopyAdmission.Transfer (declaredLength, fields))

    /// Mirrors `socket(2)`: allocate a fresh socket, and a fresh descriptor onto
    /// it.
    ///
    /// One operation for both allocations, rather than a socket-table insert
    /// beside a separate `FileDescriptorRegistry.createSocket`, because the two
    /// must agree: the identity this mints is the identity the description
    /// names, and splitting them would let a caller do one without the other.
    ///
    /// Says nothing about whether this domain/kind/protocol combination *can*
    /// exist -- `SimulatedUnixPlatform.creatableSockets` answers that, and this
    /// is reached only once it has said yes.
    let createSocket<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (domain : SocketDomain)
        (kind : SocketKind)
        (protocol : SocketProtocol)
        (system : UnixSystem<'Task, 'Handler>)
        : int * UnixSystem<'Task, 'Handler>
        =
        let socketId = system.Machine.NextSocketId
        let (SocketId raw) = socketId

        let fd, registry =
            FileDescriptorRegistry.createSocket socketId system.Process.FileDescriptors

        let socket =
            {
                Domain = domain
                Kind = kind
                Protocol = protocol
                Binding = None
                ReuseAddress = false
                Phase = SocketPhase.Idle
            }

        fd,
        { system with
            Machine =
                { system.Machine with
                    Sockets = Map.add socketId socket system.Machine.Sockets
                    NextSocketId = SocketId (raw + 1L)
                }
            Process =
                { system.Process with
                    FileDescriptors = registry
                }
        }

    /// `fcntl(F_SETFL)`'s `O_NONBLOCK` half: put the flag on the open file
    /// description `fd` names.
    ///
    /// The flag lands on the *description*, where POSIX keeps the status flags,
    /// so a `dup` of the descriptor sees it too.
    ///
    /// Only for the targets whose every modelled operation honours it: a socket
    /// (`accept` and `connect` consult it, and each transfer that lands must
    /// too), a regular file (both kernels give `O_NONBLOCK` no effect there, so
    /// an operation that never looks is right not to), and a socket event port
    /// (whose waits block per their own timeout argument, never per this flag).
    /// The one target whose modelled transfers would *ignore* a stored flag is
    /// refused rather than silently diverging.
    let setNonBlocking<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (isNonBlocking : bool)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SetNonBlockingAnswer * UnixSystem<'Task, 'Handler>, SetNonBlockingRefusal>
        =
        let stored (system : UnixSystem<'Task, 'Handler>) : UnixSystem<'Task, 'Handler> =
            { system with
                Process =
                    { system.Process with
                        FileDescriptors =
                            FileDescriptorRegistry.setNonBlocking fd isNonBlocking system.Process.FileDescriptors
                    }
            }

        match FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors with
        | None -> Ok (SetNonBlockingAnswer.Failed UnixError.EBADF, system)
        | Some (OpenFileTarget.StandardStream role) when isNonBlocking ->
            Error (SetNonBlockingRefusal.UnmodelledOnStandardStream role)
        | Some (OpenFileTarget.SocketEventPort _) ->
            // Store first, report second: measured, the platforms agree that the
            // bit toggles and disagree on the answer -- Linux succeeds where
            // Darwin reports a failure *with the bit toggled anyway*.
            let system = stored system

            match SimulatedUnixPlatform.eventPortSetStatusFlagsError system.Machine.UnixPlatform with
            | None -> Ok (SetNonBlockingAnswer.Set, system)
            | Some error -> Ok (SetNonBlockingAnswer.Failed error, system)
        | Some (OpenFileTarget.StandardStream _)
        | Some (OpenFileTarget.File _)
        | Some (OpenFileTarget.Socket _) -> Ok (SetNonBlockingAnswer.Set, stored system)

    /// `fcntl(F_GETFL)`'s `O_NONBLOCK` half: whether the open file description
    /// `fd` names carries the flag.
    ///
    /// `None` for a descriptor that is not live, which a caller reports as
    /// `EBADF`. Reads for every target kind, where `setNonBlocking` refuses one:
    /// `false` is the truth for a target the setter will not flag.
    ///
    /// Changes nothing: a read of a status flag is a question.
    let isNonBlocking<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (system : UnixSystem<'Task, 'Handler>)
        : bool option
        =
        FileDescriptorRegistry.tryFind fd system.Process.FileDescriptors
        |> Option.map (fun description -> description.NonBlocking)

    /// Whether any *other* socket's binding conflicts with `candidate`, taken on
    /// behalf of `socket`.
    ///
    /// The relation `bind(2)` decides admission with, and `listen(2)` asks
    /// again -- on the flavour whose `listen` re-runs it, and for the implicit
    /// bind an unbound `listen` performs. One definition because it is one
    /// kernel rule; the callers differ only in *when* they ask.
    let private bindingConflicts<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (socketId : SocketId)
        (socket : SocketDescription)
        (candidate : SocketBinding)
        (system : UnixSystem<'Task, 'Handler>)
        : bool
        =
        system.Machine.Sockets
        |> Map.exists (fun otherId (other : SocketDescription) ->
            if otherId = socketId then
                false
            else

            match other.Binding with
            | None -> false
            | Some existing ->
                // Separate port namespaces per transport, measured: a UDP socket
                // takes a port a listening TCP socket holds.
                other.Kind = socket.Kind
                && SimulatedUnixPlatform.bindConflict
                    system.Machine.UnixPlatform
                    existing
                    other.ReuseAddress
                    other.Phase
                    candidate
                    socket.ReuseAddress
        )

    /// `bind(2)`: give `fd` a local address.
    ///
    /// `family` and `endpoint` are what the caller read out of its sockaddr, and
    /// must be exactly what `admitSockaddrCopy` asked for -- the same contract
    /// `connect` states, and for the same reason: this kernel's answer for a
    /// field it *could not read* is measured and different from its answer for a
    /// field nobody read.
    ///
    /// `reuseAddress` folds in the `SO_REUSEADDR` that a foreign-function layer
    /// may set on the way past. It is applied before every answer below, the
    /// address fault included, because the option is set by a separate call that
    /// no failure of this one undoes -- measured: after a bind that answered
    /// EFAULT, the option still reads back set. A caller with no such layer
    /// passes `false`.
    ///
    /// Answers where the socket ended up, which for a request of port 0 is a
    /// port this kernel chose.
    let bind<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (destination : UserBuffer)
        (declaredLength : int)
        (reuseAddress : bool)
        (family : int option)
        (endpoint : InternetEndpoint option)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<BindAnswer * UnixSystem<'Task, 'Handler>, BindRefusal>
        =
        // The descriptor is resolved here rather than left to the admission
        // below, because the `SO_REUSEADDR` write needs the socket and has to
        // happen before the admission's own answers. The admission resolves it
        // again; that is a lookup repeated, not a rule.
        match FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors with
        | None -> Ok (BindAnswer.Failed UnixError.EBADF, system)
        | Some target ->

        match target with
        | OpenFileTarget.File _
        | OpenFileTarget.StandardStream _
        | OpenFileTarget.SocketEventPort _ -> Ok (BindAnswer.Failed UnixError.ENOTSOCK, system)
        | OpenFileTarget.Socket socketId ->

        let socket = UnixMachineState.socket socketId system.Machine

        match socket.Domain with
        | SocketDomain.InterNetworkV6
        | SocketDomain.Unix -> Error (BindRefusal.Copy (SockaddrCopyRefusal.UnmodelledDomain (socketId, socket.Domain)))
        | SocketDomain.InterNetwork ->

        let socket =
            if reuseAddress then
                { socket with
                    ReuseAddress = true
                }
            else
                socket

        let withSocket (socket : SocketDescription) (system : UnixSystem<'Task, 'Handler>) =
            { system with
                Machine =
                    { system.Machine with
                        Sockets = Map.add socketId socket system.Machine.Sockets
                    }
            }

        let system = withSocket socket system

        match admitSockaddrCopy fd destination declaredLength system with
        | Error refusal -> Error (BindRefusal.Copy refusal)
        | Ok (SockaddrCopyAdmission.Answered error) -> Ok (BindAnswer.Failed error, system)
        | Ok (SockaddrCopyAdmission.Transfer (_, fields)) ->

        SockaddrCopyFields.checkSupplied "UnixSocket.bind" fields family endpoint

        let platform = system.Machine.UnixPlatform

        // Asked again rather than carried out of the admission: the admission
        // answers the *outright* rejection, and this is the same verdict read
        // for the fault set, where a length that is merely not a `sockaddr_in`
        // ranks against the other faults rather than pre-empting them.
        let lengthFault =
            SimulatedUnixPlatform.bindAddressLength
                platform
                (SimulatedUnixPlatform.socketAddressSizes platform).InterNetwork
                declaredLength
            <> BindLengthVerdict.Accepted

        let familyFault =
            match family with
            // Unreadable: no family to disagree with, and the length fault fires
            // instead.
            | None -> false
            | Some family when family = SimulatedUnixPlatform.internetAddressFamily -> false
            | Some 0 ->
                // AF_UNSPEC is two different rules. Linux accepts the blob only
                // when the address is all-zero, and answers EAFNOSUPPORT
                // otherwise; Darwin reads the address and port out of it and
                // binds them, exactly as for AF_INET. Both measured.
                match SimulatedUnixPlatform.flavour platform with
                | SimulatedUnixFlavour.Darwin -> false
                | SimulatedUnixFlavour.Linux ->
                    match endpoint with
                    | Some endpoint -> endpoint.Address <> InternetEndpoint.WildcardAddress
                    | None -> false
            | Some _ -> true

        let candidate =
            endpoint
            |> Option.map (fun endpoint ->
                {
                    Endpoint = endpoint
                    // bind(2)'s own address is locked: a Linux refusal delivery
                    // reverts a later connect's source resolution back to
                    // exactly this.
                    LockedAddress = Some endpoint.Address
                }
            )

        let addressNotLocalFault =
            match endpoint with
            | None -> false
            | Some endpoint ->
                SimulatedUnixPlatform.bindAddressFaults
                    platform
                    system.Machine.LocalAddresses
                    system.Machine.LocalRoutes
                    endpoint.Address

        let privilegedPortFault =
            match endpoint with
            | None -> false
            | Some endpoint ->
                endpoint.Port > 0us
                && endpoint.Port < SimulatedUnixPlatform.privilegedPortCeiling
                && system.Process.UserId <> 0u

        let conflictsWith (binding : SocketBinding) : bool =
            bindingConflicts socketId socket binding system

        // A request for port 0 needs no special case here, and had one until a
        // mutation showed nothing could falsify it: `bindConflict` answers
        // `false` outright when the ports differ, and no bound socket holds port
        // 0 -- every port-0 request allocates a real one. So a port-0 candidate
        // conflicts with nothing, and the allocator's own search below is what
        // keeps it that way.
        let addressInUseFault =
            match candidate with
            | Some binding -> conflictsWith binding
            | None -> false

        let faults =
            [
                BindFault.Length, lengthFault
                BindFault.Family, familyFault
                BindFault.AddressNotLocal, addressNotLocalFault
                BindFault.PrivilegedPort, privilegedPortFault
                BindFault.AlreadyBound, socket.Binding.IsSome
                BindFault.AddressInUse, addressInUseFault
            ]
            |> List.choose (fun (fault, holds) -> if holds then Some fault else None)
            |> Set.ofList

        match SimulatedUnixPlatform.firstBindFault platform faults, endpoint with
        | Some BindFault.AddressNotLocal, Some endpoint when
            SimulatedUnixPlatform.isBroadcastOrMulticast endpoint.Address
            ->
            Error (BindRefusal.UnmodelledMulticast (socketId, endpoint.Address))
        | fault, _ ->

        match fault with
        | Some fault ->
            let error =
                match fault with
                // `RejectedBeforeCopy` never reaches the fault order: the
                // admission answers it before anything is read.
                | BindFault.Length -> UnixError.EINVAL
                | BindFault.AlreadyBound -> UnixError.EINVAL
                | BindFault.Family -> UnixError.EAFNOSUPPORT
                | BindFault.AddressNotLocal -> UnixError.EADDRNOTAVAIL
                | BindFault.PrivilegedPort -> UnixError.EACCES
                | BindFault.AddressInUse -> UnixError.EADDRINUSE

            Ok (BindAnswer.Failed error, system)
        | None ->

        let binding =
            match candidate with
            | Some binding -> binding
            | None ->
                failwith
                    $"UnixSocket.bind: no fault was reported for fd %d{fd} and yet the sockaddr was too short to read an address from (declared length %d{declaredLength}). The length fault should have fired (this is a bug in this library)."

        match
            (if binding.Endpoint.Port > 0us then
                 Some (binding, system.Machine)
             else
                 let acceptable (port : uint16) : bool =
                     not (
                         conflictsWith
                             { binding with
                                 Endpoint =
                                     { binding.Endpoint with
                                         Port = port
                                     }
                             }
                     )

                 UnixMachineState.allocateEphemeralPort acceptable system.Machine
                 |> Option.map (fun (port, machine) ->
                     { binding with
                         Endpoint =
                             { binding.Endpoint with
                                 Port = port
                             }
                     },
                     machine
                 ))
        with
        | None -> Error (BindRefusal.EphemeralPortsExhausted system.Machine.EphemeralPortRange)
        | Some (bound, machine) ->

        // From `machine` rather than `system.Machine`: the ephemeral allocator
        // advanced the cursor, and that advance is part of this bind.
        let system =
            { system with
                Machine = machine
            }
            |> withSocket
                { socket with
                    Binding = Some bound
                }

        Ok (BindAnswer.Bound bound.Endpoint, system)

    /// `listen(2)`: make `fd` a passive socket, and give it an address if it has
    /// none.
    ///
    /// `backlog` is recorded verbatim rather than clamped: every value is
    /// accepted -- measured, 0, -1 and `INT_MAX` all succeed on both -- and the
    /// accept-queue capacity a later `connect` enforces is derived from it per
    /// flavour, so storing the input keeps one flavour's arithmetic out of the
    /// stored value.
    ///
    /// A re-listen keeps the queue and updates the backlog, which is Linux's
    /// documented behaviour (`sk_max_ack_backlog` is simply re-assigned).
    let listen<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (backlog : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<ListenAnswer * UnixSystem<'Task, 'Handler>, ListenRefusal>
        =
        match FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors with
        | None -> Ok (ListenAnswer.Failed UnixError.EBADF, system)
        | Some target ->

        match target with
        | OpenFileTarget.File _
        | OpenFileTarget.StandardStream _
        | OpenFileTarget.SocketEventPort _ -> Ok (ListenAnswer.Failed UnixError.ENOTSOCK, system)
        | OpenFileTarget.Socket socketId ->

        let socket = UnixMachineState.socket socketId system.Machine

        match socket.Domain with
        | SocketDomain.InterNetworkV6
        | SocketDomain.Unix -> Error (ListenRefusal.UnmodelledDomain (socketId, socket.Domain))
        | SocketDomain.InterNetwork ->

        match socket.Kind with
        | SocketKind.Datagram -> Ok (ListenAnswer.Failed UnixError.EOPNOTSUPP, system)
        | SocketKind.Raw
        | SocketKind.SeqPacket -> Error (ListenRefusal.UnmeasuredKind (socketId, socket.Kind))
        | SocketKind.Stream ->

        // Split out rather than matched in place: the original handler could put
        // its refusal in a `failwith`, which types as anything; a refusal that is
        // a value has to be produced before the rest of the function continues.
        let unmeasuredPhase =
            match socket.Phase with
            | SocketPhase.Idle
            | SocketPhase.Listening _ -> None
            | phase -> Some phase

        match unmeasuredPhase with
        | Some phase -> Error (ListenRefusal.UnmeasuredPhase (socketId, phase))
        | None ->

        // On Linux two sockets carrying SO_REUSEADDR may share an endpoint until
        // one of them listens, and the second `listen(2)` is then EADDRINUSE.
        // Darwin asks nothing of a socket that already has a port; see
        // `listenRescreensBinding` for why that is not a strictness difference
        // to round in the safe direction.
        match socket.Binding with
        | Some binding when
            SimulatedUnixPlatform.listenRescreensBinding system.Machine.UnixPlatform
            && bindingConflicts socketId socket binding system
            ->
            Ok (ListenAnswer.Failed UnixError.EADDRINUSE, system)
        | _ ->

        match
            (match socket.Binding with
             | Some binding -> Some (binding, system.Machine)
             | None ->
                 // `listen(2)` on an unbound socket binds it to the wildcard and
                 // an ephemeral port. Measured on both -- and note it does *not*
                 // go through `bind(2)`, so no `SO_REUSEADDR` is set, which is a
                 // distinction a later bind can see.
                 let candidate (port : uint16) : SocketBinding =
                     {
                         Endpoint = InternetEndpoint.ofParts InternetEndpoint.WildcardAddress port
                         // This implicit bind runs no `bind(2)`, so nothing is
                         // locked.
                         LockedAddress = None
                     }

                 let acceptable (port : uint16) : bool =
                     not (bindingConflicts socketId socket (candidate port) system)

                 UnixMachineState.allocateEphemeralPort acceptable system.Machine
                 |> Option.map (fun (port, machine) -> candidate port, machine))
        with
        | None -> Error (ListenRefusal.EphemeralPortsExhausted system.Machine.EphemeralPortRange)
        | Some (bound, machine) ->

        let listenPhase =
            match socket.Phase with
            | SocketPhase.Listening listenState ->
                SocketPhase.Listening
                    { listenState with
                        Backlog = backlog
                    }
            | _ ->
                SocketPhase.Listening
                    {
                        Backlog = backlog
                        Queue = []
                    }

        let system =
            { system with
                Machine =
                    { machine with
                        Sockets =
                            Map.add
                                socketId
                                { socket with
                                    Binding = Some bound
                                    Phase = listenPhase
                                }
                                machine.Sockets
                    }
            }

        Ok (ListenAnswer.Listening bound.Endpoint, system)

    /// `getsockname(2)`: report the local address the socket `fd` names.
    ///
    /// Answers an endpoint rather than a `struct sockaddr_in`: the layout is the
    /// client's, which is the same division `bind(2)` and `connect(2)` already
    /// use in the other direction.
    ///
    /// `declaredLength` is how much of the caller's buffer may be written, and
    /// **does not bound what is reported**. Measured on both flavours: a call
    /// declaring 8 writes eight bytes and reports 16, and one declaring 128
    /// writes 16 and still reports 16. The shim asserts the opposite
    /// (`assert(addrLen <= *socketAddressLen)`, `pal_networking.c:1887`) and is
    /// wrong on both platforms; the assertion is compiled out of the shipped
    /// build, which is why nobody has noticed. A client writes
    /// `min declaredLength reportedLength` bytes of the address it encodes.
    ///
    /// `declaredLength` must not be negative. A kernel never sees one -- the
    /// shim screens `*socketAddressLen < 0` before it converts to `socklen_t`,
    /// where the cast would otherwise make the bound `SIZE_MAX` -- so a caller
    /// that has not screened it is asking a question no kernel this library
    /// models was ever asked.
    ///
    /// Changes nothing and returns no system: a `getsockname` reads.
    let getsockname<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (destination : UserBuffer)
        (declaredLength : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<GetSockNameAnswer, GetSockNameRefusal>
        =
        if declaredLength < 0 then
            failwith
                $"UnixSocket.getsockname: declared length %d{declaredLength} is negative, which no kernel is ever asked -- a shim that casts it to `socklen_t` makes the bound SIZE_MAX rather than passing it on. Screen this in the client (this is a bug in the caller)."

        // The descriptor is classified before the destination is looked at, and
        // that ordering is measured rather than assumed: with a closed
        // descriptor or a non-socket one, an unmapped, read-only or null
        // destination still answers EBADF or ENOTSOCK on both flavours, at every
        // declared length probed. Both leave the caller's length cell alone
        // there, which is why neither reports one.
        match FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors with
        | None -> Ok (GetSockNameAnswer.Failed (UnixError.EBADF, None))
        | Some target ->

        match target with
        | OpenFileTarget.File _
        | OpenFileTarget.StandardStream _
        | OpenFileTarget.SocketEventPort _ -> Ok (GetSockNameAnswer.Failed (UnixError.ENOTSOCK, None))
        | OpenFileTarget.Socket socketId ->

        let socket = UnixMachineState.socket socketId system.Machine

        match socket.Domain with
        | SocketDomain.InterNetworkV6
        | SocketDomain.Unix -> Error (GetSockNameRefusal.UnmodelledDomain (socketId, socket.Domain))
        | SocketDomain.InterNetwork ->

        let reportedLength =
            (SimulatedUnixPlatform.socketAddressSizes system.Machine.UnixPlatform).InterNetwork

        // An unbound socket reports its family and nothing else: the wildcard
        // address and port zero. Measured on both flavours -- a fresh AF_INET
        // socket reads back sixteen bytes whose only content is the family, and
        // on Darwin the `sa_len` byte its layout puts in front of it.
        let endpoint =
            match socket.Binding with
            | Some binding -> binding.Endpoint
            | None -> InternetEndpoint.ofParts InternetEndpoint.WildcardAddress 0us

        // A call that may write nothing never consults the destination at all,
        // so a declared length of zero succeeds through an address naming no
        // storage -- measured on both flavours, and on both it still reports the
        // full 16. There is no up-front address screen to fail either: that is
        // why an `Addressless` destination is refused at the transfer below and
        // not here.
        if declaredLength = 0 then
            Ok (GetSockNameAnswer.Reported (endpoint, reportedLength))
        else

        match destination with
        | UserBuffer.Opaque -> Error (GetSockNameRefusal.Buffer BufferRefusal.OpaqueAtTransfer)
        | UserBuffer.Addressless -> Error (GetSockNameRefusal.Buffer BufferRefusal.AddresslessAtTransfer)
        | UserBuffer.Unmapped _ ->
            let overwritten =
                match SimulatedUnixPlatform.getSockNameFaultLength system.Machine.UnixPlatform with
                | GetSockNameFaultLength.Untouched -> None
                | GetSockNameFaultLength.AlreadyReported -> Some reportedLength

            Ok (GetSockNameAnswer.Failed (UnixError.EFAULT, overwritten))
        | UserBuffer.Mapped -> Ok (GetSockNameAnswer.Reported (endpoint, reportedLength))
