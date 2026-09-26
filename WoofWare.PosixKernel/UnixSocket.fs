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
    /// The call failed with this errno. Nothing changed: the socket is bound,
    /// or not, exactly as it was.
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
    /// unmeasured: `SOCK_SEQPACKET` does accept connections, but that has not
    /// been measured.
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

/// Why this kernel will not answer a `setsockopt(2)` or a `getsockopt(2)`.
[<RequireQualifiedAccess>]
type SocketOptionRefusal =
    /// The `level` and `optionName` name an option this kernel does not model,
    /// in the simulated platform's numbering.
    ///
    /// Not `ENOPROTOOPT`, which is a real kernel's answer for an option *it*
    /// does not know. These are options a real kernel does know, or numbers this
    /// library has not measured a real kernel's answer for.
    | UnmodelledOption of socket : SocketId * level : int * optionName : int
    /// A buffer the call reaches has no answer at the step it is reached.
    | Buffer of BufferRefusal
    /// The call would change `SO_REUSEADDR` on a listening socket whose
    /// accept queue holds completed connections.
    ///
    /// A real kernel gives each of those connections its own copy of the
    /// listener's options when the connection completes, so the socket a later
    /// `accept(2)` returns keeps the value from then. This kernel copies the
    /// listener's value at `accept(2)` instead, which agrees only while the
    /// value has not changed since.
    | ListenerWithQueuedConnections of socket : SocketId

[<RequireQualifiedAccess>]
module SocketOptionRefusal =
    /// What this kernel knows about why it cannot answer. The client supplies
    /// its own half -- which entry point asked, and which argument.
    let describe (refusal : SocketOptionRefusal) : string =
        match refusal with
        | SocketOptionRefusal.Buffer refusal -> BufferRefusal.describe refusal
        | SocketOptionRefusal.ListenerWithQueuedConnections socket ->
            $"socket %O{socket} is listening with completed connections in its accept queue, and the call would change its SO_REUSEADDR. Measured on both flavours, each queued connection keeps the value the listener had when that connection completed, so an accept after the change returns a socket carrying the old value. This kernel does not record that per-connection copy: it gives the accepted socket the listener's value at accept time. Record the value with each queued connection before allowing the change."
        | SocketOptionRefusal.UnmodelledOption (socket, level, optionName) ->
            $"socket %O{socket} was asked about option %d{optionName} at level %d{level}, and SO_REUSEADDR at SOL_SOCKET is the only option this kernel models. A real kernel either knows this option, in which case its value is socket state nothing here holds, or answers an errno nobody has measured for it; ENOPROTOOPT would be a guess either way. SO_ERROR in particular is refused because reading it consumes a pending connect refusal, which changes what the next connect(2) answers and what poll(2) reports on Linux, and turns a Darwin RefusedPendingDelivery socket into a Dead one. Model the option before asking for it."

/// Whether a `setsockopt(2)` reaches the point at which the kernel copies the
/// option's value in, which is where a client that cannot always produce those
/// bytes needs to be let off. The counterpart of `SockaddrCopyAdmission`.
[<RequireQualifiedAccess>]
type SetSockOptAdmission =
    /// Answered without the value being read at all. Always a failure, for the
    /// same reason `SockaddrCopyAdmission.Answered` is.
    | Answered of error : UnixError
    /// The copy is reached: it takes exactly `length` bytes from the start of
    /// the caller's value buffer, whatever length the caller declared, and they
    /// are a C `int` in the simulated machine's byte order.
    | Transfer of length : int

/// What a `setsockopt(2)` answered.
[<RequireQualifiedAccess>]
type SetSockOptAnswer =
    /// The option now holds the value the caller asked for.
    | Set
    /// The call failed with this errno, and the option keeps the value it had.
    | Failed of error : UnixError

/// Whether a `getsockopt(2)` reaches the point at which the kernel reads the
/// caller's length cell, which is where a client that cannot always produce
/// those bytes needs to be let off.
[<RequireQualifiedAccess>]
type GetSockOptAdmission =
    /// Answered without the length cell being read at all. Always a failure.
    | Answered of error : UnixError
    /// The kernel reads the caller's length cell: a `socklen_t`, four bytes in
    /// the simulated machine's byte order. Pass what it holds.
    | ReadLength

/// What a `getsockopt(2)` answered.
[<RequireQualifiedAccess>]
type GetSockOptAnswer =
    /// The call succeeded. The kernel wrote the first `length` bytes of `value`,
    /// a C `int` in the simulated machine's byte order, to the caller's value
    /// buffer, and then wrote `length` to the caller's length cell.
    ///
    /// `length` may be zero, and the value buffer is then untouched.
    | Reported of value : int * length : uint32
    /// The call failed with this errno, and neither the value buffer nor the
    /// length cell was written.
    | Failed of error : UnixError

/// Why this library will not answer a `socket(2)`.
///
/// Distinct from an errno: each case is a request whose real answer depends on
/// something this library does not model, or that names a socket it does not
/// model. Each carries the caller's arguments as they were passed.
[<RequireQualifiedAccess>]
type SocketRefusal =
    /// A protocol family other than `AF_UNIX`, `AF_INET` and `AF_INET6` that
    /// the simulated kernel has, or on Linux may have depending on how it was
    /// built. This library models no socket in it.
    | UnmodelledDomain of domain : int
    /// `SOCK_RAW` in an internet domain, or Linux's `SOCK_PACKET` under
    /// `AF_INET`, which asks for a packet socket. This library models no raw
    /// socket, and which of them a real kernel creates depends on the caller's
    /// privilege.
    | RawSocket of domain : int * socketType : int * protocol : int
    /// An ICMP datagram socket: `IPPROTO_ICMP` under `AF_INET`, or
    /// `IPPROTO_ICMPV6` under `AF_INET6`. This library does not model one.
    ///
    /// Darwin creates one for any caller, and Linux only for a caller whose
    /// group the system's `net.ipv4.ping_group_range` setting admits.
    | IcmpDatagram of domain : int * protocol : int
    /// On Linux, a request whose answer depends on which protocols the kernel
    /// was built with or has modules for: SCTP, MPTCP, SMC, L2TP and UDP-Lite.
    /// Depending on that, a real kernel creates a socket this library does not
    /// model, or refuses with one errno or another.
    | BuildDependentProtocol of domain : int * socketType : int * protocol : int

[<RequireQualifiedAccess>]
module SocketRefusal =
    /// What this kernel knows about why it cannot answer, for a client composing
    /// a diagnostic.
    let describe (refusal : SocketRefusal) : string =
        match refusal with
        | SocketRefusal.UnmodelledDomain domain ->
            $"domain %d{domain} names a protocol family other than AF_UNIX, AF_INET and AF_INET6, which the simulated kernel has (or, on Linux, may have, depending on how it was built). This kernel models no socket in it."
        | SocketRefusal.RawSocket (domain, socketType, protocol) ->
            $"domain %d{domain}, type 0x%x{socketType}, protocol %d{protocol} asks for a raw or packet socket. This kernel models none, and which of them a real kernel creates depends on the caller's privilege, which this kernel does not model either."
        | SocketRefusal.IcmpDatagram (domain, protocol) ->
            $"domain %d{domain}, protocol %d{protocol} asks for an ICMP datagram socket, which this kernel does not model. Darwin creates one for any caller; Linux creates one only for a group its net.ipv4.ping_group_range setting admits."
        | SocketRefusal.BuildDependentProtocol (domain, socketType, protocol) ->
            $"domain %d{domain}, type 0x%x{socketType}, protocol %d{protocol}: what Linux answers depends on which protocols the kernel was built with or has modules for (SCTP, MPTCP, SMC, L2TP, UDP-Lite), and this kernel models none of them."

/// What `socket(2)` decided, before any allocation.
[<RequireQualifiedAccess>]
type private SocketDecoding =
    | Creates of domain : SocketDomain * kind : SocketKind * protocol : SocketProtocol * nonBlocking : bool
    | Fails of UnixError
    | Refused of SocketRefusal

/// What a Darwin protocol switch entry does when `socket(2)` selects it.
[<RequireQualifiedAccess>]
type private DarwinAttach =
    | Creates of SocketKind
    | Raw
    | IcmpDatagram
    | NotSupported

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
        | OpenFileTarget.Directory _
        | OpenFileTarget.StandardStream _
        | OpenFileTarget.SocketEventPort _ -> answered (UnixError.ENOTSOCK)
        | OpenFileTarget.Socket socketId ->

        let socket = UnixMachineState.socket socketId system.Machine

        match socket.Domain with
        | SocketDomain.Inet6
        | SocketDomain.Unix -> Error (SockaddrCopyRefusal.UnmodelledDomain (socketId, socket.Domain))
        | SocketDomain.Inet ->

        let platform = system.Machine.UnixPlatform
        let exactSize = SimulatedUnixPlatform.internetSocketAddressSize

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

    // `<sys/socket.h>` and `<netinet/in.h>`: these numbers are the same on both
    // flavours. `AF_INET6` is not, and is `SimulatedUnixPlatform`'s.
    [<Literal>]
    let private AfUnspec = 0

    [<Literal>]
    let private AfUnix = 1

    [<Literal>]
    let private SockStream = 1

    [<Literal>]
    let private SockDgram = 2

    [<Literal>]
    let private SockRaw = 3

    [<Literal>]
    let private SockSeqPacket = 5

    [<Literal>]
    let private IpProtoIcmp = 1

    [<Literal>]
    let private IpProtoTcp = 6

    [<Literal>]
    let private IpProtoUdp = 17

    [<Literal>]
    let private IpProtoIcmpV6 = 58

    /// The condition a protocol number names, for a socket whose selection has
    /// already accepted that number.
    let private requestedProtocol (protocol : int) : SocketProtocol =
        match protocol with
        | IpProtoTcp -> SocketProtocol.Tcp
        | IpProtoUdp -> SocketProtocol.Udp
        | _ -> SocketProtocol.Default

    // Linux's rules are `__sys_socket`, `__sock_create`, `unix_create` and
    // `inet_create`/`inet6_create`, none of which is architecture-specific, and
    // every answer below was measured on Linux 6.18.5 aarch64 at euid 1000 and
    // again as root, by
    // `docs/plans/2026-08-23-posix-kernel-extraction/socket-arguments.c`, over
    // every domain in [-2, 63] and a few beyond, every type in [0, 15] with and
    // without flag bits, and every protocol in [-2, 300] and a few beyond. (An
    // x86-64 build of the probe under Rosetta answered identically, but that is
    // the same aarch64 kernel behind an x86-64 ABI.) The sweep is checked in,
    // and `TestSocketSyscall` holds this function to it;
    // `TestSocketSyscallAgainstHost` repeats the answered calls on whatever
    // kernel runs the suite, which in CI is x86-64.

    /// Linux's `SOCK_NONBLOCK` and `SOCK_CLOEXEC`, which are `O_NONBLOCK` and
    /// `O_CLOEXEC`: `asm-generic/fcntl.h`'s values, which x86-64 and aarch64
    /// both use. Measured on aarch64 only.
    [<Literal>]
    let private LinuxSockNonBlock = 0x800

    [<Literal>]
    let private LinuxSockCloExec = 0x80000

    /// `SOCK_TYPE_MASK`: the bits of the type word that name a type rather than
    /// a flag.
    [<Literal>]
    let private LinuxSockTypeMask = 0xf

    /// `SOCK_PACKET`, the obsolete way to ask `AF_INET` for a packet socket.
    [<Literal>]
    let private LinuxSockPacket = 10

    /// `SOCK_MAX`: one past `SOCK_PACKET`.
    [<Literal>]
    let private LinuxSockMax = 11

    /// `AF_MAX`, which is `NPROTO`: one past the highest family number Linux
    /// has assigned.
    [<Literal>]
    let private LinuxAfMax = 46

    /// `IPPROTO_MAX`: one past `IPPROTO_MPTCP`.
    [<Literal>]
    let private LinuxIpProtoMax = 263

    let private decodeLinux
        (platform : SimulatedUnixPlatform)
        (domain : int)
        (socketType : int)
        (protocol : int)
        : SocketDecoding
        =
        let flags = socketType &&& ~~~LinuxSockTypeMask
        let baseType = socketType &&& LinuxSockTypeMask
        let nonBlocking = flags &&& LinuxSockNonBlock <> 0
        let inet6 = SimulatedUnixPlatform.internetV6AddressFamily platform

        let internet (icmp : int) : SocketDecoding =
            if protocol < 0 || protocol >= LinuxIpProtoMax then
                SocketDecoding.Fails UnixError.EINVAL
            else

            let shape (kind : SocketKind) : SocketDecoding =
                let domain =
                    if domain = SimulatedUnixPlatform.internetAddressFamily then
                        SocketDomain.Inet
                    else
                        SocketDomain.Inet6

                SocketDecoding.Creates (domain, kind, requestedProtocol protocol, nonBlocking)

            let buildDependent () : SocketDecoding =
                SocketDecoding.Refused (SocketRefusal.BuildDependentProtocol (domain, socketType, protocol))

            // The protocols a type's switch holds, each tried in turn, with 0
            // taking the type's first. Absent protocols are EPROTONOSUPPORT, and
            // a type with no protocols at all is ESOCKTNOSUPPORT. Beside the ones
            // modelled, a kernel may register SCTP (132, stream and seqpacket),
            // SMC (256) and MPTCP (262) for streams, and L2TP (115) and UDP-Lite
            // (136) for datagrams, depending on how it was built.
            match baseType with
            | SockStream ->
                match protocol with
                | 0
                | IpProtoTcp -> shape SocketKind.Stream
                | 132
                | 256
                | 262 -> buildDependent ()
                | _ -> SocketDecoding.Fails UnixError.EPROTONOSUPPORT
            | SockDgram ->
                match protocol with
                | 0
                | IpProtoUdp -> shape SocketKind.Datagram
                | _ when protocol = icmp -> SocketDecoding.Refused (SocketRefusal.IcmpDatagram (domain, protocol))
                | 115
                | 136 -> buildDependent ()
                | _ -> SocketDecoding.Fails UnixError.EPROTONOSUPPORT
            | SockRaw ->
                // The raw switch entry matches any protocol but 0, and only
                // then is the caller's privilege checked.
                if protocol = 0 then
                    SocketDecoding.Fails UnixError.EPROTONOSUPPORT
                else
                    SocketDecoding.Refused (SocketRefusal.RawSocket (domain, socketType, protocol))
            // SCTP is the only protocol a seqpacket switch ever holds, so
            // whether it is there decides every answer: a socket for 0 or 132,
            // and EPROTONOSUPPORT against ESOCKTNOSUPPORT for the rest.
            | SockSeqPacket -> buildDependent ()
            | _ -> SocketDecoding.Fails UnixError.ESOCKTNOSUPPORT

        if flags &&& ~~~(LinuxSockNonBlock ||| LinuxSockCloExec) <> 0 then
            SocketDecoding.Fails UnixError.EINVAL
        elif domain < 0 || domain >= LinuxAfMax then
            SocketDecoding.Fails UnixError.EAFNOSUPPORT
        elif baseType >= LinuxSockMax then
            SocketDecoding.Fails UnixError.EINVAL
        elif
            domain = SimulatedUnixPlatform.internetAddressFamily
            && baseType = LinuxSockPacket
        then
            // `__sock_create` redirects this to `AF_PACKET`, whose sockets need
            // `CAP_NET_RAW` whatever the protocol: measured, every protocol is
            // EPERM at euid 1000, out-of-range ones included, and every one
            // creates a socket as root.
            SocketDecoding.Refused (SocketRefusal.RawSocket (domain, socketType, protocol))
        elif domain = AfUnix then
            // `unix_create` accepts `PF_UNIX` as well as 0, and discards it:
            // `getsockopt(SO_PROTOCOL)` reads 0 for both. It checks the
            // protocol before the type.
            if protocol <> 0 && protocol <> AfUnix then
                SocketDecoding.Fails UnixError.EPROTONOSUPPORT
            else

            match baseType with
            | SockStream ->
                SocketDecoding.Creates (SocketDomain.Unix, SocketKind.Stream, SocketProtocol.Default, nonBlocking)
            // A `SOCK_RAW` request makes a datagram socket: `SO_TYPE` reads
            // `SOCK_DGRAM` for it.
            | SockDgram
            | SockRaw ->
                SocketDecoding.Creates (SocketDomain.Unix, SocketKind.Datagram, SocketProtocol.Default, nonBlocking)
            | SockSeqPacket ->
                SocketDecoding.Creates (SocketDomain.Unix, SocketKind.SeqPacket, SocketProtocol.Default, nonBlocking)
            | _ -> SocketDecoding.Fails UnixError.ESOCKTNOSUPPORT
        elif domain = SimulatedUnixPlatform.internetAddressFamily then
            internet IpProtoIcmp
        elif domain = inet6 then
            internet IpProtoIcmpV6
        elif domain = AfUnspec then
            SocketDecoding.Fails UnixError.EAFNOSUPPORT
        else
            // Every other number below `AF_MAX` is a family Linux has assigned,
            // and whether this kernel has it depends on how it was built and
            // which modules it can load: EAFNOSUPPORT if not.
            SocketDecoding.Refused (SocketRefusal.UnmodelledDomain domain)

    // Darwin's rule is `socreate_internal` over each domain's protocol switch.
    // With a protocol, it takes the entry of exactly that protocol and type, or
    // for `SOCK_RAW` the domain's catch-all raw entry; without one, the first
    // entry of that type. Finding nothing, it answers EAFNOSUPPORT for a domain
    // it does not have, EPROTOTYPE for a protocol it has under some other type,
    // and EPROTONOSUPPORT otherwise. Darwin has no type flags: every bit of the
    // type word must match an entry.
    //
    // Measured on Darwin 27.0.0 (arm64) at euid 501 by the same probe and sweep
    // as Linux's, and checked in beside it. The switches below are that
    // measurement's: every protocol that answered EPROTOTYPE under some type,
    // at the type that did not. Only the order of the first two entries of a
    // type matters, and that is `in_proto.c`'s and `in6_proto.c`'s.

    let private darwinUnixSwitch : (int * int * DarwinAttach) list =
        [
            SockStream, 0, DarwinAttach.Creates SocketKind.Stream
            SockDgram, 0, DarwinAttach.Creates SocketKind.Datagram
        ]

    let private darwinInetSwitch : (int * int * DarwinAttach) list =
        [
            SockDgram, IpProtoUdp, DarwinAttach.Creates SocketKind.Datagram
            SockStream, IpProtoTcp, DarwinAttach.Creates SocketKind.Stream
            SockRaw, 255, DarwinAttach.Raw
            SockRaw, IpProtoIcmp, DarwinAttach.Raw
            SockDgram, IpProtoIcmp, DarwinAttach.IcmpDatagram
            SockRaw, 2, DarwinAttach.Raw
            SockRaw, 4, DarwinAttach.Raw
            SockRaw, 41, DarwinAttach.Raw
            SockRaw, 47, DarwinAttach.Raw
            // ESP and AH answer EOPNOTSUPP rather than EPERM at euid 501.
            SockRaw, 50, DarwinAttach.Raw
            SockRaw, 51, DarwinAttach.Raw
            SockRaw, 0, DarwinAttach.Raw
        ]

    let private darwinInet6Switch : (int * int * DarwinAttach) list =
        [
            // A placeholder entry with no type, which only a request for type 0
            // and protocol 41 selects: measured, that is EOPNOTSUPP.
            0, 41, DarwinAttach.NotSupported
            SockDgram, IpProtoUdp, DarwinAttach.Creates SocketKind.Datagram
            SockStream, IpProtoTcp, DarwinAttach.Creates SocketKind.Stream
            SockRaw, 255, DarwinAttach.Raw
            SockRaw, IpProtoIcmpV6, DarwinAttach.Raw
            SockDgram, IpProtoIcmpV6, DarwinAttach.IcmpDatagram
            SockRaw, 4, DarwinAttach.Raw
            SockRaw, 41, DarwinAttach.Raw
            // Routing, fragment, ESP, AH and destination-options headers
            // answer EOPNOTSUPP rather than EPERM at euid 501.
            SockRaw, 43, DarwinAttach.Raw
            SockRaw, 44, DarwinAttach.Raw
            SockRaw, 50, DarwinAttach.Raw
            SockRaw, 51, DarwinAttach.Raw
            SockRaw, 60, DarwinAttach.Raw
            SockRaw, 0, DarwinAttach.Raw
        ]

    /// The families Darwin has besides the three modelled: `AF_ROUTE`,
    /// `AF_NDRV`, `AF_KEY`, `AF_SYSTEM`, 34, `AF_MULTIPATH` and `AF_VSOCK`.
    let private darwinOtherDomains : Set<int> =
        Set.ofList [ 17 ; 27 ; 29 ; 32 ; 34 ; 39 ; 40 ]

    let private decodeDarwin
        (platform : SimulatedUnixPlatform)
        (domain : int)
        (socketType : int)
        (protocol : int)
        : SocketDecoding
        =
        let inet6 = SimulatedUnixPlatform.internetV6AddressFamily platform

        let select (switch : (int * int * DarwinAttach) list) (socketDomain : SocketDomain) : SocketDecoding =
            let selected =
                if protocol <> 0 then
                    switch
                    |> List.tryFind (fun (entryType, entryProtocol, _) ->
                        entryProtocol = protocol && entryType = socketType
                    )
                    |> Option.orElse (
                        if socketType = SockRaw then
                            switch
                            |> List.tryFind (fun (entryType, entryProtocol, _) ->
                                entryType = SockRaw && entryProtocol = 0
                            )
                        else
                            None
                    )
                else
                    // An entry with no type is never the first of a type.
                    switch
                    |> List.tryFind (fun (entryType, _, _) -> entryType <> 0 && entryType = socketType)

            match selected with
            | Some (_, _, DarwinAttach.Creates kind) ->
                SocketDecoding.Creates (socketDomain, kind, requestedProtocol protocol, false)
            | Some (_, _, DarwinAttach.Raw) ->
                SocketDecoding.Refused (SocketRefusal.RawSocket (domain, socketType, protocol))
            | Some (_, _, DarwinAttach.IcmpDatagram) ->
                SocketDecoding.Refused (SocketRefusal.IcmpDatagram (domain, protocol))
            | Some (_, _, DarwinAttach.NotSupported) -> SocketDecoding.Fails UnixError.EOPNOTSUPP
            | None ->
                if
                    protocol <> 0
                    && switch |> List.exists (fun (_, entryProtocol, _) -> entryProtocol = protocol)
                then
                    SocketDecoding.Fails UnixError.EPROTOTYPE
                else
                    SocketDecoding.Fails UnixError.EPROTONOSUPPORT

        if domain = AfUnix then
            select darwinUnixSwitch SocketDomain.Unix
        elif domain = SimulatedUnixPlatform.internetAddressFamily then
            select darwinInetSwitch SocketDomain.Inet
        elif domain = inet6 then
            select darwinInet6Switch SocketDomain.Inet6
        elif Set.contains domain darwinOtherDomains then
            SocketDecoding.Refused (SocketRefusal.UnmodelledDomain domain)
        else
            SocketDecoding.Fails UnixError.EAFNOSUPPORT

    /// Allocate a fresh socket and a fresh descriptor onto it.
    ///
    /// One operation for both allocations, rather than a socket-table insert
    /// beside a separate `FileDescriptorRegistry.createSocket`, because the two
    /// must agree: the identity this mints is the identity the description
    /// names, and splitting them would let a caller do one without the other.
    let private allocate<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (domain : SocketDomain)
        (kind : SocketKind)
        (protocol : SocketProtocol)
        (nonBlocking : bool)
        (system : UnixSystem<'Task, 'Handler>)
        : int * UnixSystem<'Task, 'Handler>
        =
        let socketId = system.Machine.NextSocketId
        let (SocketId raw) = socketId

        let fd, registry =
            FileDescriptorRegistry.createSocket socketId system.Process.FileDescriptors

        let registry =
            if nonBlocking then
                FileDescriptorRegistry.setNonBlocking fd true registry
            else
                registry

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

    /// `socket(2)`: create a socket in `domain`, of `socketType`, speaking
    /// `protocol`, and a descriptor onto it: the lowest one not in use.
    ///
    /// All three numbers are the simulated flavour's own, as a caller of its
    /// libc would pass them: `AF_INET6` is 10 on Linux and 30 on Darwin.
    /// `socketType` may carry Linux's `SOCK_NONBLOCK`, which makes the new open
    /// file description non-blocking, and `SOCK_CLOEXEC`, which is accepted and
    /// has no effect here: it sets `FD_CLOEXEC`, which matters only across
    /// `exec`, and this kernel models neither `exec` nor any per-descriptor flag.
    /// Darwin has neither flag, and a type carrying either bit names no type.
    ///
    /// The sockets created are stream and datagram sockets in `AF_INET` and
    /// `AF_INET6` (TCP and UDP), and in `AF_UNIX` stream and datagram sockets,
    /// and on Linux seqpacket ones. Linux makes an `AF_UNIX` `SOCK_RAW` request
    /// a datagram socket. Anything else is the errno the flavour answers, or a
    /// `SocketRefusal` where that answer depends on what this kernel does not
    /// model. A failure changes nothing.
    let socket<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (domain : int)
        (socketType : int)
        (protocol : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<Result<int * UnixSystem<'Task, 'Handler>, UnixError>, SocketRefusal>
        =
        let decoding =
            match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
            | SimulatedUnixFlavour.Linux -> decodeLinux system.Machine.UnixPlatform domain socketType protocol
            | SimulatedUnixFlavour.Darwin -> decodeDarwin system.Machine.UnixPlatform domain socketType protocol

        match decoding with
        | SocketDecoding.Refused refusal -> Error refusal
        | SocketDecoding.Fails error -> Ok (Error error)
        | SocketDecoding.Creates (socketDomain, kind, socketProtocol, nonBlocking) ->
            Ok (Ok (allocate socketDomain kind socketProtocol nonBlocking system))

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
        | Some (OpenFileTarget.Directory _)
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

    /// `bind(2)`: give `fd` a local address.
    ///
    /// `family` and `endpoint` are what the caller read out of its sockaddr, and
    /// must be exactly what `admitSockaddrCopy` asked for -- the same contract
    /// `connect` states, and for the same reason: this kernel's answer for a
    /// field it *could not read* is measured and different from its answer for a
    /// field nobody read.
    ///
    /// Which existing bindings the new one conflicts with depends on
    /// `SO_REUSEADDR` as `setsockopt` last left it, on this socket and on the
    /// others; see `SimulatedUnixPlatform.bindConflict`.
    ///
    /// Answers where the socket ended up, which for a request of port 0 is a
    /// port this kernel chose.
    let bind<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (destination : UserBuffer)
        (declaredLength : int)
        (family : int option)
        (endpoint : InternetEndpoint option)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<BindAnswer * UnixSystem<'Task, 'Handler>, BindRefusal>
        =
        match admitSockaddrCopy fd destination declaredLength system with
        | Error refusal -> Error (BindRefusal.Copy refusal)
        | Ok (SockaddrCopyAdmission.Answered error) -> Ok (BindAnswer.Failed error, system)
        | Ok (SockaddrCopyAdmission.Transfer (_, fields)) ->

        SockaddrCopyFields.checkSupplied "UnixSocket.bind" fields family endpoint

        // The admission has classified the descriptor as an IPv4 socket, or it
        // would not have reached the copy.
        let socketId =
            match FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors with
            | Some (OpenFileTarget.Socket socketId) -> socketId
            | other ->
                failwith
                    $"UnixSocket.bind: fd %d{fd} names %A{other}, yet the sockaddr admission reached the copy, which it does only for a socket (this is a bug in this library)."

        let socket = UnixMachineState.socket socketId system.Machine

        let withSocket (socket : SocketDescription) (system : UnixSystem<'Task, 'Handler>) =
            { system with
                Machine =
                    { system.Machine with
                        Sockets = Map.add socketId socket system.Machine.Sockets
                    }
            }

        let platform = system.Machine.UnixPlatform

        // Asked again rather than carried out of the admission: the admission
        // answers the *outright* rejection, and this is the same verdict read
        // for the fault set, where a length that is merely not a `sockaddr_in`
        // ranks against the other faults rather than pre-empting them.
        let lengthFault =
            SimulatedUnixPlatform.bindAddressLength
                platform
                SimulatedUnixPlatform.internetSocketAddressSize
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
                    LockedPort = endpoint.Port <> 0us
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
                && UnixProcessState.callerPrivilege system.Process = CallerPrivilege.Unprivileged

        let conflictsWith (binding : SocketBinding) : bool =
            UnixMachineState.bindingConflicts socketId socket binding system.Machine

        // A request for port 0 conflicts with nothing here: the allocator's
        // own search is what keeps the port it picks free, and a socket a
        // Linux dissolve left half-bound at `address:0` reserves no port, so
        // comparing two zero ports would refuse a bind that succeeds.
        let addressInUseFault =
            match candidate with
            | Some binding when binding.Endpoint.Port <> 0us -> conflictsWith binding
            | Some _
            | None -> false

        let faults =
            [
                BindFault.Length, lengthFault
                BindFault.Family, familyFault
                BindFault.AddressNotLocal, addressNotLocalFault
                BindFault.PrivilegedPort, privilegedPortFault
                // Linux's `inet_bind` refuses on `inet_num`, the port, not on
                // the address: a datagram socket whose dissolve left it
                // half-bound (`127.0.0.1:0`) rebinds -- measured, on both
                // addresses and both kinds of port.
                BindFault.AlreadyBound, (socket.Binding |> Option.exists (fun binding -> binding.Endpoint.Port <> 0us))
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
                 let candidate (port : uint16) : SocketBinding =
                     { binding with
                         Endpoint =
                             { binding.Endpoint with
                                 Port = port
                             }
                     }

                 UnixMachineState.allocateEphemeralPort
                     EphemeralPortUse.Reserve
                     socketId
                     socket
                     candidate
                     system.Machine)
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
        | OpenFileTarget.Directory _
        | OpenFileTarget.StandardStream _
        | OpenFileTarget.SocketEventPort _ -> Ok (ListenAnswer.Failed UnixError.ENOTSOCK, system)
        | OpenFileTarget.Socket socketId ->

        let socket = UnixMachineState.socket socketId system.Machine

        match socket.Domain with
        | SocketDomain.Inet6
        | SocketDomain.Unix -> Error (ListenRefusal.UnmodelledDomain (socketId, socket.Domain))
        | SocketDomain.Inet ->

        match socket.Kind with
        | SocketKind.Datagram -> Ok (ListenAnswer.Failed UnixError.EOPNOTSUPP, system)
        | SocketKind.SeqPacket -> Error (ListenRefusal.UnmeasuredKind (socketId, socket.Kind))
        | SocketKind.Stream ->

        // Split out rather than matched in place: a refusal in a `failwith`
        // types as anything, but a refusal that is a value has to be produced
        // before the rest of the function continues.
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
            && UnixMachineState.bindingConflicts socketId socket binding system.Machine
            ->
            Ok (ListenAnswer.Failed UnixError.EADDRINUSE, system)
        | _ ->

        match
            (match socket.Binding with
             | Some binding -> Some (binding, system.Machine)
             | None ->
                 // `listen(2)` on an unbound socket binds it to the wildcard and
                 // an ephemeral port. Measured on both.
                 let candidate (port : uint16) : SocketBinding =
                     {
                         Endpoint = InternetEndpoint.ofParts InternetEndpoint.WildcardAddress port
                         // This implicit bind runs no `bind(2)`, so nothing is
                         // locked.
                         LockedAddress = None
                         LockedPort = false
                     }

                 UnixMachineState.allocateEphemeralPort
                     EphemeralPortUse.Reserve
                     socketId
                     socket
                     candidate
                     system.Machine)
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
        | OpenFileTarget.Directory _
        | OpenFileTarget.StandardStream _
        | OpenFileTarget.SocketEventPort _ -> Ok (GetSockNameAnswer.Failed (UnixError.ENOTSOCK, None))
        | OpenFileTarget.Socket socketId ->

        let socket = UnixMachineState.socket socketId system.Machine

        match socket.Domain with
        | SocketDomain.Inet6
        | SocketDomain.Unix -> Error (GetSockNameRefusal.UnmodelledDomain (socketId, socket.Domain))
        | SocketDomain.Inet ->

        let reportedLength = SimulatedUnixPlatform.internetSocketAddressSize

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

    /// `sizeof(int)`: what both kernels copy in for `SO_REUSEADDR` whatever
    /// length the caller declares, and the most they copy out.
    let private optionIntSize : int = 4

    /// The options this kernel models, each named once its number has been
    /// decoded under the simulated platform.
    [<RequireQualifiedAccess>]
    type private ModelledOption = | ReuseAddress

    let private decodeOption
        (platform : SimulatedUnixPlatform)
        (level : int)
        (optionName : int)
        : ModelledOption option
        =
        if
            level = SimulatedUnixPlatform.socketOptionLevel platform
            && optionName = SimulatedUnixPlatform.reuseAddressOption platform
        then
            Some ModelledOption.ReuseAddress
        else
            None

    /// The descriptor screens `setsockopt(2)` and `getsockopt(2)` share, which
    /// both flavours make ahead of anything about the option: EBADF, then
    /// ENOTSOCK.
    let private socketOf<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SocketId, UnixError>
        =
        match FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors with
        | None -> Error UnixError.EBADF
        | Some (OpenFileTarget.File _)
        | Some (OpenFileTarget.Directory _)
        | Some (OpenFileTarget.StandardStream _)
        | Some (OpenFileTarget.SocketEventPort _) -> Error UnixError.ENOTSOCK
        | Some (OpenFileTarget.Socket socketId) -> Ok socketId

    /// Everything `setsockopt(2)` decides before the kernel copies the option's
    /// value in. See `SetSockOptAdmission`.
    ///
    /// `level` and `optionName` are in the simulated platform's own numbering;
    /// `SimulatedUnixPlatform.socketOptionLevel` and
    /// `SimulatedUnixPlatform.reuseAddressOption` name the one pair modelled.
    /// `optionLength` is the caller's `socklen_t` exactly as passed: the
    /// platforms disagree about whether one at or above 2^31 is negative.
    ///
    /// Changes nothing: everything a `setsockopt` does before the copy is a
    /// question.
    let admitSetSockOpt<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (level : int)
        (optionName : int)
        (value : UserBuffer)
        (optionLength : uint32)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SetSockOptAdmission, SocketOptionRefusal>
        =
        let platform = system.Machine.UnixPlatform
        let flavour = SimulatedUnixPlatform.flavour platform
        let answered (error : UnixError) = Ok (SetSockOptAdmission.Answered error)

        // Darwin's `setsockopt` compares the value pointer with NULL before it
        // looks at the descriptor, whatever the option: measured EFAULT on a
        // closed descriptor, on a pipe, and at an unknown level. It asks only
        // when the declared length is non-zero.
        let darwinNullScreen : Result<UnixError option, SocketOptionRefusal> =
            match flavour with
            | SimulatedUnixFlavour.Linux -> Ok None
            | SimulatedUnixFlavour.Darwin ->
                if optionLength = 0u then
                    Ok None
                else
                    match value with
                    | UserBuffer.Unmapped 0UL -> Ok (Some UnixError.EFAULT)
                    | UserBuffer.Addressless -> Error (SocketOptionRefusal.Buffer BufferRefusal.AddresslessAtScreen)
                    | UserBuffer.Unmapped _
                    | UserBuffer.Mapped
                    | UserBuffer.Opaque -> Ok None

        match darwinNullScreen with
        | Error refusal -> Error refusal
        | Ok (Some error) -> answered error
        | Ok None ->

        match socketOf fd system with
        | Error error -> answered error
        | Ok socketId ->

        // Linux reads the length as an `int` and refuses a negative one before
        // it dispatches on the option: measured EINVAL at an unknown level too.
        // Darwin reads it as the `socklen_t` it is, so the same bits are a
        // length of more than two gigabytes there.
        if flavour = SimulatedUnixFlavour.Linux && int optionLength < 0 then
            answered UnixError.EINVAL
        else

        match decodeOption platform level optionName with
        | None -> Error (SocketOptionRefusal.UnmodelledOption (socketId, level, optionName))
        | Some ModelledOption.ReuseAddress ->

        let socket = UnixMachineState.socket socketId system.Machine

        // Darwin refuses every option on a socket that can neither send nor
        // receive any more, ahead of the length and the copy: measured EINVAL
        // after a refused connect, whether or not the refusal is still pending,
        // even through an unmapped value. Linux takes the option in every phase.
        let darwinShutDown =
            flavour = SimulatedUnixFlavour.Darwin
            && match socket.Phase with
               | SocketPhase.RefusedPendingDelivery
               | SocketPhase.Dead -> true
               | SocketPhase.Idle
               | SocketPhase.Listening _
               | SocketPhase.EstablishedPendingReport _
               | SocketPhase.Established _
               | SocketPhase.DatagramPeer _ -> false

        if darwinShutDown then
            answered UnixError.EINVAL
        elif optionLength < uint32 optionIntSize then
            answered UnixError.EINVAL
        else

        match value with
        | UserBuffer.Unmapped _ -> answered UnixError.EFAULT
        | UserBuffer.Opaque -> Error (SocketOptionRefusal.Buffer BufferRefusal.OpaqueAtTransfer)
        | UserBuffer.Addressless -> Error (SocketOptionRefusal.Buffer BufferRefusal.AddresslessAtTransfer)
        | UserBuffer.Mapped -> Ok (SetSockOptAdmission.Transfer optionIntSize)

    /// `setsockopt(2)`: set a socket option.
    ///
    /// `supplied` is the `int` the caller read out of its value buffer, and
    /// must be present exactly when `admitSetSockOpt` answers `Transfer`: the
    /// kernel's answer for a value it could not read is different from its
    /// answer for a value nobody read.
    ///
    /// For `SO_REUSEADDR`, any non-zero value sets the option and zero clears
    /// it; bytes beyond the first `int` are never read. The option persists
    /// until the next `setsockopt` of it, and no later failure of another call
    /// undoes it. A change on a listener with connections waiting to be
    /// accepted is refused; see
    /// `SocketOptionRefusal.ListenerWithQueuedConnections`.
    let setsockopt<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (level : int)
        (optionName : int)
        (value : UserBuffer)
        (optionLength : uint32)
        (supplied : int option)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SetSockOptAnswer * UnixSystem<'Task, 'Handler>, SocketOptionRefusal>
        =
        match admitSetSockOpt fd level optionName value optionLength system with
        | Error refusal ->
            if supplied.IsSome then
                failwith
                    "UnixSocket.setsockopt: the caller supplied a value that the kernel never reads, because the call is refused before the copy (this is a bug in the caller)."

            Error refusal
        | Ok (SetSockOptAdmission.Answered error) ->
            if supplied.IsSome then
                failwith
                    $"UnixSocket.setsockopt: the caller supplied a value that the kernel never reads, because the call answers %O{error} before the copy (this is a bug in the caller)."

            Ok (SetSockOptAnswer.Failed error, system)
        | Ok (SetSockOptAdmission.Transfer _) ->

        let supplied =
            match supplied with
            | Some supplied -> supplied
            | None ->
                failwith
                    "UnixSocket.setsockopt: the kernel copies the option's value in, but the caller supplied none (this is a bug in the caller)."

        let socketId =
            match socketOf fd system with
            | Ok socketId -> socketId
            | Error error ->
                failwith
                    $"UnixSocket.setsockopt: fd %d{fd} answers %O{error}, yet the admission reached the copy (this is a bug in this library)."

        let socket = UnixMachineState.socket socketId system.Machine
        let requested = supplied <> 0

        let hasQueuedConnections =
            match socket.Phase with
            | SocketPhase.Listening listenState -> not (List.isEmpty listenState.Queue)
            | SocketPhase.Idle
            | SocketPhase.EstablishedPendingReport _
            | SocketPhase.Established _
            | SocketPhase.RefusedPendingDelivery
            | SocketPhase.Dead
            | SocketPhase.DatagramPeer _ -> false

        // Setting the value it already has changes nothing a queued connection
        // could have copied, so only a change is refused.
        if hasQueuedConnections && requested <> socket.ReuseAddress then
            Error (SocketOptionRefusal.ListenerWithQueuedConnections socketId)
        else

        let system =
            { system with
                Machine =
                    { system.Machine with
                        Sockets =
                            Map.add
                                socketId
                                { socket with
                                    ReuseAddress = requested
                                }
                                system.Machine.Sockets
                    }
            }

        Ok (SetSockOptAnswer.Set, system)

    /// Everything `getsockopt(2)` decides before the kernel reads the caller's
    /// length cell. See `GetSockOptAdmission`.
    ///
    /// `level` and `optionName` are numbered as for `admitSetSockOpt`.
    ///
    /// Changes nothing.
    let admitGetSockOpt<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (level : int)
        (optionName : int)
        (length : UserBuffer)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<GetSockOptAdmission, SocketOptionRefusal>
        =
        // Unlike `setsockopt`, the descriptor comes first on both flavours:
        // measured EBADF and ENOTSOCK through a null length cell.
        match socketOf fd system with
        | Error error -> Ok (GetSockOptAdmission.Answered error)
        | Ok socketId ->

        match decodeOption system.Machine.UnixPlatform level optionName with
        | None -> Error (SocketOptionRefusal.UnmodelledOption (socketId, level, optionName))
        | Some ModelledOption.ReuseAddress ->

        // Measured EFAULT on both for a null and for an unmapped cell, with
        // neither buffer written.
        match length with
        | UserBuffer.Unmapped _ -> Ok (GetSockOptAdmission.Answered UnixError.EFAULT)
        | UserBuffer.Opaque -> Error (SocketOptionRefusal.Buffer BufferRefusal.OpaqueAtTransfer)
        | UserBuffer.Addressless -> Error (SocketOptionRefusal.Buffer BufferRefusal.AddresslessAtTransfer)
        | UserBuffer.Mapped -> Ok GetSockOptAdmission.ReadLength

    /// `getsockopt(2)`: read a socket option.
    ///
    /// `declaredLength` is the `socklen_t` the caller read out of its length
    /// cell, and must be present exactly when `admitGetSockOpt` answers
    /// `ReadLength`.
    ///
    /// For `SO_REUSEADDR` the value is 0 when the option is clear; when it is
    /// set, Linux reports 1 and Darwin reports the option's own number, 4.
    ///
    /// Changes nothing and returns no system: this `getsockopt` reads.
    let getsockopt<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (level : int)
        (optionName : int)
        (value : UserBuffer)
        (length : UserBuffer)
        (declaredLength : uint32 option)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<GetSockOptAnswer, SocketOptionRefusal>
        =
        match admitGetSockOpt fd level optionName length system with
        | Error refusal ->
            if declaredLength.IsSome then
                failwith
                    "UnixSocket.getsockopt: the caller supplied a length that the kernel never reads, because the call is refused before it would (this is a bug in the caller)."

            Error refusal
        | Ok (GetSockOptAdmission.Answered error) ->
            if declaredLength.IsSome then
                failwith
                    $"UnixSocket.getsockopt: the caller supplied a length that the kernel never reads, because the call answers %O{error} first (this is a bug in the caller)."

            Ok (GetSockOptAnswer.Failed error)
        | Ok GetSockOptAdmission.ReadLength ->

        let declaredLength =
            match declaredLength with
            | Some declaredLength -> declaredLength
            | None ->
                failwith
                    "UnixSocket.getsockopt: the kernel reads the caller's length cell, but the caller supplied no length (this is a bug in the caller)."

        let platform = system.Machine.UnixPlatform
        let flavour = SimulatedUnixPlatform.flavour platform

        let socketId =
            match socketOf fd system with
            | Ok socketId -> socketId
            | Error error ->
                failwith
                    $"UnixSocket.getsockopt: fd %d{fd} answers %O{error}, yet the admission read the length cell (this is a bug in this library)."

        let socket = UnixMachineState.socket socketId system.Machine

        // Darwin's `so_options & SO_REUSEADDR`, which is the option's bit;
        // Linux's `sk_reuse`, which a set stores as 1 whatever non-zero value
        // it was given. Both measured.
        let reported =
            if not socket.ReuseAddress then
                0
            else
                match flavour with
                | SimulatedUnixFlavour.Linux -> 1
                | SimulatedUnixFlavour.Darwin -> SimulatedUnixPlatform.reuseAddressOption platform

        // As for `setsockopt`, Linux reads the length as an `int`: measured
        // EINVAL at -1 and at 2^31, with neither buffer written. Darwin reads
        // the `socklen_t` and copies `sizeof(int)`.
        if flavour = SimulatedUnixFlavour.Linux && int declaredLength < 0 then
            Ok (GetSockOptAnswer.Failed UnixError.EINVAL)
        else

        let count = min declaredLength (uint32 optionIntSize)

        if count = 0u then
            Ok (GetSockOptAnswer.Reported (reported, 0u))
        else

        match flavour, value with
        // Darwin skips the copy for a null value buffer and reports a length of
        // 0, whatever was declared: measured at 2, 4 and -1. Linux copies
        // through it like any other address.
        | SimulatedUnixFlavour.Darwin, UserBuffer.Unmapped 0UL -> Ok (GetSockOptAnswer.Reported (reported, 0u))
        | SimulatedUnixFlavour.Darwin, UserBuffer.Addressless ->
            Error (SocketOptionRefusal.Buffer BufferRefusal.AddresslessAtScreen)
        // Measured on both: EFAULT, and the length cell keeps what the caller
        // put there.
        | _, UserBuffer.Unmapped _ -> Ok (GetSockOptAnswer.Failed UnixError.EFAULT)
        | _, UserBuffer.Opaque -> Error (SocketOptionRefusal.Buffer BufferRefusal.OpaqueAtTransfer)
        | _, UserBuffer.Addressless -> Error (SocketOptionRefusal.Buffer BufferRefusal.AddresslessAtTransfer)
        | _, UserBuffer.Mapped -> Ok (GetSockOptAnswer.Reported (reported, count))
