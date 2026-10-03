namespace WoofWare.PosixKernel.Test

open System
open System.Buffers.Binary
open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// The kernel's own reading of the bytes `bind(2)` and `connect(2)` copy in,
/// held to the reading its first client used to make on its behalf.
///
/// That client read the family, port and address out of the caller's buffer
/// itself, guided by which of them the copy reached, and passed them in. The
/// oracle below is that path transcribed: the field selection the library used
/// to answer, and the client's reads. Every answer through the bytes must equal
/// the answer through the oracle's fields, on every flavour, socket, length and
/// byte content generated.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSockaddrDecoding =

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 2000

    /// The decoding the library's first client did, before the library decoded
    /// for itself. Written with literal offsets rather than the library's layout
    /// descriptors, so that it shares nothing with the code under test.
    [<RequireQualifiedAccess>]
    module private Oracle =

        [<RequireQualifiedAccess>]
        type private Fields =
            | Nothing
            | Family
            | FamilyAndEndpoint

        /// Which fields a copy of `length` bytes reached: the family once it
        /// is reached (two bytes at 0 on Linux, one at 1 on Darwin, so 2 bytes
        /// either way), and the endpoint from `sin_addr`'s end at 8.
        let private fields (length : int) : Fields =
            if length >= 8 then Fields.FamilyAndEndpoint
            elif length >= 2 then Fields.Family
            else Fields.Nothing

        let private family (platform : SimulatedUnixPlatform) (blob : byte[]) : int =
            match SimulatedUnixPlatform.flavour platform with
            | SimulatedUnixFlavour.Darwin -> int blob.[1]
            | SimulatedUnixFlavour.Linux -> int (BinaryPrimitives.ReadUInt16LittleEndian (ReadOnlySpan (blob, 0, 2)))

        let private endpoint (blob : byte[]) : InternetEndpoint =
            InternetEndpoint.ofParts
                (BinaryPrimitives.ReadUInt32BigEndian (ReadOnlySpan (blob, 4, 4)))
                (BinaryPrimitives.ReadUInt16BigEndian (ReadOnlySpan (blob, 2, 2)))

        /// `sin_addr`, taking every byte the copy did not reach as zero.
        let private zeroFilled (blob : byte[]) (length : int) : uint32 =
            let word = [| for i in 4..7 -> if i < length then blob.[i] else 0uy |]
            BinaryPrimitives.ReadUInt32BigEndian (ReadOnlySpan word)

        let decode (platform : SimulatedUnixPlatform) (blob : byte[]) (length : int) : CopiedInternetSockaddr =
            let zeroFilledAddress = zeroFilled blob length

            match fields length with
            | Fields.Nothing ->
                {
                    Family = None
                    Endpoint = None
                    ZeroFilledAddress = zeroFilledAddress
                }
            | Fields.Family ->
                {
                    Family = Some (family platform blob)
                    Endpoint = None
                    ZeroFilledAddress = zeroFilledAddress
                }
            | Fields.FamilyAndEndpoint ->
                {
                    Family = Some (family platform blob)
                    Endpoint = Some (endpoint blob)
                    ZeroFilledAddress = zeroFilledAddress
                }

    let private platforms : SimulatedUnixPlatform list =
        [
            SimulatedUnixPlatform.linuxX64
            SimulatedUnixPlatform.linuxArm64
            SimulatedUnixPlatform.macOsArm64
        ]

    let private loopback (port : uint16) : InternetEndpoint =
        InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress port

    /// The socket a call is made on, before it is made.
    [<RequireQualifiedAccess>]
    type private Socket =
        | Fresh of SocketKind
        | BoundAt5000 of SocketKind
        | ListeningAt5000
        | NonBlockingFresh of SocketKind

    /// A system holding a listener at 127.0.0.1:6000, so that a connect has
    /// somewhere to go, and the socket `socket` describes; and the descriptor
    /// the call is made on, which is that socket's, or one that is not open.
    let private setUp
        (platform : SimulatedUnixPlatform)
        (socket : Socket)
        (closedDescriptor : bool)
        : int * UnixSystem<int, string>
        =
        let system =
            UnixSystem.initial platform UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> UnixBootImage.withLocalAddresses UnixSystem.defaultLocalAddresses []
            |> UnixBootImage.boot

        let listen (fd : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
            match UnixSocket.listen fd 8 system with
            | Ok (ListenAnswer.Listening _, system) -> system
            | other -> failwith $"setUp: listen answered %O{other}"

        let bindTo (fd : int) (port : uint16) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
            match CopyIn.bind fd UserBuffer.Mapped 16u (CopyIn.inet platform (loopback port)) system with
            | Ok (BindAnswer.Bound _, system) -> system
            | other -> failwith $"setUp: bind answered %O{other}"

        let listener, system =
            NewSocket.create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp system

        let system = system |> bindTo listener 6000us |> listen listener

        let create (kind : SocketKind) (system : UnixSystem<int, string>) =
            let protocol =
                match kind with
                | SocketKind.Datagram -> SocketProtocol.Udp
                | _ -> SocketProtocol.Tcp

            NewSocket.create SocketDomain.Inet kind protocol system

        let fd, system =
            match socket with
            | Socket.Fresh kind -> create kind system
            | Socket.BoundAt5000 kind ->
                let fd, system = create kind system
                fd, bindTo fd 5000us system
            | Socket.ListeningAt5000 ->
                let fd, system = create SocketKind.Stream system
                fd, system |> bindTo fd 5000us |> listen fd
            | Socket.NonBlockingFresh kind ->
                let fd, system = create kind system
                fd, snd (UnixSocket.setNonBlocking fd true system)

        (if closedDescriptor then 99 else fd), system

    let private socketGen : Gen<Socket> =
        let kind = Gen.elements [ SocketKind.Stream ; SocketKind.Datagram ]

        Gen.oneof
            [
                Gen.map Socket.Fresh kind
                Gen.map Socket.BoundAt5000 kind
                Gen.constant Socket.ListeningAt5000
                Gen.map Socket.NonBlockingFresh kind
            ]

    /// A declared length: mostly the ones a copy can take, and a few the copy
    /// helpers reject outright.
    let private lengthGen : Gen<uint32> =
        Gen.frequency
            [
                10, Gen.choose (0, 130) |> Gen.map uint32
                1, Gen.elements [ 255u ; 256u ; 0x7FFFFFFFu ; 0x80000000u ; 0xFFFFFFFFu ]
            ]

    /// A caller's buffer: random bytes, usually with a family, port and address
    /// worth reaching a connect's or a bind's later rows with.
    let private blobGen : Gen<SimulatedUnixPlatform -> byte[]> =
        gen {
            let! bytes = Gen.arrayOfLength CopyIn.Length (Gen.choose (0, 255))

            let! family =
                Gen.frequency
                    [
                        4, Gen.elements [ 0 ; 1 ; 2 ; 10 ; 30 ] |> Gen.map Some
                        1, Gen.constant None
                    ]

            let! wideFamily = Gen.choose (0, 65535)

            let! endpoint =
                Gen.frequency
                    [
                        3,
                        Gen.elements [ loopback 6000us ; loopback 5000us ; loopback 0us ]
                        |> Gen.map Some
                        1, Gen.constant (Some (InternetEndpoint.ofParts InternetEndpoint.WildcardAddress 6000us))
                        1, Gen.constant None
                    ]

            return
                fun platform ->
                    let blob = bytes |> Array.map byte

                    match family with
                    | None -> ()
                    | Some family ->
                        let laidOut = CopyIn.blob platform family (loopback 0us)
                        // The family, and on Linux a high byte that is sometimes not zero.
                        Array.blit laidOut 0 blob 0 2

                        if wideFamily % 4 = 0 then
                            match SimulatedUnixPlatform.flavour platform with
                            | SimulatedUnixFlavour.Linux -> blob.[1] <- byte (wideFamily >>> 8)
                            | SimulatedUnixFlavour.Darwin -> ()

                    match endpoint with
                    | None -> ()
                    | Some endpoint -> Array.blit (CopyIn.blob platform 0 endpoint) 2 blob 2 6

                    blob
        }

    let private bufferGen : Gen<UserBuffer> =
        Gen.frequency
            [
                6, Gen.constant UserBuffer.Mapped
                1, Gen.constant (UserBuffer.Unmapped 0x1000UL)
            ]

    type private Case =
        {
            Platform : SimulatedUnixPlatform
            Socket : Socket
            Closed : bool
            Buffer : UserBuffer
            Length : uint32
            Blob : byte[]
        }

    let private caseGen : Gen<Case> =
        gen {
            let! platform = Gen.elements platforms
            let! socket = socketGen
            let! closed = Gen.frequency [ 9, Gen.constant false ; 1, Gen.constant true ]
            let! buffer = bufferGen
            let! length = lengthGen
            let! blob = blobGen

            return
                {
                    Platform = platform
                    Socket = socket
                    Closed = closed
                    Buffer = buffer
                    Length = length
                    Blob = blob platform
                }
        }

    /// The bytes the admission says the copy takes from the case's buffer.
    let private copied
        (syscall : SockaddrCopySyscall)
        (fd : int)
        (case : Case)
        (system : UnixSystem<int, string>)
        : ImmutableArray<byte>
        =
        CopyIn.admitted syscall fd case.Buffer case.Length case.Blob system

    [<Test>]
    let ``bind decodes its bytes as its client used to`` () : unit =
        let property (case : Case) : unit =
            let fd, system = setUp case.Platform case.Socket case.Closed

            let throughBytes =
                UnixSocket.bind fd case.Buffer case.Length (copied SockaddrCopySyscall.Bind fd case system) system

            let throughOracle =
                match UnixSocket.admitSockaddrCopy SockaddrCopySyscall.Bind fd case.Buffer case.Length system with
                | Error refusal -> Error (BindRefusal.Copy refusal)
                | Ok (SockaddrCopyAdmission.Answered error) -> Ok (BindAnswer.Failed error, system)
                | Ok (SockaddrCopyAdmission.Transfer length) ->
                    UnixSocket.bindDecoded fd case.Length (Oracle.decode case.Platform case.Blob length) system

            throughBytes |> shouldEqual throughOracle

        Check.One (config, Prop.forAll (Arb.fromGen caseGen) property)

    [<Test>]
    let ``connect decodes its bytes as its client used to`` () : unit =
        let property (case : Case) : unit =
            let fd, system = setUp case.Platform case.Socket case.Closed

            let throughBytes =
                UnixConnection.connect
                    fd
                    case.Buffer
                    case.Length
                    (copied SockaddrCopySyscall.Connect fd case system)
                    system

            let throughOracle =
                match UnixSocket.admitSockaddrCopy SockaddrCopySyscall.Connect fd case.Buffer case.Length system with
                | Error refusal -> Error (ConnectRefusal.Copy refusal)
                | Ok (SockaddrCopyAdmission.Answered error) -> Ok (ConnectOutcome.Failed error, system)
                | Ok (SockaddrCopyAdmission.Transfer length) ->
                    let socketId =
                        match FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors with
                        | Some (OpenFileTarget.Socket socketId) -> socketId
                        | other -> failwith $"the admission reached the copy on %O{other}"

                    let nonBlocking =
                        (FileDescriptorRegistry.tryFind fd system.Process.FileDescriptors).Value.NonBlocking

                    UnixConnection.connectDecoded
                        socketId
                        nonBlocking
                        case.Length
                        (Oracle.decode case.Platform case.Blob length)
                        system

            throughBytes |> shouldEqual throughOracle

        Check.One (config, Prop.forAll (Arb.fromGen caseGen) property)

    [<Test>]
    let ``connectSocket decodes its bytes as its client used to`` () : unit =
        let property (case : Case) : unit =
            let fd, system = setUp case.Platform case.Socket false

            let socketId =
                match FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors with
                | Some (OpenFileTarget.Socket socketId) -> socketId
                | other -> failwith $"setUp made %O{other}"

            let length = UnixSocket.mappedCopyLength case.Platform case.Length

            UnixConnection.connectSocket socketId false case.Length (CopyIn.prefix case.Blob length) system
            |> shouldEqual (
                UnixConnection.connectDecoded
                    socketId
                    false
                    case.Length
                    (Oracle.decode case.Platform case.Blob length)
                    system
            )

        Check.One (config, Prop.forAll (Arb.fromGen caseGen) property)

    /// Measured on both flavours (`sockaddr-decoding.c`, sections L, Z and X):
    /// Darwin's `sa_len` byte, `sin_zero`, and every byte after it change no
    /// answer. Stated directly, beside the oracle that implies it.
    [<Test>]
    let ``sa_len, sin_zero and the bytes after them change no answer`` () : unit =
        let gen =
            gen {
                let! case = caseGen
                let! replacement = Gen.arrayOfLength CopyIn.Length (Gen.choose (0, 255))
                return case, replacement |> Array.map byte
            }

        let property (case : Case, replacement : byte[]) : unit =
            let ignored =
                [
                    match SimulatedUnixPlatform.flavour case.Platform with
                    | SimulatedUnixFlavour.Darwin -> yield 0
                    | SimulatedUnixFlavour.Linux -> ()
                    yield! [ 8 .. CopyIn.Length - 1 ]
                ]

            let mutated = Array.copy case.Blob

            for i in ignored do
                mutated.[i] <- replacement.[i]

            let fd, system = setUp case.Platform case.Socket case.Closed

            CopyIn.bind fd case.Buffer case.Length mutated system
            |> shouldEqual (CopyIn.bind fd case.Buffer case.Length case.Blob system)

            CopyIn.connect fd case.Buffer case.Length mutated system
            |> shouldEqual (CopyIn.connect fd case.Buffer case.Length case.Blob system)

        Check.One (config, Prop.forAll (Arb.fromGen gen) property)

    /// A caller that passes any other number of bytes than the copy takes is
    /// refused: a field the kernel could not read and one holding zero have
    /// different answers, so neither too few bytes nor too many has one.
    [<Test>]
    let ``bytes other than the copy's are refused`` () : unit =
        for platform in platforms do
            let fd, system = setUp platform (Socket.Fresh SocketKind.Stream) false
            let blob = CopyIn.inet platform (loopback 7000us)

            for passed in [ 15 ; 17 ; 0 ] do
                (fun () ->
                    UnixSocket.bind fd UserBuffer.Mapped 16u (CopyIn.prefix blob passed) system
                    |> ignore
                )
                |> shouldFail

                (fun () ->
                    UnixConnection.connect fd UserBuffer.Mapped 16u (CopyIn.prefix blob passed) system
                    |> ignore
                )
                |> shouldFail

            // A call the admission answers copies nothing, so it takes nothing.
            (fun () ->
                UnixSocket.bind 99 UserBuffer.Mapped 16u (CopyIn.prefix blob 16) system
                |> ignore
            )
            |> shouldFail

            (fun () ->
                UnixSocket.bind fd UserBuffer.Mapped 16u ImmutableArray<byte>.Empty system
                |> ignore
            )
            |> shouldFail

    /// The family's width and byte order, at the rows the probe measured: Linux
    /// reads two bytes, so a high byte that is not zero is not `AF_INET`, and
    /// Darwin reads the one at offset 1, whatever byte 0 holds.
    [<Test>]
    let ``the family is read at this platform's width and order`` () : unit =
        let decode (platform : SimulatedUnixPlatform) (bytes : byte list) =
            (SimulatedUnixPlatform.decodeInternetSockaddr platform (ImmutableArray.CreateRange bytes)).Family

        decode SimulatedUnixPlatform.linuxX64 [ 2uy ; 0uy ] |> shouldEqual (Some 2)
        decode SimulatedUnixPlatform.linuxX64 [ 2uy ; 2uy ] |> shouldEqual (Some 0x0202)

        decode SimulatedUnixPlatform.linuxArm64 [ 2uy ; 0x80uy ]
        |> shouldEqual (Some 0x8002)

        decode SimulatedUnixPlatform.linuxX64 [ 2uy ] |> shouldEqual None
        decode SimulatedUnixPlatform.macOsArm64 [ 16uy ; 2uy ] |> shouldEqual (Some 2)
        decode SimulatedUnixPlatform.macOsArm64 [ 0uy ; 30uy ] |> shouldEqual (Some 30)
        decode SimulatedUnixPlatform.macOsArm64 [ 2uy ] |> shouldEqual None
