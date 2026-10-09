namespace WoofWare.PosixKernel.Test

open System
open System.Buffers.Binary
open System.Collections.Immutable
open System.Text
open WoofWare.PosixKernel

/// `docs/probes/dual-mode/dual-mode.c`, run against the model instead of a
/// kernel: each section makes the probe's calls, in its order, and prints its
/// lines in its format, so that `TestDualModeMeasured` can hold the model to
/// the probe's outputs line for line.
///
/// A call the model refuses answers `REFUSED`, where the probe prints an errno.
/// Where the probe's own code would take a branch on an answer, so does this.
[<RequireQualifiedAccess>]
module DualModeProbe =

    /// The model, as the probe's C library would see it: a booted system the
    /// calls change in turn.
    type private Libc (platform : SimulatedUnixPlatform) =
        let mutable system : UnixSystem<int, string> =
            UnixSystem.initial platform
            |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

        member _.Platform = platform
        member _.Flavour = SimulatedUnixPlatform.flavour platform

        member _.System
            with get () = system
            and set value = system <- value

    /// A call's answer, as the probe prints it.
    [<RequireQualifiedAccess>]
    type private Answer =
        | Ok
        | Errno of UnixError
        | Refused of reason : string

    let private en (answer : Answer) : string =
        match answer with
        | Answer.Ok -> "OK"
        | Answer.Errno error -> $"%A{error}"
        | Answer.Refused _ -> "REFUSED"

    // `<sys/socket.h>` and `<netinet/in.h>`.
    let private sockStream = 1
    let private ipProtoTcp = 6
    let private inaddrLoopback = 0x7F000001u

    let private af6 (libc : Libc) : int =
        SimulatedUnixPlatform.internetV6AddressFamily libc.Platform

    let private socket (libc : Libc) (domain : int) : int =
        match UnixSocket.socket domain sockStream ipProtoTcp libc.System with
        | Ok (Ok (fd, system)) ->
            libc.System <- system
            fd
        | other -> failwith $"DualModeProbe: socket(%d{domain}) answered %A{other}"

    let private tcp4 (libc : Libc) : int = socket libc 2
    let private tcp6 (libc : Libc) : int = socket libc (af6 libc)

    let private setInt (libc : Libc) (fd : int) (level : int) (name : int) (value : int) : Answer =
        let bytes : byte[] = SimulatedUnixPlatform.encodeCInt libc.Platform value

        let supplied =
            match UnixSocket.admitSetSockOpt fd level name UserBuffer.Mapped 4u libc.System with
            | Result.Ok (SetSockOptAdmission.Transfer count) -> Some (ImmutableArray.Create<byte> (bytes, 0, count))
            | _ -> None

        match UnixSocket.setsockopt fd level name UserBuffer.Mapped 4u supplied libc.System with
        | Result.Ok (SetSockOptAnswer.Set, system) ->
            libc.System <- system
            Answer.Ok
        | Result.Ok (SetSockOptAnswer.Failed error, system) ->
            libc.System <- system
            Answer.Errno error
        | Result.Error refusal -> Answer.Refused (SocketOptionRefusal.describe refusal)

    let private getInt (libc : Libc) (fd : int) (level : int) (name : int) : Answer * int =
        let read =
            match UnixSocket.admitGetSockOpt fd level name UserBuffer.Mapped UserBuffer.Mapped libc.System with
            | Result.Ok GetSockOptAdmission.ReadLength -> Some 4u
            | _ -> None

        match UnixSocket.getsockopt fd level name UserBuffer.Mapped UserBuffer.Mapped read libc.System with
        | Result.Ok (GetSockOptAnswer.Reported bytes, system) ->
            libc.System <- system
            Answer.Ok, SimulatedUnixPlatform.decodeCInt libc.Platform bytes 0
        | Result.Ok (GetSockOptAnswer.Failed (error, _), system) ->
            libc.System <- system
            Answer.Errno error, -1
        | Result.Error refusal -> Answer.Refused (SocketOptionRefusal.describe refusal), -1

    let private ipv6Only (libc : Libc) : int * int =
        SimulatedUnixPlatform.ipv6OptionLevel libc.Platform, SimulatedUnixPlatform.ipv6OnlyOption libc.Platform

    let private tcp6V6Only (libc : Libc) (v6only : int) : int =
        let s = tcp6 libc
        let level, name = ipv6Only libc

        match setInt libc s level name v6only with
        | Answer.Ok -> ()
        | other -> failwith $"DualModeProbe: IPV6_V6ONLY answered %A{other}"

        s

    /// `sockaddr_in`, as `sin_of` lays it out, in a buffer of 300 bytes.
    let private sinOf (libc : Libc) (address : uint32) (port : uint16) : byte[] =
        let blob = Array.zeroCreate<byte> 300
        let inet = CopyIn.inet libc.Platform (InternetEndpoint.ofParts address port)
        Array.blit inet 0 blob 0 16
        blob

    /// `sockaddr_in6`, as `sin6_of` lays it out, in a buffer of 300 bytes.
    let private sin6Of (libc : Libc) (mapped : bool) (address : uint32) (port : uint16) : byte[] =
        let bytes =
            if mapped then
                CopyIn.mappedAddress address
            else
                let bytes = Array.zeroCreate<byte> 16

                if address = 1u then
                    bytes.[15] <- 1uy

                bytes

        let blob = Array.zeroCreate<byte> 300
        Array.blit (CopyIn.blob6 libc.Platform (af6 libc) bytes port) 0 blob 0 28
        blob

    /// Put `family` in a sockaddr's family field, as the probe does by
    /// assigning `sin6_family`.
    let private withFamily (libc : Libc) (family : int) (blob : byte[]) : byte[] =
        let blob = Array.copy blob

        match libc.Flavour with
        | SimulatedUnixFlavour.Linux ->
            BinaryPrimitives.WriteUInt16LittleEndian (Span<byte> (blob, 0, 2), uint16 family)
        | SimulatedUnixFlavour.Darwin -> blob.[1] <- byte family

        blob

    let private bind (libc : Libc) (fd : int) (blob : byte[]) (length : int) : Answer =
        match CopyIn.bind fd UserBuffer.Mapped (uint32 length) blob libc.System with
        | Result.Ok (BindAnswer.Bound _, system) ->
            libc.System <- system
            Answer.Ok
        | Result.Ok (BindAnswer.Failed error, system) ->
            libc.System <- system
            Answer.Errno error
        | Result.Error refusal -> Answer.Refused (BindRefusal.describe refusal)

    let private connect (libc : Libc) (fd : int) (blob : byte[]) (length : int) : Answer =
        match CopyIn.connect fd UserBuffer.Mapped (uint32 length) blob libc.System with
        | Result.Ok (ConnectOutcome.Completed, system) ->
            libc.System <- system
            Answer.Ok
        | Result.Ok (ConnectOutcome.Failed error, system) ->
            libc.System <- system
            Answer.Errno error
        | Result.Error refusal -> Answer.Refused (ConnectRefusal.describe refusal)

    let private listen (libc : Libc) (fd : int) (backlog : int) : Answer =
        match UnixSocket.listen fd backlog libc.System with
        | Result.Ok (ListenAnswer.Listening _, system) ->
            libc.System <- system
            Answer.Ok
        | Result.Ok (ListenAnswer.Failed error, system) ->
            libc.System <- system
            Answer.Errno error
        | Result.Error refusal -> Answer.Refused (ListenRefusal.describe refusal)

    let private close (libc : Libc) (fd : int) : unit =
        match UnixDescriptor.close fd libc.System with
        | Result.Ok (_, system) -> libc.System <- system
        | Result.Error refusal -> failwith $"DualModeProbe: close(%d{fd}) was refused: %A{refusal}"

    let private nonBlocking (libc : Libc) (fd : int) : unit =
        libc.System <-
            UnixSystemState.withFileDescriptors
                (FileDescriptorRegistry.setNonBlocking fd true (UnixSystemState.fileDescriptors libc.System))
                libc.System

    /// A non-blocking accept, as every accept in the probe is by the time the
    /// listener has nothing queued: the new descriptor, or the errno.
    let private accept (libc : Libc) (fd : int) (cell : uint32) : Result<int * ImmutableArray<byte> * int, Answer> =
        match UnixConnection.accept 0 fd UserBuffer.Mapped cell libc.System with
        | Result.Ok (AcceptOutcome.Accepted (accepted, copied, reported), system) ->
            libc.System <- system
            Result.Ok (accepted, copied, reported)
        | Result.Ok (AcceptOutcome.Failed error, system) ->
            libc.System <- system
            Result.Error (Answer.Errno error)
        | Result.Ok (other, _) -> failwith $"DualModeProbe: accept(%d{fd}) answered %A{other}"
        | Result.Error refusal -> Result.Error (Answer.Refused (AcceptRefusal.describe refusal))

    /// `getsockname` or `getpeername` into a buffer of 128 bytes of 0xAA, the
    /// cell preset to `cell`: the answer, the buffer, and the cell after.
    let private name
        (libc : Libc)
        (call : int -> UserBuffer -> uint32 -> UnixSystem<int, string> -> Result<GetSockNameAnswer, GetSockNameRefusal>)
        (fd : int)
        (cell : int)
        : Answer * byte[] * int
        =
        let buffer = Array.create 128 0xAAuy

        match call fd UserBuffer.Mapped (uint32 cell) libc.System with
        | Result.Ok (GetSockNameAnswer.Reported (copied, reported)) ->
            copied.CopyTo (buffer, 0)
            Answer.Ok, buffer, reported
        | Result.Ok (GetSockNameAnswer.Failed (error, _)) -> Answer.Errno error, buffer, cell
        | Result.Error refusal -> Answer.Refused (GetSockNameRefusal.describe refusal), buffer, cell

    /// The probe's port symbols: the listener's `L`, the row's bound `B`.
    type private Ports =
        {
            mutable L : uint16
            mutable B : uint16
        }

    let private sym (ports : Ports) (port : uint16) : string =
        if port = 0us then "0"
        elif port = ports.L then "L"
        elif port = ports.B then "B"
        else "eph"

    /// `inet_ntop(AF_INET6, ...)` for the addresses the probe meets.
    let private ntop6 (bytes : byte[]) : string =
        let zeroUpTo (n : int) =
            Seq.forall (fun i -> bytes.[i] = 0uy) (seq { 0 .. n - 1 })

        let quad = $"%d{bytes.[12]}.%d{bytes.[13]}.%d{bytes.[14]}.%d{bytes.[15]}"

        if zeroUpTo 16 then
            "::"
        elif zeroUpTo 10 && bytes.[10] = 0xFFuy && bytes.[11] = 0xFFuy then
            $"::ffff:%s{quad}"
        elif zeroUpTo 15 && bytes.[15] = 1uy then
            "::1"
        elif zeroUpTo 12 then
            $"::%s{quad}"
        else
            failwith $"DualModeProbe.ntop6: %A{bytes} is not an address the probe meets"

    let private familyOf (libc : Libc) (buffer : byte[]) : int =
        match libc.Flavour with
        | SimulatedUnixFlavour.Linux -> int buffer.[0] ||| (int buffer.[1] <<< 8)
        | SimulatedUnixFlavour.Darwin -> int buffer.[1]

    /// `addr_text`.
    let private addrText (libc : Libc) (ports : Ports) (buffer : byte[]) (n : int) : string =
        let family = familyOf libc buffer
        let port = (uint16 buffer.[2] <<< 8) ||| uint16 buffer.[3]

        if family = 2 && n >= 8 then
            $" family=AF_INET %d{buffer.[4]}.%d{buffer.[5]}.%d{buffer.[6]}.%d{buffer.[7]}:%s{sym ports port}"
        elif family = af6 libc && n >= 24 then
            let flow = BinaryPrimitives.ReadUInt32BigEndian (ReadOnlySpan<byte> (buffer, 4, 4))
            let scope = if n >= 28 then BitConverter.ToUInt32 (buffer, 24) else 0u

            $" family=AF_INET6 [%s{ntop6 buffer.[8..23]}]:%s{sym ports port} flowinfo=%d{flow} scope=%d{scope}"
        else
            $" family=%d{family}"

    /// `dump`.
    let private dump (buffer : byte[]) (n : int) : string =
        let parts =
            [
                for i in 0 .. n - 1 do
                    if i = 2 || i = 3 then "pp" else $"%02x{buffer.[i]}"
            ]

        " [" + String.Join (" ", parts) + "]"

    /// `name_len`'s line.
    let private nameLine
        (libc : Libc)
        (ports : Ports)
        (output : StringBuilder)
        (label : string)
        (call : int -> UserBuffer -> uint32 -> UnixSystem<int, string> -> Result<GetSockNameAnswer, GetSockNameRefusal>)
        (fd : int)
        (cell : int)
        : unit
        =
        let answer, buffer, reported = name libc call fd cell
        output.Append ($"%s{label} cell=%d{cell} -> %s{en answer}") |> ignore

        match answer with
        | Answer.Ok ->
            let shown = min reported cell
            output.Append ($" len=%d{reported}") |> ignore
            output.Append (addrText libc ports buffer shown) |> ignore
            output.Append (dump buffer (min cell 32)) |> ignore
        | Answer.Errno _
        | Answer.Refused _ -> ()

        output.AppendLine () |> ignore

    let private getsockname = UnixSocket.getsockname<int, string>
    let private getpeername = UnixSocket.getpeername<int, string>

    let private line (output : StringBuilder) (label : string) (answer : Answer) : unit =
        output.AppendLine ($"%s{label} -> %s{en answer}") |> ignore

    let private portOf (libc : Libc) (fd : int) : uint16 =
        match name libc getsockname fd 28 with
        | Answer.Ok, buffer, _ -> (uint16 buffer.[2] <<< 8) ||| uint16 buffer.[3]
        | other -> failwith $"DualModeProbe: getsockname(%d{fd}) answered %A{other}"

    let private listener4 (libc : Libc) (ports : Ports) (address : uint32) : int =
        let l = tcp4 libc

        match bind libc l (sinOf libc address 0us) 16 with
        | Answer.Ok -> ()
        | other -> failwith $"DualModeProbe: listener bind answered %A{other}"

        match listen libc l 8 with
        | Answer.Ok -> ()
        | other -> failwith $"DualModeProbe: listen answered %A{other}"

        ports.L <- portOf libc l
        l

    /// `free_port`: bind an IPv4 socket to the wildcard and port 0, read the
    /// port, close it.
    let private freePort (libc : Libc) : uint16 =
        let s = tcp4 libc
        bind libc s (sinOf libc 0u 0us) 16 |> ignore<Answer>
        let port = portOf libc s
        close libc s
        port

    let private acceptAndClose (libc : Libc) (l : int) : unit =
        match accept libc l 0u with
        | Result.Ok (fd, _, _) -> close libc fd
        | Result.Error _ -> ()

    /// Section A: the fresh socket, and connect to a v4-mapped loopback.
    let private sectionA (libc : Libc) (ports : Ports) (output : StringBuilder) : unit =
        output.AppendLine "== A: connect to ::ffff:127.0.0.1 ==" |> ignore
        let nameLine = nameLine libc ports output
        let line = line output
        let s = tcp6V6Only libc 0
        nameLine "A1 fresh V6ONLY=0 getsockname" getsockname s 28
        nameLine "A1 fresh V6ONLY=0 getpeername" getpeername s 28
        nameLine "A1 fresh getsockname, cell 16" getsockname s 16
        nameLine "A1 fresh getsockname, cell 128" getsockname s 128
        nameLine "A1 fresh getsockname, cell 0" getsockname s 0
        close libc s

        let l = listener4 libc ports inaddrLoopback
        let s = tcp6V6Only libc 0

        line
            "A2 V6ONLY=0 unbound, connect ::ffff:127.0.0.1:L"
            (connect libc s (sin6Of libc true inaddrLoopback ports.L) 28)

        nameLine "A2 after: getsockname" getsockname s 28
        nameLine "A2 after: getpeername" getpeername s 28
        nameLine "A2 after: getpeername cell 16" getpeername s 16
        nameLine "A2 after: getpeername cell 24" getpeername s 24
        nameLine "A2 after: getsockname cell 128" getsockname s 128
        let level, optionName = ipv6Only libc
        let answer, value = getInt libc s level optionName

        output.AppendLine ($"A2 after: getsockopt IPV6_V6ONLY -> %s{en answer} value=%d{value}")
        |> ignore

        line "A2 after: setsockopt IPV6_V6ONLY=1" (setInt libc s level optionName 1)
        line "A2 after: setsockopt IPV6_V6ONLY=0" (setInt libc s level optionName 0)
        line "A2 again: connect ::ffff:127.0.0.1:L" (connect libc s (sin6Of libc true inaddrLoopback ports.L) 28)

        match accept libc l 28u with
        | Result.Error answer ->
            output.AppendLine ($"A2 accept on the AF_INET listener -> %s{en answer} len=28")
            |> ignore
        | Result.Ok (srv, _, reported) ->
            output.AppendLine ($"A2 accept on the AF_INET listener -> OK len=%d{reported}")
            |> ignore

            nameLine "A2 accepted: getsockname" getsockname srv 28
            nameLine "A2 accepted: getpeername" getpeername srv 28
            let mine = portOf libc s

            let theirs =
                match name libc getpeername srv 16 with
                | Answer.Ok, buffer, _ -> (uint16 buffer.[2] <<< 8) ||| uint16 buffer.[3]
                | other -> failwith $"DualModeProbe: getpeername answered %A{other}"

            let same = if mine = theirs then "yes" else "no"

            output.AppendLine ($"A2 client's local port = server's peer port -> %s{same}")
            |> ignore

            let wrote =
                match UnixReadWrite.write 0 s (ImmutableArray.Create<byte> (byte 'x')) libc.System with
                | Result.Ok (WriteOutcome.Returns (WriteAnswer.Completed count, system)) ->
                    libc.System <- system
                    $"%d{count}"
                | Result.Error _ -> "REFUSED"
                | Result.Ok other -> failwith $"DualModeProbe: write answered %A{other}"

            let read =
                match UnixReadWrite.read 0 srv UserBuffer.Mapped 1UL libc.System with
                | Result.Ok (ReadOutcome.Answered (ReadAnswer.Completed bytes), system) ->
                    libc.System <- system
                    $"%d{bytes.Length} '%c{char bytes.[0]}'"
                | Result.Error _ -> "REFUSED"
                | Result.Ok other -> failwith $"DualModeProbe: read answered %A{other}"

            output.AppendLine ($"A2 a byte client to server -> wrote %s{wrote} read %s{read}")
            |> ignore

            close libc srv

        close libc s

        let s = tcp6V6Only libc 1
        line "A3 V6ONLY=1, connect ::ffff:127.0.0.1:L" (connect libc s (sin6Of libc true inaddrLoopback ports.L) 28)
        nameLine "A3 after: getsockname" getsockname s 28
        line "A3 V6ONLY=1, again" (connect libc s (sin6Of libc true inaddrLoopback ports.L) 28)
        close libc s

        let s = tcp6V6Only libc 0
        nonBlocking libc s

        line
            "A4 V6ONLY=0 non-blocking, connect ::ffff:127.0.0.1:L"
            (connect libc s (sin6Of libc true inaddrLoopback ports.L) 28)

        line "A4 again" (connect libc s (sin6Of libc true inaddrLoopback ports.L) 28)
        line "A4 and again" (connect libc s (sin6Of libc true inaddrLoopback ports.L) 28)
        nameLine "A4 getsockname" getsockname s 28
        nameLine "A4 getpeername" getpeername s 28
        acceptAndClose libc l
        close libc s

        let closed = freePort libc
        let s = tcp6V6Only libc 0

        line
            "A5 V6ONLY=0, connect ::ffff:127.0.0.1:<closed>"
            (connect libc s (sin6Of libc true inaddrLoopback closed) 28)

        nameLine "A5 after: getsockname" getsockname s 28
        nameLine "A5 after: getpeername" getpeername s 28
        line "A5 again, to L" (connect libc s (sin6Of libc true inaddrLoopback ports.L) 28)
        close libc s

        let s = tcp6V6Only libc 0
        line "A6 V6ONLY=0, connect ::ffff:0.0.0.0:L" (connect libc s (sin6Of libc true 0u ports.L) 28)
        nameLine "A6 after: getsockname" getsockname s 28
        nameLine "A6 after: getpeername" getpeername s 28
        acceptAndClose libc l
        close libc s

        let s = tcp6V6Only libc 0

        line
            "A7 V6ONLY=0, connect ::ffff:127.0.0.2:L (listener 127.0.0.1)"
            (connect libc s (sin6Of libc true 0x7F000002u ports.L) 28)

        nameLine "A7 after: getsockname" getsockname s 28
        close libc s

        let s = tcp6V6Only libc 0
        line "A8 V6ONLY=0, connect ::ffff:127.0.0.1:0" (connect libc s (sin6Of libc true inaddrLoopback 0us) 28)
        nameLine "A8 after: getsockname" getsockname s 28
        close libc s

        let s = tcp6V6Only libc 0
        line "A9 V6ONLY=0, connect ::ffff:224.0.0.1:L" (connect libc s (sin6Of libc true 0xE0000001u ports.L) 28)
        nameLine "A9 after: getsockname" getsockname s 28
        close libc s

        let s = tcp6V6Only libc 0
        line "A10 V6ONLY=0, connect ::ffff:255.255.255.255:L" (connect libc s (sin6Of libc true 0xFFFFFFFFu ports.L) 28)
        close libc s

        let s = tcp6V6Only libc 0
        let a = sin6Of libc true inaddrLoopback ports.L
        BinaryPrimitives.WriteUInt32BigEndian (Span<byte> (a, 4, 4), 0x12345u)
        line "A11 V6ONLY=0, v4-mapped with flowinfo 0x12345" (connect libc s a 28)
        nameLine "A11 getpeername" getpeername s 28
        acceptAndClose libc l
        close libc s
        let s = tcp6V6Only libc 0
        let a = sin6Of libc true inaddrLoopback ports.L
        BitConverter.GetBytes(7u).CopyTo (a, 24)
        line "A12 V6ONLY=0, v4-mapped with scope id 7" (connect libc s a 28)
        nameLine "A12 getpeername" getpeername s 28
        acceptAndClose libc l
        close libc s

        match libc.Flavour with
        | SimulatedUnixFlavour.Darwin ->
            let s = tcp6V6Only libc 0
            let a = sin6Of libc true inaddrLoopback ports.L
            a.[0] <- 0uy
            line "A13 V6ONLY=0, v4-mapped with sin6_len 0" (connect libc s a 28)
            acceptAndClose libc l
            close libc s
        | SimulatedUnixFlavour.Linux -> ()

        for length in
            [
                0
                1
                2
                8
                16
                23
                24
                25
                27
                28
                29
                32
                128
                129
                255
                256
            ] do
            let s = tcp6V6Only libc 0
            let answer = connect libc s (sin6Of libc true inaddrLoopback ports.L) length

            match answer with
            | Answer.Ok -> acceptAndClose libc l
            | Answer.Errno _
            | Answer.Refused _ -> ()

            let bound =
                match name libc getsockname s 32 with
                | Answer.Ok, buffer, _ -> sym ports ((uint16 buffer.[2] <<< 8) ||| uint16 buffer.[3])
                | _ -> "0"

            output.AppendLine ($"A14 AF_INET6 v4-mapped at length %d{length} -> %s{en answer} bound-port=%s{bound}")
            |> ignore

            close libc s

        for length in [ 16 ; 28 ] do
            let s = tcp6V6Only libc 0
            let answer = connect libc s (sinOf libc inaddrLoopback ports.L) length
            line $"A15 AF_INET sockaddr_in at length %d{length}" answer

            match answer with
            | Answer.Ok -> acceptAndClose libc l
            | _ -> ()

            close libc s

        let s = tcp6V6Only libc 0

        line
            "A16 AF_UNSPEC at length 28, idle"
            (connect libc s (withFamily libc 0 (sin6Of libc true inaddrLoopback ports.L)) 28)

        nameLine "A16 after: getsockname" getsockname s 28
        close libc s
        close libc l


    /// Section N: native IPv6 destinations.
    let private sectionN (libc : Libc) (ports : Ports) (output : StringBuilder) : unit =
        output.AppendLine "== N: native IPv6 ==" |> ignore
        let line = line output
        let closed = freePort libc
        let s = tcp6V6Only libc 0
        line "N1 V6ONLY=0, connect [::1]:<closed>" (connect libc s (sin6Of libc false 1u closed) 28)
        close libc s
        let s = tcp6V6Only libc 0
        line "N2 V6ONLY=0, connect [::]:<closed>" (connect libc s (sin6Of libc false 0u closed) 28)
        close libc s
        let l = listener4 libc ports inaddrLoopback
        let s = tcp6V6Only libc 0
        line "N3 V6ONLY=0, connect [::1]:L (only a v4 listener)" (connect libc s (sin6Of libc false 1u ports.L) 28)
        close libc s
        close libc l

    let private bind4 (libc : Libc) (s : int) (address : uint32) (port : uint16) : Answer =
        bind libc s (sinOf libc address port) 16

    let private bind6 (libc : Libc) (s : int) (mapped : bool) (address : uint32) (port : uint16) : Answer =
        bind libc s (sin6Of libc mapped address port) 28

    /// Section B: bind of a dual-mode socket on its own.
    let private sectionB (libc : Libc) (ports : Ports) (output : StringBuilder) : unit =
        output.AppendLine "== B: bind ==" |> ignore
        let nameLine = nameLine libc ports output
        let line = line output

        let addresses =
            [
                "::ffff:127.0.0.1", true, inaddrLoopback
                "::ffff:0.0.0.0", true, 0u
                "::", false, 0u
                "::1", false, 1u
                "::ffff:127.0.0.2", true, 0x7F000002u
                "::ffff:10.255.255.1", true, 0x0AFFFF01u
                "::ffff:224.0.0.1", true, 0xE0000001u
            ]

        for v6only in [ 0 ; 1 ] do
            for text, mapped, address in addresses do
                for port0 in [ false ; true ] do
                    let s = tcp6V6Only libc v6only
                    ports.B <- if port0 then 0us else freePort libc
                    let portText = if port0 then "0" else "B"
                    line $"B1 V6ONLY=%d{v6only} bind [%s{text}]:%s{portText}" (bind6 libc s mapped address ports.B)
                    nameLine "   getsockname" getsockname s 28
                    close libc s

        ports.B <- 0us
        let level, optionName = ipv6Only libc
        let s = tcp6V6Only libc 0
        bind6 libc s true inaddrLoopback 0us |> ignore<Answer>
        line "B2 bound ::ffff:127.0.0.1, set IPV6_V6ONLY=1" (setInt libc s level optionName 1)
        close libc s
        let s = tcp6V6Only libc 0
        bind6 libc s true inaddrLoopback 0us |> ignore<Answer>
        line "B3 bound ::ffff:127.0.0.1, bind again ::ffff:127.0.0.1:0" (bind6 libc s true inaddrLoopback 0us)
        close libc s
        let s = tcp6V6Only libc 0
        line "B4 V6ONLY=0 bind sockaddr_in 127.0.0.1:0 len 16" (bind4 libc s inaddrLoopback 0us)
        close libc s

        for length in [ 0 ; 8 ; 16 ; 23 ; 24 ; 27 ; 28 ; 29 ; 128 ; 129 ; 255 ; 256 ] do
            let s = tcp6V6Only libc 0

            line
                $"B5 AF_INET6 ::ffff:127.0.0.1:0 at length %d{length}"
                (bind libc s (sin6Of libc true inaddrLoopback 0us) length)

            close libc s

        let s = tcp6V6Only libc 0
        line "B6 V6ONLY=0 unbound listen" (listen libc s 1)
        nameLine "B6 getsockname" getsockname s 28
        close libc s

    /// One side of a section C row: a socket's family, `IPV6_V6ONLY` and
    /// address.
    type private Side =
        {
            Name : string
            V6 : bool
            Mapped : bool
            Address : uint32
        }

    /// The sides of section C this kernel binds: the IPv4 ones, and a
    /// dual-mode socket at a specific v4-mapped address. The rest (`::`, `::1`,
    /// `::ffff:0.0.0.0`, and any V6ONLY socket's) are refused.
    let private modelledSides : Side list =
        [
            {
                Name = "4 127.0.0.1"
                V6 = false
                Mapped = false
                Address = inaddrLoopback
            }
            {
                Name = "4 0.0.0.0"
                V6 = false
                Mapped = false
                Address = 0u
            }
            {
                Name = "6d ::ffff:127.0.0.1"
                V6 = true
                Mapped = true
                Address = inaddrLoopback
            }
        ]

    let private make (libc : Libc) (side : Side) (reuse : bool) : int =
        let s = if side.V6 then tcp6V6Only libc 0 else tcp4 libc

        if reuse then
            setInt
                libc
                s
                (SimulatedUnixPlatform.socketOptionLevel libc.Platform)
                (SimulatedUnixPlatform.reuseAddressOption libc.Platform)
                1
            |> ignore<Answer>

        s

    let private bindSide (libc : Libc) (s : int) (side : Side) (port : uint16) : Answer =
        if side.V6 then
            bind6 libc s side.Mapped side.Address port
        else
            bind4 libc s side.Address port

    /// Section C, over `modelledSides` only, and only the rows that do not
    /// listen on an IPv6 socket, which this kernel refuses.
    let private sectionC (libc : Libc) (_ : Ports) (output : StringBuilder) : unit =
        output.AppendLine "== C: bind conflicts ==" |> ignore

        for listenFirst in [ 0 ; 1 ] do
            for reuse in [ 0 ; 1 ; 2 ; 3 ] do
                for first in modelledSides do
                    for second in modelledSides do
                        if (first.V6 || second.V6) && not (listenFirst = 1 && first.V6) then
                            let port = freePort libc
                            let a = make libc first (reuse &&& 1 <> 0)
                            let ea = bindSide libc a first port

                            let ea =
                                match ea with
                                | Answer.Ok when listenFirst = 1 -> listen libc a 1
                                | other -> other

                            let b = make libc second (reuse &&& 2 <> 0)

                            let answer =
                                match ea with
                                | Answer.Ok -> en (bindSide libc b second port)
                                | other -> $"first-failed: %s{en other}"

                            let row =
                                sprintf
                                    "C listen=%d reuse=%d%d [%-20s] then [%-20s] -> %s"
                                    listenFirst
                                    (reuse &&& 1)
                                    ((reuse >>> 1) &&& 1)
                                    first.Name
                                    second.Name
                                    answer

                            output.AppendLine row |> ignore
                            close libc a
                            close libc b

    /// Section E: what a refused connect leaves a dual-mode socket presenting.
    let private sectionE (libc : Libc) (ports : Ports) (output : StringBuilder) : unit =
        output.AppendLine "== E: refusals ==" |> ignore
        let nameLine = nameLine libc ports output
        let line = line output
        let closed = freePort libc
        let s = tcp6V6Only libc 0
        bind6 libc s true inaddrLoopback 0us |> ignore<Answer>
        ports.B <- portOf libc s

        line
            "E1 bound ::ffff:127.0.0.1:B, blocking connect to <closed>"
            (connect libc s (sin6Of libc true inaddrLoopback closed) 28)

        nameLine "E1 after: getsockname" getsockname s 28
        nameLine "E1 after: getpeername" getpeername s 28
        close libc s
        ports.B <- 0us

        let s = tcp6V6Only libc 0
        nonBlocking libc s
        line "E2 unbound non-blocking connect to <closed>" (connect libc s (sin6Of libc true inaddrLoopback closed) 28)
        nameLine "E2 pending: getsockname" getsockname s 28
        nameLine "E2 pending: getpeername" getpeername s 28

        let answer, error =
            getInt
                libc
                s
                (SimulatedUnixPlatform.socketOptionLevel libc.Platform)
                (SimulatedUnixPlatform.socketErrorOption libc.Platform)

        let errorText =
            match answer with
            | Answer.Ok ->
                match UnixError.ofRawErrnoUnder (SimulatedUnixPlatform.rawErrnoNumbering libc.Platform) error with
                | Some error -> $"%A{error}"
                | None when error = 0 -> "OK"
                | None -> $"errno%d{error}"
            | other -> en other

        output.AppendLine ($"E2 SO_ERROR -> %s{errorText}") |> ignore
        nameLine "E2 taken: getsockname" getsockname s 28
        close libc s

        let l = listener4 libc ports inaddrLoopback
        let s = tcp6V6Only libc 0
        line "E3 unbound blocking connect to <closed>" (connect libc s (sin6Of libc true inaddrLoopback closed) 28)
        line "E3 then to L" (connect libc s (sin6Of libc true inaddrLoopback ports.L) 28)
        nameLine "E3 after: getsockname" getsockname s 28
        nameLine "E3 after: getpeername" getpeername s 28
        close libc s
        close libc l

    /// Section F: which of two faults a dual-mode socket's bind reports. The
    /// probe drops to an unprivileged user for F17 to F20; the model's process
    /// is unprivileged throughout.
    let private sectionF (libc : Libc) (_ : Ports) (output : StringBuilder) : unit =
        output.AppendLine "== F: bind fault order ==" |> ignore
        let line = line output
        let nonlocal = sin6Of libc true 0x0AFFFF01u 0us
        let local = sin6Of libc true inaddrLoopback 0us
        let group = sin6Of libc true 0xE0000001u 0us
        let bcast = sin6Of libc true 0xFFFFFFFFu 0us
        let native = sin6Of libc false 1u 0us
        let v4long = sinOf libc inaddrLoopback 0us

        let row (label : string) (v6only : int) (prepare : int -> unit) (blob : byte[]) (length : int) =
            let s = tcp6V6Only libc v6only
            prepare s
            line label (bind libc s blob length)
            close libc s

        let bound (s : int) =
            bind6 libc s true inaddrLoopback 0us |> ignore<Answer>

        let boundWildcard (s : int) =
            bind6 libc s false 0u 0us |> ignore<Answer>

        let nothing (_ : int) = ()

        row "F1 bound, rebind non-local ::ffff:10.255.255.1" 0 bound nonlocal 28
        row "F2 bound, rebind sockaddr_in at 28" 0 bound v4long 28
        row "F3 bound, rebind at length 16" 0 bound local 16
        row "F4 bound, rebind ::ffff:224.0.0.1" 0 bound group 28
        row "F5 V6ONLY=1 bound [::], rebind ::ffff:127.0.0.1" 1 boundWildcard local 28
        row "F6 V6ONLY=1, ::ffff:10.255.255.1" 1 nothing nonlocal 28
        row "F7 V6ONLY=1, ::ffff:255.255.255.255" 1 nothing bcast 28
        row "F8 V6ONLY=1, ::ffff:127.0.0.1 at length 16" 1 nothing local 16
        row "F9 V6ONLY=1, sockaddr_in at 28" 1 nothing v4long 28
        row "F10 V6ONLY=0, ::ffff:255.255.255.255" 0 nothing bcast 28
        row "F11 V6ONLY=0, sockaddr_in at length 16" 0 nothing v4long 16
        row "F12 V6ONLY=0, sockaddr_in at length 23" 0 nothing v4long 23

        let s = tcp6V6Only libc 0
        line "F13 V6ONLY=0, AF_UNSPEC ::ffff:127.0.0.1 at 28" (bind libc s (withFamily libc 0 local) 28)

        nameLine
            libc
            {
                L = 0us
                B = 0us
            }
            output
            "F13 getsockname"
            getsockname
            s
            28

        close libc s

        let port = freePort libc
        let holder = tcp4 libc
        bind4 libc holder inaddrLoopback port |> ignore<Answer>
        let taken = sin6Of libc true inaddrLoopback port
        row "F14 bound, rebind to a port in use" 0 bound taken 28
        row "F15 V6ONLY=1, a v4-mapped port in use" 1 nothing taken 28
        close libc holder
        row "F16 V6ONLY=0, [::1] (native)" 0 nothing native 28

        let low = sin6Of libc true inaddrLoopback 80us
        let lowNonlocal = sin6Of libc true 0x0AFFFF01u 80us
        row "F17 unprivileged, ::ffff:127.0.0.1:80" 0 nothing low 28
        row "F18 unprivileged, ::ffff:10.255.255.1:80" 0 nothing lowNonlocal 28
        row "F19 unprivileged, bound, rebind ::ffff:127.0.0.1:80" 0 bound low 28
        row "F20 unprivileged, V6ONLY=1, ::ffff:127.0.0.1:80" 1 nothing low 28

    /// Section G: connect's ladder on a dual-mode socket, and on a V6ONLY one.
    let private sectionG (libc : Libc) (ports : Ports) (output : StringBuilder) : unit =
        output.AppendLine "== G: connect ladder ==" |> ignore
        let nameLine = nameLine libc ports output
        let line = line output
        let l = listener4 libc ports inaddrLoopback
        let target = sin6Of libc true inaddrLoopback ports.L
        let v4long = sinOf libc inaddrLoopback ports.L
        let unspec = withFamily libc 0 target
        let group = sin6Of libc true 0xE0000001u ports.L

        let s = tcp6V6Only libc 1
        line "G1 V6ONLY=1, mapped at length 16" (connect libc s target 16)
        close libc s
        let s = tcp6V6Only libc 1
        line "G2 V6ONLY=1, sockaddr_in at 28" (connect libc s v4long 28)
        close libc s
        let s = tcp6V6Only libc 1
        line "G3 V6ONLY=1, AF_UNSPEC mapped at 28" (connect libc s unspec 28)
        nameLine "G3 after: getsockname" getsockname s 28
        close libc s
        let s = tcp6V6Only libc 1
        line "G4 V6ONLY=1, ::ffff:224.0.0.1" (connect libc s group 28)
        close libc s
        let s = tcp6V6Only libc 1
        line "G5 V6ONLY=1, mapped port 0" (connect libc s (sin6Of libc true inaddrLoopback 0us) 28)
        nameLine "G5 after: getsockname" getsockname s 28
        close libc s

        let s = tcp6V6Only libc 0
        bind6 libc s true inaddrLoopback 0us |> ignore<Answer>
        ports.B <- portOf libc s
        line "G6 bound ::ffff:127.0.0.1:B, connect to L" (connect libc s target 28)
        nameLine "G6 after: getsockname" getsockname s 28
        nameLine "G6 after: getpeername" getpeername s 28

        match accept libc l 0u with
        | Result.Ok (srv, _, _) ->
            nameLine "G6 accepted: getpeername" getpeername srv 16
            close libc srv
        | Result.Error answer -> failwith $"DualModeProbe: G6's accept answered %A{answer}"

        close libc s
        ports.B <- 0us

        let s = tcp6V6Only libc 0
        line "G7 connect to L" (connect libc s target 28)
        line "G7 established, connect at length 16" (connect libc s target 16)
        line "G7 established, connect AF_UNSPEC" (connect libc s unspec 28)
        acceptAndClose libc l
        close libc s
        let s = tcp6V6Only libc 0
        connect libc s target 28 |> ignore<Answer>
        line "G7 established, connect sockaddr_in at 28" (connect libc s v4long 28)
        acceptAndClose libc l
        close libc s

        // G8 reads SO_SNDBUF, SO_RCVBUF and TCP_MAXSEG, which this kernel does
        // not model as options.
        line "G8 AF_INET client" (Answer.Refused "SO_SNDBUF is not modelled")
        line "G8 dual-mode client" (Answer.Refused "SO_SNDBUF is not modelled")
        close libc l

    /// Section H: connect's family and length screens against a dual-mode
    /// socket's phase.
    let private sectionH (libc : Libc) (ports : Ports) (output : StringBuilder) : unit =
        output.AppendLine "== H: connect screens by phase ==" |> ignore
        let line = line output
        let l = listener4 libc ports inaddrLoopback
        nonBlocking libc l
        let closed = freePort libc

        let raw (family : int) (port : uint16) : byte[] =
            let blob =
                if family = 2 then
                    sinOf libc inaddrLoopback port
                else
                    withFamily libc family (sin6Of libc true inaddrLoopback port)

            withFamily libc family blob

        let rows =
            [
                "AF_INET at 16", 2, 16
                "AF_INET at 28", 2, 28
                "AF_INET6 at 16", af6 libc, 16
                "AF_INET6 at 28", af6 libc, 28
                "family 99 at 28", 99, 28
                "family 99 at 16", 99, 16
            ]

        for text, family, length in rows do
            let s = tcp6V6Only libc 0
            connect libc s (sin6Of libc true inaddrLoopback ports.L) 28 |> ignore<Answer>
            line $"H1 established, %s{text}" (connect libc s (raw family ports.L) length)
            acceptAndClose libc l
            close libc s

            let s = tcp6V6Only libc 0
            nonBlocking libc s
            connect libc s (sin6Of libc true inaddrLoopback closed) 28 |> ignore<Answer>
            line $"H2 refused pending, %s{text}" (connect libc s (raw family closed) length)
            close libc s

            let s = tcp6V6Only libc 0
            nonBlocking libc s
            connect libc s (sin6Of libc true inaddrLoopback ports.L) 28 |> ignore<Answer>
            line $"H3 completed, unreported, %s{text}" (connect libc s (raw family ports.L) length)
            acceptAndClose libc l
            close libc s

            let s = tcp6V6Only libc 0
            line $"H4 idle, %s{text}" (connect libc s (raw family ports.L) length)
            acceptAndClose libc l
            close libc s

        close libc l

    /// The sections this replays, by the letter `main` runs them under.
    let run (platform : SimulatedUnixPlatform) (section : char) : string =
        let libc = Libc platform

        let ports =
            {
                L = 0us
                B = 0us
            }

        let output = StringBuilder ()

        match section with
        | 'A' -> sectionA libc ports output
        | 'N' -> sectionN libc ports output
        | 'B' -> sectionB libc ports output
        | 'C' -> sectionC libc ports output
        | 'E' -> sectionE libc ports output
        | 'F' -> sectionF libc ports output
        | 'G' -> sectionG libc ports output
        | 'H' -> sectionH libc ports output
        | other -> failwith $"DualModeProbe.run: no section %c{other}"

        output.ToString ()
