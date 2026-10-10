namespace WoofWare.PosixKernel.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// The options of the socket `accept(2)` makes for a connection, when the
/// listener's options change while the connection waits in its queue: each
/// option as the listener held it when the connection completed.
///
/// `queued-options.c` (docs/plans/2026-10-08-tcp-shutdown-linger), measured on
/// Linux 6.18.5 aarch64 and Darwin 27.0 and embedded from beside the probe, is
/// replayed through the syscalls on both flavours, printing the probe's lines.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestQueuedOptions =

    let private port : uint16 = 5000us

    let private platformOf (flavour : SimulatedUnixFlavour) : SimulatedUnixPlatform =
        match flavour with
        | SimulatedUnixFlavour.Linux -> SimulatedUnixPlatform.linuxX64
        | SimulatedUnixFlavour.Darwin -> SimulatedUnixPlatform.macOsArm64

    let private flavours : SimulatedUnixFlavour list =
        [ SimulatedUnixFlavour.Linux ; SimulatedUnixFlavour.Darwin ]

    /// The embedded output of `queued-options.c` for `flavour`.
    let private measured (flavour : SimulatedUnixFlavour) : string list =
        let flavourName =
            match flavour with
            | SimulatedUnixFlavour.Linux -> "linux"
            | SimulatedUnixFlavour.Darwin -> "darwin"

        let resource = $"WoofWare.PosixKernel.Test.queuedOptions.%s{flavourName}.txt"

        use stream =
            match Assembly.GetExecutingAssembly().GetManifestResourceStream resource with
            | null -> failwith $"embedded resource %s{resource} not found"
            | stream -> stream

        use reader = new StreamReader (stream)

        reader.ReadToEnd().Split '\n'
        |> Array.map (fun line -> line.TrimEnd '\r')
        |> Array.filter (fun line -> line <> "")
        |> Array.toList

    /// A booted system of `platform`, with tasks 1 to 4.
    let private systemOn (platform : SimulatedUnixPlatform) : UnixSystem<int, string> =
        UnixSystem.initial<int, string> platform
        |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)
        |> fun system -> ([ 1..4 ], system) ||> List.foldBack Tasks.ensure

    /// An option the probe sets, by the name it prints: its level and number
    /// under `platform`, and whether its value is a `struct linger` rather than
    /// an `int`.
    let private optionNamed (platform : SimulatedUnixPlatform) (name : string) : int * int * bool =
        match name with
        | "SO_REUSEADDR" ->
            SimulatedUnixPlatform.socketOptionLevel platform, SimulatedUnixPlatform.reuseAddressOption platform, false
        | "TCP_NODELAY" ->
            SimulatedUnixPlatform.tcpOptionLevel platform, SimulatedUnixPlatform.noDelayOption platform, false
        | "SO_LINGER" ->
            SimulatedUnixPlatform.socketOptionLevel platform, SimulatedUnixPlatform.lingerOption platform, true
        | "SO_LINGER_SEC" ->
            match SimulatedUnixPlatform.lingerSecondsOption platform with
            | Some option -> SimulatedUnixPlatform.socketOptionLevel platform, option, true
            | None -> failwith $"%O{platform} has no SO_LINGER_SEC"
        | other -> failwith $"the probe set %s{other}, which this test does not know"

    /// A value as the probe prints it, `7` or `{1,7}`, as the bytes a caller's
    /// buffer holds.
    let private valueOf (text : string) : ImmutableArray<byte> =
        if text.StartsWith ("{", StringComparison.Ordinal) then
            match text.Trim('{', '}').Split ',' with
            | [| onOff ; linger |] -> OptionValue.ofLinger (int onOff) (int linger)
            | _ -> failwith $"the probe printed %s{text}, which is not a struct linger"
        else
            OptionValue.ofInt (int text)

    /// The probe's `set`, which must succeed.
    let private set
        (fd : int)
        (level : int, option : int)
        (value : ImmutableArray<byte>)
        (system : UnixSystem<int, string>)
        : UnixSystem<int, string>
        =
        let supplied =
            match UnixSocket.admitSetSockOpt fd level option UserBuffer.Mapped (uint32 value.Length) system with
            | Ok (SetSockOptAdmission.Transfer count) -> Some (ImmutableArray.Create (value, 0, count))
            | other -> failwith $"setsockopt(%d{level}, %d{option}) on fd %d{fd} was admitted as %A{other}"

        match UnixSocket.setsockopt fd level option UserBuffer.Mapped (uint32 value.Length) supplied system with
        | Ok (SetSockOptAnswer.Set, system) -> system
        | other -> failwith $"setsockopt(%d{level}, %d{option}) of %A{value} on fd %d{fd}: %A{other}"

    /// The probe's `get`, printed as it prints it.
    let private get (fd : int) (level : int, option : int, linger : bool) (system : UnixSystem<int, string>) : string =
        let size = if linger then 8u else 4u

        let read =
            match UnixSocket.admitGetSockOpt fd level option UserBuffer.Mapped UserBuffer.Mapped system with
            | Ok GetSockOptAdmission.ReadLength -> Some size
            | other -> failwith $"getsockopt(%d{level}, %d{option}) on fd %d{fd} was admitted as %A{other}"

        match UnixSocket.getsockopt fd level option UserBuffer.Mapped UserBuffer.Mapped read system with
        | Ok (GetSockOptAnswer.Reported bytes, _) when bytes.Length = int size ->
            let field (offset : int) =
                BitConverter.ToInt32 (bytes.AsSpan().Slice (offset, 4))

            if linger then
                $"{{%d{field 0},%d{field 4}}}"
            else
                $"%d{field 0}"
        | other -> failwith $"getsockopt(%d{level}, %d{option}) on fd %d{fd}: %A{other}"

    /// A blocking socket connected to the listener at `port` by a blocking
    /// connect, as the probe's `connect_to` makes one.
    let private connectTo (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        let fd, system = KeventWorld.stream false system

        match KeventWorld.connect fd port system with
        | ConnectOutcome.Completed, system -> fd, system
        | other, _ -> failwith $"connect: %A{other}"

    // ------------------------------------------------------------------
    // Section O
    // ------------------------------------------------------------------

    /// Section O's row for `name` and the listener's three values, made on the
    /// kernel and printed as the probe prints it.
    let private kernelO (flavour : SimulatedUnixFlavour) (name : string) (values : string list) : string =
        let system = systemOn (platformOf flavour)
        let level, option, linger = optionNamed system.Machine.UnixPlatform name

        let v0, v1, v2 =
            match values with
            | [ v0 ; v1 ; v2 ] -> v0, v1, v2
            | _ -> failwith $"the probe printed %A{values}, not three values"

        let listener, system = KeventWorld.listenerAt port system
        let system = set listener (level, option) (valueOf v0) system
        let _, system = connectTo system
        let system = set listener (level, option) (valueOf v1) system
        let _, system = connectTo system
        let system = set listener (level, option) (valueOf v2) system
        let first, system = KeventWorld.accept listener system
        let second, system = KeventWorld.accept listener system
        UnixSystem.checkInvariants system |> shouldEqual []
        let read (fd : int) = get fd (level, option, linger) system

        $"O\t%s{name}\t%s{v0} %s{v1} %s{v2}\tfirst=%s{read first} second=%s{read second} listener=%s{read listener}"

    /// Every O line of `flavour`'s run.
    let private measuredO (flavour : SimulatedUnixFlavour) : string list =
        measured flavour
        |> List.filter (fun line -> line.StartsWith ("O\t", StringComparison.Ordinal))

    [<Test>]
    let ``each flavour's run holds the rows this test replays`` () : unit =
        for flavour in flavours do
            let o = measuredO flavour
            let options = o |> List.map (fun line -> line.Split('\t').[1]) |> List.distinct

            match flavour with
            | SimulatedUnixFlavour.Linux ->
                options |> shouldEqual [ "SO_REUSEADDR" ; "TCP_NODELAY" ; "SO_LINGER" ]

                measured flavour
                |> List.filter (fun line -> line.StartsWith ("D\t", StringComparison.Ordinal))
                |> List.length
                |> shouldEqual 2
            | SimulatedUnixFlavour.Darwin ->
                options
                |> shouldEqual [ "SO_REUSEADDR" ; "TCP_NODELAY" ; "SO_LINGER" ; "SO_LINGER_SEC" ]

            o |> List.length |> shouldEqual (2 * List.length options)

    [<Test>]
    let ``an accepted socket has its listener's options as they were when its connection completed, as each flavour was measured to``
        ()
        : unit
        =
        for flavour in flavours do
            let measured = measuredO flavour

            measured
            |> List.map (fun line ->
                match line.Split '\t' with
                | [| _ ; name ; values ; _ |] -> kernelO flavour name (values.Split ' ' |> Array.toList)
                | _ -> failwith $"the probe printed %s{line}, which this test cannot read"
            )
            |> shouldEqual measured

    // ------------------------------------------------------------------
    // Section D
    // ------------------------------------------------------------------

    /// Section D's row on the kernel: the listener's `SO_LINGER` `completed`
    /// as the client connects and `accepted` as an accept with a negative
    /// length takes the connection; the accept's answer and what the client
    /// then reads, printed as the probe prints them; or the refusal, with the
    /// listener's socket.
    let private kernelD (completed : string) (accepted : string) : Result<string, AcceptRefusal * SocketId> =
        let system = systemOn SimulatedUnixPlatform.linuxX64
        let level, option, _ = optionNamed system.Machine.UnixPlatform "SO_LINGER"
        let listener, system = KeventWorld.listenerAt port system
        let system = set listener (level, option) (valueOf completed) system
        let client, system = connectTo system
        let system = set listener (level, option) (valueOf accepted) system

        match UnixConnection.accept 1 listener UserBuffer.Mapped UInt32.MaxValue system with
        | Error refusal ->
            match FileDescriptorRegistry.tryFindTarget listener (UnixSystemState.fileDescriptors system) with
            | Some (OpenFileTarget.Socket socket) -> Error (refusal, socket)
            | other -> failwith $"the listener's fd %d{listener} names %A{other}"
        | Ok (AcceptOutcome.DroppedConnection UnixError.EINVAL, system) ->
            UnixSystem.checkInvariants system |> shouldEqual []
            let _, system = UnixDescriptor.setNonBlocking client true system

            let read =
                match ReadOutcomes.read client UserBuffer.Mapped 16UL system with
                | Ok (ReadAnswer.Completed bytes, _) -> $"%d{bytes.Length}"
                | Ok (ReadAnswer.Failed UnixError.ECONNRESET, _) -> "-1 ECONNRESET"
                | other -> failwith $"the client's read: %A{other}"

            Ok $"D\tcompleted %s{completed} accepted %s{accepted}\taccept=-1 EINVAL client-read=%s{read}"
        | Ok (other, _) -> failwith $"an accept with a negative length answered %A{other}"

    /// The two D lines, with the two values each names.
    let private measuredD () : (string * string * string) list =
        measured SimulatedUnixFlavour.Linux
        |> List.filter (fun line -> line.StartsWith ("D\t", StringComparison.Ordinal))
        |> List.map (fun line ->
            match line.Split('\t').[1].Split ' ' with
            | [| "completed" ; completed ; "accepted" ; accepted |] -> line, completed, accepted
            | _ -> failwith $"the probe printed %s{line}, which this test cannot read"
        )

    /// Linux closes the socket an accept made and could not return with the
    /// linger its connection completed with, not the listener's at the accept:
    /// a listener lingering for no time only once the connection has queued
    /// drops it with a FIN.
    [<Test>]
    let ``Linux: a dropped connection closes with the linger it completed with`` () : unit =
        match measuredD () with
        | [ (line, completed, accepted) ; _ ] ->
            completed |> shouldEqual "{0,0}"
            kernelD completed accepted |> Result.mapError fst |> shouldEqual (Ok line)
        | other -> failwith $"expected two D lines, got %A{other}"

    /// The other row: a connection that completed under {1, 0} is reset as
    /// it is dropped, which this kernel refuses, as it refuses every abortive
    /// close of a connection something still refers to.
    [<Test>]
    let ``Linux: a dropped connection that completed lingering for no time is refused`` () : unit =
        match measuredD () with
        | [ _ ; (line, completed, accepted) ] ->
            completed |> shouldEqual "{1,0}"
            line |> shouldContainText "client-read=-1 ECONNRESET"

            match kernelD completed accepted with
            | Error (AcceptRefusal.AbortiveDrop (listener, _), socket) -> listener |> shouldEqual socket
            | other -> failwith $"expected the drop to be refused, got %A{other}"
        | other -> failwith $"expected two D lines, got %A{other}"
