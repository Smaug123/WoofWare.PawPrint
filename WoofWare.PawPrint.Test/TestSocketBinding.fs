namespace WoofWare.PawPrint.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// The rules behind `SystemNative_Bind`, at the level the guests cannot reach:
/// the fault ordering as an object, the conflict relation across the whole
/// measured matrix, and the port allocator's own guarantees.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSocketBinding =

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    let private platforms =
        [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ]

    let private endpoint (address : uint32) (port : uint16) : InternetEndpoint = InternetEndpoint.ofParts address port

    let private binding (address : uint32) (port : uint16) : SocketBinding =
        {
            Endpoint = endpoint address port
            LockedAddress = None
        }

    let private loopback = InternetEndpoint.LoopbackAddress
    let private wildcard = InternetEndpoint.WildcardAddress

    /// Every fault, so a truncated order cannot pass unnoticed.
    let private allFaults =
        [
            BindFault.Length
            BindFault.Family
            BindFault.AddressNotLocal
            BindFault.PrivilegedPort
            BindFault.AlreadyBound
            BindFault.AddressInUse
        ]

    [<Test>]
    let ``each flavour orders every fault exactly once`` () : unit =
        for platform in platforms do
            let order = SimulatedUnixPlatform.bindFaultOrder platform
            order |> List.distinct |> shouldEqual order
            order |> List.sort |> shouldEqual (List.sort allFaults)

    /// The two orders differ, and specifically in the two places measured. Were
    /// they equal, every per-flavour ordering row in `SocketBindLinux.cs` and its
    /// Darwin sibling would be asserting the same thing twice.
    [<Test>]
    let ``the flavours disagree about which fault comes first`` () : unit =
        let linux = SimulatedUnixPlatform.bindFaultOrder SimulatedUnixPlatform.linuxX64
        let darwin = SimulatedUnixPlatform.bindFaultOrder SimulatedUnixPlatform.macOsArm64
        linux |> shouldNotEqual darwin

        let firstOf (order : BindFault list) (a : BindFault) (b : BindFault) : BindFault =
            order |> List.find (fun fault -> fault = a || fault = b)

        // Linux checks the declared length before it reads the family; Darwin
        // reads the family first.
        firstOf linux BindFault.Length BindFault.Family |> shouldEqual BindFault.Length
        firstOf darwin BindFault.Length BindFault.Family |> shouldEqual BindFault.Family

        // Linux validates the address before it notices the socket is bound;
        // Darwin rejects the bound socket first.
        firstOf linux BindFault.AddressNotLocal BindFault.AlreadyBound
        |> shouldEqual BindFault.AddressNotLocal

        firstOf darwin BindFault.AddressNotLocal BindFault.AlreadyBound
        |> shouldEqual BindFault.AlreadyBound

    [<Test>]
    let ``firstBindFault reports nothing when nothing is wrong`` () : unit =
        for platform in platforms do
            SimulatedUnixPlatform.firstBindFault platform Set.empty |> shouldEqual None

    /// For any set of faults, the one reported is the earliest in that
    /// platform's order — stated against the order rather than against a table,
    /// so the two cannot drift apart.
    [<Test>]
    let ``firstBindFault reports the earliest fault present`` () : unit =
        let property (indices : int list) : bool =
            let faults =
                indices
                |> List.map (fun i -> allFaults.[((i % allFaults.Length) + allFaults.Length) % allFaults.Length])
                |> Set.ofList

            platforms
            |> List.forall (fun platform ->
                let order = SimulatedUnixPlatform.bindFaultOrder platform

                match SimulatedUnixPlatform.firstBindFault platform faults with
                | None -> Set.isEmpty faults
                | Some reported ->
                    Set.contains reported faults
                    && order
                       |> List.takeWhile (fun fault -> fault <> reported)
                       |> List.forall (fun earlier -> not (Set.contains earlier faults))
            )

        Check.One (propertyConfig, property)

    let private idle = SocketPhase.Idle

    let private listeningPhase =
        SocketPhase.Listening
            {
                Backlog = 8
                Queue = []
            }

    let private establishedPhase = SocketPhase.Established (ConnectionId 0L)

    /// The measured conflict matrix, row by row: `(first address, first phase,
    /// first reuse, second address, second reuse)` against what each flavour
    /// answered. Ports are equal throughout, since an unequal port never
    /// conflicts.
    [<Test>]
    let ``the conflict relation matches the measured matrix`` () : unit =
        let rows =
            [
                // exact address, neither listening, both reuse: Linux permits,
                // Darwin refuses the duplicate.
                "exact, unlistened, both reuse", loopback, idle, true, loopback, true, false, true
                // ...and once the first listens, both refuse.
                "exact, listening, both reuse", loopback, listeningPhase, true, loopback, true, true, true
                // Without the flag on both sides the two agree, whatever the
                // addresses: this is every UDP bind, and every PT_UNSPECIFIED one.
                "exact, unlistened, no reuse", loopback, idle, false, loopback, false, true, true
                "exact, unlistened, reuse on second only", loopback, idle, false, loopback, true, true, true
                // wildcard against a specific address, both reuse: Linux refuses
                // once the wildcard listens, Darwin permits regardless.
                "wildcard listening, then specific", wildcard, listeningPhase, true, loopback, true, true, false
                "wildcard unlistened, then specific", wildcard, idle, true, loopback, true, false, false
                "specific listening, then wildcard", loopback, listeningPhase, true, wildcard, true, true, false
                // No overlap at all, so nothing to refuse.
                "different specific addresses", loopback, listeningPhase, true, 0x7F000002u, true, false, false
                // An established socket's pcb is keyed by its full peer tuple:
                // a reuse-carrying rebind over it passes on both flavours (a
                // replacement listener over accepted children — measured), and
                // a flagless one is refused on both.
                "exact, established, both reuse", loopback, establishedPhase, true, loopback, true, false, false
                "exact, established, no reuse on second", loopback, establishedPhase, true, loopback, false, true, true
            ]

        for name, firstAddress, firstPhase, firstReuse, secondAddress, secondReuse, linuxConflicts, darwinConflicts in
            rows do
            let existing = binding firstAddress 40000us
            let candidate = binding secondAddress 40000us

            SimulatedUnixPlatform.bindConflict
                SimulatedUnixPlatform.linuxX64
                existing
                firstReuse
                firstPhase
                candidate
                secondReuse
            |> fun actual ->
                if actual <> linuxConflicts then
                    failwith $"linux, %s{name}: expected conflict=%b{linuxConflicts} but the model said %b{actual}"

            SimulatedUnixPlatform.bindConflict
                SimulatedUnixPlatform.macOsArm64
                existing
                firstReuse
                firstPhase
                candidate
                secondReuse
            |> fun actual ->
                if actual <> darwinConflicts then
                    failwith $"darwin, %s{name}: expected conflict=%b{darwinConflicts} but the model said %b{actual}"

    /// A different port never conflicts, whatever else is true. Stated as a
    /// property because it is the clause that keeps two servers on one machine
    /// from colliding.
    [<Test>]
    let ``different ports never conflict`` () : unit =
        let property (firstPort : uint16) (secondPort : uint16) (listening : bool) (reuse : bool) : bool =
            if firstPort = secondPort then
                true
            else

            let existing = binding loopback firstPort
            let candidate = binding loopback secondPort

            let phase = if listening then listeningPhase else idle

            platforms
            |> List.forall (fun platform ->
                not (SimulatedUnixPlatform.bindConflict platform existing reuse phase candidate reuse)
            )

        Check.One (propertyConfig, property)

    /// Which addresses each flavour will bind. `127.9.9.9` is the row that
    /// separates them: inside loopback's prefix, and not the address Darwin
    /// assigned.
    [<Test>]
    let ``the flavours read one address list differently`` () : unit =
        let addresses = EmulatedKernel.defaultLocalAddresses
        let routes = EmulatedKernel.defaultLocalRoutes

        let bindable (platform : SimulatedUnixPlatform) (address : uint32) : bool =
            SimulatedUnixPlatform.isBindableAddress platform addresses routes address

        for platform in platforms do
            // The wildcard always binds, and is not in the list at all.
            bindable platform wildcard |> shouldEqual true
            bindable platform loopback |> shouldEqual true
            // 8.8.8.8 is nobody's.
            bindable platform 0x08080808u |> shouldEqual false

        // 127.9.9.9.
        bindable SimulatedUnixPlatform.linuxX64 0x7F090909u |> shouldEqual true
        bindable SimulatedUnixPlatform.macOsArm64 0x7F090909u |> shouldEqual false

    /// A multicast or broadcast address is one PawPrint refuses rather than
    /// answers, and the refusal is placed at the fault position where the
    /// address itself is judged. The classifier that puts it there must not be
    /// silenced by a host that lists such an address as one this machine holds,
    /// or covers it with a local route — otherwise the bind is *recorded*, and a
    /// guest holds a multicast binding nothing downstream can honour.
    [<Test>]
    let ``a multicast address faults however the host configures the machine`` () : unit =
        let multicast = 0xE0000001u // 224.0.0.1
        let broadcast = System.UInt32.MaxValue // 255.255.255.255

        let hostile =
            [
                "listed among this machine's addresses", [ multicast ; broadcast ], []
                "covered by a local route", [], [ Ipv4Prefix.create 0xE0000000u 4 ; Ipv4Prefix.create 0u 0 ]
                "both", [ multicast ; broadcast ], [ Ipv4Prefix.create 0u 0 ]
            ]

        for name, addresses, routes in hostile do
            for platform in platforms do
                for address in [ multicast ; broadcast ] do
                    // The classifier this one wraps is *not* asserted false here:
                    // it answers a different question, and on Linux a local route
                    // covering the address makes it genuinely bindable.
                    SimulatedUnixPlatform.bindAddressFaults platform addresses routes address
                    |> fun actual ->
                        if not actual then
                            failwith
                                $"%s{name}: %s{InternetEndpoint.toString (endpoint address 0us)} stopped faulting, so the refusal is silenced"

        // ...while an address the host really does hold still binds, so the
        // above is not passing by refusing everything.
        for platform in platforms do
            SimulatedUnixPlatform.bindAddressFaults platform [ loopback ] [] loopback
            |> shouldEqual false

            SimulatedUnixPlatform.bindAddressFaults platform [ loopback ] [] 0x08080808u
            |> shouldEqual true

    /// Whether `listen(2)` re-runs the port admission rule for a socket that is
    /// already bound. Measured on both kernels; the Linux answer is what makes
    /// two SO_REUSEADDR sockets stop coexisting when one listens, and the Darwin
    /// answer is why re-checking there would *invent* an EADDRINUSE rather than
    /// tighten one — its bind rule keys on the candidate's flag alone, so asking
    /// it again reverses the roles it was answered under. The guests hold the
    /// end-to-end rows: `SocketBindLinux.cs` and `SocketBindDarwin.cs`, both
    /// section 18.
    [<Test>]
    let ``only Linux re-screens an already-bound listen`` () : unit =
        SimulatedUnixPlatform.listenRescreensBinding SimulatedUnixPlatform.linuxX64
        |> shouldEqual true

        SimulatedUnixPlatform.listenRescreensBinding SimulatedUnixPlatform.macOsArm64
        |> shouldEqual false

        // The pair the flavours answer oppositely, as the relation the listen
        // re-check would consult: a wildcard admitted alongside a specific
        // address, asked again with the roles swapped. Darwin says "conflict",
        // which is precisely why it must not be asked.
        let wildcardBinding = binding wildcard 40000us
        let specificBinding = binding loopback 40000us

        SimulatedUnixPlatform.bindConflict
            SimulatedUnixPlatform.macOsArm64
            specificBinding
            true
            idle
            wildcardBinding
            false
        |> shouldEqual true

    [<Test>]
    let ``an ephemeral port is in range, free, and a function of the kernel alone`` () : unit =
        let property (NonNegativeInt seed : NonNegativeInt) : bool =
            let rng = System.Random seed
            let low = uint16 (1024 + rng.Next 1000)
            let high = low + uint16 (rng.Next 50)

            let kernel =
                EmulatedKernel.initial |> EmulatedKernel.withEphemeralPortRange (low, high)

            // An arbitrary subset of the range is already taken.
            let taken = [ low..high ] |> List.filter (fun _ -> rng.Next 3 = 0) |> Set.ofList

            let acceptable (port : uint16) : bool = not (Set.contains port taken)

            match EmulatedKernel.allocateEphemeralPort acceptable kernel with
            | Some (port, kernel') ->
                // In range, free, and the cursor moved on.
                port >= low
                && port <= high
                && acceptable port
                && kernel'.EphemeralPortRange = (low, high)
                // Deterministic: the same kernel answers the same way.
                && (
                    match EmulatedKernel.allocateEphemeralPort acceptable kernel with
                    | Some (again, _) -> again = port
                    | None -> false
                )
            | None ->
                // Only when the range really is exhausted.
                [ low..high ] |> List.forall (fun port -> not (acceptable port))

        Check.One (propertyConfig, property)

    /// Successive allocations do not repeat, which is what stops a bind of port
    /// 0 from handing two sockets one port before either has been recorded.
    [<Test>]
    let ``successive allocations advance`` () : unit =
        let kernel =
            EmulatedKernel.initial
            |> EmulatedKernel.withEphemeralPortRange (40000us, 40004us)

        let rec take (n : int) (kernel : EmulatedKernel) (acc : uint16 list) : uint16 list =
            if n = 0 then
                List.rev acc
            else

            match EmulatedKernel.allocateEphemeralPort (fun _ -> true) kernel with
            | Some (port, kernel) -> take (n - 1) kernel (port :: acc)
            | None -> failwith "the range is not exhausted here"

        take 5 kernel []
        |> shouldEqual [ 40000us ; 40001us ; 40002us ; 40003us ; 40004us ]

    /// ...and wrap rather than running off the end.
    [<Test>]
    let ``allocation wraps at the top of the range`` () : unit =
        let kernel =
            EmulatedKernel.initial
            |> EmulatedKernel.withEphemeralPortRange (40000us, 40001us)

        let _, kernel = (EmulatedKernel.allocateEphemeralPort (fun _ -> true) kernel).Value
        let _, kernel = (EmulatedKernel.allocateEphemeralPort (fun _ -> true) kernel).Value
        let port, _ = (EmulatedKernel.allocateEphemeralPort (fun _ -> true) kernel).Value
        port |> shouldEqual 40000us

    [<Test>]
    let ``an exhausted range is refused rather than looping`` () : unit =
        let kernel =
            EmulatedKernel.initial
            |> EmulatedKernel.withEphemeralPortRange (40000us, 40010us)

        EmulatedKernel.allocateEphemeralPort (fun _ -> false) kernel |> shouldEqual None

    [<Test>]
    let ``an empty or zero-based ephemeral range is refused`` () : unit =
        let shouldFail (low : uint16) (high : uint16) (substring : string) : unit =
            let exn =
                Assert.Throws<System.Exception> (fun () ->
                    EmulatedKernel.withEphemeralPortRange (low, high) EmulatedKernel.initial
                    |> ignore<EmulatedKernel>
                )

            exn.Message |> shouldContainText substring

        shouldFail 0us 100us "port 0 is how a guest"
        shouldFail 100us 99us "is empty"
