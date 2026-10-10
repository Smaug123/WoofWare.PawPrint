namespace WoofWare.PosixKernel.Test

open System
open NUnit.Framework
open WoofWare.PosixKernel

/// Dual-mode IPv6 TCP sockets against every row `docs/probes/dual-mode/dual-mode.c`
/// measured on Linux 6.18.5 aarch64 and Darwin 27.0.0 arm64, whose outputs are
/// embedded here: `DualModeProbe` makes the probe's calls on the model and
/// prints its lines, and each section must print what the kernel printed.
///
/// Every line must be printed as measured, except those this kernel refuses or
/// whose setup it refuses, which are listed by label, with why, and must be
/// printed otherwise -- so a row that comes to be modelled fails here until it
/// leaves the list.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestDualModeMeasured =

    /// Collapse each run of spaces to one, as the probe pads its labels.
    let private normalise (line : string) : string =
        String.Join (" ", line.Split (' ', StringSplitOptions.RemoveEmptyEntries))

    /// The measured lines of `section`, from its `== X:` header to the next.
    let private measured (flavour : SimulatedUnixFlavour) (section : char) : string list =
        let name =
            match flavour with
            | SimulatedUnixFlavour.Linux -> "dual-mode.linux.txt"
            | SimulatedUnixFlavour.Darwin -> "dual-mode.darwin.txt"

        SocketLadder.resource name
        |> Array.map (fun line -> line.TrimEnd '\r')
        |> Array.skipWhile (fun line -> not (line.StartsWith ($"== %c{section}:", StringComparison.Ordinal)))
        |> Array.toList
        |> function
            | [] -> failwith $"no section %c{section} in %s{name}"
            | header :: rest ->
                header
                :: (rest
                    |> List.takeWhile (fun line -> not (line.StartsWith ("== ", StringComparison.Ordinal))))
        |> List.map normalise

    /// A line this kernel does not print as measured.
    [<RequireQualifiedAccess>]
    type private Unmodelled =
        /// The line labelled so: a call this kernel refuses, or answers
        /// otherwise because of an earlier refusal in the same row.
        | Row of label : string * why : string
        /// A line labelled so straight after an unmodelled one: a read of the
        /// state a refused call left otherwise than the kernel did. It may
        /// happen to print as measured.
        | Following of label : string * why : string

    let private darwinShort : string =
        "Darwin lengths below 24 are refused (SockaddrCopyRefusal.DarwinShortInet6Sockaddr)"

    let private ipv6Address : string =
        "an IPv6 address other than a specific v4-mapped one is refused (BindRefusal.UnmodelledIpv6Address)"

    let private bindRows (v6only : int) (address : string) (why : string) : Unmodelled list =
        [
            Unmodelled.Row ($"B1 V6ONLY=%d{v6only} bind [%s{address}]:B", why)
            Unmodelled.Following ("getsockname", "the socket stays unbound here")
            Unmodelled.Row ($"B1 V6ONLY=%d{v6only} bind [%s{address}]:0", why)
        ]

    let private notModelled (flavour : SimulatedUnixFlavour) : Unmodelled list =
        let both =
            [
                Unmodelled.Row (
                    "N1 V6ONLY=0, connect [::1]:<closed>",
                    "no IPv6 transport (ConnectRefusal.Ipv6Destination)"
                )
                Unmodelled.Row (
                    "N2 V6ONLY=0, connect [::]:<closed>",
                    "no IPv6 transport (ConnectRefusal.Ipv6Destination)"
                )
                Unmodelled.Row (
                    "N3 V6ONLY=0, connect [::1]:L (only a v4 listener)",
                    "no IPv6 transport (ConnectRefusal.Ipv6Destination)"
                )
                yield! bindRows 0 "::ffff:0.0.0.0" ipv6Address
                yield! bindRows 0 "::" ipv6Address
                yield! bindRows 0 "::1" ipv6Address
                yield! bindRows 1 "::" ipv6Address
                yield! bindRows 1 "::1" ipv6Address
                Unmodelled.Row ("B6 V6ONLY=0 unbound listen", "no IPv6 listener (ListenRefusal.Ipv6Listener)")
                Unmodelled.Following ("B6 getsockname", "the listen bound nothing here")
                Unmodelled.Row ("F16 V6ONLY=0, [::1] (native)", ipv6Address)
                Unmodelled.Row ("G8 AF_INET client", "SO_SNDBUF, SO_RCVBUF and TCP_MAXSEG are not modelled options")
                Unmodelled.Row ("G8 dual-mode client", "SO_SNDBUF, SO_RCVBUF and TCP_MAXSEG are not modelled options")
            ]

        let abortive =
            "the close under SO_LINGER {1, 0} is refused while the client still holds the connection (DescriptionReleaseRefusal.AbortiveClose), so no reset reaches the client here"

        both
        @ [
            for who in [ "dual-mode" ; "AF_INET" ] do
                Unmodelled.Row ($"R3 %s{who}, abortive close: getpeername", abortive)
                Unmodelled.Row ($"R3 %s{who}, abortive close: SO_ERROR", abortive)
        ]
        @ match flavour with
          | SimulatedUnixFlavour.Linux ->
              [
                  Unmodelled.Row (
                      "A7 V6ONLY=0, connect ::ffff:127.0.0.2:L (listener 127.0.0.1)",
                      "a source address for a destination other than 127.0.0.1 is unmeasured (ConnectRefusal.SourceForNonLoopbackDestination)"
                  )
                  Unmodelled.Following ("A7 after: getsockname", "the refused connect bound nothing here")
                  yield!
                      bindRows
                          0
                          "::ffff:224.0.0.1"
                          "Linux binds a v4-mapped group address, which this kernel cannot honour (BindRefusal.UnmodelledMulticast)"
                  Unmodelled.Row (
                      "F10 V6ONLY=0, ::ffff:255.255.255.255",
                      "Linux binds the v4-mapped broadcast address (BindRefusal.UnmodelledMulticast)"
                  )
                  Unmodelled.Row (
                      "G7 established, connect AF_UNSPEC",
                      "AF_UNSPEC on a connected Linux stream socket is unmeasured (ConnectRefusal.LinuxUnspecOnPhase)"
                  )
              ]
          | SimulatedUnixFlavour.Darwin ->
              [
                  Unmodelled.Row (
                      "A7 V6ONLY=0, connect ::ffff:127.0.0.2:L (listener 127.0.0.1)",
                      "127.0.0.2 is not an address Darwin holds (ConnectRefusal.DestinationNotLocal)"
                  )
                  Unmodelled.Following ("A7 after: getsockname", "the refused connect bound nothing here")
                  for length in [ 0 ; 1 ; 2 ; 8 ; 16 ; 23 ] do
                      Unmodelled.Row ($"A14 AF_INET6 v4-mapped at length %d{length}", darwinShort)
                  Unmodelled.Row ("A15 AF_INET sockaddr_in at length 16", darwinShort)
                  for length in [ 0 ; 8 ; 16 ; 23 ] do
                      Unmodelled.Row ($"B5 AF_INET6 ::ffff:127.0.0.1:0 at length %d{length}", darwinShort)
                  Unmodelled.Row ("B4 V6ONLY=0 bind sockaddr_in 127.0.0.1:0 len 16", darwinShort)
                  Unmodelled.Row ("F3 bound, rebind at length 16", darwinShort)
                  Unmodelled.Row (
                      "F5 V6ONLY=1 bound [::], rebind ::ffff:127.0.0.1",
                      "its setup's bind to [::] is refused, so the socket is not bound here"
                  )
                  Unmodelled.Row ("F8 V6ONLY=1, ::ffff:127.0.0.1 at length 16", darwinShort)
                  Unmodelled.Row ("F11 V6ONLY=0, sockaddr_in at length 16", darwinShort)
                  Unmodelled.Row ("F12 V6ONLY=0, sockaddr_in at length 23", darwinShort)
                  Unmodelled.Row ("G1 V6ONLY=1, mapped at length 16", darwinShort)
                  Unmodelled.Row ("G7 established, connect at length 16", darwinShort)
                  Unmodelled.Row ("R3 dual-mode, abortive close: getsockname", abortive)
                  Unmodelled.Row ("R3 dual-mode, error taken: getsockname", abortive)
                  for phase in
                      [
                          "H1 established"
                          "H2 refused pending"
                          "H3 completed, unreported"
                          "H4 idle"
                      ] do
                      for family in [ "AF_INET" ; "AF_INET6" ; "family 99" ] do
                          Unmodelled.Row ($"%s{phase}, %s{family} at 16", darwinShort)
              ]

    let private compare (platform : SimulatedUnixPlatform) (section : char) : unit =
        let flavour = SimulatedUnixPlatform.flavour platform
        // A line's label: what comes before its cell, or before its answer.
        let labelOf (line : string) : string =
            let cut (marker : string) (text : string) =
                match text.IndexOf (marker, StringComparison.Ordinal) with
                | -1 -> text
                | i -> text.Substring (0, i)

            line |> cut " cell=" |> cut " -> "

        let expected = measured flavour section

        let actual =
            DualModeProbe.run platform section
            |> fun text -> text.Split ('\n', StringSplitOptions.RemoveEmptyEntries)
            |> Array.map (fun line -> normalise (line.TrimEnd '\r'))
            |> Array.toList

        let unmodelled = notModelled flavour

        let isRow (line : string) =
            unmodelled
            |> List.exists (fun entry ->
                match entry with
                | Unmodelled.Row (label, _) -> labelOf line = normalise label
                | Unmodelled.Following _ -> false
            )

        let isFollowing (line : string) =
            unmodelled
            |> List.exists (fun entry ->
                match entry with
                | Unmodelled.Following (label, _) -> labelOf line = normalise label
                | Unmodelled.Row _ -> false
            )

        // Section C's rows are each one line with a label of its own, and this
        // kernel answers only those whose sides it binds, so they are matched
        // by label; every other section line by line.
        let expected, actual =
            if section = 'C' then
                let printed = actual |> List.map (fun line -> labelOf line, line) |> Map.ofList

                let replayed =
                    expected
                    |> List.filter (fun line ->
                        line.StartsWith ("==", StringComparison.Ordinal)
                        || Map.containsKey (labelOf line) printed
                    )

                let unanswered =
                    printed
                    |> Map.filter (fun label _ -> not (List.exists (fun line -> labelOf line = label) replayed))

                if not (Map.isEmpty unanswered) then
                    failwith $"%O{flavour} section C: rows printed that the probe did not measure: %A{unanswered}"

                // The header, and the 28 rows over `DualModeProbe`'s three sides.
                if List.length replayed <> 29 then
                    failwith $"%O{flavour} section C: %d{List.length replayed - 1} rows replayed, not 28"

                replayed, actual
            else
                expected, actual

        if List.length expected <> List.length actual then
            failwith
                $"%O{flavour} section %c{section}: %d{List.length actual} lines printed against %d{List.length expected} measured.\nPrinted:\n%s{String.Join ('\n', actual)}"

        // Which measured lines are unmodelled: a listed row, or a listed
        // following line straight after an unmodelled one.
        let skipped =
            expected
            |> List.fold
                (fun (acc : bool list) (line : string) ->
                    let previous =
                        match acc with
                        | previous :: _ -> previous
                        | [] -> false

                    (isRow line || (previous && isFollowing line)) :: acc
                )
                []
            |> List.rev

        let rows = List.zip3 expected actual skipped

        let differing =
            rows
            |> List.filter (fun (e, a, skip) -> e <> a && not skip)
            |> List.map (fun (e, a, _) -> e, a)

        // A listed row must still differ, or it no longer belongs in the list.
        let nowMatching =
            rows
            |> List.filter (fun (e, a, skip) -> e = a && skip && isRow e)
            |> List.map (fun (e, a, _) -> e, a)

        if not (List.isEmpty differing) || not (List.isEmpty nowMatching) then
            let show (rows : (string * string) list) =
                rows
                |> List.map (fun (e, a) -> $"  measured: %s{e}\n  printed:  %s{a}")
                |> fun rows -> String.Join ('\n', rows)

            failwith
                $"%O{flavour} section %c{section}: %d{List.length differing} lines printed otherwise than measured:\n%s{show differing}\n%d{List.length nowMatching} listed lines now printed as measured:\n%s{show nowMatching}"

    /// Section D is not replayed: it listens on IPv6 sockets, which this kernel
    /// refuses (`ListenRefusal.Ipv6Listener`), so none of its calls after the
    /// listen has a counterpart here.
    let private sections : char list =
        [ 'A' ; 'N' ; 'B' ; 'C' ; 'E' ; 'F' ; 'G' ; 'H' ; 'R' ]

    [<TestCaseSource(nameof sections)>]
    let ``every Linux row is printed as measured`` (section : char) : unit =
        compare SimulatedUnixPlatform.linuxX64 section

    [<TestCaseSource(nameof sections)>]
    let ``every Darwin row is printed as measured`` (section : char) : unit =
        compare SimulatedUnixPlatform.macOsArm64 section
