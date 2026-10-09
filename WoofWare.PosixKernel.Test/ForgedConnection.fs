namespace WoofWare.PosixKernel.Test

open WoofWare.PosixKernel

/// A TCP connection's table entry, for a test that forges its sockets'
/// phases by hand rather than through `connect(2)` and `accept(2)`.
[<RequireQualifiedAccess>]
module internal ForgedConnection =

    /// `system` with `connection` in the connection table, between two
    /// loopback ports, its bytes as a fresh connection of `domain` holds them
    /// on this machine (`TcpBufferSizing.newTransfer`), and then each end in
    /// `closed` closed in order (`TcpTransfer.close`): an end whose socket the
    /// test has not forged, or has forged as gone.
    let add<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (connection : ConnectionId)
        (domain : SocketDomain)
        (closed : ConnectionEnd list)
        (system : UnixSystem<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        let transfer =
            (TcpBufferSizing.newTransfer domain system.Machine, closed)
            ||> List.fold (fun transfer closer -> TcpTransfer.close closer transfer |> snd)

        { system with
            Machine =
                { system.Machine with
                    Connections =
                        Map.add
                            connection
                            {
                                ClientAddress = InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress 40000us
                                ServerAddress = InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress 80us
                                Transfer = transfer
                            }
                            system.Machine.Connections
                }
        }
