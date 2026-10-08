namespace WoofWare.PosixKernel

/// One TCP connection, as the emulated kernel's connection table holds it.
///
/// Keyed by `ConnectionId`, and holding the two endpoints' addresses and the
/// bytes travelling between them. The bytes are the connection's rather than
/// either socket's because a client may write before the server end has a
/// socket at all.
///
/// Deliberately no references back to the sockets on its ends: a connection
/// outlives the client that opened it (measured: close the client while its
/// connection sits in an accept queue, and `accept(2)` still returns it), and
/// the server end has no socket at all until that accept, so an end-to-socket
/// field would spend most of its life dangling or `None`. Cleanup instead
/// scans the socket table for references, which `UnixDescriptor.close` does.
type TcpConnection =
    {
        /// The connecting side's address — what `accept(2)` reports as the
        /// peer.
        ClientAddress : InternetEndpoint
        /// The accepted side's address: the destination the client connected
        /// to, with a wildcard destination already rewritten to loopback. The
        /// accepted socket's own `getsockname(2)` reports this.
        ServerAddress : InternetEndpoint
        /// The bytes travelling each way, and what each end knows of the
        /// connection's ending.
        Transfer : TcpTransfer
    }
