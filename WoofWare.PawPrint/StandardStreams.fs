namespace WoofWare.PawPrint

open System.Collections.Immutable
open WoofWare.PosixKernel

/// How PawPrint launches a guest, and how it reads back what the guest wrote.
///
/// Every guest starts as the oracle `RealRuntime` starts one: each standard
/// stream on a pipe of its own, standard input supplying nothing and the two
/// output streams drained by PawPrint as fast as the guest writes. The kernel
/// knows those pipes only by the descriptor each was launched on; the roles
/// are PawPrint's.
[<RequireQualifiedAccess>]
module StandardStreams =
    /// The launch table every guest starts with: descriptors 0, 1 and 2, as
    /// `UnixSystem.pipedStandardStreams` describes.
    let launch : Map<int, LaunchDescriptor> = UnixSystem.pipedStandardStreams

    /// The standard stream whose pipe the client's `endpoint` is the far end of.
    ///
    /// Total over the endpoints `launch` makes, and a failure for any other:
    /// PawPrint launches no other descriptor, so an endpoint it did not make
    /// means the kernel was built some other way.
    let roleOf (endpoint : ExternalEndpoint) : FileDescriptorRole =
        match endpoint with
        | ExternalEndpoint 0 -> FileDescriptorRole.StandardInput
        | ExternalEndpoint 1 -> FileDescriptorRole.StandardOutput
        | ExternalEndpoint 2 -> FileDescriptorRole.StandardError
        | ExternalEndpoint fd ->
            failwith
                $"StandardStreams.roleOf: %O{endpoint} is not one PawPrint launches; it launches descriptors 0, 1 and 2 only, so a delivery to launch descriptor %d{fd} means the kernel was not built from StandardStreams.launch (this is an interpreter bug)."

    /// `delivered`, each delivery labelled with the standard stream it reached:
    /// what the guest wrote to its output streams, one entry per write, in the
    /// order it wrote them.
    let outputLog (delivered : ImmutableArray<Delivery>) : ImmutableArray<OutputLogEntry> =
        let builder = ImmutableArray.CreateBuilder<OutputLogEntry> delivered.Length

        for delivery in delivered do
            builder.Add
                {
                    Role = roleOf delivery.Endpoint
                    Bytes = delivery.Bytes
                }

        builder.MoveToImmutable ()
