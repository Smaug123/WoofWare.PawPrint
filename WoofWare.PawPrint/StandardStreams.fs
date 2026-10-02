namespace WoofWare.PawPrint

open System.Collections.Immutable
open WoofWare.PosixKernel

/// Which of its standard streams a guest was launched with a pipe for: what
/// PawPrint calls the far ends it holds (see `StandardStreams.roleOf`).
[<RequireQualifiedAccess>]
type FileDescriptorRole =
    | StandardInput
    | StandardOutput
    | StandardError

/// One entry in `EmulatedKernel.OutputLog`: the stream a write reached
/// (standard output or error) and the bytes that single `write(2)` delivered
/// there. Chunks are not coalesced across
/// calls because write boundaries matter for diagnostics (line
/// boundaries, prompt boundaries) and are what a real reader of the stream
/// could observe.
type OutputLogEntry =
    {
        Role : FileDescriptorRole
        Bytes : ImmutableArray<byte>
    }

[<RequireQualifiedAccess>]
module OutputLogEntry =
    /// Concatenate every entry in `log` whose `Role` matches `role`,
    /// preserving the original write order. Used by tests that want to
    /// assert on the cumulative bytes the guest sent to a specific
    /// standard stream (the equivalent of capturing one of host
    /// stdout/stderr in isolation).
    let bytesFor (role : FileDescriptorRole) (log : ImmutableArray<OutputLogEntry>) : ImmutableArray<byte> =
        let builder = ImmutableArray.CreateBuilder<byte> ()

        for entry in log do
            if entry.Role = role then
                builder.AddRange (entry.Bytes : ImmutableArray<byte>)

        builder.ToImmutable ()

/// How PawPrint launches a guest, and how it reads back what the guest wrote.
///
/// Every guest starts as the oracle `RealRuntime` starts one: each standard
/// stream on a pipe of its own, standard input supplying the bytes the host
/// gave it (`KernelConfig.StandardInput`) and the two output streams drained
/// by PawPrint as fast as the guest writes. The kernel knows those pipes only
/// by the descriptor each was launched on; the roles are PawPrint's.
[<RequireQualifiedAccess>]
module StandardStreams =
    /// The launch table every guest starts with: descriptors 0, 1 and 2, as
    /// `UnixSystem.pipedStandardStreams` describes, with `standardInput` the
    /// bytes PawPrint writes into descriptor 0's pipe before closing it.
    let launch (standardInput : ImmutableArray<byte>) : Map<int, LaunchDescriptor> =
        Map.add 0 (LaunchDescriptor.Supplied standardInput) UnixSystem.pipedStandardStreams

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
