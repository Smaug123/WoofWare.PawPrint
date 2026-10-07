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

/// What the launcher does with the read end of one of a guest's output
/// streams.
[<RequireQualifiedAccess>]
type OutputStreamReader =
    /// PawPrint reads every byte the moment the guest writes it, so a write is
    /// never short and never waits.
    | Drained
    /// The launcher closed the read end before the guest started, as when the
    /// reader of a shell pipeline has already exited. Nothing will ever read
    /// the stream: every write to it answers EPIPE and raises SIGPIPE, which
    /// the runtime ignores (`StartupSignalDispositions`).
    | Gone

/// How a guest's standard streams are launched: each on a pipe of its own,
/// whose far end the launcher holds.
///
/// There is no `Gone` for standard input: a writer that closed before the
/// guest started, having written nothing, is `Input` empty.
type StandardStreamsConfig =
    {
        /// The bytes the launcher writes into standard input, a pipe, with one
        /// blocking write before closing its end, as `cmd < file` or a harness
        /// feeding a child does. Empty: the guest reads end of file at once.
        ///
        /// What the pipe cannot hold yet goes in as the guest reads, so a
        /// guest never waits for input and sees end of file only after the last
        /// byte. More than one `write(2)` moves on the platform (0x7FFFF000
        /// bytes on Linux) is refused.
        Input : ImmutableArray<byte>
        /// What the launcher does with the read end of standard output.
        Output : OutputStreamReader
        /// What the launcher does with the read end of standard error.
        Error : OutputStreamReader
    }

[<RequireQualifiedAccess>]
module StandardStreamsConfig =
    /// Nothing on standard input, and both output streams drained: how the
    /// oracle `RealRuntime` starts a guest by default.
    let piped : StandardStreamsConfig =
        {
            Input = ImmutableArray.Empty
            Output = OutputStreamReader.Drained
            Error = OutputStreamReader.Drained
        }

/// How PawPrint launches a guest, and how it reads back what the guest wrote.
///
/// Every guest starts as the oracle `RealRuntime` starts one: each standard
/// stream on a pipe of its own, standard input supplying the bytes the host
/// gave it and each output stream drained by PawPrint as fast as the guest
/// writes, or with no reader at all, as `KernelConfig.StandardStreams` says.
/// The kernel knows those pipes only by the descriptor each was launched on;
/// the roles are PawPrint's.
[<RequireQualifiedAccess>]
module StandardStreams =
    /// The launch table a guest starts with: descriptors 0, 1 and 2, as
    /// `UnixSystem.pipedStandardStreams` describes, with what `config` says
    /// each pipe's far end does.
    let launch (config : StandardStreamsConfig) : Map<int, LaunchDescriptor> =
        let output (reader : OutputStreamReader) : LaunchDescriptor =
            match reader with
            | OutputStreamReader.Drained -> LaunchDescriptor.Drained
            | OutputStreamReader.Gone -> LaunchDescriptor.Gone

        if config.Input.IsDefault then
            failwith
                "StandardStreams.launch: Input is the default ImmutableArray, whose underlying array is null. To supply nothing, pass ImmutableArray<byte>.Empty."

        UnixSystem.pipedStandardStreams
        |> Map.add 0 (LaunchDescriptor.Supplied config.Input)
        |> Map.add 1 (output config.Output)
        |> Map.add 2 (output config.Error)

    /// The standard stream whose pipe the client's `endpoint` is the far end of,
    /// for the guest whose process ID is `guest`.
    ///
    /// Total over the endpoints `launch` makes for that guest, and a failure for
    /// any other: PawPrint launches no other descriptor, so an endpoint it did
    /// not make means the kernel was built some other way.
    let roleOf (guest : ProcessId) (endpoint : ExternalEndpoint) : FileDescriptorRole =
        match endpoint with
        | ExternalEndpoint (launchedInto, fd) when launchedInto <> guest ->
            failwith
                $"StandardStreams.roleOf: %O{endpoint} was launched into process %O{launchedInto}, not into the guest, process %O{guest}, so its descriptor %d{fd} is not one of the guest's standard streams (this is an interpreter bug)."
        | ExternalEndpoint (_, 0) -> FileDescriptorRole.StandardInput
        | ExternalEndpoint (_, 1) -> FileDescriptorRole.StandardOutput
        | ExternalEndpoint (_, 2) -> FileDescriptorRole.StandardError
        | ExternalEndpoint (_, fd) ->
            failwith
                $"StandardStreams.roleOf: %O{endpoint} is not one PawPrint launches; it launches descriptors 0, 1 and 2 only, so a delivery to launch descriptor %d{fd} means the kernel was not built from StandardStreams.launch (this is an interpreter bug)."

    /// The deliveries in `delivered` to the pipes launched into the guest whose
    /// process ID is `guest`, each labelled with the standard stream it reached:
    /// what the guest wrote to its output streams, one entry per write, in the
    /// order it wrote them.
    let outputLog (guest : ProcessId) (delivered : DeliveryLog) : ImmutableArray<OutputLogEntry> =
        let builder = ImmutableArray.CreateBuilder<OutputLogEntry> ()

        for delivery in DeliveryLog.toList delivered do
            match delivery.Endpoint with
            | ExternalEndpoint (launchedInto, _) when launchedInto <> guest -> ()
            | ExternalEndpoint _ ->
                builder.Add
                    {
                        Role = roleOf guest delivery.Endpoint
                        Bytes = delivery.Bytes
                    }

        builder.ToImmutable ()
