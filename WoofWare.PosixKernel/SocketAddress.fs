namespace WoofWare.PosixKernel

open System.Collections.Immutable
open System.Text

/// <summary>
/// A Unix-domain socket's name: the address <c>bind(2)</c> gives an <c>AF_UNIX</c> socket.
/// </summary>
/// <remarks>
/// The representation is hidden, because what Darwin stores for a name (in particular
/// whether it keeps bytes past the first NUL of <c>sun_path</c>) is unmeasured, and
/// measuring it may change what a name has to hold. Read one with
/// <c>UnixSocketName.view</c>.
///
/// An unnamed socket (never bound, or made by <c>socketpair(2)</c>) has no name rather
/// than an empty one, so no value of this type stands for "unnamed".
///
/// This kernel binds no Unix-domain socket yet, so nothing it hands out holds one.
/// </remarks>
type UnixSocketName =
    private
    | Path of UnixByteString
    // A `byte[]` rather than an `ImmutableArray<byte>`: F# compares and hashes
    // an array by its elements, where `ImmutableArray`'s comparison throws on
    // two arrays of different lengths. Never mutated, and never handed out:
    // `UnixSocketName.ofAbstractName` copies in and `view` copies out.
    | Abstract of byte[]

/// <summary>
/// A Unix-domain socket's name, as <c>UnixSocketName.view</c> reads it.
/// </summary>
[<RequireQualifiedAccess>]
[<NoComparison>]
type UnixSocketNameView =
    /// <summary>
    /// A pathname socket: <c>sun_path</c> up to its first NUL, exactly as the caller of
    /// <c>bind(2)</c> wrote it. Not empty. Not resolved: a relative path stays relative,
    /// since that is how <c>getsockname(2)</c> reports it.
    /// </summary>
    | Path of path : UnixByteString
    /// <summary>
    /// A Linux abstract socket: <c>sun_path</c> began with a NUL, and the name is every
    /// byte after that NUL up to the length the caller declared. NULs within it, and
    /// trailing ones, are part of the name. Possibly empty. Has no place in the
    /// filesystem. Darwin has no abstract sockets.
    /// </summary>
    /// <remarks>
    /// Two views are equal when their bytes are.
    /// </remarks>
    | Abstract of name : ImmutableArray<byte>

/// <summary>
/// A socket address in any of the families this kernel's sockets can be in: what an
/// answer carries where one signature must answer for every family.
/// </summary>
[<RequireQualifiedAccess>]
type SocketAddress =
    /// <summary>An <c>AF_INET</c> address: a <c>struct sockaddr_in</c>.</summary>
    | Inet of Ipv4Endpoint
    /// <summary>An <c>AF_INET6</c> address: a <c>struct sockaddr_in6</c>.</summary>
    | Inet6 of Ipv6Endpoint
    /// <summary>An <c>AF_UNIX</c> address: a <c>struct sockaddr_un</c>'s name.</summary>
    | Unix of UnixSocketName

[<RequireQualifiedAccess>]
module UnixSocketName =

    /// The pathname socket named `path`, as `sun_path` up to its first NUL.
    /// Throws for an empty `path`: an empty `sun_path` names no pathname socket.
    let internal ofPath (path : UnixByteString) : UnixSocketName =
        if UnixByteString.length path = 0 then
            failwith
                "UnixSocketName.ofPath: an empty sun_path names no pathname socket (this is a bug in the caller, which should have answered for it first)."

        Path path

    /// The Linux abstract socket named `name`: the bytes of `sun_path` after its
    /// leading NUL, up to the declared length.
    let internal ofAbstractName (name : ImmutableArray<byte>) : UnixSocketName =
        if name.IsDefault then
            failwith
                "UnixSocketName.ofAbstractName: the name carries no bytes at all, so it came from `Unchecked.defaultof` or C# `default` (this is a bug in the caller)."

        Abstract (Seq.toArray name)

    /// <summary>
    /// What the name is: a path, or a Linux abstract name.
    /// </summary>
    let view (name : UnixSocketName) : UnixSocketNameView =
        match name with
        | Path path -> UnixSocketNameView.Path path
        | Abstract name -> UnixSocketNameView.Abstract (ImmutableArray.Create<byte> name)

    /// <summary>
    /// The name as text, for a diagnostic: a path as <c>UnixByteString.toEscaped</c>
    /// writes it, and an abstract name as its whole <c>sun_path</c>, so beginning with
    /// the NUL, written <c>\x00</c>. In an abstract name, every byte outside printable
    /// ASCII is written <c>\xNN</c>, and a backslash is doubled.
    /// </summary>
    /// <remarks>
    /// No two names are written alike: a path's own backslashes are doubled, so no
    /// path's text begins <c>\x</c>.
    ///
    /// Not a rendering any process sees.
    /// </remarks>
    let toString (name : UnixSocketName) : string =
        match name with
        | Path path -> UnixByteString.toEscaped path
        | Abstract name ->
            let builder = StringBuilder "\\x00"

            for b in name do
                if b = byte '\\' then
                    builder.Append "\\\\" |> ignore
                elif b >= 0x20uy && b < 0x7Fuy then
                    builder.Append (char b) |> ignore
                else
                    builder.Append $"\\x%02x{b}" |> ignore

            builder.ToString ()

[<RequireQualifiedAccess>]
module SocketAddress =

    /// <summary>
    /// The address as text, for a diagnostic: <c>Ipv4Endpoint.toString</c>,
    /// <c>Ipv6Endpoint.toString</c> or <c>UnixSocketName.toString</c>.
    /// </summary>
    /// <remarks>
    /// Not a rendering any process sees.
    /// </remarks>
    let toString (address : SocketAddress) : string =
        match address with
        | SocketAddress.Inet endpoint -> Ipv4Endpoint.toString endpoint
        | SocketAddress.Inet6 endpoint -> Ipv6Endpoint.toString endpoint
        | SocketAddress.Unix name -> UnixSocketName.toString name
