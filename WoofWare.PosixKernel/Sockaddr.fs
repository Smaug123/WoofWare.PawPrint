namespace WoofWare.PosixKernel

/// The four `sizeof`s `SystemNative_GetSocketAddressSizes` reports in one call,
/// which `System.Net.Primitives`' `SocketAddressPal` class initialiser latches
/// and every `SocketAddress` is then sized by.
///
/// Compile-time properties of the native shim rather than of any socket, like
/// `reportsBirthTime`. Measured with a `sizeof` probe compiled on macOS arm64 and
/// on Linux, rather than recalled; all four are invariant of pointer width, since
/// every member of these structs is fixed-width and the two variable-length tails
/// (`sun_path`, `sockaddr_storage`'s padding) are sized from a constant.
type SocketAddressSizes =
    {
        /// <summary>
        /// <c>sizeof(struct sockaddr_in)</c>.
        /// </summary>
        /// <example>16 on both Linux and Darwin.</example>
        InterNetwork : int
        /// <summary>
        /// <c>sizeof(struct sockaddr_in6)</c>.
        /// </summary>
        /// <example>28 on both Linux and Darwin.</example>
        InterNetworkV6 : int
        /// `sizeof(struct sockaddr_un)`. The one of the four that differs: 110 on
        /// Linux, whose `sun_path` is 108 bytes, against 106 on Darwin, whose is
        /// 104.
        UnixDomain : int
        /// `sizeof(struct sockaddr_storage)`. 128 on both, and the same number
        /// `SystemNative_GetMaximumAddressSize` reports through its own entry
        /// point — hence `SimulatedUnixPlatform.maximumSocketAddressSize` rather
        /// than a second literal.
        Storage : int
    }

/// Where a `struct sockaddr`'s address family sits and how wide it is — the only
/// part of the socket-address layout the two Unixes lay out differently.
///
/// BSD gave `struct sockaddr` a leading one-byte `sa_len` and narrowed
/// `sa_family_t` to one byte to pay for it; Linux kept the original two-byte
/// `sa_family_t` and has no length byte. That is why every *later* field agrees
/// between the two — `sin_port` at 2, `sin_addr` at 4, `sin6_addr` at 8,
/// `sin6_scope_id` at 24, all measured on both — since the two layouts spend the
/// same two leading bytes differently rather than in different amounts.
///
/// One field of one of the `sockaddr` structs, as a byte range within it.
///
/// Offset and width travel together because a caller that has one always wants
/// the other: every use is either "read these bytes" or "does the caller's
/// declared length reach them".
///
/// **Carries no byte order**, deliberately. The fields' orders are kernel ABI --
/// `sin_port` and `sin_addr` are network order, `sin6_scope_id` is the host's --
/// but whether a given *caller* swaps is that caller's own contract, and the two
/// do not agree: `SystemNative_GetPort` byte-swaps where
/// `SystemNative_GetIPv4Address` copies the address word verbatim, both sides of
/// that call holding it in network order. An order carried here would invite an
/// order-normalising accessor, and the first caller to reach for one would
/// silently acquire a swap its own contract does not have.
type SockaddrField =
    {
        /// Byte offset of the field from the start of the struct.
        Offset : int
        /// The field's width in bytes.
        Width : int
    }

[<RequireQualifiedAccess>]
module SockaddrField =
    /// Whether a declared sockaddr length reaches all of this field.
    ///
    /// A negative length fails it, and that is not incidental: a layer that casts
    /// the length to an unsigned type makes the bound enormous rather than
    /// negative, so this answers for what the *caller declared* before any such
    /// cast.
    ///
    /// A malformed descriptor is refused rather than answered for. `SockaddrField`
    /// is a public record, so unlike the closed `SockaddrFamilyField` it can be
    /// built with nonsense; a negative offset or width describes no field of any
    /// struct.
    let reachedBy (field : SockaddrField) (declaredLength : int) : bool =
        if field.Offset < 0 || field.Width < 0 then
            failwith
                $"SockaddrField.reachedBy: a field at offset %d{field.Offset} of width %d{field.Width} describes no part of any struct (this is a bug in the caller)."

        // Rearranged to subtract rather than add, so that a field whose end is
        // past `Int32.MaxValue` is not reached instead of wrapping onto a low
        // bound that every length satisfies. The same rearrangement, for the same
        // reason, as `UserBufferCheck.faultsBeforeOperation`. The guard above is
        // what keeps this subtraction from underflowing.
        declaredLength >= field.Offset && declaredLength - field.Offset >= field.Width

/// <summary>
/// <c>struct sockaddr_in</c>'s fields, excluding the family.
/// </summary>
/// <remarks>
/// For the family field, see <c>SockaddrFamilyField</c> (which is split out because it differs between platforms).
/// </remarks>
(*
Measured on Linux 6.18.5 and Darwin 25.6.0 with
`docs/plans/2026-08-23-posix-kernel-extraction/sockaddr-layout.c`: both put
`sin_port` at 2 and `sin_addr` at 4, and both make the struct 16 bytes. These
are therefore plain values rather than functions of the platform -- the same
distinction `internetAddressFamily` draws against `internetV6AddressFamily`.
*)
[<RequireQualifiedAccess>]
module InternetSockaddr =
    /// <summary><c>sin_port</c>, in network byte order.</summary>
    let port : SockaddrField =
        {
            Offset = 2
            Width = 2
        }

    /// <summary><c>sin_addr</c>, four bytes in network byte order.</summary>
    let address : SockaddrField =
        {
            Offset = 4
            Width = 4
        }

/// `struct sockaddr_in6`'s fields beyond the family, measured alongside the
/// above and likewise identical on both platforms.
///
/// A separate module from `InternetSockaddr` rather than a shared set of
/// constants, though `sin6_port` and `sin_port` coincide: they are two fields of
/// two structs, and a use site should say which struct it means. Note what this
/// does *not* buy -- `sin6_flowinfo` and `sin_addr` both sit at offset 4, so
/// confusing them is still a mutation nothing can catch.
[<RequireQualifiedAccess>]
module InternetV6Sockaddr =
    /// `sin6_port`, in network byte order. The same offset and width as
    /// `sin_port`, and stated separately because it is a different field.
    let port : SockaddrField =
        {
            Offset = 2
            Width = 2
        }

    /// `sin6_flowinfo`. Nothing in the managed surface reads it, but
    /// `SystemNative_SetIPv6Address` zeroes it, so it is not merely ignored.
    let flowInfo : SockaddrField =
        {
            Offset = 4
            Width = 4
        }

    /// `sin6_addr`. Its width is `sizeof(struct in6_addr)`, which is the length
    /// every IPv6 address buffer must have room for.
    let address : SockaddrField =
        {
            Offset = 8
            Width = 16
        }

    /// `sin6_scope_id`, four bytes in the *host's* own byte order -- unlike the
    /// port beside it, which is network order.
    let scopeId : SockaddrField =
        {
            Offset = 24
            Width = 4
        }

/// A pair of numbers rather than an `int * int` so that no caller can pair an
/// offset with the wrong width: the two vary together and never independently.
[<RequireQualifiedAccess>]
type SockaddrFamilyField =
    /// Linux: `sa_family_t` is a two-byte `unsigned short` at offset 0, in the
    /// machine's own byte order, and there is no length byte before it.
    | TwoBytesAtOffsetZero
    /// Darwin and the BSDs: `sa_len` occupies byte 0 and the one-byte
    /// `sa_family_t` follows it at offset 1.
    ///
    /// Nothing in the shim writes `sa_len` — grep `pal_networking.c` and there is
    /// no mention of it. The byte a guest sees there is written by managed code:
    /// `SocketAddress..ctor` stores `(byte) _size` at index 0 before calling
    /// `SetAddressFamily`, unconditionally on every platform, so BSD gets its
    /// length byte and Linux has the same store overwritten by the wider family.
    | OneByteAtOffsetOne

[<RequireQualifiedAccess>]
module SockaddrFamilyField =
    /// Byte offset of the family field within any `struct sockaddr`.
    let offset (field : SockaddrFamilyField) : int =
        match field with
        | SockaddrFamilyField.TwoBytesAtOffsetZero -> 0
        | SockaddrFamilyField.OneByteAtOffsetOne -> 1

    /// Width of the family field in bytes. Also what the shim's
    /// `sizeof_member(sockaddr, sa_family)` bounds check uses, and what a
    /// conversion failure truncates the unconvertible value to.
    let width (field : SockaddrFamilyField) : int =
        match field with
        | SockaddrFamilyField.TwoBytesAtOffsetZero -> 2
        | SockaddrFamilyField.OneByteAtOffsetOne -> 1

    /// Whether a declared sockaddr length reaches the family field at all.
    ///
    /// Two callers with two justifications. A kernel's copy-in helper reads
    /// nothing on Darwin at a length this rejects, which is why `connect(2)` can
    /// answer without touching the caller's buffer at all; and a foreign-function
    /// layer that screens the field before reading or writing it asks exactly the
    /// same arithmetic. Both are this one comparison, so it lives here rather
    /// than being written out twice.
    ///
    /// A negative length fails it, and that is not incidental: a layer that casts
    /// the length to an unsigned type makes the bound enormous rather than
    /// negative, so this answers for what the *caller declared* before any such
    /// cast.
    let reachedBy (field : SockaddrFamilyField) (declaredLength : int) : bool =
        SockaddrField.reachedBy
            {
                Offset = offset field
                Width = width field
            }
            declaredLength

/// A reason `bind(2)` refuses, as one of the checks it makes rather than as an
/// errno: which errno a fault becomes is fixed, but *which fault is reported*
/// when several hold at once is per-flavour. See
/// `SimulatedUnixPlatform.bindFaultOrder`.
/// What this platform's `bind(2)` makes of a declared `socketAddressLen`.
///
/// The two rejections are not interchangeable, and the difference is *when* they
/// happen rather than which errno they carry. Measured on both: a length past the
/// upper bound is rejected before the kernel copies anything, so it beats a
/// faulting pointer and beats the family check — an unmapped pointer at 129 is
/// EINVAL on Linux where at 8 it is EFAULT, and a wrong-family blob at 256 is
/// ENAMETOOLONG on Darwin where at 129 it is EAFNOSUPPORT. A length merely too
/// short takes its ordinary place in `bindFaultOrder`.
[<RequireQualifiedAccess>]
type BindLengthVerdict =
    /// A length this platform will parse an address out of.
    | Accepted
    /// Past the greatest length this platform will consider, and so refused
    /// before the address is copied or read at all. Linux answers `EINVAL` above
    /// `sizeof(struct sockaddr_storage)`; Darwin answers `ENAMETOOLONG` above its
    /// own, larger threshold.
    | RejectedBeforeCopy of error : UnixError
    /// `EINVAL`, from the `Length` position of this platform's fault order.
    | Invalid

[<RequireQualifiedAccess>]
type BindFault =
    /// The declared `socketAddressLen` is not one this platform accepts for the
    /// address family in the blob. Which errno that becomes is the
    /// `BindLengthVerdict` the length classifier gave — `EINVAL`, or
    /// `ENAMETOOLONG` past the greatest length the platform considers — but the
    /// *position* in the order is the same either way, which is why the verdict
    /// is not carried here.
    | Length
    /// The blob's address family is not the socket's. `EAFNOSUPPORT`.
    | Family
    /// No local interface holds the address. `EADDRNOTAVAIL`.
    | AddressNotLocal
    /// The port is below `privilegedPortCeiling` and the process is not root.
    /// `EACCES`.
    | PrivilegedPort
    /// This socket already has a local address. `EINVAL`.
    | AlreadyBound
    /// Another socket holds a conflicting address. `EADDRINUSE`.
    | AddressInUse
