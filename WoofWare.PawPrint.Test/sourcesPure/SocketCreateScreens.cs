using System;
using System.Runtime.InteropServices;

// `SystemNative_Socket`'s contract, restricted to the rows every kernel agrees
// on: the three argument screens the C wrapper applies before any syscall
// (pal_networking.c:2812-2838), the order it applies them in, the `-1` it stores
// through the out-parameter on each, and the handful of sockets that both Linux
// and macOS hand to an ordinary user.
//
// Differential, and only these rows can be. The screens are the shim's own, so
// they answer identically wherever the shim was built; the success rows below
// were measured to succeed unprivileged on both platforms. The rows the *kernel*
// decides are not like that -- `AF_INET`/`SOCK_STREAM`/`PT_UDP` is
// EPROTONOSUPPORT on Linux and EPROTOTYPE on macOS -- and are deliberately
// absent. PawPrint's answers for the whole 330-row matrix are checked against
// the measurement itself in TestSocketCreation.fs.
//
// Reached by hand-rolled P/Invoke rather than through `System.Net.Sockets.Socket`
// because the managed path turns the returned `Interop.Error` into a
// `SocketError` through an `EnumEqualityComparer`, which needs the
// `RuntimeHelpers.EnumEquals` JIT intrinsic that PawPrint does not yet
// implement. That is a lookup table, not part of this entry point's contract.
//
// The pairs exist to pin the screens' order: each supplies two bad arguments at
// once and asserts which one is reported, so a handler that ran them in a
// different order would fail rather than coincidentally agreeing.
//
// The exit code is the index of the first check that failed; 0 means all passed.
// Kept below 128, since an exit code is eight bits.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Socket")]
    static extern unsafe int Socket(int addressFamily, int socketType, int protocolType, IntPtr* createdSocket);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close")]
    static extern int Close(IntPtr fd);

    // Interop.Error values, not raw errnos: this entry point returns the PAL enum
    // directly rather than -1-and-errno. The raw numbers for these three differ
    // between the platforms (97/91/93 against 47/41/43), which is exactly why a
    // differential guest must read the PAL value.
    const int PAL_SUCCESS = 0;
    const int PAL_EAFNOSUPPORT = 0x10005;
    const int PAL_EFAULT = 0x10015;
    const int PAL_EPROTONOSUPPORT = 0x10045;
    const int PAL_EPROTOTYPE = 0x10046;

    // PAL AddressFamily.
    const int AF_UNIX = 1;
    const int AF_INET = 2;

    // PAL SocketType.
    const int SOCK_STREAM = 1;
    const int SOCK_DGRAM = 2;

    // PAL ProtocolType.
    const int PT_UNSPECIFIED = 0;
    const int PT_TCP = 6;
    const int PT_UDP = 17;
    const int PT_ICMPV6 = 58;

    // Outside their respective enums, so the conversion's default arm is what
    // answers rather than a case that happens to be compiled out.
    const int BadFamily = 12345;
    const int BadType = 99;

    // Something no `socket(2)` would return, so that "the wrapper stored -1" is
    // distinguishable from "the wrapper stored nothing".
    static readonly IntPtr Untouched = (IntPtr)0x5EED;

    /// Returns true if the triple was refused with exactly `expected`, and the
    /// out-parameter was left holding -1.
    static unsafe bool Refuses(int addressFamily, int socketType, int protocolType, int expected)
    {
        IntPtr created = Untouched;
        int result = Socket(addressFamily, socketType, protocolType, &created);
        return result == expected && created == (IntPtr)(-1);
    }

    /// Returns true if the triple produced a socket. The descriptor *number* is
    /// not asserted: it is unpredictable under the real runtime, which holds
    /// descriptors of its own (see OpenFdNumbering.cs).
    static unsafe bool Creates(int addressFamily, int socketType, int protocolType)
    {
        IntPtr created = Untouched;
        int result = Socket(addressFamily, socketType, protocolType, &created);

        if (result != PAL_SUCCESS || (long)created < 0)
        {
            return false;
        }

        // Leaving it open would be a descriptor leak in a guest that goes on to
        // make more, and the close is itself a check that the thing handed back
        // is a live descriptor.
        return Close(created) == 0;
    }

    static unsafe int Main()
    {
        // The wrapper's first screen, ahead of every conversion: a null
        // out-parameter, with an otherwise entirely valid triple.
        if (Socket(AF_INET, SOCK_STREAM, PT_TCP, null) != PAL_EFAULT)
            return 1;

        // An unknown address family.
        if (!Refuses(BadFamily, SOCK_STREAM, PT_TCP, PAL_EAFNOSUPPORT))
            return 2;

        // The family screen precedes the socket-type screen: both arguments are
        // bad, and the family is what comes back.
        if (!Refuses(BadFamily, BadType, PT_TCP, PAL_EAFNOSUPPORT))
            return 3;

        // An unknown socket type. Note the shim reports this as EPROTOTYPE,
        // where a kernel asked the same question would say ESOCKTNOSUPPORT.
        if (!Refuses(AF_INET, BadType, PT_TCP, PAL_EPROTOTYPE))
            return 4;

        // The socket-type screen precedes the protocol screen, by the same
        // construction as check 3.
        if (!Refuses(AF_INET, BadType, PT_ICMPV6, PAL_EPROTOTYPE))
            return 5;

        // A protocol with no entry in this family's table. ICMPv6 converts under
        // AF_INET6 and not under AF_INET, so this is a fact about the per-family
        // table rather than about the protocol number.
        if (!Refuses(AF_INET, SOCK_STREAM, PT_ICMPV6, PAL_EPROTONOSUPPORT))
            return 6;

        // AF_UNIX shares the conversion's `default` arm, which takes the
        // unspecified protocol and nothing else -- so TCP, legal under AF_INET
        // two checks above, is refused here.
        if (!Refuses(AF_UNIX, SOCK_STREAM, PT_TCP, PAL_EPROTONOSUPPORT))
            return 7;

        // The sockets both platforms make for an ordinary user. The unspecified
        // protocol and the explicit one are separate rows because they take
        // different paths through the conversion.
        if (!Creates(AF_INET, SOCK_STREAM, PT_UNSPECIFIED))
            return 8;

        if (!Creates(AF_INET, SOCK_STREAM, PT_TCP))
            return 9;

        if (!Creates(AF_INET, SOCK_DGRAM, PT_UNSPECIFIED))
            return 10;

        if (!Creates(AF_INET, SOCK_DGRAM, PT_UDP))
            return 11;

        if (!Creates(AF_UNIX, SOCK_STREAM, PT_UNSPECIFIED))
            return 12;

        if (!Creates(AF_UNIX, SOCK_DGRAM, PT_UNSPECIFIED))
            return 13;

        return 0;
    }
}
