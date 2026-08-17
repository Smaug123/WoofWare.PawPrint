using System;
using System.Runtime.InteropServices;

// Exercises the SystemNative_GetMaximumAddressSize PawPrint handler directly via
// a P/Invoke stub. Upstream (pal_networking.c) it is a one-liner:
//
//     int32_t SystemNative_GetMaximumAddressSize(void)
//     { return sizeof(struct sockaddr_storage); }
//
// A direct stub rather than reaching it through the BCL. The only managed caller
// is System.Net.Sockets.SocketPal's class initialiser, and the statement right
// after it calls Interop.Sys.PlatformSupportsDualModeIPv4PacketInfo -- a
// different entry point, which is its own change -- so a socket-using guest
// cannot be the vehicle for this one. The shim exports the symbol on every Unix
// (pal_networking.c has no platform #if around it), so the stub resolves on the
// real runtime too.
//
// This is a *pure* case, so it is differentially compared against the real
// runtime and may only assert facts that hold on both. The exact value is such a
// fact, and asserting it is what stops this test being vacuous: a guest that
// merely checked "the call returned" would exit 0 on both runtimes whatever
// PawPrint answered, so it could not tell a wrong constant from a right one.
// Because the fixture runs the real half too, the literal below is re-pinned
// against a real platform on every run -- macOS locally and Linux in CI, so both
// families we model are continuously checked rather than measured once.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetMaximumAddressSize")]
    static extern int GetMaximumAddressSize();

    // sizeof(struct sockaddr_storage). Both families we run on *define* this
    // rather than computing it -- _SS_MAXSIZE on Darwin
    // (MacOSX.sdk/usr/include/sys/socket.h) and _SS_SIZE in glibc's generic
    // bits/sockaddr.h -- and derive the padding members from it, so it is
    // invariant of pointer width as well as agreed between the two; both descend
    // from RFC 2553's sample definition, which is why they agree. Contrast
    // sockaddr_un, which really does differ (106 on Darwin, 110 on Linux); that
    // difference belongs to SystemNative_GetDomainSocketSizes.
    const int ExpectedMaximumAddressSize = 128;

    static int Main(string[] args)
    {
        int first = GetMaximumAddressSize();
        if (first != ExpectedMaximumAddressSize) return 1;

        // A compile-time constant of the shim, not a sample of anything: every
        // call must answer identically. This states the contract rather than
        // hunting a plausible bug -- the only implementation it could catch is
        // one answering from mutable state.
        int second = GetMaximumAddressSize();
        if (second != first) return 2;

        return 0;
    }
}
