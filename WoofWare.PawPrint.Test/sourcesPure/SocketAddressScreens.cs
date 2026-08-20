using System;
using System.Runtime.InteropServices;

// The `SocketAddressPal` entry points reached by hand-rolled P/Invoke, for the
// contract the managed surface cannot express: the return codes.
// `System.Net.SocketAddress` never passes a null out-parameter, never declares a
// length shorter than the buffer, and -- in the one case that would matter --
// discards the class initialiser's return value entirely (`pop`, at IL_0019 of
// `SocketAddressPal..cctor`). So `SocketAddressRoundTrip.cs` exercises these nine
// functions without ever seeing them fail.
//
// Differential, and every row below was measured to answer identically on macOS
// arm64 and on Linux. That is not automatic here: the screens themselves are the
// shim's own C and so are flavour-free, but two of the values they read are not,
// and the rows are chosen to avoid them. `sizeof(struct sockaddr_un)` is 110 on
// Linux and 106 on Darwin, so it is checked only for being one of the two; and
// the *bytes* a blob ends up holding differ in both position and numbering, so
// nothing here reads one -- `SocketAddressLinuxBytes.cs` and its Darwin sibling
// make those claims, under PawPrint alone where the flavour is known.
//
// Three ordering rows are the point of the exercise, because each distinguishes
// two screens that a plausible implementation would collapse:
//
//   * a blob too short to hold its own family is EFAULT, while one long enough
//     for the family but too short for the struct is EFAULT only after the family
//     switch has accepted it -- so an unsupported family beats a short struct;
//   * `GetPort` answers EAFNOSUPPORT for a family it has no case for, but
//     `GetIPv4Address` answers EINVAL for the same blob, since the two are not
//     written to the same shape;
//   * a *negative* length is EFAULT even for a family whose switch arm would have
//     answered EAFNOSUPPORT, because the family bounds check runs first and the
//     cast of a negative length to `size_t` makes that check fail rather than
//     pass. That last one is measured; it is the opposite of what reading
//     `IsInBounds` suggests.
//
// The exit code is the index of the first check that failed; 0 means all passed.
// Kept below 128, since an exit code is eight bits.

class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetSocketAddressSizes")]
    static extern unsafe int GetSocketAddressSizes(int* ipv4, int* ipv6, int* uds, int* max);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetAddressFamily")]
    static extern unsafe int GetAddressFamily(byte* socketAddress, int socketAddressLen, int* addressFamily);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SetAddressFamily")]
    static extern unsafe int SetAddressFamily(byte* socketAddress, int socketAddressLen, int addressFamily);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetPort")]
    static extern unsafe int GetPort(byte* socketAddress, int socketAddressLen, ushort* port);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SetPort")]
    static extern unsafe int SetPort(byte* socketAddress, int socketAddressLen, ushort port);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetIPv4Address")]
    static extern unsafe int GetIPv4Address(byte* socketAddress, int socketAddressLen, uint* address);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SetIPv4Address")]
    static extern unsafe int SetIPv4Address(byte* socketAddress, int socketAddressLen, uint address);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetIPv6Address")]
    static extern unsafe int GetIPv6Address(
        byte* socketAddress, int socketAddressLen, byte* address, int addressLen, uint* scopeId);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SetIPv6Address")]
    static extern unsafe int SetIPv6Address(
        byte* socketAddress, int socketAddressLen, byte* address, int addressLen, uint scopeId);

    // Interop.Error values, not raw errnos: these entry points return the PAL enum
    // directly rather than -1-and-errno.
    const int PAL_SUCCESS = 0;
    const int PAL_EFAULT = 0x10015;
    const int PAL_EAFNOSUPPORT = 0x10005;
    const int PAL_EINVAL = 0x1001C;

    // PAL AddressFamily, which is not any platform's numbering.
    const int AF_UNKNOWN = -1;
    const int AF_UNIX = 1;
    const int AF_INET = 2;
    const int AF_INET6 = 23;

    // Big enough for a `sockaddr_storage` on either platform, so that a declared
    // length is always the shim's constraint rather than the buffer's.
    const int BlobSize = 128;

    static unsafe int Main(string[] args)
    {
        int ipv4 = -7, ipv6 = -7, uds = -7, max = -7;

        if (GetSocketAddressSizes(&ipv4, &ipv6, &uds, &max) != PAL_SUCCESS) return 1;
        if (ipv4 != 16) return 2;
        if (ipv6 != 28) return 3;
        if (max != 128) return 4;
        // The one of the four that takes the flavour: 110 where `sun_path` is 108
        // bytes, 106 where it is 104.
        if (uds != 110 && uds != 106) return 5;

        // Each out-parameter screened, and screened *before* any of them is
        // written -- so the three survivors keep the sentinel they went in with.
        for (int nulled = 0; nulled < 4; nulled++)
        {
            int a = -7, b = -7, c = -7, d = -7;

            int rc =
                GetSocketAddressSizes(
                    nulled == 0 ? null : &a,
                    nulled == 1 ? null : &b,
                    nulled == 2 ? null : &c,
                    nulled == 3 ? null : &d);

            if (rc != PAL_EFAULT) return 6 + nulled * 2;
            if (a != -7 || b != -7 || c != -7 || d != -7) return 7 + nulled * 2;
        }

        byte[] blob = new byte[BlobSize];

        fixed (byte* p = blob)
        {
            int family = -7;
            ushort port = 7;
            uint address = 7;

            // A blob too short to hold its own family, in either layout: one byte
            // is short on both, two bytes is exactly enough on both.
            if (SetAddressFamily(p, 1, AF_INET) != PAL_EFAULT) return 20;
            if (SetAddressFamily(p, 0, AF_INET) != PAL_EFAULT) return 21;
            if (SetAddressFamily(p, -1, AF_INET) != PAL_EFAULT) return 22;
            if (SetAddressFamily(p, 2, AF_INET) != PAL_SUCCESS) return 23;

            if (GetAddressFamily(p, 1, &family) != PAL_EFAULT) return 24;
            if (GetAddressFamily(p, -1, &family) != PAL_EFAULT) return 25;
            if (family != -7) return 26;

            if (GetAddressFamily(p, BlobSize, &family) != PAL_SUCCESS) return 27;
            if (family != AF_INET) return 28;

            // A family the shim's conversion has no case for. It still reports
            // EAFNOSUPPORT rather than refusing, and the blob is left naming a
            // family nothing recognises -- which `GetAddressFamily` then reports
            // as `AF_UNKNOWN`, and successfully.
            if (SetAddressFamily(p, BlobSize, 99) != PAL_EAFNOSUPPORT) return 29;
            if (GetAddressFamily(p, BlobSize, &family) != PAL_SUCCESS) return 30;
            if (family != AF_UNKNOWN) return 31;

            // The three ordering rows. Same blob, three different answers.
            if (GetPort(p, BlobSize, &port) != PAL_EAFNOSUPPORT) return 32;
            if (GetIPv4Address(p, BlobSize, &address) != PAL_EINVAL) return 33;
            if (GetPort(p, -1, &port) != PAL_EFAULT) return 34;
            if (port != 7 || address != 7) return 35;

            // A family that converts but that the port switch still has no case
            // for, so the EAFNOSUPPORT above was not merely the unconvertible one.
            if (SetAddressFamily(p, BlobSize, AF_UNIX) != PAL_SUCCESS) return 36;
            if (GetPort(p, BlobSize, &port) != PAL_EAFNOSUPPORT) return 37;
            if (SetPort(p, BlobSize, 1) != PAL_EAFNOSUPPORT) return 38;

            // IPv4, and the length screen that lives inside the family arm: long
            // enough for the family, one byte short of the struct.
            if (SetAddressFamily(p, BlobSize, AF_INET) != PAL_SUCCESS) return 39;
            if (GetPort(p, 15, &port) != PAL_EFAULT) return 40;
            if (SetPort(p, 15, 1) != PAL_EFAULT) return 41;
            if (GetIPv4Address(p, 15, &address) != PAL_EFAULT) return 42;
            if (SetIPv4Address(p, 15, 1) != PAL_EFAULT) return 43;
            if (port != 7 || address != 7) return 44;

            if (SetPort(p, 16, 0x1234) != PAL_SUCCESS) return 45;
            if (GetPort(p, 16, &port) != PAL_SUCCESS) return 46;
            if (port != 0x1234) return 47;

            // 1.2.3.4 as the shim sees it: network order, so the leading octet is
            // the low byte of the `uint` on a little-endian machine.
            if (SetIPv4Address(p, 16, 0x04030201) != PAL_SUCCESS) return 48;
            if (GetIPv4Address(p, 16, &address) != PAL_SUCCESS) return 49;
            if (address != 0x04030201) return 50;
            // Setting the address leaves the port alone.
            if (GetPort(p, 16, &port) != PAL_SUCCESS) return 51;
            if (port != 0x1234) return 52;

            // The v6 accessors on a v4 blob: EINVAL, like `GetIPv4Address` on the
            // wrong family and unlike the port accessors' EAFNOSUPPORT.
            byte[] v6 =
                new byte[] { 0xfe, 0x80, 0, 0, 0, 0, 0, 0, 0x02, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77 };
            byte[] readBack = new byte[16];

            fixed (byte* v6p = v6)
            fixed (byte* readBackP = readBack)
            {
                uint scopeId = 7;

                if (SetIPv6Address(p, 28, v6p, 16, 42) != PAL_EINVAL) return 53;
                if (GetIPv6Address(p, 28, readBackP, 16, &scopeId) != PAL_EINVAL) return 54;

                // An address buffer shorter than the sixteen bytes an IPv6 address
                // needs, screened before the family is even looked at.
                if (SetAddressFamily(p, BlobSize, AF_INET6) != PAL_SUCCESS) return 55;
                if (SetIPv6Address(p, 28, v6p, 15, 42) != PAL_EFAULT) return 56;
                if (GetIPv6Address(p, 28, readBackP, 15, &scopeId) != PAL_EFAULT) return 57;
                // ... and a blob one byte short of a `sockaddr_in6`.
                if (SetIPv6Address(p, 27, v6p, 16, 42) != PAL_EFAULT) return 58;
                if (GetIPv6Address(p, 27, readBackP, 16, &scopeId) != PAL_EFAULT) return 59;
                if (scopeId != 7) return 60;

                if (SetIPv6Address(p, 28, v6p, 16, 42) != PAL_SUCCESS) return 61;
                if (GetIPv6Address(p, 28, readBackP, 16, &scopeId) != PAL_SUCCESS) return 62;
                if (scopeId != 42) return 63;

                for (int i = 0; i < 16; i++)
                {
                    if (readBack[i] != v6[i]) return 64 + i;
                }

                // The port lives at the same offset in a `sockaddr_in6`, and
                // `SetIPv6Address` does not disturb it.
                if (GetPort(p, 28, &port) != PAL_SUCCESS) return 90;
                if (port != 0x1234) return 91;

                // The family survived, and is now reported as the v6 one.
                if (GetAddressFamily(p, BlobSize, &family) != PAL_SUCCESS) return 92;
                if (family != AF_INET6) return 93;

                // `SetIPv6Address` clears `sin6_flowinfo`, which nothing else in
                // this surface reads -- but it occupies the same four bytes as a
                // `sockaddr_in`'s address, so writing an IPv4 address first and
                // reading it back afterwards makes the clearing observable. The
                // family has to be moved either way for the accessors to accept
                // the blob at all, and moving it touches only the first two bytes.
                if (SetAddressFamily(p, BlobSize, AF_INET) != PAL_SUCCESS) return 94;
                if (SetIPv4Address(p, 16, 0xDEADBEEF) != PAL_SUCCESS) return 95;
                if (SetAddressFamily(p, BlobSize, AF_INET6) != PAL_SUCCESS) return 96;
                if (SetIPv6Address(p, 28, v6p, 16, 42) != PAL_SUCCESS) return 97;
                if (SetAddressFamily(p, BlobSize, AF_INET) != PAL_SUCCESS) return 98;
                if (GetIPv4Address(p, 16, &address) != PAL_SUCCESS) return 99;
                if (address != 0) return 100;
            }
        }

        return 0;
    }
}
