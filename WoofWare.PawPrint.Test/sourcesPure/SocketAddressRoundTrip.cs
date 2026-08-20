using System.Net;
using System.Net.Sockets;

// `System.Net.SocketAddress`, which is the whole of `SocketAddressPal`: the nine
// `libSystem.Native` entry points that lay a `struct sockaddr_in` or
// `sockaddr_in6` out in a buffer the guest owns, and read one back. No socket is
// created here, and none is needed -- not one of the nine touches a descriptor,
// an errno or any kernel state, so this exercises the entire layer without
// `SystemNative_Bind` or anything downstream of it existing.
//
// Differential, and therefore restricted to the claims that hold whichever Unix
// laid the bytes out. The blob itself is *not* such a claim: a `sockaddr`'s
// family field is two bytes at offset 0 on Linux and one byte at offset 1 on
// Darwin (byte 0 being `sa_len`), and `AF_INET6` is 10 against 30. PawPrint
// emulates Linux while the oracle here runs on the host, so asserting bytes
// would compare two platforms rather than two runtimes. `SocketAddressLinuxBytes.cs`
// makes the byte-level claim, under PawPrint alone where the flavour is known.
//
// What survives that restriction is still enough to pin every entry point:
// `Size` and `Family` are the PAL's own answers rather than the platform's, and
// a round trip runs each setter and its getter in turn, so a getter that
// disagreed with its setter about width, offset or byte order fails here even
// though the bytes between them are unstated.
//
// The values are chosen so that a wrong answer inverts rather than merely
// perturbs. The port's two bytes differ (0x1234), so a `sin_port` written in host
// order rather than network order round-trips to 0x3412 instead. The address's
// four bytes are distinct and not a palindrome (1.2.3.4), so an `in_addr` copied
// backwards is visible. The scope id is 42 rather than 0, so a `sin6_scope_id`
// dropped on the floor is visible -- and it is the one field of the six that is
// *not* in network order, so writing it byte-swapped like the port is caught too.
//
// The exit code is the index of the first check that failed; 0 means all passed.

class Program
{
    // 0 if the endpoint survives a serialise/deserialise round trip with the
    // expected blob size, else `baseCode` plus which of the four claims failed.
    static int RoundTrip(IPEndPoint endpoint, int expectedSize, AddressFamily expectedFamily, int baseCode)
    {
        SocketAddress serialized = endpoint.Serialize();

        if (serialized.Size != expectedSize) return baseCode;
        if (serialized.Family != expectedFamily) return baseCode + 1;

        IPEndPoint restored = (IPEndPoint) endpoint.Create(serialized);

        if (restored.Port != endpoint.Port) return baseCode + 2;
        if (!restored.Address.Equals(endpoint.Address)) return baseCode + 3;

        return 0;
    }

    static int Main(string[] args)
    {
        int r;

        // IPv4. `sockaddr_in` is 16 bytes on both platforms.
        r = RoundTrip(new IPEndPoint(IPAddress.Parse("1.2.3.4"), 0x1234), 16, AddressFamily.InterNetwork, 10);
        if (r != 0) return r;

        // The two ends of the port range, which a signed 16-bit read or a
        // sign-extending one would get wrong at 65535 specifically.
        r = RoundTrip(new IPEndPoint(IPAddress.Parse("1.2.3.4"), 0), 16, AddressFamily.InterNetwork, 20);
        if (r != 0) return r;

        r = RoundTrip(new IPEndPoint(IPAddress.Parse("1.2.3.4"), 65535), 16, AddressFamily.InterNetwork, 30);
        if (r != 0) return r;

        // The loopback address the socket rungs actually bind to.
        r = RoundTrip(new IPEndPoint(IPAddress.Loopback, 0), 16, AddressFamily.InterNetwork, 40);
        if (r != 0) return r;

        // An address whose high bit is set in every byte, so that a `sin_addr`
        // moved through a sign-extending path rather than copied is caught.
        r = RoundTrip(new IPEndPoint(IPAddress.Parse("200.201.202.203"), 0x1234), 16, AddressFamily.InterNetwork, 50);
        if (r != 0) return r;

        // IPv6. `sockaddr_in6` is 28 bytes on both platforms.
        IPAddress linkLocal =
            new IPAddress(
                new byte[] { 0xfe, 0x80, 0, 0, 0, 0, 0, 0, 0x02, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77 },
                42);

        r = RoundTrip(new IPEndPoint(linkLocal, 0xABCD), 28, AddressFamily.InterNetworkV6, 60);
        if (r != 0) return r;

        // `IPAddress.Equals` ignores the scope id, so the round trip above would
        // pass with `sin6_scope_id` dropped entirely. Assert it separately.
        SocketAddress scoped = new IPEndPoint(linkLocal, 0xABCD).Serialize();
        IPEndPoint scopedBack = (IPEndPoint) new IPEndPoint(linkLocal, 0xABCD).Create(scoped);
        if (scopedBack.Address.ScopeId != 42) return 70;

        r = RoundTrip(new IPEndPoint(IPAddress.IPv6Loopback, 0), 28, AddressFamily.InterNetworkV6, 80);
        if (r != 0) return r;

        r = RoundTrip(new IPEndPoint(IPAddress.IPv6Any, 65535), 28, AddressFamily.InterNetworkV6, 90);
        if (r != 0) return r;

        // A freshly constructed `SocketAddress` runs `SetAddressFamily` alone,
        // with no port or address written after it -- the one call shape where a
        // handler that wrote the family at the wrong width or offset is not
        // covered up by a later write to the neighbouring bytes.
        SocketAddress bare = new SocketAddress(AddressFamily.InterNetwork);
        if (bare.Family != AddressFamily.InterNetwork) return 100;

        SocketAddress bare6 = new SocketAddress(AddressFamily.InterNetworkV6);
        if (bare6.Family != AddressFamily.InterNetworkV6) return 101;

        return 0;
    }
}
