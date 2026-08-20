using System;
using System.Net;
using System.Net.Sockets;

// The bytes `SocketAddressPal` lays down, under the Darwin flavour. The
// sibling `SocketAddressRoundTrip.cs` is the differential test and deliberately
// says nothing about the blob: a `sockaddr`'s family field is two bytes at offset
// 0 on Linux and one byte at offset 1 on Darwin -- byte 0 being `sa_len` -- and
// `AF_INET6` is 10 against 30, so no claim about these bytes holds on both. This
// guest makes that claim for one flavour, which is why it is impure.
//
// A round trip cannot substitute. It is self-consistent: a setter and getter that
// agreed with each other on a wrong offset, a wrong width or a swapped byte order
// would pass it. These rows are what pins the layout to the platform's own.
//
// Every row below was *measured*, not derived -- this same guest's values printed
// by real .NET, on macOS for the Darwin rows and on a Linux container for the
// Linux ones -- because an impure case's expectation is our claim rather than an
// oracle's answer.
//
// Two rows are about managed code rather than the shim, and are here because they
// are what makes the family field's width observable at all.
// `SocketAddress..ctor` writes `_buffer[0] = (byte) _size` before calling
// `SetAddressFamily`, unconditionally on every platform -- it is speculatively
// filling in BSD's `sa_len`. On Darwin the one-byte family lands at offset 1 and
// leaves it standing; on Linux the two-byte family overwrites it. So a Linux
// handler that wrote only one byte would leave the size behind at offset 1, and
// `AddressFamily.Unknown` -- which `SocketAddressPal.SetAddressFamily` screens out
// before reaching the shim at all -- shows the same byte surviving with no native
// call involved.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.

class Program
{
    // 0 if `actual` is exactly `expected`, else `baseCode` (wrong length) or
    // `baseCode + 1 + index` naming the first byte that differs.
    static int Expect(byte[] actual, byte[] expected, int baseCode)
    {
        if (actual.Length != expected.Length) return baseCode;

        for (int i = 0; i < expected.Length; i++)
        {
            if (actual[i] != expected[i]) return baseCode + 1 + i;
        }

        return 0;
    }

    static byte[] Blob(SocketAddress address)
    {
        byte[] copy = new byte[address.Size];

        for (int i = 0; i < copy.Length; i++)
        {
            copy[i] = address[i];
        }

        return copy;
    }

    static int Main(string[] args)
    {
        int r;

        IPAddress linkLocal =
            new IPAddress(
                new byte[] { 0xfe, 0x80, 0, 0, 0, 0, 0, 0, 0x02, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77 },
                42);

        // `struct sockaddr_in` for 1.2.3.4:0x1234. The port is at offset 2 in
        // network order, so it reads 12-34 rather than 34-12; the address follows
        // at offset 4 in its own order; `sin_zero` fills the last eight bytes.
        r = Expect(Blob(new IPEndPoint(IPAddress.Parse("1.2.3.4"), 0x1234).Serialize()),
                   new byte[] { 0x10, 0x02, 0x12, 0x34, 0x01, 0x02, 0x03, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 }, 10);
        if (r != 0) return r;

        // `struct sockaddr_in6` for [fe80::211:2233:4455:6677%42]:0xABCD.
        // `sin6_flowinfo` at offset 4 is left zero -- nothing in the managed
        // surface sets it -- and `sin6_scope_id` at offset 24 is the one field
        // that is *not* in network order, so 42 reads 2A-00-00-00.
        r = Expect(Blob(new IPEndPoint(linkLocal, 0xABCD).Serialize()),
                   new byte[] { 0x1C, 0x1E, 0xAB, 0xCD, 0x00, 0x00, 0x00, 0x00, 0xFE, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x2A, 0x00, 0x00, 0x00 }, 30);
        if (r != 0) return r;

        // `SetAddressFamily` with nothing written after it. See the note above on
        // `_buffer[0]`: this is the row that catches a family written at the wrong
        // width, since there is no later write to cover the leftover byte.
        r = Expect(Blob(new SocketAddress(AddressFamily.InterNetwork)),
                   new byte[] { 0x10, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 }, 60);
        if (r != 0) return r;

        r = Expect(Blob(new SocketAddress(AddressFamily.InterNetworkV6)),
                   new byte[] { 0x1C, 0x1E, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 }, 78);
        if (r != 0) return r;

        // The two sizes the class initialiser reports that no endpoint above
        // exercises: `sizeof(struct sockaddr_un)`, the one of the four that
        // differs between the platforms, and `sizeof(struct sockaddr_storage)`,
        // which `SocketAddress.GetMaximumAddressSize` falls back to.
        SocketAddress uds = new SocketAddress(AddressFamily.Unix);
        if (uds.Size != 106) return 106;
        if (uds[0] != 0x6A || uds[1] != 0x01) return 107;

        SocketAddress unspecified = new SocketAddress(AddressFamily.Unspecified);
        if (unspecified.Size != 128) return 108;
        if (unspecified[0] != 0x80 || unspecified[1] != 0) return 109;

        // `AddressFamily.Unknown` is -1, which `SocketAddressPal.SetAddressFamily`
        // returns on without calling the shim. So byte 0 keeps the `_buffer[0]`
        // the constructor wrote, on both platforms -- a handler that answered here
        // regardless would be visible as a changed byte.
        SocketAddress unknown = new SocketAddress(AddressFamily.Unknown);
        if (unknown.Size != 128) return 110;
        if (unknown[0] != 128) return 111;

        return 0;
    }
}
