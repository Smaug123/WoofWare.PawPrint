using System;
using System.Net;
using System.Net.Sockets;

// `Socket.Poll` and `Socket.Select`, which reach `SystemNative_Poll`.
//
// Only rows the two kernels *agree* on: this file is differential against real
// .NET on the host, and PawPrint's emulated kernel is Linux-flavoured whatever
// the host is, so a macOS dev box compares Linux answers against Darwin ones.
// Poll diverges on most of its interesting rows — an idle TCP socket presents
// OUT|HUP on Linux and nothing at all on Darwin, a half-closed connection
// IN|OUT against IN|PRI|HUP — and every one of those lives in
// `sourcesImpure/SocketPollLinux.cs` instead. The agreement of what is left was
// measured, not reasoned: this guest exits 0 on real .NET on both kernels.
//
// Two things here are load-bearing and must survive any edit:
//
//  * **Every row that expects `false` polls with timeout 0.** PawPrint models a
//    blocking poll only when something is already ready; a not-ready entry at a
//    positive timeout is a refusal, so such a row would abort rather than
//    answer. `Socket.Poll` divides microseconds by 1000, so 500µs is also a
//    0ms timeout — which is why check 9 is safe and check 10 is the deliberate
//    exception.
//  * **The connect is blocking.** A non-blocking one races the SYN, and the
//    listener's read-readiness row would flake on the real-runtime side.
//
// The exit code is the index of the first check that failed; 0 means all
// passed.
class SocketPoll
{
    static int Main()
    {
        using var listener = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        listener.Bind(new IPEndPoint(IPAddress.Loopback, 0));
        listener.Listen(4);
        var port = ((IPEndPoint)listener.LocalEndPoint).Port;

        // A listener's read-readiness is exactly "the accept queue is nonempty",
        // which is the one socket row both kernels agree on in all three states.
        if (listener.Poll(0, SelectMode.SelectRead)) return 1;

        using var client = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        client.Connect(new IPEndPoint(IPAddress.Loopback, port));

        if (!listener.Poll(0, SelectMode.SelectRead)) return 2;

        using var accepted = listener.Accept();

        if (listener.Poll(0, SelectMode.SelectRead)) return 3;

        // Both ends of an established connection are write-ready, and neither is
        // read-ready with nothing sent.
        if (!client.Poll(0, SelectMode.SelectWrite)) return 4;
        if (!accepted.Poll(0, SelectMode.SelectWrite)) return 5;
        if (client.Poll(0, SelectMode.SelectRead)) return 6;
        if (accepted.Poll(0, SelectMode.SelectRead)) return 7;

        // An idle datagram socket is write-ready on both kernels. Its *stream*
        // counterpart is not — that row diverges and lives in the impure guest.
        using var udp = new Socket(AddressFamily.InterNetwork, SocketType.Dgram, ProtocolType.Udp);
        if (!udp.Poll(0, SelectMode.SelectWrite)) return 8;

        // Sub-millisecond timeouts truncate to 0, so this is still the
        // non-blocking path despite the argument.
        if (!client.Poll(500, SelectMode.SelectWrite)) return 9;

        // The one genuinely positive timeout, and the only check that
        // distinguishes "poll answers whenever something is ready" from "poll
        // answers only at timeout 0". A ready descriptor returns immediately at
        // any timeout, so this must not wait 100ms.
        if (!client.Poll(100_000, SelectMode.SelectWrite)) return 10;

        // No `Socket.Select` here, deliberately: which PAL entry point it
        // reaches is a CoreLib *flavour* fact, not a kernel one.
        // `SocketPal.Select` branches on `SelectOverPollIsBroken`, which is
        // `OperatingSystem.IsMacOS() || IsIOS() || IsTvOS() || IsMacCatalyst()`,
        // and `IsMacOS()` is `#if TARGET_OSX` in CoreLib. So a macOS-flavour
        // image routes Select to `SystemNative_Select` — a different entry
        // point, and not implemented — while a Linux-flavour image routes it to
        // `SelectViaPoll` and lands here. A row using Select would pass in CI
        // and fail on a macOS dev box, for a reason that has nothing to do with
        // what it was testing. `sourcesImpure/SocketPollLinux.cs` covers the
        // multi-entry array path by calling the entry point directly, where no
        // such branch exists.
        return 0;
    }
}
