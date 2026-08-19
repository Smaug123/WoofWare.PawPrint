using System.Net.Sockets;

// `System.Net.Sockets.Socket`'s constructor, the managed consumer sitting on top of the emulated
// `SystemNative_Socket` that the sibling `SocketCreateScreens.cs` reaches by hand-rolled P/Invoke.
//
// It is here because it is what motivated allowlisting `RuntimeHelpers.EnumEquals`: `SocketErrorPal`
// keys its errno table on the enum `Interop.Error`, so a `Dictionary` operation on it asks
// `EqualityComparer<Interop.Error>.Default`, whose `EnumEqualityComparer<T>.Equals` is that
// [Intrinsic]'s only caller in this path. Measured rather than assumed: with the allowlist entry
// removed this guest stops at "TODO: implement JIT intrinsic ... EnumEquals(Error, Error)".
//
// The property reads are the point, not the constructor returning: they pin that the descriptor the
// interpreter created is the one the triple asked for, and that its state is the *unbound* one, so a
// handle fabricated without a live descriptor behind it would not pass. `Dispose` closes it, which
// is the only part of the descriptor's lifetime a guest can drive from here.

class Program
{
    static int Main(string[] args)
    {
        Socket s = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        if (s.Handle == System.IntPtr.Zero) return 3;
        if (s.AddressFamily != AddressFamily.InterNetwork) return 4;
        if (s.SocketType != SocketType.Stream) return 5;
        if (s.ProtocolType != ProtocolType.Tcp) return 6;
        if (s.Connected) return 7;
        if (s.IsBound) return 8;
        if (s.LocalEndPoint != null) return 9;
        if (!s.Blocking) return 10;
        s.Dispose();
        return 0;
    }
}
