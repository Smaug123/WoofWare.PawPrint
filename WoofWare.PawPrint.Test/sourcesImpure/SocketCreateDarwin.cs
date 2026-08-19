using System;
using System.Runtime.InteropServices;

// The `SystemNative_Socket` rows Darwin answers differently, which are exactly
// the two address families the shim's own `#ifdef`s compile out there.
//
// `AF_PACKET` and `AF_CAN` have cases in `TryConvertAddressFamilyPalToPlatform`
// (pal_networking.c:239, :245) guarded on symbols Linux's headers define and
// Darwin's do not, so on Darwin they fall through to the default arm and the
// wrapper answers EAFNOSUPPORT without reaching any kernel. That is a property
// of which shim was built rather than of the running machine, which is why
// PawPrint can state it from the configured flavour alone.
//
// Not differential, and not merely because the answer differs: under the Linux
// flavour these families *convert*, and then reach a socket PawPrint does not
// model, so the Linux sibling cannot assert anything about them at all.
// Configured as macOS for the same reason `SocketEventPortDarwin.cs` is.
//
// The exit code is the index of the first check that failed; 0 means all passed.
// Kept below 128, since an exit code is eight bits.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Socket")]
    static extern unsafe int Socket(int addressFamily, int socketType, int protocolType, IntPtr* createdSocket);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close")]
    static extern int Close(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_LSeek", SetLastError = true)]
    static extern long LSeek(IntPtr fd, long offset, int whence);

    const int PAL_SUCCESS = 0;
    const int PAL_EAFNOSUPPORT = 0x10005;

    const int AF_INET = 2;
    const int AF_PACKET = 65536;
    const int AF_CAN = 65537;

    const int SOCK_STREAM = 1;
    const int SOCK_DGRAM = 2;
    const int SOCK_RAW = 3;

    const int PT_UNSPECIFIED = 0;
    const int PT_TCP = 6;
    const int PT_RAW = 255;

    // Darwin numbering, which is what PawPrint reports under this flavour.
    const int ESPIPE = 29;

    static unsafe bool Refuses(int addressFamily, int socketType, int protocolType)
    {
        IntPtr created = (IntPtr)0x5EED;
        int result = Socket(addressFamily, socketType, protocolType, &created);
        return result == PAL_EAFNOSUPPORT && created == (IntPtr)(-1);
    }

    static unsafe int Main()
    {
        // `AF_PACKET`, with the protocol arm that *would* accept anything under
        // Linux -- so what is being observed is the address-family screen and not
        // the protocol one.
        if (!Refuses(AF_PACKET, SOCK_DGRAM, PT_UNSPECIFIED))
            return 1;

        if (!Refuses(AF_PACKET, SOCK_RAW, PT_TCP))
            return 2;

        // `AF_CAN`, likewise with a protocol its Linux arm accepts.
        if (!Refuses(AF_CAN, SOCK_RAW, PT_RAW))
            return 3;

        if (!Refuses(AF_CAN, SOCK_DGRAM, PT_UNSPECIFIED))
            return 4;

        // The families that convert on both platforms still do, so the screen
        // above is about these two families rather than about the whole table.
        IntPtr sock = (IntPtr)0x5EED;
        if (Socket(AF_INET, SOCK_STREAM, PT_TCP, &sock) != PAL_SUCCESS || (long)sock != 3)
            return 5;

        // A socket is unseekable, and Darwin decides that *before* looking at the
        // whence: an invalid whence still gets ESPIPE, where Linux answers
        // EINVAL. The valid whences are ESPIPE on both, and are asserted
        // differentially in SocketCreateScreens.cs.
        Marshal.SetLastSystemError(0);
        if (LSeek(sock, 0, 9) != -1 || Marshal.GetLastSystemError() != ESPIPE)
            return 6;

        if (Close(sock) != 0)
            return 7;

        return 0;
    }
}
