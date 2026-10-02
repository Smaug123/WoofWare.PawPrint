using System;
using System.ComponentModel;
using System.IO;
using System.Runtime.InteropServices;

// The text System.Native's `SystemNative_StrErrorR` answers under the
// Darwin flavour, whose C library is Darwin's libc. The text is the C
// library's and differs between the flavours, so this pair exists rather than
// a differential `sourcesPure` guest: each is compared against real .NET only
// on a host of its own flavour.
//
// Reached three ways: `Marshal.GetPInvokeErrorMessage` and `Win32Exception`,
// which are CoreLib's `Interop.Sys.StrError` with its 1024-byte buffer; the
// inner `IOException` of an `UnauthorizedAccessException`, which is
// `Interop.GetExceptionForIoErrno` building its message after
// `SystemNative_ConvertErrorPalToPlatform`; and a hand-rolled import, for the
// buffer sizes and returned pointers CoreLib never shows. The rows were
// measured by docs/plans/2026-08-23-posix-kernel-extraction/strerror-r.c.
//
// The exit code is the index of the first check that failed; 0 means all
// passed.
class StrErrorDarwin
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_StrErrorR")]
    static extern unsafe byte* StrErrorR(int platformErrno, byte* buffer, int bufferSize);

    static int check;

    static bool Fails(bool ok)
    {
        check++;
        return !ok;
    }

    static unsafe string Read(byte* text) => Marshal.PtrToStringUTF8((IntPtr)text);

    static unsafe int Main(string[] args)
    {
        // Through CoreLib, as every exception message is built.
        if (Fails(Marshal.GetPInvokeErrorMessage(0) == "Undefined error: 0")) return check;
        if (Fails(Marshal.GetPInvokeErrorMessage(1) == "Operation not permitted")) return check;
        if (Fails(Marshal.GetPInvokeErrorMessage(2) == "No such file or directory")) return check;
        // EBUSY is 16 on both, but its words are not.
        if (Fails(Marshal.GetPInvokeErrorMessage(16) == "Resource busy")) return check;
        if (Fails(Marshal.GetPInvokeErrorMessage(11) == "Resource deadlock avoided")) return check;
        if (Fails(Marshal.GetPInvokeErrorMessage(35) == "Resource temporarily unavailable")) return check;
        if (Fails(Marshal.GetPInvokeErrorMessage(39) == "Destination address required")) return check;
        if (Fails(Marshal.GetPInvokeErrorMessage(40) == "Message too long")) return check;
        if (Fails(Marshal.GetPInvokeErrorMessage(45) == "Operation not supported")) return check;
        if (Fails(Marshal.GetPInvokeErrorMessage(102) == "Operation not supported on socket")) return check;
        if (Fails(Marshal.GetPInvokeErrorMessage(107) == "Capabilities insufficient")) return check;
        if (Fails(Marshal.GetPInvokeErrorMessage(108) == "Unknown error: 108")) return check;
        if (Fails(Marshal.GetPInvokeErrorMessage(4096) == "Unknown error: 4096")) return check;
        if (Fails(Marshal.GetPInvokeErrorMessage(-1) == "Unknown error: -1")) return check;
        if (Fails(Marshal.GetPInvokeErrorMessage(int.MinValue) == "Unknown error: -2147483648")) return check;
        if (Fails(Marshal.GetPInvokeErrorMessage(-0x20001) == "nodename nor servname provided, or not known")) return check;
        if (Fails(Marshal.GetPInvokeErrorMessage(-0x20002) == "Unknown socket error")) return check;
        if (Fails(new Win32Exception(2).Message == "No such file or directory")) return check;

        // A directory as File.Copy's source: EACCES, reported as
        // UnauthorizedAccessException with the errno's text inside.
        Directory.CreateDirectory("d");
        try
        {
            File.Copy("d", "dcopy");
            if (Fails(false)) return check;
        }
        catch (UnauthorizedAccessException e)
        {
            if (Fails(e.InnerException is IOException inner && inner.Message == "Permission denied" && inner.HResult == 13)) return check;
        }

        // Through the shim directly, at sizes CoreLib never passes. The
        // buffer is filled first, so what the call left in it shows.
        byte* buffer = stackalloc byte[64];
        for (int i = 0; i < 64; i++) buffer[i] = 0xAA;

        // A negative size is NULL, before anything is written.
        if (Fails(StrErrorR(2, buffer, -1) == null && buffer[0] == 0xAA)) return check;

        // Darwin's libc writes every text into the buffer, and answers NULL
        // for a named error whose text did not fit.
        if (Fails(StrErrorR(2, buffer, 8) == null && Read(buffer) == "No such" && buffer[8] == 0xAA)) return check;
        if (Fails(StrErrorR(2, buffer, 26) == buffer && Read(buffer) == "No such file or directory")) return check;

        // A number it names nothing for is the buffer, fitting or not.
        if (Fails(StrErrorR(108, buffer, 8) == buffer && Read(buffer) == "Unknown")) return check;

        // Size 0: nothing written, and NULL for a named error.
        for (int i = 0; i < 64; i++) buffer[i] = 0xAA;
        if (Fails(StrErrorR(2, buffer, 0) == null && buffer[0] == 0xAA)) return check;
        if (Fails(StrErrorR(108, buffer, 0) == buffer && buffer[0] == 0xAA)) return check;

        // The shim's own pseudo-errno, cut the same way.
        if (Fails(StrErrorR(-0x20001, buffer, 5) == buffer && Read(buffer) == "node")) return check;
        return 0;
    }
}
