using System;
using System.IO;
using Microsoft.Win32.SafeHandles;

// What `SystemNative_Read` and `SystemNative_LSeek` unlock at the BCL level:
// `FileStream` over a `SafeFileHandle`, which neither the raw-P/Invoke guests
// nor `File.ReadAllBytes` reaches.
//
// The two routes here are genuinely different, and each pins one syscall:
//
//   * **A regular file.** `SafeFileHandle.Init` sets `_canSeek` from its `fstat`
//     without a syscall, so the seekability *probe* never fires — but
//     `OSFileStreamStrategy`'s handle-taking constructor issues an
//     unconditional `LSeek(handle, 0, SEEK_CUR)` to learn where the handle
//     already is (OSFileStreamStrategy.cs:27-31). Reads then go through `pread`,
//     since the handle supports random access.
//   * **A standard stream.** `new SafeFileHandle((IntPtr)0, false)` was not
//     opened by `Init`, so nothing has classified it: `CanSeek` runs the probe,
//     `LSeek` answers ESPIPE, and `RandomAccess.ReadAtOffset` therefore takes
//     its non-seekable branch and calls `SystemNative_Read`
//     (RandomAccess.Unix.cs:53). This is the *only* route by which ordinary
//     managed code reaches `Read` rather than `PRead`.
//
// The second route also asserts stdin's model: fd 0 is the read end of a pipe
// whose write end the launcher closed, so reading it is an immediate 0 rather
// than a block. `RealRuntime` starts guests exactly that way — it redirects all
// three streams and closes the child's stdin — which is what makes this
// assertable against the oracle rather than merely plausible.
//
// The exit code is the index of the first check that failed; 0 means all passed.
//
// Seed (see TestPureCases): f = "hello" (5 bytes).
class Program
{
    static int Main(string[] args)
    {
        int check;

        // --- a seekable handle ---

        using (SafeFileHandle handle = File.OpenHandle("f"))
        using (FileStream fs = new FileStream(handle, FileAccess.Read))
        {
            check = 1;
            if (!fs.CanSeek) return check;
            // The position the constructor learned by asking the kernel.
            check = 2;
            if (fs.Position != 0) return check;
            check = 3;
            if (fs.Length != 5) return check;

            byte[] buf = new byte[8];
            check = 4;
            if (fs.Read(buf, 0, 8) != 5) return check;
            check = 5;
            if (buf[0] != (byte)'h' || buf[4] != (byte)'o') return check;
            check = 6;
            if (fs.Position != 5) return check;

            check = 7;
            if (fs.Seek(1, SeekOrigin.Begin) != 1) return check;
            check = 8;
            if (fs.Read(buf, 0, 2) != 2 || buf[0] != (byte)'e' || buf[1] != (byte)'l') return check;
            check = 9;
            if (fs.Seek(-2, SeekOrigin.End) != 3) return check;
            check = 10;
            if (fs.Read(buf, 0, 8) != 2 || buf[0] != (byte)'l' || buf[1] != (byte)'o') return check;
            // At the end, a read is 0 and the position stays put.
            check = 11;
            if (fs.Read(buf, 0, 8) != 0) return check;
            check = 12;
            if (fs.Position != 5) return check;
        }

        // --- an unseekable one ---

        // ownsHandle: false, so disposing the stream does not close fd 0.
        SafeFileHandle stdin = new SafeFileHandle((IntPtr)0, false);
        using (FileStream fs = new FileStream(stdin, FileAccess.Read))
        {
            // The probe ran and answered ESPIPE.
            check = 13;
            if (fs.CanSeek) return check;

            byte[] buf = new byte[8];
            // ...so this went through `read`, not `pread`, and found end of file.
            check = 14;
            if (fs.Read(buf, 0, 8) != 0) return check;
            check = 15;
            if (fs.Read(buf, 0, 8) != 0) return check;
            // A zero-length read is fine on an unseekable stream too.
            check = 16;
            if (fs.Read(buf, 0, 0) != 0) return check;
        }

        return 0;
    }
}
