using System;
using System.Runtime.InteropServices;

// The forward P/Invoke stub CoreCLR emits for an import declaring
// `SetLastError = true`. `NDirectStubLinker::DoNDirect` (dllimport.cpp:706-719)
// wraps the call in three steps:
//
//     errno = 0                     // StubHelpers.ClearLastError
//     <call>
//     t_lastPInvokeError = errno    // StubHelpers.SetLastError
//
// On Unix the PAL's last-error *is* errno: `CPalThread::SetLastError` is
// literally `errno = dwLastError`, under the comment "Reuse errno to store last
// error" (pal/src/include/pal/thread.hpp:416-431). So
// `Marshal.Get/SetLastSystemError` address the same slot the syscall does,
// while `Marshal.Get/SetLastPInvokeError` address the separate thread-local
// (`t_lastPInvokeError`, marshalnative.cpp:311-319) that the stub copies into
// and that CoreLib's own `Interop.Sys.GetLastErrorInfo` reads.
//
// Each entry point is declared twice, once flagged and once not. The unflagged
// twin is the control: it shows what the *native* did to the two slots, which
// is what makes every difference below attributable to the stub rather than to
// the call.
//
// Measured on the pinned runtime (macOS, 10.0.7) rather than read off the
// source:
//
//   import      outcome   errno after       GetLastPInvokeError after
//   flagged     fail      9 (EBADF)         9    <- the stub captured it
//   unflagged   fail      9 (EBADF)         0    <- untouched (control)
//   flagged     success   0                 0    <- cleared, then captured
//   unflagged   success   4242 (preserved)  777  <- untouched (control)
//
// The third row is the whole evidence for the *pre*-call clear: errno was 4242
// going in, the call succeeded and touched errno not at all, and errno reads 0
// coming out. The fourth row is what rules out the native having done that.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_LSeek", SetLastError = true)]
    static extern long LSeekFlagged(IntPtr fd, long offset, int whence);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_LSeek")]
    static extern long LSeekUnflagged(IntPtr fd, long offset, int whence);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Dup", SetLastError = true)]
    static extern IntPtr DupFlagged(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Dup")]
    static extern IntPtr DupUnflagged(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close")]
    static extern int Close(IntPtr fd);

    const int EBADF = 9;
    const int SeekSet = 0;

    // Sentinels distinct from each other and from every errno a row produces, so
    // "preserved" and "overwritten" are never coincidentally the same number.
    const int SysSentinel = 4242;
    const int PinvSentinel = 777;

    // A descriptor the process never opened.
    static readonly IntPtr BadFd = new IntPtr(12345);
    static readonly IntPtr Stdout = new IntPtr(1);

    static int Main()
    {
        int check;

        // ---- Warm every import before measuring anything.
        //
        // The first call through a `DllImport` resolves the symbol, and that
        // resolution happens *inside* the stub -- after `ClearLastError` and
        // before the target runs. A row whose native leaves errno alone would
        // otherwise be reporting whatever the loader left behind rather than
        // the stub's clear. Nothing here is asserted; the calls exist only to
        // move that work out of the measured region.
        LSeekFlagged(BadFd, 0, SeekSet);
        LSeekUnflagged(BadFd, 0, SeekSet);
        IntPtr warmA = DupFlagged(Stdout);
        IntPtr warmB = DupUnflagged(Stdout);
        Close(warmA);
        Close(warmB);

        // ---- Flagged + failure: the stub copies errno into the P/Invoke slot.
        Marshal.SetLastSystemError(0);
        Marshal.SetLastPInvokeError(0);
        long aRet = LSeekFlagged(BadFd, 0, SeekSet);
        int aSys = Marshal.GetLastSystemError();
        int aPinv = Marshal.GetLastPInvokeError();
        check = 1;
        if (aRet != -1) return check;
        check = 2;
        if (aSys != EBADF) return check;
        check = 3;
        if (aPinv != EBADF) return check;

        // ---- Control: the same failure unflagged leaves the P/Invoke slot alone.
        Marshal.SetLastSystemError(0);
        Marshal.SetLastPInvokeError(0);
        long bRet = LSeekUnflagged(BadFd, 0, SeekSet);
        int bSys = Marshal.GetLastSystemError();
        int bPinv = Marshal.GetLastPInvokeError();
        check = 4;
        if (bRet != -1) return check;
        check = 5;
        if (bSys != EBADF) return check;
        check = 6;
        if (bPinv != 0) return check;

        // ---- Flagged + success: the pre-call clear is visible in *both* slots.
        // `dup(2)` succeeds and touches neither, so the zeros can only be the
        // stub's -- it cleared errno going in and captured that zero coming out.
        Marshal.SetLastSystemError(SysSentinel);
        Marshal.SetLastPInvokeError(PinvSentinel);
        IntPtr cRet = DupFlagged(Stdout);
        int cSys = Marshal.GetLastSystemError();
        int cPinv = Marshal.GetLastPInvokeError();
        check = 7;
        if (cRet == new IntPtr(-1)) return check;
        check = 8;
        if (cSys != 0) return check;
        check = 9;
        if (cPinv != 0) return check;
        Close(cRet);

        // ---- Control: unflagged, nothing is touched at all.
        Marshal.SetLastSystemError(SysSentinel);
        Marshal.SetLastPInvokeError(PinvSentinel);
        IntPtr dRet = DupUnflagged(Stdout);
        int dSys = Marshal.GetLastSystemError();
        int dPinv = Marshal.GetLastPInvokeError();
        check = 10;
        if (dRet == new IntPtr(-1)) return check;
        check = 11;
        if (dSys != SysSentinel) return check;
        check = 12;
        if (dPinv != PinvSentinel) return check;
        Close(dRet);

        return 0;
    }
}
