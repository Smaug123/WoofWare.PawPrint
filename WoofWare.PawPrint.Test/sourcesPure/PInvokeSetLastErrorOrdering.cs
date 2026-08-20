using System;
using System.Runtime.InteropServices;

// *Where* in the P/Invoke stub the errno clear sits. The sibling
// PInvokeSetLastError.cs shows that a flagged import clears errno and captures
// it, but every row there would pass equally well if the clear ran *after* the
// call instead of before it -- each of those rows either zeroes errno itself
// first or ends with the native having written it last.
//
// These two entry points are what tell the orders apart, because errno is not
// incidental to them but their entire contract (pal_errno.c:21-29, and both are
// exported: entrypoints.c:269-270):
//
//     int32_t SystemNative_GetErrNo(void)            { return errno; }
//     void    SystemNative_SetErrNo(int32_t code)    { errno = code; }
//
// So with the clear *before* the call:
//
//   - a flagged `GetErrNo()` reports 0 whatever errno held on the way in,
//     because the stub zeroed it before the native looked;
//   - a flagged `SetErrNo(x)` leaves both slots at x, because the native wrote
//     errno after the clear and the stub then captured that.
//
// With the clear after the call, both answers invert: `GetErrNo()` would
// report the incoming errno, and `SetErrNo(x)` would leave both slots at 0.
//
// Each entry point is declared twice; the unflagged twin is the control that
// shows what the native does on its own.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetErrNo", SetLastError = true)]
    static extern int GetErrNoFlagged();

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetErrNo")]
    static extern int GetErrNoUnflagged();

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SetErrNo", SetLastError = true)]
    static extern void SetErrNoFlagged(int errorCode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_SetErrNo")]
    static extern void SetErrNoUnflagged(int errorCode);

    // Raw errnos, and portable ones: these two name the same error on every Unix.
    const int EBADF = 9;
    const int EINVAL = 22;

    const int PinvSentinel = 777;

    static int Main()
    {
        int check;

        // Warm every import before measuring. Symbol resolution happens inside
        // the stub, between the clear and the target, so an unwarmed row that
        // asserts "errno is 0 here" could be reporting the loader rather than
        // the clear. Nothing here is asserted.
        SetErrNoUnflagged(0);
        SetErrNoFlagged(0);
        GetErrNoUnflagged();
        GetErrNoFlagged();

        // ---- Control: unflagged, the native simply reads errno back.
        SetErrNoUnflagged(EBADF);
        check = 1;
        if (GetErrNoUnflagged() != EBADF) return check;
        check = 2;
        if (Marshal.GetLastSystemError() != EBADF) return check;

        // ---- Flagged read: 0, because the stub cleared errno before the native
        // read it. This is the row that fails if the clear moves after the call.
        SetErrNoUnflagged(EBADF);
        Marshal.SetLastPInvokeError(PinvSentinel);
        int flaggedRead = GetErrNoFlagged();
        int afterSys = Marshal.GetLastSystemError();
        int afterPinv = Marshal.GetLastPInvokeError();
        check = 3;
        if (flaggedRead != 0) return check;
        // The native left errno alone, so what the clear wrote is still there...
        check = 4;
        if (afterSys != 0) return check;
        // ...and that is what the capture copied over the sentinel.
        check = 5;
        if (afterPinv != 0) return check;

        // ---- Flagged write: the native's write survives, because it happened
        // after the clear, and the capture reports it. Under the other order the
        // clear would erase what the native had just written and both slots
        // would read 0.
        SetErrNoUnflagged(EBADF);
        Marshal.SetLastPInvokeError(PinvSentinel);
        SetErrNoFlagged(EINVAL);
        check = 6;
        if (Marshal.GetLastSystemError() != EINVAL) return check;
        check = 7;
        if (Marshal.GetLastPInvokeError() != EINVAL) return check;

        // ---- Control: the same write unflagged reaches errno and stops there.
        SetErrNoUnflagged(EBADF);
        Marshal.SetLastPInvokeError(PinvSentinel);
        SetErrNoUnflagged(EINVAL);
        check = 8;
        if (Marshal.GetLastSystemError() != EINVAL) return check;
        check = 9;
        if (Marshal.GetLastPInvokeError() != PinvSentinel) return check;

        return 0;
    }
}
