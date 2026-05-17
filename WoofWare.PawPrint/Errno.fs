namespace WoofWare.PawPrint

/// Raw Unix kernel errno values, as defined in `<errno.h>` on every platform
/// PawPrint models (Linux, Darwin, BSD). These are the values that land in
/// `EmulatedKernel.LastSystemError` when a modelled `SystemNative_*` shim
/// fails — i.e. exactly what `Marshal.GetLastSystemError` / the host's
/// `errno` would return on the real syscall.
///
/// Note that this is *not* the same as the BCL's `Interop.Error` enum, whose
/// values (e.g. `Interop.Error.EBADF = 0x10008`) are deliberately chosen
/// outside the typical errno range. The BCL converts raw → PAL by calling
/// `SystemNative_ConvertErrorPlatformToPal` before switching on the enum;
/// guest code that reaches `LastSystemError` directly (via
/// `Marshal.GetLastSystemError` or `Marshal.GetLastPInvokeError`) sees the
/// raw kernel value stored here.
[<RequireQualifiedAccess>]
module Errno =
    /// `EBADF` — Bad file descriptor. Returned by `dup`, `close`, `read`,
    /// `write`, etc. when the supplied fd is not currently open.
    let EBADF : int = 9

    /// `EFAULT` — Bad address. Returned by `read(2)` / `write(2)` and friends
    /// when the buffer pointer is outside the process's accessible address
    /// space — most notably when the caller passes `NULL` (or any other
    /// non-dereferenceable bit pattern) with a non-zero `bufferSize`. The
    /// kernel performs no I/O for such a call. PawPrint maps a null /
    /// non-managed buffer to this errno rather than crashing the
    /// interpreter, so guests that issue a direct P/Invoke (skipping the
    /// BCL's null-guard in `Stream.Write`) see the same error as on the
    /// real runtime.
    let EFAULT : int = 14

    /// `EINVAL` — Invalid argument. Returned by `sigaction(2)` when the
    /// caller asks to install a handler for an uncatchable signal
    /// (`SIGKILL` or `SIGSTOP`). PawPrint surfaces this through
    /// `SystemNative_EnablePosixSignalHandling`, which the BCL's
    /// `PosixSignalRegistration.Create` reads via
    /// `Marshal.GetLastSystemError` to throw a meaningful error to the
    /// guest. Value matches Linux and Darwin (both define it as 22).
    let EINVAL : int = 22

    /// `ERANGE` — Result too large / out of range. Used by PawPrint's
    /// `SystemNative_Write` / `SystemNative_Read` shims when the caller
    /// supplies a negative `bufferSize`, matching the behaviour of
    /// `Common_Write` / `Common_Read` in `pal_io_common.h` (which set
    /// `errno = ERANGE` and return -1 for negative sizes before the real
    /// `read(2)` / `write(2)` is invoked).
    let ERANGE : int = 34
