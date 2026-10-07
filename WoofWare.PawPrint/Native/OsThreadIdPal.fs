namespace WoofWare.PawPrint

open WoofWare.PosixKernel

/// The two widths `libSystem.Native` reports a thread's OS thread ID in, which is
/// what CoreLib's `Lock.ThreadId.InitializeForCurrentThread` reads: a Linux CoreLib
/// through `SystemNative_TryGetUInt32OSThreadId`, and a macOS one through
/// `SystemNative_GetUInt64OSThreadId`.
///
/// Both start from `minipal_get_current_thread_id()` (src/native/minipal/thread.h),
/// a `size_t` holding `gettid(2)` on Linux and `pthread_threadid_np(3)` on macOS,
/// which is the kernel's `OsThreadId` whole. Both are implemented whichever
/// flavour the kernel is, because which one a guest calls depends on the CoreLib
/// PawPrint resolved, not on the kernel it simulates.
[<RequireQualifiedAccess>]
module OsThreadIdPal =

    /// `SystemNative_GetUInt64OSThreadId` (pal_threading.c):
    /// `return (uint64_t)minipal_get_current_thread_id();`, the ID verbatim.
    let getUInt64 (id : OsThreadId) : uint64 = OsThreadId.toUInt64 id

    /// `SystemNative_TryGetUInt32OSThreadId` (pal_threading.c):
    ///
    ///     uint32_t result = (uint32_t)minipal_get_current_thread_id();
    ///     return result == 0 ? (uint32_t)-1 : result;
    ///
    /// The ID's low 32 bits, except that `(uint32)-1`, the shim's "cannot
    /// determine a thread ID", stands in for 0. A Linux tid is below 2^22, the
    /// greatest `pid_max` Linux takes, so it survives whole; a Darwin ID past 32
    /// bits does not, as on a real Mac.
    let tryGetUInt32 (id : OsThreadId) : uint32 =
        match uint32 (OsThreadId.toUInt64 id) with
        | 0u -> System.UInt32.MaxValue
        | truncated -> truncated
