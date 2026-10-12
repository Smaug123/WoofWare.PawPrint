namespace WoofWare.PawPrint

/// The BCL's `NotifyEvents` encoding of inotify's event and flag bits
/// (`pal_io.h`), and the conversion `SystemNative_INotifyAddWatch` performs
/// across it.
///
/// Each `PAL_IN_*` is the value of the Linux `IN_*` of the same name: `pal_io.c`
/// asserts so at compile time wherever the shim is built with inotify. So the
/// shim hands the caller's mask to `inotify_add_watch(2)` as it is, every bit
/// of it, those `NotifyEvents` does not name included. The one build that
/// changes it is one without `IN_EXCL_UNLINK`, which clears that bit; the C
/// libraries of every Linux PawPrint models declare it. The library therefore
/// gets the guest's word, in Linux's `<sys/inotify.h>` numbering.
///
/// The records a read of the instance returns cross back the same way: the
/// shim's read is `read(2)`, and the managed watcher reads each record's mask
/// as `NotifyEvents`.
///
/// Where the shim is built without inotify (every Darwin build) it answers
/// ENOTSUP before any mask is looked at, so this is Linux's alone.
///
/// The equality is a transcription, so the compiler cannot keep it true. Its
/// oracle is upstream: `TestNotifyEventsPal` re-reads the enum from the pinned
/// `pal_io.h`, and the pass-through from `pal_io.c`.
[<RequireQualifiedAccess>]
module NotifyEventsPal =

    /// The mask the shim hands `inotify_add_watch(2)` when
    /// `SystemNative_INotifyAddWatch` is called with `mask`: the same word.
    let toInotifyMask (mask : uint32) : uint32 = mask
