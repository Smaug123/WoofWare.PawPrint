using System;
using System.IO;

// `FileShare` contention between two open handles on one file, which on Unix is
// `flock(2)` and nothing else.
//
// CoreCLR implements `FileShare` on Unix as a single advisory whole-file lock
// taken at open time (`SafeFileHandle.Unix.cs`): `FileShare.None` asks for
// `LOCK_EX`, every other value asks for `LOCK_SH`, always with `LOCK_NB`, and
// only `EWOULDBLOCK` is turned into an exception. So the entire observable
// matrix below is decided by the shared/exclusive rule, not by anything
// resembling Windows' richer sharing modes — `FileShare.Read` and
// `FileShare.Write` are indistinguishable here, which is why both appear.
//
// The two handles are opened by the *same process*, which is what makes this
// testable: a `flock` lock belongs to the open file
// description, not to the process, so two separate `open(2)` calls contend with
// each other even from one thread. A model that keyed locks on the process
// would pass every check by never conflicting at all.
//
// Every check requests `FileAccess.Read`, which keeps `CanLockTheFile` on its
// simple path (it consults the filesystem type only for a *shared* lock under
// write access) and keeps the file unmodified, so the checks are independent of
// each other and of order.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
class Program
{
    static FileStream Open(FileShare share)
        => File.Open("f", FileMode.Open, FileAccess.Read, share);

    // True if a second handle with `inner` can be opened while `outer` is held.
    static bool CanOpenUnder(FileShare outer, FileShare inner)
    {
        using (var a = Open(outer))
        {
            try
            {
                using (var b = Open(inner)) { }
                return true;
            }
            catch (IOException)
            {
                // The specific type matters: CoreCLR maps `EWOULDBLOCK` through
                // `GetExceptionForIoErrno` to a plain `IOException`, whereas an
                // errno such as `EACCES` would surface as
                // `UnauthorizedAccessException`. Catching the base `Exception`
                // here would let a wrong errno pass.
                return false;
            }
        }
    }

    static int Main(string[] args)
    {
        int check = 0;

        // Exclusive against exclusive. This is the check a runtime that does
        // not implement locking at all fails first.
        check = 1;
        if (CanOpenUnder(FileShare.None, FileShare.None)) return check;

        // Shared against shared: the common case, and the one that must *not*
        // fail. `File.OpenRead` and `File.ReadAllBytes` both land here, so an
        // implementation that treated every lock as exclusive would break
        // ordinary reading rather than merely being over-strict.
        check = 2;
        if (!CanOpenUnder(FileShare.Read, FileShare.Read)) return check;

        // Exclusive blocks a later shared...
        check = 3;
        if (CanOpenUnder(FileShare.None, FileShare.Read)) return check;

        // ...and a held shared blocks a later exclusive. These two are separate
        // checks because they exercise opposite halves of the conflict rule: an
        // implementation that only ever compared the *incoming* request against
        // "is anything locked" would pass one and fail the other.
        check = 4;
        if (CanOpenUnder(FileShare.Read, FileShare.None)) return check;

        // `FileShare.ReadWrite` and `FileShare.Write` are also merely "not
        // None", so they too take `LOCK_SH` and block a later exclusive. On
        // Windows these would be materially different sharing modes; on Unix
        // they are not, and asserting that pins the collapse.
        check = 5;
        if (CanOpenUnder(FileShare.ReadWrite, FileShare.None)) return check;
        check = 6;
        if (CanOpenUnder(FileShare.Write, FileShare.None)) return check;

        // A lock does not outlive the handles that took it. This pins the
        // *net* effect of disposal, not `LOCK_UN` specifically: CoreCLR issues
        // `LOCK_UN` and then `close(2)`, and closing the last descriptor drops
        // the lock anyway, so no managed guest can tell the two apart. (Measured
        // by mutation: making `LOCK_UN` a no-op leaves this whole file passing.)
        // `TestFileDescriptorRegistry` covers the release itself.
        check = 7;
        using (var a = Open(FileShare.None)) { }
        using (var b = Open(FileShare.None)) { }
        if (!CanOpenUnder(FileShare.Read, FileShare.Read)) return check;

        // A lock is per *file*, so a handle on one file does not disturb
        // another. Without this, "lock everything globally" passes every check
        // above.
        check = 8;
        using (var a = Open(FileShare.None))
        {
            try
            {
                using (var b = File.Open("g", FileMode.Open, FileAccess.Read, FileShare.None)) { }
            }
            catch (IOException)
            {
                return check;
            }
        }

        // Two handles on the *same* file reached by two different paths are
        // still one file, so they must contend. `lf` is a symlink to `f`, and
        // an implementation that keyed locks on the path rather than on the
        // resolved inode would let this through.
        check = 9;
        using (var a = Open(FileShare.None))
        {
            bool opened = false;
            try
            {
                using (var b = File.Open("lf", FileMode.Open, FileAccess.Read, FileShare.None)) { }
                opened = true;
            }
            catch (IOException) { }
            if (opened) return check;
        }

        return 0;
    }
}
