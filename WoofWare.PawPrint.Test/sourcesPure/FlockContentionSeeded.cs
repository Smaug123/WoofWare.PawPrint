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
// The checks below the divider request `FileAccess.Read`, which keeps
// `CanLockTheFile` on its simple path and keeps the file unmodified, so they
// are independent of each other and of order. The write-access checks are
// segregated at the end for that reason, and because they are the only ones
// that reach `SystemNative_GetFileSystemType` at all.
//
// Two environmental premises the differential comparison rests on, neither of
// which any check can state for itself. The real runtime takes these locks only
// because the harness scratch directory is on a filesystem CoreCLR considers
// safe to `flock` — it refuses NFS, CIFS and SMB — and only because
// `DOTNET_SYSTEM_IO_DISABLEFILELOCKING` is unset, which would short-circuit
// `CanLockTheFile` to `false` before any of this. Both hold everywhere the
// suite runs; a machine where either failed would make every check here pass
// vacuously against a runtime that took no locks at all.
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

        // --- the write-access half: `CanLockTheFile`'s long path ---
        //
        // A shared lock taken under *write* access is the one combination that
        // consults `SystemNative_GetFileSystemType`, because `flock` is unsafe
        // on NFS, CIFS and SMB. Everything above returns from `CanLockTheFile`
        // before that call. This is the combination `File.WriteAllBytes` asks
        // for, so these two checks are what say the emulated filesystem
        // reports a type CoreCLR is willing to lock on — a runtime answering
        // "I do not know what filesystem this is" takes no lock at all and
        // fails check 10 while passing every check above it.

        // A write-access shared lock is still a lock, so it blocks a later
        // exclusive one.
        check = 10;
        using (FileStream a = File.Open("f", FileMode.Open, FileAccess.Write, FileShare.Read))
        {
            bool opened = false;
            try
            {
                using (FileStream b = Open(FileShare.None)) { }
                opened = true;
            }
            catch (IOException) { }
            if (opened) return check;
        }

        // ...and it is *shared*, so it does not block a later shared one. Both
        // halves are needed: a runtime that answered the filesystem-type
        // question by taking `LOCK_EX` instead would pass check 10 and fail
        // this one.
        check = 11;
        using (FileStream a = File.Open("f", FileMode.Open, FileAccess.Write, FileShare.Read))
        {
            try
            {
                using (FileStream b = Open(FileShare.Read)) { }
            }
            catch (IOException)
            {
                return check;
            }
        }

        return 0;
    }
}
