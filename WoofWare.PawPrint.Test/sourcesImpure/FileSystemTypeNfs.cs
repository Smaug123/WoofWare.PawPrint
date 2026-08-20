using System;
using System.IO;

// What `KernelConfig.FileSystemType` actually buys, on the one filesystem where
// it changes behaviour rather than just a reported number.
//
// `SafeFileHandle.CanLockTheFile` refuses to take a *shared* lock under write
// access on NFS, CIFS and SMB, because `flock` is unsafe there. Every other
// filesystem PawPrint can claim to be takes the lock, so this is the only
// configuration under which a `(FileAccess.Write, FileShare.Read)` handle holds
// no lock at all — and hence the only guest-visible consequence of that whole
// four-way switch.
//
// Not differential, and could not be: putting the oracle's scratch directory on
// NFS is not something this suite can arrange. `sourcesPure/FlockContentionSeeded.cs`
// carries the *other* side of this pair — under the default filesystem the same
// two opens do contend — so the two files together say that the configured type
// is what decides it, rather than locking being broken in one direction or the
// other.
//
// The exit code is the index of the first check that failed; 0 means all passed.
// Kept below 128, since an exit code is eight bits.
//
// Seed (see TestImpureCases): f = "hello". Kernel: FileSystemType = Nfs.
class Program
{
    static FileStream Open(FileAccess access, FileShare share)
        => File.Open("f", FileMode.Open, access, share);

    static int Main(string[] args)
    {
        int check;

        // The headline. On any other filesystem this handle takes `LOCK_SH`, and
        // the second open's `LOCK_EX` is refused; on NFS the first handle takes
        // nothing, so the second succeeds. `FlockContentionSeeded.cs` check 10 is
        // exactly this pair under the default filesystem, and it must fail there.
        check = 1;
        using (FileStream a = Open(FileAccess.Write, FileShare.Read))
        {
            try
            {
                using (FileStream b = Open(FileAccess.Read, FileShare.None)) { }
            }
            catch (IOException)
            {
                return check;
            }
        }

        // The control, and it is not optional: `CanLockTheFile` consults the
        // filesystem type *only* for a shared lock under write access, so
        // `FileShare.None` still asks for `LOCK_EX` and still contends. Without
        // this row, a runtime whose locking had stopped working altogether would
        // pass check 1 and look like a correct NFS.
        check = 2;
        using (FileStream a = Open(FileAccess.Read, FileShare.None))
        {
            bool opened = false;
            try
            {
                using (FileStream b = Open(FileAccess.Read, FileShare.None)) { }
                opened = true;
            }
            catch (IOException) { }
            if (opened) return check;
        }

        // ...and a shared lock taken for *reading* is likewise unaffected: that
        // path returns from `CanLockTheFile` before the filesystem type is
        // reached, so it must behave exactly as it does everywhere else.
        check = 3;
        using (FileStream a = Open(FileAccess.Read, FileShare.Read))
        {
            bool opened = false;
            try
            {
                using (FileStream b = Open(FileAccess.Read, FileShare.None)) { }
                opened = true;
            }
            catch (IOException) { }
            if (opened) return check;
        }

        return 0;
    }
}
