using System;
using System.IO;

// The BCL's symbolic-link surface, all of which reaches SystemNative_ReadLink
// and none of which was reachable before it: FileSystemInfo.LinkTarget on both
// FileInfo and DirectoryInfo, and FileSystemInfo.ResolveLinkTarget with
// returnFinalTarget both ways.
//
// Differential, so every check is a fact both runtimes must agree on. Paths are
// **relative** for that reason: PawPrint puts the seed at the root of an
// otherwise-empty filesystem with "/" as the current directory, while the
// oracle materialises it into a scratch directory the guest is started in, so
// the two agree on relative names but not on absolute ones. Where a full path
// is unavoidable it is compared against Path.GetFullPath rather than a literal,
// which still pins the composition -- a link target is spliced onto the
// *directory* of the link, not appended to the link itself -- without
// asserting where the tree lives.
//
// **Why the static File.ResolveLinkTarget / Directory.ResolveLinkTarget
// overloads are absent.** They do not GetFullPath their argument, where the
// instance method passes FileSystemInfo.FullPath. Given a relative link path,
// FileSystem.ResolveLinkTarget's GetLinkTargetFullPath truncates the builder to
// the directory-name offset -- zero, for a bare name -- and appends a
// separator, so the answer comes back rooted at "/". Measured on real .NET:
// File.ResolveLinkTarget("lf", false).FullName is "/f", and with
// returnFinalTarget the walk then stops after one hop at "/lf", because the
// host has no such path. Under PawPrint "/lf" is precisely the seeded link, so
// the walk would continue -- and "cyc" would reach the 40-link limit where the
// host stops at once. That is the one shape in this harness where the two roots
// are observably different, so it is not a cross-runtime fact and cannot be
// asserted here.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
//
// Seed (see TestPureCases.seededCases): f, d/, lf -> f, ld -> d, l2 -> lf,
// dang -> nx, cyc -> cyc, long -> "a" x 300. "nx" deliberately does not exist.
class Program
{
    static int Main(string[] args)
    {
        int check = 0;

        check = 1;
        if (new FileInfo("lf").LinkTarget != "f") return check;

        // Not a link: null rather than an exception. This is the EINVAL path,
        // and it is why the handler must answer EINVAL rather than any other
        // errno for a non-link -- every other errno becomes an exception.
        check = 2;
        if (new FileInfo("f").LinkTarget != null) return check;

        // Nothing there at all: also null, so LinkTarget alone cannot tell
        // "not a link" from "no such file".
        check = 3;
        if (new FileInfo("nx").LinkTarget != null) return check;

        check = 4;
        if (new DirectoryInfo("ld").LinkTarget != "d") return check;

        // A dangling link is still a link, and readlink reports its target
        // without caring that nothing is there.
        check = 5;
        if (new FileInfo("dang").LinkTarget != "nx") return check;

        // A link to itself reads back as itself; nothing here follows it.
        check = 6;
        if (new FileInfo("cyc").LinkTarget != "cyc") return check;

        // 300 bytes, which is what makes this check worth more than the ones
        // above: Interop.Sys.ReadLink's first call uses a 256-byte stackalloc,
        // so this target can only be read at all by the buffer being grown and
        // the call retried -- i.e. by the handler truncating faithfully and
        // reporting the buffer size rather than the target's length.
        check = 7;
        string? big = new FileInfo("long").LinkTarget;
        if (big == null || big.Length != 300) return check;
        check = 8;
        foreach (char c in big)
        {
            if (c != 'a') return check;
        }

        // ResolveLinkTarget resolves the target *relative to the link's own
        // directory*, and hands back a FileSystemInfo rather than a string.
        check = 9;
        FileSystemInfo? resolved = new FileInfo("lf").ResolveLinkTarget(false);
        if (resolved == null || resolved.FullName != Path.GetFullPath("f")) return check;
        check = 10;
        if (!(resolved is FileInfo)) return check;

        // Not a link: null, for the same EINVAL reason as check 2 -- and
        // reached only if the errno really is EINVAL, since every other one is
        // rethrown.
        check = 11;
        if (new FileInfo("f").ResolveLinkTarget(false) != null) return check;

        // No such file: ENOENT, which does throw. The exception type carries
        // the isDirectory flag the call site passed down, so the two entry
        // points below differ -- measured on real .NET, and a check that
        // accepted either would not notice a handler that lost the flag.
        check = 12;
        try
        {
            new FileInfo("nx").ResolveLinkTarget(false);
            return check;
        }
        catch (FileNotFoundException) { }

        check = 13;
        try
        {
            new DirectoryInfo("nx").ResolveLinkTarget(false);
            return check;
        }
        catch (DirectoryNotFoundException) { }

        // One hop only, even though the target is itself a link.
        check = 14;
        FileSystemInfo? oneHop = new FileInfo("l2").ResolveLinkTarget(false);
        if (oneHop == null || oneHop.FullName != Path.GetFullPath("lf")) return check;

        // ...and all the way, which needs the loop to run more than once.
        check = 15;
        FileSystemInfo? finalTarget = new FileInfo("l2").ResolveLinkTarget(true);
        if (finalTarget == null || finalTarget.FullName != Path.GetFullPath("f")) return check;

        // Following to the end stops at the first thing that is not a link,
        // and a name that exists at all is not required: the loop ends when
        // readlink fails for *any* reason, ENOENT included.
        check = 16;
        FileSystemInfo? dangling = new FileInfo("dang").ResolveLinkTarget(true);
        if (dangling == null || dangling.FullName != Path.GetFullPath("nx")) return check;

        // A link to itself never stops being a link, so the walk is bounded by
        // FileSystem.Unix.cs's MaxFollowedLinks of 40 rather than by the
        // kernel: every one of those 41 readlink calls succeeds. Note
        // FileNotFoundException is itself an IOException, so it is excluded
        // explicitly -- otherwise a handler that answered ENOENT for a cycle
        // would pass this check.
        check = 17;
        try
        {
            new FileInfo("cyc").ResolveLinkTarget(true);
            return check;
        }
        catch (IOException e) when (!(e is FileNotFoundException)) { }

        check = 18;
        FileSystemInfo? dir = new DirectoryInfo("ld").ResolveLinkTarget(false);
        if (dir == null || dir.FullName != Path.GetFullPath("d")) return check;
        check = 19;
        if (!(dir is DirectoryInfo)) return check;

        // A link to a directory, followed to the end: the target is a real
        // directory rather than a link, so the walk stops there rather than
        // going round again.
        check = 20;
        FileSystemInfo? dirFinal = new DirectoryInfo("ld").ResolveLinkTarget(true);
        if (dirFinal == null || dirFinal.FullName != Path.GetFullPath("d")) return check;

        return 0;
    }
}
