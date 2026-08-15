using System;
using System.Runtime.InteropServices;
using System.Text;

// Drives SystemNative_Stat with over-long paths, to pin that the emulated
// kernel enforces PATH_MAX and NAME_MAX and reports ENAMETOOLONG.
//
// PawPrint-only. The BCL cannot see any of this: File.Exists answers false for
// ENOENT and ENAMETOOLONG alike, and CoreLib never builds a path this long, so
// only a hand-rolled P/Invoke reading errno can tell the two apart. The raw
// number is the Linux one because that is the kernel KernelConfig configures by
// default; on Darwin ENAMETOOLONG is 63, which is exactly why UnixError refuses
// to state a raw number without being told the platform.
//
// The two limits are exercised so that neither could be deleted without a
// failure here:
//
//   * the PATH_MAX case is built from components of 200 bytes, all well under
//     NAME_MAX, so the *only* rule that can refuse it is the length of the
//     whole argument;
//   * the NAME_MAX case is a single 256-byte component in a path far shorter
//     than PATH_MAX, so the only rule that can refuse it is the component one.
//
// Both controls sit just inside their limit and must report ENOENT, so "long
// paths fail" is not enough to pass.
//
// Errors are read with Marshal.GetLastSystemError, for the reason
// StatFieldsSeeded.cs sets out.
//
// The exit code is the index of the first check that failed; 0 means all
// passed. Kept below 128, since an exit code is eight bits.
class Program
{
    [StructLayout(LayoutKind.Sequential)]
    struct FileStatus
    {
        public int Flags;
        public int Mode;
        public uint Uid;
        public uint Gid;
        public long Size;
        public long ATime;
        public long ATimeNsec;
        public long MTime;
        public long MTimeNsec;
        public long CTime;
        public long CTimeNsec;
        public long BirthTime;
        public long BirthTimeNsec;
        public long Dev;
        public long RDev;
        public long Ino;
        public uint UserFlags;
    }

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Stat", SetLastError = true)]
    static extern unsafe int Stat(byte* path, FileStatus* output);

    // Linux numbering, per the comment above.
    const int ENOENT = 2;
    const int ENAMETOOLONG_LINUX = 36;

    static int check = 0;

    static bool Is(bool condition)
    {
        check++;
        return condition;
    }

    static unsafe int StatOf(string path)
    {
        FileStatus status;
        byte[] bytes = Encoding.UTF8.GetBytes(path + "\0");
        fixed (byte* p = bytes)
        {
            return Stat(p, &status);
        }
    }

    /// A path of `totalBytes` bytes whose every component is 200 bytes, so that
    /// no component can be what makes it too long.
    static string LongPath(int totalBytes)
    {
        var sb = new StringBuilder();
        while (sb.Length < totalBytes)
        {
            sb.Append('/');
            sb.Append(new string('a', 200));
        }
        return sb.ToString(0, totalBytes);
    }

    static int Main(string[] args)
    {
        // ---- PATH_MAX: 4095 bytes is the longest Linux accepts ----
        string justInside = LongPath(4095);
        if (!Is(StatOf(justInside) == -1)) return check;
        if (!Is(Marshal.GetLastSystemError() == ENOENT)) return check;

        string justOver = LongPath(4096);
        if (!Is(StatOf(justOver) == -1)) return check;
        if (!Is(Marshal.GetLastSystemError() == ENAMETOOLONG_LINUX)) return check;

        // ---- NAME_MAX: 255 bytes is the longest component ----
        string longestName = "/" + new string('b', 255);
        if (!Is(StatOf(longestName) == -1)) return check;
        if (!Is(Marshal.GetLastSystemError() == ENOENT)) return check;

        string overLongName = "/" + new string('b', 256);
        if (!Is(StatOf(overLongName) == -1)) return check;
        if (!Is(Marshal.GetLastSystemError() == ENAMETOOLONG_LINUX)) return check;

        // ---- NAME_MAX is bytes on Linux, not UTF-16 code units ----
        // 86 three-byte characters are 258 bytes but only 86 UTF-16 units, so a
        // kernel counting units would permit this. Linux does not.
        string multiByte = "/" + new string('中', 86);
        if (!Is(StatOf(multiByte) == -1)) return check;
        if (!Is(Marshal.GetLastSystemError() == ENAMETOOLONG_LINUX)) return check;

        // ...and 85 of them are exactly 255 bytes, so they are permitted.
        string multiByteInside = "/" + new string('中', 85);
        if (!Is(StatOf(multiByteInside) == -1)) return check;
        if (!Is(Marshal.GetLastSystemError() == ENOENT)) return check;

        // ---- an unterminated buffer ----
        // A real kernel copies at most PATH_MAX bytes looking for the NUL and
        // reports ENAMETOOLONG if it finds none, so this is an ordinary error a
        // guest can provoke rather than something that should take the runtime
        // down. The buffer is exactly PATH_MAX bytes and holds no zero, so an
        // implementation that scanned for a terminator first would read past the
        // end of this array.
        if (!Is(StatOfUnterminated(4096) == -1)) return check;
        if (!Is(Marshal.GetLastSystemError() == ENAMETOOLONG_LINUX)) return check;

        return 0;
    }

    static unsafe int StatOfUnterminated(int length)
    {
        FileStatus status;
        byte[] bytes = new byte[length];
        for (int i = 0; i < length; i++) bytes[i] = (byte)'a';
        fixed (byte* p = bytes)
        {
            return Stat(p, &status);
        }
    }
}
