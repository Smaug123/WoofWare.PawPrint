using System;
using System.Runtime.InteropServices;

// `DirectoryEntry.NameLength`, the one thing the two kernels' `readdir` shims
// disagree about. **PawPrint only** -- there is no differential comparison to be
// had, because the real runtime answers for the machine it is on rather than for
// the flavour this kernel claims.
//
// **Darwin**: macOS's `sys/dirent.h` declares `d_namlen`, so
// `HAVE_DIRENT_NAME_LEN` is 1 and `ConvertDirent` copies the real length.
//
// The names are chosen so that a *byte* count is the only rule that fits: "e"
// is 1 byte and 1 char, "\u00e9" is 2 bytes and 1 char, and "\u4e2d\u4e2d" is
// 6 bytes and 2 chars. A handler reporting `String.Length` would answer 1, 1
// and 2 -- agreeing on the first name only.
//
// Invisible to managed code: `DirectoryEntry.GetName` takes
// `CreateReadOnlySpanFromNullTerminated` for the sentinel and a plain span
// otherwise, so both decode to the same string. Only a guest that hand-rolls the
// P/Invoke can tell, which is what this is.
//
// This is also the only thing that sees the *wiring*: `TestOpenDirRules` and the
// host oracle both call the model directly, so a handler that hardcoded one
// flavour's answer instead of reading `Kernel.UnixPlatform` would satisfy every
// one of them. The uid is set away from its default in the registration for the
// same reason.
//
// The exit code is the index of the first check that failed; 0 means all passed.
//
// Seed (see TestImpureCases.enumerateWiringSeed): d/ holding "e", "\u00e9" and
// "\u4e2d\u4e2d".
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_OpenDir", SetLastError = true)]
    static extern unsafe IntPtr OpenDir(byte* path);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_CloseDir", SetLastError = true)]
    static extern int CloseDir(IntPtr dir);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_ReadDir")]
    static extern unsafe int ReadDir(IntPtr dir, DirectoryEntry* entry);

    // Must match `Interop.Sys.DirectoryEntry` exactly: a pointer then two 32-bit
    // fields, 16 bytes.
    [StructLayout(LayoutKind.Sequential)]
    unsafe struct DirectoryEntry
    {
        public byte* Name;
        public int NameLength;
        public int InodeType;
    }

    /// The name's length in bytes, counted by the guest from the terminator --
    /// which is a decode valid on both kernels, and so is an independent
    /// measurement rather than a restatement of the field under test.
    static unsafe int Bytes(byte* name)
    {
        int length = 0;
        while (name[length] != 0) length++;
        return length;
    }

    static unsafe int Main()
    {
        byte* path = stackalloc byte[8];
        path[0] = (byte)'d';
        path[1] = 0;

        IntPtr dir = OpenDir(path);
        if (dir == IntPtr.Zero) return 1;

        DirectoryEntry entry;
        int names = 0;

        while (ReadDir(dir, &entry) == 0)
        {
            // `.` and `..` carry a length too, and it must follow the same rule.
            // The byte length, not the character count. `expected`
            // is computed by the guest from the bytes it walked, so this
            // compares two independent counts rather than a constant.
            if (entry.NameLength != Bytes(entry.Name)) return 3;

            names++;
        }

        if (CloseDir(dir) != 0) return 4;

        // Two dots plus the three seeded names: proof the loop ran at all, so a
        // handler that answered end-of-stream immediately could not pass.
        if (names != 5) return 5;

        // Fifty streams, opened and closed. Nothing here reads them: the point
        // is what `CloseDir` releases, which no guest can see -- the terminal
        // assertion checks the kernel's tables afterwards, and fifty leaked
        // descriptors or name buffers would be unmissable there.
        for (int i = 0; i < 50; i++)
        {
            IntPtr again = OpenDir(path);
            if (again == IntPtr.Zero) return 6;
            if (CloseDir(again) != 0) return 7;
        }

        return 0;
    }
}
