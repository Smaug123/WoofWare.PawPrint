using System;
using System.Runtime.InteropServices;

// `DirectoryEntry.NameLength`, the one thing the two kernels' `readdir` shims
// disagree about. **PawPrint only** -- there is no differential comparison to be
// had, because the real runtime answers for the machine it is on rather than for
// the flavour this kernel claims.
//
// **Linux**: glibc's `struct dirent` has no `d_namlen` member at all -- `gcc`
// rejects `d.d_namlen` outright -- so `HAVE_DIRENT_NAME_LEN` is 0 and
// `ConvertDirent` writes the sentinel -1, meaning "walk to the NUL yourself".
// Every row below therefore expects -1, whatever the name's length.
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
            // Every name, whatever its length in bytes or characters,
            // reports the same sentinel. Three lengths rather than one, so a
            // handler that answered -1 only for some names could not pass.
            if (entry.NameLength != -1) return 3;

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
