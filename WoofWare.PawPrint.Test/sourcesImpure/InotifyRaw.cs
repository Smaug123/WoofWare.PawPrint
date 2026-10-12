using System;
using System.Runtime.InteropServices;
using System.Threading;

// System.Native's three inotify entry points called directly, as
// FileSystemWatcher.Linux.cs calls them, with the read, poll and FIONREAD its
// watcher thread makes of the descriptor, and a blocking read on a second thread
// that a filesystem change ends, then one that inotify_rm_watch ends.
//
// The shim is a pass-through to inotify_init1(IN_CLOEXEC), inotify_add_watch and
// inotify_rm_watch where it is built with inotify (pal_io.c), and answers -1 with
// ENOTSUP from all three where it is not, which is every Darwin build. So the
// guest runs to one of two ends: 0 on Linux, having checked every row below, or
// 100 on Darwin, having seen all three calls answer ENOTSUP.
//
// Each row is measured on Linux 6.18.5 (docs/plans/2026-10-11-inotify,
// inotify-probe.c: sections I, W, N, L, C, R, P and B).
//
// The second thread's read is made with nothing queued, so it cannot return
// until the main thread changes something: the main thread checks it has not,
// then makes the change, then joins it. Each change the main thread makes while
// it waits is one syscall that queues one record, so what the read returns does
// not depend on when it was entered.
//
// The exit code is the index of the first check that failed; 0 means all passed.
//
// Seed (see TestImpureCases): d/ (an empty directory) and f (a regular file).
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_INotifyInit", SetLastError = true)]
    static extern IntPtr INotifyInit();

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_INotifyAddWatch", SetLastError = true)]
    static extern unsafe int INotifyAddWatch(IntPtr fd, byte* pathName, uint mask);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_INotifyRemoveWatch", SetLastError = true)]
    static extern int INotifyRemoveWatch(IntPtr fd, int wd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Read", SetLastError = true)]
    static extern unsafe int Read(IntPtr fd, byte* buffer, int bufferSize);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Poll")]
    static extern unsafe int Poll(PollEvent* pollEvents, uint eventCount, int milliseconds, uint* triggered);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetBytesAvailable")]
    static extern unsafe int GetBytesAvailable(IntPtr fd, int* available);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_FcntlGetFD", SetLastError = true)]
    static extern int FcntlGetFD(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_FcntlSetIsNonBlocking", SetLastError = true)]
    static extern int SetIsNonBlocking(IntPtr fd, int isNonBlocking);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_MkDir", SetLastError = true)]
    static extern unsafe int MkDir(byte* path, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_ChMod", SetLastError = true)]
    static extern unsafe int ChMod(byte* path, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Close", SetLastError = true)]
    static extern int Close(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_ConvertErrorPlatformToPal")]
    static extern int ConvertErrorPlatformToPal(int platformErrno);

    [StructLayout(LayoutKind.Sequential)]
    struct PollEvent
    {
        public int FileDescriptor;
        public short Events;
        public short TriggeredEvents;
    }

    const int PAL_SUCCESS = 0;
    const int PAL_EAGAIN = 0x10006;
    const int PAL_EBADF = 0x10008;
    const int PAL_EFAULT = 0x10015;
    const int PAL_EINVAL = 0x1001C;
    const int PAL_ENOENT = 0x1002D;
    const int PAL_ENOTDIR = 0x10039;
    const int PAL_ENOTSUP = 0x1003D;

    const short POLLIN = 0x0001;

    const int FD_CLOEXEC = 1;

    // `NotifyEvents` (pal_io.h), each equal to Linux's IN_* value.
    const uint IN_MODIFY = 0x00000002;
    const uint IN_ATTRIB = 0x00000004;
    const uint IN_MOVED_FROM = 0x00000040;
    const uint IN_MOVED_TO = 0x00000080;
    const uint IN_CREATE = 0x00000100;
    const uint IN_DELETE = 0x00000200;
    const uint IN_IGNORED = 0x00008000;
    const uint IN_ONLYDIR = 0x01000000;
    const uint IN_MASK_CREATE = 0x10000000;
    const uint IN_MASK_ADD = 0x20000000;
    const uint IN_EXCL_UNLINK = 0x04000000;
    const uint IN_ISDIR = 0x40000000;

    // The mask FileSystemWatcher asks for under its default NotifyFilters
    // (FileSystemWatcher.Linux.cs, TranslateFilters).
    const uint WatcherMask =
        IN_CREATE | IN_DELETE | IN_MODIFY | IN_ATTRIB | IN_MOVED_FROM | IN_MOVED_TO | IN_ONLYDIR | IN_EXCL_UNLINK;

    // Well past anything the descriptor table hands out, and never opened.
    const int NeverOpened = 4096;

    // FileSystemWatcher's _internalBufferSize.
    const int ReadBufferSize = 8192;

    static int LastPalError() => ConvertErrorPlatformToPal(Marshal.GetLastSystemError());

    static byte[] CString(string name)
    {
        byte[] bytes = new byte[name.Length + 1];
        for (int i = 0; i < name.Length; i++) bytes[i] = (byte)name[i];
        bytes[name.Length] = 0;
        return bytes;
    }

    static unsafe int AddWatch(IntPtr fd, string path, uint mask)
    {
        fixed (byte* p = CString(path)) return INotifyAddWatch(fd, p, mask);
    }

    static unsafe int MkDirPath(string path, int mode)
    {
        fixed (byte* p = CString(path)) return MkDir(p, mode);
    }

    static unsafe int ChModPath(string path, int mode)
    {
        fixed (byte* p = CString(path)) return ChMod(p, mode);
    }

    static unsafe int Available(IntPtr fd)
    {
        int available = -1;
        if (GetBytesAvailable(fd, &available) != PAL_SUCCESS) return -1;
        return available;
    }

    // The events `poll` reports for `fd` asked for POLLIN with no wait, or -1 if
    // the call failed or counted other than 0 or 1 descriptors.
    static unsafe int PollIn(IntPtr fd)
    {
        PollEvent one = new PollEvent { FileDescriptor = (int)fd, Events = POLLIN };
        uint triggered = 12345;
        if (Poll(&one, 1, 0, &triggered) != PAL_SUCCESS) return -1;
        if (triggered != (one.TriggeredEvents == 0 ? 0u : 1u)) return -1;
        return one.TriggeredEvents;
    }

    // Whether `buffer` holds, from `offset`, the record for watch `wd` with
    // `mask`, no cookie, and `name` padded with NULs to `len` bytes.
    static unsafe bool IsRecord(byte* buffer, int offset, int wd, uint mask, string name, int len)
    {
        byte* record = buffer + offset;
        if (*(int*)record != wd) return false;
        if (*(uint*)(record + 4) != mask) return false;
        if (*(uint*)(record + 8) != 0) return false;
        if (*(uint*)(record + 12) != (uint)len) return false;
        for (int i = 0; i < len; i++)
        {
            byte expected = i < name.Length ? (byte)name[i] : (byte)0;
            if (record[16 + i] != expected) return false;
        }

        return true;
    }

    // A read made on another thread, which records what it returned.
    sealed class BackgroundRead
    {
        readonly IntPtr _fd;
        readonly byte[] _buffer = new byte[ReadBufferSize];
        readonly Thread _thread;
        int _done;
        int _result;

        public BackgroundRead(IntPtr fd)
        {
            _fd = fd;
            // Background, so that a check failing while the read sleeps ends
            // the process with that check's code rather than leaving it
            // waiting for this thread.
            _thread = new Thread(Run) { IsBackground = true };
            _thread.Start();
        }

        unsafe void Run()
        {
            fixed (byte* p = _buffer)
            {
                _result = Read(_fd, p, _buffer.Length);
            }

            Volatile.Write(ref _done, 1);
        }

        public bool Done => Volatile.Read(ref _done) == 1;

        public int Join()
        {
            _thread.Join();
            return _result;
        }

        public byte[] Buffer => _buffer;
    }

    static int Unsupported()
    {
        // Every Darwin build of the shim: all three answer -1 with ENOTSUP
        // before reaching any kernel, whatever their arguments.
        if (LastPalError() != PAL_ENOTSUP) return 101;
        if (AddWatch((IntPtr)0, "d", WatcherMask) != -1) return 102;
        if (LastPalError() != PAL_ENOTSUP) return 103;
        if (INotifyRemoveWatch((IntPtr)0, 1) != -1) return 104;
        if (LastPalError() != PAL_ENOTSUP) return 105;
        return 100;
    }

    static unsafe int Main()
    {
        IntPtr fd = INotifyInit();
        if (fd == (IntPtr)(-1)) return Unsupported();
        if ((long)fd < 0) return 1;

        // Section I: IN_CLOEXEC, so FD_CLOEXEC; an empty instance polls as
        // nothing, holds no bytes, and a non-blocking read of it is EAGAIN.
        if (FcntlGetFD(fd) != FD_CLOEXEC) return 2;
        if (PollIn(fd) != 0) return 3;
        if (Available(fd) != 0) return 4;
        if (SetIsNonBlocking(fd, 1) != 0) return 5;
        byte* small = stackalloc byte[64];
        if (Read(fd, small, 64) != -1) return 6;
        if (LastPalError() != PAL_EAGAIN) return 7;
        if (SetIsNonBlocking(fd, 0) != 0) return 8;

        // Section W: the add's failures.
        if (AddWatch((IntPtr)NeverOpened, "d", WatcherMask) != -1) return 9;
        if (LastPalError() != PAL_EBADF) return 10;
        if (AddWatch(fd, "missing", WatcherMask) != -1) return 11;
        if (LastPalError() != PAL_ENOENT) return 12;
        if (AddWatch(fd, "f", WatcherMask) != -1) return 13;
        if (LastPalError() != PAL_ENOTDIR) return 14;
        if (AddWatch(fd, "d", 0) != -1) return 15;
        if (LastPalError() != PAL_EINVAL) return 16;
        // The shim's `assert(pathName != NULL)` is compiled out of the shipped
        // build, so a null path reaches the kernel, which cannot copy it in.
        if (INotifyAddWatch(fd, null, WatcherMask) != -1) return 17;
        if (LastPalError() != PAL_EFAULT) return 18;

        // Linux decides a bad mask, then the descriptor, then IN_MASK_ADD with
        // IN_MASK_CREATE, then a descriptor that is no instance, all before it
        // copies the path in (inotify-instance-probe.c section G), so each of
        // these answers whatever the path pointer names. A type handle is
        // readable memory on the real runtime, and memory PawPrint holds as a
        // runtime structure rather than as bytes.
        byte* typeHandle = (byte*)typeof(int).TypeHandle.Value;
        if (INotifyAddWatch((IntPtr)NeverOpened, typeHandle, WatcherMask) != -1) return 50;
        if (LastPalError() != PAL_EBADF) return 51;
        if (INotifyAddWatch(fd, typeHandle, 0) != -1) return 52;
        if (LastPalError() != PAL_EINVAL) return 53;
        if (INotifyAddWatch(fd, typeHandle, IN_CREATE | IN_MASK_ADD | IN_MASK_CREATE) != -1) return 54;
        if (LastPalError() != PAL_EINVAL) return 55;
        // Standard output, which is not an inotify instance.
        if (INotifyAddWatch((IntPtr)1, typeHandle, WatcherMask) != -1) return 56;
        if (LastPalError() != PAL_EINVAL) return 57;

        // The first watch is 1, and adding its directory again answers it again
        // and queues nothing.
        if (AddWatch(fd, "d", WatcherMask) != 1) return 19;
        if (AddWatch(fd, "d", WatcherMask) != 1) return 20;
        if (Available(fd) != 0) return 21;

        // Section B: a read asleep on the instance; mkdir queues
        // CREATE|ISDIR "sub" (section N), and the read returns that one
        // 32-byte record (section L: a three-byte name is padded to 16).
        BackgroundRead first = new BackgroundRead(fd);
        Thread.Sleep(100);
        if (first.Done) return 22;
        if (MkDirPath("d/sub", 0x1FF) != 0) return 23;
        if (first.Join() != 32) return 24;
        fixed (byte* p = first.Buffer)
        {
            if (!IsRecord(p, 0, 1, IN_CREATE | IN_ISDIR, "sub", 16)) return 25;
        }

        // Section C: two chmods of one name queue one ATTRIB|ISDIR, which
        // polls as POLLIN and counts its 32 bytes (section P).
        if (ChModPath("d/sub", 0x1C0) != 0) return 26;
        if (ChModPath("d/sub", 0x1ED) != 0) return 27;
        if (Available(fd) != 32) return 28;
        if (PollIn(fd) != POLLIN) return 29;

        // Section R: a count shorter than the first record, zero included, is
        // EINVAL and takes nothing; enough for it returns it.
        byte* buffer = stackalloc byte[64];
        if (Read(fd, buffer, 31) != -1) return 30;
        if (LastPalError() != PAL_EINVAL) return 31;
        if (Read(fd, buffer, 0) != -1) return 32;
        if (LastPalError() != PAL_EINVAL) return 33;
        if (Read(fd, buffer, 64) != 32) return 34;
        if (!IsRecord(buffer, 0, 1, IN_ATTRIB | IN_ISDIR, "sub", 16)) return 35;
        if (Available(fd) != 0) return 36;
        if (PollIn(fd) != 0) return 37;

        // Section W: the removal's failures, which queue nothing.
        if (INotifyRemoveWatch(fd, 999) != -1) return 38;
        if (LastPalError() != PAL_EINVAL) return 39;
        if (INotifyRemoveWatch((IntPtr)NeverOpened, 1) != -1) return 40;
        if (LastPalError() != PAL_EBADF) return 41;
        if (Available(fd) != 0) return 42;

        // Section B: inotify_rm_watch of the only watch ends a read asleep on
        // the instance with the 16-byte IN_IGNORED record. This is how
        // FileSystemWatcher stops its own thread.
        BackgroundRead second = new BackgroundRead(fd);
        Thread.Sleep(100);
        if (second.Done) return 43;
        if (INotifyRemoveWatch(fd, 1) != 0) return 44;
        if (second.Join() != 16) return 45;
        fixed (byte* p = second.Buffer)
        {
            if (!IsRecord(p, 0, 1, IN_IGNORED, "", 0)) return 46;
        }

        // The watch is gone, so removing it again is EINVAL.
        if (INotifyRemoveWatch(fd, 1) != -1) return 47;
        if (LastPalError() != PAL_EINVAL) return 48;

        if (Close(fd) != 0) return 49;
        return 0;
    }
}
