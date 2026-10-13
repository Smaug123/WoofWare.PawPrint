using System;
using System.Collections.Generic;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading;

// A FileSystemWatcher over the directory w, on the Linux CoreLib, whose watcher
// is inotify (FileSystemWatcher.Linux.cs): the guest creates, writes, renames
// and deletes a file in w, and checks that the watcher raises Created, Changed,
// Renamed and Deleted for them, in that order and nothing else. Each event is
// printed as it is checked.
//
// Every operation is one syscall that queues the records of one event under the
// watcher's mask (inotify-probe.c, sections E and N):
// - FileMode.CreateNew opens with O_CREAT|O_EXCL and truncates nothing: IN_CREATE.
//   (FileMode.Create would also ftruncate, whose IN_MODIFY Linux may or may not
//   coalesce with the write's, depending on when the watcher thread reads.)
// - One write through a descriptor opened without O_TRUNC: IN_MODIFY.
// - File.Move to a name that does not exist is rename(2): IN_MOVED_FROM and
//   IN_MOVED_TO with one cookie, which the watcher pairs into Renamed.
// - File.Delete is unlink(2): IN_DELETE.
// The guest waits for each event before making the next change, so what the
// watcher raises does not depend on when its thread reads.
//
// The events are raised on the watcher's own thread, which blocks in read(2) on
// the inotify descriptor. Each wait is bounded by a count of bounded waits, not
// by a clock, so that a missing event ends the run with that event's code
// rather than hanging it; under PawPrint the waits are in virtual time.
//
// Dispose removes the watch, and Linux then queues IN_IGNORED, which is the
// only thing that wakes the watcher thread's read; the thread then sees the
// cancellation, leaves its loop and closes the descriptor. That thread is a
// background thread, so the process would exit without it, and the guest
// checks instead that the descriptor is closed. It finds the descriptor among
// those that enabling the watcher opened (the real runtime opens others then,
// for the assemblies it loads) as the one that answers inotify_add_watch of w,
// with the watcher's own mask, with the watcher's watch descriptor: Linux
// replaces a watch's mask with the same mask and queues nothing (inotify-probe.c,
// section W).
//
// The exit code is the index of the first check that failed; 0 means all passed.
//
// Seed (see TestImpureCases): w/ (an empty directory).
class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_FcntlGetFD", SetLastError = true)]
    static extern int FcntlGetFD(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_ConvertErrorPlatformToPal")]
    static extern int ConvertErrorPlatformToPal(int platformErrno);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_INotifyAddWatch", SetLastError = true)]
    static extern unsafe int INotifyAddWatch(IntPtr fd, byte* pathName, uint mask);

    const int PAL_EBADF = 0x10008;

    // `NotifyEvents` (pal_io.h), each equal to Linux's IN_* value.
    const uint IN_MODIFY = 0x00000002;
    const uint IN_ATTRIB = 0x00000004;
    const uint IN_MOVED_FROM = 0x00000040;
    const uint IN_MOVED_TO = 0x00000080;
    const uint IN_CREATE = 0x00000100;
    const uint IN_DELETE = 0x00000200;
    const uint IN_ONLYDIR = 0x01000000;
    const uint IN_EXCL_UNLINK = 0x04000000;

    // The mask FileSystemWatcher asks for under its default NotifyFilters
    // (FileSystemWatcher.Linux.cs, TranslateFilters).
    const uint WatcherMask =
        IN_CREATE | IN_DELETE | IN_MODIFY | IN_ATTRIB | IN_MOVED_FROM | IN_MOVED_TO | IN_ONLYDIR | IN_EXCL_UNLINK;

    // Well past anything a process this small holds open.
    const int DescriptorsScanned = 256;

    // Each wait is at most this many waits of WaitMilliseconds.
    const int Waits = 100;
    const int WaitMilliseconds = 100;

    static readonly object Gate = new object();
    static readonly List<string> Seen = new List<string>();

    static void Record(string what)
    {
        lock (Gate)
        {
            Seen.Add(what);
            Monitor.PulseAll(Gate);
        }
    }

    // Whether the watcher has raised event `index` (counting from 0), within the bound.
    static bool AwaitEvent(int index)
    {
        lock (Gate)
        {
            for (int i = 0; i < Waits && Seen.Count <= index; i++)
            {
                Monitor.Wait(Gate, WaitMilliseconds);
            }

            return Seen.Count > index;
        }
    }

    static string EventAt(int index)
    {
        lock (Gate)
        {
            return Seen[index];
        }
    }

    static int EventCount()
    {
        lock (Gate)
        {
            return Seen.Count;
        }
    }

    static bool IsClosed(int fd)
    {
        return FcntlGetFD((IntPtr)fd) == -1
            && ConvertErrorPlatformToPal(Marshal.GetLastSystemError()) == PAL_EBADF;
    }

    // Whether `fd` is an inotify instance whose first watch is of w with the
    // watcher's mask.
    static unsafe bool WatchesW(int fd)
    {
        byte* w = stackalloc byte[2];
        w[0] = (byte)'w';
        w[1] = 0;
        return INotifyAddWatch((IntPtr)fd, w, WatcherMask) == 1;
    }

    static HashSet<int> OpenDescriptors()
    {
        HashSet<int> open = new HashSet<int>();
        for (int fd = 0; fd < DescriptorsScanned; fd++)
        {
            if (FcntlGetFD((IntPtr)fd) != -1) open.Add(fd);
        }

        return open;
    }

    // Wait for event `index`, check it is `expected`, and print it.
    static int Expect(int index, string expected, int missing, int wrong)
    {
        if (!AwaitEvent(index)) return missing;
        if (EventAt(index) != expected) return wrong;
        Console.WriteLine(expected);
        return 0;
    }

    static int Main()
    {
        FileSystemWatcher watcher = new FileSystemWatcher("w");
        watcher.Created += (_, e) => Record("Created " + e.Name);
        watcher.Changed += (_, e) => Record("Changed " + e.Name);
        watcher.Renamed += (_, e) => Record("Renamed " + e.OldName + " " + e.Name);
        watcher.Deleted += (_, e) => Record("Deleted " + e.Name);
        watcher.Error += (_, e) => Record("Error " + e.GetException().GetType().Name);

        // Enabling the watcher opens one inotify instance, watching w.
        HashSet<int> before = OpenDescriptors();
        watcher.EnableRaisingEvents = true;
        HashSet<int> opened = OpenDescriptors();
        opened.ExceptWith(before);
        int inotify = -1;
        foreach (int fd in opened)
        {
            if (!WatchesW(fd)) continue;
            if (inotify != -1) return 1;
            inotify = fd;
        }

        if (inotify == -1) return 2;

        using (FileStream created = new FileStream("w/a", FileMode.CreateNew, FileAccess.Write))
        {
        }

        int failed = Expect(0, "Created a", 3, 4);
        if (failed != 0) return failed;

        using (FileStream written = new FileStream("w/a", FileMode.Open, FileAccess.Write))
        {
            byte[] hello = Encoding.UTF8.GetBytes("hello");
            written.Write(hello, 0, hello.Length);
        }

        failed = Expect(1, "Changed a", 5, 6);
        if (failed != 0) return failed;

        File.Move("w/a", "w/b");
        failed = Expect(2, "Renamed a b", 7, 8);
        if (failed != 0) return failed;

        File.Delete("w/b");
        failed = Expect(3, "Deleted b", 9, 10);
        if (failed != 0) return failed;

        watcher.Dispose();

        bool closed = IsClosed(inotify);
        for (int i = 0; i < Waits && !closed; i++)
        {
            Thread.Sleep(WaitMilliseconds);
            closed = IsClosed(inotify);
        }

        if (!closed) return 11;

        // Nothing raised beyond the four, now that the watcher thread has ended.
        if (EventCount() != 4) return 12;

        return 0;
    }
}
