using System;
using System.Threading;

namespace HelloWorldApp
{
    class Program
    {
        // `WaitHandle.WaitAny` over handles that are already signalled, which
        // routes through the `WaitHandle_WaitMultipleIgnoringSyncContext`
        // QCall without ever blocking. The value under test is the *index*:
        // Win32's contract is "the array index of the signalled object with
        // the smallest index value of all the signalled objects", so a scan
        // that stopped at the wrong element, or that reported a queue
        // position rather than an array position, would show up here.
        static int Main(string[] args)
        {
            using (var unsignalled = new ManualResetEvent(false))
            using (var signalled = new ManualResetEvent(true))
            using (var alsoSignalled = new ManualResetEvent(true))
            {
                if (WaitHandle.WaitAny(new WaitHandle[] { unsignalled, signalled }) != 1)
                {
                    return 1;
                }

                if (WaitHandle.WaitAny(new WaitHandle[] { signalled, unsignalled }) != 0)
                {
                    return 2;
                }

                // Two signalled handles: the smaller index wins.
                if (WaitHandle.WaitAny(new WaitHandle[] { unsignalled, signalled, alsoSignalled }) != 1)
                {
                    return 3;
                }

                // Duplicates are legal for a wait-any, and resolve to the
                // first occurrence.
                if (WaitHandle.WaitAny(new WaitHandle[] { signalled, signalled }) != 0)
                {
                    return 4;
                }

                // Nothing signalled and a finite timeout: the wait must expire
                // rather than park forever, and report WaitTimeout.
                if (WaitHandle.WaitAny(new WaitHandle[] { unsignalled }, 50) != WaitHandle.WaitTimeout)
                {
                    return 5;
                }

                // Zero timeout is the non-blocking probe; it must not enqueue.
                if (WaitHandle.WaitAny(new WaitHandle[] { unsignalled }, 0) != WaitHandle.WaitTimeout)
                {
                    return 6;
                }

                // A manual-reset event is not consumed by acquiring it, so the
                // same wait repeats indefinitely.
                if (WaitHandle.WaitAny(new WaitHandle[] { unsignalled, signalled }) != 1)
                {
                    return 7;
                }

                return 0;
            }
        }
    }
}
