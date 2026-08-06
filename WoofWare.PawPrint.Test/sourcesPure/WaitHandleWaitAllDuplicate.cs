using System;
using System.Threading;

namespace HelloWorldApp
{
    class Program
    {
        // `WaitHandle.WaitAll` naming the same handle twice.
        //
        // The rejection happens in two stages, both *inside* the QCall. The
        // PAL's brute-force duplicate scan sets ERROR_INVALID_PARAMETER and
        // returns WAIT_FAILED; `Thread::DoAppropriateWait` then rescans and
        // converts that into a managed `DuplicateWaitObjectException`
        // (threads.cpp:3404) before any value reaches the BCL wrapper.
        //
        // That second stage is easy to miss, and getting it wrong is silent
        // rather than loud: `WaitHandle.WaitMultiple` never special-cases
        // WAIT_FAILED, and the abandoned-mutex range check does not match -1,
        // so simply returning WAIT_FAILED would make `WaitAll` report *success*
        // (`-1 != WaitTimeout`) for a wait that acquired nothing. Returning
        // WAIT_TIMEOUT would report a plain failure, also wrong. Only the
        // throw is faithful, and only a differential test distinguishes the
        // three.
        //
        // Duplicates are legal for a wait-any, which the last case covers.
        static int Main(string[] args)
        {
            using (var signalled = new ManualResetEvent(true))
            using (var sem = new Semaphore(1, 1))
            {
                try
                {
                    WaitHandle.WaitAll(new WaitHandle[] { signalled, signalled });
                    return 1;
                }
                catch (DuplicateWaitObjectException e)
                {
                    // The runtime overwrites the constructor's HResult with the
                    // one EEException::GetHR maps for the type, so this pins
                    // the mapping as well as the throw.
                    if (e.HResult != unchecked((int) 0x80131529))
                    {
                        return 8;
                    }
                }

                try
                {
                    WaitHandle.WaitAll(new WaitHandle[] { sem, sem });
                    return 2;
                }
                catch (DuplicateWaitObjectException)
                {
                }

                // A rejected wait-all must not have consumed anything on its
                // way to the exception.
                if (!sem.WaitOne(0))
                {
                    return 3;
                }

                sem.Release();

                // A three-element array whose duplicate is not adjacent still
                // has to be caught.
                try
                {
                    WaitHandle.WaitAll(new WaitHandle[] { signalled, sem, signalled });
                    return 4;
                }
                catch (DuplicateWaitObjectException)
                {
                }

                if (!sem.WaitOne(0))
                {
                    return 5;
                }

                sem.Release();

                // Distinct handles are of course fine.
                if (!WaitHandle.WaitAll(new WaitHandle[] { signalled, sem }))
                {
                    return 6;
                }

                sem.Release();

                // Duplicates are legal for a wait-any, and resolve to the
                // first occurrence.
                if (WaitHandle.WaitAny(new WaitHandle[] { signalled, signalled }) != 0)
                {
                    return 7;
                }

                return 0;
            }
        }
    }
}
