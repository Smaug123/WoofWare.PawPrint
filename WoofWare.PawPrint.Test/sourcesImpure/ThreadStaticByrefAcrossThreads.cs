// A managed pointer to a `[ThreadStatic]` field is an *address*, not a late-bound "current
// thread's slot" indirection: `ldsflda` resolves to a concrete per-thread address when it
// executes. So a pointer taken on thread A and dereferenced on thread B still addresses **A's**
// slot - B does not silently get its own.
//
// This is the observable consequence of recording the owning thread in the pointer itself
// (`ByrefRoot.StaticField` carries a `StaticOwner`) rather than resolving it from whichever
// thread dereferences. An implementation that re-resolved against the reading thread passes
// every other thread-static test and fails only this one.
//
// Why this is an impure (PawPrint-only) case rather than a differential one, despite the
// semantics being a genuine CLI fact: the only way to move a byref across a thread boundary in
// C# is to launder it through a raw pointer, and in .NET 9+ a thread-static's storage lives in
// GC-heap `ThreadStaticBlock` objects. An unpinned pointer into those is movable, so on the
// real runtime this program is undefined behaviour - and observably so: `RealRuntime` loads the
// guest *in-process*, and under the allocation pressure of the full test suite a collection
// really does move the block between the capture and the read, whereupon the deref returns
// garbage. PawPrint's byrefs are symbolic and never move, so the assertion is exact here.
// `TestThreadStatics.fs` carries the same contract as a unit property; the cross-runtime facts
// about thread-static isolation live in `sourcesPure/ThreadStaticIsolation.cs`.
//
// The worker only *reads* through the pointer, never writes: this file must stay safe to run
// under a hypothetical future differential harness too.
using System;
using System.Runtime.CompilerServices;
using System.Threading;

namespace ThreadStaticByrefAcrossThreads
{
    unsafe class Program
    {
        [ThreadStatic]
        static int t_value;

        // Ordinary statics: shared, so the worker can find the captured address and report what
        // it saw.
        static long s_capturedPointer;
        static int s_workerObservedThroughPointer = -1;
        static int s_workerObservedOwnSlot = -1;

        static void Worker()
        {
            int* p = (int*) s_capturedPointer;
            s_workerObservedThroughPointer = *p;

            // The worker's own slot is untouched and therefore zero; if the pointer were
            // re-resolved against the reading thread, the two would agree.
            s_workerObservedOwnSlot = t_value;
        }

        static int Main(string[] args)
        {
            t_value = 12345;
            s_capturedPointer = (long) Unsafe.AsPointer(ref t_value);

            Thread t = new Thread(Worker);
            t.Start();
            t.Join();

            if (s_workerObservedThroughPointer != 12345) return 1;
            if (s_workerObservedOwnSlot != 0) return 2;

            // The capture did not disturb the main thread's own slot.
            if (t_value != 12345) return 3;

            return 0;
        }
    }
}
