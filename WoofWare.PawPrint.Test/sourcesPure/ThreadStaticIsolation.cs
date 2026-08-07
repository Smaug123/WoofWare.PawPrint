// A `[ThreadStatic]` field has one storage slot per thread: each thread's slot is
// zero-initialised on first access, and no thread can observe another's. This is a
// fact about any conforming CLI runtime (ECMA-335 / `System.ThreadStaticAttribute`),
// not a PawPrint-specific contract, so it belongs in the differential suite.
//
// The three field shapes are covered separately because PawPrint stores each in a
// different `CliType` shape: a primitive, an object reference, and a value type.
// The ordinary (non-thread-static) `s_shared` field is the control: it must stay
// shared, so a "give every static its own per-thread slot" over-correction fails here.
using System;
using System.Threading;

namespace ThreadStaticIsolation
{
    class Payload
    {
        public int Value;
    }

    struct Point
    {
        public int X;
        public int Y;
    }

    class Program
    {
        [ThreadStatic]
        static int t_int;

        [ThreadStatic]
        static Payload t_ref;

        [ThreadStatic]
        static Point t_struct;

        // Control: an ordinary static, shared by every thread.
        static int s_shared;

        // Ordinary statics used to report what the worker saw.
        static int workerObservedInt;
        static bool workerObservedRefWasNull;
        static int workerObservedStructX;
        static int workerObservedStructY;
        static int workerObservedShared;

        static void Worker()
        {
            workerObservedInt = t_int;
            workerObservedRefWasNull = (t_ref == null);
            workerObservedStructX = t_struct.X;
            workerObservedStructY = t_struct.Y;
            workerObservedShared = s_shared;

            // Now write the worker's own slots; the main thread must not see these.
            t_int = 7;
            t_ref = new Payload { Value = 8 };
            t_struct = new Point { X = 9, Y = 10 };
        }

        static int Main(string[] args)
        {
            t_int = 100;
            t_ref = new Payload { Value = 101 };
            t_struct = new Point { X = 102, Y = 103 };
            s_shared = 104;

            Thread t = new Thread(Worker);
            t.Start();
            t.Join();

            // The worker's first read of each thread-static must see the zero value,
            // never the main thread's writes.
            if (workerObservedInt != 0) return 1;
            if (!workerObservedRefWasNull) return 2;
            if (workerObservedStructX != 0) return 3;
            if (workerObservedStructY != 0) return 4;

            // ... but the ordinary static is genuinely shared.
            if (workerObservedShared != 104) return 5;

            // The main thread's slots survive the worker's writes to its own.
            if (t_int != 100) return 6;
            if (t_ref == null) return 7;
            if (t_ref.Value != 101) return 8;
            if (t_struct.X != 102) return 9;
            if (t_struct.Y != 103) return 10;

            return 0;
        }
    }
}
