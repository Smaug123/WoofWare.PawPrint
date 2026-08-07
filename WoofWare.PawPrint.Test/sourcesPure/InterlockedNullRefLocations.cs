// Every `Interlocked` overload documents
// `<exception cref="NullReferenceException">The address of location1 is a null
// pointer.</exception>`, and each managed body opens by dereferencing the location,
// so the fault is the load and the runtime raises the parameterless
// NullReferenceException. This must be catchable guest state, not a runtime abort.
//
// One case per intrinsic shape in `Intrinsics.fs`: the Add/ExchangeAdd int32 and
// int64 helpers, and the scalar / native-int / reference-type / enum shapes of
// both CompareExchange and Exchange.
using System;
using System.Runtime.CompilerServices;
using System.Threading;

namespace InterlockedNullRefLocations
{
    public class Program
    {
        class Marker
        {
        }

        enum States
        {
            NotCanceled = 0,
            Notifying = 1,
        }

        public static int Main(string[] args)
        {
            // Add / ExchangeAdd, int32 helper.
            try
            {
                Interlocked.Add(ref Unsafe.NullRef<int>(), 1);
                return 1;
            }
            catch (NullReferenceException)
            {
            }

            // Increment and Decrement are plain managed wrappers over ExchangeAdd,
            // so they must surface the same exception from the same helper.
            try
            {
                Interlocked.Increment(ref Unsafe.NullRef<int>());
                return 2;
            }
            catch (NullReferenceException)
            {
            }

            try
            {
                Interlocked.Decrement(ref Unsafe.NullRef<int>());
                return 3;
            }
            catch (NullReferenceException)
            {
            }

            // Add / ExchangeAdd, int64 helper.
            try
            {
                Interlocked.Add(ref Unsafe.NullRef<long>(), 1L);
                return 4;
            }
            catch (NullReferenceException)
            {
            }

            // The unsigned overloads are `Unsafe.As`-to-signed forwarders onto the
            // same two helpers.
            try
            {
                Interlocked.Add(ref Unsafe.NullRef<uint>(), 1U);
                return 5;
            }
            catch (NullReferenceException)
            {
            }

            try
            {
                Interlocked.Add(ref Unsafe.NullRef<ulong>(), 1UL);
                return 6;
            }
            catch (NullReferenceException)
            {
            }

            // CompareExchange, scalar-integral shape.
            try
            {
                Interlocked.CompareExchange(ref Unsafe.NullRef<int>(), 1, 0);
                return 7;
            }
            catch (NullReferenceException)
            {
            }

            try
            {
                Interlocked.CompareExchange(ref Unsafe.NullRef<long>(), 1L, 0L);
                return 8;
            }
            catch (NullReferenceException)
            {
            }

            // Read(ref long) is `CompareExchange(ref location, 0, 0)`.
            try
            {
                Interlocked.Read(ref Unsafe.NullRef<long>());
                return 9;
            }
            catch (NullReferenceException)
            {
            }

            // CompareExchange, native-int shape.
            try
            {
                Interlocked.CompareExchange(ref Unsafe.NullRef<IntPtr>(), new IntPtr(1), IntPtr.Zero);
                return 10;
            }
            catch (NullReferenceException)
            {
            }

            // CompareExchange, reference-type shape.
            Marker marker = new Marker();
            try
            {
                ref Marker location = ref Unsafe.NullRef<Marker>();
                Interlocked.CompareExchange(ref location, marker, null);
                return 11;
            }
            catch (NullReferenceException)
            {
            }

            // CompareExchange, enum shape.
            try
            {
                Interlocked.CompareExchange(ref Unsafe.NullRef<States>(), States.Notifying, States.NotCanceled);
                return 12;
            }
            catch (NullReferenceException)
            {
            }

            // Exchange, scalar-integral shape.
            try
            {
                Interlocked.Exchange(ref Unsafe.NullRef<int>(), 1);
                return 13;
            }
            catch (NullReferenceException)
            {
            }

            try
            {
                Interlocked.Exchange(ref Unsafe.NullRef<long>(), 1L);
                return 14;
            }
            catch (NullReferenceException)
            {
            }

            // Exchange, native-int shape.
            try
            {
                Interlocked.Exchange(ref Unsafe.NullRef<IntPtr>(), new IntPtr(1));
                return 15;
            }
            catch (NullReferenceException)
            {
            }

            // Exchange, reference-type shape.
            try
            {
                ref Marker location = ref Unsafe.NullRef<Marker>();
                Interlocked.Exchange(ref location, marker);
                return 16;
            }
            catch (NullReferenceException)
            {
            }

            // Exchange, enum shape.
            try
            {
                Interlocked.Exchange(ref Unsafe.NullRef<States>(), States.Notifying);
                return 17;
            }
            catch (NullReferenceException)
            {
            }

            // Execution continues normally afterwards: none of the aborted intrinsics
            // left wreckage on the eval stack.
            int live = 5;
            if (Interlocked.Add(ref live, 3) != 8 || live != 8) return 18;
            if (Interlocked.Exchange(ref live, 20) != 8 || live != 20) return 19;
            if (Interlocked.CompareExchange(ref live, 30, 20) != 20 || live != 30) return 20;

            States liveState = States.NotCanceled;
            if (Interlocked.CompareExchange(ref liveState, States.Notifying, States.NotCanceled) != States.NotCanceled
                || liveState != States.Notifying) return 21;
            if (Interlocked.Exchange(ref liveState, States.NotCanceled) != States.Notifying
                || liveState != States.NotCanceled) return 22;

            Marker slot = null;
            if (Interlocked.Exchange(ref slot, marker) != null || slot != marker) return 23;

            return 0;
        }
    }
}
