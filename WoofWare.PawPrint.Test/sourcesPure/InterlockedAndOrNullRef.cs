// `Interlocked.And` / `Interlocked.Or` document
// `<exception cref="NullReferenceException">The address of location1 is a null
// pointer.</exception>`. The managed body's first statement is a plain load of
// `location1`, so the fault comes from the dereference and the runtime raises the
// parameterless NullReferenceException — catchable guest state, not a runtime abort.
using System;
using System.Runtime.CompilerServices;
using System.Threading;

namespace InterlockedAndOrNullRef
{
    public class Program
    {
        public static int Main(string[] args)
        {
            try
            {
                Interlocked.Or(ref Unsafe.NullRef<int>(), 1);
                return 1;
            }
            catch (NullReferenceException)
            {
            }

            try
            {
                Interlocked.And(ref Unsafe.NullRef<int>(), 1);
                return 2;
            }
            catch (NullReferenceException)
            {
            }

            try
            {
                Interlocked.Or(ref Unsafe.NullRef<long>(), 1L);
                return 3;
            }
            catch (NullReferenceException)
            {
            }

            try
            {
                Interlocked.And(ref Unsafe.NullRef<long>(), 1L);
                return 4;
            }
            catch (NullReferenceException)
            {
            }

            // The unsigned overloads are `Unsafe.As`-to-signed forwarders, so they must
            // surface the same exception from the same underlying dereference.
            try
            {
                Interlocked.Or(ref Unsafe.NullRef<uint>(), 1U);
                return 5;
            }
            catch (NullReferenceException)
            {
            }

            try
            {
                Interlocked.And(ref Unsafe.NullRef<ulong>(), 1UL);
                return 6;
            }
            catch (NullReferenceException)
            {
            }

            // Execution continues normally afterwards: the exception left no wreckage
            // on the eval stack.
            int live = 0b0100;
            if (Interlocked.Or(ref live, 0b0011) != 0b0100 || live != 0b0111) return 7;

            return 0;
        }
    }
}
