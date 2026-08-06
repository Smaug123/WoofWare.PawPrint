using System;
using System.Runtime.InteropServices;
using System.Threading;

namespace HelloWorldApp
{
    class Program
    {
        // Storing a *provenance-carrying* IntPtr into stack- or native-memory
        // through a byte-view byref. PawPrint models a handle-shaped IntPtr as
        // a tagged native int (`NativeIntSource.WaitHandlePtr` here) with no
        // bit pattern at all, so such a value cannot be flattened to bytes: the
        // only way to store it is as a whole typed cell. The interpreter
        // already does that for a *bare* `StackMemoryByte` / `NativeMemoryByte`
        // byref, but the shape the C# below produces carries a trailing
        // `ReinterpretAs System.IntPtr` byte-view projection (the span's
        // `GetPinnableReference` / element indexer), and that shape used to
        // fail loud even though the layer beneath it can service it.
        //
        // `WaitHandle.ObtainSafeWaitHandles` — on the path to every
        // `WaitHandle.WaitAny` / `WaitAll` — writes exactly this shape, which
        // is how the gap was found; this file reproduces it without needing
        // any of the multi-wait machinery.
        //
        // Everything asserted here is a round-trip identity, so it holds
        // identically on the real runtime (where these are ordinary addresses)
        // and under PawPrint (where they are opaque tagged cells). No pointer
        // *value* is ever observed.
        static unsafe int Main(string[] args)
        {
            using (var sem = new Semaphore(0, 1))
            {
                IntPtr handle = sem.SafeWaitHandle.DangerousGetHandle();

                if (handle == IntPtr.Zero)
                {
                    return 1;
                }

                // Stack memory, constant index.
                Span<IntPtr> stack = stackalloc IntPtr[3];

                stack[1] = handle;

                if (stack[1] != handle)
                {
                    return 2;
                }

                // Neighbouring cells must be untouched: a whole-cell store of a
                // pointer-width value must not smear over its siblings.
                if (stack[0] != IntPtr.Zero || stack[2] != IntPtr.Zero)
                {
                    return 3;
                }

                // Stack memory, non-constant index — Roslyn emits a different
                // offset computation for a loop variable than for a literal, so
                // both shapes are worth pinning.
                for (int i = 0; i < stack.Length; i++)
                {
                    stack[i] = handle;
                }

                for (int i = 0; i < stack.Length; i++)
                {
                    if (stack[i] != handle)
                    {
                        return 4;
                    }
                }

                // Native memory: the same byte-view shape over a
                // `NativeMemoryByte` root rather than a `StackMemoryByte` one.
                IntPtr* native = (IntPtr*) NativeMemory.AllocZeroed (3, (nuint) sizeof (IntPtr));

                try
                {
                    native[1] = handle;

                    if (native[1] != handle)
                    {
                        return 5;
                    }

                    if (native[0] != IntPtr.Zero || native[2] != IntPtr.Zero)
                    {
                        return 6;
                    }

                    for (int i = 0; i < 3; i++)
                    {
                        native[i] = handle;
                    }

                    for (int i = 0; i < 3; i++)
                    {
                        if (native[i] != handle)
                        {
                            return 7;
                        }
                    }
                }
                finally
                {
                    NativeMemory.Free (native);
                }

                // Overwriting a handle slot with an ordinary value. The store
                // itself is byte-addressable, but its destination is a cell
                // that carries provenance and therefore has no bytes to
                // scatter over; the only representable outcome is exact-width
                // whole-cell replacement. `Span<T>.Clear()` and plain
                // reassignment both take this path, so it is a routine
                // sequence rather than an exotic one.
                stack[0] = IntPtr.Zero;

                if (stack[0] != IntPtr.Zero)
                {
                    return 8;
                }

                if (stack[1] != handle || stack[2] != handle)
                {
                    return 9;
                }

                stack.Clear ();

                for (int i = 0; i < stack.Length; i++)
                {
                    if (stack[i] != IntPtr.Zero)
                    {
                        return 10;
                    }
                }

                return 0;
            }
        }
    }
}
