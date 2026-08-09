using System;

// Storing into a pointer-typed field through a byref *aliased to a different pointer-width
// primitive type* (`long*` / `double*` rather than `void**`).
//
// On a 64-bit runtime `stind.i8` and `stind.r8` are exact-width stores into a `void*` slot, so
// the width test that guards the pointer-cell routing in `writeIndirectPrimitiveStore` cannot
// tell them apart from the `stind.i` case. But an `Int64` or `Float64` payload has no pointer
// provenance to carry, so replacing the cell with it would change a `RuntimePointer` slot into
// a numeric one, and the *next* read of the field would push the wrong evaluation-stack kind.
//
// PawPrint therefore refuses these at the store, where the cause is visible, rather than
// restamping the cell and failing somewhere downstream. That is a real divergence from the
// runtime, which just writes eight bytes -- hence this file is parked.
//
// Each check returns 0 when it holds; `Main` returns the number of the first that does not.

namespace PointerFieldAliasedWidthStoreTest
{
    public unsafe class Holder
    {
        public void* Untyped;
    }

    public unsafe class Program
    {
        public static int Main(string[] args)
        {
            Holder h = new Holder();

            // Write the pointer slot as if it were a `long`. The real runtime zeroes the eight
            // bytes, leaving the field null.
            fixed (void** slot = &h.Untyped)
            {
                long* asLong = (long*) slot;
                *asLong = 0L;
            }

            if (h.Untyped != null)
            {
                return 1;
            }

            // Same again with a non-zero bit pattern, read back through the aliased view so the
            // check does not depend on the pointer surviving as a pointer.
            fixed (void** slot = &h.Untyped)
            {
                long* asLong = (long*) slot;
                *asLong = 0x1234L;

                if (*asLong != 0x1234L)
                {
                    return 2;
                }
            }

            return 0;
        }
    }
}
