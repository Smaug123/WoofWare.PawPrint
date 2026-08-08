using System;

// `stind` through a byref that points at a *pointer-typed* field.
//
// A `void*` / `delegate*<...>` field is stored as a runtime-pointer cell, which has no byte
// image at all, so the byte-scatter write path cannot service it — not even for a store of
// plain zero. The destination-cell test in `writeIndirectPrimitiveStore` has to notice that
// and route a same-width store to the typed writer, which replaces the whole cell.
//
// The shape shows up for real wherever a `ref`/`out` parameter is bound to a pointer-typed
// field: CoreLib's `RuntimeTypeHandle.GetActivationInfo` shim assigns all five of its results
// into `RuntimeType.ActivatorCache`'s fields exactly this way, which is how
// `Activator.CreateInstance` reaches it. This file covers the primitive on its own so a
// regression is not diagnosed as an Activator bug.
//
// Each check returns 0 when it holds; `Main` returns the number of the first that does not.

namespace PointerFieldIndirectStoreTest
{
    public unsafe class Holder
    {
        public void* Untyped;
        public int* Typed;
    }

    public unsafe class Program
    {
        private static void SetUntyped(void** slot, void* value)
        {
            *slot = value;
        }

        private static void SetTyped(int** slot, int* value)
        {
            *slot = value;
        }

        public static int Main(string[] args)
        {
            Holder h = new Holder();
            int local = 42;

            // Store a null through a byref to a pointer field. The cell already holds null, so
            // this is the case where nothing changes but the write still has to be *serviced*
            // rather than refused.
            fixed (void** slot = &h.Untyped)
            {
                SetUntyped(slot, null);
            }

            if (h.Untyped != null)
            {
                return 1;
            }

            // Store a real address the same way, and read it back through the field.
            fixed (int** slot = &h.Typed)
            {
                SetTyped(slot, &local);
            }

            if (h.Typed == null)
            {
                return 2;
            }

            if (*h.Typed != 42)
            {
                return 3;
            }

            // Overwrite a non-null pointer cell with null: the destination now carries pointer
            // provenance of its own, which is the other half of the same routing decision.
            fixed (int** slot = &h.Typed)
            {
                SetTyped(slot, null);
            }

            if (h.Typed != null)
            {
                return 4;
            }

            return 0;
        }
    }
}
