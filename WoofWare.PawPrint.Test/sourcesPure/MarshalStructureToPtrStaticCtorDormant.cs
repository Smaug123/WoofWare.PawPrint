using System;
using System.Runtime.InteropServices;

public class Program
{
    public static int Marker;

    // An explicit static constructor makes `S` non-beforefieldinit, so the CLR must run it at a
    // precisely specified moment — and marshalling is not one of those moments. Nothing here
    // touches a static field of `S`, calls a method on it, or constructs one: `default(S)` and
    // `Marshal.StructureToPtr` both leave the initialiser dormant, so `Marker` stays 0.
    //
    // This is the observable that keeps PawPrint honest about what a synthesised marshal stub
    // *is*. The stub is declared on `S` so that its identity is one-per-marshalled-type, which is
    // the identity CoreCLR's per-MethodTable stub cache has — but it is not a *member* of `S`, and
    // CoreCLR underlines the point by putting such stubs in their own `ILStubClass`. Treating the
    // declaring type as an owner, and running its initialiser before entering the stub, would
    // manufacture a side effect the real runtime does not have.
    [StructLayout(LayoutKind.Sequential)]
    public struct S
    {
        static S()
        {
            Program.Marker = 42;
        }

        public DateTime D;
    }

    public static int Main(string[] args)
    {
        S s = default;

        // Nothing so far should have run the initialiser.
        if (Marker != 0) return 1;

        IntPtr p = Marshal.AllocHGlobal(Marshal.SizeOf<S>());
        try
        {
            // A DateTime field routes this through the synthesised struct-marshal stub rather
            // than the blittable memmove path, which is what makes the stub's declaring type
            // observable at all.
            Marshal.StructureToPtr(s, p, false);

            // Still dormant: 0 if the stub left `S` alone, 42 if entering it ran the initialiser.
            return Marker;
        }
        finally
        {
            Marshal.FreeHGlobal(p);
        }
    }
}
