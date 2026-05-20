using System;
using System.Runtime.CompilerServices;

// Reproduces the Bne_un_s struct-vs-NativeInt pattern the BCL uses in
// RuntimeType.InitializeCache: an IntPtr instance field (loaded via Ldfld
// as a raw NativeInt) compared to IntPtr.Zero (loaded via Ldsfld as a
// struct wrapping a NativeInt), branched via bne.un.s.
//
// Expression-bodied methods skip the debug-mode ceq-ceq-brfalse dance and
// emit Beq_s / Bne_un_s directly, so this shape is robust across Roslyn
// optimisation levels.
public class Program
{
    private IntPtr m_cache;

    [MethodImpl(MethodImplOptions.NoInlining)]
    int CheckEqZero() => (m_cache == IntPtr.Zero) ? 1 : 0;

    [MethodImpl(MethodImplOptions.NoInlining)]
    int CheckNeZero() => (m_cache != IntPtr.Zero) ? 1 : 0;

    static int Main(string[] args)
    {
        var p = new Program();

        // m_cache defaults to IntPtr.Zero.
        if (p.CheckEqZero() != 1) return 1;
        if (p.CheckNeZero() != 0) return 2;

        p.m_cache = new IntPtr(42);
        if (p.CheckEqZero() != 0) return 3;
        if (p.CheckNeZero() != 1) return 4;

        return 0;
    }
}
