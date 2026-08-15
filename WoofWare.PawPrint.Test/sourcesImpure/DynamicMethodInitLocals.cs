using System;
using System.Reflection.Emit;

public class Program
{
    // When a dynamic method's `InitLocals` is read, and when it stops being readable.
    //
    // `DynamicMethod.InitLocals` has a setter that never latches (`DynamicMethod.cs`,
    // `set => _initLocals = value;`). CoreCLR reads it *late*: the managed
    // `DynamicResolver.GetCodeInfo` returns `m_method.InitLocals` at call time
    // (`DynamicILGenerator.cs:729`), and the native `LCGMethodResolver::GetCodeInfo`
    // (`vm/dynamicmethod.cpp`) calls that during the method's *first JIT*, caching the answer under
    // `if (!m_Code)`. So the flag is read after minting and fixed at first execution, and PawPrint
    // must do both: reading at mint is too early, re-reading per invocation is too late.
    //
    // On how much of this real .NET actually witnesses: PawPrint models uninitialised stack memory
    // as unreadable-until-written, so a wrongly-captured flag makes the reads below *refuse* rather
    // than return a wrong number. Real .NET instead returns unspecified bytes, and those bytes are
    // only usually non-zero -- measured on the host at the time of writing, a dynamic method with
    // `InitLocals = false` reading four unwritten bytes gave 0x00000000 on its first call and
    // 0x6D1F8F90 on every call after that. So real .NET agreeing that this program returns 0
    // establishes that the expectations here are *legal*, not that they are forced; it is PawPrint
    // that this pins tightly. Treat this as a PawPrint regression test whose expected value was
    // checked once against the real runtime, which is the standing of every impure case.
    //
    // Returns 0 on success, or the number of the first check that failed.

    /// `ldc.i4.4; localloc; ldind.i4; ret` -- four bytes of stack, read back without ever being
    /// written, which is the whole of what `initLocals` decides.
    private static DynamicMethod ReadUnwritten(string name)
    {
        DynamicMethod dm = new DynamicMethod(name, typeof(int), new Type[0], typeof(Program).Module);
        ILGenerator il = dm.GetILGenerator();
        il.Emit(OpCodes.Ldc_I4, 4);
        il.Emit(OpCodes.Localloc);
        il.Emit(OpCodes.Ldind_I4);
        il.Emit(OpCodes.Ret);
        return dm;
    }

    public static int Main(string[] args)
    {
        // Control: `localloc` runs at all in a dynamic method's frame. Writes before reading, so it
        // holds whichever way the flag went -- it isolates "localloc works in a synthesised frame"
        // from "the flag was captured correctly", and it is the only check here that a body
        // carrying `localloc` could pass before this slice, when such a body was refused outright.
        DynamicMethod control =
            new DynamicMethod("Control", typeof(int), new Type[0], typeof(Program).Module);
        ILGenerator cil = control.GetILGenerator();
        cil.Emit(OpCodes.Ldc_I4, 4);
        cil.Emit(OpCodes.Localloc);
        cil.Emit(OpCodes.Dup);
        cil.Emit(OpCodes.Ldc_I4, 7);
        cil.Emit(OpCodes.Stind_I4);
        cil.Emit(OpCodes.Ldind_I4);
        cil.Emit(OpCodes.Ret);
        if (((Func<int>) control.CreateDelegate(typeof(Func<int>)))() != 7)
        {
            return 1;
        }

        // Read late. `CreateDelegate` is where the method is minted, so assigning after it and
        // before the first invocation is precisely the window that separates the two readings:
        // an implementation that captured the flag at mint records `false` here, and the read
        // below then hits memory it is not allowed to read.
        DynamicMethod a = ReadUnwritten("A");
        a.InitLocals = false;
        Func<int> fa = (Func<int>) a.CreateDelegate(typeof(Func<int>));
        a.InitLocals = true;
        if (fa() != 0)
        {
            return 2;
        }

        // ...and latched there. The first invocation fixes the flag; assigning afterwards is legal
        // and does nothing, because on real .NET the method has already been compiled. An
        // implementation that re-read the field on every invocation would see `false` on the
        // second call.
        DynamicMethod b = ReadUnwritten("B");
        b.InitLocals = true;
        Func<int> fb = (Func<int>) b.CreateDelegate(typeof(Func<int>));
        if (fb() != 0)
        {
            return 3;
        }

        b.InitLocals = false;
        if (fb() != 0)
        {
            return 4;
        }

        return 0;
    }
}
