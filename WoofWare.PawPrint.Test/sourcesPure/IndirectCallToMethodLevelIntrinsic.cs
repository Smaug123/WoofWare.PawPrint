using System;
using System.Runtime.CompilerServices;
using System.Runtime.Intrinsics;
using System.Runtime.Intrinsics.Arm;
using System.Runtime.Intrinsics.X86;

// Intrinsics reached through an entry point rather than named by a call instruction: a `calli`,
// a method-group delegate, and reflection-bound delegates. None of these is a call site the JIT
// can expand, and the file pins the two ways real .NET still gives the intrinsic's meaning:
//
//  * `Unsafe.As<T>(object)` has a method-level `[Intrinsic]`, and its IL in CoreLib is
//    `throw new PlatformNotSupportedException()`. CoreCLR substitutes the body whenever it
//    compiles the method (`getILIntrinsicImplementationForUnsafe`, jitinterface.cpp), so reaching
//    it indirectly must still reinterpret rather than throw.
//  * `IsSupported` on the hardware-intrinsic classes and `Vector128.IsHardwareAccelerated` are
//    recursive in IL (`get => IsSupported`) where the flavour supports them, and the recursive call
//    is a call instruction naming the intrinsic, so it is expanded there. Where the flavour does
//    not support them the body is a constant `false` instead; either way the delegate must agree
//    with the direct read.

public static unsafe class Program
{
    public static int Main(string[] args)
    {
        delegate*<object, string> asViaCalli = &Unsafe.As<string>;
        if (asViaCalli("calli") != "calli")
        {
            return 1;
        }

        Func<object, string> asViaDelegate = Unsafe.As<string>;
        if (asViaDelegate("delegate") != "delegate")
        {
            return 2;
        }

        Func<bool> hardwareAccelerated =
            typeof(Vector128).GetProperty("IsHardwareAccelerated").GetMethod.CreateDelegate<Func<bool>>();
        if (hardwareAccelerated() != Vector128.IsHardwareAccelerated)
        {
            return 3;
        }

        Func<bool> x86Base = typeof(X86Base).GetProperty("IsSupported").GetMethod.CreateDelegate<Func<bool>>();
        if (x86Base() != X86Base.IsSupported)
        {
            return 4;
        }

        Func<bool> advSimd = typeof(AdvSimd).GetProperty("IsSupported").GetMethod.CreateDelegate<Func<bool>>();
        if (advSimd() != AdvSimd.IsSupported)
        {
            return 5;
        }

        return 0;
    }
}
