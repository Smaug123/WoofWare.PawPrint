using System;
using System.Reflection.Emit;

public class Program
{
    // A `call` naming a `DynamicMethod` the guest never minted itself. Real .NET mints the callee
    // from inside `ResolveToken`, by calling `dm.GetMethodDescriptor()`
    // (`DynamicILGenerator.cs:797-801`) -- which is *managed guest code*: it takes `lock (this)`,
    // double-checks `_methodHandle`, throws `InvalidOperationException` for an empty body, and
    // otherwise runs `GetCallableMethod`, reaching the `ModuleHandle.GetDynamicMethod` QCall.
    // PawPrint does the same, by suspending the `call` for that managed callee and re-executing.
    //
    // The residual divergence from real .NET is *when* a method token is resolved: real .NET
    // resolves at JIT, before the body's first instruction, and PawPrint when the instruction is
    // reached. `DynamicScopeOperand.mintDynamicMethod` records the measurements and why matching
    // real .NET would cost more correctness than it buys. Nothing here tests it, because no guest
    // can observe it yet -- a body would have to have a side effect or a handler of its own before
    // the call site, and fields, metadata callees and exception regions are all still refused. So
    // every check below keeps its call sites on the executed path, where the two runtimes agree.
    //
    // Returns 0 on success, or the number of the first check that failed. Every expectation was
    // measured on the host's real .NET before being written down, because impure cases get no
    // automatic differential oracle; this program returns 0 there.

    private static DynamicMethod Unary(string name)
    {
        return new DynamicMethod(name, typeof(int), new Type[] { typeof(int) }, typeof(Program).Module);
    }

    private static Func<int, int> Mint(DynamicMethod dm)
    {
        return (Func<int, int>) dm.CreateDelegate(typeof(Func<int, int>));
    }

    // x |-> x + k, as its own dynamic method. Never minted by anything here.
    private static DynamicMethod Adder(string name, int k)
    {
        DynamicMethod dm = Unary(name);
        ILGenerator il = dm.GetILGenerator();
        il.Emit(OpCodes.Ldarg_0);
        il.Emit(OpCodes.Ldc_I4, k);
        il.Emit(OpCodes.Add);
        il.Emit(OpCodes.Ret);
        return dm;
    }

    public static int Main()
    {
        // 1. The basic shape, and the one #1015 refused: nothing binds a delegate to the callee
        // or invokes it, so its `_methodHandle` is still null when the caller's `call` runs.
        {
            DynamicMethod callee = Adder("Basic", 10);
            DynamicMethod caller = Unary("BasicCaller");
            ILGenerator il = caller.GetILGenerator();
            il.Emit(OpCodes.Ldarg_0);
            il.Emit(OpCodes.Call, callee);
            il.Emit(OpCodes.Ret);
            if (Mint(caller)(1) != 11)
            {
                return 1;
            }
        }

        // 2. A chain of never-minted callees, so the minting nests: `mid` is minted while `top` is
        // part-way through an instruction, and `bottom` while `mid` is. An implementation that
        // could only suspend once, or that lost the outer frame's position, fails here and not
        // above. The three constants are distinct powers of ten so a dropped level is visible in
        // the answer rather than merely wrong.
        {
            DynamicMethod bottom = Adder("Bottom", 1);
            DynamicMethod mid = Unary("Mid");
            {
                ILGenerator il = mid.GetILGenerator();
                il.Emit(OpCodes.Ldarg_0);
                il.Emit(OpCodes.Call, bottom);
                il.Emit(OpCodes.Ldc_I4, 20);
                il.Emit(OpCodes.Add);
                il.Emit(OpCodes.Ret);
            }
            DynamicMethod top = Unary("Top");
            {
                ILGenerator il = top.GetILGenerator();
                il.Emit(OpCodes.Ldarg_0);
                il.Emit(OpCodes.Call, mid);
                il.Emit(OpCodes.Ldc_I4, 300);
                il.Emit(OpCodes.Add);
                il.Emit(OpCodes.Ret);
            }
            if (Mint(top)(0) != 321)
            {
                return 2;
            }
        }

        // 3. Mutual recursion between two never-minted methods. This is what pins that minting a
        // callee does *not* resolve that callee's own tokens: minting `odd` while `even` runs must
        // not reach for `even` again. Measured on real .NET, where `CreateDelegate(even)` leaves
        // `odd` unminted and both answers are still right.
        {
            DynamicMethod even = Unary("Even");
            DynamicMethod odd = Unary("Odd");
            {
                ILGenerator il = even.GetILGenerator();
                Label nonZero = il.DefineLabel();
                il.Emit(OpCodes.Ldarg_0);
                il.Emit(OpCodes.Brtrue, nonZero);
                il.Emit(OpCodes.Ldc_I4_1);
                il.Emit(OpCodes.Ret);
                il.MarkLabel(nonZero);
                il.Emit(OpCodes.Ldarg_0);
                il.Emit(OpCodes.Ldc_I4_1);
                il.Emit(OpCodes.Sub);
                il.Emit(OpCodes.Call, odd);
                il.Emit(OpCodes.Ret);
            }
            {
                ILGenerator il = odd.GetILGenerator();
                Label nonZero = il.DefineLabel();
                il.Emit(OpCodes.Ldarg_0);
                il.Emit(OpCodes.Brtrue, nonZero);
                il.Emit(OpCodes.Ldc_I4_0);
                il.Emit(OpCodes.Ret);
                il.MarkLabel(nonZero);
                il.Emit(OpCodes.Ldarg_0);
                il.Emit(OpCodes.Ldc_I4_1);
                il.Emit(OpCodes.Sub);
                il.Emit(OpCodes.Call, even);
                il.Emit(OpCodes.Ret);
            }
            Func<int, int> isEven = Mint(even);
            if (isEven(4) != 1 || isEven(7) != 0 || isEven(0) != 1)
            {
                return 3;
            }
        }

        // 4. `EmitCall`'s `VarArgMethod` wrapper round a never-minted callee: the unwrap #1015
        // added and the mint this file adds have to compose, and neither test alone says so.
        {
            DynamicMethod callee = Adder("ViaEmitCall", 10);
            DynamicMethod caller = Unary("ViaEmitCallCaller");
            ILGenerator il = caller.GetILGenerator();
            il.Emit(OpCodes.Ldarg_0);
            il.EmitCall(OpCodes.Call, callee, null);
            il.Emit(OpCodes.Ret);
            if (Mint(caller)(1) != 11)
            {
                return 4;
            }
        }

        // 5. Having been minted by the caller, the callee binds to a delegate of its own and
        // answers the same thing: the mint produced the identity `CreateDelegate` would have, not
        // a second one that happens to work at the call site.
        {
            DynamicMethod callee = Adder("Reusable", 10);
            DynamicMethod caller = Unary("ReusableCaller");
            ILGenerator il = caller.GetILGenerator();
            il.Emit(OpCodes.Ldarg_0);
            il.Emit(OpCodes.Call, callee);
            il.Emit(OpCodes.Ret);
            if (Mint(caller)(1) != 11)
            {
                return 5;
            }
            if (Mint(callee)(1) != 11)
            {
                return 6;
            }
        }

        // 6. The callee's body is emitted *after* the caller has been bound to a delegate. Real
        // .NET bakes the callee when the caller first runs, which is later, so the 700-body wins.
        // This is the measurement that rules out minting the whole scope when the caller is
        // minted: that design answers 1 here.
        {
            DynamicMethod callee = Unary("LateBody");
            DynamicMethod caller = Unary("LateBodyCaller");
            {
                ILGenerator il = caller.GetILGenerator();
                il.Emit(OpCodes.Ldarg_0);
                il.Emit(OpCodes.Call, callee);
                il.Emit(OpCodes.Ret);
            }
            Func<int, int> bound = Mint(caller);
            {
                ILGenerator il = callee.GetILGenerator();
                il.Emit(OpCodes.Ldarg_0);
                il.Emit(OpCodes.Ldc_I4, 700);
                il.Emit(OpCodes.Add);
                il.Emit(OpCodes.Ret);
            }
            if (bound(1) != 701)
            {
                return 7;
            }
        }

        // 7. Two *different* never-minted callees named from one body, answering different
        // amounts, so an implementation that minted "the first unminted entry" rather than the one
        // the token names gives 20 or 2000 rather than 1010. Strings are interleaved so that an
        // off-by-one on the scope index lands on a wrong-kind entry -- a refusal -- rather than on
        // a coincidental match.
        {
            DynamicMethod first = Adder("TwoFirst", 10);
            DynamicMethod second = Adder("TwoSecond", 1000);
            DynamicMethod caller = Unary("TwoCaller");
            ILGenerator il = caller.GetILGenerator();
            il.Emit(OpCodes.Ldstr, "a");
            il.Emit(OpCodes.Pop);
            il.Emit(OpCodes.Ldarg_0);
            il.Emit(OpCodes.Call, first);
            il.Emit(OpCodes.Ldstr, "b");
            il.Emit(OpCodes.Pop);
            il.Emit(OpCodes.Call, second);
            il.Emit(OpCodes.Ret);
            if (Mint(caller)(0) != 1010)
            {
                return 8;
            }
        }

        // 8. A callee whose `ILGenerator` emitted nothing cannot be minted, and the
        // `InvalidOperationException` `GetMethodDescriptor` raises for it reaches the guest. This
        // is what proves the mint runs the real managed method rather than a native shortcut:
        // nothing but `GetMethodDescriptor` produces this exception, and a shortcut that read the
        // resolver's fields directly would have to fabricate it.
        {
            DynamicMethod callee = Unary("EmptyBody");
            callee.GetILGenerator(); // exists, but emits nothing, so ILOffset == 0
            DynamicMethod caller = Unary("EmptyBodyCaller");
            ILGenerator il = caller.GetILGenerator();
            il.Emit(OpCodes.Ldarg_0);
            il.Emit(OpCodes.Call, callee);
            il.Emit(OpCodes.Ret);
            Func<int, int> bound = Mint(caller);
            try
            {
                bound(0);
                return 9;
            }
            catch (InvalidOperationException)
            {
            }
        }

        // 9. The mint happens with the callee's arguments already on the caller's evaluation
        // stack, so anything the suspension leaves behind lands among them. A non-commutative
        // callee called with two distinct arguments answers 1 rather than -1 if they are swapped,
        // and fails outright if a third value appears beneath them.
        {
            DynamicMethod sub = new DynamicMethod("MintSub", typeof(int), new Type[] { typeof(int), typeof(int) }, typeof(Program).Module);
            {
                ILGenerator il = sub.GetILGenerator();
                il.Emit(OpCodes.Ldarg_0);
                il.Emit(OpCodes.Ldarg_1);
                il.Emit(OpCodes.Sub);
                il.Emit(OpCodes.Ret);
            }
            DynamicMethod caller = Unary("MintSubCaller");
            {
                ILGenerator il = caller.GetILGenerator();
                il.Emit(OpCodes.Ldc_I4_1);
                il.Emit(OpCodes.Ldc_I4_2);
                il.Emit(OpCodes.Call, sub);
                il.Emit(OpCodes.Ret);
            }
            if (Mint(caller)(0) != -1)
            {
                return 10;
            }
        }

        // 10. A void-returning never-minted callee. `GetMethodDescriptor` returns a
        // `RuntimeMethodHandle`, and the mint must throw it away rather than leave it where the
        // callee's own return value would go: an implementation that kept it leaves 7 under the
        // 33 and answers the wrong one.
        {
            DynamicMethod voidCallee = new DynamicMethod("MintVoid", typeof(void), new Type[] { typeof(int) }, typeof(Program).Module);
            {
                ILGenerator il = voidCallee.GetILGenerator();
                il.Emit(OpCodes.Ret);
            }
            DynamicMethod caller = Unary("MintVoidCaller");
            {
                ILGenerator il = caller.GetILGenerator();
                il.Emit(OpCodes.Ldc_I4_7);
                il.Emit(OpCodes.Call, voidCallee);
                il.Emit(OpCodes.Ldc_I4, 33);
                il.Emit(OpCodes.Ret);
            }
            if (Mint(caller)(0) != 33)
            {
                return 11;
            }
        }

        return 0;
    }
}
