using System;
using System.Reflection.Emit;

public class Program
{
    // Method-shaped operands in a dynamic method's body. The operand is `index | 0x06000000`, which
    // is a perfectly well-formed MethodDef token naming an unrelated real row, so this is about
    // resolving against the method's `DynamicScope` instead of against metadata. The scope entry is
    // the callee's `DynamicMethod` object itself -- `Emit(OpCode, MethodInfo)` has a dedicated
    // branch for that case and `GetTokenFor(DynamicMethod)` appends the builder rather than a handle
    // (DynamicILGenerator.cs:531-534) -- and CoreCLR's `ResolveToken` (DynamicILGenerator.cs:798)
    // reads `dm.GetMethodDescriptor().Value` at JIT.
    //
    // Every callee here is minted by the guest, via `CreateDelegate`, before the caller runs. That
    // is not incidental: `ResolveToken` also *mints* an unminted callee, by running the guest's
    // `GetMethodDescriptor` under a lock, and PawPrint cannot yet run a managed call from an IL op.
    // A caller naming a callee the guest never minted is refused loudly, which
    // `TestDynamicMethodMethodToken` pins; when that is implemented, cases 1, 9 and 11 of the
    // measurement table on the PR belong here.
    //
    // The one shape that needs nothing minted first is self-reference, and check 1 is it: CoreCLR
    // assigns `_methodHandle` before anything walks the body's tokens, so a method executing its own
    // `call` necessarily has one.
    //
    // Signatures and locals stay primitive throughout: a user-defined type in a signature takes
    // `SignatureHelper`'s `m_module == null` branch and emits ELEMENT_TYPE_INTERNAL, which PawPrint
    // cannot spell. That is a separate gap and this file does not touch it.
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

    // x |-> x + k, as its own dynamic method.
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
        // 1. Self-recursion, which is the check that could not pass if the entry were resolved when
        // the body was decoded: at that moment `_methodHandle` is still null, because the method
        // being decoded is the one the entry names. Sum of 5,4,3,2,1 = 15.
        DynamicMethod self = Unary("Self");
        {
            ILGenerator il = self.GetILGenerator();
            Label baseCase = il.DefineLabel();
            il.Emit(OpCodes.Ldarg_0);
            il.Emit(OpCodes.Ldc_I4_0);
            il.Emit(OpCodes.Ble, baseCase);
            il.Emit(OpCodes.Ldarg_0);
            il.Emit(OpCodes.Ldarg_0);
            il.Emit(OpCodes.Ldc_I4_1);
            il.Emit(OpCodes.Sub);
            il.Emit(OpCodes.Call, self);
            il.Emit(OpCodes.Add);
            il.Emit(OpCodes.Ret);
            il.MarkLabel(baseCase);
            il.Emit(OpCodes.Ldc_I4_0);
            il.Emit(OpCodes.Ret);
        }

        if (Mint(self)(5) != 15)
        {
            return 1;
        }

        // 2. A control, not a claim about tokens: `plus10` answers 10 when invoked directly, so a
        // failure of check 3 is about the `call` rather than about the callee.
        DynamicMethod plus10 = Adder("Plus10", 10);
        DynamicMethod plus1000 = Adder("Plus1000", 1000);
        Mint(plus1000);

        if (Mint(plus10)(0) != 10)
        {
            return 2;
        }

        // 3. Two *different* callees named from one body, answering different amounts, so an
        // implementation that resolved every method entry to the same thing -- the first one, say --
        // gives 20 or 2000 rather than 1010. String entries are interleaved so that an off-by-one on
        // the scope index lands on a wrong-kind entry (a refusal) rather than on a coincidental
        // match: `DynamicScope.GetTokenFor` appends without dedup, so the guest controls the layout,
        // which here is [null, sigblob, "first", plus10, "second", plus1000].
        DynamicMethod both = Unary("Both");
        {
            ILGenerator il = both.GetILGenerator();
            il.Emit(OpCodes.Ldstr, "first");
            il.Emit(OpCodes.Pop);
            il.Emit(OpCodes.Ldarg_0);
            il.Emit(OpCodes.Call, plus10);
            il.Emit(OpCodes.Ldstr, "second");
            il.Emit(OpCodes.Pop);
            il.Emit(OpCodes.Call, plus1000);
            il.Emit(OpCodes.Ret);
        }
        Mint(both);

        if (Mint(both)(0) != 1010)
        {
            return 3;
        }

        // 4. The callee's own body names further dynamic methods, so the scope a token indexes has
        // to be the *executing* method's rather than the outermost one. `chain`'s scope is
        // [null, sigblob, both, plus1000], so index 3 means `plus1000` here and `plus10` in `both`:
        // an implementation that read the caller's scope for a callee's token gives 1010 + 10.
        DynamicMethod chain = Unary("Chain");
        {
            ILGenerator il = chain.GetILGenerator();
            il.Emit(OpCodes.Ldarg_0);
            il.Emit(OpCodes.Call, both);
            il.Emit(OpCodes.Call, plus1000);
            il.Emit(OpCodes.Ret);
        }

        if (Mint(chain)(0) != 2010)
        {
            return 4;
        }

        // 5. Arguments and the return value cross the boundary in the right order: a non-commutative
        // callee called with two distinct arguments gives 1 rather than -1 if they are swapped.
        DynamicMethod sub = new DynamicMethod("Sub", typeof(int), new Type[] { typeof(int), typeof(int) }, typeof(Program).Module);
        {
            ILGenerator il = sub.GetILGenerator();
            il.Emit(OpCodes.Ldarg_0);
            il.Emit(OpCodes.Ldarg_1);
            il.Emit(OpCodes.Sub);
            il.Emit(OpCodes.Ret);
        }
        sub.CreateDelegate(typeof(Func<int, int, int>));

        DynamicMethod callsSub = Unary("CallsSub");
        {
            ILGenerator il = callsSub.GetILGenerator();
            il.Emit(OpCodes.Ldc_I4_1);
            il.Emit(OpCodes.Ldc_I4_2);
            il.Emit(OpCodes.Call, sub);
            il.Emit(OpCodes.Ret);
        }

        if (Mint(callsSub)(0) != -1)
        {
            return 5;
        }

        // 6. A void-returning callee, so the call site pushes nothing and the caller must not find a
        // stray value on its stack: a `call` that pushed one regardless would leave 7 under the 33
        // and return the wrong one.
        DynamicMethod voidCallee = new DynamicMethod("Void", typeof(void), new Type[] { typeof(int) }, typeof(Program).Module);
        {
            ILGenerator il = voidCallee.GetILGenerator();
            il.Emit(OpCodes.Ret);
        }
        voidCallee.CreateDelegate(typeof(Action<int>));

        DynamicMethod callsVoid = Unary("CallsVoid");
        {
            ILGenerator il = callsVoid.GetILGenerator();
            il.Emit(OpCodes.Ldc_I4_7);
            il.Emit(OpCodes.Call, voidCallee);
            il.Emit(OpCodes.Ldc_I4, 33);
            il.Emit(OpCodes.Ret);
        }

        if (Mint(callsVoid)(0) != 33)
        {
            return 6;
        }

        // 7. `EmitCall` rather than `Emit`, which is the *same call* spelled the other way and is
        // not an exotic vararg-only API: `GetMemberRefToken` wraps whatever it is given in a
        // `VarArgMethod` unconditionally, so this scope entry is a wrapper where every other one in
        // this file is a bare `DynamicMethod`. `ResolveToken` unwraps it and ignores the wrapper's
        // signature; a `DynamicMethod` is always `CallingConventions.Standard`, so `EmitCall` would
        // have thrown had this passed a non-null `optionalParameterTypes`.
        DynamicMethod callsViaEmitCall = Unary("CallsViaEmitCall");
        {
            ILGenerator il = callsViaEmitCall.GetILGenerator();
            il.Emit(OpCodes.Ldarg_0);
            il.EmitCall(OpCodes.Call, plus10, null);
            il.Emit(OpCodes.Ret);
        }

        if (Mint(callsViaEmitCall)(1) != 11)
        {
            return 7;
        }

        return 0;
    }
}
