using System;
using System.Reflection.Emit;

// Exception-handling regions in a `DynamicMethod` body. The clauses live on the resolver's
// `m_exceptions` as `__ExceptionInfo` records, which `DynamicResolver.GetEHInfo` projects into
// clauses for the JIT; PawPrint performs the same projection when the method is minted, and
// resolves each `catch` clause's type -- a `DynamicScope` index, not a metadata token -- when the
// method is first prepared for execution, which is where CoreCLR's JIT resolves it.
//
// Two constraints shape every case, and both are about what a dynamic method body can reach
// today rather than about exception handling. A dynamic method body cannot *construct* an exception --
// `newobj` naming a reflected `ConstructorInfo` stops at `RuntimeMethodHandle::GetMethodDef` -- so
// exception objects are passed in, and the signature is `(object, object) -> int`: `object` is one
// of `SignatureHelper.IsSimpleType`'s types, so the signature needs no `ELEMENT_TYPE_INTERNAL`.
// And a dynamic method body cannot touch a field, so a cleanup clause running on the *exceptional*
// path has no local left to report through: it reports by throwing a *different* exception, which
// the caller then sees instead of the original. That channel needs only `ldarg` and `throw`.

public class Program
{
    private static DynamicMethod New(string name) =>
        new DynamicMethod(name, typeof(int), new Type[] { typeof(object), typeof(object), typeof(object) }, typeof(Program).Module);

    private static Func<object, object, object, int> Mint(DynamicMethod dm) =>
        (Func<object, object, object, int>) dm.CreateDelegate(typeof(Func<object, object, object, int>));

    // The type of the exception `f` throws, or null if it returned.
    private static Type Threw(Func<object, object, object, int> f, object a, object b, out int returned)
    {
        try
        {
            returned = f(a, b, null);
            return null;
        }
        catch (Exception e)
        {
            returned = 0;
            return e.GetType();
        }
    }

    // Returns 0 on success, or the number of the first check that failed. Every expectation was
    // measured on the host's real .NET before being written down, because impure cases get no
    // automatic differential oracle; this program returns 0 there.
    public static int Main()
    {
        Exception ioe = new InvalidOperationException();
        Exception ovf = new OverflowException();

        // 1. A catch clause that matches. The clause's type is a DynamicScope entry, so this one
        // line is the whole feature.
        {
            DynamicMethod dm = New("Caught");
            ILGenerator il = dm.GetILGenerator();
            LocalBuilder loc = il.DeclareLocal(typeof(int));
            il.BeginExceptionBlock();
            il.Emit(OpCodes.Ldarg_0);
            il.Emit(OpCodes.Throw);
            il.BeginCatchBlock(typeof(InvalidOperationException));
            il.Emit(OpCodes.Pop);
            il.Emit(OpCodes.Ldc_I4, 42);
            il.Emit(OpCodes.Stloc, loc);
            il.EndExceptionBlock();
            il.Emit(OpCodes.Ldloc, loc);
            il.Emit(OpCodes.Ret);
            if (Mint(dm)(ioe, null, null) != 42)
            {
                return 1;
            }
        }

        // 2. A catch clause that does *not* match, so the exception leaves the dynamic method and
        // this ordinary frame catches it. An implementation resolving the clause type to something
        // wrong-but-plausible -- `System.Object`, or whatever real metadata row the token's bits
        // happen to name -- passes check 1 and fails here.
        {
            DynamicMethod dm = New("NotCaught");
            ILGenerator il = dm.GetILGenerator();
            LocalBuilder loc = il.DeclareLocal(typeof(int));
            il.BeginExceptionBlock();
            il.Emit(OpCodes.Ldarg_0);
            il.Emit(OpCodes.Throw);
            il.BeginCatchBlock(typeof(OverflowException));
            il.Emit(OpCodes.Pop);
            il.Emit(OpCodes.Ldc_I4, 42);
            il.Emit(OpCodes.Stloc, loc);
            il.EndExceptionBlock();
            il.Emit(OpCodes.Ldloc, loc);
            il.Emit(OpCodes.Ret);
            int got;
            if (Threw(Mint(dm), ioe, null, out got) != typeof(InvalidOperationException))
            {
                return 2;
            }
        }

        // 3. Catching a supertype: `catch (Exception)` takes an InvalidOperationException. A clause
        // compared by type *identity* rather than by assignability passes 1 and fails here.
        {
            DynamicMethod dm = New("Supertype");
            ILGenerator il = dm.GetILGenerator();
            LocalBuilder loc = il.DeclareLocal(typeof(int));
            il.BeginExceptionBlock();
            il.Emit(OpCodes.Ldarg_0);
            il.Emit(OpCodes.Throw);
            il.BeginCatchBlock(typeof(Exception));
            il.Emit(OpCodes.Pop);
            il.Emit(OpCodes.Ldc_I4, 3);
            il.Emit(OpCodes.Stloc, loc);
            il.EndExceptionBlock();
            il.Emit(OpCodes.Ldloc, loc);
            il.Emit(OpCodes.Ret);
            if (Mint(dm)(ioe, null, null) != 3)
            {
                return 3;
            }
        }

        // 4. Two catch clauses on one try, the first not matching. Both come from a single
        // `__ExceptionInfo` whose `m_currentCatch` is 2, so an implementation reading one clause
        // per region -- or reading the over-allocated arrays' full length -- differs here.
        {
            DynamicMethod dm = New("TwoClauses");
            ILGenerator il = dm.GetILGenerator();
            LocalBuilder loc = il.DeclareLocal(typeof(int));
            il.BeginExceptionBlock();
            il.Emit(OpCodes.Ldarg_0);
            il.Emit(OpCodes.Throw);
            il.BeginCatchBlock(typeof(OverflowException));
            il.Emit(OpCodes.Pop);
            il.Emit(OpCodes.Ldc_I4, 40);
            il.Emit(OpCodes.Stloc, loc);
            il.BeginCatchBlock(typeof(InvalidOperationException));
            il.Emit(OpCodes.Pop);
            il.Emit(OpCodes.Ldc_I4, 4);
            il.Emit(OpCodes.Stloc, loc);
            il.EndExceptionBlock();
            il.Emit(OpCodes.Ldloc, loc);
            il.Emit(OpCodes.Ret);
            if (Mint(dm)(ioe, null, null) != 4)
            {
                return 4;
            }
        }

        // 5. Nested try where the inner clause does not match and the outer does: two
        // `__ExceptionInfo`s, which `SortExceptions` hands back innermost-first.
        {
            DynamicMethod dm = New("NestedOuter");
            ILGenerator il = dm.GetILGenerator();
            LocalBuilder loc = il.DeclareLocal(typeof(int));
            il.BeginExceptionBlock();
            il.BeginExceptionBlock();
            il.Emit(OpCodes.Ldarg_0);
            il.Emit(OpCodes.Throw);
            il.BeginCatchBlock(typeof(OverflowException));
            il.Emit(OpCodes.Pop);
            il.Emit(OpCodes.Ldc_I4, 50);
            il.Emit(OpCodes.Stloc, loc);
            il.EndExceptionBlock();
            il.BeginCatchBlock(typeof(InvalidOperationException));
            il.Emit(OpCodes.Pop);
            il.Emit(OpCodes.Ldc_I4, 5);
            il.Emit(OpCodes.Stloc, loc);
            il.EndExceptionBlock();
            il.Emit(OpCodes.Ldloc, loc);
            il.Emit(OpCodes.Ret);
            if (Mint(dm)(ioe, null, null) != 5)
            {
                return 5;
            }
        }

        // 6. The same two regions decided the other way: the inner clause matches, and must win
        // over an outer clause that would also have accepted. Together with 5, this pins that the
        // choice is by coverage and assignability rather than by position in the region list.
        {
            DynamicMethod dm = New("NestedInner");
            ILGenerator il = dm.GetILGenerator();
            LocalBuilder loc = il.DeclareLocal(typeof(int));
            il.BeginExceptionBlock();
            il.BeginExceptionBlock();
            il.Emit(OpCodes.Ldarg_0);
            il.Emit(OpCodes.Throw);
            il.BeginCatchBlock(typeof(InvalidOperationException));
            il.Emit(OpCodes.Pop);
            il.Emit(OpCodes.Ldc_I4, 6);
            il.Emit(OpCodes.Stloc, loc);
            il.EndExceptionBlock();
            il.BeginCatchBlock(typeof(Exception));
            il.Emit(OpCodes.Pop);
            il.Emit(OpCodes.Ldc_I4, 60);
            il.Emit(OpCodes.Stloc, loc);
            il.EndExceptionBlock();
            il.Emit(OpCodes.Ldloc, loc);
            il.Emit(OpCodes.Ret);
            if (Mint(dm)(ioe, null, null) != 6)
            {
                return 6;
            }
        }

        // 7. `finally` on the normal path. A finally clause's `TryLength` is computed from
        // `m_endFinally` where every other clause kind uses `m_endAddr` -- the one arithmetic
        // special case in `GetEHInfo` -- so a projection that missed it covers the wrong range.
        {
            DynamicMethod dm = New("FinallyNormal");
            ILGenerator il = dm.GetILGenerator();
            LocalBuilder loc = il.DeclareLocal(typeof(int));
            il.Emit(OpCodes.Ldc_I4_0);
            il.Emit(OpCodes.Stloc, loc);
            il.BeginExceptionBlock();
            il.Emit(OpCodes.Ldloc, loc);
            il.Emit(OpCodes.Ldc_I4, 10);
            il.Emit(OpCodes.Add);
            il.Emit(OpCodes.Stloc, loc);
            il.BeginFinallyBlock();
            il.Emit(OpCodes.Ldloc, loc);
            il.Emit(OpCodes.Ldc_I4, 5);
            il.Emit(OpCodes.Add);
            il.Emit(OpCodes.Stloc, loc);
            il.EndExceptionBlock();
            il.Emit(OpCodes.Ldloc, loc);
            il.Emit(OpCodes.Ret);
            if (Mint(dm)(null, null, null) != 15)
            {
                return 7;
            }
        }

        // 8. `finally` on the *exceptional* path, which check 7 cannot see: an implementation that
        // decoded the regions and then unwound straight past the frame still answers 15 there. The
        // clause reports by throwing arg1, so the caller sees an OverflowException where the body
        // threw an InvalidOperationException.
        {
            DynamicMethod dm = New("FinallyExceptional");
            ILGenerator il = dm.GetILGenerator();
            il.BeginExceptionBlock();
            il.Emit(OpCodes.Ldarg_0);
            il.Emit(OpCodes.Throw);
            il.BeginFinallyBlock();
            il.Emit(OpCodes.Ldarg_1);
            il.Emit(OpCodes.Throw);
            il.EndExceptionBlock();
            il.Emit(OpCodes.Ldc_I4_0);
            il.Emit(OpCodes.Ret);
            int got;
            if (Threw(Mint(dm), ioe, ovf, out got) != typeof(OverflowException))
            {
                return 8;
            }
        }

        // 9. `fault` runs on the exceptional path...
        {
            DynamicMethod dm = New("FaultExceptional");
            ILGenerator il = dm.GetILGenerator();
            il.BeginExceptionBlock();
            il.Emit(OpCodes.Ldarg_0);
            il.Emit(OpCodes.Throw);
            il.BeginFaultBlock();
            il.Emit(OpCodes.Ldarg_1);
            il.Emit(OpCodes.Throw);
            il.EndExceptionBlock();
            il.Emit(OpCodes.Ldc_I4_0);
            il.Emit(OpCodes.Ret);
            int got;
            if (Threw(Mint(dm), ioe, ovf, out got) != typeof(OverflowException))
            {
                return 9;
            }
        }

        // 10. ...and not on the normal one, which is the whole difference between `fault` and
        // `finally`. Without this, an implementation treating Fault as Finally passes check 9.
        {
            DynamicMethod dm = New("FaultNormal");
            ILGenerator il = dm.GetILGenerator();
            LocalBuilder loc = il.DeclareLocal(typeof(int));
            il.BeginExceptionBlock();
            il.Emit(OpCodes.Ldc_I4, 10);
            il.Emit(OpCodes.Stloc, loc);
            il.BeginFaultBlock();
            il.Emit(OpCodes.Ldarg_1);
            il.Emit(OpCodes.Throw);
            il.EndExceptionBlock();
            il.Emit(OpCodes.Ldloc, loc);
            il.Emit(OpCodes.Ret);
            if (Mint(dm)(null, ovf, null) != 10)
            {
                return 10;
            }
        }

        // 11. A filter that accepts. Its `ClassTokenOrFilterOffset` is an IL offset rather than a
        // scope token, so an implementation reading every clause's slot as a token resolves this
        // one against an arbitrary scope entry.
        {
            DynamicMethod dm = New("FilterAccepts");
            ILGenerator il = dm.GetILGenerator();
            LocalBuilder loc = il.DeclareLocal(typeof(int));
            il.BeginExceptionBlock();
            il.Emit(OpCodes.Ldarg_0);
            il.Emit(OpCodes.Throw);
            il.BeginExceptFilterBlock();
            il.Emit(OpCodes.Pop);
            il.Emit(OpCodes.Ldc_I4_1);
            il.BeginCatchBlock(null);
            il.Emit(OpCodes.Pop);
            il.Emit(OpCodes.Ldc_I4, 11);
            il.Emit(OpCodes.Stloc, loc);
            il.EndExceptionBlock();
            il.Emit(OpCodes.Ldloc, loc);
            il.Emit(OpCodes.Ret);
            if (Mint(dm)(ioe, null, null) != 11)
            {
                return 11;
            }
        }

        // 12. A filter that rejects, so the exception escapes despite a covering clause.
        {
            DynamicMethod dm = New("FilterRejects");
            ILGenerator il = dm.GetILGenerator();
            LocalBuilder loc = il.DeclareLocal(typeof(int));
            il.BeginExceptionBlock();
            il.Emit(OpCodes.Ldarg_0);
            il.Emit(OpCodes.Throw);
            il.BeginExceptFilterBlock();
            il.Emit(OpCodes.Pop);
            il.Emit(OpCodes.Ldc_I4_0);
            il.BeginCatchBlock(null);
            il.Emit(OpCodes.Pop);
            il.Emit(OpCodes.Ldc_I4, 120);
            il.Emit(OpCodes.Stloc, loc);
            il.EndExceptionBlock();
            il.Emit(OpCodes.Ldloc, loc);
            il.Emit(OpCodes.Ret);
            int got;
            if (Threw(Mint(dm), ioe, null, out got) != typeof(InvalidOperationException))
            {
                return 12;
            }
        }

        // 13. `rethrow` from a dynamic method's catch: the original exception continues, so the
        // caller sees the type the body was handed rather than the clause's type.
        {
            DynamicMethod dm = New("Rethrown");
            ILGenerator il = dm.GetILGenerator();
            il.BeginExceptionBlock();
            il.Emit(OpCodes.Ldarg_0);
            il.Emit(OpCodes.Throw);
            il.BeginCatchBlock(typeof(Exception));
            il.Emit(OpCodes.Pop);
            il.Emit(OpCodes.Rethrow);
            il.EndExceptionBlock();
            il.Emit(OpCodes.Ldc_I4_0);
            il.Emit(OpCodes.Ret);
            int got;
            if (Threw(Mint(dm), ioe, null, out got) != typeof(InvalidOperationException))
            {
                return 13;
            }
        }

        // 14. A value-type catch clause. `BeginCatchBlock` accepts any `RuntimeType`, and measured
        // on real .NET the clause is legal and never matches -- so this must *not* be
        // refused, and must not accidentally match either.
        {
            DynamicMethod dm = New("CatchValueType");
            ILGenerator il = dm.GetILGenerator();
            LocalBuilder loc = il.DeclareLocal(typeof(int));
            il.BeginExceptionBlock();
            il.Emit(OpCodes.Ldarg_0);
            il.Emit(OpCodes.Throw);
            il.BeginCatchBlock(typeof(int));
            il.Emit(OpCodes.Pop);
            il.Emit(OpCodes.Ldc_I4, 140);
            il.Emit(OpCodes.Stloc, loc);
            il.EndExceptionBlock();
            il.Emit(OpCodes.Ldloc, loc);
            il.Emit(OpCodes.Ret);
            int got;
            if (Threw(Mint(dm), ioe, null, out got) != typeof(InvalidOperationException))
            {
                return 14;
            }
        }

        // 15. One dynamic method's catch clause catching what another dynamic method threw, with
        // the two scopes deliberately disagreeing at the colliding index: the callee's entry 3 is
        // a string and the caller's is the clause type. An implementation resolving a clause
        // against whichever scope happens to be executing -- rather than against the scope of the
        // method the clause belongs to -- reads the wrong entry here and not in any single-method
        // case above.
        {
            DynamicMethod callee = New("Inner");
            {
                ILGenerator il = callee.GetILGenerator();
                il.Emit(OpCodes.Ldstr, "pad");
                il.Emit(OpCodes.Pop);
                il.Emit(OpCodes.Ldarg_0);
                il.Emit(OpCodes.Throw);
            }
            DynamicMethod caller = New("Outer");
            {
                ILGenerator il = caller.GetILGenerator();
                LocalBuilder loc = il.DeclareLocal(typeof(int));
                il.BeginExceptionBlock();
                il.Emit(OpCodes.Ldarg_0);
                il.Emit(OpCodes.Ldarg_1);
                il.Emit(OpCodes.Ldarg_2);
                il.Emit(OpCodes.Call, callee);
                il.Emit(OpCodes.Stloc, loc);
                il.BeginCatchBlock(typeof(InvalidOperationException));
                il.Emit(OpCodes.Pop);
                il.Emit(OpCodes.Ldc_I4, 15);
                il.Emit(OpCodes.Stloc, loc);
                il.EndExceptionBlock();
                il.Emit(OpCodes.Ldloc, loc);
                il.Emit(OpCodes.Ret);
            }
            if (Mint(caller)(ioe, null, null) != 15)
            {
                return 15;
            }
        }

        // 16. `try/catch/finally` on one region, where what is observed is that the `finally` runs
        // *after* the `catch` handler. A finally clause's try range covers the handler as well as
        // the try body -- measured on this exact body, the catch clause covers [0,+7) and the
        // finally [0,+20), with the handler at [7,15) -- and that is the whole reason its length
        // comes from `m_endFinally` where every other kind uses `m_endAddr`.
        //
        // Check 7 cannot see this, and neither can "did the finally run at all": for a
        // `try/finally` with no catch, `m_endAddr` and `m_endFinally` are the same number, and a
        // finally whose range stops at the try body still runs -- just too early, before the
        // handler, because the second pass then reads it as lying between the throw point and the
        // handler rather than enclosing it. So the handler leaves a mark and the finally reads it.
        {
            DynamicMethod dm = New("FinallyOverCatchHandler");
            ILGenerator il = dm.GetILGenerator();
            LocalBuilder ran = il.DeclareLocal(typeof(int));
            il.Emit(OpCodes.Ldc_I4_0);
            il.Emit(OpCodes.Stloc, ran);
            il.BeginExceptionBlock();
            il.Emit(OpCodes.Ldarg_0);
            il.Emit(OpCodes.Throw);
            il.BeginCatchBlock(typeof(InvalidOperationException));
            il.Emit(OpCodes.Pop);
            il.Emit(OpCodes.Ldc_I4, 16);
            il.Emit(OpCodes.Stloc, ran);
            il.BeginFinallyBlock();
            Label handlerAlreadyRan = il.DefineLabel();
            il.Emit(OpCodes.Ldloc, ran);
            il.Emit(OpCodes.Brtrue, handlerAlreadyRan);
            il.Emit(OpCodes.Ldarg_1);
            il.Emit(OpCodes.Throw);
            il.MarkLabel(handlerAlreadyRan);
            il.EndExceptionBlock();
            il.Emit(OpCodes.Ldloc, ran);
            il.Emit(OpCodes.Ret);
            try
            {
                if (Mint(dm)(ioe, ovf, null) != 16)
                {
                    return 16;
                }
            }
            catch (OverflowException)
            {
                // The `finally` ran before the `catch` handler did. That is what a `finally` whose
                // try range was taken from `m_endAddr` produces: its range then stops where the
                // try body stops, so the second pass treats it as lying *between* the throw point
                // and the handler rather than enclosing the handler, and runs it first. Caught
                // rather than left to escape, so the two outcomes differ by exit code alone.
                return 17;
            }
        }

        return 0;
    }
}
