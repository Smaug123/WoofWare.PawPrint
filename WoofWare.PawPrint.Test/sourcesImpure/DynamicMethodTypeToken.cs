using System;
using System.Collections.Generic;
using System.Reflection.Emit;

public struct Small
{
    public int A;
}

public struct Big
{
    public long A;
    public long B;
    public long C;
}

public class Program
{
    // Type-shaped operands in a dynamic method's body. The operand is `index | 0x02000000`, which
    // is a perfectly well-formed TypeDef token naming an unrelated real row, so this is about
    // resolving against the method's `DynamicScope` instead of against metadata. The scope entry
    // is a boxed `RuntimeTypeHandle` -- `DynamicILGenerator.GetTokenFor(RuntimeType)` is
    // `m_scope.GetTokenFor(rtType.TypeHandle)` (DynamicILGenerator.cs:496) -- and CoreCLR's
    // `ResolveToken` (DynamicILGenerator.cs:772) reads `((RuntimeTypeHandle)handle).Value` at JIT.
    //
    // Two of these checks discriminate a right implementation from a consistently-wrong one, and
    // the rest do not; that is called out per check below. Every
    // expectation was measured on the host's real .NET before being written down, because impure
    // cases get no automatic differential oracle.
    //
    // Dynamic method *signatures* and *locals* stay primitive throughout (plus `string`, which
    // `SignatureHelper.IsSimpleType` covers). A user-defined type in a signature takes
    // `SignatureHelper`'s `m_module == null` branch and emits `ELEMENT_TYPE_INTERNAL` followed by
    // the raw bytes of the type handle, which PawPrint cannot spell. That is a separate gap; a
    // user-defined type as an *operand* -- which is what this file is about -- does not touch it.
    //
    // Returns 0 on success, or the number of the first check that failed.

    private static Func<int> IntMethod(Action<ILGenerator> emit)
    {
        DynamicMethod dm = new DynamicMethod("D", typeof(int), new Type[0], typeof(Program).Module);
        emit(dm.GetILGenerator());
        return (Func<int>) dm.CreateDelegate(typeof(Func<int>));
    }

    private static Func<string> StringMethod(Action<ILGenerator> emit)
    {
        DynamicMethod dm = new DynamicMethod("S", typeof(string), new Type[0], typeof(Program).Module);
        emit(dm.GetILGenerator());
        return (Func<string>) dm.CreateDelegate(typeof(Func<string>));
    }

    public static int Main()
    {
        // 1. newarr. Smoke only: the length comes back whatever
        // element type was resolved, so this check is type-blind and cannot catch a wrong answer.
        Func<int> newarr = IntMethod(il =>
        {
            il.Emit(OpCodes.Ldc_I4, 7);
            il.Emit(OpCodes.Newarr, typeof(int));
            il.Emit(OpCodes.Ldlen);
            il.Emit(OpCodes.Conv_I4);
            il.Emit(OpCodes.Ret);
        });

        if (newarr() != 7)
        {
            return 1;
        }

        // 2, 3. sizeof over two guest structs of *different* sizes. This is the primary
        // type-correctness anchor: the answer names the type that was resolved. Two structs rather
        // than one so that "resolve the first user-defined entry, whatever was asked for" fails.
        if (IntMethod(il => { il.Emit(OpCodes.Sizeof, typeof(Small)); il.Emit(OpCodes.Ret); })() != 4)
        {
            return 2;
        }

        if (IntMethod(il => { il.Emit(OpCodes.Sizeof, typeof(Big)); il.Emit(OpCodes.Ret); })() != 24)
        {
            return 3;
        }

        // 4. Interleaving. `DynamicScope.GetTokenFor` appends without dedup, so the guest controls
        // the scope layout; putting string entries between the two type entries means any
        // off-by-one on the scope index lands on a wrong-kind entry (refused) or on the other
        // struct (4 + 4 or 24 + 24, neither of which is 28) rather than on a coincidental match.
        Func<int> interleaved = IntMethod(il =>
        {
            il.Emit(OpCodes.Ldstr, "first");
            il.Emit(OpCodes.Pop);
            il.Emit(OpCodes.Sizeof, typeof(Small));
            il.Emit(OpCodes.Ldstr, "second");
            il.Emit(OpCodes.Pop);
            il.Emit(OpCodes.Sizeof, typeof(Big));
            il.Emit(OpCodes.Add);
            il.Emit(OpCodes.Ret);
        });

        if (interleaved() != 28)
        {
            return 4;
        }

        // 5, 6. isinst. The *miss* is the other discriminating check here: an `int[]` is not a
        // `string`, and an implementation that resolved the operand to something else would have to
        // resolve it to a type an `int[]` is not an instance of to still pass -- whereas the hit
        // passes for any type a `string` happens to satisfy.
        Func<int> isinstHit = IntMethod(il =>
        {
            il.Emit(OpCodes.Ldstr, "s");
            il.Emit(OpCodes.Isinst, typeof(string));
            il.Emit(OpCodes.Ldnull);
            il.Emit(OpCodes.Ceq);
            il.Emit(OpCodes.Ret);
        });

        if (isinstHit() != 0)
        {
            return 5;
        }

        Func<int> isinstMiss = IntMethod(il =>
        {
            il.Emit(OpCodes.Ldc_I4_1);
            il.Emit(OpCodes.Newarr, typeof(int));
            il.Emit(OpCodes.Isinst, typeof(string));
            il.Emit(OpCodes.Ldnull);
            il.Emit(OpCodes.Ceq);
            il.Emit(OpCodes.Ret);
        });

        if (isinstMiss() != 1)
        {
            return 6;
        }

        // 7, 8. castclass, both directions. The failing direction discriminates: casting an `int[]`
        // to `string` must raise, so an implementation that resolved the operand to `object` (or to
        // the array's own type) would pass check 7 and fail check 8.
        if (StringMethod(il =>
            {
                il.Emit(OpCodes.Ldstr, "abc");
                il.Emit(OpCodes.Castclass, typeof(string));
                il.Emit(OpCodes.Ret);
            })() != "abc")
        {
            return 7;
        }

        Func<string> badCast = StringMethod(il =>
        {
            il.Emit(OpCodes.Ldc_I4_1);
            il.Emit(OpCodes.Newarr, typeof(int));
            il.Emit(OpCodes.Castclass, typeof(string));
            il.Emit(OpCodes.Ret);
        });

        try
        {
            badCast();
            return 8;
        }
        catch (InvalidCastException)
        {
        }

        // 9, 10. box/unbox.any and box/unbox. Not discriminating on their own -- the same helper
        // resolves both the box and the unbox, so a consistently-wrong resolution round-trips --
        // but they do establish that the two agree, which the checks above cannot.
        if (IntMethod(il =>
            {
                il.Emit(OpCodes.Ldc_I4, 42);
                il.Emit(OpCodes.Box, typeof(int));
                il.Emit(OpCodes.Unbox_Any, typeof(int));
                il.Emit(OpCodes.Ret);
            })() != 42)
        {
            return 9;
        }

        // A user-defined struct, reached without ever putting one in a signature or a local: an
        // array of them is built with `newarr`, an element address taken with `ldelema`, and the
        // value loaded with `ldobj`, all of which are type operands and none of which touches
        // `SignatureHelper`. `Small`'s only field is the `int` at offset 0, so `ldind.i4` reads it.
        // `unbox` of a boxed *primitive* is unimplemented in PawPrint for any token universe (the
        // box wraps it in a synthetic single-field struct), which is why this goes through `Small`
        // rather than through `int`.
        if (IntMethod(il =>
            {
                il.Emit(OpCodes.Ldc_I4_1);
                il.Emit(OpCodes.Newarr, typeof(Small));
                il.Emit(OpCodes.Dup);
                il.Emit(OpCodes.Ldc_I4_0);
                il.Emit(OpCodes.Ldelema, typeof(Small));
                il.Emit(OpCodes.Ldc_I4, 43);
                il.Emit(OpCodes.Stind_I4);
                il.Emit(OpCodes.Ldc_I4_0);
                il.Emit(OpCodes.Ldelema, typeof(Small));
                il.Emit(OpCodes.Ldobj, typeof(Small));
                il.Emit(OpCodes.Box, typeof(Small));
                il.Emit(OpCodes.Unbox, typeof(Small));
                il.Emit(OpCodes.Ldind_I4);
                il.Emit(OpCodes.Ret);
            })() != 43)
        {
            return 10;
        }

        // 11, 12, 13. initobj, ldobj, stobj through a byref to a local. Locals stay `int` so the
        // local signature avoids ELEMENT_TYPE_INTERNAL; these are not discriminating for the same
        // reason as 9 and 10.
        if (IntMethod(il =>
            {
                il.DeclareLocal(typeof(int));
                il.Emit(OpCodes.Ldc_I4, 5);
                il.Emit(OpCodes.Stloc_0);
                il.Emit(OpCodes.Ldloca_S, (byte) 0);
                il.Emit(OpCodes.Initobj, typeof(int));
                il.Emit(OpCodes.Ldloc_0);
                il.Emit(OpCodes.Ret);
            })() != 0)
        {
            return 11;
        }

        if (IntMethod(il =>
            {
                il.DeclareLocal(typeof(int));
                il.Emit(OpCodes.Ldc_I4, 6);
                il.Emit(OpCodes.Stloc_0);
                il.Emit(OpCodes.Ldloca_S, (byte) 0);
                il.Emit(OpCodes.Ldobj, typeof(int));
                il.Emit(OpCodes.Ret);
            })() != 6)
        {
            return 12;
        }

        if (IntMethod(il =>
            {
                il.DeclareLocal(typeof(int));
                il.Emit(OpCodes.Ldloca_S, (byte) 0);
                il.Emit(OpCodes.Ldc_I4, 9);
                il.Emit(OpCodes.Stobj, typeof(int));
                il.Emit(OpCodes.Ldloc_0);
                il.Emit(OpCodes.Ret);
            })() != 9)
        {
            return 13;
        }

        // 14. ldelema.
        if (IntMethod(il =>
            {
                il.Emit(OpCodes.Ldc_I4_2);
                il.Emit(OpCodes.Newarr, typeof(int));
                il.Emit(OpCodes.Dup);
                il.Emit(OpCodes.Ldc_I4_0);
                il.Emit(OpCodes.Ldc_I4, 11);
                il.Emit(OpCodes.Stelem_I4);
                il.Emit(OpCodes.Ldc_I4_0);
                il.Emit(OpCodes.Ldelema, typeof(int));
                il.Emit(OpCodes.Ldind_I4);
                il.Emit(OpCodes.Ret);
            })() != 11)
        {
            return 14;
        }

        // 15. A type operand that is not a closed type. `Emit` accepts an open generic definition
        // happily -- it is a perfectly good `RuntimeType` -- and the program is rejected when the
        // method is compiled. Measured on real .NET for the open definition, a bare generic
        // parameter, and an open constructed type: all three are InvalidProgramException, against a
        // closed control that runs.
        Func<int> openGeneric = IntMethod(il =>
        {
            il.Emit(OpCodes.Ldc_I4_1);
            il.Emit(OpCodes.Newarr, typeof(List<>));
            il.Emit(OpCodes.Ldlen);
            il.Emit(OpCodes.Conv_I4);
            il.Emit(OpCodes.Ret);
        });

        try
        {
            openGeneric();
            return 15;
        }
        catch (InvalidProgramException)
        {
        }

        // 16. Closed is not the same as legal: `System.Void` is a closed type and still an invalid
        // operand, measured on real .NET for `sizeof`, `newarr` and `box` alike.
        //
        // A byref type is rejected the same way, and a *pointer* type is by contrast perfectly legal
        // and answers 8 -- both measured on real .NET, and both implemented -- but neither is
        // checkable from here: `Type.MakeByRefType` and `Type.MakePointerType` bottom out in the
        // `RuntimeTypeHandle_MakeByRef` QCall, which PawPrint does not implement, so no guest can
        // construct either operand yet.
        Func<int> voidOperand = IntMethod(il =>
        {
            il.Emit(OpCodes.Sizeof, typeof(void));
            il.Emit(OpCodes.Ret);
        });

        try
        {
            voidOperand();
            return 16;
        }
        catch (InvalidProgramException)
        {
        }

        // 17, 18. A byref-like type is closed, and still not a legal `newarr` element: `Span<int>`
        // is stack-only, so CoreCLR fails to load `Span<int>[]` and never allocates. Measured, and
        // 18 is the control that keeps the refusal honest -- `sizeof` of the same type is perfectly
        // legal and answers 16, so this is a fact about `newarr` rather than about the type, and a
        // refusal that widened to the type itself would fail here.
        Func<int> byrefLikeArray = IntMethod(il =>
        {
            il.Emit(OpCodes.Ldc_I4_1);
            il.Emit(OpCodes.Newarr, typeof(Span<int>));
            il.Emit(OpCodes.Ldlen);
            il.Emit(OpCodes.Conv_I4);
            il.Emit(OpCodes.Ret);
        });

        try
        {
            byrefLikeArray();
            return 17;
        }
        catch (TypeLoadException)
        {
        }

        if (IntMethod(il => { il.Emit(OpCodes.Sizeof, typeof(Span<int>)); il.Emit(OpCodes.Ret); })() != 16)
        {
            return 18;
        }

        // 19, 20. Nor is a byref-like type something `box` can make an object of: the JIT refuses
        // the method with InvalidProgramException (measured). 20 is the control: the same body
        // without the `box` runs, so the refusal is the `box`'s and not the `Span<int>` value's.
        Func<int> byrefLikeBox = IntMethod(il =>
        {
            il.Emit(OpCodes.Call, typeof(Span<int>).GetProperty("Empty").GetMethod);
            il.Emit(OpCodes.Box, typeof(Span<int>));
            il.Emit(OpCodes.Pop);
            il.Emit(OpCodes.Ldc_I4_0);
            il.Emit(OpCodes.Ret);
        });

        try
        {
            byrefLikeBox();
            return 19;
        }
        catch (InvalidProgramException)
        {
        }

        Func<int> byrefLikeUnboxed = IntMethod(il =>
        {
            il.Emit(OpCodes.Call, typeof(Span<int>).GetProperty("Empty").GetMethod);
            il.Emit(OpCodes.Pop);
            il.Emit(OpCodes.Ldc_I4_7);
            il.Emit(OpCodes.Ret);
        });

        if (byrefLikeUnboxed() != 7)
        {
            return 20;
        }

        // 21, 22. Unless the JIT folds the box away, which it does where the box's block tests the
        // box (here through `isinst`) or unboxes it again at once (measured), resolving the scope
        // entries of the instructions after it as it matches them.
        Func<int> byrefLikeBoxTested = IntMethod(il =>
        {
            Label taken = il.DefineLabel();
            il.Emit(OpCodes.Call, typeof(Span<int>).GetProperty("Empty").GetMethod);
            il.Emit(OpCodes.Box, typeof(Span<int>));
            il.Emit(OpCodes.Isinst, typeof(object));
            il.Emit(OpCodes.Brtrue, taken);
            il.Emit(OpCodes.Ldc_I4_1);
            il.Emit(OpCodes.Ret);
            il.MarkLabel(taken);
            il.Emit(OpCodes.Ldc_I4_2);
            il.Emit(OpCodes.Ret);
        });

        if (byrefLikeBoxTested() != 2)
        {
            return 21;
        }

        Func<int> byrefLikeBoxUnboxed = IntMethod(il =>
        {
            il.Emit(OpCodes.Call, typeof(Span<int>).GetProperty("Empty").GetMethod);
            il.Emit(OpCodes.Box, typeof(Span<int>));
            il.Emit(OpCodes.Unbox_Any, typeof(Span<int>));
            il.Emit(OpCodes.Pop);
            il.Emit(OpCodes.Ldc_I4_3);
            il.Emit(OpCodes.Ret);
        });

        if (byrefLikeBoxUnboxed() != 3)
        {
            return 22;
        }

        // 23. It resolves the `unbox.any` after an `isinst` only where the `isinst` names the
        // boxed type, so here it refuses the box without looking at `void`, which is no type
        // `unbox.any` takes (measured).
        Func<int> byrefLikeBoxCastOther = IntMethod(il =>
        {
            il.Emit(OpCodes.Call, typeof(Span<int>).GetProperty("Empty").GetMethod);
            il.Emit(OpCodes.Box, typeof(Span<int>));
            il.Emit(OpCodes.Isinst, typeof(object));
            il.Emit(OpCodes.Unbox_Any, typeof(void));
            il.Emit(OpCodes.Pop);
            il.Emit(OpCodes.Ldc_I4_0);
            il.Emit(OpCodes.Ret);
        });

        try
        {
            byrefLikeBoxCastOther();
            return 23;
        }
        catch (InvalidProgramException)
        {
        }

        // 24. `System.Void` is refused by every opcode that consumes a type, not only by the
        // `sizeof` of 16 (measured, for each of these, against the `int` control 25 that runs).
        // The same is true of a metadata operand; `TestFabricatedTypeOperandRefusal` checks that
        // universe, and a byref operand, which no guest can construct here yet.
        var consumers = new List<Action<ILGenerator, Type>>
        {
            (il, t) => { il.Emit(OpCodes.Ldnull); il.Emit(OpCodes.Isinst, t); il.Emit(OpCodes.Pop); },
            (il, t) => { il.Emit(OpCodes.Ldnull); il.Emit(OpCodes.Castclass, t); il.Emit(OpCodes.Pop); },
            (il, t) => { il.Emit(OpCodes.Ldc_I4_0); il.Emit(OpCodes.Box, t); il.Emit(OpCodes.Pop); },
            (il, t) => { il.Emit(OpCodes.Ldc_I4_7); il.Emit(OpCodes.Box, typeof(int)); il.Emit(OpCodes.Unbox, t); il.Emit(OpCodes.Pop); },
            (il, t) => { il.Emit(OpCodes.Ldc_I4_7); il.Emit(OpCodes.Box, typeof(int)); il.Emit(OpCodes.Unbox_Any, t); il.Emit(OpCodes.Pop); },
            (il, t) => { il.Emit(OpCodes.Ldc_I4_0); il.Emit(OpCodes.Newarr, t); il.Emit(OpCodes.Pop); },
            (il, t) => { il.Emit(OpCodes.Sizeof, t); il.Emit(OpCodes.Pop); },
            (il, t) => { il.Emit(OpCodes.Ldloca, il.DeclareLocal(typeof(int))); il.Emit(OpCodes.Initobj, t); },
            (il, t) => { il.Emit(OpCodes.Ldloca, il.DeclareLocal(typeof(int))); il.Emit(OpCodes.Ldobj, t); il.Emit(OpCodes.Pop); },
            (il, t) => { il.Emit(OpCodes.Ldloca, il.DeclareLocal(typeof(int))); il.Emit(OpCodes.Ldc_I4_0); il.Emit(OpCodes.Stobj, t); },
            (il, t) =>
            {
                il.Emit(OpCodes.Ldc_I4_1);
                il.Emit(OpCodes.Newarr, typeof(int));
                il.Emit(OpCodes.Ldc_I4_0);
                il.Emit(OpCodes.Ldelema, t);
                il.Emit(OpCodes.Pop);
            },
        };

        foreach (var consume in consumers)
        {
            Func<int> Consuming(Type t) => IntMethod(il =>
            {
                consume(il, t);
                il.Emit(OpCodes.Ldc_I4_1);
                il.Emit(OpCodes.Ret);
            });

            try
            {
                Consuming(typeof(void))();
                return 24;
            }
            catch (InvalidProgramException)
            {
            }

            if (Consuming(typeof(int))() != 1)
            {
                return 25;
            }
        }

        // 26. Nor does an `isinst` after a box take it, which the JIT refuses as it resolves the
        // entry while matching the pattern (measured).
        Func<int> byrefLikeBoxCastVoid = IntMethod(il =>
        {
            Label taken = il.DefineLabel();
            il.Emit(OpCodes.Call, typeof(Span<int>).GetProperty("Empty").GetMethod);
            il.Emit(OpCodes.Box, typeof(Span<int>));
            il.Emit(OpCodes.Isinst, typeof(void));
            il.Emit(OpCodes.Brtrue, taken);
            il.Emit(OpCodes.Ldc_I4_1);
            il.Emit(OpCodes.Ret);
            il.MarkLabel(taken);
            il.Emit(OpCodes.Ldc_I4_2);
            il.Emit(OpCodes.Ret);
        });

        try
        {
            byrefLikeBoxCastVoid();
            return 26;
        }
        catch (InvalidProgramException)
        {
        }

        // 27, 28. A `catch` clause's type is resolved as an operand is, so `System.Void` is refused
        // there too, even when nothing is thrown (measured), against the `Exception` control 28.
        Func<int> Catching(Type caught) => IntMethod(il =>
        {
            LocalBuilder result = il.DeclareLocal(typeof(int));
            il.BeginExceptionBlock();
            il.Emit(OpCodes.Ldc_I4_1);
            il.Emit(OpCodes.Stloc, result);
            il.BeginCatchBlock(caught);
            il.Emit(OpCodes.Pop);
            il.Emit(OpCodes.Ldc_I4_2);
            il.Emit(OpCodes.Stloc, result);
            il.EndExceptionBlock();
            il.Emit(OpCodes.Ldloc, result);
            il.Emit(OpCodes.Ret);
        });

        try
        {
            Catching(typeof(void))();
            return 27;
        }
        catch (InvalidProgramException)
        {
        }

        if (Catching(typeof(Exception))() != 1)
        {
            return 28;
        }

        return 0;
    }
}
