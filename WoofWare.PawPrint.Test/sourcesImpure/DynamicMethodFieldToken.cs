using System;
using System.Reflection;
using System.Reflection.Emit;

public class Holder
{
    public static int First = 101;
    public static int Second = 202;
    public int X = 11;
    public int Y = 22;
}

public class Other
{
    // Deliberately the same name as Holder's, and a different value.
    public static int First = 303;
}

public class Box<T>
{
    public T Item;
}

public struct Pair
{
    public int A;
    public int B;
}

public class Program
{
    // Field-shaped operands in a dynamic method's body. The operand is `index | 0x04000000`, a
    // well-formed FieldDef token naming an unrelated real row, so this is about resolving against
    // the method's `DynamicScope` rather than against metadata.
    //
    // The entry is *not* a bare `RuntimeFieldHandle`: `DynamicILGenerator.Emit(OpCode, FieldInfo)`
    // (`DynamicILGenerator.cs:140-153`) takes the two-argument
    // `GetTokenFor(runtimeField, runtimeField.GetRuntimeType())` whenever `field.DeclaringType` is
    // non-null -- which it always is for anything `Type.GetField` returns -- and
    // `DynamicScope.GetTokenFor(RuntimeFieldHandle, RuntimeTypeHandle)` (`:1047`) stores a
    // `GenericFieldInfo` wrapper. The name is misleading: an ordinary static `int` on a
    // non-generic type arrives wrapped too.
    //
    // Which checks discriminate a right implementation from a consistently-wrong one is called out
    // per check. Every expectation was measured on the host's real .NET before being written down:
    // impure cases get no differential oracle, so these numbers are a claim rather than a
    // derivation.
    //
    // Signatures and locals stay primitive (plus `string` and `object`, which
    // `SignatureHelper.IsSimpleType` covers). A user-defined type in a *signature* takes
    // `SignatureHelper`'s `m_module == null` branch and needs `ELEMENT_TYPE_INTERNAL`, which is a
    // separate gap; a user-defined type reached through an *operand* does not touch it.
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

    private static Func<object, int> ObjIntMethod(Action<ILGenerator> emit)
    {
        DynamicMethod dm = new DynamicMethod("O", typeof(int), new Type[] { typeof(object) }, typeof(Program).Module);
        emit(dm.GetILGenerator());
        return (Func<object, int>) dm.CreateDelegate(typeof(Func<object, int>));
    }

    private static Func<object, object> ObjObjMethod(Action<ILGenerator> emit)
    {
        DynamicMethod dm = new DynamicMethod("Q", typeof(object), new Type[] { typeof(object) }, typeof(Program).Module);
        emit(dm.GetILGenerator());
        return (Func<object, object>) dm.CreateDelegate(typeof(Func<object, object>));
    }

    private static Action<object, int> ObjIntAction(Action<ILGenerator> emit)
    {
        DynamicMethod dm = new DynamicMethod(
            "A",
            typeof(void),
            new Type[] { typeof(object), typeof(int) },
            typeof(Program).Module);
        emit(dm.GetILGenerator());
        return (Action<object, int>) dm.CreateDelegate(typeof(Action<object, int>));
    }

    public static int Main()
    {
        FieldInfo holderFirst = typeof(Holder).GetField("First");
        FieldInfo holderSecond = typeof(Holder).GetField("Second");
        FieldInfo otherFirst = typeof(Other).GetField("First");
        FieldInfo holderX = typeof(Holder).GetField("X");
        FieldInfo holderY = typeof(Holder).GetField("Y");

        // 1. ldsfld of a static int. Smoke: any resolution answering 101 passes, and only check 2
        // makes that a statement about *which* field was resolved.
        if (IntMethod(il => { il.Emit(OpCodes.Ldsfld, holderFirst); il.Emit(OpCodes.Ret); })() != 101)
        {
            return 1;
        }

        // 2. A second static on the same type. DISCRIMINATING on the field row: "resolve the first
        // field entry, whatever was asked for" answers 101 here and fails.
        if (IntMethod(il => { il.Emit(OpCodes.Ldsfld, holderSecond); il.Emit(OpCodes.Ret); })() != 202)
        {
            return 2;
        }

        // 3. A same-named static on a different type. This discriminates *name*-keying only, and is
        // labelled accordingly: `FieldDefinitionHandle` rows are numbered per assembly, not per
        // type, so Holder::First and Other::First already have different rows and an implementation
        // keying on the row alone passes this. Checks 14 and 15 are the ones that discriminate on
        // the declaring type.
        if (IntMethod(il => { il.Emit(OpCodes.Ldsfld, otherFirst); il.Emit(OpCodes.Ret); })() != 303)
        {
            return 3;
        }

        // 4. A corelib static. The only check that exercises the *assembly* half of the field's
        // identity and the declaring-assembly threading that `resolveFieldToken`'s docs say
        // interprets `field.Signature`; every other check stays inside the guest assembly.
        //
        // `bool.TrueString` rather than the more obvious `IntPtr.Zero`, which would have been
        // vacuous: `Zero`'s value is the zero of its own type, so reading the wrong cell, reading
        // uninitialised storage, skipping the `.cctor`, or never touching storage at all and
        // answering `cliTypeZeroOf` all produce the expected answer. `TrueString` is a genuine
        // `static readonly string` (not a const, so it survives `Emit`) with a distinctive value,
        // and the assertion is *reference* equality, which says the scope path reached that cell
        // rather than an equal copy of it.
        FieldInfo trueString = typeof(bool).GetField("TrueString");

        if (!ReferenceEquals(StringMethod(il =>
            {
                il.Emit(OpCodes.Ldsfld, trueString);
                il.Emit(OpCodes.Ret);
            })(), bool.TrueString))
        {
            return 4;
        }

        // 5. stsfld, read back through ordinary C# -- i.e. through the *metadata* universe. This is
        // what distinguishes "wrote the right cell" from "wrote and read one cell consistently",
        // which a scope-only round trip cannot. DISCRIMINATING.
        IntMethod(il =>
            {
                il.Emit(OpCodes.Ldc_I4, 555);
                il.Emit(OpCodes.Stsfld, holderSecond);
                il.Emit(OpCodes.Ldc_I4_0);
                il.Emit(OpCodes.Ret);
            })();

        if (Holder.Second != 555)
        {
            return 5;
        }

        // 6. ldsflda, written through the address. Same cross-universe read-back as check 5.
        IntMethod(il =>
            {
                il.Emit(OpCodes.Ldsflda, holderSecond);
                il.Emit(OpCodes.Ldc_I4, 666);
                il.Emit(OpCodes.Stind_I4);
                il.Emit(OpCodes.Ldc_I4_0);
                il.Emit(OpCodes.Ret);
            })();

        if (Holder.Second != 666)
        {
            return 6;
        }

        // 7, 8. ldfld of an instance field, the receiver arriving as an `object` parameter so that
        // no signature mentions a user-defined type. Check 8 invokes the *same* delegate with a
        // second receiver holding a different value: DISCRIMINATING against an implementation that
        // resolved the field to a cell rather than to an offset, or that cached the first receiver.
        Func<object, int> readX = ObjIntMethod(il =>
        {
            il.Emit(OpCodes.Ldarg_0);
            il.Emit(OpCodes.Castclass, typeof(Holder));
            il.Emit(OpCodes.Ldfld, holderX);
            il.Emit(OpCodes.Ret);
        });

        Holder h1 = new Holder();
        Holder h2 = new Holder();
        h2.X = 77;

        if (readX(h1) != 11)
        {
            return 7;
        }

        if (readX(h2) != 77)
        {
            return 8;
        }

        // 9. A second instance field at a different offset. DISCRIMINATING on the offset: an
        // implementation that resolved every instance field of a type to its first slot answers 11
        // here.
        if (ObjIntMethod(il =>
            {
                il.Emit(OpCodes.Ldarg_0);
                il.Emit(OpCodes.Castclass, typeof(Holder));
                il.Emit(OpCodes.Ldfld, holderY);
                il.Emit(OpCodes.Ret);
            })(h1) != 22)
        {
            return 9;
        }

        // 10. stfld, and 11, ldflda + stind, both observed from C#.
        ObjIntAction(il =>
            {
                il.Emit(OpCodes.Ldarg_0);
                il.Emit(OpCodes.Castclass, typeof(Holder));
                il.Emit(OpCodes.Ldarg_1);
                il.Emit(OpCodes.Stfld, holderY);
                il.Emit(OpCodes.Ret);
            })(h1, 88);

        if (h1.Y != 88)
        {
            return 10;
        }

        ObjIntAction(il =>
            {
                il.Emit(OpCodes.Ldarg_0);
                il.Emit(OpCodes.Castclass, typeof(Holder));
                il.Emit(OpCodes.Ldflda, holderY);
                il.Emit(OpCodes.Ldarg_1);
                il.Emit(OpCodes.Stind_I4);
                il.Emit(OpCodes.Ret);
            })(h1, 99);

        if (h1.Y != 99)
        {
            return 11;
        }

        // 12. Interleaved scope entries. `DynamicScope.GetTokenFor` appends without dedup, so the
        // guest controls the layout; putting a string and a type handle between the two field
        // entries means an off-by-one on the scope index lands on a wrong-kind entry (refused)
        // rather than on the other field (101 + 101 or 202 + 202, neither of which is 303).
        Func<int> interleaved = IntMethod(il =>
        {
            il.Emit(OpCodes.Ldsfld, holderFirst);
            il.Emit(OpCodes.Ldstr, "between");
            il.Emit(OpCodes.Pop);
            il.Emit(OpCodes.Sizeof, typeof(long));
            il.Emit(OpCodes.Pop);
            il.Emit(OpCodes.Ldsfld, otherFirst);
            il.Emit(OpCodes.Add);
            il.Emit(OpCodes.Ret);
        });

        if (interleaved() != 404)
        {
            return 12;
        }

        // 13. A field of a struct, reached through `ldflda` on an array element -- so a
        // user-defined type is named only by operands, never by a signature or a local.
        if (IntMethod(il =>
            {
                il.Emit(OpCodes.Ldc_I4_1);
                il.Emit(OpCodes.Newarr, typeof(Pair));
                il.Emit(OpCodes.Dup);
                il.Emit(OpCodes.Ldc_I4_0);
                il.Emit(OpCodes.Ldelema, typeof(Pair));
                il.Emit(OpCodes.Ldflda, typeof(Pair).GetField("B"));
                il.Emit(OpCodes.Ldc_I4, 44);
                il.Emit(OpCodes.Stind_I4);
                il.Emit(OpCodes.Ldc_I4_0);
                il.Emit(OpCodes.Ldelema, typeof(Pair));
                il.Emit(OpCodes.Ldfld, typeof(Pair).GetField("B"));
                il.Emit(OpCodes.Ret);
            })() != 44)
        {
            return 13;
        }

        // 14, 15. A field on a *closed generic instantiation*, twice over, at two different
        // instantiations. DISCRIMINATING on the declaring type, and the only check that is: the two
        // `FieldInfo`s share an assembly and a `FieldDefinitionHandle` row and differ only in the
        // declaring `RuntimeTypeHandleTarget` the field-handle registry recorded, so an
        // implementation that dropped that half of the key would resolve `Box<string>::Item` as
        // `Box<int>::Item` and answer with the wrong field type.
        //
        // Returned as `object`, so the field's type is a *result* rather than something a signature
        // has to spell.
        FieldInfo boxIntItem = typeof(Box<int>).GetField("Item");
        FieldInfo boxStringItem = typeof(Box<string>).GetField("Item");

        Func<object, object> readIntItem = ObjObjMethod(il =>
        {
            il.Emit(OpCodes.Ldarg_0);
            il.Emit(OpCodes.Castclass, typeof(Box<int>));
            il.Emit(OpCodes.Ldfld, boxIntItem);
            il.Emit(OpCodes.Box, typeof(int));
            il.Emit(OpCodes.Ret);
        });

        Box<int> bi = new Box<int>();
        bi.Item = 31;

        if (!readIntItem(bi).Equals(31))
        {
            return 14;
        }

        Func<object, object> readStringItem = ObjObjMethod(il =>
        {
            il.Emit(OpCodes.Ldarg_0);
            il.Emit(OpCodes.Castclass, typeof(Box<string>));
            il.Emit(OpCodes.Ldfld, boxStringItem);
            il.Emit(OpCodes.Ret);
        });

        Box<string> bs = new Box<string>();
        bs.Item = "thirty-two";

        if (!"thirty-two".Equals(readStringItem(bs)))
        {
            return 15;
        }

        // 16. The *open generic definition*'s FieldInfo. `Emit` accepts it happily -- it is a
        // perfectly good `RuntimeFieldInfo` -- and the method is rejected when it is compiled.
        // Measured on real .NET: InvalidProgramException, against check 14's closed control which
        // runs.
        FieldInfo boxOpenItem = typeof(Box<>).GetField("Item");

        Func<object, object> readOpenItem = ObjObjMethod(il =>
        {
            il.Emit(OpCodes.Ldarg_0);
            il.Emit(OpCodes.Castclass, typeof(Box<int>));
            il.Emit(OpCodes.Ldfld, boxOpenItem);
            il.Emit(OpCodes.Box, typeof(int));
            il.Emit(OpCodes.Ret);
        });

        try
        {
            readOpenItem(bi);
            return 16;
        }
        catch (InvalidProgramException)
        {
        }

        // Deliberately *not* checked here: an op whose staticness disagrees with its field's.
        // `ILGenerator.Emit` accepts either mismatch, so both are newly reachable through a scope
        // operand in a way no compiler can produce for a metadata one, and both were measured on
        // real .NET -- a static op on an instance field is an `InvalidProgramException`, and an
        // instance op on a static field *runs*, with the receiver evaluated and discarded.
        //
        // PawPrint refuses both, loudly, in `checkFieldStaticness`, which is shared with the
        // metadata universe. The second is legal IL PawPrint has never implemented, where a crash is
        // the honest answer rather than a wrong one. Both are recorded in docs/divergences.md. A
        // check here could only assert that the interpreter crashes, which is not something a guest
        // can observe.
        return 0;
    }
}
