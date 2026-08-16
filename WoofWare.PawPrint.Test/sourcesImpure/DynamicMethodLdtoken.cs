using System;
using System.Reflection.Emit;

public class Box<T>
{
    public T Item;
}

public class Program
{
    // `ldtoken` with a type-shaped operand resolved against the method's `DynamicScope` rather than
    // against metadata. The emitted operand is `index | 0x01000000`, a well-formed TypeDef token
    // naming an unrelated real row, so a resolution that consulted metadata would answer something
    // else entirely rather than failing.
    //
    // `ldtoken` accepts every type shape, which is what separates it from the other eleven
    // type-shaped opcodes: `newarr`/`box`/`sizeof` and friends demand a closed type, and
    // `DynamicScopeOperand.closedType` refuses a byref, `System.Void`, and anything not `Closed`.
    // All three of those refusals are wrong here. Measured on real .NET: `ldtoken` of an open
    // generic definition, of a bare generic parameter, of `System.Void`, of a pointer and of a
    // byref all run, and all round-trip through `Type.GetTypeFromHandle` to the right `Type`.
    // Checks 2, 3 and 4 are the ones that pin that.
    //
    // The dynamic method only ever does `ldtoken; box; ret`: calling `Type.GetTypeFromHandle` from
    // *inside* an emitted body needs a `call` naming a reflected MethodInfo, which is still blocked
    // on RuntimeMethodHandle::GetMethodDef. The caller does the round trip in ordinary C#, which is
    // the metadata universe, and that cross-universe read is what makes checks 7-9 meaningful.
    //
    // Every expectation was measured on the host's real .NET before being written down: impure
    // cases get no differential oracle, so these are a claim rather than a derivation.
    //
    // Returns 0 on success, or the number of the first check that failed.
    static Func<object> Handle(Type t)
    {
        var dm = new DynamicMethod("f", typeof(object), Type.EmptyTypes, typeof(Program));
        var il = dm.GetILGenerator();
        il.Emit(OpCodes.Ldtoken, t);
        il.Emit(OpCodes.Box, typeof(RuntimeTypeHandle));
        il.Emit(OpCodes.Ret);
        return (Func<object>)dm.CreateDelegate(typeof(Func<object>));
    }

    public static int Main()
    {
        // 1. A closed type. Smoke: any resolution answering some handle passes unless it is the
        // wrong one, and the comparison is against the *metadata* universe's handle for the same
        // type, so "resolved consistently but wrongly" fails.
        if (!((RuntimeTypeHandle)Handle(typeof(Box<int>))()).Equals(typeof(Box<int>).TypeHandle))
        {
            return 1;
        }

        // 2. An OPEN generic definition. DISCRIMINATING against reusing `closedType`, whose
        // non-closed arm is an InvalidProgramException: measured to run on real .NET.
        if (!((RuntimeTypeHandle)Handle(typeof(Box<>))()).Equals(typeof(Box<>).TypeHandle))
        {
            return 2;
        }

        // 3. A bare generic parameter. DISCRIMINATING for the same reason as check 2, and a
        // different `RuntimeTypeHandleTarget` case (`GenericParameter`, not
        // `OpenGenericTypeDefinition`).
        var t = typeof(Box<>).GetGenericArguments()[0];
        if (!((RuntimeTypeHandle)Handle(t)()).Equals(t.TypeHandle))
        {
            return 3;
        }

        // 4. System.Void. DISCRIMINATING against `closedType`'s explicit Void refusal, which is
        // correct for sizeof/newarr/box and wrong here: measured to run.
        if (!((RuntimeTypeHandle)Handle(typeof(void))()).Equals(typeof(void).TypeHandle))
        {
            return 4;
        }

        // 5. An array type, i.e. a structural target rather than a nominal one.
        if (!((RuntimeTypeHandle)Handle(typeof(int[]))()).Equals(typeof(int[]).TypeHandle))
        {
            return 5;
        }

        // 6. A corelib type. The only check exercising the *assembly* half of the target's
        // identity; every other check stays inside the guest assembly.
        if (!((RuntimeTypeHandle)Handle(typeof(string))()).Equals(typeof(string).TypeHandle))
        {
            return 6;
        }

        // 7, 8. The handle round-trips to the canonical `Type`, by *reference* rather than by
        // `Equals`, so an equal-but-distinct Type fails.
        //
        // Regression guards rather than discriminating checks. The resolved target is read out of a
        // `RuntimeType`'s `m_handle`, and only `TypeHandleRegistry.getOrAllocate` plants one there,
        // recording `target -> that same address` as it goes; so pushing the handle back through
        // `getOrAllocateType` is a map hit that cannot miss. These two rows can only fail if that
        // canonicalisation itself breaks, which breaks the interpreter far more visibly first.
        if (!ReferenceEquals(Type.GetTypeFromHandle((RuntimeTypeHandle)Handle(typeof(Box<int>))()), typeof(Box<int>)))
        {
            return 7;
        }

        if (!ReferenceEquals(Type.GetTypeFromHandle((RuntimeTypeHandle)Handle(typeof(Box<>))()), typeof(Box<>)))
        {
            return 8;
        }

        // 9. Negative control: the open definition and a closed instantiation of it must NOT
        // resolve to the same handle. Kills a canonicalisation collapse that every positive row
        // above would tolerate -- e.g. narrowing `Box<>` to `Box<int>`, or dropping the
        // instantiation when building the target.
        if (((RuntimeTypeHandle)Handle(typeof(Box<>))()).Equals(typeof(Box<int>).TypeHandle))
        {
            return 9;
        }

        // 10. Two ldtokens in one body, with an unrelated entry between them. `GetTokenFor` appends
        // without dedup, so the guest controls the scope layout: an off-by-one on the scope index
        // lands on the `box` operand's own RuntimeTypeHandle entry rather than on the other
        // ldtoken, and the exact comparisons below reject it.
        var two = new DynamicMethod("g", typeof(object), Type.EmptyTypes, typeof(Program));
        var il2 = two.GetILGenerator();
        il2.Emit(OpCodes.Ldtoken, typeof(Box<int>));
        il2.Emit(OpCodes.Box, typeof(RuntimeTypeHandle));
        il2.Emit(OpCodes.Pop);
        il2.Emit(OpCodes.Ldstr, "between");
        il2.Emit(OpCodes.Pop);
        il2.Emit(OpCodes.Ldtoken, typeof(Box<>));
        il2.Emit(OpCodes.Box, typeof(RuntimeTypeHandle));
        il2.Emit(OpCodes.Ret);
        var second = (RuntimeTypeHandle)((Func<object>)two.CreateDelegate(typeof(Func<object>)))();

        if (!second.Equals(typeof(Box<>).TypeHandle))
        {
            return 10;
        }

        return 0;
    }
}
