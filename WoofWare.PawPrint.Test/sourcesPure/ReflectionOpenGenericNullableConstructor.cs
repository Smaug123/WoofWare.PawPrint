using System;
using System.Reflection;

public static class Program
{
    // `typeof(Nullable<>).GetConstructor(typeof(Nullable<>).GetGenericArguments())` is what
    // System.Dynamic.Utils.TypeUtils' static initialiser runs, so this shape is the first thing any
    // use of System.Linq.Expressions reaches. It lands in
    // RuntimeMethodHandle.GetStubIfNeededSlow's rebind with an open generic *definition* as the
    // declaring type and an empty method instantiation: Nullable<T> is a value type, so CoreCLR
    // wants an instantiating stub and the fast path does not short-circuit.
    //
    // The corelib counterpart of sourcesPure/ReflectionOpenGenericStructConstructors.cs, which
    // covers the same arm for a user-declared struct. Both are worth having: the declaring-type
    // facts come from `isValueType` over a corelib primitive-like struct here, and over an ordinary
    // user-assembly TypeDef there.
    //
    // GetConstructors is deliberately the query rather than GetConstructor(Type[]): filtering
    // candidates by parameter type decodes the ctor's MethodSig under the definition's formal type
    // context, which is a separate gap (Signature_Init refuses an open declaring type).
    //
    // Exit code is the index of the first failing check, so a failure names itself.
    public static int Main()
    {
        ConstructorInfo[] open = typeof(Nullable<>).GetConstructors();
        if (open.Length != 1) return 1;
        if (!open[0].IsPublic) return 2;
        if (open[0].IsStatic) return 3;

        // The definition's handle must not be the instantiation's.
        ConstructorInfo[] closed = typeof(int?).GetConstructors();
        if (closed.Length != 1) return 4;
        if (open[0].MethodHandle.Equals(closed[0].MethodHandle)) return 5;

        // Vacuity guard: the two share the MethodDef row, so check 5 turns on the declaring type
        // and cannot pass merely because these are different methods.
        if (open[0].MetadataToken != closed[0].MetadataToken) return 6;

        // Asking again yields the same handle: the rebind names an identity the registry already
        // holds rather than minting a fresh id per query.
        if (!typeof(Nullable<>).GetConstructors()[0].MethodHandle.Equals(open[0].MethodHandle)) return 7;

        // A second caller into the same rebind. MethodBase.GetMethodFromHandle hands the declaring
        // type through unchanged here -- reflected and declared types agree, so nothing remaps the
        // handle on the way -- and rebinding an already-rebound handle must be idempotent.
        MethodBase again = MethodBase.GetMethodFromHandle(open[0].MethodHandle, typeof(Nullable<>).TypeHandle)!;
        if (!again.MethodHandle.Equals(open[0].MethodHandle)) return 8;
        if (again.DeclaringType != typeof(Nullable<>)) return 9;

        return 0;
    }
}
