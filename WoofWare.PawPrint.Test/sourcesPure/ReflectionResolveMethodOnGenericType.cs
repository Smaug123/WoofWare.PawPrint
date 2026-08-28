using System;
using System.Reflection;

public class Holder<T>
{
    public T Value;

    public Holder() { }

    public T Get() => Value;

    public U Convert<U>(T value) => default(U);
}

public struct SBox<T>
{
    public T Item;

    public T Get() => Item;

    public override string ToString() => "SBox";
}

public interface IFace<T>
{
    T Read();
}

public static class Statics
{
    public static int Plain() => 1;

    public static U Generic<U>() => default(U);
}

public static class Program
{
    // `Module.ResolveMethod` for a MethodDef token whose declaring type, or whose method, is
    // generic. CoreCLR answers with the *typical* definition and ignores the caller's
    // type/method-instantiation context entirely for a MethodDef
    // (`MemberLoader::GetMethodDescFromMethodDef` takes no `SigTypeContext`), so the method comes
    // back declared on `Holder<>` rather than on whatever instantiation was asked about.
    //
    // The `ReferenceEquals` checks are what pin the *identity* rather than a look-alike: the
    // reflected type's member cache keys on the method handle, so a second handle for the same
    // method would surface here as a second `MethodInfo`.
    //
    // Exit code is the index of the first failing check, so a failure names itself.
    public static int Main()
    {
        Module mod = typeof(Holder<>).Module;

        MethodInfo closedGet = typeof(Holder<int>).GetMethod("Get");
        MethodBase resolvedGet = mod.ResolveMethod(closedGet.MetadataToken);
        if (resolvedGet.Name != "Get") return 1;
        if (resolvedGet.MetadataToken != closedGet.MetadataToken) return 2;
        if (resolvedGet.DeclaringType != typeof(Holder<>)) return 3;
        if (!ReferenceEquals(resolvedGet, typeof(Holder<>).GetMethod("Get"))) return 4;

        // The instantiation context is not consulted for a MethodDef token, so supplying the
        // instantiation that was asked about changes nothing.
        MethodBase resolvedWithContext =
            mod.ResolveMethod(closedGet.MetadataToken, new Type[] { typeof(int) }, null);
        if (!ReferenceEquals(resolvedWithContext, resolvedGet)) return 5;

        // A generic method on a generic type: both instantiation contexts are ignored at once.
        MethodInfo closedConvert = typeof(Holder<int>).GetMethod("Convert");
        MethodBase resolvedConvert = mod.ResolveMethod(closedConvert.MetadataToken);
        if (resolvedConvert.Name != "Convert") return 6;
        if (!((MethodInfo)resolvedConvert).IsGenericMethodDefinition) return 7;
        if (!ReferenceEquals(resolvedConvert, typeof(Holder<>).GetMethod("Convert"))) return 8;

        // A generic method on a *non*-generic type, which the generic declaring type cannot reach.
        MethodInfo genericStatic = typeof(Statics).GetMethod("Generic");
        MethodBase resolvedGeneric = mod.ResolveMethod(genericStatic.MetadataToken);
        if (!((MethodInfo)resolvedGeneric).IsGenericMethodDefinition) return 9;
        if (!ReferenceEquals(resolvedGeneric, genericStatic)) return 10;

        // A value type, whose typical method table is the one CoreCLR hands an unboxing stub for
        // when the method is virtual, and an ordinary instantiating stub otherwise.
        MethodInfo structGet = typeof(SBox<int>).GetMethod("Get");
        MethodBase resolvedStructGet = mod.ResolveMethod(structGet.MetadataToken);
        if (!ReferenceEquals(resolvedStructGet, typeof(SBox<>).GetMethod("Get"))) return 11;

        MethodInfo structToString = typeof(SBox<int>).GetMethod("ToString");
        MethodBase resolvedToString = mod.ResolveMethod(structToString.MetadataToken);
        if (resolvedToString.DeclaringType != typeof(SBox<>)) return 12;

        // An interface method.
        MethodInfo read = typeof(IFace<int>).GetMethod("Read");
        MethodBase resolvedRead = mod.ResolveMethod(read.MetadataToken);
        if (!ReferenceEquals(resolvedRead, typeof(IFace<>).GetMethod("Read"))) return 13;

        // A constructor on a generic type.
        ConstructorInfo ctor = typeof(Holder<int>).GetConstructor(Type.EmptyTypes);
        MethodBase resolvedCtor = mod.ResolveMethod(ctor.MetadataToken);
        if (!ReferenceEquals(resolvedCtor, typeof(Holder<>).GetConstructor(Type.EmptyTypes))) return 14;

        // An *open* type as the supplied context. CoreCLR never looks at the arrays for a MethodDef,
        // so it does not care that `Holder<>` names no closed type; a resolver that decoded them
        // anyway would have to reject this.
        MethodBase resolvedOpenContext =
            mod.ResolveMethod (genericStatic.MetadataToken, null, new Type[] { typeof (Holder<>) });
        if (!ReferenceEquals (resolvedOpenContext, genericStatic)) return 15;

        // The same shape with a *generic declaring type* is not checked here: `Module.ResolveMethod`
        // then re-resolves the declaring TypeDef through `ModuleHandle.ResolveType`, which does
        // consume its instantiation arrays and refuses an open argument. That is a gap in that
        // QCall rather than in this one, and reaching it needs no MethodDef token at all.

        // Control: an ordinary method on a non-generic type still round-trips to itself.
        MethodInfo plain = typeof(Statics).GetMethod("Plain");
        if (!ReferenceEquals (mod.ResolveMethod (plain.MetadataToken), plain)) return 16;

        return 0;
    }
}
