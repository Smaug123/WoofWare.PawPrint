using System;
using System.Reflection;

public class SharedBase<T>
{
    public virtual int Describe(T value) => 1;

    public virtual int Named() => 2;
}

public class SharedDerived<T> : SharedBase<T>
{
    public override int Describe(T value) => 11;
}

public static class Program
{
    // Enumerating the methods of an open generic definition whose *base type mentions its own type
    // parameter*: `SharedDerived<T> : SharedBase<T>`. The layout question is the same one
    // sourcesPure/ReflectionOpenGenericDefinitionMethods.cs asks and PawPrint answers -- the
    // override folds onto the inherited slot -- but reaching it needs the parent to be nameable
    // first, and `RuntimeType.GetParentType` fails before any of it: PawPrint's
    // `resolveBaseRuntimeTypeHandleTarget` refuses a base type that references generic parameters
    // (CoreCLR calls that the shared/canonical parent). That is the same refusal
    // sourcesPure/EnumQueriesOpenGenericSharedParent.cs parks, reached by a different query.
    //
    // Exit code is the index of the first failing check, so a failure names itself.
    public static int Main()
    {
        MethodInfo[] methods = typeof(SharedDerived<>).GetMethods();

        // Object's four, plus Describe and Named.
        if (methods.Length != 6) return 1;

        int describes = 0;
        foreach (MethodInfo m in methods)
        {
            if (m.Name == "Describe") describes++;
        }

        // The override and the method it overrides share a slot, and that slot was placed on
        // `SharedBase<T>` with `T` symbolic: the signature `Describe(!0)` is the deriving
        // definition's own `!0` only because the extends clause passes it through.
        if (describes != 1) return 2;
        if (typeof(SharedDerived<>).GetMethod("Describe").DeclaringType != typeof(SharedDerived<>)) return 3;
        // The inherited method is reported by the *parent*, which is the open construction
        // `SharedBase<T>` rather than the definition `SharedBase<>`.
        Type namedOn = typeof(SharedDerived<>).GetMethod("Named").DeclaringType;

        if (namedOn.GetGenericTypeDefinition() != typeof(SharedBase<>)) return 4;
        if (namedOn.IsGenericTypeDefinition) return 5;

        // The parent really is the open construction rather than the definition: this is what fails
        // if the shared parent is ever approximated by `SharedBase<>` itself.
        Type parent = typeof(SharedDerived<>).BaseType;

        if (parent.GetGenericTypeDefinition() != typeof(SharedBase<>)) return 6;
        if (parent.IsGenericTypeDefinition) return 7;
        if (parent.GetGenericArguments()[0] != typeof(SharedDerived<>).GetGenericArguments()[0]) return 8;

        return 0;
    }
}
