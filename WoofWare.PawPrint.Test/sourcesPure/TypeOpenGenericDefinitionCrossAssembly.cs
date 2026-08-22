using System;
using System.Collections.Generic;

public class SameAssemblyBox<T> { }

// `typeof(SomeGeneric<>)` -- an open generic *definition* -- named from inside a generic type
// context, where the enclosing `!0` is bound.
//
// `sourcesPure/TypeOpenGenericDefinitionInGenericContext.cs` is the same shape for a type declared
// in the guest's own assembly. This is its cross-assembly twin, and the two reach different arms of
// `ldtoken`'s type-token decoding: a type in another assembly is a `TypeReference` token, one in
// this assembly a `TypeDefinition`.
//
// The generic context is what makes this interesting. Outside one there is nothing to substitute,
// so the token resolves the same way whatever the arms do; inside one, a resolution that treats the
// enclosing frame's type arguments as arguments *for the referenced type* closes it at that
// instantiation. That is a positional coincidence -- `Holder<T>`'s `T` and `List<T>`'s `T` are
// unrelated -- which is why `crossOpen` is checked against `crossClosedForContext` rather than only
// for `IsGenericTypeDefinition`.
//
// The `Dictionary<,>` checks are the differing-arity half: with one enclosing argument and two
// parameters to fill, a positional substitution leaves the second unbound and produces a *partly*
// closed type, which is a different wrong answer from the one `List<>` produces.
//
// Exit 0 on success, otherwise the index of the first failing check.
public class CrossAssemblyHolder<T>
{
    public static int Check()
    {
        // --- One parameter, exactly matching the enclosing arity: the case where a positional
        // --- substitution succeeds silently and yields `List<T>`.
        Type crossOpen = typeof(List<>);
        if (!crossOpen.IsGenericTypeDefinition) return 1;
        if (crossOpen.ContainsGenericParameters != true) return 2;
        if (ReferenceEquals(crossOpen, typeof(List<T>))) return 3;
        if (!ReferenceEquals(crossOpen, typeof(List<int>).GetGenericTypeDefinition())) return 4;
        if (crossOpen.GetGenericArguments().Length != 1) return 5;

        // --- Two parameters against one enclosing argument.
        Type crossOpenTwo = typeof(Dictionary<,>);
        if (!crossOpenTwo.IsGenericTypeDefinition) return 6;
        if (crossOpenTwo.GetGenericArguments().Length != 2) return 7;
        if (!ReferenceEquals(crossOpenTwo, typeof(Dictionary<int, string>).GetGenericTypeDefinition())) return 8;
        if (ReferenceEquals(crossOpenTwo, typeof(Dictionary<T, T>))) return 9;

        // --- The same-assembly control, which is the shape that already worked. Keeping it here
        // --- means a change that fixed the TypeReference arm by breaking the TypeDefinition one
        // --- shows up in this file rather than only in its sibling.
        Type sameOpen = typeof(SameAssemblyBox<>);
        if (!sameOpen.IsGenericTypeDefinition) return 10;
        if (ReferenceEquals(sameOpen, typeof(SameAssemblyBox<T>))) return 11;
        if (!ReferenceEquals(sameOpen, typeof(SameAssemblyBox<int>).GetGenericTypeDefinition())) return 12;

        // --- And the closed forms still close: the fix must not make *every* type token open.
        if (typeof(List<T>) != typeof(List<int>)) return 13;
        if (typeof(SameAssemblyBox<T>) != typeof(SameAssemblyBox<int>)) return 14;

        return 0;
    }
}

public static class Program
{
    public static int Main() => CrossAssemblyHolder<int>.Check();
}
