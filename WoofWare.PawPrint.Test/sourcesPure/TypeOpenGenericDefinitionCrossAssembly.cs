using System;
using System.Collections.Generic;

public class SameAssemblyBox<T> { }

// `typeof(SomeGeneric<>)` named from inside a generic *type* context. `sourcesPure/
// TypeOpenGenericDefinitionInGenericContext.cs` is the same shape for a type declared in the
// guest's own assembly, and passes; this is its cross-assembly twin, and does not.
//
// The two reach different arms of `ldtoken`'s type-token decoding -- a type in another assembly
// is a `TypeReference` token, one in this assembly a `TypeDefinition` -- and only the second arm
// produces the open definition here. Measured: PawPrint returns 1 (`IsGenericTypeDefinition` is
// false, i.e. `typeof(List<>)` came back closed at the enclosing context's instantiation) where
// real .NET returns 0. Independent of any member-token work: `main` diverges identically.
//
// Found while mutation-testing the per-arm `allowOpenGenericDefinition` flags. Flipping the
// `TypeReference` arm's flag survives the entire suite, and this file is why: no *passing* guest
// can observe that arm's flag while this path is broken. Un-parking this is what makes that
// mutant killable.
public class CrossAssemblyHolder<T>
{
    public static int Check()
    {
        Type crossOpen = typeof(List<>);
        if (!crossOpen.IsGenericTypeDefinition) return 1;
        if (ReferenceEquals(crossOpen, typeof(List<T>))) return 2;
        if (!ReferenceEquals(crossOpen, typeof(List<int>).GetGenericTypeDefinition())) return 3;

        // The same-assembly control, which is the shape that already works.
        Type sameOpen = typeof(SameAssemblyBox<>);
        if (!sameOpen.IsGenericTypeDefinition) return 4;
        if (ReferenceEquals(sameOpen, typeof(SameAssemblyBox<T>))) return 5;

        return 0;
    }
}

public static class Program
{
    public static int Main() => CrossAssemblyHolder<int>.Check();
}
