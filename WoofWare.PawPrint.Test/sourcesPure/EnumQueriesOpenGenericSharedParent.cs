using System;

class ClosedBase { }

class DerivedFromClosedBase<T> : ClosedBase { }

class OpenBase<T> { }

// The base type still mentions the type parameter, so the parent of this definition's typical
// instantiation is an open constructed type rather than a closed one.
class DerivedFromSharedBase<T> : OpenBase<T> { }

class DerivedFromNestedSharedBase<T> : OpenBase<OpenBase<T>> { }

class Program
{
    static int next = 1;
    static int firstFailure = 0;

    static void Check(bool ok)
    {
        int index = next;
        next = next + 1;
        if (!ok && firstFailure == 0)
        {
            firstFailure = index;
        }
    }

    // `RuntimeType.IsActualEnum` is internal; `GetEnumUnderlyingType` is
    // `if (!IsActualEnum) ThrowMustBeEnum(); ...`, and `ThrowMustBeEnum` raises `ArgumentException`.
    static bool IsActualEnum(Type t)
    {
        try
        {
            t.GetEnumUnderlyingType();
            return true;
        }
        catch (ArgumentException)
        {
            return false;
        }
    }

    static int Main(string[] args)
    {
        // Controls: the two parent shapes that already resolve. These also live in the active
        // `TypeIsActualEnum.cs`, so a regression in them is caught there rather than here.
        Check(!typeof(DerivedFromClosedBase<>).IsEnum);
        Check(!IsActualEnum(typeof(DerivedFromClosedBase<>)));

        // The gap. Both queries read `MethodTable::ParentMethodTable`, and neither is an enum,
        // so both must answer false rather than aborting.
        Check(!typeof(DerivedFromSharedBase<>).IsEnum);
        Check(!IsActualEnum(typeof(DerivedFromSharedBase<>)));
        Check(!typeof(DerivedFromNestedSharedBase<>).IsEnum);
        Check(!IsActualEnum(typeof(DerivedFromNestedSharedBase<>)));

        // Answering false is necessary but not sufficient: a walk that reported *no* parent at
        // all would satisfy every check above while being wrong, since a null parent is how
        // CoreCLR spells "this is System.Object". These two pin that the real base was found.
        Check(typeof(DerivedFromSharedBase<>).BaseType.Name == "OpenBase`1");
        Check(typeof(DerivedFromSharedBase<>).IsSubclassOf(typeof(object)));

        return firstFailure;
    }
}
