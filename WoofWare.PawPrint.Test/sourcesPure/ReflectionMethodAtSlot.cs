using System;
using System.Reflection;

// Every check here ends in `RuntimeTypeHandle.GetMethodAt(type, slot)`: the method occupying a
// slot of a type's method table, which CoreCLR reads as the slot's *content*
// (`MethodTable::GetMethodDescForSlot`). Two managed callers reach it from public API:
//
//  - `Associates.AssignAssociates` (Associates.cs:98), while a property is being populated for a
//    reflected type: a virtual accessor found on an ancestor is replaced by the override visible
//    from the reflected type, `GetMethodAt(reflectedType, GetSlot(accessor))`;
//  - `RuntimeMethodInfo.GetBaseDefinition` (RuntimeMethodInfo.CoreCLR.cs:335), which walks the
//    base chain reading the slot on each ancestor whose vtable is long enough.
//
// Exit code is the index of the first failing check, so a failure names itself.
public class Base
{
    public virtual int P => 1;
    public virtual int Q => 2;
    public virtual string M() => "base";
    public virtual string Shadowed() => "base";
    public override string ToString() => "Base";
}

public class Mid : Base
{
    public override int P => 3;
    public override string M() => "mid";
}

public class Derived : Mid
{
    public override int Q => 4;
    public new virtual string Shadowed() => "derived";
}

public class GenericBase<T>
{
    public virtual T Value => default!;
    public virtual T Echo(T value) => value;
}

public class GenericDerived : GenericBase<int>
{
    public override int Value => 7;
    public override int Echo(int value) => value + 1;
}

public class NonGenericBase
{
    public virtual int R => 1;
    public virtual string Named() => "base";
}

// Derives from a non-generic base on purpose: `GetBaseDefinition` walks `BaseType`, and the base
// type of a definition whose base is an open construction (`OpenDerived<T> : GenericBase<T>`) is
// not answered yet -- see sourcesPure/ReflectionBaseDefinitionOpenGenericBase.cs, parked on it.
public class OpenDerived<T> : NonGenericBase
{
    public override int R => 2;
    public override string Named() => "derived";
    public virtual T Own(T value) => value;
}

// An open definition over a *closed* generic ancestor: the occupant's declaring type on the
// definition's chain is `GenericBase<int>`, which is closed however generic its definition is.
public class ClosedBaseDerived<T> : GenericBase<int>
{
    public virtual T Extra(T value) => value;
}

public class Mid<A, B> : GenericBase<A>
{
}

public class TwoStep<T> : Mid<int, T>
{
}

public class HasArray<T>
{
    public void Takes(T[] values) { }
}

public abstract class AbstractBase
{
    public abstract int A();
}

public class Concrete : AbstractBase
{
    public override int A() => 9;
}

public static class Program
{
    static MethodInfo Getter(Type type, string property)
    {
        return type.GetProperty(property, BindingFlags.Public | BindingFlags.Instance).GetGetMethod();
    }

    public static int Main()
    {
        // --- Accessor association through the reflected type. ---
        // `P` is declared on Base and overridden on Mid; seen from Derived, the property found
        // first is Mid's, whose accessor is Mid's own.
        if (Getter(typeof(Derived), "P").DeclaringType != typeof(Mid)) return 1;
        // `Q` is Base's and Derived's; seen from Mid, only Base's version is in the chain.
        if (Getter(typeof(Mid), "Q").DeclaringType != typeof(Base)) return 2;
        if (Getter(typeof(Derived), "Q").DeclaringType != typeof(Derived)) return 3;
        // Seen from Base itself, its own accessor.
        if (Getter(typeof(Base), "P").DeclaringType != typeof(Base)) return 4;
        // A generic base instantiated at int: the accessor is the derived override.
        if (Getter(typeof(GenericDerived), "Value").DeclaringType != typeof(GenericDerived)) return 5;
        // And read from the closed base itself, the base's.
        if (Getter(typeof(GenericBase<int>), "Value").DeclaringType != typeof(GenericBase<int>)) return 6;
        // The reflected accessor really is the override: invoking it says so.
        if ((int)Getter(typeof(Derived), "P").Invoke(new Derived(), null) != 3) return 7;

        // --- GetBaseDefinition walks the chain by slot. ---
        MethodInfo derivedM = typeof(Derived).GetMethod("M");
        if (derivedM.DeclaringType != typeof(Mid)) return 8;
        if (derivedM.GetBaseDefinition().DeclaringType != typeof(Base)) return 9;
        if (typeof(Mid).GetMethod("M").GetBaseDefinition().DeclaringType != typeof(Base)) return 10;
        // A base definition is its own base definition.
        MethodInfo baseM = typeof(Base).GetMethod("M");
        if (!ReferenceEquals(baseM.GetBaseDefinition(), baseM)) return 11;
        // A `new virtual` shadow owns a fresh slot, so it is its own base definition.
        MethodInfo shadow = typeof(Derived).GetMethod("Shadowed");
        if (shadow.DeclaringType != typeof(Derived)) return 12;
        if (shadow.GetBaseDefinition().DeclaringType != typeof(Derived)) return 13;
        // Through a corelib base: the slot is System.Object's.
        if (typeof(Derived).GetMethod("ToString").GetBaseDefinition().DeclaringType != typeof(object)) return 14;
        if (typeof(Base).GetMethod("ToString").GetBaseDefinition().DeclaringType != typeof(object)) return 15;
        // A generic base at int, both an override and the base's own.
        if (typeof(GenericDerived).GetMethod("Echo").GetBaseDefinition().DeclaringType != typeof(GenericBase<int>)) return 16;
        if (typeof(GenericBase<int>).GetMethod("Echo").GetBaseDefinition().DeclaringType != typeof(GenericBase<int>)) return 17;
        // An abstract slot filled by an override.
        if (typeof(Concrete).GetMethod("A").GetBaseDefinition().DeclaringType != typeof(AbstractBase)) return 18;
        // Not checked here: an array. Its slots are System.Array's, but `typeof(int[]).GetMethod`
        // enumerates the array's introduced methods first, and PawPrint does not yet surface an
        // array's intrinsic Get/Set/Address methods (`RuntimeTypeHandle.GetFirstIntroducedMethod`
        // refuses), so no slot is ever read.
        // A non-virtual method is its own base definition without any slot being read.
        MethodInfo main = typeof(Program).GetMethod("Main");
        if (!ReferenceEquals(main.GetBaseDefinition(), main)) return 21;

        // --- On an open generic definition: the slot read on the definition's own method table. ---
        // Declared by the definition itself: its own slot, so its own base definition.
        MethodInfo own = typeof(OpenDerived<>).GetMethod("Own");
        if (own.GetBaseDefinition().DeclaringType != typeof(OpenDerived<>)) return 22;
        // Declared by Object, a non-generic base.
        if (typeof(OpenDerived<>).GetMethod("ToString").GetBaseDefinition().DeclaringType != typeof(object)) return 23;
        // Overriding a non-generic base: the base definition is the base's.
        if (typeof(OpenDerived<>).GetMethod("Named").GetBaseDefinition().DeclaringType != typeof(NonGenericBase)) return 24;
        // A property accessor read off the definition is the definition's own override.
        if (Getter(typeof(OpenDerived<>), "R").DeclaringType != typeof(OpenDerived<>)) return 25;
        if (Getter(typeof(NonGenericBase), "R").DeclaringType != typeof(NonGenericBase)) return 26;
        // A closed generic ancestor of an open definition: the inherited accessor and base
        // definition both belong to `GenericBase<int>`.
        if (Getter(typeof(ClosedBaseDerived<>), "Value").DeclaringType != typeof(GenericBase<int>)) return 27;
        if (typeof(ClosedBaseDerived<>).GetMethod("Echo").GetBaseDefinition().DeclaringType != typeof(GenericBase<int>)) return 28;
        if (typeof(ClosedBaseDerived<>).GetMethod("Extra").GetBaseDefinition().DeclaringType != typeof(ClosedBaseDerived<>)) return 29;

        // PROBE: an array over the variable as the receiver: its slots are System.Array's.
        Type openArray = typeof(HasArray<>).GetMethod("Takes").GetParameters()[0].ParameterType;
        if (openArray.GetProperty("SyncRoot").GetGetMethod().DeclaringType != typeof(Array)) return 31;

        return 0;
    }
}
