using System;

// A covariant-return override does not take the slot it overrides. Roslyn emits it `newslot` plus a
// MethodImpl naming the base declaration, so the base slot keeps its original *declaration* while
// its *implementation* becomes the override -- CoreCLR's `MethodTableBuilder::SetVirtualMethodImpl`
// changes the Impl and leaves the Decl. Dispatch through the base declaration must therefore still
// reach the override.
//
// Both shapes here have exactly one covariant level, which is what makes them the shapes PawPrint
// already serves: the MethodImpl on the middle type names the call site's own declaration, so
// `findMatchingMethodImplBodies` finds it directly. Chains with a covariant step at *every* level
// need the base slot's content to be carried up transitively, which is a different rule and is
// parked as `CovariantReturnDispatchMultiHop.cs`.
//
//  * `A`/`B`/`C`: `B` is covariant, `C` is a plain override of `B`.
//  * `GA<int>`/`GB`/`GC`: the same over a generic base, where the MethodImpl's declaration is a
//    MemberRef whose parent is a *TypeSpec* rather than a TypeDef. Resolving that declaration is a
//    distinct piece of work from resolving a TypeDef one, and any guest covariant override of a
//    generic base takes this shape.
//
// Mid-chain receivers (checks 4 and 8) are what stop a rule from simply answering with the
// most-derived declaration it can find: dispatching on a `B` must reach `B.F`, not `C.F`.
//
// Exit code is the index of the first failing check, so a failure names itself.

public class A
{
    public virtual object F() => "A";
}

public class B : A
{
    public override string F() => "B";
}

public class C : B
{
    public override string F() => "C";
}

public class GA<T>
{
    public virtual object G() => "GA";
}

public class GB : GA<int>
{
    public override string G() => "GB";
}

public class GC : GB
{
    public override string G() => "GC";
}

public static class Program
{
    public static int Main()
    {
        C c = new C();

        if ((string)((A)c).F() != "C") return 1;
        if (((B)c).F() != "C") return 2;
        if (c.F() != "C") return 3;

        B b = new B();
        if ((string)((A)b).F() != "B") return 4;

        GC gc = new GC();

        if ((string)((GA<int>)gc).G() != "GC") return 5;
        if (((GB)gc).G() != "GC") return 6;
        if (gc.G() != "GC") return 7;

        GB gb = new GB();
        if ((string)((GA<int>)gb).G() != "GB") return 8;

        return 0;
    }
}
