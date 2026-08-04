// Virtual dispatch onto a type that carries a *type-level* `[Intrinsic]` attribute.
//
// `Int128`/`UInt128` are declared `[Intrinsic]` on the STRUCT, not on these members —
// `Int128.GetHashCode` is `HashCode.Combine(_lower, _upper)`, ordinary managed code with no
// method-level attribute. The type-level attribute tells the JIT it knows this type's own
// arithmetic surface; it says nothing about the `System.Object` overrides reached by boxing.
//
// So `callvirt System.Object::GetHashCode()` on a boxed Int128 must interpret the override's
// IL. PawPrint's type-level `[Intrinsic]` guard is therefore keyed on the CALL SITE's declaring
// type (`System.Object`, not intrinsic) rather than on the resolved override's declaring type
// (`Int128`, intrinsic) — see the comment in `IlMachineStateExecution.callMethod`.
//
// Note `default(Int128)` is reachable without any intrinsic constructor, which is what makes
// these instances obtainable in the first place.
//
// Scope note: `default` is the only way to obtain an `Int128` without touching a member whose
// call site is itself the intrinsic type. `Int128.One`, the `int` conversion, and `Equals`
// (which bottoms out in a non-virtual `call` to `Int128::Equals(Int128)`) are all still
// rejected. Those are pre-existing limitations, unchanged by the intrinsic-classification work
// and verified to fail identically on main — they are not what this test is pinning.

using System;

class Program
{
    static int TestInt128()
    {
        Int128 a = default;
        Int128 b = default;
        object oa = a;
        object ob = b;

        // Equal values hash equally. Avoids depending on the actual hash value.
        if (oa.GetHashCode() != ob.GetHashCode()) return 1;

        return 0;
    }

    static int TestUInt128()
    {
        UInt128 a = default;
        UInt128 b = default;

        if (((object)a).GetHashCode() != ((object)b).GetHashCode()) return 10;

        return 0;
    }

    static int Main()
    {
        int result;

        result = TestInt128();
        if (result != 0) return 100 + result;

        result = TestUInt128();
        if (result != 0) return 200 + result;

        return 0;
    }
}
