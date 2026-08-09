// `RuntimeHelpers.GetObjectValue` is one of the few CoreLib methods PawPrint does not intercept,
// so the guest runs the real managed body:
//
//     MethodTable* pMT = GetMethodTable(obj);
//     if (!pMT->IsValueType || pMT->IsPrimitive) return obj;
//     return obj.MemberwiseClone();
//
// which makes it a reachable observation of the projected `MethodTable.Flags`. CoreCLR gives an
// enum the primitive category (its element type normalises to the underlying integer), so
// `IsPrimitive` is true and a boxed enum comes back as the *same reference*.
//
// The three rows below are what make this discriminating rather than merely consistent with the
// fix: a true primitive and an enum must both come back as the same reference, and an ordinary
// struct must *not*, because it is a value type that is not primitive and so takes the
// `MemberwiseClone` branch. A projection that called every value type primitive would satisfy the
// first two rows and fail the third.
//
// The struct row needs `Object.MemberwiseClone` to work, which is why it could not be written
// until that landed.

using System;
using System.Runtime.CompilerServices;

public class Program
{
    enum E
    {
        A = 1,
    }

    struct Plain
    {
        public int X;
    }

    public static int Main(string[] args)
    {
        // A true primitive is IsValueType && IsPrimitive, so it is returned as-is.
        object boxedInt = 5;
        if (!ReferenceEquals(RuntimeHelpers.GetObjectValue(boxedInt), boxedInt))
        {
            return 1;
        }

        // The case under test: an enum is IsPrimitive on CoreCLR too, so it must also be returned
        // as-is rather than cloned.
        object boxedEnum = E.A;
        if (!ReferenceEquals(RuntimeHelpers.GetObjectValue(boxedEnum), boxedEnum))
        {
            return 2;
        }

        // The discriminating row: an ordinary struct is a value type that is *not* primitive, so
        // it must be cloned rather than returned as-is.
        object boxedStruct = new Plain { X = 7 };
        object clonedStruct = RuntimeHelpers.GetObjectValue(boxedStruct);
        if (ReferenceEquals(clonedStruct, boxedStruct))
        {
            return 3;
        }

        // ...and the clone must carry the same contents, so that "cloned" is not being satisfied
        // by handing back some unrelated object.
        if (((Plain) clonedStruct).X != 7)
        {
            return 4;
        }

        return 0;
    }
}
