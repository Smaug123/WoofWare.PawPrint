// ECMA-335 III.4.10: without the `readonly.` prefix, `ldelema typeTok` must
// throw ArrayTypeMismatchException when the array's runtime element type is
// not exactly `typeTok`. This is the array-covariance trap: `string[]` is
// assignable to `object[]` for reads, but a writable byref into the storage
// would let a non-string be stored, breaking type safety.
//
// `ref object slot = ref objs[0]` against an `object[]`-typed local whose
// runtime allocation is `string[]` emits `ldelema object`, which the runtime
// must trap.

using System;

public class TestLdelemaArrayTypeMismatch
{
    public static int Main(string[] argv)
    {
        string[] strs = new string[] { "hello" };
        object[] objs = strs;

        try
        {
            ref object slot = ref objs[0];
            slot = "irrelevant";
            return 1;
        }
        catch (ArrayTypeMismatchException)
        {
            return 0;
        }
    }
}
