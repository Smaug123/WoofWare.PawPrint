using System;
using System.Reflection;

// `MethodBase.Invoke` on an instance method of a *value* type. CoreCLR forms `this` by
// `gc.target->UnBox()` (reflectioninvocation.cpp:502) — a pointer into the payload of the very box
// the caller handed over, not a copy of it — so a mutating method is observable through that box
// afterwards. That in-place behaviour is the part worth pinning: an implementation that unboxed into
// a copy would pass every non-mutating case here.
//
// No method below is virtual, and no type reflected over declares one: a virtual struct method would
// reach `RuntimeMethodHandle.GetSlot` during `GetMethod` (see
// `sourcesPure/ReflectionInvokeVirtualMethod.cs`) and would additionally raise CoreCLR's
// unboxing-stub question, which is a different `this` rule.
//
// As in the sibling files, each MethodInfo is invoked exactly once: after the first invocation
// `MethodInvokerCommon.DetermineStrategy_*` switches to a Reflection.Emit delegate and stops
// exercising the `RuntimeMethodHandle_InvokeMethod` QCall.
public class Program
{
    // One field, so the box is the "primitive-like" shape: PawPrint stores such a value in a single
    // wrapper cell rather than as a field map, and a byref to the box addresses that wrapper.
    private struct Counter
    {
        public int Value;

        public void Add (int n)
        {
            Value += n;
        }
    }

    // More than one field, and of mixed kinds, so the box is a genuine field map. A `this` that
    // addressed the wrong thing would corrupt the sibling field rather than merely losing the write.
    private struct Pair
    {
        public int First;
        public string Second;

        public string Describe ()
        {
            return First + ":" + Second;
        }

        public void Bump ()
        {
            First++;
        }
    }

    private static MethodInfo Get (Type t, string name)
    {
        MethodInfo m = t.GetMethod (
            name,
            BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic);

        if (m == null)
            throw new Exception ("could not find " + name);

        return m;
    }

    public static int Main (string[] args)
    {
        // Mutating method on a boxed single-field struct: the write lands in the caller's box.
        object boxedCounter = new Counter
        {
            Value = 40,
        };

        object addResult = Get (typeof (Counter), "Add").Invoke (boxedCounter, new object[] { 2 });

        if (addResult != null)
            return 1;

        if (((Counter) boxedCounter).Value != 42)
            return 2;

        // Multi-field struct, non-mutating, reference-type return: `this` must see both fields.
        object boxedPair = new Pair
        {
            First = 1,
            Second = "x",
        };

        object described = Get (typeof (Pair), "Describe").Invoke (boxedPair, null);

        if (!(described is string s) || s != "1:x")
            return 3;

        // Multi-field struct, mutating: the named field moves and its sibling does not.
        object bumpTarget = new Pair
        {
            First = 7,
            Second = "y",
        };

        Get (typeof (Pair), "Bump").Invoke (bumpTarget, null);

        Pair bumped = (Pair) bumpTarget;

        if (bumped.First != 8)
            return 4;

        if (bumped.Second != "y")
            return 5;

        return 0;
    }
}
