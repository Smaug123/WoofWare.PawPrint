using System;

/// <summary>
/// Characterization tests for managed pointer (byref) semantics.
/// Exercises ref-to-local and passing ref to methods.
/// </summary>
class ByrefSemantics
{
    // --- Test 1: ref to local variable ---
    static int RefToLocal()
    {
        int x = 42;
        ref int r = ref x;
        r = 100;
        // Writing through ref should update the original local.
        if (x != 100) return 1;
        return 0;
    }

    // --- Test 2: ref to field of local struct (stind through Field(LocalVariable)) ---
    struct S
    {
        public int X;
        public int Y;
    }

    static int RefToStructField()
    {
        S s = new S();
        s.X = 1;
        s.Y = 2;
        ref int r = ref s.X;
        r = 99;
        // Writing through ref should update the struct's field.
        if (s.X != 99) return 2;
        // Other field should be unaffected.
        if (s.Y != 2) return 3;
        return 0;
    }

    // --- Test 3: passing ref to a method (local variable) ---
    static void SetViaRef(ref int target, int value)
    {
        target = value;
    }

    static int RefPassedToMethod()
    {
        int local = 0;
        SetViaRef(ref local, 42);
        if (local != 42) return 4;
        return 0;
    }

    static int Main(string[] args)
    {
        int result;

        result = RefToLocal();
        if (result != 0) return result;

        result = RefToStructField();
        if (result != 0) return result;

        result = RefPassedToMethod();
        if (result != 0) return result;

        return 0;
    }
}
