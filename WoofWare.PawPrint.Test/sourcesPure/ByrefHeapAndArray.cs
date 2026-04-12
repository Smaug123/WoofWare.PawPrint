using System;

/// <summary>
/// Characterization tests for byref semantics through heap object fields, array elements,
/// and nested struct fields via projection chains.
/// These exercise stind/stfld through ManagedPointerSource paths that are currently
/// unimplemented (Heap, ArrayIndex, Field(Field(...))).
/// </summary>
class ByrefHeapAndArray
{
    class MyObj
    {
        public int X;
    }

    struct Inner
    {
        public int Value;
    }

    struct Outer
    {
        public Inner In;
        public int Other;
    }

    // --- Test 1: ref to field of heap object ---
    static int RefToHeapField()
    {
        MyObj c = new MyObj();
        c.X = 1;
        ref int r = ref c.X;
        r = 99;
        // Writing through ref should update the heap object's field.
        if (c.X != 99) return 1;
        return 0;
    }

    // --- Test 2: ref to array element ---
    static int RefToArrayElement()
    {
        int[] arr = new int[3];
        arr[0] = 1;
        arr[1] = 2;
        arr[2] = 3;
        ref int r = ref arr[1];
        r = 99;
        // Writing through ref should update the array element.
        if (arr[1] != 99) return 2;
        // Other elements should be unaffected.
        if (arr[0] != 1) return 3;
        if (arr[2] != 3) return 4;
        return 0;
    }

    // --- Test 3: ref to heap object field, read back through both paths ---
    static int RefToHeapFieldReadBack()
    {
        MyObj obj = new MyObj();
        obj.X = 10;
        ref int r = ref obj.X;
        // Read through ref should see the value.
        if (r != 10) return 5;
        // Write through original path.
        obj.X = 20;
        // Read through ref should see the updated value.
        if (r != 20) return 6;
        return 0;
    }

    // --- Test 4: passing ref to a method (array element) ---
    static void SetViaRef(ref int target, int value)
    {
        target = value;
    }

    static int RefPassedToMethodArray()
    {
        int[] arr = new int[2];
        SetViaRef(ref arr[0], 77);
        if (arr[0] != 77) return 7;
        if (arr[1] != 0) return 8;
        return 0;
    }

    // --- Test 5: nested struct field byref ---
    static int NestedStructByref()
    {
        Outer o = new Outer();
        o.In.Value = 42;
        o.Other = 10;
        ref Inner r = ref o.In;
        r.Value = 99;
        // Writing through ref to nested struct should update the outer struct.
        if (o.In.Value != 99) return 9;
        // Other field should be unaffected.
        if (o.Other != 10) return 10;
        return 0;
    }

    static int Main(string[] args)
    {
        int result;

        result = RefToHeapField();
        if (result != 0) return result;

        result = RefToArrayElement();
        if (result != 0) return result;

        result = RefToHeapFieldReadBack();
        if (result != 0) return result;

        result = RefPassedToMethodArray();
        if (result != 0) return result;

        result = NestedStructByref();
        if (result != 0) return result;

        return 0;
    }
}
