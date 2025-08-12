using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

public class TestInitobj
{
    // Simple struct with various primitive types
    private struct SimpleStruct
    {
        public int IntField;
        public bool BoolField;
        public char CharField;
        public double DoubleField;
        public byte ByteField;
    }

    // Nested struct
    private struct NestedStruct
    {
        public SimpleStruct Inner;
        public int OuterField;
    }

    // Struct with arrays and references
    private struct ComplexStruct
    {
        public object ObjectRef;
        public string StringRef;
        public int[] ArrayRef;
        public int ValueField;
    }

    // Generic struct
    private struct GenericStruct<T>
    {
        public T Value;
        public int Count;
    }

    // Struct for field tests
    private struct StructWithStructField
    {
        public SimpleStruct NestedField;
        public int OtherField;
    }

    // Class with struct field for heap test
    private class ClassWithStructField
    {
        public SimpleStruct StructField;
        public int IntField;
    }

    // Test 1: Initialize simple struct with local variable
    public static int Test1()
    {
        SimpleStruct s = new SimpleStruct
        {
            IntField = 42,
            BoolField = true,
            CharField = 'X',
            DoubleField = 3.14,
            ByteField = 255
        };

        // Verify initial values
        if (s.IntField != 42) return 1;
        if (s.BoolField != true) return 2;
        if (s.CharField != 'X') return 3;
        if (s.DoubleField != 3.14) return 4;
        if (s.ByteField != 255) return 5;

        // Use initobj to reset the struct
        Unsafe.InitBlockUnaligned(ref Unsafe.As<SimpleStruct, byte>(ref s), 0, (uint)Unsafe.SizeOf<SimpleStruct>());

        // Verify all fields are zeroed
        if (s.IntField != 0) return 6;
        if (s.BoolField != false) return 7;
        if (s.CharField != '\0') return 8;
        if (s.DoubleField != 0.0) return 9;
        if (s.ByteField != 0) return 10;

        return 0;
    }

    // Test 2: Initialize nested struct
    public static int Test2()
    {
        NestedStruct n = new NestedStruct
        {
            Inner = new SimpleStruct
            {
                IntField = 100,
                BoolField = true,
                CharField = 'A',
                DoubleField = 1.23,
                ByteField = 128
            },
            OuterField = 999
        };

        // Verify initial values
        if (n.Inner.IntField != 100) return 20;
        if (n.OuterField != 999) return 21;

        // Reset using default keyword (which should generate initobj)
        n = default(NestedStruct);

        // Verify all fields are zeroed
        if (n.Inner.IntField != 0) return 22;
        if (n.Inner.BoolField != false) return 23;
        if (n.Inner.CharField != '\0') return 24;
        if (n.Inner.DoubleField != 0.0) return 25;
        if (n.Inner.ByteField != 0) return 26;
        if (n.OuterField != 0) return 27;

        return 0;
    }

    // Test 3: Initialize struct with reference types
    public static int Test3()
    {
        ComplexStruct c = new ComplexStruct
        {
            ObjectRef = new object(),
            StringRef = "Hello",
            ArrayRef = new int[] { 1, 2, 3 },
            ValueField = 42
        };

        // Verify initial values
        if (c.ObjectRef == null) return 30;
        if (c.StringRef != "Hello") return 31;
        if (c.ArrayRef == null || c.ArrayRef.Length != 3) return 32;
        if (c.ValueField != 42) return 33;

        // Reset using default
        c = default(ComplexStruct);

        // Verify references are null and value is zero
        if (c.ObjectRef != null) return 34;
        if (c.StringRef != null) return 35;
        if (c.ArrayRef != null) return 36;
        if (c.ValueField != 0) return 37;

        return 0;
    }

    // Test 4: Initialize generic struct
    public static int Test4()
    {
        GenericStruct<int> gi = new GenericStruct<int>
        {
            Value = 123,
            Count = 456
        };

        if (gi.Value != 123) return 40;
        if (gi.Count != 456) return 41;

        gi = default(GenericStruct<int>);

        if (gi.Value != 0) return 42;
        if (gi.Count != 0) return 43;

        // Test with reference type
        GenericStruct<string> gs = new GenericStruct<string>
        {
            Value = "Test",
            Count = 789
        };

        if (gs.Value != "Test") return 44;
        if (gs.Count != 789) return 45;

        gs = default(GenericStruct<string>);

        if (gs.Value != null) return 46;
        if (gs.Count != 0) return 47;

        return 0;
    }

    // Test 5: Initialize struct in array element
    public static int Test5()
    {
        SimpleStruct[] array = new SimpleStruct[3];
        
        // Set values in first element
        array[0].IntField = 111;
        array[0].BoolField = true;
        array[0].CharField = 'Z';

        if (array[0].IntField != 111) return 50;
        if (array[0].BoolField != true) return 51;
        if (array[0].CharField != 'Z') return 52;

        // Reset first element using ref and Unsafe
        ref SimpleStruct firstElement = ref array[0];
        Unsafe.InitBlockUnaligned(ref Unsafe.As<SimpleStruct, byte>(ref firstElement), 0, (uint)Unsafe.SizeOf<SimpleStruct>());

        if (array[0].IntField != 0) return 53;
        if (array[0].BoolField != false) return 54;
        if (array[0].CharField != '\0') return 55;

        return 0;
    }

    // Test 6: Initialize struct through method parameter
    public static int Test6()
    {
        SimpleStruct s = new SimpleStruct
        {
            IntField = 200,
            BoolField = true,
            CharField = 'M',
            DoubleField = 2.71,
            ByteField = 64
        };

        ResetStruct(ref s);

        if (s.IntField != 0) return 60;
        if (s.BoolField != false) return 61;
        if (s.CharField != '\0') return 62;
        if (s.DoubleField != 0.0) return 63;
        if (s.ByteField != 0) return 64;

        return 0;
    }

    private static void ResetStruct(ref SimpleStruct s)
    {
        s = default(SimpleStruct);
    }

    // Test 7: Initialize multiple structs
    public static int Test7()
    {
        SimpleStruct s1 = new SimpleStruct { IntField = 1 };
        SimpleStruct s2 = new SimpleStruct { IntField = 2 };
        SimpleStruct s3 = new SimpleStruct { IntField = 3 };

        if (s1.IntField != 1) return 70;
        if (s2.IntField != 2) return 71;
        if (s3.IntField != 3) return 72;

        s1 = default(SimpleStruct);
        s2 = default(SimpleStruct);
        s3 = default(SimpleStruct);

        if (s1.IntField != 0) return 73;
        if (s2.IntField != 0) return 74;
        if (s3.IntField != 0) return 75;

        return 0;
    }

    // Test 8: Initialize struct passed as argument (tests Argument case)
    public static int Test8()
    {
        SimpleStruct s = new SimpleStruct
        {
            IntField = 333,
            BoolField = true,
            CharField = 'Q',
            DoubleField = 4.56,
            ByteField = 77
        };

        int result = InitializeArgumentStruct(ref s);
        if (result != 0) return result;

        // Verify struct was reset
        if (s.IntField != 0) return 80;
        if (s.BoolField != false) return 81;
        if (s.CharField != '\0') return 82;
        if (s.DoubleField != 0.0) return 83;
        if (s.ByteField != 0) return 84;

        return 0;
    }

    private static int InitializeArgumentStruct(ref SimpleStruct arg)
    {
        // Verify initial values
        if (arg.IntField != 333) return 85;
        if (arg.BoolField != true) return 86;
        
        // Reset using default - this should use initobj on the argument
        arg = default(SimpleStruct);
        
        return 0;
    }

    // Test 9: Initialize struct field (tests Field case)
    public static int Test9()
    {
        StructWithStructField container = new StructWithStructField
        {
            NestedField = new SimpleStruct
            {
                IntField = 444,
                BoolField = true,
                CharField = 'F',
                DoubleField = 7.89,
                ByteField = 88
            },
            OtherField = 555
        };

        // Verify initial values
        if (container.NestedField.IntField != 444) return 90;
        if (container.OtherField != 555) return 91;

        // Reset the nested field using ref
        ref SimpleStruct fieldRef = ref container.NestedField;
        fieldRef = default(SimpleStruct);

        // Verify nested field was reset but other field unchanged
        if (container.NestedField.IntField != 0) return 92;
        if (container.NestedField.BoolField != false) return 93;
        if (container.NestedField.CharField != '\0') return 94;
        if (container.NestedField.DoubleField != 0.0) return 95;
        if (container.NestedField.ByteField != 0) return 96;
        if (container.OtherField != 555) return 97;

        return 0;
    }

    // Test 10: Initialize struct in heap-allocated object (tests Heap case)
    public static int Test10()
    {
        ClassWithStructField obj = new ClassWithStructField
        {
            StructField = new SimpleStruct
            {
                IntField = 666,
                BoolField = true,
                CharField = 'H',
                DoubleField = 9.99,
                ByteField = 99
            },
            IntField = 777
        };

        // Verify initial values
        if (obj.StructField.IntField != 666) return 100;
        if (obj.IntField != 777) return 101;

        // Reset the struct field
        obj.StructField = default(SimpleStruct);

        // Verify struct field was reset but other field unchanged
        if (obj.StructField.IntField != 0) return 102;
        if (obj.StructField.BoolField != false) return 103;
        if (obj.StructField.CharField != '\0') return 104;
        if (obj.StructField.DoubleField != 0.0) return 105;
        if (obj.StructField.ByteField != 0) return 106;
        if (obj.IntField != 777) return 107;

        return 0;
    }

    // Test 11: Initialize struct through unsafe pointer manipulation
    public static unsafe int Test11()
    {
        SimpleStruct s = new SimpleStruct
        {
            IntField = 888,
            BoolField = true,
            CharField = 'P',
            DoubleField = 11.11,
            ByteField = 111
        };

        // Get a pointer to the struct
        SimpleStruct* ptr = &s;
        
        // Initialize through pointer
        *ptr = default(SimpleStruct);

        // Verify all fields are zeroed
        if (s.IntField != 0) return 110;
        if (s.BoolField != false) return 111;
        if (s.CharField != '\0') return 112;
        if (s.DoubleField != 0.0) return 113;
        if (s.ByteField != 0) return 114;

        return 0;
    }

    public static int Main(string[] argv)
    {
        var result = Test1();
        if (result != 0) return result;

        result = Test2();
        if (result != 0) return result;

        result = Test3();
        if (result != 0) return result;

        result = Test4();
        if (result != 0) return result;

        result = Test5();
        if (result != 0) return result;

        result = Test6();
        if (result != 0) return result;

        result = Test7();
        if (result != 0) return result;

        result = Test8();
        if (result != 0) return result;

        result = Test9();
        if (result != 0) return result;

        result = Test10();
        if (result != 0) return result;

        result = Test11();
        if (result != 0) return result;

        // All tests passed
        return 0;
    }
}