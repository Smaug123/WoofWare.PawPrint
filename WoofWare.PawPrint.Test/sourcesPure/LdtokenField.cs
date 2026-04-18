using System;
using System.Linq;
using System.Reflection;
using System.Runtime.CompilerServices;

namespace LdtokenFieldTest
{
    class Program
    {
        // Various field types to test ldtoken with
        public static int StaticIntField = 42;
        public string InstanceStringField = "test";
        private readonly double PrivateReadonlyField = 3.14;
        internal decimal InternalField;
        protected bool ProtectedField;
        public static readonly DateTime StaticReadonlyField = DateTime.MinValue;

        // Generic type fields
        public GenericClass<int>.NestedClass<string> GenericField;

        static int Main(string[] args)
        {
            int testsFailed = 0;

            // Test 1: Static field via FieldInfo
            FieldInfo staticField = typeof(Program).GetField(nameof(StaticIntField));
            if (staticField == null || staticField.FieldType != typeof(int))
            {
                testsFailed++;
            }

            // Test 2: Instance field via FieldInfo
            FieldInfo instanceField = typeof(Program).GetField(nameof(InstanceStringField));
            if (instanceField == null || instanceField.FieldType != typeof(string))
            {
                testsFailed++;
            }

            // Test 3: Private field via FieldInfo with binding flags
            FieldInfo privateField = typeof(Program).GetField("PrivateReadonlyField",
                BindingFlags.NonPublic | BindingFlags.Instance);
            if (privateField == null || privateField.FieldType != typeof(double))
            {
                testsFailed++;
            }

            // Test 4: Using RuntimeFieldHandle directly
            RuntimeFieldHandle handle = staticField.FieldHandle;
            FieldInfo fieldFromHandle = FieldInfo.GetFieldFromHandle(handle);
            if (!ReferenceEquals(fieldFromHandle, staticField))
            {
                testsFailed++;
            }

            // Test 5: Field from generic type
            Type genericType = typeof(GenericClass<>);
            FieldInfo genericFieldInfo = genericType.GetField("GenericField");
            if (genericFieldInfo == null)
            {
                testsFailed++;
            }

            // Test 6: Field from nested type
            Type nestedType = typeof(OuterClass.InnerClass);
            FieldInfo nestedField = nestedType.GetField("NestedField");
            if (nestedField == null || nestedField.FieldType != typeof(int))
            {
                testsFailed++;
            }

            // Test 7: Field handle with generic context
            Type constructedGeneric = typeof(GenericClass<int>);
            FieldInfo constructedField = constructedGeneric.GetField("GenericField");
            RuntimeFieldHandle genericHandle = constructedField.FieldHandle;
            FieldInfo reconstructed = FieldInfo.GetFieldFromHandle(genericHandle, constructedGeneric.TypeHandle);
            if (reconstructed.DeclaringType != constructedGeneric)
            {
                testsFailed++;
            }

            // Test 8: Struct field
            Type structType = typeof(TestStruct);
            FieldInfo structField = structType.GetField("StructField");
            if (structField == null || structField.FieldType != typeof(long))
            {
                testsFailed++;
            }

            // Test 9: Volatile field
            FieldInfo volatileField = typeof(VolatileFieldClass).GetField("VolatileField");
            if (volatileField == null || !volatileField.GetRequiredCustomModifiers().Any(t => t == typeof(IsVolatile)))
            {
                testsFailed++;
            }

            return testsFailed;
        }
    }

    // Supporting types for testing
    public class GenericClass<T>
    {
        public T GenericField;

        public class NestedClass<U>
        {
            public U NestedGenericField;
        }
    }

    public class OuterClass
    {
        public class InnerClass
        {
            public int NestedField = 100;
        }
    }

    public struct TestStruct
    {
        public long StructField;
    }

    public class VolatileFieldClass
    {
        public volatile int VolatileField;
    }
}
