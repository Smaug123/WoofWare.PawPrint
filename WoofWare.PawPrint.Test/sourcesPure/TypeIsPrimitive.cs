using System;
using System.Reflection;

namespace TypeIsPrimitive
{
    enum ByteEnum : byte { A, B }

    enum IntEnum { A, B }

    struct MyStruct { public int X; }

    class MyClass { }

    class Box<T> { }

    delegate void MyDelegate();

    class Program
    {
        static unsafe int Main(string[] args)
        {
            // `Type.IsPrimitive` is `IsPrimitiveType(GetCorElementType())`: true for exactly
            // the fourteen CorElementTypes I1,U1,I2,U2,I4,U4,I8,U8,R4,R8,I,U,CHAR,BOOLEAN.
            if (!typeof(bool).IsPrimitive) return 1;
            if (!typeof(char).IsPrimitive) return 2;
            if (!typeof(sbyte).IsPrimitive) return 3;
            if (!typeof(byte).IsPrimitive) return 4;
            if (!typeof(short).IsPrimitive) return 5;
            if (!typeof(ushort).IsPrimitive) return 6;
            if (!typeof(int).IsPrimitive) return 7;
            if (!typeof(uint).IsPrimitive) return 8;
            if (!typeof(long).IsPrimitive) return 9;
            if (!typeof(ulong).IsPrimitive) return 10;
            if (!typeof(float).IsPrimitive) return 11;
            if (!typeof(double).IsPrimitive) return 12;

            // ELEMENT_TYPE_I / ELEMENT_TYPE_U are in the mask, so the pointer-sized integers
            // are primitive even though they are ordinary managed structs in metadata.
            if (!typeof(IntPtr).IsPrimitive) return 13;
            if (!typeof(UIntPtr).IsPrimitive) return 14;
            if (!typeof(nint).IsPrimitive) return 15;
            if (!typeof(nuint).IsPrimitive) return 16;

            // Decimal is a plain value type: its MethodTable is ELEMENT_TYPE_VALUETYPE.
            if (typeof(decimal).IsPrimitive) return 17;

            // String and Object have their own ELEMENT_TYPE shorthands in signature blobs,
            // but a MethodTable reports ELEMENT_TYPE_CLASS for both, and neither shorthand
            // is in the mask anyway.
            if (typeof(string).IsPrimitive) return 18;
            if (typeof(object).IsPrimitive) return 19;

            // ELEMENT_TYPE_VOID (0x01) is below the mask's lowest set bit.
            if (typeof(void).IsPrimitive) return 20;

            // An enum's *underlying* type is primitive, but the enum is not: CoreCLR
            // categorises it as PrimitiveValueType, and `GetSignatureCorElementType` maps
            // that whole category to ELEMENT_TYPE_VALUETYPE rather than to the underlying
            // element type. Both a byte-backed and an int-backed enum must say false.
            if (typeof(ByteEnum).IsPrimitive) return 21;
            if (typeof(IntEnum).IsPrimitive) return 22;

            if (typeof(MyStruct).IsPrimitive) return 23;
            if (typeof(MyClass).IsPrimitive) return 24;
            if (typeof(MyDelegate).IsPrimitive) return 25;
            if (typeof(ValueType).IsPrimitive) return 26;
            if (typeof(Enum).IsPrimitive) return 27;

            // Nullable is its own MethodTable category, also mapped to ELEMENT_TYPE_VALUETYPE.
            if (typeof(int?).IsPrimitive) return 28;

            // Structural types: SZARRAY, ARRAY, PTR, BYREF. None is in the mask, and in
            // particular an array or pointer *of* a primitive is not itself primitive.
            if (typeof(int[]).IsPrimitive) return 29;
            if (typeof(int[,]).IsPrimitive) return 30;
            if (typeof(int*).IsPrimitive) return 31;
            if (typeof(void*).IsPrimitive) return 32;

            // An open generic definition and its type parameters (ELEMENT_TYPE_VAR).
            if (typeof(Box<>).IsPrimitive) return 33;
            if (typeof(Box<>).GetGenericArguments()[0].IsPrimitive) return 34;

            // A closed generic instantiation over a primitive is not itself primitive.
            if (typeof(Box<int>).IsPrimitive) return 35;

            // Reached through a generic method's own type parameter, so the receiver is a
            // closed handle produced by substitution rather than by a `ldtoken` of a
            // literal type.
            if (!IsPrimitiveOf<int>()) return 36;
            if (IsPrimitiveOf<MyStruct>()) return 37;
            if (IsPrimitiveOf<IntEnum>()) return 38;
            if (IsPrimitiveOf<string>()) return 39;

            // `IsPrimitiveImpl` is abstract on `Type`, so the getter's `callvirt` must be a
            // real virtual dispatch: a receiver that is not a `RuntimeType` has to answer
            // from its own override rather than from any handle the runtime holds. These two
            // fail if the getter is ever short-circuited on the declaring type alone, which
            // is the property that keeps it out of `Intrinsics.call`.
            if (!new TypeDelegator(typeof(int)).IsPrimitive) return 40;
            if (new TypeDelegator(typeof(string)).IsPrimitive) return 41;
            if (new AlwaysPrimitiveType().IsPrimitive) return 42;

            return 0;
        }

        /// A `Type` subclass whose answer disagrees with the type it delegates to, so the
        /// override is the only thing that can produce it. `TypeDelegator` forwards
        /// `IsPrimitiveImpl` to `typeImpl`, which here is `int` — a primitive — while the
        /// override below says false.
        sealed class AlwaysPrimitiveType : TypeDelegator
        {
            public AlwaysPrimitiveType()
                : base(typeof(int))
            {
            }

            protected override bool IsPrimitiveImpl() => false;
        }

        static bool IsPrimitiveOf<T>() => typeof(T).IsPrimitive;
    }
}
