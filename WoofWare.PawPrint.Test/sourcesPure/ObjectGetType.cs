using System;

public class ObjectGetType
{
    class Base { }
    class Derived : Base { }

    struct PlainStruct
    {
        public int Value;
    }

    enum SmallEnum
    {
        One = 1
    }

    static bool Same(Type actual, Type expected)
    {
        return object.ReferenceEquals(actual, expected);
    }

    public static int Main(string[] args)
    {
        object plainObject = new object();
        if (!Same(plainObject.GetType(), typeof(object))) return 1;

        Derived derived = new Derived();
        if (!Same(derived.GetType(), typeof(Derived))) return 2;

        Base asBase = derived;
        if (!Same(asBase.GetType(), typeof(Derived))) return 3;

        object boxedInt = 123;
        if (!Same(boxedInt.GetType(), typeof(int))) return 4;

        int unboxedInt = 456;
        if (!Same(unboxedInt.GetType(), typeof(int))) return 5;

        PlainStruct plainStruct = new PlainStruct { Value = 7 };
        if (!Same(plainStruct.GetType(), typeof(PlainStruct))) return 6;

        SmallEnum smallEnum = SmallEnum.One;
        if (!Same(smallEnum.GetType(), typeof(SmallEnum))) return 7;

        return 0;
    }
}
