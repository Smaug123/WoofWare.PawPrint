using System;

public class GetDeclaringTypeNestedGeneric
{
    public static int Main(string[] argv)
    {
        Type intDeclaringType = typeof(Outer<int>.Inner).DeclaringType;
        Type stringDeclaringType = typeof(Outer<string>.Inner).DeclaringType;
        Type openOuterType = typeof(Outer<>);

        if (!object.ReferenceEquals(intDeclaringType, openOuterType))
        {
            return 1;
        }

        if (!object.ReferenceEquals(stringDeclaringType, openOuterType))
        {
            return 2;
        }

        if (object.ReferenceEquals(intDeclaringType, typeof(Outer<int>)))
        {
            return 3;
        }

        Type nestedGenericDeclaringType = typeof(Outer<int>.InnerGeneric<string>).DeclaringType;

        if (!object.ReferenceEquals(nestedGenericDeclaringType, openOuterType))
        {
            return 4;
        }

        return intDeclaringType.IsGenericType ? 0 : 5;
    }
}

public class Outer<T>
{
    public class Inner
    {
    }

    public class InnerGeneric<U>
    {
    }
}
