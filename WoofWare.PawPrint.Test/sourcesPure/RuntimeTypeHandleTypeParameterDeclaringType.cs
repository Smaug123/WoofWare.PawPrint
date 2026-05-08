using System;

namespace RuntimeTypeHandleTypeParameterDeclaringType
{
    class Box<T> { }

    class Pair<T, U> { }

    class Outer
    {
        public class Inner<X> { }
    }

    class Program
    {
        static int Main(string[] args)
        {
            // The DeclaringType of a type-generic parameter is the open generic type
            // that owns it. Reference equality must hold against typeof(Box<>) — the
            // RuntimeType registry keys on structural identity, so the parameter's
            // DeclaringType and the original typeof(Box<>) must be the same instance.
            Type boxOpen = typeof(Box<>);
            Type boxParam = boxOpen.GetGenericArguments()[0];
            if (boxParam.DeclaringType == null) return 1;
            if (!object.ReferenceEquals(boxParam.DeclaringType, boxOpen)) return 2;

            // Both parameters of Pair<,> must report Pair<,> itself as their declaring
            // type, not different types. Their position differs but their owner is shared.
            Type pairOpen = typeof(Pair<,>);
            Type[] pairParams = pairOpen.GetGenericArguments();
            if (pairParams[0].DeclaringType == null) return 3;
            if (pairParams[1].DeclaringType == null) return 4;
            if (!object.ReferenceEquals(pairParams[0].DeclaringType, pairOpen)) return 5;
            if (!object.ReferenceEquals(pairParams[1].DeclaringType, pairOpen)) return 6;

            // For a parameter on a nested generic type, the declaring type is the nested
            // type itself (Outer.Inner<>), not the enclosing Outer. This distinguishes
            // "declaring type of a parameter" from "declaring type of a type" — for the
            // latter, typeof(Outer.Inner<>).DeclaringType is Outer.
            Type innerOpen = typeof(Outer.Inner<>);
            Type innerParam = innerOpen.GetGenericArguments()[0];
            if (innerParam.DeclaringType == null) return 7;
            if (!object.ReferenceEquals(innerParam.DeclaringType, innerOpen)) return 8;
            if (object.ReferenceEquals(innerParam.DeclaringType, typeof(Outer))) return 9;

            return 0;
        }
    }
}
