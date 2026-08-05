using System;
using System.Reflection;

class G<T>
{
}

class Program
{
    static void M<T> ()
    {
    }

    static int Main (string[] args)
    {
        // `MakeGenericMethod` accepts a type argument that still contains generic parameters.
        // Real .NET returns a MethodInfo with ContainsGenericParameters = true, which can be
        // inspected but not invoked; both an open generic type definition and a bare type
        // parameter are legal arguments.
        //
        // PawPrint cannot represent the resulting handle: `MethodHandle.MethodGenerics` is a
        // `ConcreteTypeHandle list`, and `ConcreteTypeHandle` indexes `AllConcreteTypes`, whose
        // entries carry only *closed* generic arguments. So
        // `RuntimeMethodHandle_GetStubIfNeededSlow` fails with a precise TODO here. Widening the
        // representation reaches concretization and every other MethodHandle consumer, so this
        // case is parked in TestPureCases.unimplemented rather than being solved alongside the
        // QCall itself.
        MethodInfo def = typeof (Program).GetMethod ("M", BindingFlags.Static | BindingFlags.NonPublic);

        if (def == null)
            return 1;

        MethodInfo openArgument = def.MakeGenericMethod (typeof (G<>));

        if (!openArgument.ContainsGenericParameters)
            return 2;

        if (openArgument.GetGenericArguments ().Length != 1)
            return 3;

        if (openArgument.GetGenericArguments ()[0] != typeof (G<>))
            return 4;

        Type typeParameter = typeof (G<>).GetGenericArguments ()[0];
        MethodInfo parameterArgument = def.MakeGenericMethod (typeParameter);

        if (!parameterArgument.ContainsGenericParameters)
            return 5;

        if (parameterArgument.GetGenericArguments ()[0] != typeParameter)
            return 6;

        return 0;
    }
}
