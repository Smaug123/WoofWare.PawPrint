using System;
using System.Reflection;

// `MethodInfo.GetGenericMethodDefinition`, the BCL's only route to the
// `RuntimeMethodHandle_StripMethodInstantiation` QCall (RuntimeMethodInfo.CoreCLR.cs:468). The QCall
// drops the method's own instantiation and keeps the declaring type's, and the managed side then
// rebinds the result onto the method's declaring type with `RuntimeType.GetMethodBase`.
//
// The shapes vary the two halves independently: the declaring type is non-generic, closed over a
// value type, closed over a reference type (which CoreCLR shares over `System.__Canon`, so its
// stripped handle is on the canonical type and the rebind is what restores `Holder<string>`), and a
// struct (whose instance methods reflection hands out without an unboxing stub).
class MethodGetGenericMethodDefinition
{
    class Holder<T>
    {
        internal static U Pair<U>(T t, U u)
        {
            return u;
        }

        internal U Instance<U>(U u)
        {
            return u;
        }
    }

    struct ValueHolder<T>
    {
        internal U Instance<U>(U u)
        {
            return u;
        }
    }

    class Base<T>
    {
        internal U Inherited<U>(U u)
        {
            return u;
        }
    }

    class Derived : Base<int>
    {
    }

    static T Identity<T>(T t)
    {
        return t;
    }

    const BindingFlags Any =
        BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static | BindingFlags.Instance;

    // What `GetGenericMethodDefinition` must hand back, checked against the definition that
    // `GetMethod` reached directly: the same method, on the same declaring type, still generic in its
    // own parameter.
    static bool IsDefinitionOf(MethodInfo stripped, MethodInfo definition, Type declaringType)
    {
        if (stripped == null || definition == null)
        {
            return false;
        }

        if (!stripped.IsGenericMethodDefinition || !stripped.IsGenericMethod)
        {
            return false;
        }

        if (stripped.DeclaringType != declaringType)
        {
            return false;
        }

        if (!stripped.Equals(definition) || stripped != definition)
        {
            return false;
        }

        Type[] arguments = stripped.GetGenericArguments();

        if (arguments.Length != 1 || !arguments[0].IsGenericParameter)
        {
            return false;
        }

        return true;
    }

    static int Main(string[] args)
    {
        // A generic method on a non-generic type.
        MethodInfo identity = typeof(MethodGetGenericMethodDefinition).GetMethod("Identity", Any);
        MethodInfo identityOfInt = identity.MakeGenericMethod(typeof(int));

        if (!IsDefinitionOf(identityOfInt.GetGenericMethodDefinition(), identity, typeof(MethodGetGenericMethodDefinition)))
        {
            return 1;
        }

        // A definition is its own definition.
        if (!IsDefinitionOf(identity.GetGenericMethodDefinition(), identity, typeof(MethodGetGenericMethodDefinition)))
        {
            return 2;
        }

        // A declaring type closed over a value type: the class instantiation survives.
        MethodInfo pairOnInt = typeof(Holder<int>).GetMethod("Pair", Any);
        MethodInfo pairOnIntOfString = pairOnInt.MakeGenericMethod(typeof(string));

        if (!IsDefinitionOf(pairOnIntOfString.GetGenericMethodDefinition(), pairOnInt, typeof(Holder<int>)))
        {
            return 3;
        }

        // A declaring type closed over a reference type.
        MethodInfo pairOnString = typeof(Holder<string>).GetMethod("Pair", Any);
        MethodInfo pairOnStringOfInt = pairOnString.MakeGenericMethod(typeof(int));

        if (!IsDefinitionOf(pairOnStringOfInt.GetGenericMethodDefinition(), pairOnString, typeof(Holder<string>)))
        {
            return 4;
        }

        // A definition on that declaring type is its own definition.
        if (!IsDefinitionOf(pairOnString.GetGenericMethodDefinition(), pairOnString, typeof(Holder<string>)))
        {
            return 5;
        }

        // The definition's parameters are the declaring type's argument and the method's own formal.
        ParameterInfo[] parameters = pairOnStringOfInt.GetGenericMethodDefinition().GetParameters();

        if (parameters.Length != 2 || parameters[0].ParameterType != typeof(string) || !parameters[1].ParameterType.IsGenericParameter)
        {
            return 6;
        }

        // An instance method.
        MethodInfo instanceOnObject = typeof(Holder<object>).GetMethod("Instance", Any);

        if (!IsDefinitionOf(instanceOnObject.MakeGenericMethod(typeof(long)).GetGenericMethodDefinition(), instanceOnObject, typeof(Holder<object>)))
        {
            return 7;
        }

        // An instance method on a struct.
        MethodInfo onValueHolder = typeof(ValueHolder<byte>).GetMethod("Instance", Any);

        if (!IsDefinitionOf(onValueHolder.MakeGenericMethod(typeof(string)).GetGenericMethodDefinition(), onValueHolder, typeof(ValueHolder<byte>)))
        {
            return 8;
        }

        // Reflected through a subclass: `GetGenericMethodDefinition` rebinds onto the *declaring*
        // type, so the result's ReflectedType is `Base<int>` and not `Derived`.
        MethodInfo inheritedViaDerived = typeof(Derived).GetMethod("Inherited", Any);
        MethodInfo inheritedStripped = inheritedViaDerived.MakeGenericMethod(typeof(char)).GetGenericMethodDefinition();

        if (inheritedStripped.ReflectedType != typeof(Base<int>) || inheritedStripped.DeclaringType != typeof(Base<int>))
        {
            return 9;
        }

        if (!IsDefinitionOf(inheritedStripped, typeof(Base<int>).GetMethod("Inherited", Any), typeof(Base<int>)))
        {
            return 10;
        }

        // A non-generic method has no definition to strip to.
        try
        {
            typeof(MethodGetGenericMethodDefinition).GetMethod("Main", Any).GetGenericMethodDefinition();
            return 11;
        }
        catch (InvalidOperationException)
        {
        }

        return 0;
    }
}
