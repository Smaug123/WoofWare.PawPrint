using System;
using System.Reflection;

class Base<T>
{
    public virtual int Describe(T value) => 1;
    public virtual int Named() => 2;
    public static int Make() => 3;
}

class Pair<A, B>
{
    public virtual B Second(A first, B second) => second;
}

// Each parent below is an open construction: `Base<T>` over Derived's own `T`, `Base<B>` over the
// *second* formal of `Twice`, and `Pair<int, T>`, whose arguments mix a closed type and a variable.
class Derived<T> : Base<T>
{
    public override int Describe(T value) => 11;
}

class Twice<A, B> : Base<B> { }

class Mixed<T> : Pair<int, T> { }

class Program
{
    static int next = 1;
    static int firstFailure = 0;

    static void Check(bool ok)
    {
        int index = next;
        next = next + 1;
        if (!ok && firstFailure == 0)
        {
            firstFailure = index;
        }
    }

    // Methods whose declaring type is an open construction: the parent of an open generic
    // definition whose extends clause mentions one of its parameters. Such a type has method
    // descriptors of its own, distinct from the definition's, and its signatures read `!i` as its
    // own `i`th argument.
    //
    // Exit code is the index of the first failing check, so a failure names itself.
    static int Main(string[] args)
    {
        Type open = typeof(Derived<>).BaseType;
        Type derivedT = typeof(Derived<>).GetGenericArguments()[0];

        MethodInfo named = open.GetMethod("Named");
        Check(named != null && named.DeclaringType == open);
        // Not the definition's method: the handles differ, while asking twice gives the same one.
        Check(named.MethodHandle != typeof(Base<>).GetMethod("Named").MethodHandle);
        Check(named.MethodHandle == open.GetMethod("Named").MethodHandle);
        Check(!named.IsGenericMethodDefinition);

        // `!0` in `Describe(!0)` is the construction's argument, Derived's own `T`.
        MethodInfo describe = open.GetMethod("Describe");
        Check(describe.GetParameters()[0].ParameterType == derivedT);
        Check(describe.ContainsGenericParameters);

        // A static method is rebound by `GetStubIfNeeded` onto the construction.
        MethodInfo make = open.GetMethod("Make");
        Check(make.DeclaringType == open);
        Check(make.MethodHandle != typeof(Base<>).GetMethod("Make").MethodHandle);

        // A handle's declaring type is the construction: naming it recovers the method, and naming
        // nothing is refused because that declaring type is generic.
        MethodBase fromHandle = MethodBase.GetMethodFromHandle(named.MethodHandle, open.TypeHandle);
        Check(fromHandle.DeclaringType == open);
        Check(fromHandle.MethodHandle == named.MethodHandle);
        bool refused = false;
        try
        {
            MethodBase.GetMethodFromHandle(named.MethodHandle);
        }
        catch (ArgumentException)
        {
            refused = true;
        }
        Check(refused);

        // The overridden method's base definition is declared by the construction, and is the
        // very object the construction's own method list holds.
        MethodInfo baseDescribe = typeof(Derived<>).GetMethod("Describe").GetBaseDefinition();
        Check(baseDescribe.DeclaringType == open);
        Check(ReferenceEquals(baseDescribe, describe));

        // Substitution is by position: `Twice<A, B>`'s parent reads `!0` as `B`.
        MethodInfo twiceDescribe = typeof(Twice<,>).BaseType.GetMethod("Describe");
        Check(twiceDescribe.GetParameters()[0].ParameterType == typeof(Twice<,>).GetGenericArguments()[1]);

        // A mixed construction reads each `!i` as its own argument, closed or not.
        Type mixedT = typeof(Mixed<>).GetGenericArguments()[0];
        MethodInfo second = typeof(Mixed<>).BaseType.GetMethod("Second");
        Check(second.GetParameters()[0].ParameterType == typeof(int));
        Check(second.GetParameters()[1].ParameterType == mixedT);
        Check(second.ReturnType == mixedT);
        Check(second.DeclaringType == typeof(Mixed<>).BaseType);

        return firstFailure;
    }
}
