using System;
using System.IO;
using System.Reflection;
using System.Reflection.Emit;

public class Base
{
    public virtual string Who() => "Base";
}

public class Derived : Base
{
    public override string Who() => "Derived";
}

public class Other
{
    private static int Secret() => 7;
}

public struct Val
{
    public int X;
    public int Get() => X;
    public override string ToString() => "Val" + X;
}

public class Gen<T>
{
    public static string Name() => typeof(T).Name;
    public virtual string Inst() => "Gen" + typeof(T).Name;
}

public static class Generic
{
    public static T Identity<T>(T x) => x;
}

public class Holder<T>
{
    public static T Echo(T x) => x;
}

public class Program
{
    // Dynamic methods whose `call` and `callvirt` name reflected methods rather than other dynamic
    // methods. `ILGenerator.Emit(OpCode, MethodInfo)` stores a bare RuntimeMethodHandle when the
    // method's declaring type is neither generic nor an array, and a GenericMethodInfo pairing the
    // handle with that type otherwise. Every expected answer, including each exception, was
    // measured on real .NET 10.
    //
    // Returns 0 on success, or the number of the first check that failed.

    private static DynamicMethod Make(Type returnType, Type[] parameters, Action<ILGenerator> body)
    {
        DynamicMethod dm = new DynamicMethod("Probe", returnType, parameters, typeof(Program).Module);
        body(dm.GetILGenerator());
        return dm;
    }

    private static MethodInfo MethodOf(Type type, string name)
    {
        return type.GetMethod(name, BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static | BindingFlags.Instance, null, Type.EmptyTypes, null);
    }

    public static int Main(string[] args)
    {
        // A static method on a non-generic type: a bare RuntimeMethodHandle.
        MethodInfo max = typeof(Math).GetMethod("Max", new Type[] { typeof(int), typeof(int) });
        Func<int> staticCall = (Func<int>) Make(typeof(int), Type.EmptyTypes, il =>
        {
            il.Emit(OpCodes.Ldc_I4, 40);
            il.Emit(OpCodes.Ldc_I4, 42);
            il.Emit(OpCodes.Call, max);
            il.Emit(OpCodes.Ret);
        }).CreateDelegate(typeof(Func<int>));
        if (staticCall() != 42)
        {
            return 1;
        }

        // An instantiated generic method on a non-generic type: the handle carries the method's
        // own instantiation. This is what the expression interpreter's zero-argument thunk calls.
        MethodInfo empty = typeof(Array).GetMethod("Empty").MakeGenericMethod(typeof(object));
        Func<object[]> emptyCall = (Func<object[]>) Make(typeof(object[]), Type.EmptyTypes, il =>
        {
            il.Emit(OpCodes.Call, empty);
            il.Emit(OpCodes.Ret);
        }).CreateDelegate(typeof(Func<object[]>));
        object[] emptyArray = emptyCall();
        if (emptyArray == null || emptyArray.Length != 0)
        {
            return 2;
        }

        // `callvirt` dispatches on the receiver's runtime type; `call` of the same method does not.
        MethodInfo who = typeof(Base).GetMethod("Who");
        Func<Base, string> virtualCall = (Func<Base, string>) Make(typeof(string), new Type[] { typeof(Base) }, il =>
        {
            il.Emit(OpCodes.Ldarg_0);
            il.Emit(OpCodes.Callvirt, who);
            il.Emit(OpCodes.Ret);
        }).CreateDelegate(typeof(Func<Base, string>));
        if (virtualCall(new Derived()) != "Derived")
        {
            return 3;
        }

        Func<Base, string> directCall = (Func<Base, string>) Make(typeof(string), new Type[] { typeof(Base) }, il =>
        {
            il.Emit(OpCodes.Ldarg_0);
            il.Emit(OpCodes.Call, who);
            il.Emit(OpCodes.Ret);
        }).CreateDelegate(typeof(Func<Base, string>));
        if (directCall(new Derived()) != "Base")
        {
            return 4;
        }

        // `callvirt` checks its receiver for null.
        try
        {
            virtualCall(null);
            return 5;
        }
        catch (NullReferenceException)
        {
        }

        // A method of a generic interface: a GenericMethodInfo, dispatched through the interface.
        MethodInfo compareTo = typeof(IComparable<int>).GetMethod("CompareTo");
        Func<object, int> interfaceCall = (Func<object, int>) Make(typeof(int), new Type[] { typeof(object) }, il =>
        {
            il.Emit(OpCodes.Ldarg_0);
            il.Emit(OpCodes.Castclass, typeof(IComparable<int>));
            il.Emit(OpCodes.Ldc_I4, 3);
            il.Emit(OpCodes.Callvirt, compareTo);
            il.Emit(OpCodes.Ret);
        }).CreateDelegate(typeof(Func<object, int>));
        if (interfaceCall(5) != 1)
        {
            return 6;
        }

        // Methods of closed generic types, over a value type and over a reference type.
        Func<string> genericStatic = (Func<string>) Make(typeof(string), Type.EmptyTypes, il =>
        {
            il.Emit(OpCodes.Call, typeof(Gen<int>).GetMethod("Name"));
            il.Emit(OpCodes.Ret);
        }).CreateDelegate(typeof(Func<string>));
        if (genericStatic() != "Int32")
        {
            return 7;
        }

        Func<Gen<string>, string> genericInstance = (Func<Gen<string>, string>) Make(typeof(string), new Type[] { typeof(Gen<string>) }, il =>
        {
            il.Emit(OpCodes.Ldarg_0);
            il.Emit(OpCodes.Callvirt, typeof(Gen<string>).GetMethod("Inst"));
            il.Emit(OpCodes.Ret);
        }).CreateDelegate(typeof(Func<Gen<string>, string>));
        if (genericInstance(new Gen<string>()) != "GenString")
        {
            return 8;
        }

        // An instance method of a value type, called through a byref to the argument.
        Func<Val, int> valueCall = (Func<Val, int>) Make(typeof(int), new Type[] { typeof(Val) }, il =>
        {
            il.Emit(OpCodes.Ldarga_S, (byte) 0);
            il.Emit(OpCodes.Call, typeof(Val).GetMethod("Get"));
            il.Emit(OpCodes.Ret);
        }).CreateDelegate(typeof(Func<Val, int>));
        if (valueCall(new Val { X = 42 }) != 42)
        {
            return 9;
        }

        // `callvirt` of `object.ToString` on a boxed value type reaches the struct's override.
        Func<object, string> boxedCall = (Func<object, string>) Make(typeof(string), new Type[] { typeof(object) }, il =>
        {
            il.Emit(OpCodes.Ldarg_0);
            il.Emit(OpCodes.Callvirt, typeof(object).GetMethod("ToString"));
            il.Emit(OpCodes.Ret);
        }).CreateDelegate(typeof(Func<object, string>));
        if (boxedCall(new Val { X = 5 }) != "Val5")
        {
            return 10;
        }

        // Real .NET makes no visibility check on a dynamic method's callee: a private method of an
        // unrelated type runs, without restrictedSkipVisibility.
        Func<int> privateCall = (Func<int>) Make(typeof(int), Type.EmptyTypes, il =>
        {
            il.Emit(OpCodes.Call, MethodOf(typeof(Other), "Secret"));
            il.Emit(OpCodes.Ret);
        }).CreateDelegate(typeof(Func<int>));
        if (privateCall() != 7)
        {
            return 11;
        }

        // `callvirt` of an abstract method dispatches to the override; `call` of it is refused.
        MethodInfo flush = MethodOf(typeof(Stream), "Flush");
        Action<Stream> virtualFlush = (Action<Stream>) Make(typeof(void), new Type[] { typeof(Stream) }, il =>
        {
            il.Emit(OpCodes.Ldarg_0);
            il.Emit(OpCodes.Callvirt, flush);
            il.Emit(OpCodes.Ret);
        }).CreateDelegate(typeof(Action<Stream>));
        virtualFlush(new MemoryStream());

        Action<Stream> directFlush = (Action<Stream>) Make(typeof(void), new Type[] { typeof(Stream) }, il =>
        {
            il.Emit(OpCodes.Ldarg_0);
            il.Emit(OpCodes.Call, flush);
            il.Emit(OpCodes.Ret);
        }).CreateDelegate(typeof(Action<Stream>));
        try
        {
            directFlush(new MemoryStream());
            return 12;
        }
        catch (BadImageFormatException e)
        {
            if (e.Message != "Bad IL format.")
            {
                return 13;
            }
        }

        // `callvirt` of a static method is refused as a missing method.
        Func<int> virtualStatic = (Func<int>) Make(typeof(int), Type.EmptyTypes, il =>
        {
            il.Emit(OpCodes.Ldc_I4, 40);
            il.Emit(OpCodes.Ldc_I4, 42);
            il.Emit(OpCodes.Callvirt, max);
            il.Emit(OpCodes.Ret);
        }).CreateDelegate(typeof(Func<int>));
        try
        {
            virtualStatic();
            return 14;
        }
        catch (MissingMethodException e)
        {
            if (e.Message != "Method not found: '?'.")
            {
                return 15;
            }
        }

        // A DynamicMethod is always static, so `callvirt` of one is refused in the same way.
        DynamicMethod fortyTwo = Make(typeof(int), Type.EmptyTypes, il =>
        {
            il.Emit(OpCodes.Ldc_I4, 42);
            il.Emit(OpCodes.Ret);
        });
        Func<int> virtualDynamic = (Func<int>) Make(typeof(int), Type.EmptyTypes, il =>
        {
            il.Emit(OpCodes.Callvirt, fortyTwo);
            il.Emit(OpCodes.Ret);
        }).CreateDelegate(typeof(Func<int>));
        try
        {
            virtualDynamic();
            return 18;
        }
        catch (MissingMethodException e)
        {
            if (e.Message != "Method not found: '?'.")
            {
                return 19;
            }
        }

        // The refusal comes before the arguments are looked at: a `callvirt` of a static method
        // with none of its arguments pushed is still MissingMethodException, not an invalid
        // program, for a reflected method and for a DynamicMethod alike.
        Func<int> virtualStaticNoArguments = (Func<int>) Make(typeof(int), Type.EmptyTypes, il =>
        {
            il.Emit(OpCodes.Callvirt, max);
            il.Emit(OpCodes.Ret);
        }).CreateDelegate(typeof(Func<int>));
        try
        {
            virtualStaticNoArguments();
            return 22;
        }
        catch (MissingMethodException)
        {
        }

        DynamicMethod takesInt = Make(typeof(int), new Type[] { typeof(int) }, il =>
        {
            il.Emit(OpCodes.Ldarg_0);
            il.Emit(OpCodes.Ret);
        });
        Func<int> virtualDynamicNoArguments = (Func<int>) Make(typeof(int), Type.EmptyTypes, il =>
        {
            il.Emit(OpCodes.Callvirt, takesInt);
            il.Emit(OpCodes.Ret);
        }).CreateDelegate(typeof(Func<int>));
        try
        {
            virtualDynamicNoArguments();
            return 23;
        }
        catch (MissingMethodException)
        {
        }

        // A reflected constructor, called as an ordinary instance method on an existing object.
        Action<Base> constructorCall = (Action<Base>) Make(typeof(void), new Type[] { typeof(Base) }, il =>
        {
            il.Emit(OpCodes.Ldarg_0);
            il.Emit(OpCodes.Call, typeof(object).GetConstructor(Type.EmptyTypes));
            il.Emit(OpCodes.Ret);
        }).CreateDelegate(typeof(Action<Base>));
        constructorCall(new Base());

        // A method of an open generic type definition is an invalid program.
        Func<string> openCall = (Func<string>) Make(typeof(string), Type.EmptyTypes, il =>
        {
            il.Emit(OpCodes.Call, typeof(Gen<>).GetMethod("Name"));
            il.Emit(OpCodes.Ret);
        }).CreateDelegate(typeof(Func<string>));
        try
        {
            openCall();
            return 16;
        }
        catch (InvalidProgramException)
        {
        }

        // A float returned through a generic parameter, bound by the method's instantiation and by
        // its declaring type's: the value on the stack after the call is a float either way.
        Func<float, float> floatIdentity = (Func<float, float>) Make(typeof(float), new Type[] { typeof(float) }, il =>
        {
            il.Emit(OpCodes.Ldarg_0);
            il.Emit(OpCodes.Call, typeof(Generic).GetMethod("Identity").MakeGenericMethod(typeof(float)));
            il.Emit(OpCodes.Ret);
        }).CreateDelegate(typeof(Func<float, float>));
        if (floatIdentity(1.5f) != 1.5f)
        {
            return 20;
        }

        Func<double, double> doubleEcho = (Func<double, double>) Make(typeof(double), new Type[] { typeof(double) }, il =>
        {
            il.Emit(OpCodes.Ldarg_0);
            il.Emit(OpCodes.Call, typeof(Holder<double>).GetMethod("Echo"));
            il.Emit(OpCodes.Ret);
        }).CreateDelegate(typeof(Func<double, double>));
        if (doubleEcho(2.5) != 2.5)
        {
            return 21;
        }

        // The thunk System.Linq.Expressions emits for a delegate of more than two parameters
        // (DelegateHelpers.CreateObjectArrayDelegateRefEmit), closed over the interpreter's
        // Func<object[], object>, whose Invoke it names through a GenericMethodInfo.
        Func<object[], object> handler = xs => (int) xs[0] * (int) xs[1] + (int) xs[2];
        DynamicMethod thunk = new DynamicMethod(
            "Thunk",
            typeof(int),
            new Type[] { typeof(Func<object[], object>), typeof(int), typeof(int), typeof(int) },
            typeof(Program),
            true);
        ILGenerator thunkIl = thunk.GetILGenerator();
        LocalBuilder array = thunkIl.DeclareLocal(typeof(object[]));
        LocalBuilder result = thunkIl.DeclareLocal(typeof(object));
        thunkIl.Emit(OpCodes.Ldc_I4, 3);
        thunkIl.Emit(OpCodes.Newarr, typeof(object));
        thunkIl.Emit(OpCodes.Stloc, array);
        for (int i = 0; i < 3; i++)
        {
            thunkIl.Emit(OpCodes.Ldloc, array);
            thunkIl.Emit(OpCodes.Ldc_I4, i);
            thunkIl.Emit(OpCodes.Ldarg, i + 1);
            thunkIl.Emit(OpCodes.Box, typeof(int));
            thunkIl.Emit(OpCodes.Stelem_Ref);
        }
        thunkIl.BeginExceptionBlock();
        thunkIl.Emit(OpCodes.Ldarg_0);
        thunkIl.Emit(OpCodes.Ldloc, array);
        thunkIl.Emit(OpCodes.Callvirt, typeof(Func<object[], object>).GetMethod("Invoke"));
        thunkIl.Emit(OpCodes.Stloc, result);
        thunkIl.BeginFinallyBlock();
        thunkIl.EndExceptionBlock();
        thunkIl.Emit(OpCodes.Ldloc, result);
        thunkIl.Emit(OpCodes.Unbox_Any, typeof(int));
        thunkIl.Emit(OpCodes.Ret);

        Func<int, int, int, int> bound = (Func<int, int, int, int>) thunk.CreateDelegate(typeof(Func<int, int, int, int>), handler);
        if (bound(5, 8, 2) != 42)
        {
            return 17;
        }

        return 0;
    }
}
