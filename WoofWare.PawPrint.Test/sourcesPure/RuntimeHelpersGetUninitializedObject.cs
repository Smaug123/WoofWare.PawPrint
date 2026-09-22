using System;
using System.Collections.Generic;
using System.Reflection;
using System.Runtime.CompilerServices;

// `RuntimeHelpers.GetUninitializedObject`, whose primitive is the QCall
// `ReflectionSerialization_GetCreateUninitializedObjectInfo`. The managed wrapper hands any
// `RuntimeType` straight to the QCall, so every check in CoreCLR's
// `ValidateTypeAbleToBeInstantiated` is reachable from here.
//
// Every rejection is checked against the exception's *exact* type. The plausible wrong answers
// are related types: `MissingMethodException` is a `MemberAccessException`, and it is what the
// sibling `RuntimeTypeHandle_GetActivationInfo` throws for the same checks.
//
// Returns 0 on success, or the number of the first check that failed.
public static class Log
{
    public static string Trace = "";

    public static int Mark(string s)
    {
        Trace += s;
        return 0;
    }
}

public class PreciseBase
{
    static PreciseBase() { Log.Trace += "B"; }
}

public class PreciseDerived : PreciseBase
{
    static PreciseDerived() { Log.Trace += "D"; }
}

public class OtherPreciseBase
{
    static OtherPreciseBase() { Log.Trace += "O"; }
}

// No static constructor, so `beforefieldinit`: its own initialiser is not run, but its base's is.
// Nothing reads `Marker`, so real .NET has no occasion to run it either.
public class LaxOverPrecise : OtherPreciseBase
{
    public static int Marker = Log.Mark("L");
}

public struct PreciseStruct
{
    public int Field;

    static PreciseStruct() { Log.Trace += "S"; }
}

public class ThrowingInitialiser
{
    static ThrowingInitialiser() { throw new InvalidOperationException("boom"); }
}

public class WithConstructor
{
    public int Initialised = 5;
    public string Name;

    public WithConstructor(int x)
    {
        Initialised = x;
        Name = "constructed";
    }
}

public struct Pair
{
    public int A;
    public long B;
}

public ref struct Ref
{
    public int X;
}

public abstract class Abstract
{
}

public static class Static
{
}

public class Generic<T>
{
}

public class Constrained<T> where T : IComparable<T>
{
}

public class ConstrainedToClass<T> where T : Generic<T>
{
}

public unsafe class Program
{
    private const int Ok = 0;
    private const int Argument = 1;
    private const int MemberAccess = 2;
    private const int NotSupported = 3;
    private const int Other = 4;

    private static int Classify(Type t)
    {
        try
        {
            RuntimeHelpers.GetUninitializedObject(t);
            return Ok;
        }
        catch (Exception e)
        {
            if (e.GetType() == typeof(ArgumentException)) return Argument;
            if (e.GetType() == typeof(MemberAccessException)) return MemberAccess;
            if (e.GetType() == typeof(NotSupportedException)) return NotSupported;
            return Other;
        }
    }

    private static void MethodGeneric<U>()
    {
    }

    public static int Main(string[] args)
    {
        // `void`, and every TypeDesc or array. A generic parameter is a TypeDesc in CoreCLR, so
        // it lands here rather than at the generic-variables check.
        if (Classify(typeof(void)) != Argument) return 1;
        if (Classify(typeof(int[])) != Argument) return 2;
        if (Classify(typeof(int[,])) != Argument) return 3;
        if (Classify(typeof(int*)) != Argument) return 4;
        if (Classify(typeof(int).MakeByRefType()) != Argument) return 5;
        if (Classify(typeof(delegate*<void>)) != Argument) return 6;
        if (Classify(typeof(List<>).GetGenericArguments()[0]) != Argument) return 7;

        Type methodGenericParameter =
            typeof(Program)
                .GetMethod("MethodGeneric", BindingFlags.NonPublic | BindingFlags.Static)
                .GetGenericArguments()[0];

        if (Classify(methodGenericParameter) != Argument) return 8;
        if (Classify(typeof(List<>).GetGenericArguments()[0].MakeArrayType()) != Argument) return 9;

        // Delegates, closed or open: the delegate check precedes the generic-variables one.
        if (Classify(typeof(Action)) != Argument) return 10;
        if (Classify(typeof(Func<int>)) != Argument) return 11;
        if (Classify(typeof(Func<>)) != Argument) return 12;

        // String, the one variable-length type that is not an array.
        if (Classify(typeof(string)) != Argument) return 13;

        // Abstract types, including interfaces, static classes, and the delegate base classes,
        // whose immediate base is not MulticastDelegate.
        if (Classify(typeof(IDisposable)) != MemberAccess) return 14;
        if (Classify(typeof(Abstract)) != MemberAccess) return 15;
        if (Classify(typeof(Static)) != MemberAccess) return 16;
        if (Classify(typeof(Delegate)) != MemberAccess) return 17;
        if (Classify(typeof(MulticastDelegate)) != MemberAccess) return 18;
        if (Classify(typeof(IList<>)) != MemberAccess) return 19;
        if (Classify(typeof(Enum)) != MemberAccess) return 20;

        // Open types with a MethodTable: definitions, and an open construction. `Span<>` is
        // byref-like, but the generic-variables check comes first.
        if (Classify(typeof(Generic<>)) != MemberAccess) return 21;
        if (Classify(typeof(Nullable<>)) != MemberAccess) return 22;
        if (Classify(typeof(Span<>)) != MemberAccess) return 23;
        Type openInterface = typeof(Constrained<>).GetGenericArguments()[0].GetGenericParameterConstraints()[0];
        if (Classify(openInterface) != MemberAccess) return 24;

        Type openClass = typeof(ConstrainedToClass<>).GetGenericArguments()[0].GetGenericParameterConstraints()[0];
        if (Classify(openClass) != MemberAccess) return 42;

        // Byref-like types.
        if (Classify(typeof(Span<int>)) != NotSupported) return 25;
        if (Classify(typeof(Ref)) != NotSupported) return 26;
        if (Classify(typeof(TypedReference)) != NotSupported) return 27;

        // Accepted types. A primitive is a boxed default.
        object i = RuntimeHelpers.GetUninitializedObject(typeof(int));
        if (!(i is int iValue) || iValue != 0) return 28;

        // Nullable<T> allocates a boxed T.
        object n = RuntimeHelpers.GetUninitializedObject(typeof(int?));
        if (n == null || n.GetType() != typeof(int) || (int)n != 0) return 29;

        object pair = RuntimeHelpers.GetUninitializedObject(typeof(Pair));
        if (!(pair is Pair p) || p.A != 0 || p.B != 0) return 30;

        object day = RuntimeHelpers.GetUninitializedObject(typeof(DayOfWeek));
        if (!(day is DayOfWeek d) || d != DayOfWeek.Sunday) return 31;

        if (RuntimeHelpers.GetUninitializedObject(typeof(object)).GetType() != typeof(object)) return 32;

        // A reference type with no parameterless constructor is allocated, and neither its
        // constructor nor its field initialisers run.
        object w = RuntimeHelpers.GetUninitializedObject(typeof(WithConstructor));
        if (!(w is WithConstructor withCtor) || withCtor.Initialised != 0 || withCtor.Name != null) return 33;

        // Two allocations are two objects.
        if (ReferenceEquals(
                RuntimeHelpers.GetUninitializedObject(typeof(WithConstructor)),
                RuntimeHelpers.GetUninitializedObject(typeof(WithConstructor))))
            return 34;

        // Class initialisers: a type that is not `beforefieldinit` has its own run, and then its
        // ancestors', most-derived first.
        if (Log.Trace != "") return 35;
        RuntimeHelpers.GetUninitializedObject(typeof(PreciseDerived));
        if (Log.Trace != "DB") return 36;

        // A `beforefieldinit` type with a precise base runs the base's, and not its own.
        RuntimeHelpers.GetUninitializedObject(typeof(LaxOverPrecise));
        if (Log.Trace != "DBO") return 37;

        // Value types too, and a Nullable<T> runs T's.
        RuntimeHelpers.GetUninitializedObject(typeof(PreciseStruct?));
        if (Log.Trace != "DBOS") return 38;

        // Each initialiser runs once.
        RuntimeHelpers.GetUninitializedObject(typeof(PreciseDerived));
        RuntimeHelpers.GetUninitializedObject(typeof(PreciseStruct));
        if (Log.Trace != "DBOS") return 39;

        // A throwing initialiser surfaces as TypeInitializationException, on every call.
        for (int attempt = 0; attempt < 2; attempt++)
        {
            try
            {
                RuntimeHelpers.GetUninitializedObject(typeof(ThrowingInitialiser));
                return 40;
            }
            catch (TypeInitializationException e)
            {
                if (!(e.InnerException is InvalidOperationException)) return 41;
            }
        }

        return 0;
    }
}
