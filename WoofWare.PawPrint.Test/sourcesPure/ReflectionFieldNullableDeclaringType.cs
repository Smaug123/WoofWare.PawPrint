using System;
using System.Reflection;

// Reading and writing the fields of Nullable<T> itself (`hasValue`, `value`) reflectively, by two
// routes which real .NET refuses with different exceptions.
//
// FieldInfo.GetValue and SetValue: FieldAccessor's constructor routes a Nullable<T> declaring type
// to FieldAccessorType.NoInvoke, whose GetValue and SetValue arms throw FieldAccessException before
// looking at the target or reaching any QCall.
//
// The internal RuntimeFieldHandle.GetValue and SetValue, reached by private reflection, call the
// RuntimeFieldHandle_GetValue/_SetValue QCalls directly. Those refuse a Nullable<T> declaring type
// with NotSupportedException (InvokeUtil::GetFieldValue and SetValidField, invokeutil.cpp) before
// running the class initialiser or touching the target. A boxed `int?` is a boxed `int`, so a
// runtime that went on to the field store would be looking up Nullable<int>'s field on an `int`
// box.
//
// Each case records which exception escaped, so a runtime that throws the other route's exception
// (or nothing, or a different refusal) fails at a distinct index.

struct Pair
{
    public int A;
    public long B;
}

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

    const BindingFlags Instance = BindingFlags.NonPublic | BindingFlags.Instance;

    static bool SetThrowsFieldAccess(FieldInfo field, object target, object value)
    {
        try
        {
            field.SetValue(target, value);
            return false;
        }
        catch (FieldAccessException)
        {
            return true;
        }
        catch (Exception)
        {
            return false;
        }
    }

    static bool GetThrowsFieldAccess(FieldInfo field, object target)
    {
        try
        {
            field.GetValue(target);
            return false;
        }
        catch (FieldAccessException)
        {
            return true;
        }
        catch (Exception)
        {
            return false;
        }
    }

    // The internal static wrapper (first parameter RtFieldInfo) rather than the private
    // LibraryImport it calls (first parameter IntPtr); the two SetValue overloads have the same
    // arity, so arity alone does not tell them apart.
    static MethodInfo InternalWrapper(string name, int arity)
    {
        MethodInfo found = null;
        foreach (MethodInfo m in typeof(RuntimeFieldHandle).GetMethods(BindingFlags.NonPublic | BindingFlags.Static))
        {
            ParameterInfo[] ps = m.GetParameters();
            if (m.Name == name && ps.Length == arity && ps[0].ParameterType != typeof(IntPtr))
            {
                found = m;
            }
        }

        return found;
    }

    static bool InvokeThrowsNotSupported(MethodInfo method, object[] args)
    {
        try
        {
            method.Invoke(null, args);
            return false;
        }
        catch (TargetInvocationException e)
        {
            return e.InnerException != null && e.InnerException.GetType() == typeof(NotSupportedException);
        }
        catch (Exception)
        {
            return false;
        }
    }

    static int Main()
    {
        FieldInfo intValue = typeof(int?).GetField("value", Instance);
        FieldInfo intHasValue = typeof(int?).GetField("hasValue", Instance);
        Check(intValue != null);
        Check(intHasValue != null);

        object boxed = (int?)3;

        // The shape the issue names: a boxed `int?` is a boxed `int`.
        Check(SetThrowsFieldAccess(intValue, boxed, 5));
        Check(SetThrowsFieldAccess(intHasValue, boxed, false));
        Check(GetThrowsFieldAccess(intValue, boxed));
        Check(GetThrowsFieldAccess(intHasValue, boxed));

        // The refusal precedes target verification: a null target would otherwise be a
        // TargetException, and a target of the wrong type an ArgumentException.
        Check(SetThrowsFieldAccess(intValue, null, 5));
        Check(GetThrowsFieldAccess(intValue, null));
        Check(SetThrowsFieldAccess(intValue, "not an int", 5));

        // The refused writes did not land.
        Check((int)boxed == 3);

        // The same for a Nullable over a user-defined struct.
        FieldInfo pairValue = typeof(Pair?).GetField("value", Instance);
        Check(pairValue != null);
        object boxedPair = (Pair?)new Pair { A = 1, B = 2 };
        Check(SetThrowsFieldAccess(pairValue, boxedPair, new Pair { A = 7, B = 8 }));
        Check(GetThrowsFieldAccess(pairValue, boxedPair));
        Check(((Pair)boxedPair).A == 1 && ((Pair)boxedPair).B == 2);

        // The QCalls themselves, through the internal wrappers FieldAccessor would call:
        // SetValue(RtFieldInfo, object? obj, object? value, RuntimeType fieldType,
        //          RuntimeType? declaringType, ref bool isClassInitialized)
        // GetValue(RtFieldInfo, object? instance, RuntimeType fieldType,
        //          RuntimeType? declaringType, ref bool isClassInitialized)
        MethodInfo setValue = InternalWrapper("SetValue", 6);
        MethodInfo getValue = InternalWrapper("GetValue", 5);
        Check(setValue != null);
        Check(getValue != null);

        Check(InvokeThrowsNotSupported(setValue, new object[] { intValue, boxed, 5, typeof(int), typeof(int?), false }));
        Check(InvokeThrowsNotSupported(setValue, new object[] { intHasValue, boxed, false, typeof(bool), typeof(int?), false }));
        // Refused whatever the caller says about the class initialiser.
        Check(InvokeThrowsNotSupported(setValue, new object[] { intValue, boxed, 5, typeof(int), typeof(int?), true }));
        Check(InvokeThrowsNotSupported(getValue, new object[] { intValue, boxed, typeof(int), typeof(int?), true }));
        Check(InvokeThrowsNotSupported(getValue, new object[] { intHasValue, boxed, typeof(bool), typeof(int?), false }));
        Check(InvokeThrowsNotSupported(setValue, new object[] { pairValue, boxedPair, new Pair { A = 7, B = 8 }, typeof(Pair), typeof(Pair?), false }));
        Check(InvokeThrowsNotSupported(getValue, new object[] { pairValue, boxedPair, typeof(Pair), typeof(Pair?), false }));

        Check((int)boxed == 3);
        Check(((Pair)boxedPair).A == 1 && ((Pair)boxedPair).B == 2);

        return firstFailure;
    }
}
