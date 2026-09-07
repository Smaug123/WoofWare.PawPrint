using System;
using System.Collections.Generic;
using System.Reflection;

class Sample
{
    public void ByRef(ref int value, ref List<int> list) => value++;
}

static class Program
{
    static int Main()
    {
        // `Type.MakeByRefType` on a RuntimeType is the `RuntimeTypeHandle_MakeByRef` QCall. The
        // shapes here are the ones PawPrint can express -- a byref over a closed type; a byref over
        // a generic parameter or an open definition is refused by the interpreter and so cannot be
        // pinned from a guest.
        Type intRef = typeof(int).MakeByRefType();
        if (!intRef.IsByRef) return 1;
        if (!intRef.HasElementType) return 2;
        if (intRef.IsPointer || intRef.IsArray) return 3;
        if (intRef.GetElementType() != typeof(int)) return 4;
        if (intRef.Name != "Int32&") return 5;
        if (intRef.FullName != "System.Int32&") return 6;
        if (intRef.IsValueType) return 7;
        if (intRef.BaseType != null) return 8;
        if (intRef.IsGenericType || intRef.ContainsGenericParameters) return 9;

        // One RuntimeType per type handle: the byref reached through reflection on a `ref int`
        // parameter is the same object, and so is a second MakeByRefType call.
        ParameterInfo[] parameters = typeof(Sample).GetMethod("ByRef").GetParameters();
        if (!ReferenceEquals(intRef, parameters[0].ParameterType)) return 10;
        if (!ReferenceEquals(intRef, typeof(int).MakeByRefType())) return 11;
        if (intRef != parameters[0].ParameterType) return 12;

        Type stringRef = typeof(string).MakeByRefType();
        if (stringRef.FullName != "System.String&") return 13;
        if (stringRef.GetElementType() != typeof(string)) return 14;

        Type arrayRef = typeof(int[]).MakeByRefType();
        if (arrayRef.Name != "Int32[]&") return 15;
        if (arrayRef.GetElementType() != typeof(int[])) return 16;
        if (arrayRef.IsArray) return 17;

        Type listRef = typeof(List<int>).MakeByRefType();
        if (listRef.Name != "List`1&") return 18;
        if (listRef.GetElementType() != typeof(List<int>)) return 19;
        if (listRef.IsGenericType) return 20;
        if (!ReferenceEquals(listRef, parameters[1].ParameterType)) return 21;

        // `void&` is a legal type handle even though no signature can carry it.
        Type voidRef = typeof(void).MakeByRefType();
        if (voidRef.FullName != "System.Void&") return 22;
        if (voidRef.GetElementType() != typeof(void)) return 23;

        // The one input the type loader refuses: a byref of a byref. The message names the type
        // being wrapped and the assembly that declares its element.
        try
        {
            intRef.MakeByRefType();
            return 24;
        }
        catch (TypeLoadException e)
        {
            if (!e.Message.StartsWith("Could not create a ByRef of a ByRef. Type: 'System.Int32&'. Assembly: 'System.Private.CoreLib, Version=", StringComparison.Ordinal)) return 25;
        }

        return 0;
    }
}
