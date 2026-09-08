using System;
using System.Reflection;

public class GenericBase<T>
{
    public virtual T Echo(T value) => value;
}

public class OpenDerived<T> : GenericBase<T>
{
    public override T Echo(T value) => value;
}

public static class Program
{
    // `GetBaseDefinition` on a method of an open definition whose base definition is declared by a
    // *generic* base: `OpenDerived<T>.Echo` overrides `GenericBase<T>.Echo`, and the answer's
    // declaring type is the open construction `GenericBase<T of OpenDerived>`.
    //
    // Measured: PawPrint stops before `RuntimeTypeHandle_GetMethodAt` is reached at all, in
    // `resolveBaseRuntimeTypeHandleTarget`, because `GetBaseDefinition` first asks the definition
    // for its `BaseType` and that base is an open construction. `GetMethodAt` refuses the same
    // shape one step later, since `MethodHandleRegistry` cannot mint a handle whose declaring type
    // is an open construction; see docs/plans/2026-09-08-get-method-at.md, decision 2.
    //
    // Exit code is the index of the first failing check, so a failure names itself.
    public static int Main()
    {
        MethodInfo echo = typeof(OpenDerived<>).GetMethod("Echo");
        MethodInfo baseEcho = echo.GetBaseDefinition();
        if (baseEcho.DeclaringType.GetGenericTypeDefinition() != typeof(GenericBase<>)) return 1;
        if (baseEcho.DeclaringType.IsGenericTypeDefinition) return 2;
        if (!baseEcho.DeclaringType.ContainsGenericParameters) return 3;
        if (baseEcho.DeclaringType.GetGenericArguments()[0] != typeof(OpenDerived<>).GetGenericArguments()[0]) return 4;
        return 0;
    }
}
