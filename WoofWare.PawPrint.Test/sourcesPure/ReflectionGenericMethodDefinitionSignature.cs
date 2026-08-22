using System;
using System.Collections.Generic;
using System.Reflection;

public static class Holder
{
    public static T Echo<T>(T value) => value;

    public static V Pick<U, V>(U first, V second) => second;

    public static int Plain(int value) => value;

    public static List<T> Constructed<T>(List<T> value) => value;

    public static Dictionary<int, U> Mixed<U>(Dictionary<int, U> value) => value;
}

public class Box<T>
{
    // The mixed shape: under `Box<int>` the first parameter resolves to a runtime type while the
    // second stays a method variable, so one signature needs both answers at once.
    public U Mix<U>(List<T> closesFully, U staysFormal) => staysFormal;

    public void OnlyTypeVariable<U>(List<T> value) { }

    public T NoMethodGenerics(T value) => value;
}

public static class Program
{
    // Decoding a MethodSig for a *generic method definition* -- a method that declares type
    // parameters with none bound to the handle in hand. CoreCLR resolves such a signature against
    // the typical instantiation, whose `!!i` are the method's own variables, so each has to come
    // back as the RuntimeType for that variable rather than being substituted away.
    //
    // This is independent of whether the *declaring type* is a definition or an instantiation:
    // those are two axes, and every combination of them appears below. The combination that has no
    // precedent in sourcesPure/ReflectionOpenGenericDefinitionParameterTypes.cs is a *closed*
    // declaring type with a generic method definition, where one element of a single signature
    // resolves fully closed (`List<T>` under `Box<int>`) while another stays open (`!!0`).
    //
    // Reference equality throughout, not `==`: a walk that minted a second, equal-but-distinct
    // `Type` for a method variable would satisfy an equality check while breaking the identity
    // .NET guarantees, and `GetMethod(name, Type[])` finds nothing unless the parameter type is
    // the very object `GetGenericArguments` handed back.
    //
    // Shapes deliberately absent, each for a gap of its own rather than for this one:
    //
    //  - a parameter typed `U[]`, `ref U` or `U*`. Those are an array, byref or pointer over a
    //    *variable*, which `RuntimeTypeHandleTarget` cannot name;
    //    sourcesPure/ReflectionOpenGenericDefinitionElementTypes.cs parks that for the type-formal
    //    side, and the method-formal side has the same gap for the same reason.
    //  - `Type.DeclaringMethod`, the most direct way to tell a method variable from a type
    //    variable. It reaches `RuntimeTypeHandle.GetDeclaringMethodForGenericParameter`, which
    //    needs an `IRuntimeMethodInfo` for the declaring method and is unimplemented. Reference
    //    equality against `GetGenericArguments()` distinguishes the two axes without it, and is
    //    the stronger statement anyway: checks 33/36/37 pin that the method variable is the
    //    method's own and is *not* the declaring type's, which is what a walk that confused the
    //    axes would get wrong.
    //
    // Exit code is the index of the first failing check, so a failure names itself.
    public static int Main()
    {
        // --- Axis 1: non-generic declaring type, generic method definition. ---
        // The rung-H shape, and the one that reproduces in the fewest frames.
        MethodInfo echo = typeof(Holder).GetMethod("Echo");
        if (!echo.IsGenericMethodDefinition) return 1;

        Type[] echoArgs = echo.GetGenericArguments();
        if (echoArgs.Length != 1) return 2;

        Type echoParam = echo.GetParameters()[0].ParameterType;
        if (!ReferenceEquals(echoParam, echoArgs[0])) return 3;
        if (!echoParam.IsGenericParameter) return 4;
        if (echoParam.GenericParameterPosition != 0) return 5;
        if (!echoParam.ContainsGenericParameters) return 6;
        if (!ReferenceEquals(echo.ReturnType, echoArgs[0])) return 7;

        // Position 1, so an implementation that always answers with the zeroth method variable
        // fails here while passing check 3.
        MethodInfo pick = typeof(Holder).GetMethod("Pick");
        Type[] pickArgs = pick.GetGenericArguments();
        if (pickArgs.Length != 2) return 8;
        if (!ReferenceEquals(pick.GetParameters()[0].ParameterType, pickArgs[0])) return 9;
        if (!ReferenceEquals(pick.GetParameters()[1].ParameterType, pickArgs[1])) return 10;
        if (pickArgs[1].GenericParameterPosition != 1) return 11;
        if (!ReferenceEquals(pick.ReturnType, pickArgs[1])) return 12;

        // The closed control on a non-generic declaring type: enumerating the type's methods
        // decodes this signature too, so it must keep working.
        if (typeof(Holder).GetMethod("Plain").GetParameters()[0].ParameterType != typeof(int)) return 13;

        // A method variable *inside* a constructed type: an open constructed type whose own
        // argument is the method's variable.
        MethodInfo constructed = typeof(Holder).GetMethod("Constructed");
        Type constructedParam = constructed.GetParameters()[0].ParameterType;
        if (constructedParam.IsGenericParameter) return 14;
        if (!constructedParam.ContainsGenericParameters) return 15;
        if (constructedParam.GetGenericTypeDefinition() != typeof(List<>)) return 16;
        if (!ReferenceEquals(constructedParam.GetGenericArguments()[0], constructed.GetGenericArguments()[0])) return 17;

        // A closed argument beside a method variable in one constructed type, so a walk that
        // resolves an instantiation's arguments all-or-nothing fails here.
        MethodInfo mixedArgs = typeof(Holder).GetMethod("Mixed");
        Type mixedParam = mixedArgs.GetParameters()[0].ParameterType;
        if (mixedParam.GetGenericTypeDefinition() != typeof(Dictionary<,>)) return 18;
        Type[] mixedInner = mixedParam.GetGenericArguments();
        if (mixedInner[0] != typeof(int)) return 19;
        if (!ReferenceEquals(mixedInner[1], mixedArgs.GetGenericArguments()[0])) return 20;

        // --- Axis 2: closed generic declaring type, generic method definition. ---
        // The combination with no precedent: one signature, two answers.
        Type closedBox = typeof(Box<int>);
        MethodInfo closedMix = closedBox.GetMethod("Mix");
        if (!closedMix.IsGenericMethodDefinition) return 21;

        ParameterInfo[] closedMixParams = closedMix.GetParameters();
        // `List<T>` with T bound to int: every argument resolves to a runtime type, so this is a
        // closed type and must NOT come back as an open construction.
        Type closesFully = closedMixParams[0].ParameterType;
        if (closesFully != typeof(List<int>)) return 22;
        if (closesFully.ContainsGenericParameters) return 23;
        if (closesFully.IsGenericTypeDefinition) return 24;
        // ... while the method variable in the same signature stays open.
        Type staysFormal = closedMixParams[1].ParameterType;
        if (!staysFormal.IsGenericParameter) return 25;
        if (staysFormal.GenericParameterPosition != 0) return 26;
        if (!ReferenceEquals(staysFormal, closedMix.GetGenericArguments()[0])) return 27;

        // A generic method whose signature mentions only the *type* axis: every element closes,
        // but the method axis is still a definition, so the context is built either way.
        if (closedBox.GetMethod("OnlyTypeVariable").GetParameters()[0].ParameterType != typeof(List<int>)) return 28;

        // A non-generic method on the same closed type: unchanged by any of this.
        if (closedBox.GetMethod("NoMethodGenerics").GetParameters()[0].ParameterType != typeof(int)) return 29;

        // --- Axis 3: open generic declaring type, generic method definition. Both axes formal. ---
        Type openBox = typeof(Box<>);
        Type openBoxFormal = openBox.GetGenericArguments()[0];
        MethodInfo openMix = openBox.GetMethod("Mix");

        Type openFirst = openMix.GetParameters()[0].ParameterType;
        if (openFirst.IsGenericParameter) return 30;
        if (!openFirst.ContainsGenericParameters) return 31;
        if (openFirst.GetGenericTypeDefinition() != typeof(List<>)) return 32;
        // The type variable, reached through the declaring definition rather than the method.
        if (!ReferenceEquals(openFirst.GetGenericArguments()[0], openBoxFormal)) return 33;
        if (!openBoxFormal.IsGenericParameter) return 34;

        Type openSecond = openMix.GetParameters()[1].ParameterType;
        if (!openSecond.IsGenericParameter) return 35;
        if (!ReferenceEquals(openSecond, openMix.GetGenericArguments()[0])) return 36;
        // Both axes are formal here, and they must not be confused: the method variable is not the
        // type variable, even though both are position 0 of their respective owners. This is what
        // separates the two axes in the absence of `Type.DeclaringMethod` -- see the note above.
        if (ReferenceEquals(openSecond, openBoxFormal)) return 37;
        if (openSecond.GenericParameterPosition != 0) return 38;

        // --- The identity rule, measured: a method variable belongs to the definition. ---
        // Real .NET hands back the *same* Type object whether the method is reached through the
        // generic definition or through an instantiation of it, and its DeclaringType is the
        // definition either way. An implementation that keyed the variable on the declaring
        // type as the handle names it would mint two objects here where .NET has one.
        if (!ReferenceEquals(openSecond, staysFormal)) return 39;
        if (openSecond.DeclaringType != openBox) return 40;
        if (staysFormal.DeclaringType != openBox) return 41;

        // --- Substitution really substitutes: binding the method's own arguments. ---
        MethodInfo bound = echo.MakeGenericMethod(typeof(string));
        if (bound.IsGenericMethodDefinition) return 42;
        if (bound.GetParameters()[0].ParameterType != typeof(string)) return 43;
        if (bound.ReturnType != typeof(string)) return 44;
        // So check 3 cannot be passing because everything answers with a variable.
        if (ReferenceEquals(bound.GetParameters()[0].ParameterType, echoArgs[0])) return 45;

        // Candidate filtering by parameter type: `GetMethod(name, Type[])` decodes every
        // candidate's signature, so this fails unless the parameter type is that very object.
        if (typeof(Holder).GetMethod("Plain", new[] { typeof(int) }) == null) return 46;

        return 0;
    }
}
