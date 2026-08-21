using System;
using System.Collections.Generic;
using System.Reflection;

public class SignatureDefinition<T, U>
{
    public int TakesFormal(T value) => 1;

    public int TakesClosed(int value) => 2;

    public int TakesConstructed(List<T> value) => 3;

    public int TakesSecondFormal(U value) => 4;

    public int TakesNested(List<List<T>> value) => 5;

    public int TakesSwapped(Dictionary<U, int> value) => 6;

    public int TakesSelf(SignatureDefinition<T, U> other) => 7;

    public T ReturnsFormal() => default!;
}

public static class Program
{
    // Decoding a MethodSig whose declaring type is an open generic *definition* rather than an
    // instantiation, which is what `Signature_Init` does when CoreCLR's SigTypeContext carries the
    // typical instantiation -- the definition's own type variables. There is nothing to substitute,
    // so each `!i` has to come back as the RuntimeType for that variable.
    //
    // Check 2 is the shape `System.Dynamic.Utils.TypeUtils`' static initialiser runs, so it is the
    // first thing any use of `System.Linq.Expressions` reaches:
    // `typeof(Nullable<>).GetConstructor(typeof(Nullable<>).GetGenericArguments())`. Its sibling
    // sourcesPure/ReflectionOpenGenericNullableConstructor.cs deliberately asks `GetConstructors()`
    // instead, because filtering candidates by parameter type is exactly this decoding.
    //
    // Reference equality, not `==`, is what most of these check. A walk that minted a second,
    // equal-but-distinct `Type` for `T` would satisfy an equality check while breaking the identity
    // .NET guarantees -- and `GetConstructor(Type[])` above finds nothing at all unless the
    // parameter type is the very object `GetGenericArguments` handed back.
    //
    // Shapes deliberately absent: a parameter typed `T[]`, `ref T` or `T*`. Real .NET reflects all
    // three, with the element type reference-equal to `T`, and PawPrint's `RuntimeTypeHandleTarget`
    // has no case for an array, byref or pointer over a variable, so they are refused rather than
    // answered. sourcesPure/ReflectionOpenGenericDefinitionElementTypes.cs parks them.
    //
    // Exit code is the index of the first failing check, so a failure names itself.
    public static int Main()
    {
        Type[] nullableArgs = typeof(Nullable<>).GetGenericArguments();
        if (nullableArgs.Length != 1) return 1;

        ConstructorInfo nullableCtor = typeof(Nullable<>).GetConstructor(nullableArgs);
        if (nullableCtor == null) return 2;

        ParameterInfo[] nullableParams = nullableCtor.GetParameters();
        if (nullableParams.Length != 1) return 3;
        if (!ReferenceEquals(nullableParams[0].ParameterType, nullableArgs[0])) return 4;

        Type definition = typeof(SignatureDefinition<,>);
        Type[] formals = definition.GetGenericArguments();
        if (formals.Length != 2) return 5;

        Type takesFormal = definition.GetMethod("TakesFormal").GetParameters()[0].ParameterType;
        if (!ReferenceEquals(takesFormal, formals[0])) return 6;
        if (!takesFormal.IsGenericParameter) return 7;
        if (takesFormal.GenericParameterPosition != 0) return 8;
        if (takesFormal.DeclaringType != definition) return 9;

        // The closed control: an ordinary type in the same signature must still be that type.
        if (definition.GetMethod("TakesClosed").GetParameters()[0].ParameterType != typeof(int)) return 10;

        // Position 1, so an implementation that always answers with the zeroth variable fails.
        Type takesSecond = definition.GetMethod("TakesSecondFormal").GetParameters()[0].ParameterType;
        if (!ReferenceEquals(takesSecond, formals[1])) return 11;
        if (takesSecond.GenericParameterPosition != 1) return 12;

        // A variable *inside* a constructed type: an open constructed type, not a definition and
        // not a variable, whose own argument is the definition's variable.
        Type takesConstructed = definition.GetMethod("TakesConstructed").GetParameters()[0].ParameterType;
        if (takesConstructed.IsGenericParameter) return 13;
        if (!takesConstructed.ContainsGenericParameters) return 14;
        if (takesConstructed.IsGenericTypeDefinition) return 15;
        if (takesConstructed.GetGenericTypeDefinition() != typeof(List<>)) return 16;
        if (!ReferenceEquals(takesConstructed.GetGenericArguments()[0], formals[0])) return 17;

        // Two levels deep, so a walk that resolves only an instantiation's *top-level* arguments
        // through the type-variable environment fails here while passing check 17.
        Type takesNested = definition.GetMethod("TakesNested").GetParameters()[0].ParameterType;
        if (takesNested.GetGenericTypeDefinition() != typeof(List<>)) return 18;
        Type nestedInner = takesNested.GetGenericArguments()[0];
        if (nestedInner.GetGenericTypeDefinition() != typeof(List<>)) return 19;
        if (!ReferenceEquals(nestedInner.GetGenericArguments()[0], formals[0])) return 20;

        // A closed argument beside a variable, with the variable *first* and at the definition's
        // second position, so a walk that mapped an instantiation's arguments positionally onto the
        // declaring definition's own formals answers `T` here and fails.
        Type takesSwapped = definition.GetMethod("TakesSwapped").GetParameters()[0].ParameterType;
        if (takesSwapped.GetGenericTypeDefinition() != typeof(Dictionary<,>)) return 21;
        Type[] swappedArgs = takesSwapped.GetGenericArguments();
        if (!ReferenceEquals(swappedArgs[0], formals[1])) return 22;
        if (swappedArgs[1] != typeof(int)) return 23;

        // The definition applied to its own formals in order is the typical instantiation, which
        // CoreCLR represents as the definition itself rather than as an instantiation of it.
        Type takesSelf = definition.GetMethod("TakesSelf").GetParameters()[0].ParameterType;
        if (!ReferenceEquals(takesSelf, definition)) return 24;
        if (!takesSelf.IsGenericTypeDefinition) return 25;

        // The return type takes the same walk as the parameters.
        if (!ReferenceEquals(definition.GetMethod("ReturnsFormal").ReturnType, formals[0])) return 26;

        // Candidate filtering by parameter type on the user definition, not only through the
        // Nullable constructor above: `GetMethod(name, Type[])` decodes every candidate's signature.
        if (definition.GetMethod("TakesFormal", new[] { formals[0] }) == null) return 27;
        if (definition.GetMethod("TakesSelf", new[] { definition }) == null) return 28;

        // The instantiation is unaffected: substituting really substitutes, and the two answers are
        // different objects, so check 6 cannot be passing because everything answers `int`.
        Type instantiation = typeof(SignatureDefinition<int, string>);
        if (instantiation.GetMethod("TakesFormal").GetParameters()[0].ParameterType != typeof(int)) return 29;
        if (instantiation.GetMethod("ReturnsFormal").ReturnType != typeof(int)) return 30;
        if (ReferenceEquals(instantiation.GetMethod("TakesFormal").GetParameters()[0].ParameterType, formals[0])) return 31;

        // A constructed parameter under a closed declaring type: every argument resolves to a
        // runtime type, which is the shape that must keep taking concretization rather than the
        // open-construction path.
        Type closedConstructed = instantiation.GetMethod("TakesConstructed").GetParameters()[0].ParameterType;
        if (closedConstructed != typeof(List<int>)) return 32;
        if (closedConstructed.ContainsGenericParameters) return 33;

        return 0;
    }
}
