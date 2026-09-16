using System;
using System.Collections.Generic;
using System.Reflection;

namespace TypeofIsValueTypeFold
{
    enum Colour { Red, Green }

    struct Point { public int X; public int Y; }

    struct Wrapper<T> { public T Value; }

    ref struct RefOnly { public int A; }

    class Node { }

    interface IShape { }

    delegate int Op(int x);

    static class Probe<T>
    {
        // `ldtoken !T; call Type::GetTypeFromHandle; call Type::get_IsValueType` inside a generic
        // type: the shape CoreCLR's importer folds to a constant, and the one that dominates
        // `Dictionary<TKey, TValue>` and `HashSet<T>` under PawPrint when not folded.
        public static bool ViaClass() => typeof(T).IsValueType;
    }

    class Program
    {
        static bool ViaMethod<T>() => typeof(T).IsValueType;

        // The receiver arrives through a local rather than straight from `GetTypeFromHandle`, so
        // this is not the folded shape and answers through the managed getter and its callees.
        static bool ViaLocal<T>()
        {
            Type t = typeof(T);
            return t.IsValueType;
        }

        static int failures = 0;

        static void Check(bool actual, bool expected)
        {
            if (actual != expected)
            {
                failures++;
            }
        }

        static unsafe int Main()
        {
            // Literal `typeof`, one per handle shape the fold can meet.
            Check(typeof(int).IsValueType, true);
            Check(typeof(bool).IsValueType, true);
            Check(typeof(decimal).IsValueType, true);
            Check(typeof(IntPtr).IsValueType, true);
            Check(typeof(DateTime).IsValueType, true);
            Check(typeof(Colour).IsValueType, true);
            Check(typeof(Point).IsValueType, true);
            Check(typeof(Wrapper<int>).IsValueType, true);
            Check(typeof(Wrapper<string>).IsValueType, true);
            Check(typeof(int?).IsValueType, true);
            Check(typeof(KeyValuePair<int, string>).IsValueType, true);
            Check(typeof(RefOnly).IsValueType, true);
            Check(typeof(Span<int>).IsValueType, true);
            Check(typeof(void).IsValueType, true);
            Check(typeof(string).IsValueType, false);
            Check(typeof(object).IsValueType, false);
            Check(typeof(Node).IsValueType, false);
            Check(typeof(IShape).IsValueType, false);
            Check(typeof(Op).IsValueType, false);
            Check(typeof(List<int>).IsValueType, false);
            Check(typeof(int[]).IsValueType, false);
            Check(typeof(Point[]).IsValueType, false);
            Check(typeof(int[,]).IsValueType, false);
            Check(typeof(int*).IsValueType, false);
            Check(typeof(delegate*<int, int>).IsValueType, false);

            // `System.ValueType` and `System.Enum` are the two classes whose *descendants* are
            // value types; they are not value types themselves.
            Check(typeof(ValueType).IsValueType, false);
            Check(typeof(Enum).IsValueType, false);

            // Open generic definitions: the token names no closed type, so the fold does not
            // apply and the getter runs.
            Check(typeof(List<>).IsValueType, false);
            Check(typeof(KeyValuePair<,>).IsValueType, true);

            // `typeof(T)` under a type-generic context.
            Check(Probe<int>.ViaClass(), true);
            Check(Probe<Point>.ViaClass(), true);
            Check(Probe<Colour>.ViaClass(), true);
            Check(Probe<int?>.ViaClass(), true);
            Check(Probe<Wrapper<Node>>.ViaClass(), true);
            Check(Probe<string>.ViaClass(), false);
            Check(Probe<Node>.ViaClass(), false);
            Check(Probe<IShape>.ViaClass(), false);
            Check(Probe<int[]>.ViaClass(), false);
            Check(Probe<Op>.ViaClass(), false);

            // `typeof(T)` under a method-generic context.
            Check(ViaMethod<int>(), true);
            Check(ViaMethod<Point>(), true);
            Check(ViaMethod<Colour>(), true);
            Check(ViaMethod<int?>(), true);
            Check(ViaMethod<Wrapper<Node>>(), true);
            Check(ViaMethod<string>(), false);
            Check(ViaMethod<Node>(), false);
            Check(ViaMethod<IShape>(), false);
            Check(ViaMethod<int[]>(), false);
            Check(ViaMethod<Op>(), false);

            // The same questions through a local, i.e. through the managed getter, must agree
            // with the folded answers above.
            Check(ViaLocal<int>(), true);
            Check(ViaLocal<Point>(), true);
            Check(ViaLocal<Colour>(), true);
            Check(ViaLocal<string>(), false);
            Check(ViaLocal<Node>(), false);
            Check(ViaLocal<int[]>(), false);

            // Neighbours with the same three-instruction shape but a different callee. Each pair
            // has a type whose `IsValueType` answer differs from the getter's own, so a fold that
            // recognised the shape without checking *which* getter it feeds would fail here.
            Check(typeof(DateTime).IsPrimitive, false);
            Check(typeof(int).IsPrimitive, true);
            Check(typeof(Colour).IsPrimitive, false);
            Check(typeof(Point).IsByRefLike, false);
            Check(typeof(Span<int>).IsByRefLike, true);
            Check(typeof(Colour).IsEnum, true);
            Check(typeof(Point).IsEnum, false);
            Check(typeof(Point).IsGenericType, false);
            Check(typeof(Wrapper<int>).IsGenericType, true);
            Check(typeof(int) == typeof(int), true);
            Check(typeof(int) == typeof(long), false);

            // A `Type` subclass reached through a `Type`-typed receiver still answers from its
            // own override: the getter body's `callvirt IsValueTypeImpl` is what selects it.
            Type delegated = new TypeDelegator(typeof(Point));
            Check(delegated.IsValueType, true);
            Type delegatedRef = new TypeDelegator(typeof(Node));
            Check(delegatedRef.IsValueType, false);

            return failures;
        }
    }
}
