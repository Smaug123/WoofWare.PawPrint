using System;
using System.Linq.Expressions;

// `Expression.Lambda<T>(...).Compile()` for delegates of three and more parameters.
//
// PawPrint runs the expression interpreter, and `LightLambda.MakeDelegate` hands back a delegate of
// the caller's type through `DelegateHelpers.CreateObjectArrayDelegate`. That has prebuilt thunks
// only for delegates of at most two parameters (`ExpressionLambdaCompile.cs` covers those); for a
// wider one it emits a thunk as an anonymously hosted `DynamicMethod`, whatever the dynamic-code
// switch says. `RequestDelegateFactory` compiles one of these for every minimal-API endpoint that
// binds a request body.
//
// Returns 0 on success, or the number of the first check that failed.

public static class Program
{
    public static int Main ()
    {
        ParameterExpression a = Expression.Parameter (typeof (int), "a");
        ParameterExpression b = Expression.Parameter (typeof (int), "b");
        ParameterExpression c = Expression.Parameter (typeof (int), "c");

        Func<int, int, int, int> fused = Expression.Lambda<Func<int, int, int, int>> (
            Expression.Add (Expression.Multiply (a, b), c),
            a,
            b,
            c
        )
            .Compile ();

        if (fused (5, 8, 2) != 42)
        {
            return 1;
        }

        // 2: reusable, so nothing about the thunk was consumed by the first call.
        if (fused (1, 1, 1) != 2)
        {
            return 2;
        }

        // 3: a reference-typed parameter and a fourth parameter, so the thunk's boxing is not only
        // over one primitive type.
        ParameterExpression s = Expression.Parameter (typeof (string), "s");

        Func<string, int, int, int, string> describe = Expression.Lambda<Func<string, int, int, int, string>> (
            Expression.Call (
                typeof (string).GetMethod ("Concat", new[] { typeof (object), typeof (object) }),
                s,
                Expression.Convert (Expression.Add (Expression.Add (a, b), c), typeof (object))
            ),
            s,
            a,
            b,
            c
        )
            .Compile ();

        if (describe ("sum=", 1, 2, 3) != "sum=6")
        {
            return 3;
        }

        // 4: a void-returning delegate, which the thunk builds without a return value.
        int observed = 0;
        ParameterExpression sink = Expression.Parameter (typeof (int[]), "sink");

        Action<int[], int, int> store = Expression.Lambda<Action<int[], int, int>> (
            Expression.Assign (Expression.ArrayAccess (sink, a), b),
            sink,
            a,
            b
        )
            .Compile ();

        int[] cells = new int[2];
        store (cells, 1, 17);
        observed = cells[1];

        if (observed != 17)
        {
            return 4;
        }

        return 0;
    }
}
