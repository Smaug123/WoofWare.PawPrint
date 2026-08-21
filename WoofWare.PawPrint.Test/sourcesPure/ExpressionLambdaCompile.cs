using System;
using System.Linq.Expressions;

// `Expression.Lambda<T>(...).Compile()`, which is what `Delegate_BindToMethodInfo` over a metadata
// method unblocks.
//
// The interpreter is what runs the lambda: `LightLambda.MakeDelegate` asks
// `DelegateHelpers.CreateObjectArrayDelegate`, whose `GetCSharpThunk` finds a pre-baked static
// generic thunk (`DelegateHelpers.FuncThunk1<T1, TReturn>`) in a table and instantiates it, so the
// `ILGenerator` path is never entered. That thunk is then bound as a *closed* delegate over the
// interpreter's `Func<object[], object>` — a static metadata method one argument wider than the
// delegate's `Invoke` — which is the binding this file exercises end to end.
//
// Returns 0 on success, or the number of the first check that failed.

public static class Program
{
    public static int Main ()
    {
        ParameterExpression x = Expression.Parameter (typeof (int), "x");

        Func<int, int> addThree =
            Expression.Lambda<Func<int, int>> (Expression.Add (x, Expression.Constant (3)), x).Compile ();

        if (addThree (4) != 7)
        {
            return 1;
        }

        // 2: the compiled delegate is reusable, so nothing about the binding was consumed by the
        // first call.
        if (addThree (-10) != -7)
        {
            return 2;
        }

        // 3: a second arity, so the thunk table is indexed rather than one entry being lucky.
        ParameterExpression y = Expression.Parameter (typeof (int), "y");

        Func<int, int, int> multiply =
            Expression.Lambda<Func<int, int, int>> (Expression.Multiply (x, y), x, y).Compile ();

        if (multiply (6, 7) != 42)
        {
            return 3;
        }

        // 4: a reference-typed parameter and return, so the thunk's boxing is not only over
        // primitives.
        ParameterExpression s = Expression.Parameter (typeof (string), "s");

        Func<string, string> upper = Expression.Lambda<Func<string, string>> (
            Expression.Call (s, typeof (string).GetMethod ("ToUpperInvariant", Type.EmptyTypes)),
            s
        )
            .Compile ();

        if (upper ("ab") != "AB")
        {
            return 4;
        }

        // 5: a zero-argument lambda, which takes the thunk table's first entry.
        Func<int> constant = Expression.Lambda<Func<int>> (Expression.Constant (11)).Compile ();

        if (constant () != 11)
        {
            return 5;
        }

        return 0;
    }
}
