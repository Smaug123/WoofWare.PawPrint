using System;
using System.Linq.Expressions;
using System.Reflection;

// An expression tree calling a method with an `out` parameter, run by the LINQ expression
// *interpreter*. This is the route ASP.NET's route-parameter binding takes under PawPrint:
// `ByRefMethodInfoCallInstruction.Run` calls `MethodBase.Invoke(null, object[])` and then
// `ByRefUpdater.Update` reads each byref argument's new value back out of the array.
//
// `preferInterpretation: true` makes the host take the interpreter too. Without it the host would
// compile the tree to IL (dynamic code is supported there) and never reflection-invoke at all,
// while PawPrint, which seeds `IsDynamicCodeSupported` false, always interprets — and the two
// runs would then agree on the answer for entirely different reasons.
//
// Returns 0 on success, or the number of the first check that failed.
public static class Program
{
    public static int Main ()
    {
        MethodInfo tryParse =
            typeof (int).GetMethod ("TryParse", new[] { typeof (string), typeof (int).MakeByRefType () });

        if (tryParse == null)
            return 1;

        ParameterExpression input = Expression.Parameter (typeof (string), "s");
        ParameterExpression result = Expression.Variable (typeof (int), "r");

        // s => int.TryParse(s, out r) ? r : -1
        Expression<Func<string, int>> lambda =
            Expression.Lambda<Func<string, int>> (
                Expression.Block (
                    new[] { result },
                    Expression.Condition (
                        Expression.Call (tryParse, input, result),
                        result,
                        Expression.Constant (-1))),
                input);

        Func<string, int> parseOrMinusOne = lambda.Compile (preferInterpretation : true);

        // 2: the `out` value written by the callee reaches the interpreter's variable. This is the
        // first invocation of `tryParse`, and so the one that exercises the QCall on both runtimes.
        if (parseOrMinusOne ("42") != 42)
            return 2;

        // 3: a failed parse leaves the variable at the default the interpreter filled the slot
        // with, and the condition takes the other branch.
        if (parseOrMinusOne ("nope") != -1)
            return 3;

        return 0;
    }
}
