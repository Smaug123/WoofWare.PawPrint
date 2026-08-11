using System;

// A type-initialisation failure raised on entry to a method is not catchable by that method.
//
// The CLR emits the check into the prologue, outside the method's own exception-handling
// regions, so the `TypeInitializationException` goes to the *caller* even when the method's try
// region starts at the first instruction and catches exactly that type. Measured on .NET 10:
// `Main` catches it and `M`'s handler never runs.
//
// PawPrint runs the check with the callee's frame established, which is what lets the trace name
// the method — so the frame is on the stack when the exception is raised, and its clauses have
// to be excluded deliberately rather than by accident of the frame not existing yet. A frame
// whose prologue has not completed has not started, so it has no active handlers.
//
// What this file actually covers is the end-to-end path: priming a failure, then calling a method
// on the failed type, and the exception arriving at the caller. It does *not* discriminate the
// handler-scope rule, and cannot — the test harness compiles unoptimized, so `M` begins with a
// `nop` at IL offset 0 that sits outside its try region, and the offset the prologue raises from
// is therefore uncovered whatever the rule is. `TestPrologueExceptionScope` is where the rule is
// asserted, against a method whose try does cover offset 0, as optimized IL's would.
//
// The failure is primed first, so the call below meets a type already marked failed rather than
// running the `.cctor` afresh; that is the path that raises directly at the callee's first
// instruction.
class CctorFailureNotCaughtByCallee
{
    class Bad
    {
        static Bad()
        {
            throw new InvalidOperationException("boom");
        }

        public static int X = 1;

        public static int M()
        {
            try
            {
                return 2;
            }
            catch (TypeInitializationException)
            {
                return 3;
            }
        }
    }

    static int Main(string[] args)
    {
        bool primed = false;

        try
        {
            int unused = Bad.X;
        }
        catch (TypeInitializationException)
        {
            primed = true;
        }

        if (!primed)
        {
            return 1;
        }

        try
        {
            // Returns 2 if the body ran, 3 if this method caught its own prologue's failure.
            // Both mean the exception was handled somewhere it should not have been.
            return M_result();
        }
        catch (TypeInitializationException)
        {
            return 0;
        }
    }

    static int M_result() => Bad.M();
}
