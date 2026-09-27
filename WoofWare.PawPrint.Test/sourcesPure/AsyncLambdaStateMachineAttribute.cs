using System;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Threading.Tasks;

// Roslyn marks an async lambda's method `[AsyncStateMachine(typeof(<>c.<<Main>b__0_0>d))]`, and
// the state machine is nested two deep. Decoding that attribute resolves the `System.Type`
// argument by name, "Program+<>c+<<Main>b__0_0>d", which reaches `RuntimeAssembly.GetTypeCore`
// with nested names, and so the `LibraryImport` stub that marshals them into a `stackalloc`
// buffer it reads before writing. Nothing here runs the lambda: only its attribute is read.
class Program
{
    static int Main(string[] args)
    {
        Func<Task<int>> lambda = async () =>
        {
            await Task.Yield();
            return 1;
        };

        AsyncStateMachineAttribute attribute = lambda.Method.GetCustomAttribute<AsyncStateMachineAttribute>();
        if (attribute is null) return 1;

        Type stateMachine = attribute.StateMachineType;
        if (stateMachine is null) return 2;
        if (stateMachine.FullName != "Program+<>c+<<Main>b__0_0>d") return 3;
        if (stateMachine.DeclaringType is null || stateMachine.DeclaringType.DeclaringType != typeof(Program)) return 4;
        if (!typeof(IAsyncStateMachine).IsAssignableFrom(stateMachine)) return 5;

        return 0;
    }
}
