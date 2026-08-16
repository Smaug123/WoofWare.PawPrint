using System;
using System.Runtime.CompilerServices;

public class Program
{
    // The dynamic-code default is a *baseline*, laid down beneath whatever the guest's
    // `runtimeconfig.json` says rather than replacing it. A guest that explicitly declares
    // the switch true therefore observes true.
    //
    // Forcing the value instead would break hostpolicy fidelity and remove the only way to
    // ask PawPrint to exercise a dynamic-code path, without even buying immutability:
    // `AppContext.SetSwitch` remains available to the guest at any moment.
    //
    // The test harness registers this case with the switch set to "true", standing in for a
    // `runtimeconfig.json` that says so.
    public static int Main(string[] args)
    {
        if (!RuntimeFeature.IsDynamicCodeSupported)
        {
            return 1;
        }

        if (!RuntimeFeature.IsDynamicCodeCompiled)
        {
            return 2;
        }

        // Overriding the switch does not conjure an implementation: PawPrint still cannot
        // emit. The guest gets as far as the missing runtime primitive rather than being
        // turned away at the door. Nothing here asserts on that, because it is the emit gap's
        // business and is covered by the parked `sourcesPure/DynamicMethod*.cs` cases.
        return 0;
    }
}
