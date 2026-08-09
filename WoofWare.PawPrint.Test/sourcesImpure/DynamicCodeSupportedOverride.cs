using System;
using System.Runtime.CompilerServices;

public class Program
{
    // The dynamic-code default is a *baseline*, laid down beneath whatever the guest's
    // `runtimeconfig.json` says rather than replacing it. A guest that explicitly declares
    // the switch true therefore observes true.
    //
    // That precedence is deliberate. Forcing the value would make `AppContextSeed` stop
    // being a faithful reproduction of hostpolicy — the guest would read back something its
    // own configuration did not say — and it would not even buy immutability, since
    // `AppContext.SetSwitch` remains available to the guest at any moment. What it would buy
    // is the loss of the only way to ask PawPrint to exercise a dynamic-code path once one
    // exists.
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
        // turned away at the door, which is exactly the state of affairs before this default
        // existed. Nothing here asserts on that, because it is the emit gap's business and
        // is covered by the parked `sourcesPure/DynamicMethod*.cs` cases.
        return 0;
    }
}
