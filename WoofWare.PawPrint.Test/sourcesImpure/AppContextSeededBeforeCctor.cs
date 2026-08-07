using System;

// The ordering contract, which is the whole reason seeding sits where it does in
// `Program.prepare`.
//
// BCL feature switches are `static readonly` fields latched on first read
// (`EventSource.IsSupported` is the motivating one), so it is not enough for AppContext to
// be seeded before Main: it has to be seeded before the entry type's static constructor,
// which `Program.prepare` pumps first. This guest latches a seeded property into a static
// exactly the way those switches do; if seeding moved to after the cctor pump, the field
// would be null and Main would return 1.
public class AppContextSeededBeforeCctor
{
    private static readonly string LatchedInCctor =
        AppContext.GetData("Test.Latched") as string ?? "<absent at cctor time>";

    public static int Main()
    {
        if (LatchedInCctor != "latched") { return 1; }

        // Sanity: the property really is readable from Main too, so a failure above is
        // specifically about *when* seeding happened rather than whether it happened.
        if (AppContext.GetData("Test.Latched") as string != "latched") { return 2; }

        return 0;
    }
}
