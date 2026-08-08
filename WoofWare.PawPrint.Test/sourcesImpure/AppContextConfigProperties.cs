using System;

// AppContext properties seeded by the host, as `hostpolicy` does from
// `runtimeconfig.json`. PawPrint-only: the differential oracle runs the guest in-process on
// the host runtime, whose AppContext we cannot reseed, so this is not a cross-runtime fact.
//
// Each failure returns a distinct code so a regression says which property broke.
public class AppContextConfigProperties
{
    public static int Main()
    {
        // A plain string value, passed through verbatim.
        if (AppContext.GetData("Test.String") is not string s) { return 1; }
        if (s != "hello world") { return 2; }

        // Strings are seeded through NUL-terminated UTF-16 `char*` buffers, so a value that
        // needs a surrogate pair, or that is empty, is where a length or terminator bug shows
        // up. "\U0001F436" is one astral character: two UTF-16 code units.
        if (AppContext.GetData("Test.Astral") is not string astral) { return 3; }
        if (astral != "p\U0001F436w") { return 4; }
        if (astral.Length != 4) { return 5; }

        if (AppContext.GetData("Test.Empty") is not string empty) { return 6; }
        if (empty.Length != 0) { return 7; }

        // Feature switches: `TryGetSwitch` falls back to `bool.TryParse` on the string, which
        // is why the JSON booleans have to render as exactly "true"/"false".
        if (!AppContext.TryGetSwitch("Test.True", out bool t)) { return 8; }
        if (!t) { return 9; }

        if (!AppContext.TryGetSwitch("Test.False", out bool f)) { return 10; }
        if (f) { return 11; }

        // A value that is present but not a parseable bool is *not* a switch.
        if (AppContext.TryGetSwitch("Test.String", out _)) { return 12; }

        // An absent key is absent, rather than defaulting to anything.
        if (AppContext.GetData("Test.Absent") is not null) { return 13; }
        if (AppContext.TryGetSwitch("Test.Absent", out _)) { return 14; }

        // The guest can still overwrite a seeded property; the host's values are a starting
        // point, not a lock.
        AppContext.SetData("Test.String", "replaced");
        if (AppContext.GetData("Test.String") is not string replaced) { return 15; }
        if (replaced != "replaced") { return 16; }

        // ... and can add ones the host never mentioned.
        AppContext.SetData("Test.Added", "added");
        if (AppContext.GetData("Test.Added") is not string added) { return 17; }
        if (added != "added") { return 18; }

        return 0;
    }
}
