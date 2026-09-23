using System;

// `Environment.GetEnvironmentVariable` with a name holding an unpaired UTF-16
// surrogate, which a .NET string can carry and a Unix environment cannot.
//
// On Unix the `kernel32!GetEnvironmentVariableW` QCall is the PAL's, and the
// PAL holds the environment as the bytes the process was started with. It
// converts the requested name to UTF-8 before comparing (`WideCharToMultiByte`
// in pal/src/misc/environ.cpp), and that conversion substitutes U+FFFD for
// each unpaired surrogate rather than failing. So the name "\uD800" is looked
// up as the three bytes of U+FFFD, and finds an entry whose name is those
// bytes.
//
// Registered under `environmentCases` in TestPureCases with the entry
// "�=found", which is set in the oracle process's environment as well as
// in the kernel's.
public class TestEnvironmentVariableUnpairedSurrogateName
{
    public static int Main(string[] argv)
    {
        // The control: the name as it really is.
        if (Environment.GetEnvironmentVariable("�") != "found") return 1;

        // An unpaired high surrogate, and an unpaired low one.
        if (Environment.GetEnvironmentVariable("\uD800") != "found") return 2;
        if (Environment.GetEnvironmentVariable("\uDFFF") != "found") return 3;

        // One replacement per unpaired code unit, so two of them name a
        // different, absent variable.
        if (Environment.GetEnvironmentVariable("\uDC00\uD800") != null) return 4;

        // A well-formed pair is a real character, not a replacement.
        if (Environment.GetEnvironmentVariable("𐀀") != null) return 5;

        return 0;
    }
}
