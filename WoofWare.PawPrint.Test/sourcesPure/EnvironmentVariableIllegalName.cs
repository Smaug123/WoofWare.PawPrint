using System;
using System.Collections;

// `Environment.GetEnvironmentVariable` with a name no environment entry could
// ever carry.
//
// `SetEnvironmentVariable` validates its name (ValidateVariable rejects empty
// and any '='), but the *getter* only null-checks — so these calls really do
// reach the runtime, and a guest can make them from ordinary user input.
//
// Real .NET answers null for all of them, and deliberately rather than by
// accident: CoreCLR's PAL refuses such names above its matching loop
// (`GetEnvironmentVariableA` in pal/src/misc/environ.cpp — "GetEnvironmentVariable
// doesn't permit '=' in variable names"), setting ERROR_ENVVAR_NOT_FOUND. The
// loop underneath, `FindEnvVarValue`, would otherwise happily resolve the name
// `A=B` against an entry `A=B=C`.
//
// PawPrint reaches the same answers by a different route — its environment map
// cannot hold such a name at all, so the lookup misses and reports the identical
// ERROR_ENVVAR_NOT_FOUND — and this file is what pins that they really do agree.
public class TestEnvironmentVariableIllegalName
{
    public static int Main(string[] argv)
    {
        // A name containing '=' — the one the PAL's matching loop would otherwise
        // resolve against a longer entry.
        if (Environment.GetEnvironmentVariable("A=B") != null) return 1;

        // '=' first, which is also the shape of Windows' hidden "=C:" variables.
        if (Environment.GetEnvironmentVariable("=C:") != null) return 2;

        // The empty name, which the PAL rejects with its own explicit guard.
        if (Environment.GetEnvironmentVariable("") != null) return 3;

        // The sharp case: an existing variable's name with "=..." appended. A
        // lookup that split the requested name at '=' before consulting the table
        // would wrongly answer with that variable's value.
        //
        // The witness is *discovered* rather than named, because the two runtimes
        // have entirely different environments — the real one inherits the test
        // host's, PawPrint's is the seeded kernel table — so no hardcoded name
        // (not even PATH) is present in both. Deriving it from enumeration makes
        // this check bite in whichever environment it finds itself in.
        IDictionary vars = Environment.GetEnvironmentVariables();

        int witnessesChecked = 0;

        foreach (DictionaryEntry entry in vars)
        {
            string key = (string)entry.Key;

            // Only a non-empty value makes this check meaningful: appending
            // "=x" to a name whose variable is empty would be indistinguishable
            // from a correct null.
            if (((string)entry.Value).Length == 0) continue;

            if (Environment.GetEnvironmentVariable(key) == null) return 4;
            if (Environment.GetEnvironmentVariable(key + "=x") != null) return 5;
            if (Environment.GetEnvironmentVariable(key + "=") != null) return 6;

            witnessesChecked++;

            // One is enough, and enumerating every variable through a slow
            // interpreter is not.
            break;
        }

        // An environment with no non-empty variable is legal (`env -i` gives one
        // with none at all), so finding no witness is not a failure — but say so
        // rather than reporting a pass that checked nothing. Both runtimes in this
        // suite do have one.
        if (witnessesChecked == 0) return 7;

        // The setter, by contrast, must reject rather than answer: that asymmetry
        // is what lets an illegal name reach the getter at all.
        try
        {
            Environment.SetEnvironmentVariable("A=B", "z");
            return 8;
        }
        catch (ArgumentException)
        {
        }

        try
        {
            Environment.SetEnvironmentVariable("", "z");
            return 9;
        }
        catch (ArgumentException)
        {
        }

        // A null name is the one input the getter does check.
        try
        {
            Environment.GetEnvironmentVariable(null);
            return 10;
        }
        catch (ArgumentNullException)
        {
        }

        return 0;
    }
}
