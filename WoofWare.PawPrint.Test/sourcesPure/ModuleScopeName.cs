using System;
using System.Reflection;

namespace ModuleScopeName
{
    public class Sibling { }

    public class Program
    {
        public static int Main(string[] args)
        {
            Module module = typeof(Program).Module;
            string scopeName = module.ScopeName;

            if (string.IsNullOrEmpty(scopeName))
            {
                return 1;
            }

            // `Module.ToString()` is defined as `ScopeName`.
            if (module.ToString() != scopeName)
            {
                return 2;
            }

            // One module per assembly, so a sibling type reports the same scope.
            if (typeof(Sibling).Module.ScopeName != scopeName)
            {
                return 3;
            }

            // The scope name is the Module row's own `Name` column, which the compiler stamps
            // from the output file name -- so it starts with the assembly's simple name and
            // carries an extension after it. Checked as a relationship rather than a literal:
            // the harness picks the assembly name, and the extension depends on how the image
            // was emitted.
            string fullName = typeof(Program).Assembly.FullName;
            int comma = fullName.IndexOf(',');
            string simpleName = comma < 0 ? fullName : fullName.Substring(0, comma);

            if (!scopeName.StartsWith(simpleName, StringComparison.Ordinal))
            {
                return 4;
            }

            if (scopeName.Length <= simpleName.Length)
            {
                return 5;
            }

            // A different module reports a different scope, and corelib's is fixed.
            Module corelib = typeof(int).Module;

            if (corelib.ScopeName != "System.Private.CoreLib.dll")
            {
                return 6;
            }

            if (corelib.ScopeName == scopeName)
            {
                return 7;
            }

            // CoreCLR does not intern the QCall's result, so each read allocates afresh.
            if (object.ReferenceEquals(module.ScopeName, module.ScopeName))
            {
                return 8;
            }

            return 0;
        }
    }
}
