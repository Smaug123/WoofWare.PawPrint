using System;
using System.IO;
using System.Reflection;

// An `AssemblyName` built by hand rather than parsed, so that its `Name` can be things the
// parser never produces. A `Name` never set reaches `AssemblyNative_InternalLoad` as a null
// pointer, which CoreCLR refuses as an argument before binding anything; a `Name` set to `""`
// binds nothing and is reported under `<Unknown>`, whatever else the request carried.
public class Program
{
    public static int Main (string[] args)
    {
        try
        {
            Assembly.Load (new AssemblyName ());
            return 1;
        }
        catch (ArgumentException e)
        {
            if (e.Message != "String cannot have zero length.")
                return 2;
        }

        try
        {
            Assembly.Load (new AssemblyName { Name = "", Version = new Version (1, 2, 3, 4) });
            return 3;
        }
        catch (FileNotFoundException e)
        {
            if (e.FileName != "<Unknown>")
                return 4;
            if (e.Message != "Could not load file or assembly '<Unknown>'. The system cannot find the file specified.\n")
                return 5;
        }

        return 0;
    }
}
