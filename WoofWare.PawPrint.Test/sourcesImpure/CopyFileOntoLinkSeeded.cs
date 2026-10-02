using System.IO;

// File.Copy with overwrite onto a symbolic link to a file. A Linux CoreLib
// opens the link for writing and calls SystemNative_CopyFile; a Darwin CoreLib
// finds clonefile refused by the existing name, will not unlink a link, and
// does the same. On a Darwin kernel that shim is fcopyfile(3), which PawPrint
// refuses to run, so this guest is run to see it stop (see TestImpureCases).
class Program
{
    static int Main(string[] args)
    {
        File.Copy("f", "lt", true);
        return File.ReadAllText("t") == "hello" ? 0 : 1;
    }
}
