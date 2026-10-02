// File.Copy's guest-visible results on real .NET, as a .NET 10 console app:
// what the destination ends up with (mode, owner, times, inode) and which
// exception each refusal throws, for the rows the File.Copy guests assert.
//
// Darwin: build it as a net10.0 console app (AllowUnsafeBlocks) and run
//         `dotnet netprobe.dll <empty directory>` as an ordinary user.
// Linux:  container run --rm -v "$PWD":/probe mcr.microsoft.com/dotnet/runtime:10.0.7 sh -c 'dotnet /probe/out/netprobe.dll /dev/shm/cp; dotnet /probe/out/netprobe.dll /tmp/cp'
//         as root, and as uid 1000 through `setpriv` (copy-file-guests-linux.sh
//         sets up the root-owned rows), with `one <src> <dst> <overwrite>` for a
//         single copy.
//
// Measured with .NET 10.0.7 on Darwin 27.0 (uid 501, APFS) and Linux 6.18.5
// aarch64 (root and uid 1000, tmpfs and ext4) on 2026-10-01; the output is
// beside this file.
using System;
using System.Diagnostics;
using System.IO;
using System.Runtime.InteropServices;

// Measures File.Copy's guest-visible results: what the destination ends up
// with (mode, owner, times, inode) and which exception each refusal throws.
static class P
{
    [DllImport("libc", SetLastError = true)] static extern int umask(int m);

    static string Stat(string path)
    {
        bool mac = OperatingSystem.IsMacOS();
        var psi = new ProcessStartInfo(mac ? "/usr/bin/stat" : "stat", mac ? new[] { "-f", "mode=%Sp/%Lp uid=%u gid=%g ino=%i a=%Fa m=%Fm c=%Fc b=%FB size=%z", path }
                                                 : new[] { "-c", "mode=%A/%a uid=%u gid=%g ino=%i a=%.9X m=%.9Y c=%.9Z b=%.9W size=%s", path })
        { RedirectStandardOutput = true, RedirectStandardError = true };
        var p = Process.Start(psi)!;
        string o = p.StandardOutput.ReadToEnd().Trim() + p.StandardError.ReadToEnd().Trim();
        p.WaitForExit();
        return o;
    }

    static void Run(string name, Action a)
    {
        try { a(); Console.WriteLine($"{name}: ok"); }
        catch (Exception e) { Console.WriteLine($"{name}: {e.GetType().Name} HResult=0x{e.HResult:x8} \"{e.Message}\""); }
    }

    static void Fresh(string root)
    {
        if (Directory.Exists(root)) { foreach (var d in Directory.GetDirectories(root, "*", SearchOption.AllDirectories)) File.SetUnixFileMode(d, (UnixFileMode)0x1ff); Directory.Delete(root, true); }
        Directory.CreateDirectory(root);
    }

    static void Main(string[] args)
    {
        if (args.Length > 0 && args[0] == "times")
        {
            Directory.SetCurrentDirectory(args[1]);
            File.WriteAllText("f", "hello");
            File.SetUnixFileMode("f", (UnixFileMode)Convert.ToInt32("640", 8));
            Console.WriteLine($"f before: w={File.GetLastWriteTimeUtc("f").Ticks} a={File.GetLastAccessTimeUtc("f").Ticks} {Stat("f")}");
            File.Copy("f", "copy");
            Console.WriteLine($"f after: w={File.GetLastWriteTimeUtc("f").Ticks} a={File.GetLastAccessTimeUtc("f").Ticks} {Stat("f")}");
            Console.WriteLine($"copy: w={File.GetLastWriteTimeUtc("copy").Ticks} a={File.GetLastAccessTimeUtc("copy").Ticks} {Stat("copy")}");
            return;
        }
        if (args.Length > 0 && args[0] == "one")
        {
            Console.WriteLine($"src before: {Stat(args[1])}");
            Console.WriteLine($"dst before: {Stat(args[2])}");
            Run($"copy {args[1]}->{args[2]} overwrite:{args[3]}", () => File.Copy(args[1], args[2], args[3] == "true"));
            Console.WriteLine($"dst after: {Stat(args[2])} content={(File.Exists(args[2]) ? File.ReadAllText(args[2]) : "<none>")}");
            return;
        }
        string root = Path.GetFullPath(args.Length > 0 ? args[0] : "copyprobe");
        Console.WriteLine($"uid-ish: {Environment.UserName} umask-default");
        int old = umask(Convert.ToInt32("022", 8));
        Fresh(root);
        Directory.SetCurrentDirectory(root);
        var past = new DateTime(2001, 2, 3, 4, 5, 6, DateTimeKind.Utc).AddTicks(1234567);
        var past2 = new DateTime(2002, 3, 4, 5, 6, 7, DateTimeKind.Utc).AddTicks(7654321);

        void Src(string n, string content, int mode)
        {
            File.WriteAllText(n, content);
            File.SetUnixFileMode(n, (UnixFileMode)mode);
            File.SetLastWriteTimeUtc(n, past);
            File.SetLastAccessTimeUtc(n, past2);
        }

        // 1. New destination, various source modes, umask 022.
        foreach (int mode in new[] { Convert.ToInt32("640", 8), Convert.ToInt32("777", 8), Convert.ToInt32("4755", 8), Convert.ToInt32("2755", 8), Convert.ToInt32("1644", 8), Convert.ToInt32("000", 8), Convert.ToInt32("200", 8) })
        {
            string s = $"s{Convert.ToString(mode, 8)}", d = $"d{Convert.ToString(mode, 8)}";
            Src(s, "hello", mode);
            Console.WriteLine($"src {s}: {Stat(s)}");
            Run($"copy {s}->{d}", () => File.Copy(s, d));
            if (File.Exists(d)) Console.WriteLine($"dst {d}: {Stat(d)}");
            if (File.Exists(d)) Console.WriteLine($"src after {s}: {Stat(s)}");
        }
        // umask 077 with mode 0777.
        umask(Convert.ToInt32("077", 8));
        Src("su", "hello", Convert.ToInt32("777", 8));
        Run("copy su->du (umask 077)", () => File.Copy("su", "du"));
        Console.WriteLine($"dst du: {Stat("du")}");
        umask(Convert.ToInt32("022", 8));

        // 2. Existing destination.
        Src("e1", "hello", Convert.ToInt32("640", 8));
        File.WriteAllText("x1", "previous content that is longer");
        File.SetUnixFileMode("x1", (UnixFileMode)Convert.ToInt32("600", 8));
        Console.WriteLine($"x1 before: {Stat("x1")}");
        Run("copy e1->x1 overwrite:false", () => File.Copy("e1", "x1", false));
        Console.WriteLine($"x1 after false: {Stat("x1")} content={File.ReadAllText("x1")}");
        Run("copy e1->x1 overwrite:true", () => File.Copy("e1", "x1", true));
        Console.WriteLine($"x1 after true: {Stat("x1")} content={File.ReadAllText("x1")}");

        // 3. Directories.
        Directory.CreateDirectory("dir");
        Run("copy dir->nd", () => File.Copy("dir", "nd"));
        Run("copy e1->dir overwrite:false", () => File.Copy("e1", "dir", false));
        Run("copy e1->dir overwrite:true", () => File.Copy("e1", "dir", true));
        Console.WriteLine($"dir after: {Stat("dir")}");

        // 4. Missing source / missing destination parent.
        Run("copy missing->m", () => File.Copy("missing", "m"));
        Run("copy e1->nodir/m", () => File.Copy("e1", "nodir/m"));
        Run("copy e1/sub->m", () => File.Copy("e1/sub", "m"));
        Run("copy e1->e1/sub", () => File.Copy("e1", "e1/sub"));

        // 5. Same file.
        Run("copy e1->e1 overwrite:false", () => File.Copy("e1", "e1", false));
        Run("copy e1->e1 overwrite:true", () => File.Copy("e1", "e1", true));
        Console.WriteLine($"e1 after self: {Stat("e1")} content={File.ReadAllText("e1")}");
        File.CreateSymbolicLink("le1", "e1");
        Run("copy le1->e1 overwrite:true", () => File.Copy("le1", "e1", true));
        Run("copy e1->le1 overwrite:true", () => File.Copy("e1", "le1", true));
        Console.WriteLine($"le1 after: {Stat("le1")}; e1 {Stat("e1")} content={File.ReadAllText("e1")}");
        File.Delete("le1");

        // 6. Unwritable destination directory.
        Directory.CreateDirectory("ro");
        File.WriteAllText("ro/w", "writable inside unwritable");
        File.SetUnixFileMode("ro/w", (UnixFileMode)Convert.ToInt32("666", 8));
        File.SetUnixFileMode("ro", (UnixFileMode)Convert.ToInt32("555", 8));
        Run("copy e1->ro/new", () => File.Copy("e1", "ro/new"));
        Run("copy e1->ro/w overwrite:false", () => File.Copy("e1", "ro/w", false));
        Console.WriteLine($"ro/w before overwrite: {Stat("ro/w")}");
        Run("copy e1->ro/w overwrite:true", () => File.Copy("e1", "ro/w", true));
        Console.WriteLine($"ro/w after overwrite: {Stat("ro/w")} content={File.ReadAllText("ro/w")}");
        File.SetUnixFileMode("ro", (UnixFileMode)Convert.ToInt32("755", 8));

        // 7. Symlinks.
        Src("t", "target", Convert.ToInt32("640", 8));
        File.CreateSymbolicLink("lt", "t");
        Run("copy lt->fromlink", () => File.Copy("lt", "fromlink"));
        Console.WriteLine($"fromlink: {Stat("fromlink")}");
        File.WriteAllText("t2", "t2 content");
        File.CreateSymbolicLink("lt2", "t2");
        Run("copy e1->lt2 overwrite:false", () => File.Copy("e1", "lt2", false));
        Run("copy e1->lt2 overwrite:true", () => File.Copy("e1", "lt2", true));
        Console.WriteLine($"lt2 is link: {new FileInfo("lt2").LinkTarget ?? "<not a link>"}; t2 content={File.ReadAllText("t2")}; lt2 {Stat("lt2")}");
        File.CreateSymbolicLink("dang", "nowhere");
        Run("copy e1->dang overwrite:false", () => File.Copy("e1", "dang", false));
        Run("copy e1->dang overwrite:true", () => File.Copy("e1", "dang", true));
        Console.WriteLine($"dang link: {new FileInfo("dang").LinkTarget ?? "<not a link>"}; nowhere exists={File.Exists("nowhere")}");

        // 8. Empty source, read-only existing destination, unreadable source.
        Src("empty", "", Convert.ToInt32("644", 8));
        Run("copy empty->dempty", () => File.Copy("empty", "dempty"));
        Console.WriteLine($"dempty: {Stat("dempty")}");
        File.WriteAllText("rod", "readonly dest");
        File.SetUnixFileMode("rod", (UnixFileMode)Convert.ToInt32("444", 8));
        Run("copy e1->rod overwrite:true", () => File.Copy("e1", "rod", true));
        Console.WriteLine($"rod: {Stat("rod")} content={File.ReadAllText("rod")}");
        Src("unr", "secret", Convert.ToInt32("200", 8));
        Run("copy unr->dunr", () => File.Copy("unr", "dunr"));

        // 9. Held lock on destination.
        File.WriteAllText("held", "held");
        using (var h = new FileStream("held", FileMode.Open, FileAccess.Read, FileShare.Read))
        {
            Run("copy e1->held overwrite:true (held shared)", () => File.Copy("e1", "held", true));
        }
        Console.WriteLine($"held: {File.ReadAllText("held")}");
        umask(old);
    }
}
