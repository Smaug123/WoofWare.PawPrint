namespace WoofWare.PawPrint.Test

open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// A guest writing into the characters of an interned string, which PawPrint refuses.
///
/// CoreCLR's literal map compares a lookup against each entry's *current* characters
/// (`EEUnicodeStringLiteralHashTableHelper::CompareKeys`, vm/eehash.cpp), so once an interned
/// object changes, a lookup of its old value misses and allocates a fresh literal. But the JIT
/// resolves an `ldstr` when it compiles the method (`fgMorphConst` → `constructStringLiteral`),
/// except in a throw block, which resolves it on every execution. Measured on .NET 10: a method
/// compiled before the write keeps returning the written object; at tier 1 a recompilation
/// re-resolves it to the fresh one; full opts constant-fold `literal[0]` from the literal while
/// tier 0 reads the written object. Which object, and which characters, a later `ldstr`,
/// `string.Intern` or `string.IsInterned` of either value gives therefore depends on JIT tier and
/// inlining, so there is no differential oracle for it. The writes CoreCLR answers the same way
/// on every tier are `sourcesPure/StringWriteUninterned.cs`.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestInternedStringWrites =

    let private runRefused (name : string) (source : string) : GuestFailureException =
        let image = Roslyn.compile [ source ]

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", name ]

        use _loggerFactoryResource = loggerFactory
        use peImage = new MemoryStream (image)

        Assert.Throws<GuestFailureException> (fun () ->
            BoundedRun.runWith
                loggerFactory
                BoundedRun.defaultMaxSteps
                name
                (Some name)
                peImage
                (HostConfig.Default (FrameworkUnderTest.runtimeDirsFor KernelConfig.Default.UnixPlatform))
            |> ExpectRun.ended
            |> ignore<RunOutcome>
        )

    let private refusal : string = "of the interned string"

    [<Test>]
    let ``writing a literal's character through a pointer is refused`` () : unit =
        let source =
            """
using System.Runtime.CompilerServices;

public class Program
{
    [MethodImpl(MethodImplOptions.NoInlining)] static string First() => "qqqqqqqqqqqqqqqqqqqqqqq";
    [MethodImpl(MethodImplOptions.NoInlining)] static string Second() => "qqqqqqqqqqqqqqqqqqqqqqq";

    public static unsafe int Main(string[] args)
    {
        string original = First();
        fixed (char* ptr = original) ptr[0] = 'r';
        string second = Second();
        return second[0] == 'q' ? 0 : 1;
    }
}
"""

        let exc = runRefused "InternedLiteralPointerWrite.cs" source
        exc.Message |> shouldContainText refusal

    [<Test>]
    let ``writing a literal's character through a ref char is refused`` () : unit =
        let source =
            """
using System;
using System.Runtime.InteropServices;

public class Program
{
    public static int Main(string[] args)
    {
        string s = "ab";
        ref char first = ref MemoryMarshal.GetReference(s.AsSpan());
        first = 'z';
        return s[0] == 'z' ? 0 : 1;
    }
}
"""

        let exc = runRefused "InternedLiteralRefCharWrite.cs" source
        exc.Message |> shouldContainText refusal

    [<Test>]
    let ``writing a literal's bytes through a byte view is refused`` () : unit =
        let source =
            """
using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

public class Program
{
    public static int Main(string[] args)
    {
        string s = "ab";
        ref byte second = ref Unsafe.As<char, byte>(ref Unsafe.Add(ref MemoryMarshal.GetReference(s.AsSpan()), 1));
        Unsafe.WriteUnaligned(ref second, (ushort)'y');
        return s[1] == 'y' ? 0 : 1;
    }
}
"""

        let exc = runRefused "InternedLiteralByteViewWrite.cs" source
        exc.Message |> shouldContainText refusal

    [<Test>]
    let ``writing a string that string.Intern interned is refused`` () : unit =
        let source =
            """
public class Program
{
    public static unsafe int Main(string[] args)
    {
        string s = string.Intern(new string('y', 5));
        fixed (char* ptr = s) ptr[4] = 'x';
        return string.IsInterned(new string('y', 5)) == null ? 0 : 1;
    }
}
"""

        let exc = runRefused "InternedNonLiteralPointerWrite.cs" source
        exc.Message |> shouldContainText refusal

    [<Test>]
    let ``writing a literal's length through reflection is refused`` () : unit =
        // CoreCLR's key comparison reads the entry's current length too.
        let source =
            """
using System.Reflection;

public class Program
{
    public static int Main(string[] args)
    {
        FieldInfo length = typeof(string).GetField("_stringLength", BindingFlags.NonPublic | BindingFlags.Instance)!;
        string literal = "abc";
        length.SetValue(literal, 2);
        return literal.Length == 2 ? 0 : 1;
    }
}
"""

        let exc = runRefused "InternedLiteralLengthWrite.cs" source
        exc.Message |> shouldContainText refusal
