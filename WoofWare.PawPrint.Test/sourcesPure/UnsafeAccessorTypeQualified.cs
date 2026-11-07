using System.Collections;
using System.Runtime.CompilerServices;
using System.Text;

// `[UnsafeAccessorType]` names given assembly-qualified, as every one of CoreLib's own accessors
// gives them: the resolver binds the assembly by display name, then finds the type in it.
//
// * `StringBuilder` is CoreLib's own.
// * `BitArray` is named in `System.Collections`, which forwards it to CoreLib.
// * `StackTraceSymbols` is in `System.Diagnostics.StackTrace`, which nothing else here loads, named
//   exactly as `StackFrameHelper.CreateStackTraceSymbols` names it: by full display name, with a
//   lower version than the shared framework's, and a public key token.
public class TestUnsafeAccessorTypeQualified
{
    private const string StackTraceSymbols =
        "System.Diagnostics.StackTraceSymbols, System.Diagnostics.StackTrace, Version=4.0.1.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a";

    private static int Run()
    {
        var sb = new StringBuilder("abc");
        if (ChunkLength(sb) != 3) return 1;

        var bits = new BitArray(5);
        if (BitLength(bits) != 5) return 2;
        BitLength(bits) = 3;
        if (bits.Length != 3) return 3;

        object symbols = NewStackTraceSymbols();
        if (symbols == null) return 4;
        if (symbols.GetType().FullName != "System.Diagnostics.StackTraceSymbols") return 5;
        if (symbols.GetType().Assembly.GetName().Name != "System.Diagnostics.StackTrace") return 6;

        return 0;
    }

    // `Main` only delegates, so that an accessor which left an argument on its caller's evaluation
    // stack is caught by the caller's return.
    public static int Main() => Run();

    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "m_ChunkLength")]
    private static extern ref int ChunkLength(
        [UnsafeAccessorType("System.Text.StringBuilder, System.Private.CoreLib")] object sb);

    [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "_bitLength")]
    private static extern ref int BitLength([UnsafeAccessorType("System.Collections.BitArray, System.Collections")] object bits);

    [UnsafeAccessor(UnsafeAccessorKind.Constructor)]
    [return: UnsafeAccessorType(StackTraceSymbols)]
    private static extern object NewStackTraceSymbols();
}
