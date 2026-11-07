using System.Collections.Generic;
using System.Runtime.CompilerServices;

// `[UnsafeAccessorType]` may name one of the accessor's own type parameters, spelled as IL spells
// them -- `!0` for the declaring type's, `!!0` for the method's -- here inside a generic
// instantiation. The managed resolver hands such a name back to the VM through the
// `UnsafeAccessors_ResolveGenericParamToTypeHandle` QCall, which answers with the type variable
// itself, so the translated signature compares as the declaration's own `List<T>` would.
public class TestUnsafeAccessorTypeGenericParameter
{
    private static int Run()
    {
        var list = new List<int> { 1, 2, 3 };
        if (Accessors<int>.Size(list) != 3) return 1;

        var strings = new List<string> { "a" };
        if (Accessors<string>.Size(strings) != 1) return 2;

        if (MethodAccessors.Size<int>(list) != 3) return 3;

        return 0;
    }

    public static int Main() => Run();

    private static class Accessors<T>
    {
        [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "_size")]
        internal static extern ref int Size(
            [UnsafeAccessorType("System.Collections.Generic.List`1[[!0]], System.Private.CoreLib")] object list);
    }

    private static class MethodAccessors
    {
        [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "_size")]
        internal static extern ref int Size<T>(
            [UnsafeAccessorType("System.Collections.Generic.List`1[[!!0]], System.Private.CoreLib")] object list);
    }
}
