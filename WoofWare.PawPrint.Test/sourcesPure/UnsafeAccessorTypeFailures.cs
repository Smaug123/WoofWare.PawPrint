using System;
using System.IO;
using System.Runtime.CompilerServices;

// What an `[UnsafeAccessorType]` declaration raises when it cannot be honoured, each a catchable
// exception from the accessor's invocation. CoreCLR resolves every name before it examines the
// accessor's kind (`ProcessUnsafeAccessorTypeAttributes`, unsafeaccessors.cpp), in Param-row order,
// and checks the resolved type against the parameter it overrides, all while compiling the stub,
// before the accessor's declaring type is initialised. The stub then type-checks each translated
// argument it passes on, with `unbox.any`.
public class TestUnsafeAccessorTypeFailures
{
    internal static int ResolutionInitialised;
    internal static int CheckedInitialised;

    private static bool Raises<T>(Action action)
        where T : Exception
    {
        try
        {
            action();
            return false;
        }
        catch (Exception e)
        {
            return e.GetType() == typeof(T);
        }
    }

    private static int Run()
    {
        // A value type: "Future versions of the runtime may support" it.
        if (!Raises<NotSupportedException>(() => Resolution.ValueType(null))) return 1;

        // The overridden parameter must be `object` (or `ref object`, or `void*`).
        if (!Raises<BadImageFormatException>(() => Resolution.NotObject(null))) return 2;

        // A byref return cannot be retyped, whatever its element.
        if (!Raises<NotSupportedException>(() => Resolution.ByrefReturn(new Hidden()))) return 3;

        // An unqualified name not in the accessor's own assembly, nor in CoreLib.
        if (!Raises<TypeLoadException>(() => Resolution.NoSuchType(null))) return 4;

        // An assembly-qualified name whose assembly does not exist.
        if (!Raises<FileNotFoundException>(() => Resolution.NoSuchAssembly(null))) return 5;

        // The return's row comes first, so its refusal wins over the parameter's.
        if (!Raises<NotSupportedException>(() => Resolution.ReturnRowFirst(null))) return 6;

        // A name is resolved before the kind is examined, so an unresolvable one raises for itself.
        if (!Raises<TypeLoadException>(() => Resolution.InvalidKindNoSuchType(null))) return 7;
        if (!Raises<BadImageFormatException>(() => Resolution.InvalidKind(new Hidden()))) return 8;

        // A resolved name with no such member is the usual missing-member refusal, on the named type.
        if (!Raises<MissingMethodException>(() => Resolution.NoSuchMember(new Hidden()))) return 9;

        // An empty name is resolved like any other, and the resolver refuses it.
        if (!Raises<TypeLoadException>(() => Resolution.EmptyName(null))) return 19;

        // The refusals recur rather than being latched.
        if (!Raises<TypeLoadException>(() => Resolution.NoSuchType(null))) return 10;

        // Every refusal above happens while the stub is compiled, before the prologue.
        if (ResolutionInitialised != 0) return 11;

        // The stub's `unbox.any` against the named type. It is in the stub's body, after the
        // prologue has initialised the accessor's declaring type (measured on real .NET 10 and 11).
        if (!Raises<InvalidCastException>(() => Checked.Twice("not hidden"))) return 12;
        if (CheckedInitialised != 1) return 18;

        // `unbox.any` to a class passes null, and the `callvirt` on it is what faults.
        if (!Raises<NullReferenceException>(() => Checked.Twice(null))) return 13;

        // A static method's first argument only names the type: the stub never loads it, so it is
        // not type-checked either.
        if (Checked.Thrice("not hidden", 5) != 15) return 14;

        // The second argument's check is reached with the first argument already checked.
        if (!Raises<InvalidCastException>(() => Checked.Add(new Hidden(), "not hidden"))) return 15;
        if (Checked.Add(new Hidden(), new Hidden()) != 42) return 16;

        if (Checked.Twice(new Hidden()) != 42) return 17;

        // A field accessor's receiver is loaded, so it is checked too.
        if (!Raises<InvalidCastException>(() => Checked.Value("not hidden"))) return 20;
        if (Checked.Value(new Hidden()) != 21) return 21;

        return 0;
    }

    public static int Main() => Run();

    private static class Resolution
    {
        static Resolution()
        {
            ResolutionInitialised++;
        }

        [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "ToString")]
        internal static extern string ValueType([UnsafeAccessorType("System.Int32")] object o);

        [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "Twice")]
        internal static extern int NotObject([UnsafeAccessorType("Hidden")] string h);

        [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "_self")]
        [return: UnsafeAccessorType("Hidden")]
        internal static extern ref object ByrefReturn([UnsafeAccessorType("Hidden")] object h);

        [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "Twice")]
        internal static extern int NoSuchType([UnsafeAccessorType("NoSuchType")] object h);

        [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "Twice")]
        internal static extern int NoSuchAssembly([UnsafeAccessorType("Hidden, NoSuchAssembly")] object h);

        [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "Make")]
        [return: UnsafeAccessorType("System.Int32")]
        internal static extern object ReturnRowFirst([UnsafeAccessorType("NoSuchType")] object h);

        [UnsafeAccessor((UnsafeAccessorKind) 99, Name = "Twice")]
        internal static extern int InvalidKindNoSuchType([UnsafeAccessorType("NoSuchType")] object h);

        [UnsafeAccessor((UnsafeAccessorKind) 99, Name = "Twice")]
        internal static extern int InvalidKind([UnsafeAccessorType("Hidden")] object h);

        [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "NoSuchMember")]
        internal static extern int NoSuchMember([UnsafeAccessorType("Hidden")] object h);

        [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "Twice")]
        internal static extern int EmptyName([UnsafeAccessorType("")] object h);
    }

    private static class Checked
    {
        static Checked()
        {
            CheckedInitialised++;
        }

        [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "Twice")]
        internal static extern int Twice([UnsafeAccessorType("Hidden")] object h);

        [UnsafeAccessor(UnsafeAccessorKind.StaticMethod, Name = "Thrice")]
        internal static extern int Thrice([UnsafeAccessorType("Hidden")] object h, int x);

        [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "Add")]
        internal static extern int Add([UnsafeAccessorType("Hidden")] object h, [UnsafeAccessorType("Hidden")] object other);

        [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "_v")]
        internal static extern ref int Value([UnsafeAccessorType("Hidden")] object h);
    }
}

internal class Hidden
{
    private int _v = 21;
    private Hidden _self;

    internal Hidden()
    {
        _self = this;
    }

    private int Twice() => _v * 2;

    private int Add(Hidden other) => _v + other._v;

    private static int Thrice(int x) => x * 3;
}
