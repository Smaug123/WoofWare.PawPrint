// `calli` through a signature carrying a custom modifier.
//
// A `ref readonly` return emits `modreq(System.Runtime.InteropServices.InAttribute)` in the
// standalone signature, which decodes to a `TypeDefn.Modified` wrapper around the byref.
// Anything in `executeCalli` that classifies a signature — the void-vs-value return check and
// the punned-signature guard — has to look through such wrappers, because custom modifiers
// carry calling-convention and language-level information and say nothing about the
// evaluation-stack shape. Classifying the wrapper instead of the type it wraps would reject
// this call as a shape mismatch.
//
// The `modopt(CallConv*) void` shape that would exercise the void branch of the same problem
// needs an unmanaged calling convention, which this runtime cannot execute yet, so this test
// covers the managed half only.

public static unsafe class Program
{
    static int backing = 7;

    static ref readonly int Get() => ref backing;

    static void Bump(ref readonly int _) { backing += 1; }

    public static int Main(string[] args)
    {
        delegate*<ref readonly int> get = &Get;

        ref readonly int r = ref get();
        if (r != 7) return 1;

        // A modifier on a parameter as well as on a return, and a void return alongside it, so
        // the parameter-position classification is exercised too.
        delegate*<ref readonly int, void> bump = &Bump;
        bump(in backing);

        if (backing != 8) return 2;

        // The byref really did alias the field rather than copying it.
        if (r != 8) return 3;

        return 0;
    }
}
