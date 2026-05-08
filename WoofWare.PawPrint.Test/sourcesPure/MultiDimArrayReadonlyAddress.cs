// ECMA-335 III.2.2: when Roslyn lowers a `ref readonly` to a multi-dim element
// over a covariantly-cast array (e.g. `object[,] o = (object[,])new string[1,1]`),
// it emits `readonly. + Address(...)`. The prefix suppresses the
// ArrayTypeMismatchException that the multi-dim Address would otherwise throw
// because the runtime element type (string) does not equal the metadata-declared
// element type (object). This is the multi-dim analogue of `ReadonlyLdelema.cs`.

public class Program
{
    public static int Main(string[] args)
    {
        string[,] s = new string[1, 1];
        s[0, 0] = "hello";
        object[,] o = s;

        ref readonly object x = ref o[0, 0];

        if (object.ReferenceEquals(x, "hello"))
        {
            return 0;
        }

        return 1;
    }
}
