public class GetAssemblyArrayTests
{
    public static int Main(string[] argv)
    {
        int result = 0;

        result |= TestArrayTypeAssemblyIdentity();

        return result;
    }

    // On real CLR, typeof(X[]).Assembly returns the same Assembly as typeof(X).Assembly.
    // Array types belong to the assembly of their element type.
    static int TestArrayTypeAssemblyIdentity()
    {
        object asm1 = typeof(GetAssemblyArrayTests).Assembly;
        object asm2 = typeof(GetAssemblyArrayTests[]).Assembly;
        return object.ReferenceEquals(asm1, asm2) ? 0 : 1;
    }
}
