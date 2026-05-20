public class GetAssemblyTests
{
    public static int Main(string[] argv)
    {
        int result = 0;

        result |= TestGetAssemblySucceeds();
        result |= TestGetAssemblyTwoTypes() << 1;
        result |= TestSameAssemblyIdentity() << 2;

        return result;
    }

    // Test 1: typeof(X).Assembly completes without throwing
    static int TestGetAssemblySucceeds()
    {
        object asm = typeof(GetAssemblyTests).Assembly;
        // If we got here, GetAssembly returned something.
        // Cast to object to avoid any operator overloads; use ReferenceEquals for null check.
        return object.ReferenceEquals(asm, null) ? 1 : 0;
    }

    // Test 2: typeof on a different class also works
    static int TestGetAssemblyTwoTypes()
    {
        object asm = typeof(HelperClass).Assembly;
        return object.ReferenceEquals(asm, null) ? 1 : 0;
    }

    // Test 3: two types from the same assembly must return the same Assembly object
    static int TestSameAssemblyIdentity()
    {
        object asm1 = typeof(GetAssemblyTests).Assembly;
        object asm2 = typeof(HelperClass).Assembly;
        return object.ReferenceEquals(asm1, asm2) ? 0 : 1;
    }
}

class HelperClass { }
