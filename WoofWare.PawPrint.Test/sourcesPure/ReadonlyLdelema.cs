// Exercises the `readonly.` IL prefix (ECMA-335 III.2.2). The C# compiler
// emits `readonly. ldelema` ahead of the `constrained. callvirt` when an
// instance method is called on a generic-typed array element with a
// struct-and-interface constraint. The prefix indicates that the resulting
// managed pointer will not be used to write through, allowing the runtime
// to skip the array covariance check.
//
// This mirrors `ConstrainedCallvirtGenericStructInterface.cs` but indexes
// the receiver out of an array, which is what triggers the `readonly.` +
// `ldelema` pair.

public interface IMyValue
{
    int Get();
}

public struct MyCell : IMyValue
{
    public int X;
    public int Get() => X;
}

public class TestReadonlyLdelema
{
    private static int CallOnArrayElement<T>(T[] arr) where T : struct, IMyValue
    {
        return arr[0].Get();
    }

    public static int Main(string[] argv)
    {
        var arr = new MyCell[1];
        arr[0].X = 42;
        return CallOnArrayElement(arr) == 42 ? 0 : 1;
    }
}
