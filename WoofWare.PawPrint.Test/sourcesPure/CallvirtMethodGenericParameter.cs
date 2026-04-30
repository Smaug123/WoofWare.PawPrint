public class GenericVirtual
{
    public virtual T Echo<T>(T value)
    {
        return value;
    }
}

public class Program
{
    private static T RoundTrip<T>(GenericVirtual receiver, T value)
    {
        return receiver.Echo<T>(value);
    }

    public static int Main(string[] args)
    {
        return RoundTrip<int>(new GenericVirtual(), 42) == 42 ? 0 : 1;
    }
}
