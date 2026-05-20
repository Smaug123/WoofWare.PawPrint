public interface ICounter
{
    void Increment();
    int Get();
}

public struct Counter : ICounter
{
    private int _value;

    public Counter(int value)
    {
        _value = value;
    }

    public void Increment()
    {
        _value++;
    }

    public int Get()
    {
        return _value;
    }
}

public struct ExplicitCounter : ICounter
{
    private int _value;

    public ExplicitCounter(int value)
    {
        _value = value;
    }

    void ICounter.Increment()
    {
        _value++;
    }

    int ICounter.Get()
    {
        return _value;
    }
}

public class Program
{
    private static int Run<T>(T value) where T : struct, ICounter
    {
        value.Increment();
        return value.Get();
    }

    public static int Main(string[] args)
    {
        if (Run(new Counter(10)) != 11) return 1;
        if (Run(new ExplicitCounter(20)) != 21) return 2;

        return 0;
    }
}
