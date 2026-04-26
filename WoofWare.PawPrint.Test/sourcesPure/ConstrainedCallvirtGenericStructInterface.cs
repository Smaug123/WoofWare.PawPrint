public interface ICell<T>
{
    void Set(T value);
    T Get();
}

public struct IntCell : ICell<int>
{
    private int _value;

    void ICell<int>.Set(int value)
    {
        _value = value;
    }

    int ICell<int>.Get()
    {
        return _value;
    }
}

public class Program
{
    private static int Run<T>(T cell) where T : struct, ICell<int>
    {
        cell.Set(42);
        return cell.Get();
    }

    public static int Main(string[] args)
    {
        return Run(new IntCell()) == 42 ? 0 : 1;
    }
}
