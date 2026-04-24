public interface IValueProducer
{
    int Produce();
}

public class RefProducer : IValueProducer
{
    private readonly int _value;
    public RefProducer(int value) { _value = value; }
    public int Produce() { return _value; }
}

public class Program
{
    // Inside a generic method the C# compiler emits `constrained. !!T callvirt IValueProducer::Produce()`
    // because T is unknown at IL-compile time. When T is a reference type the runtime takes ECMA III.2.1
    // case 1 (dereference the byref, dispatch virtually).
    private static int CallGeneric<T>(T value) where T : IValueProducer
    {
        return value.Produce();
    }

    public static int Main(string[] args)
    {
        // Direct (non-generic) callvirt on an interface method — no `constrained.` here,
        // just a sanity baseline.
        IValueProducer direct = new RefProducer(7);
        if (direct.Produce() != 7) return 1;

        // constrained.callvirt case 1: T = RefProducer (a reference type).
        RefProducer concrete = new RefProducer(42);
        if (CallGeneric(concrete) != 42) return 2;

        return 0;
    }
}
