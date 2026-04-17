class Recorder
{
    public static int Value;
}

class Base
{
    public static int BaseField = 5;

    static Base()
    {
        Recorder.Value = Recorder.Value * 10 + 2;
    }
}

class Derived : Base
{
    public static int Sum;

    static Derived()
    {
        Recorder.Value = Recorder.Value * 10 + 1;
        Sum = BaseField + 3;
    }
}

class Program
{
    static int Main(string[] argv)
    {
        int sum = Derived.Sum;
        // Accessing Derived.Sum triggers Derived.cctor, which reads Base.BaseField,
        // triggering Base.cctor mid-way through Derived.cctor.
        // Order: Derived.cctor starts (Value = 1), Base.cctor runs (Value = 12),
        // Derived.cctor finishes (Sum = 5 + 3 = 8).
        return sum == 8 && Recorder.Value == 12 ? 0 : Recorder.Value * 100 + sum;
    }
}
