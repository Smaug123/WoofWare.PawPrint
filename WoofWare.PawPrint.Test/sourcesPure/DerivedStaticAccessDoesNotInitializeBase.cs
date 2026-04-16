class Recorder
{
    public static int Value;
}

class Base
{
    static Base()
    {
        Recorder.Value = Recorder.Value * 10 + 2;
    }
}

class Derived : Base
{
    public static int Y = 7;

    static Derived()
    {
        Recorder.Value = Recorder.Value * 10 + 1;
    }
}

class Program
{
    static int Main(string[] argv)
    {
        int y = Derived.Y;
        return y == 7 && Recorder.Value == 1 ? 0 : Recorder.Value * 10 + y;
    }
}
