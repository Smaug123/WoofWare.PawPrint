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
    static Derived()
    {
        Recorder.Value = Recorder.Value * 10 + 1;
    }
}

class Program
{
    static int Main(string[] argv)
    {
        new Derived();
        return Recorder.Value == 12 ? 0 : Recorder.Value;
    }
}
