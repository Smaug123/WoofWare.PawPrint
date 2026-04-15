public delegate int MyTransform(int value);

public class InstanceDelegateDemo
{
    private int _offset;

    public InstanceDelegateDemo(int offset)
    {
        _offset = offset;
    }

    public int AddOffset(int value)
    {
        return value + _offset;
    }

    public static int Main(string[] argv)
    {
        var demo = new InstanceDelegateDemo(10);
        MyTransform transform = demo.AddOffset;
        int result = transform(5);
        if (result == 15) return 7;
        return 1;
    }
}
