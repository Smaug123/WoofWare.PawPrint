using System;

// ECMA-335 II.23.3 encodes an enum-valued attribute argument as a bare value of the enum's
// *underlying* type, with nothing in the blob to say how wide it is. The decoder therefore has to
// resolve the constructor's parameter type and read its `value__` width.
//
// Neither enum here is int32-underlying, on purpose: a decoder that assumed 4 bytes would decode
// ByteFlag.Big from the wrong bytes and then desynchronise the cursor for everything after it.
// Width is the part this test can see. Signedness is not: reading a byte-underlying enum as
// *signed* still reads one byte, and the ctor's parameter slot truncates -56 back to 200, so the
// guest cannot tell. TestCustomAttributeBlob covers that at the decoder instead.
public enum ByteFlag : byte
{
    None = 0,
    Big = 200,
}

public enum LongFlag : long
{
    Zero = 0,
    Huge = 0x123456789ABCL,
}

[AttributeUsage(AttributeTargets.Class)]
public class ByteMarkerAttribute : Attribute
{
    public ByteMarkerAttribute(ByteFlag flag)
    {
        Flag = flag;
    }

    public ByteFlag Flag { get; }
}

[AttributeUsage(AttributeTargets.Class)]
public class LongMarkerAttribute : Attribute
{
    // A second fixed arg after the enum, so a wrong-width enum read desynchronises the cursor
    // and corrupts this one too rather than failing silently.
    public LongMarkerAttribute(LongFlag flag, int tail)
    {
        Flag = flag;
        Tail = tail;
    }

    public LongFlag Flag { get; }
    public int Tail { get; }
}

[ByteMarker(ByteFlag.Big)]
[LongMarker(LongFlag.Huge, 4242)]
public class Decorated
{
}

public class CustomAttributeEnumArg
{
    public static int Main(string[] argv)
    {
        var b = (ByteMarkerAttribute)Attribute.GetCustomAttribute(typeof(Decorated), typeof(ByteMarkerAttribute));
        if (b == null) return 1;
        if (b.Flag != ByteFlag.Big) return 2;

        var l = (LongMarkerAttribute)Attribute.GetCustomAttribute(typeof(Decorated), typeof(LongMarkerAttribute));
        if (l == null) return 3;
        if (l.Flag != LongFlag.Huge) return 4;
        if (l.Tail != 4242) return 5;

        return 0;
    }
}
