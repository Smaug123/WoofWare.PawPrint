using System;

[AttributeUsage(AttributeTargets.Class)]
public class MarkerAttribute : Attribute { }

[Marker]
public class Decorated
{
}

public class Undecorated
{
}

public class TypeDefCustomAttributeEnum
{
    public static int Main(string[] argv)
    {
        int result = 0;

        result |= TestDecoratedHasAttribute();
        result |= TestUndecoratedLacksAttribute() << 1;

        return result;
    }

    static int TestDecoratedHasAttribute()
    {
        // Attribute.IsDefined triggers MetadataImport.Enum for CustomAttribute
        // rows on the TypeDef parent.
        bool defined = Attribute.IsDefined(typeof(Decorated), typeof(MarkerAttribute));
        return defined ? 0 : 1;
    }

    static int TestUndecoratedLacksAttribute()
    {
        bool defined = Attribute.IsDefined(typeof(Undecorated), typeof(MarkerAttribute));
        return defined ? 1 : 0;
    }
}
