using System.Reflection;

class Program
{
    public static int StaticIntField;
    public string InstanceStringField;

    static int Main(string[] args)
    {
        // FieldInfo.GetFieldFromHandle goes through
        // RuntimeFieldHandle.GetApproxDeclaringType, which in turn calls the
        // GetApproxDeclaringMethodTable InternalCall; round-tripping a field
        // through its handle exercises that path on both a static and an
        // instance field.
        FieldInfo staticField = typeof(Program).GetField(nameof(StaticIntField));
        if (staticField == null) return 1;

        FieldInfo staticFromHandle = FieldInfo.GetFieldFromHandle(staticField.FieldHandle);
        if (!ReferenceEquals(staticFromHandle, staticField)) return 2;
        if (staticFromHandle.DeclaringType != typeof(Program)) return 3;

        FieldInfo instanceField = typeof(Program).GetField(nameof(InstanceStringField));
        if (instanceField == null) return 4;

        FieldInfo instanceFromHandle = FieldInfo.GetFieldFromHandle(instanceField.FieldHandle);
        if (!ReferenceEquals(instanceFromHandle, instanceField)) return 5;
        if (instanceFromHandle.DeclaringType != typeof(Program)) return 6;

        return 0;
    }
}
