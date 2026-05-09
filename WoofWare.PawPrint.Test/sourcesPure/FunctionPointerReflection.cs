using System;

unsafe class FunctionPointerReflection
{
    static int Main(string[] args)
    {
        var t = typeof(delegate*<void>);
        int result = 0;

        // Function-pointer types are TypeDescs in CoreCLR and never subclass
        // System.ValueType, so IsValueType is false.
        if (t.IsValueType)
        {
            result |= 1;
        }

        // CoreCLR's TypeString::AppendType for FnPtrType returns the empty
        // string when FormatNamespace is unset; Type.Name uses FormatBasic
        // and so observes "" for a function pointer.
        if (t.Name != "")
        {
            result |= 2;
        }

        // RuntimeType.GetName(TypeNameKind.FullName) gates on
        // IsFunctionPointer and returns null without invoking ConstructName.
        if (t.FullName != null)
        {
            result |= 4;
        }

        // ToString uses FormatNamespace, so the qualified return-type name
        // (System.Void) is included around the parameter list.
        if (t.ToString() != "System.Void()")
        {
            result |= 8;
        }

        return result;
    }
}
