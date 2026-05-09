using System;
using System.Collections.Generic;

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

        // Generic instantiations inside a function pointer signature must be
        // included when FormatNamespace is set. CoreCLR's AppendType emits the
        // instantiation whenever FormatNamespace or FormatAssembly is set,
        // independent of FormatFullInst.
        var generic = typeof(delegate*<List<int>>);
        if (generic.ToString() != "System.Collections.Generic.List`1[System.Int32]()")
        {
            result |= 16;
        }

        // Sanity check on the recursion's source: List<int>.ToString is the
        // same instantiation-bearing form, with no surrounding fnptr wrapper.
        if (typeof(List<int>).ToString() != "System.Collections.Generic.List`1[System.Int32]")
        {
            result |= 32;
        }

        return result;
    }
}
