using System;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;

// Each type here derives from a type in a framework assembly that nothing has loaded by the time
// `Assembly.GetType` finds it, and nothing else touches these types first. Allocating the
// `RuntimeType` walks the base chain, so the lookup has to bring the base's assembly in itself.
// Each check uses a different framework assembly, so that an earlier check cannot have loaded
// what a later one needs.
public class DerivedFromObservableCollection : ObservableCollection<int> { }

public class Outer
{
    public class NestedFromNameValueCollection : NameValueCollection { }
}

public class DerivedFromTypeConverter : TypeConverter { }

public class Program
{
    public static int Main (string[] args)
    {
        var asm = typeof (Program).Assembly;

        // Found by its TypeDef in the asking assembly.
        Type direct = asm.GetType ("DerivedFromObservableCollection");
        if (direct == null) return 1;
        if (direct.BaseType.Name != "ObservableCollection`1") return 2;

        // Found by the case-insensitive lookup.
        Type folded = asm.GetType ("derivedfromtypeconverter", false, true);
        if (folded == null) return 5;
        if (folded.Name != "DerivedFromTypeConverter") return 6;
        if (folded.BaseType.Name != "TypeConverter") return 7;

        return 0;
    }
}
