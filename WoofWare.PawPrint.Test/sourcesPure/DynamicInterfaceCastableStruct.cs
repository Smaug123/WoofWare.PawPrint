using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

interface IUnrelated
{
    int Answer();
}

// A *struct* implementing IDynamicInterfaceCastable. CoreCLR's MethodTableBuilder sets the
// IsIDynamicInterfaceCastable flag only inside a `!IsValueClass()` guard
// (vm/methodtablebuilder.cpp:1991), so this type never gets the flag and the runtime never
// calls back into these methods -- however the cast is spelled, and whether or not the value
// is boxed (a boxed struct shares its unboxed MethodTable).
struct CastableStruct : IDynamicInterfaceCastable
{
    public RuntimeTypeHandle GetInterfaceImplementation(RuntimeTypeHandle interfaceType)
    {
        // Unreachable: nothing sets the flag for a value class. If a runtime ever calls this,
        // returning a null handle makes the ensuing failure loud rather than silently wrong.
        return default;
    }

    public bool IsInterfaceImplemented(RuntimeTypeHandle interfaceType, bool throwIfNotImplemented)
    {
        return true;
    }
}

public class Program
{
    public static int Main(string[] args)
    {
        object boxed = new CastableStruct();

        // Type.IsInstanceOfType routes through CastHelpers.IsInstanceOfAny and hence the
        // IsInstanceOf_NoCacheLookup QCall. The structural walk fails (CastableStruct does not
        // implement IUnrelated) and the target *is* an interface, which is exactly the shape
        // that reaches ObjIsInstanceOfCore's IDynamicInterfaceCastable arm -- but the flag is
        // absent, so the answer is a plain false and IsInterfaceImplemented is never consulted.
        if (typeof(IUnrelated).IsInstanceOfType(boxed)) return 1;

        // The interface the struct really does implement still casts, by the ordinary
        // structural walk, with no callback involved.
        if (!typeof(IDynamicInterfaceCastable).IsInstanceOfType(boxed)) return 2;

        // And the same question asked of the struct's own type is unaffected.
        if (!typeof(CastableStruct).IsInstanceOfType(boxed)) return 3;

        // isinst agrees. (PawPrint interprets this opcode natively rather than through the
        // QCall, so this is a consistency check between the two paths, not a second sample of
        // the same one.)
        if (boxed is IUnrelated) return 4;

        return 0;
    }
}
