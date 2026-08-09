using System;
using System.Collections.Generic;

public class Program
{
    public static int Main(string[] args)
    {
        // A generic parameter is a TypeVarTypeDesc, not a MethodTable, and ObjIsInstanceOfCore
        // answers a flat false for any TypeDesc target without consulting the structural cast
        // walk at all (src/coreclr/vm/jithelpers.cpp:409). Nothing is ever an instance of a
        // bare type parameter.
        //
        // This matters beyond the answer itself: PawPrint's cast oracle refuses a
        // generic-parameter target outright, so taking the TypeDesc branch first is what keeps
        // the IsInstanceOf_NoCacheLookup QCall total rather than crashing here.
        Type openParameter = typeof(List<>).GetGenericArguments()[0];

        if (openParameter.IsInstanceOfType(new object())) return 1;
        if (openParameter.IsInstanceOfType("a string")) return 2;
        if (openParameter.IsInstanceOfType(5)) return 3;

        // Control: null short-circuits in managed code before the QCall, so this false says
        // nothing about the TypeDesc rule; and an ordinary MethodTable-backed target still
        // answers true, so a failure above is about TypeDescs rather than these operands.
        if (openParameter.IsInstanceOfType(null)) return 4;
        if (!typeof(object).IsInstanceOfType("a string")) return 5;

        return 0;
    }
}
