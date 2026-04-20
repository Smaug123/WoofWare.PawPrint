// Issue 1 & 3: resolveTypeFromDefn's GenericInstantiation case round-trips
// through TypeInfo, and resolveBaseTypeInfo does the same for base types.
// Both collapse OneDimensionalArrayLowerBoundZero to FromDefinition(System.Array).
//
// When DerivedFromArrayBase : GenericBase<int[]> is defined, the base type
// is encoded as a TypeSpec. resolveBaseTypeInfo resolves this via
// resolveTypeFromSpecConcrete, which calls resolveTypeFromDefn.
// The GenericInstantiation case resolves the int[] arg → System.Array TypeInfo
// → typeInfoToTypeDefn → FromDefinition(System.Array). This makes the base
// type concretize as GenericBase<System.Array> instead of GenericBase<int[]>.
//
// Observable consequence: `obj is GenericBase<int[]>` fails because the
// base type in the hierarchy is GenericBase<System.Array>, not GenericBase<int[]>.

public class GenericBase<T>
{
    public T Value;

    public GenericBase() {}
}

public class DerivedFromArrayBase : GenericBase<int[]>
{
    public DerivedFromArrayBase() {}
}

public class GenericArrayCollapse1
{
    public static int Main(string[] args)
    {
        var d = new DerivedFromArrayBase();
        d.Value = new int[] { 1, 2, 3 };

        // On real CLR, DerivedFromArrayBase is assignable to GenericBase<int[]>.
        // If the base type was collapsed to GenericBase<System.Array>,
        // this cast check would fail.
        object boxed = d;
        if (!(boxed is GenericBase<int[]>)) return 1;

        // Verify the value is accessible through the base type reference.
        GenericBase<int[]> asBase = (GenericBase<int[]>)boxed;
        int[] val = asBase.Value;
        if (val[0] != 1) return 2;
        if (val.Length != 3) return 3;

        return 0;
    }
}
