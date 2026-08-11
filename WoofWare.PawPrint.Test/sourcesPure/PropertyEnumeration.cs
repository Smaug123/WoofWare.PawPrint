using System;
using System.Reflection;

// `RuntimeType.PopulateProperties` gets a type's property tokens from the `MetadataImport.Enum`
// QCall with token type `mdtProperty`, then turns each into a `RuntimePropertyInfo`.
//
// PawPrint cannot yet construct one of those (that needs `MetadataImport.GetPropertyProps`, the
// associates branch of `Enum`, and `RuntimeMethodHandle.GetSlot`), so what a guest can observe here
// is exactly the two shapes below: an enumeration that comes back empty, and one that comes back
// non-empty but whose members are all rejected by a name filter before any of them is constructed.
// Both threw before the enumeration existed.
// The field is load-bearing, not decoration: it is what makes case 1 below able to catch an
// implementation that enumerated the FieldDef table instead of the PropertyMap run. Such an
// implementation reports one "property" here, and the guest then dies trying to construct a
// `RuntimePropertyInfo` for it.
public class NoProperties
{
    public int Field;

    public int Method()
    {
        return 1;
    }
}

public class HasProperties
{
    public int Alpha { get; set; }

    public string Beta
    {
        get { return "b"; }
    }
}

public class PropertyEnumeration
{
    public static int Main(string[] argv)
    {
        // A type with members but no properties. `PopulateProperties` walks the base chain, so this
        // also enumerates `object`'s (empty) property list.
        if (typeof(NoProperties).GetProperties().Length != 0) return 1;

        // A type that *does* have properties, asked for one that does not exist. The filter calls
        // `MetadataImport.GetName` on every token the enumeration produced, so a token that is not a
        // real PropertyDef of this assembly fails here rather than being silently ignored — which is
        // what makes this stronger than the empty case. It cannot, however, distinguish a correct
        // token list from a merely well-formed one; that is the unit tests' job.
        if (typeof(HasProperties).GetProperty("NoSuchName") != null) return 2;

        return 0;
    }
}
