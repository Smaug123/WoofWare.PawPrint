using System;
using System.Reflection;

// `RuntimeType.PopulateProperties` gets a type's property tokens from the `MetadataImport.Enum`
// QCall with token type `mdtProperty`, then turns each into a `RuntimePropertyInfo`.
//
// PawPrint cannot yet construct one of those (that needs the associates branch of `Enum` and
// `RuntimeMethodHandle.GetSlot`), so what a guest can observe here is exactly the two shapes below:
// an enumeration that comes back empty, and one that comes back non-empty but whose members are all
// rejected by a name filter before any of them is constructed.
//
// Asking for a property that *does* exist is observable too. `PopulateProperties` suppresses
// vtable-slot duplicates (`RuntimeType.CoreCLR.cs:1358`) by calling `RuntimeMethodHandle.GetSlot`
// on each property's accessor with *no* `Virtual` guard, then testing `slot < numVirtuals`. An
// ordinary non-virtual getter occupies no vtable slot at all, so answering that call needs the
// non-vtable slot region (`PlaceNonVirtualMethods`); every case below past the first two dies in
// it otherwise.
//
// Be precise about what the case below can and cannot catch, because it is much less than it looks.
// `PopulateProperties` never reads the returned number except to compare it with `numVirtuals`, so
// *any* answer at or above `numVirtuals` gets a guest through here — measured, by stubbing the
// lookup to return `numVirtuals + 999` and watching this file still pass. So case 3 pins
// reachability and nothing more: that a non-virtual accessor gets an answer at all. Both the
// numbering within the region and the offset that places it after the vtable are pinned by the
// host-CLR oracle in TestVirtualMethodSlots, which compares PawPrint's slot number against the
// host's for every method of every corpus type.
//
// A *virtual* property accessor would exercise more of this — `Associates.AssignAssociates` feeds
// the slot straight back to `RuntimeTypeHandle.GetMethodAt` to find the override visible from the
// reflected type (Associates.cs:95-99), which does read the number. That QCall is unimplemented, so
// such a case cannot be written here yet; it is the next thing this file should grow when it lands.
//
// The field on `NoProperties` is what makes case 1 below able to catch an implementation that
// enumerated the FieldDef table instead of the PropertyMap run. Such an
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
    private static bool Has(PropertyInfo[] properties, string name)
    {
        foreach (PropertyInfo property in properties)
        {
            if (property.Name == name) return true;
        }

        return false;
    }

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

        // 3. A property that exists, on a type whose accessors are all non-virtual. Constructing the
        // `RuntimePropertyInfo` is what reaches `GetSlot` for a method with no vtable slot.
        PropertyInfo[] plain = typeof(HasProperties).GetProperties();
        if (plain.Length != 2) return 3;
        if (!Has(plain, "Alpha")) return 4;
        if (!Has(plain, "Beta")) return 5;
        if (typeof(HasProperties).GetProperty("Alpha") == null) return 6;

        // `Alpha` has a setter as well as a getter, so three accessors are placed here, not two —
        // which is what makes the walk's ordering visible to a debugger even though the guest cannot
        // read the numbers back.
        if (typeof(HasProperties).GetProperty("Alpha").GetSetMethod() == null) return 7;
        if (typeof(HasProperties).GetProperty("Beta").GetSetMethod() != null) return 8;

        return 0;
    }
}
