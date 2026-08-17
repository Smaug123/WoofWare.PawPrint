using System;
using System.Reflection;

// `PropertyInfo.GetRawConstantValue` / `GetConstantValue`, which reach
// `MetadataImport.GetDefaultValue` with a PropertyDef token — the third parent of the Constant
// table's HasConstant coded index (ECMA-335 II.24.2.6).
//
// C# cannot emit a Constant row on a property at all, so the only reachable behaviour is the
// absent-row one: `MdConstant` reports ELEMENT_TYPE_VOID, `RuntimePropertyInfo.GetConstantValue`
// sees DBNull and throws. That makes this a real differential check even though no property here
// has a constant — an implementation that refused the token, or that reported anything other than
// VOID, fails it.
//
// The field case is the control: it takes the same handler down a parent that *can* have a Constant
// row, so a change that broke FieldDef while fixing PropertyDef would show up here.

public class Sample
{
    public int Number { get; set; }
    public string Text { get; set; }
    public int this[int i] { get { return i; } }

    public const int ConstField = 11;
}

public class Program
{
    public static int Main()
    {
        Type sample = typeof(Sample);

        foreach (string name in new[] { "Number", "Text", "Item" })
        {
            PropertyInfo p = sample.GetProperty(name);
            if (p == null) return 1;

            try
            {
                object v = p.GetRawConstantValue();
                return 2;
            }
            catch (InvalidOperationException)
            {
            }

            try
            {
                object v = p.GetConstantValue();
                return 3;
            }
            catch (InvalidOperationException)
            {
            }
        }

        // Control: a FieldDef parent, which does have a Constant row.
        object f = sample.GetField("ConstField").GetRawConstantValue();
        if (!(f is int)) return 4;
        if ((int)f != 11) return 5;

        return 0;
    }
}
