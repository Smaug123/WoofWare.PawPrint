using System.Collections;

// Regression test for a crash in the `constrained.` prefix handler when T is a runtime array
// type. Array type handles (`ConcreteTypeHandle.OneDimArrayZero` / `Array`) are structural
// wrappers that aren't stored in the concrete-type mapping, so the previous implementation
// fell off `AllConcreteTypes.lookup |> Option.get` before it could take ECMA case 1.

public class Program
{
    // Compiler emits `constrained. !!T callvirt ICollection::get_Count` because T is unknown
    // at IL-compile time. At runtime T=int[], which is a reference type and should dispatch
    // as ECMA III.2.1 case 1 (dereference the byref receiver).
    private static int CountGeneric<T>(T value) where T : ICollection
    {
        return value.Count;
    }

    public static int Main(string[] args)
    {
        int[] arr = new int[] { 10, 20, 30 };
        return CountGeneric(arr) == 3 ? 0 : 1;
    }
}
