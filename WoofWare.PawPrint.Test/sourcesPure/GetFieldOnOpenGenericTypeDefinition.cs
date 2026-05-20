using System.Reflection;

class Program
{
    public class GenericClass<T>
    {
        public T Value;
        public static int StaticCount;
    }

    static int Main(string[] args)
    {
        // typeof(GenericClass<>).GetField goes through
        // RuntimeType.GetFieldCandidates → RuntimeTypeHandle.GetFields, which
        // dispatches the open generic type definition target down to the
        // QCall RuntimeTypeHandle_GetFields. The Closed/Concrete arm of the
        // walker is already exercised via FieldInfoGetFieldFromHandle.cs;
        // this case exercises the OpenGenericTypeDefinition arm specifically.
        System.Type openGeneric = typeof(GenericClass<>);

        FieldInfo valueField = openGeneric.GetField("Value");
        if (valueField == null) return 1;
        if (valueField.Name != "Value") return 2;
        if (valueField.IsStatic) return 3;

        FieldInfo staticCountField = openGeneric.GetField("StaticCount");
        if (staticCountField == null) return 4;
        if (staticCountField.FieldType != typeof(int)) return 5;
        if (!staticCountField.IsStatic) return 6;

        return 0;
    }
}
