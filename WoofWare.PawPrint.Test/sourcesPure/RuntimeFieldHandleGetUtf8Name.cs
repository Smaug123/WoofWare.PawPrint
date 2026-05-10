using System.Reflection;

class Program
{
    public static int FieldA;
    public static int FieldB;

    static int Main(string[] args)
    {
        FieldInfo fi = typeof(Program).GetField("FieldA");
        if (fi == null) return 1;
        if (fi.Name != "FieldA") return 2;

        FieldInfo fi2 = typeof(Program).GetField("FieldB");
        if (fi2 == null) return 3;
        if (fi2.Name != "FieldB") return 4;

        return 0;
    }
}
