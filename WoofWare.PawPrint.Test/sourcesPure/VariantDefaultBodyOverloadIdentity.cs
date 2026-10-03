// A MethodImpl implements the interface method it names, identified as a method and not by the
// signature an instantiation gives it. `J<object>` overrides both `I<object>.M(T)` and
// `I<object>.M(object)`, whose signatures coincide at `T = object`; a call through `I<string>`
// reaches them by variance, and `M(object)` must still run the override of `M(object)` alone.

public interface I<in T>
{
    int M(T value) => 1;
    int M(object value) => 2;
}

public interface J<T> : I<T>
{
    int I<T>.M(T value) => 10;
    int I<T>.M(object value) => 20;
}

public class C : J<object> { }

public static class Program
{
    public static int Main(string[] argv)
    {
        I<string> viaVariance = new C();
        if (viaVariance.M((object)"x") != 20) return 1;
        if (viaVariance.M("x") != 10) return 2;
        I<object> exact = new C();
        if (exact.M((object)"x") != 20) return 3;
        return 0;
    }
}
