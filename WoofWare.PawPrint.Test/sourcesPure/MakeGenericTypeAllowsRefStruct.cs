using System;

namespace MakeGenericTypeAllowsRefStruct
{
    public interface IMarker { }

    public ref struct MyRefStruct
    {
        public int X;
    }

    public ref struct MarkedRefStruct : IMarker
    {
        public int X;
    }

    public struct NotARefStruct : IMarker
    {
        public int X;
    }

    public class Ordinary<T> { }

    public class AllowsRef<T> where T : allows ref struct { }

    // A ref struct may implement an interface (C# 13), so a general constraint must still be
    // checked against one. This is the only route by which a byref-like type reaches the
    // assignability relation: `castclass` cannot, because a ref struct cannot be boxed.
    public class AllowsRefWithInterface<T> where T : IMarker, allows ref struct { }

    public class Program
    {
        private static bool Throws(Type openGeneric, Type arg)
        {
            try
            {
                openGeneric.MakeGenericType(arg);
                return false;
            }
            catch (ArgumentException)
            {
                return true;
            }
        }

        public static int Main(string[] args)
        {
            // gpAllowByRefLike: a byref-like argument is rejected unless the parameter
            // carries the flag (typedesc.cpp SatisfiesConstraints).
            if (!Throws(typeof(Ordinary<>), typeof(MyRefStruct))) return 1;
            if (typeof(AllowsRef<>).MakeGenericType(typeof(MyRefStruct)) != typeof(AllowsRef<MyRefStruct>)) return 2;

            // Non-byref-like arguments are unaffected by the flag either way.
            if (typeof(Ordinary<>).MakeGenericType(typeof(int)) != typeof(Ordinary<int>)) return 3;
            if (typeof(AllowsRef<>).MakeGenericType(typeof(int)) != typeof(AllowsRef<int>)) return 4;

            // `allows ref struct` widens what is accepted; it does not switch off the general
            // constraints sharing the parameter. A ref struct implementing the interface is
            // accepted, one that does not is refused, and so is a non-ref struct that does not.
            if (typeof(AllowsRefWithInterface<>).MakeGenericType(typeof(MarkedRefStruct))
                != typeof(AllowsRefWithInterface<MarkedRefStruct>)) return 5;
            if (!Throws(typeof(AllowsRefWithInterface<>), typeof(MyRefStruct))) return 6;
            if (typeof(AllowsRefWithInterface<>).MakeGenericType(typeof(NotARefStruct))
                != typeof(AllowsRefWithInterface<NotARefStruct>)) return 7;
            if (!Throws(typeof(AllowsRefWithInterface<>), typeof(object))) return 8;

            return 0;
        }
    }
}
