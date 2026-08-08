using System;

namespace MakeGenericTypeGeneralConstraint
{
    // CoreCLR's TypeVarTypeDesc::SatisfiesConstraints checks the general
    // "must be assignable to" constraints from the GenericParamConstraint table
    // (ECMA-335 II.22.21), not just the struct/class/new() flags.
    public interface IMarker { }

    public class MyBase { }

    public class MyDerived : MyBase, IMarker { }

    public class WhereInterface<T> where T : IComparable { }

    public class WhereBase<T> where T : MyBase { }

    // The constraint mentions the parameter itself, so it can only be checked
    // after substituting the supplied argument into it.
    public class WhereSelfReferential<T> where T : IComparable<T> { }

    // Two constraints on one parameter: both must hold.
    public class WhereBoth<T> where T : MyBase, IMarker { }

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
            // Interface constraint: object does not implement IComparable.
            if (!Throws(typeof(WhereInterface<>), typeof(object))) return 1;
            // int does, as a value type satisfying the constraint unboxed.
            if (typeof(WhereInterface<>).MakeGenericType(typeof(int)) != typeof(WhereInterface<int>)) return 2;
            if (typeof(WhereInterface<>).MakeGenericType(typeof(string)) != typeof(WhereInterface<string>)) return 3;

            // Base-class constraint.
            if (!Throws(typeof(WhereBase<>), typeof(string))) return 4;
            if (typeof(WhereBase<>).MakeGenericType(typeof(MyDerived)) != typeof(WhereBase<MyDerived>)) return 5;
            // The constraint type itself always satisfies it.
            if (typeof(WhereBase<>).MakeGenericType(typeof(MyBase)) != typeof(WhereBase<MyBase>)) return 6;

            // Self-referential constraint: IComparable<T> must be instantiated with
            // the supplied argument before the cast check. Were it checked in its
            // typical (uninstantiated) form, the legal cases below would be rejected.
            if (!Throws(typeof(WhereSelfReferential<>), typeof(object))) return 7;
            if (typeof(WhereSelfReferential<>).MakeGenericType(typeof(int)) != typeof(WhereSelfReferential<int>)) return 8;
            if (typeof(WhereSelfReferential<>).MakeGenericType(typeof(string)) != typeof(WhereSelfReferential<string>)) return 9;

            // Two constraints: MyBase alone satisfies only one of them.
            if (!Throws(typeof(WhereBoth<>), typeof(MyBase))) return 10;
            if (typeof(WhereBoth<>).MakeGenericType(typeof(MyDerived)) != typeof(WhereBoth<MyDerived>)) return 11;

            return 0;
        }
    }
}
