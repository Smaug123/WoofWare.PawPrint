using System;
using System.IO;
using System.Reflection;

namespace TypeGenericParameterAttributes
{
    class Box<T> { }

    class StructBox<T> where T : struct { }

    class RefBox<T> where T : class { }

    class NewBox<T> where T : new() { }

    class RefNewBox<T> where T : class, new() { }

    class StreamBox<T> where T : Stream { }

    class StreamNewBox<T> where T : Stream, new() { }

    class DisposableBox<T> where T : IDisposable { }

    class StructDisposableBox<T> where T : struct, IDisposable { }

    class UnmanagedBox<T> where T : unmanaged { }

    // C# 13 / .NET 9 `allows ref struct` sets GenericParameterAttributes.AllowByRefLike (0x20).
    class AllowsRefStructBox<T> where T : allows ref struct { }

    interface ICovariantOut<out T> { }

    interface IContravariantIn<in T> { }

    class Program
    {
        static int Main(string[] args)
        {
            // Unconstrained: None (0x0)
            Type unc = typeof(Box<>).GetGenericArguments()[0];
            if ((int)unc.GenericParameterAttributes != 0x0) return 1;

            // `where T : struct` sets NotNullableValueTypeConstraint (0x08) |
            // DefaultConstructorConstraint (0x10) = 0x18.
            Type vt = typeof(StructBox<>).GetGenericArguments()[0];
            if ((int)vt.GenericParameterAttributes != 0x18) return 2;
            if ((vt.GenericParameterAttributes & GenericParameterAttributes.NotNullableValueTypeConstraint) == 0) return 3;
            if ((vt.GenericParameterAttributes & GenericParameterAttributes.DefaultConstructorConstraint) == 0) return 4;

            // `where T : class` sets ReferenceTypeConstraint (0x04).
            Type rt = typeof(RefBox<>).GetGenericArguments()[0];
            if ((int)rt.GenericParameterAttributes != 0x04) return 5;

            // `where T : new()` sets DefaultConstructorConstraint (0x10) only.
            Type nb = typeof(NewBox<>).GetGenericArguments()[0];
            if ((int)nb.GenericParameterAttributes != 0x10) return 6;

            // `where T : class, new()` sets both (0x14).
            Type rnb = typeof(RefNewBox<>).GetGenericArguments()[0];
            if ((int)rnb.GenericParameterAttributes != 0x14) return 7;

            // A base-type constraint emits no flag bits; the constraint lives in the
            // GenericParamConstraint table only.
            Type stream = typeof(StreamBox<>).GetGenericArguments()[0];
            if ((int)stream.GenericParameterAttributes != 0x0) return 8;

            // Base-type + new() sets only DefaultConstructorConstraint.
            Type sn = typeof(StreamNewBox<>).GetGenericArguments()[0];
            if ((int)sn.GenericParameterAttributes != 0x10) return 9;

            // Interface-only constraint: no flag bits.
            Type disp = typeof(DisposableBox<>).GetGenericArguments()[0];
            if ((int)disp.GenericParameterAttributes != 0x0) return 10;

            // `where T : struct, IDisposable` keeps the struct flags.
            Type sd = typeof(StructDisposableBox<>).GetGenericArguments()[0];
            if ((int)sd.GenericParameterAttributes != 0x18) return 11;

            // `where T : unmanaged` has the same struct flag bits (0x18).
            Type um = typeof(UnmanagedBox<>).GetGenericArguments()[0];
            if ((int)um.GenericParameterAttributes != 0x18) return 12;

            // `out T` on an interface sets Covariant (0x01).
            Type cov = typeof(ICovariantOut<>).GetGenericArguments()[0];
            if ((int)cov.GenericParameterAttributes != 0x01) return 13;

            // `in T` on an interface sets Contravariant (0x02).
            Type con = typeof(IContravariantIn<>).GetGenericArguments()[0];
            if ((int)con.GenericParameterAttributes != 0x02) return 14;

            // `allows ref struct` sets AllowByRefLike (0x20).
            Type ars = typeof(AllowsRefStructBox<>).GetGenericArguments()[0];
            if ((int)ars.GenericParameterAttributes != 0x20) return 15;
            if ((ars.GenericParameterAttributes & GenericParameterAttributes.AllowByRefLike) == 0) return 16;

            return 0;
        }
    }
}
