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
    // The bit is not represented in PawPrint's decoded `GenericParamMetadata.Constraint`, so
    // this case verifies the intrinsic surfaces the raw GenericParam.Flags rather than
    // recomposing from named fields and silently dropping bits the metadata recorded.
    class AllowsRefStructBox<T> where T : allows ref struct { }

    interface ICovariantOut<out T> { }

    interface IContravariantIn<in T> { }

    class Program
    {
        static int Main(string[] args)
        {
            // RuntimeType.GenericParameterAttributes returns the raw GenericParam.Flags
            // bitmask the metadata reader recorded. Each test below pins one flag combination
            // we observed Roslyn emit and CoreCLR surface; if any of these drift the
            // intercept has bugged out.
            //
            // Bit layout (ECMA-335 §II.23.1.7, mirrored in System.Reflection.GenericParameterAttributes):
            //   None                              = 0x0000
            //   Covariant                         = 0x0001
            //   Contravariant                     = 0x0002
            //   ReferenceTypeConstraint           = 0x0004
            //   NotNullableValueTypeConstraint    = 0x0008
            //   DefaultConstructorConstraint      = 0x0010

            Type unc = typeof(Box<>).GetGenericArguments()[0];
            if ((int)unc.GenericParameterAttributes != 0x0) return 1;

            // `where T : struct` sets BOTH NotNullableValueTypeConstraint and
            // DefaultConstructorConstraint — every struct has an implicit parameterless
            // ctor, and Roslyn signals that by setting both flags simultaneously.
            Type vt = typeof(StructBox<>).GetGenericArguments()[0];
            if ((int)vt.GenericParameterAttributes != 0x18) return 2;
            if ((vt.GenericParameterAttributes & GenericParameterAttributes.NotNullableValueTypeConstraint) == 0) return 3;
            if ((vt.GenericParameterAttributes & GenericParameterAttributes.DefaultConstructorConstraint) == 0) return 4;

            // `where T : class` sets only ReferenceTypeConstraint.
            Type rt = typeof(RefBox<>).GetGenericArguments()[0];
            if ((int)rt.GenericParameterAttributes != 0x4) return 5;

            // `where T : new()` standalone sets only DefaultConstructorConstraint.
            Type nb = typeof(NewBox<>).GetGenericArguments()[0];
            if ((int)nb.GenericParameterAttributes != 0x10) return 6;

            // `where T : class, new()` sets both.
            Type rnb = typeof(RefNewBox<>).GetGenericArguments()[0];
            if ((int)rnb.GenericParameterAttributes != 0x14) return 7;

            // A non-flag base-type constraint emits no flag bits; the constraint lives
            // in the GenericParamConstraint table only.
            Type stream = typeof(StreamBox<>).GetGenericArguments()[0];
            if ((int)stream.GenericParameterAttributes != 0x0) return 8;

            // Adding `, new()` to a base-type constraint sets only DefaultConstructorConstraint.
            Type sn = typeof(StreamNewBox<>).GetGenericArguments()[0];
            if ((int)sn.GenericParameterAttributes != 0x10) return 9;

            // Interface-only constraint — same as base-type: no flag bits.
            Type disp = typeof(DisposableBox<>).GetGenericArguments()[0];
            if ((int)disp.GenericParameterAttributes != 0x0) return 10;

            // `where T : struct, IDisposable` keeps the same struct flag bits as plain `struct`.
            Type sd = typeof(StructDisposableBox<>).GetGenericArguments()[0];
            if ((int)sd.GenericParameterAttributes != 0x18) return 11;

            // `where T : unmanaged` decomposes (per Roslyn) into the same struct flag bits
            // plus a modreq-tagged ValueType TypeSpec constraint. Only the flag bits are
            // visible via GenericParameterAttributes.
            Type um = typeof(UnmanagedBox<>).GetGenericArguments()[0];
            if ((int)um.GenericParameterAttributes != 0x18) return 12;

            // `out T` on an interface sets Covariant.
            Type cov = typeof(ICovariantOut<>).GetGenericArguments()[0];
            if ((int)cov.GenericParameterAttributes != 0x1) return 13;

            // `in T` on an interface sets Contravariant.
            Type con = typeof(IContravariantIn<>).GetGenericArguments()[0];
            if ((int)con.GenericParameterAttributes != 0x2) return 14;

            // `where T : allows ref struct` sets the AllowByRefLike (0x20) bit. PawPrint
            // doesn't decode this bit into a named `GenericParamMetadata` field, so the
            // intrinsic must surface the raw GenericParam.Flags to round-trip it.
            Type ars = typeof(AllowsRefStructBox<>).GetGenericArguments()[0];
            if ((int)ars.GenericParameterAttributes != 0x20) return 15;
            if ((ars.GenericParameterAttributes & GenericParameterAttributes.AllowByRefLike) == 0) return 16;

            return 0;
        }
    }
}
