using System;
using System.IO;

namespace TypeBaseTypeGenericParameter
{
    class Box<T> { }

    class StructBox<T> where T : struct { }

    class RefBox<T> where T : class { }

    class StreamBox<T> where T : Stream { }

    class DisposableBox<T> where T : IDisposable { }

    class Program
    {
        static int Main(string[] args)
        {
            // Unconstrained T: BaseType is System.Object. CoreCLR's algorithm starts the
            // walk with Object as the default and only overrides it if a non-interface,
            // non-pure-parameter constraint applies.
            Type unc = typeof(Box<>).GetGenericArguments()[0];
            if (unc.BaseType == null) return 1;
            if (!object.ReferenceEquals(unc.BaseType, typeof(object))) return 2;

            // `where T : struct`: no class constraint walks the loop, so the walk leaves
            // baseType at Object — but the post-walk struct-flag promotion overrides
            // Object to System.ValueType.
            Type vt = typeof(StructBox<>).GetGenericArguments()[0];
            if (vt.BaseType == null) return 3;
            if (!object.ReferenceEquals(vt.BaseType, typeof(ValueType))) return 4;

            // `where T : class` does *not* promote Object to anything. The reference flag
            // only short-circuits IsValueType; BaseType stays at Object.
            Type rt = typeof(RefBox<>).GetGenericArguments()[0];
            if (rt.BaseType == null) return 5;
            if (!object.ReferenceEquals(rt.BaseType, typeof(object))) return 6;

            // A direct class constraint wins. Stream is non-final, non-interface, not a
            // generic parameter; the walk sets baseType = Stream.
            Type stream = typeof(StreamBox<>).GetGenericArguments()[0];
            if (stream.BaseType == null) return 7;
            if (!object.ReferenceEquals(stream.BaseType, typeof(Stream))) return 8;

            // Interface constraints are skipped during the walk, so an interface-only
            // constraint leaves baseType = Object.
            Type disp = typeof(DisposableBox<>).GetGenericArguments()[0];
            if (disp.BaseType == null) return 9;
            if (!object.ReferenceEquals(disp.BaseType, typeof(object))) return 10;

            return 0;
        }
    }
}
