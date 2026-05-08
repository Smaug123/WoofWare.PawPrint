using System;
using System.IO;

namespace TypeIsInterfaceGenericParameter
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
            // RuntimeType.get_IsInterface short-circuits via TypeHandle.IsTypeDesc.
            // Generic parameters are TypeVarTypeDesc in CoreCLR — i.e. they ARE
            // type-descs, so IsInterface must return false without ever consulting
            // the (non-existent) MethodTable->Flags. Exercise this for every flavor
            // of constraint we plan to support in subsequent BaseType stages.
            Type unc = typeof(Box<>).GetGenericArguments()[0];
            if (unc.IsInterface) return 1;

            Type vt = typeof(StructBox<>).GetGenericArguments()[0];
            if (vt.IsInterface) return 2;

            Type rt = typeof(RefBox<>).GetGenericArguments()[0];
            if (rt.IsInterface) return 3;

            Type stream = typeof(StreamBox<>).GetGenericArguments()[0];
            if (stream.IsInterface) return 4;

            Type disp = typeof(DisposableBox<>).GetGenericArguments()[0];
            if (disp.IsInterface) return 5;

            return 0;
        }
    }
}
