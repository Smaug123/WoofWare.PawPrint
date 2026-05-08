using System;
using System.IO;

namespace TypeGetGenericParameterConstraints
{
    class Box<T> { }

    class StructBox<T> where T : struct { }

    class RefBox<T> where T : class { }

    class StreamBox<T> where T : Stream { }

    class DisposableBox<T> where T : IDisposable { }

    class StreamDisposableBox<T> where T : Stream, IDisposable { }

    class StructDisposableBox<T> where T : struct, IDisposable { }

    class Program
    {
        static int Main(string[] args)
        {
            // Each generic parameter routes through RuntimeTypeHandle.GetConstraints; verify
            // the lengths and identities Roslyn emits in the GenericParamConstraint table —
            // including the synthetic System.ValueType row appended for `where T : struct`.

            Type unc = typeof(Box<>).GetGenericArguments()[0];
            Type[] uncCs = unc.GetGenericParameterConstraints();
            if (uncCs.Length != 0) return 1;

            Type vt = typeof(StructBox<>).GetGenericArguments()[0];
            Type[] vtCs = vt.GetGenericParameterConstraints();
            if (vtCs.Length != 1) return 2;
            if (vtCs[0] != typeof(ValueType)) return 3;

            Type rt = typeof(RefBox<>).GetGenericArguments()[0];
            Type[] rtCs = rt.GetGenericParameterConstraints();
            if (rtCs.Length != 0) return 4;

            Type stream = typeof(StreamBox<>).GetGenericArguments()[0];
            Type[] streamCs = stream.GetGenericParameterConstraints();
            if (streamCs.Length != 1) return 5;
            if (streamCs[0] != typeof(Stream)) return 6;

            Type disp = typeof(DisposableBox<>).GetGenericArguments()[0];
            Type[] dispCs = disp.GetGenericParameterConstraints();
            if (dispCs.Length != 1) return 7;
            if (dispCs[0] != typeof(IDisposable)) return 8;

            Type sd = typeof(StreamDisposableBox<>).GetGenericArguments()[0];
            Type[] sdCs = sd.GetGenericParameterConstraints();
            if (sdCs.Length != 2) return 9;
            if (sdCs[0] != typeof(Stream)) return 10;
            if (sdCs[1] != typeof(IDisposable)) return 11;

            Type sdv = typeof(StructDisposableBox<>).GetGenericArguments()[0];
            Type[] sdvCs = sdv.GetGenericParameterConstraints();
            if (sdvCs.Length != 2) return 12;
            // Roslyn writes the explicit IDisposable row first and the synthetic
            // System.ValueType row last; reflection surfaces them in that order.
            if (sdvCs[0] != typeof(IDisposable)) return 13;
            if (sdvCs[1] != typeof(ValueType)) return 14;

            return 0;
        }
    }
}
