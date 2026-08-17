using System;
using System.IO;

namespace MethodGetGenericParameterConstraints
{
    class Program
    {
        public static void Unconstrained<T>() { }

        public static void Struct<T>() where T : struct { }

        public static void Class<T>() where T : class { }

        public static void New<T>() where T : new() { }

        public static void StreamDisposable<T>() where T : Stream, IDisposable { }

        public static void StructDisposable<T>() where T : struct, IDisposable { }

        public static void Unmanaged<T>() where T : unmanaged { }

        public static void UnmanagedDisposable<T>() where T : unmanaged, IDisposable { }

        public static void MvarToMvar<T, U>() where T : U { }

        public static void ConstrainedFirstOnly<T, U>() where T : IDisposable { }

        static Type Param(string name, int index) =>
            typeof(Program).GetMethod(name).GetGenericArguments()[index];

        static Type[] Constraints(string name, int index) =>
            Param(name, index).GetGenericParameterConstraints();

        static int Main(string[] args)
        {
            // The method-level counterpart of TypeGetGenericParameterConstraints.cs. Every
            // parameter here belongs to a *method*, so its GenericParamConstraint rows hang off
            // the enclosing MethodDef rather than off a TypeDef; reflection reaches them through
            // the same `RuntimeTypeHandle.GetConstraints` QCall, which has to find the owning
            // parameter list by way of the method rather than the declaring type.

            if (Constraints("Unconstrained", 0).Length != 0) return 1;

            // `where T : struct` is a flag plus a synthetic System.ValueType row. The metadata
            // reader drops that row and the runtime re-adds it, so exactly one comes back.
            Type[] st = Constraints("Struct", 0);
            if (st.Length != 1) return 2;
            if (st[0] != typeof(ValueType)) return 3;

            // `class` and `new()` are flags with no constraint rows at all.
            if (Constraints("Class", 0).Length != 0) return 4;
            if (Constraints("New", 0).Length != 0) return 5;

            // Two ordinary rows, surfaced in metadata order.
            Type[] sd = Constraints("StreamDisposable", 0);
            if (sd.Length != 2) return 6;
            if (sd[0] != typeof(Stream)) return 7;
            if (sd[1] != typeof(IDisposable)) return 8;

            // `struct, IDisposable`: Roslyn writes the explicit row first and the synthetic
            // ValueType row last, so the re-added ValueType belongs at the end. An
            // implementation that prepends it instead fails here.
            Type[] sdv = Constraints("StructDisposable", 0);
            if (sdv.Length != 2) return 9;
            if (sdv[0] != typeof(IDisposable)) return 10;
            if (sdv[1] != typeof(ValueType)) return 11;

            // `unmanaged` encodes ValueType as a TypeSpec under an `IsUnmanaged` modreq, which
            // the reader's synthetic-row filter does not recognise. That row therefore survives
            // with its own position — first — and must not be doubled by the re-add. Together
            // with `Struct` above, this pins both halves of the de-duplication: dropping the
            // re-add fails check 2, dropping the de-duplication check fails check 12.
            Type[] um = Constraints("Unmanaged", 0);
            if (um.Length != 1) return 12;
            if (um[0] != typeof(ValueType)) return 13;

            Type[] umd = Constraints("UnmanagedDisposable", 0);
            if (umd.Length != 2) return 14;
            if (umd[0] != typeof(ValueType)) return 15;
            if (umd[1] != typeof(IDisposable)) return 16;

            // A constraint that *is* another parameter of the same method. This is the shape only
            // a method can produce: `!!1` in a constraint blob, which has no meaning without a
            // method context, and which the type-parameter path refuses outright.
            Type mt = Param("MvarToMvar", 0);
            Type mu = Param("MvarToMvar", 1);
            Type[] mc = mt.GetGenericParameterConstraints();
            if (mc.Length != 1) return 17;
            if (mc[0] != mu) return 18;
            if (!mc[0].IsGenericParameter) return 19;
            if (mc[0].GenericParameterPosition != 1) return 20;
            // U itself is unconstrained: the constraint belongs to T alone.
            if (mu.GetGenericParameterConstraints().Length != 0) return 21;

            // Two parameters of one method that disagree, so an implementation that ignores the
            // parameter's position and always reads the method's first parameter cannot pass.
            if (Constraints("ConstrainedFirstOnly", 0).Length != 1) return 22;
            if (Constraints("ConstrainedFirstOnly", 0)[0] != typeof(IDisposable)) return 23;
            if (Constraints("ConstrainedFirstOnly", 1).Length != 0) return 24;

            // Base types: the constraint list exists to be consumed by `RuntimeType.GetBaseType`,
            // which walks it looking for the most specific non-interface entry.
            if (Param("Unconstrained", 0).BaseType != typeof(object)) return 25;
            if (Param("Struct", 0).BaseType != typeof(ValueType)) return 26;
            if (Param("StreamDisposable", 0).BaseType != typeof(Stream)) return 27;
            // An interface-only constraint leaves the base type alone...
            if (Param("ConstrainedFirstOnly", 0).BaseType != typeof(object)) return 28;
            // ... and so does a constraint that is an unconstrained parameter.
            if (Param("MvarToMvar", 0).BaseType != typeof(object)) return 29;

            // A fresh array each call, holding the same Type objects.
            Type[] again = Constraints("StreamDisposable", 0);
            if (ReferenceEquals(again, sd)) return 30;
            if (!ReferenceEquals(again[0], sd[0])) return 31;

            return 0;
        }
    }
}
