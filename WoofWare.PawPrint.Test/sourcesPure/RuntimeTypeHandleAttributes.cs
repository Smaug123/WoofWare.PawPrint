using System;
using System.Collections.Generic;
using System.Reflection;

namespace RuntimeTypeHandleAttributes
{
    public sealed class SealedClass { }
    public abstract class AbstractClass { }
    public class PlainClass { }

    public class Program
    {
        public static int Main(string[] args)
        {
            // Type.Attributes calls RuntimeType.GetAttributeFlagsImpl, which is the
            // managed wrapper around the RuntimeTypeHandle.GetAttributes InternalCall.

            // Concrete reference type from CoreLib: System.String is sealed.
            TypeAttributes stringAttrs = typeof(string).Attributes;
            if ((stringAttrs & TypeAttributes.VisibilityMask) != TypeAttributes.Public) return 1;
            if ((stringAttrs & TypeAttributes.Sealed) == 0) return 2;

            // Concrete value type: System.Int32 is sealed (every value type is).
            TypeAttributes intAttrs = typeof(int).Attributes;
            if ((intAttrs & TypeAttributes.VisibilityMask) != TypeAttributes.Public) return 3;
            if ((intAttrs & TypeAttributes.Sealed) == 0) return 4;

            // Interface: System.IDisposable carries Interface and Abstract flags.
            TypeAttributes disposableAttrs = typeof(IDisposable).Attributes;
            if ((disposableAttrs & TypeAttributes.ClassSemanticsMask) != TypeAttributes.Interface) return 5;
            if ((disposableAttrs & TypeAttributes.Abstract) == 0) return 6;

            // Locally-defined sealed/abstract/plain classes.
            if ((typeof(SealedClass).Attributes & TypeAttributes.Sealed) == 0) return 7;
            if ((typeof(AbstractClass).Attributes & TypeAttributes.Abstract) == 0) return 8;
            if ((typeof(PlainClass).Attributes & TypeAttributes.Sealed) != 0) return 9;
            if ((typeof(PlainClass).Attributes & TypeAttributes.Abstract) != 0) return 10;

            // Open generic type definition: List<> reports Public visibility, no Sealed/Abstract.
            TypeAttributes listOpenAttrs = typeof(List<>).Attributes;
            if ((listOpenAttrs & TypeAttributes.VisibilityMask) != TypeAttributes.Public) return 11;
            if ((listOpenAttrs & TypeAttributes.Sealed) != 0) return 12;
            if ((listOpenAttrs & TypeAttributes.Abstract) != 0) return 13;

            // Generic type parameter: a TypeDesc in CoreCLR. CoreCLR returns tdPublic only.
            Type tParam = typeof(List<>).GetGenericArguments()[0];
            if (tParam.Attributes != TypeAttributes.Public) return 14;

            // Single-dim zero-based array: not a TypeDesc; reports Public | Sealed | Serializable.
            TypeAttributes intArrAttrs = typeof(int[]).Attributes;
            if ((intArrAttrs & TypeAttributes.VisibilityMask) != TypeAttributes.Public) return 15;
            if ((intArrAttrs & TypeAttributes.Sealed) == 0) return 16;
            if ((intArrAttrs & TypeAttributes.Serializable) == 0) return 17;

            // Multi-dim array: same shape as 1D array.
            TypeAttributes int2DArrAttrs = typeof(int[,]).Attributes;
            if ((int2DArrAttrs & TypeAttributes.Sealed) == 0) return 18;
            if ((int2DArrAttrs & TypeAttributes.Serializable) == 0) return 19;

            return 0;
        }
    }
}
