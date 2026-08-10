using System;
using System.Reflection;

namespace TypeIsValueTypeNonRuntimeType
{
    struct MyStruct { public int X; }

    /// A `Type` subclass whose answer disagrees with the type it delegates to, so the
    /// override is the only thing that can produce it. `TypeDelegator` forwards
    /// `IsValueTypeImpl` to `typeImpl`, which here is `int` — a value type — while the
    /// override below says false.
    sealed class NeverValueType : TypeDelegator
    {
        public NeverValueType()
            : base(typeof(int))
        {
        }

        protected override bool IsValueTypeImpl() => false;
    }

    /// The mirror image: delegates to `string`, but claims to be a value type.
    sealed class AlwaysValueType : TypeDelegator
    {
        public AlwaysValueType()
            : base(typeof(string))
        {
        }

        protected override bool IsValueTypeImpl() => true;
    }

    class Program
    {
        static int Main(string[] args)
        {
            // `Type.IsValueType` is a *non-virtual* property whose `[Intrinsic]` getter body
            // is `ldarg.0; callvirt Type::IsValueTypeImpl(); ret`. The getter is therefore
            // always the call target, and the `callvirt` inside it is the only thing that
            // selects an implementation. A receiver that is not a `RuntimeType` has to answer
            // from its own override.
            //
            // This is what distinguishes `IsValueType` from `IsEnum`: `IsEnum` is itself
            // virtual and `RuntimeType` overrides the whole property, so a `callvirt` on it
            // never lands on an `[Intrinsic]` body in the first place.
            if (!new TypeDelegator(typeof(int)).IsValueType) return 1;
            if (!new TypeDelegator(typeof(MyStruct)).IsValueType) return 2;
            if (new TypeDelegator(typeof(string)).IsValueType) return 3;
            if (new TypeDelegator(typeof(object)).IsValueType) return 4;

            // These two cannot be answered from the delegated-to type's handle at all: only
            // running the override produces them. They fail for any implementation that
            // resolves the receiver to a `RuntimeType` and reads its handle, however it
            // reaches that handle.
            if (new NeverValueType().IsValueType) return 5;
            if (!new AlwaysValueType().IsValueType) return 6;

            // A `TypeDelegator` reached through the `Type`-typed static type, so the call
            // site's own metadata token names `System.Type` rather than the delegator.
            Type asType = new TypeDelegator(typeof(double));
            if (!asType.IsValueType) return 7;

            return 0;
        }
    }
}
