using System;

// The *non-generic* `Activator.CreateInstance(Type)` and friends, which bottom out in
// `RuntimeType.CreateInstanceDefaultCtor` -> `RuntimeType.ActivatorCache` -> the
// `RuntimeTypeHandle_GetActivationInfo` QCall. The QCall hands back function pointers that
// ActivatorCache then invokes via `calli`, one of which is the runtime's own `newobj`
// allocation helper rather than any managed method.
//
// The sibling `ActivatorCreateInstance.cs` covers the *generic* `Activator.CreateInstance<T>()`,
// which is a `[Intrinsic]` PawPrint services inline and which never reaches this QCall.
//
// This sweeps the classification CoreCLR's `ValidateTypeAbleToBeInstantiated` performs, one
// row per (type shape x constructor shape). The real runtime is the oracle: every check below
// is an assertion that must hold on .NET too, so a wrong expectation here fails the test on
// the real runtime rather than being silently agreed with by both.
//
// Each check returns 0 when it holds; `Main` returns the number of the first that does not, so
// a failure names the row. Nothing is printed: this fixture compares exit codes only.

namespace ActivatorCreateInstanceNonGenericTest
{
    public class ImplicitCtor
    {
        public int Value = 7;
    }

    public class ExplicitPublicCtor
    {
        public int Value;

        public ExplicitPublicCtor()
        {
            Value = 11;
        }
    }

    public class PrivateCtor
    {
        public int Value;

        private PrivateCtor()
        {
            Value = 13;
        }
    }

    // CoreCLR reports `ctorIsPublic` as exactly `mdPublic`, so every non-public accessibility
    // must behave like the private one. Without these, a predicate along the lines of
    // "not private" would satisfy the private/public pair above and silently activate an
    // internal-ctor type that the real runtime refuses.
    public class InternalCtor
    {
        public int Value;

        internal InternalCtor()
        {
            Value = 17;
        }
    }

    public class ProtectedCtor
    {
        public int Value;

        protected ProtectedCtor()
        {
            Value = 19;
        }
    }

    public class NoParameterlessCtor
    {
        public int Value;

        public NoParameterlessCtor(int value)
        {
            Value = value;
        }
    }

    public abstract class AbstractClass
    {
    }

    public interface IInterface
    {
    }

    public delegate void SomeDelegate();

    public struct PlainStruct
    {
        public int X;
        public long Y;
    }

    public struct NestedStruct
    {
        public PlainStruct Inner;
        public byte Tag;
    }

    // Byref-like types. The activation QCall deliberately permits these — CoreCLR passes
    // `allowByRefLike: true` — and the rejection happens later, in managed
    // `RuntimeType.CreateInstanceDefaultCtor`, which reads the MethodTable's `IsByRefLike` flag.
    public ref struct RefStruct
    {
        public int X;
    }

    public ref struct RefStructWithSpan
    {
        public Span<int> Items;
        public int N;
    }

    public ref struct GenericRefStruct<T>
    {
        public int X;
    }

    public enum SomeEnum
    {
        Zero = 0,
        One = 1,
    }

    public class ThrowingCtor
    {
        public ThrowingCtor()
        {
            throw new InvalidOperationException("boom");
        }
    }

    public class GenericHolder<T>
    {
        public T Item;
    }

    public static class CctorWitness
    {
        public static int Ran;
    }

    // An explicit static constructor makes this type NOT `beforefieldinit`, so "the cctor has
    // not run" is a deterministic fact on both runtimes rather than a scheduling accident.
    // The struct has no instance constructor, so activation is allocator-plus-no-op-stub and
    // never touches the type initialiser: `GetActivationInfo` is documented not to run it.
    public struct StructWithCctor
    {
        public int X;

        static StructWithCctor()
        {
            CctorWitness.Ran = 1;
        }
    }

    // The other half of the cctor story. Where a type *does* have a constructor, that ctor is
    // reached through `calli`, whose managed arm runs `loadClass` on the callee's declaring
    // type, so the type initialiser must have run by the time the ctor body executes.
    //
    // The side channel is what makes this load-bearing. The obvious shape — cctor sets a static
    // on the type itself, ctor reads it — passes even if the `calli` never initialises anything,
    // because the ctor body's own `ldsfld` triggers the initialiser on its way past. Verified by
    // mutation: that version survives removing `loadClass` from `executeCalli` entirely. Routing
    // the observation through a *different* class means reading it initialises that class, not
    // this one, so the only thing that can have run this cctor first is the call path.
    public static class CtorCctorSideChannel
    {
        public static int Value;
    }

    public class ClassWithCctor
    {
        public int Observed;

        static ClassWithCctor()
        {
            CtorCctorSideChannel.Value = 41;
        }

        public ClassWithCctor()
        {
            Observed = CtorCctorSideChannel.Value;
        }
    }

    public class Program
    {
        private static string Classify(Func<object> f)
        {
            try
            {
                object o = f();
                return o == null ? "<null>" : o.GetType().Name;
            }
            catch (Exception e)
            {
                return e.GetType().Name;
            }
        }

        public static int Main(string[] args)
        {
            // --- reference types ---

            if (Classify(() => Activator.CreateInstance(typeof(ImplicitCtor))) != "ImplicitCtor")
            {
                return 1;
            }

            if (((ImplicitCtor)Activator.CreateInstance(typeof(ImplicitCtor))).Value != 7)
            {
                return 2;
            }

            // The constructor really runs: the field initialiser above would give 7 even
            // without it, so this row uses a type whose value only appears from ctor code.
            if (((ExplicitPublicCtor)Activator.CreateInstance(typeof(ExplicitPublicCtor))).Value != 11)
            {
                return 3;
            }

            // A private parameterless ctor is *found* by the runtime, and reported as
            // non-public; the public-only overload throws while the non-public one succeeds.
            // Without both halves, a `ctorIsPublic` that was inverted or hardwired to one
            // answer would still satisfy every other row here.
            if (Classify(() => Activator.CreateInstance(typeof(PrivateCtor))) != "MissingMethodException")
            {
                return 4;
            }

            if (Classify(() => Activator.CreateInstance(typeof(PrivateCtor), true)) != "PrivateCtor")
            {
                return 5;
            }

            if (((PrivateCtor)Activator.CreateInstance(typeof(PrivateCtor), true)).Value != 13)
            {
                return 6;
            }

            // `internal` and `protected` are not public either. CoreCLR's `ctorIsPublic` is
            // exactly `mdPublic`, so these must behave like the private case, not like the
            // public one.
            if (Classify(() => Activator.CreateInstance(typeof(InternalCtor))) != "MissingMethodException")
            {
                return 7;
            }

            if (((InternalCtor)Activator.CreateInstance(typeof(InternalCtor), true)).Value != 17)
            {
                return 8;
            }

            if (Classify(() => Activator.CreateInstance(typeof(ProtectedCtor))) != "MissingMethodException")
            {
                return 9;
            }

            if (((ProtectedCtor)Activator.CreateInstance(typeof(ProtectedCtor), true)).Value != 19)
            {
                return 10;
            }

            if (Classify(() => Activator.CreateInstance(typeof(NoParameterlessCtor))) != "MissingMethodException")
            {
                return 11;
            }

            // --- rejected shapes ---

            if (Classify(() => Activator.CreateInstance(typeof(AbstractClass))) != "MissingMethodException")
            {
                return 12;
            }

            if (Classify(() => Activator.CreateInstance(typeof(IInterface))) != "MissingMethodException")
            {
                return 13;
            }

            // A delegate type is rejected by its own check, which runs *before* the abstract
            // check and throws a different exception...
            if (Classify(() => Activator.CreateInstance(typeof(SomeDelegate))) != "ArgumentException")
            {
                return 14;
            }

            // ...but `MulticastDelegate` itself is not a delegate type (the runtime flag is set
            // only for types whose immediate base is MulticastDelegate), so it falls through to
            // the abstract check. This pair is what pins the classifier to "immediate base"
            // rather than "assignable to Delegate".
            if (Classify(() => Activator.CreateInstance(typeof(MulticastDelegate))) != "MissingMethodException")
            {
                return 15;
            }

            if (Classify(() => Activator.CreateInstance(typeof(Delegate))) != "MissingMethodException")
            {
                return 16;
            }

            if (Classify(() => Activator.CreateInstance(typeof(string))) != "MissingMethodException")
            {
                return 17;
            }

            if (Classify(() => Activator.CreateInstance(typeof(int[]))) != "MissingMethodException")
            {
                return 18;
            }

            // --- value types: the allocator returns a boxed default(T) ---

            if (Classify(() => Activator.CreateInstance(typeof(PlainStruct))) != "PlainStruct")
            {
                return 19;
            }

            // Unboxing the activated instance is what checks the box's *shape*, not just its
            // type: a boxed value the unboxer cannot read back would fail here.
            PlainStruct plain = (PlainStruct)Activator.CreateInstance(typeof(PlainStruct));

            if (plain.X != 0 || plain.Y != 0L)
            {
                return 20;
            }

            NestedStruct nested = (NestedStruct)Activator.CreateInstance(typeof(NestedStruct));

            if (nested.Inner.X != 0 || nested.Inner.Y != 0L || nested.Tag != 0)
            {
                return 21;
            }

            // A bare primitive: `box default(int)` stores the value inside a synthetic
            // single-field struct, so this row covers a different box shape from the two above.
            if (Classify(() => Activator.CreateInstance(typeof(int))) != "Int32")
            {
                return 22;
            }

            if ((int)Activator.CreateInstance(typeof(int)) != 0)
            {
                return 23;
            }

            if ((SomeEnum)Activator.CreateInstance(typeof(SomeEnum)) != SomeEnum.Zero)
            {
                return 24;
            }

            // --- Nullable<T> is the one type whose allocator is null ---

            if (Activator.CreateInstance(typeof(int?)) != null)
            {
                return 25;
            }

            if (Activator.CreateInstance(typeof(PlainStruct?)) != null)
            {
                return 26;
            }

            // --- byref-like types are rejected by *managed* code, not by the QCall ---

            // The unmanaged activation layer deliberately lets a ref struct through
            // (`allowByRefLike: true`); `CreateInstanceDefaultCtor` is what throws, and it does
            // so by reading `RuntimeType.IsByRefLike`, which is a MethodTable *flag*. Nothing
            // else on this path consults it, so if the flag is not projected the guard simply
            // passes and the caller is handed a boxed ref struct — which is not a legal heap
            // representation at all. That makes this row the only thing standing between the
            // allocator and a silently illegal object.
            if (Classify(() => Activator.CreateInstance(typeof(RefStruct))) != "NotSupportedException")
            {
                return 27;
            }

            // The same, for a ref struct that holds a reference: the flag has to come from the
            // type's own metadata rather than being inferred from its field shape.
            if (Classify(() => Activator.CreateInstance(typeof(RefStructWithSpan))) != "NotSupportedException")
            {
                return 28;
            }

            // A generic ref struct is byref-like both closed and open. The open form matters
            // because it is a different MethodTable with no `ConcreteType` behind it, so the flag
            // has to be projected from the type definition rather than from an instantiation.
            if (Classify(() => Activator.CreateInstance(typeof(GenericRefStruct<int>))) != "NotSupportedException")
            {
                return 29;
            }

            if (!typeof(GenericRefStruct<>).IsByRefLike)
            {
                return 30;
            }

            // ... and the flag must not leak onto types that are not byref-like at all, which is
            // what stops "project it for everything" from passing the rows above.
            if (typeof(PlainStruct).IsByRefLike)
            {
                return 31;
            }

            if (typeof(ImplicitCtor).IsByRefLike)
            {
                return 32;
            }

            // --- generics and exception wrapping ---

            if (Classify(() => Activator.CreateInstance(typeof(GenericHolder<int>))) != "GenericHolder`1")
            {
                return 33;
            }

            if (Classify(() => Activator.CreateInstance(typeof(ThrowingCtor))) != "TargetInvocationException")
            {
                return 34;
            }

            // --- the activation cache is reused, and the cached pointers still work ---

            if (((ImplicitCtor)Activator.CreateInstance(typeof(ImplicitCtor))).Value != 7)
            {
                return 35;
            }

            // --- allocation does not run the type initialiser ---

            if (CctorWitness.Ran != 0)
            {
                return 36;
            }

            StructWithCctor withCctor = (StructWithCctor)Activator.CreateInstance(typeof(StructWithCctor));

            if (withCctor.X != 0)
            {
                return 37;
            }

            // ... but where a constructor *is* called, its type initialiser must have run first,
            // so the ctor observes the initialised static rather than its default.
            if (((ClassWithCctor)Activator.CreateInstance(typeof(ClassWithCctor))).Observed != 41)
            {
                return 38;
            }

            if (CctorWitness.Ran != 0)
            {
                return 39;
            }

            return 0;
        }
    }
}
