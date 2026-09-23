using System;
using System.Reflection;

// `Activator.CreateInstance` on value types that declare an explicit parameterless constructor
// (legal since C# 10).
//
// `RuntimeTypeHandle_GetActivationInfo` hands such a constructor back twice: its *boxed* entry
// point (an unboxing stub, whose receiver arrives as `object`) in `ppfnRefCtor`, and its unboxed
// entry point (receiver arrives as `ref byte`) in `ppfnValueCtor`. `CreateInstanceDefaultCtor`
// allocates a boxed `default(T)` and calls the boxed one on it, so the constructor must run
// against that box's payload: were it to run against a copy, every check below that reads a
// field back would see zero.

namespace ActivatorCreateInstanceStructCtorTest
{
    public struct WithParameterlessCtor
    {
        public int Value;

        public WithParameterlessCtor()
        {
            Value = 5;
        }
    }

    // Reads `this` before writing it, then passes `this` on to another instance method: both must
    // address the box.
    public struct ReadsThenDelegates
    {
        public int A;
        public int B;

        public ReadsThenDelegates()
        {
            A += 3;
            Bump();
        }

        private void Bump()
        {
            B = A * 7;
        }
    }

    // Each construction is numbered, so a constructor run zero times or twice is visible, as is
    // one whose writes land in some other instance.
    public struct Counted
    {
        public static int Constructions;
        public int Id;

        public Counted()
        {
            Constructions++;
            Id = Constructions;
        }
    }

    // The constructor of a generic struct, so the stub names a method on a constructed type.
    public struct Generic<T>
    {
        public string Name;

        public Generic()
        {
            Name = typeof(T).Name;
        }
    }

    // The static constructor runs before the instance constructor's body.
    public struct WithStaticCtor
    {
        public static int Initialised;
        public int Seen;

        static WithStaticCtor()
        {
            Initialised = 11;
        }

        public WithStaticCtor()
        {
            Seen = Initialised;
        }
    }

    public struct Throws
    {
        public int X;

        public Throws()
        {
            throw new InvalidOperationException("from struct ctor");
        }
    }

    public class Program
    {
        public static int Main(string[] args)
        {
            WithParameterlessCtor v = (WithParameterlessCtor)Activator.CreateInstance(typeof(WithParameterlessCtor));

            if (v.Value != 5)
            {
                return 1;
            }

            object boxed = Activator.CreateInstance(typeof(ReadsThenDelegates));
            ReadsThenDelegates r = (ReadsThenDelegates)boxed;

            if (r.A != 3 || r.B != 21)
            {
                return 2;
            }

            // Twice, so the second call goes through the cached `ActivatorCache`.
            object first = Activator.CreateInstance(typeof(Counted));
            object second = Activator.CreateInstance(typeof(Counted));

            if (Counted.Constructions != 2)
            {
                return 3;
            }

            if (((Counted)first).Id != 1 || ((Counted)second).Id != 2)
            {
                return 4;
            }

            Generic<string> g = (Generic<string>)Activator.CreateInstance(typeof(Generic<string>));

            if (g.Name != "String")
            {
                return 5;
            }

            WithStaticCtor s = (WithStaticCtor)Activator.CreateInstance(typeof(WithStaticCtor));

            if (s.Seen != 11)
            {
                return 6;
            }

            try
            {
                Activator.CreateInstance(typeof(Throws));
                return 7;
            }
            catch (TargetInvocationException e)
            {
                if (!(e.InnerException is InvalidOperationException inner) || inner.Message != "from struct ctor")
                {
                    return 8;
                }
            }

            return 0;
        }
    }
}
