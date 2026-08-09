using System;

// `Activator.CreateInstance` on a value type that declares an explicit parameterless
// constructor (legal since C# 10).
//
// CoreCLR's `RuntimeTypeHandle_GetActivationInfo` returns *two* entry points for that ctor:
// the boxed one in `ppfnRefCtor` (receiver arrives as `object`) and the unboxed one in
// `ppfnValueCtor` (receiver arrives as `ref byte`) — the same MethodDesc reached two ways.
// `CreateInstanceDefaultCtor` then calls the boxed one. PawPrint's function-pointer value
// carries no entry-point flavour, so it cannot express the boxed entry point, and the QCall
// fails loudly rather than invoking the ctor with an ObjectRef receiver: coercing that into
// a byref `this` risks constructing into a copy of the box's payload and discarding the
// result, which would be silently wrong rather than loudly unsupported.
//
// Un-park when function pointers can name a boxed entry point.

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

    public class Program
    {
        public static int Main(string[] args)
        {
            WithParameterlessCtor v = (WithParameterlessCtor)Activator.CreateInstance(typeof(WithParameterlessCtor));

            if (v.Value != 5)
            {
                return 1;
            }

            return 0;
        }
    }
}
