using System;
using System.Reflection;

namespace ModuleGetTypes
{
    public class Outer<T>
    {
        public class Inner { }
    }

    public class Plain { }

    public struct Val
    {
        public int X;
    }

    public interface IFace { }

    public enum Colour
    {
        A,
        B,
    }

    // The witness for "loading a type is not initialising it". It lives on a different
    // type from the initialiser that writes it, so that reading the witness cannot
    // itself be what triggers that initialiser.
    public static class Witness
    {
        public static bool Ran;
    }

    public class HasCctor
    {
        static HasCctor()
        {
            Witness.Ran = true;
        }

        public static int Value = 1;
    }

    public class Program
    {
        // Makes the compiler emit two TypeDef rows no source line names: a `<>c` display
        // class for the lambda, and a `<PrivateImplementationDetails>` holding a nested
        // `__StaticArrayInitTypeSize=N` for the int array's initial data. GetTypes must
        // report both.
        private static readonly Func<int, int> Lambda = x => x + 1;
        private static readonly int[] Numbers = { 1, 2, 3, 4, 5, 6, 7, 8 };

        private static Type Named(Type[] types, string name)
        {
            Type found = null;

            for (int i = 0; i < types.Length; i++)
            {
                if (types[i].Name == name)
                {
                    found = types[i];
                }
            }

            return found;
        }

        public static int Main(string[] args)
        {
            Module module = typeof(Program).Module;
            Type[] types = module.GetTypes();

            // The global `<Module>` type is TypeDef row 1, which the enumeration skips.
            for (int i = 0; i < types.Length; i++)
            {
                if (types[i].Name == "<Module>")
                {
                    return 1;
                }
            }

            // Reported in ascending TypeDef row order.
            for (int i = 1; i < types.Length; i++)
            {
                if (types[i].MetadataToken <= types[i - 1].MetadataToken)
                {
                    return 2;
                }
            }

            // Every row is reported, whatever shape it is: a generic definition, a struct,
            // an interface, an enum, a static class, a nested class, and the two the
            // compiler synthesised. An implementation that quietly dropped the rows it
            // could not load would pass every other check here.
            string[] expected =
            {
                "Outer`1",
                "Plain",
                "Val",
                "IFace",
                "Colour",
                "Witness",
                "HasCctor",
                "Program",
                "<PrivateImplementationDetails>",
                "Inner",
                "<>c",
            };

            for (int e = 0; e < expected.Length; e++)
            {
                if (Named(types, expected[e]) == null)
                {
                    return 3;
                }
            }

            // The array-data holder's own nested row, whose name carries a byte count. Found
            // through its declaring type rather than by name, so the count cannot make this
            // check stale.
            bool foundArrayInitType = false;

            for (int i = 0; i < types.Length; i++)
            {
                Type declaring = types[i].DeclaringType;

                if (declaring != null && declaring.Name == "<PrivateImplementationDetails>")
                {
                    foundArrayInitType = true;
                }
            }

            if (!foundArrayInitType)
            {
                return 4;
            }

            // The same `Type` object reflection hands out elsewhere, not a fresh one.
            bool foundPlain = false;

            for (int i = 0; i < types.Length; i++)
            {
                if (object.ReferenceEquals(types[i], typeof(Plain)))
                {
                    foundPlain = true;
                }
            }

            if (!foundPlain)
            {
                return 5;
            }

            // A generic TypeDef row is the open generic definition, not an instantiation.
            Type outer = null;

            for (int i = 0; i < types.Length; i++)
            {
                if (object.ReferenceEquals(types[i], typeof(Outer<>)))
                {
                    outer = types[i];
                }
            }

            if (outer == null)
            {
                return 6;
            }

            if (!outer.IsGenericTypeDefinition)
            {
                return 7;
            }

            // A type nested in a generic inherits its enclosing type's parameters, so it is
            // itself an open generic definition.
            Type inner = Named(types, "Inner");

            if (!inner.IsNested)
            {
                return 8;
            }

            if (!inner.IsGenericTypeDefinition)
            {
                return 9;
            }

            if (!object.ReferenceEquals(inner.DeclaringType, outer))
            {
                return 10;
            }

            // Every row agrees with what resolving its own token gives.
            for (int i = 0; i < types.Length; i++)
            {
                if (!object.ReferenceEquals(module.ResolveType(types[i].MetadataToken), types[i]))
                {
                    return 11;
                }
            }

            // Exactly one entry per row: no null padding, no repeats.
            for (int i = 0; i < types.Length; i++)
            {
                if (types[i] == null)
                {
                    return 12;
                }

                for (int j = i + 1; j < types.Length; j++)
                {
                    if (object.ReferenceEquals(types[i], types[j]))
                    {
                        return 13;
                    }
                }
            }

            // A fresh array each call, so a caller cannot mutate what the next caller sees.
            if (object.ReferenceEquals(module.GetTypes(), module.GetTypes()))
            {
                return 14;
            }

            // Loading a type is not initialising it.
            if (Witness.Ran)
            {
                return 15;
            }

            // ... and the witness above says that because no initialiser ran, not because
            // nothing would ever have written it. Touching a static of `HasCctor` runs its
            // initialiser now.
            if (HasCctor.Value != 1)
            {
                return 16;
            }

            if (!Witness.Ran)
            {
                return 17;
            }

            return 0;
        }
    }
}
