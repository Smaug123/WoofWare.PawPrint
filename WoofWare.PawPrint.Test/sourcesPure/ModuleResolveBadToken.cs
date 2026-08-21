using System;
using System.Reflection;

namespace ModuleResolveBadToken
{
    public class Plain { }

    public class Program
    {
        /// The exact runtime type thrown, or null if the call returned. `ArgumentOutOfRangeException`
        /// derives from `ArgumentException`, so every check below compares the exact type: a
        /// `catch (ArgumentException)` cannot tell the two apart, and an implementation that
        /// answered `ArgumentOutOfRangeException` for everything would otherwise look right.
        private static Type ThrownByResolveType(Module module, int token)
        {
            try
            {
                module.ResolveType(token);
                return null;
            }
            catch (Exception e)
            {
                return e.GetType();
            }
        }

        private static Type ThrownByResolveField(Module module, int token)
        {
            try
            {
                module.ResolveField(token);
                return null;
            }
            catch (Exception e)
            {
                return e.GetType();
            }
        }

        public static int Main(string[] args)
        {
            Module module = typeof(Program).Module;

            Type outOfRange = typeof(ArgumentOutOfRangeException);
            Type argument = typeof(ArgumentException);

            // A row past the end of the TypeDef table, and rid 0, which is the nil token.
            if (ThrownByResolveType(module, 0x02000999) != outOfRange)
            {
                return 1;
            }

            if (ThrownByResolveType(module, 0x02000000) != outOfRange)
            {
                return 2;
            }

            // The same for the other two token kinds `ResolveType` accepts.
            if (ThrownByResolveType(module, 0x01000999) != outOfRange)
            {
                return 3;
            }

            if (ThrownByResolveType(module, 0x1B000999) != outOfRange)
            {
                return 4;
            }

            // The global `<Module>` type's own token, and a token from a table that holds no
            // types at all. Both are refused before any table lookup, so both are a plain
            // `ArgumentException` -- not the out-of-range one.
            if (ThrownByResolveType(module, 0x02000001) != argument)
            {
                return 5;
            }

            if (ThrownByResolveType(module, 0x06000001) != argument)
            {
                return 6;
            }

            // The screen has not become a blanket refusal.
            if (!object.ReferenceEquals(module.ResolveType(0x02000000 | 2), typeof(Plain)))
            {
                return 7;
            }

            // `ResolveField` asks "is this token valid in this module" about the raw token,
            // before screening its kind, so these reach the validity check with token kinds
            // `ResolveType` can never present.
            //
            // A Module-table token at row 5: that table has exactly one row, so out of range.
            if (ThrownByResolveField(module, 0x00000005) != outOfRange)
            {
                return 8;
            }

            // Row 1 of that same table exists, so the token *is* valid -- and `ResolveField`
            // then refuses it for being the wrong kind. This is what stops the validity check
            // being satisfiable by always answering "invalid".
            if (ThrownByResolveField(module, 0x00000001) != argument)
            {
                return 9;
            }

            // A Constant-table token. The table is real and this row may well exist, but the
            // runtime's validity check does not admit that table at all, so the answer is
            // "invalid" regardless.
            if (ThrownByResolveField(module, 0x0B000001) != outOfRange)
            {
                return 10;
            }

            return 0;
        }
    }
}
