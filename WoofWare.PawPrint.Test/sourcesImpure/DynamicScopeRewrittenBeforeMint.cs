using System;
using System.Collections.Generic;
using System.Reflection;
using System.Reflection.Emit;

public class Program
{
    // `DynamicMethod.CreateDelegate` does not compile the body, so a guest that rewrites
    // `DynamicILGenerator.m_scope.m_tokens` through private reflection *before* `CreateDelegate`
    // decides what each token names exactly as one that rewrites it afterwards: `CreateDelegate`
    // succeeds, and the first invocation raises whatever the scope then holds warrants. A slot
    // broken before `CreateDelegate` and repaired before the first invocation is a method that
    // runs. Every expected answer, including each exception and its message, was measured on real
    // .NET 10.
    //
    // Returns 0 on success, or the number of the first check that failed.

    public static int S = 5;

    public static int Helper() => 7;

    private const BindingFlags Any = BindingFlags.Instance | BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic;

    private sealed class Emitted
    {
        public DynamicMethod Method;
        public List<object> Tokens;
        public int Slot;
    }

    // A dynamic method returning `returns` whose body is `body` followed by `ret`, with the one
    // scope slot `isSlot` picks out. With `pad`, the body is preceded by a branch over an `ldstr`
    // that nothing reaches, so the operand's slot has a slot before it which no executed
    // instruction names, and truncating the list just before the operand leaves its index past the
    // end without removing slot 1, which holds the method's own signature.
    private static Emitted Emit(Type returns, bool pad, Action<ILGenerator> body, Func<object, bool> isSlot)
    {
        DynamicMethod method = new DynamicMethod("Rewritten", returns, Type.EmptyTypes, typeof(Program).Module);
        ILGenerator il = method.GetILGenerator();
        if (pad)
        {
            Label over = il.DefineLabel();
            il.Emit(OpCodes.Br, over);
            il.Emit(OpCodes.Ldstr, "pad");
            il.Emit(OpCodes.Pop);
            il.MarkLabel(over);
        }
        body(il);
        il.Emit(OpCodes.Ret);

        object scope = il.GetType().GetField("m_scope", Any).GetValue(il);
        List<object> tokens = (List<object>) scope.GetType().GetField("m_tokens", Any).GetValue(scope);
        int found = -1;
        for (int i = 0; i < tokens.Count; i++)
        {
            if (isSlot(tokens[i]))
            {
                if (found >= 0)
                {
                    throw new InvalidOperationException("several slots match");
                }
                found = i;
            }
        }
        // Slot 0 is the seed null and slot 1 the signature, so an operand is at 2 or later.
        if (found < 2)
        {
            throw new InvalidOperationException("slot not found");
        }
        if (pad && !"pad".Equals(tokens[found - 1]))
        {
            throw new InvalidOperationException("the pad is not just before the slot");
        }

        Emitted emitted = new Emitted();
        emitted.Method = method;
        emitted.Tokens = tokens;
        emitted.Slot = found;
        return emitted;
    }

    private static DynamicMethod Returning(int value)
    {
        DynamicMethod dm = new DynamicMethod("Returning" + value, typeof(int), Type.EmptyTypes, typeof(Program).Module);
        ILGenerator il = dm.GetILGenerator();
        il.Emit(OpCodes.Ldc_I4, value);
        il.Emit(OpCodes.Ret);
        return dm;
    }

    // `call` of a dynamic method that answers 42.
    private static Emitted Call(bool pad)
    {
        DynamicMethod inner = Returning(42);
        return Emit(typeof(int), pad, il => il.Emit(OpCodes.Call, inner), o => ReferenceEquals(o, inner));
    }

    // `ldsfld Program.S`.
    private static Emitted Load(bool pad) =>
        Emit(typeof(int), pad, il => il.Emit(OpCodes.Ldsfld, typeof(Program).GetField("S")), o => o != null && o.GetType().FullName == "System.Reflection.Emit.GenericFieldInfo");

    // `sizeof long`.
    private static Emitted Size(bool pad) =>
        Emit(typeof(int), pad, il => il.Emit(OpCodes.Sizeof, typeof(long)), o => o is RuntimeTypeHandle h && h.Equals(typeof(long).TypeHandle));

    // `ldtoken long`, whose token is tagged as a type.
    private static Emitted Token(bool pad) =>
        Emit(typeof(RuntimeTypeHandle), pad, il => il.Emit(OpCodes.Ldtoken, typeof(long)), o => o is RuntimeTypeHandle h && h.Equals(typeof(long).TypeHandle));

    // `ldstr "hello"`, returned so that the literal is materialised.
    private static Emitted Literal(bool pad) =>
        Emit(typeof(string), pad, il => il.Emit(OpCodes.Ldstr, "hello"), o => o is string s && s == "hello");

    // try { r = 1; } catch (ArgumentException) { r = 2; } return r; -- nothing throws, but the
    // clause's type is a scope slot all the same.
    private static Emitted Guarded()
    {
        return Emit(
            typeof(int),
            false,
            il =>
            {
                LocalBuilder r = il.DeclareLocal(typeof(int));
                il.BeginExceptionBlock();
                il.Emit(OpCodes.Ldc_I4_1);
                il.Emit(OpCodes.Stloc, r);
                il.BeginCatchBlock(typeof(ArgumentException));
                il.Emit(OpCodes.Pop);
                il.Emit(OpCodes.Ldc_I4_2);
                il.Emit(OpCodes.Stloc, r);
                il.EndExceptionBlock();
                il.Emit(OpCodes.Ldloc, r);
            },
            o => o is RuntimeTypeHandle h && h.Equals(typeof(ArgumentException).TypeHandle));
    }

    // What to do to the token list, given the operand's slot.
    private static Action<List<object>, int> Set(object replacement) => (tokens, slot) => tokens[slot] = replacement;

    // The operand's index becomes exactly the list's length.
    private static readonly Action<List<object>, int> AtEnd = (tokens, slot) => tokens.RemoveRange(slot, tokens.Count - slot);

    // The operand's index becomes one past the list's length. Needs a padded body.
    private static readonly Action<List<object>, int> PastEnd = (tokens, slot) => tokens.RemoveRange(slot - 1, tokens.Count - slot + 1);

    private static readonly Action<List<object>, int> Untouched = (tokens, slot) => { };

    // Rewrite with `before`, mint, then rewrite with `after`.
    private static Func<int> MintInt(Emitted e, Action<List<object>, int> before, Action<List<object>, int> after)
    {
        before(e.Tokens, e.Slot);
        Func<int> f = (Func<int>) e.Method.CreateDelegate(typeof(Func<int>));
        after(e.Tokens, e.Slot);
        return f;
    }

    private static Func<string> MintString(Emitted e, Action<List<object>, int> before, Action<List<object>, int> after)
    {
        before(e.Tokens, e.Slot);
        Func<string> f = (Func<string>) e.Method.CreateDelegate(typeof(Func<string>));
        after(e.Tokens, e.Slot);
        return f;
    }

    private static Func<RuntimeTypeHandle> MintHandle(Emitted e, Action<List<object>, int> before, Action<List<object>, int> after)
    {
        before(e.Tokens, e.Slot);
        Func<RuntimeTypeHandle> f = (Func<RuntimeTypeHandle>) e.Method.CreateDelegate(typeof(Func<RuntimeTypeHandle>));
        after(e.Tokens, e.Slot);
        return f;
    }

    // A slot nulled before minting and restored afterwards, before anything invokes the method.
    private static Action<List<object>, int> RestoreFrom(Emitted e)
    {
        object saved = e.Tokens[e.Slot];
        return Set(saved);
    }

    private static bool Throws<T>(Action f, string message) where T : Exception
    {
        try
        {
            f();
            return false;
        }
        catch (T e)
        {
            return e.GetType() == typeof(T) && (message == null || e.Message == message);
        }
    }

    private static readonly string InvalidProgram = new InvalidProgramException().Message;

    private static readonly string NullReference = new NullReferenceException().Message;

    private const string HResultText = "An attempt was made to load a program with an incorrect format.\n (0x8007000B)";

    public static int Main(string[] args)
    {
        // ---- Method position: `call`.
        Func<int> f;
        f = MintInt(Call(false), Set(null), Untouched);
        if (!Throws<InvalidProgramException>(() => f(), InvalidProgram))
        {
            return 1;
        }
        // The message is not compared: it carries a parameter name.
        f = MintInt(Call(false), AtEnd, Untouched);
        if (!Throws<ArgumentOutOfRangeException>(() => f(), null))
        {
            return 2;
        }
        f = MintInt(Call(true), PastEnd, Untouched);
        if (!Throws<InvalidProgramException>(() => f(), InvalidProgram))
        {
            return 3;
        }
        f = MintInt(Call(false), Set("not a method"), Untouched);
        if (!Throws<BadImageFormatException>(() => f(), "Bad method token."))
        {
            return 4;
        }
        f = MintInt(Call(false), Set(typeof(int).TypeHandle), Untouched);
        if (!Throws<BadImageFormatException>(() => f(), "Bad method token."))
        {
            return 5;
        }
        if (MintInt(Call(false), Set(Returning(99)), Untouched)() != 99)
        {
            return 6;
        }
        if (MintInt(Call(false), Set(typeof(Program).GetMethod("Helper").MethodHandle), Untouched)() != 7)
        {
            return 7;
        }
        Emitted call = Call(false);
        if (MintInt(call, Set(null), RestoreFrom(call))() != 42)
        {
            return 8;
        }

        // ---- Field position: `ldsfld`.
        f = MintInt(Load(false), Set(null), Untouched);
        if (!Throws<InvalidProgramException>(() => f(), InvalidProgram))
        {
            return 10;
        }
        f = MintInt(Load(false), AtEnd, Untouched);
        if (!Throws<ArgumentOutOfRangeException>(() => f(), null))
        {
            return 11;
        }
        f = MintInt(Load(true), PastEnd, Untouched);
        if (!Throws<InvalidProgramException>(() => f(), InvalidProgram))
        {
            return 12;
        }
        f = MintInt(Load(false), Set("not a field"), Untouched);
        if (!Throws<BadImageFormatException>(() => f(), "Field token out of range."))
        {
            return 13;
        }
        // A bare handle in place of the GenericFieldInfo names the same field.
        if (MintInt(Load(false), Set(typeof(Program).GetField("S").FieldHandle), Untouched)() != 5)
        {
            return 14;
        }
        Emitted load = Load(false);
        if (MintInt(load, Set(null), RestoreFrom(load))() != 5)
        {
            return 15;
        }

        // ---- Type position: `sizeof`.
        f = MintInt(Size(false), Set(null), Untouched);
        if (!Throws<InvalidProgramException>(() => f(), InvalidProgram))
        {
            return 20;
        }
        f = MintInt(Size(false), Set("not a type"), Untouched);
        if (!Throws<BadImageFormatException>(() => f(), "Bad class token."))
        {
            return 21;
        }
        if (MintInt(Size(false), Set(typeof(int).TypeHandle), Untouched)() != 4)
        {
            return 22;
        }
        Emitted size = Size(false);
        if (MintInt(size, Set(null), RestoreFrom(size))() != 8)
        {
            return 23;
        }

        // ---- `ldtoken` of a type-tagged token: the tag is checked against what the entry holds,
        // so a field or method handle is as bad as a string.
        Func<RuntimeTypeHandle> h;
        h = MintHandle(Token(false), Set(null), Untouched);
        if (!Throws<InvalidProgramException>(() => h(), InvalidProgram))
        {
            return 30;
        }
        h = MintHandle(Token(false), Set("not a type"), Untouched);
        if (!Throws<BadImageFormatException>(() => h(), HResultText))
        {
            return 31;
        }
        h = MintHandle(Token(false), Set(typeof(Program).GetField("S").FieldHandle), Untouched);
        if (!Throws<BadImageFormatException>(() => h(), HResultText))
        {
            return 32;
        }
        h = MintHandle(Token(false), Set(typeof(Program).GetMethod("Helper").MethodHandle), Untouched);
        if (!Throws<BadImageFormatException>(() => h(), HResultText))
        {
            return 33;
        }
        if (!MintHandle(Token(false), Set(typeof(int).TypeHandle), Untouched)().Equals(typeof(int).TypeHandle))
        {
            return 34;
        }
        Emitted token = Token(false);
        if (!MintHandle(token, Set(null), RestoreFrom(token))().Equals(typeof(long).TypeHandle))
        {
            return 35;
        }

        // ---- `ldstr`, whose slot is read as `this[token] as string`, so anything but a string is
        // a null the JIT then indirects through.
        Func<string> s;
        s = MintString(Literal(false), Set(null), Untouched);
        if (!Throws<NullReferenceException>(() => s(), NullReference))
        {
            return 40;
        }
        s = MintString(Literal(false), AtEnd, Untouched);
        if (!Throws<ArgumentOutOfRangeException>(() => s(), null))
        {
            return 41;
        }
        s = MintString(Literal(true), PastEnd, Untouched);
        if (!Throws<NullReferenceException>(() => s(), NullReference))
        {
            return 42;
        }
        s = MintString(Literal(false), Set(typeof(int).TypeHandle), Untouched);
        if (!Throws<NullReferenceException>(() => s(), NullReference))
        {
            return 43;
        }
        if (MintString(Literal(false), Set("not hello"), Untouched)() != "not hello")
        {
            return 44;
        }
        Emitted literal = Literal(false);
        if (MintString(literal, Set(null), RestoreFrom(literal))() != "hello")
        {
            return 45;
        }

        // ---- A `catch` clause's type, resolved when the body is compiled whether or not anything
        // throws.
        f = MintInt(Guarded(), Set(null), Untouched);
        if (!Throws<InvalidProgramException>(() => f(), InvalidProgram))
        {
            return 50;
        }
        f = MintInt(Guarded(), Set("not a type"), Untouched);
        if (!Throws<BadImageFormatException>(() => f(), "Bad class token."))
        {
            return 51;
        }
        Emitted guarded = Guarded();
        if (MintInt(guarded, Set(null), RestoreFrom(guarded))() != 1)
        {
            return 52;
        }

        // ---- Slot 1, the method's own signature, is the one slot `CreateDelegate` reads: it
        // builds the resolver from it.
        Emitted unsigned = Call(false);
        unsigned.Tokens.RemoveRange(1, unsigned.Tokens.Count - 1);
        if (!Throws<ArgumentOutOfRangeException>(() => unsigned.Method.CreateDelegate(typeof(Func<int>)), null))
        {
            return 60;
        }

        return 0;
    }
}
