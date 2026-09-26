using System;
using System.Collections.Generic;
using System.Reflection;
using System.Reflection.Emit;

public class Gen<T>
{
    public static string Name() => typeof(T).Name;
}

public class St<T>
{
    public static T V;
}

public class Program
{
    // A dynamic method's scope is read when its body is compiled, not when it is emitted, so a guest
    // that rewrites `DynamicILGenerator.m_scope.m_tokens` through private reflection after
    // `CreateDelegate` and before the first invocation decides what each token names. Every
    // expected answer, including each exception and its message, was measured on real .NET 10.
    //
    // Returns 0 on success, or the number of the first check that failed.

    public static int S = 5;

    public static int Helper() => 7;

    private const BindingFlags Any = BindingFlags.Instance | BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic;

    private static List<object> Tokens(ILGenerator il)
    {
        object scope = il.GetType().GetField("m_scope", Any).GetValue(il);
        return (List<object>) scope.GetType().GetField("m_tokens", Any).GetValue(scope);
    }

    // The one slot `isSlot` picks out, or -1 if it picks out none or several.
    private static int SlotOf(List<object> tokens, Func<object, bool> isSlot)
    {
        int found = -1;
        for (int i = 0; i < tokens.Count; i++)
        {
            if (isSlot(tokens[i]))
            {
                if (found >= 0)
                {
                    return -1;
                }
                found = i;
            }
        }
        return found;
    }

    private static bool IsNamed(object o, string name) => o != null && o.GetType().FullName == name;

    // An instance of one of the internal wrapper types `DynamicILGenerator` stores in the scope,
    // holding a member handle and a context. Built field by field rather than through its
    // constructor: with dynamic code enabled, the BCL's second invocation of one ConstructorInfo
    // emits an invoker stub, which is not what this program is about.
    private static object Wrapper(string typeName, string handleField, object handle, RuntimeTypeHandle context)
    {
        Type type = typeof(DynamicMethod).Assembly.GetType(typeName, true);
        object wrapper = System.Runtime.CompilerServices.RuntimeHelpers.GetUninitializedObject(type);
        type.GetField(handleField, Any).SetValue(wrapper, handle);
        type.GetField("m_context", Any).SetValue(wrapper, context);
        return wrapper;
    }

    private static object MethodInfoWrapper(RuntimeMethodHandle handle, RuntimeTypeHandle context) =>
        Wrapper("System.Reflection.Emit.GenericMethodInfo", "m_methodHandle", handle, context);

    private static object FieldInfoWrapper(RuntimeFieldHandle handle, RuntimeTypeHandle context) =>
        Wrapper("System.Reflection.Emit.GenericFieldInfo", "m_fieldHandle", handle, context);

    private static DynamicMethod Returning(int value)
    {
        DynamicMethod dm = new DynamicMethod("Returning" + value, typeof(int), Type.EmptyTypes, typeof(Program).Module);
        ILGenerator il = dm.GetILGenerator();
        il.Emit(OpCodes.Ldc_I4, value);
        il.Emit(OpCodes.Ret);
        return dm;
    }

    // `call` of a dynamic method that answers 42, with the callee's slot then replaced by
    // `replacement`, or, if `truncateTo` is given, the list truncated to end that far after the
    // slot.
    private static Func<int> CallWith(object replacement, int? truncateTo)
    {
        DynamicMethod inner = Returning(42);
        DynamicMethod outer = new DynamicMethod("Outer", typeof(int), Type.EmptyTypes, typeof(Program).Module);
        ILGenerator il = outer.GetILGenerator();
        il.Emit(OpCodes.Call, inner);
        il.Emit(OpCodes.Ret);
        Func<int> f = (Func<int>) outer.CreateDelegate(typeof(Func<int>));
        Rewrite(Tokens(il), SlotOf(Tokens(il), o => ReferenceEquals(o, inner)), replacement, truncateTo);
        return f;
    }

    // `ldsfld Program.S`, rewritten likewise.
    private static Func<int> LoadWith(object replacement, int? truncateTo)
    {
        DynamicMethod outer = new DynamicMethod("Load", typeof(int), Type.EmptyTypes, typeof(Program).Module);
        ILGenerator il = outer.GetILGenerator();
        il.Emit(OpCodes.Ldsfld, typeof(Program).GetField("S"));
        il.Emit(OpCodes.Ret);
        Func<int> f = (Func<int>) outer.CreateDelegate(typeof(Func<int>));
        Rewrite(Tokens(il), SlotOf(Tokens(il), o => IsNamed(o, "System.Reflection.Emit.GenericFieldInfo")), replacement, truncateTo);
        return f;
    }

    // The slot index is always at least 1, since `DynamicScope` never hands out index 0; a slot of
    // -1 means the probe addressed nothing, which `Rewrite` turns into an exception no check expects.
    private static void Rewrite(List<object> tokens, int slot, object replacement, int? truncateTo)
    {
        if (slot < 1)
        {
            throw new InvalidOperationException("slot not found");
        }
        if (truncateTo is int offset)
        {
            tokens.RemoveRange(slot + offset, tokens.Count - slot - offset);
        }
        else
        {
            tokens[slot] = replacement;
        }
    }

    // Truncation offsets relative to the slot: at it, so the index equals the list's length, and
    // one before it, so the index is past the length.
    private const int AtSlot = 0;
    private const int PastSlot = -1;
    private static readonly int? Replace = null;

    private static bool Throws<T>(Func<int> f, string message) where T : Exception
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

    private const string HResultText = "An attempt was made to load a program with an incorrect format.\n (0x8007000B)";

    public static int Main(string[] args)
    {
        MethodInfo helper = typeof(Program).GetMethod("Helper");
        FieldInfo s = typeof(Program).GetField("S");

        // ---- Method position: `call`.
        if (CallWith(Returning(42), Replace)() != 42)
        {
            return 1;
        }
        if (!Throws<InvalidProgramException>(CallWith(null, Replace), InvalidProgram))
        {
            return 2;
        }
        // `DynamicScope`'s indexer lets an index equal to the length through its own bound check,
        // and `List<T>` faults on it. The message is not compared: it carries a parameter name.
        if (!Throws<ArgumentOutOfRangeException>(CallWith(null, AtSlot), null))
        {
            return 3;
        }
        if (!Throws<InvalidProgramException>(CallWith(null, PastSlot), InvalidProgram))
        {
            return 4;
        }
        if (!Throws<BadImageFormatException>(CallWith("not a method", Replace), "Bad method token."))
        {
            return 5;
        }
        if (!Throws<BadImageFormatException>(CallWith(typeof(int).TypeHandle, Replace), "Bad method token."))
        {
            return 6;
        }
        if (!Throws<BadImageFormatException>(CallWith(s.FieldHandle, Replace), "Bad method token."))
        {
            return 7;
        }
        if (!Throws<BadImageFormatException>(CallWith(new byte[] { 0, 0 }, Replace), "Bad method token."))
        {
            return 8;
        }
        if (!Throws<BadImageFormatException>(CallWith(FieldInfoWrapper(s.FieldHandle, typeof(Program).TypeHandle), Replace), "Bad method token."))
        {
            return 9;
        }
        if (!Throws<BadImageFormatException>(CallWith(default(RuntimeMethodHandle), Replace), "Bad method token."))
        {
            return 10;
        }
        // A null method is refused before its context is looked at, open or not.
        if (!Throws<BadImageFormatException>(CallWith(MethodInfoWrapper(default(RuntimeMethodHandle), typeof(Gen<>).TypeHandle), Replace), "Bad method token."))
        {
            return 11;
        }
        // A null context is no context: the method is called on its own type.
        if (CallWith(MethodInfoWrapper(helper.MethodHandle, default(RuntimeTypeHandle)), Replace)() != 7)
        {
            return 12;
        }
        // An open context is an invalid program, however good the method.
        if (!Throws<InvalidProgramException>(CallWith(MethodInfoWrapper(helper.MethodHandle, typeof(Gen<>).TypeHandle), Replace), InvalidProgram))
        {
            return 13;
        }
        // A dynamic method's own handle, which `DynamicMethod.MethodHandle` refuses to hand out but
        // its internal `GetMethodDescriptor` does not, names that method.
        object otherHandle = typeof(DynamicMethod).GetMethod("GetMethodDescriptor", Any).Invoke(Returning(99), null);
        if (CallWith(otherHandle, Replace)() != 99)
        {
            return 14;
        }

        // ---- Field position: `ldsfld`.
        if (LoadWith(FieldInfoWrapper(s.FieldHandle, typeof(Program).TypeHandle), Replace)() != 5)
        {
            return 20;
        }
        if (!Throws<InvalidProgramException>(LoadWith(null, Replace), InvalidProgram))
        {
            return 21;
        }
        if (!Throws<ArgumentOutOfRangeException>(LoadWith(null, AtSlot), null))
        {
            return 22;
        }
        if (!Throws<InvalidProgramException>(LoadWith(null, PastSlot), InvalidProgram))
        {
            return 23;
        }
        if (!Throws<BadImageFormatException>(LoadWith("not a field", Replace), "Field token out of range."))
        {
            return 24;
        }
        if (!Throws<BadImageFormatException>(LoadWith(helper.MethodHandle, Replace), "Field token out of range."))
        {
            return 25;
        }
        if (!Throws<BadImageFormatException>(LoadWith(default(RuntimeFieldHandle), Replace), "Field token out of range."))
        {
            return 26;
        }
        // A null context is no context: the field is read on its own type.
        if (LoadWith(FieldInfoWrapper(s.FieldHandle, default(RuntimeTypeHandle)), Replace)() != 5)
        {
            return 27;
        }
        // An open context is an invalid program, whether or not the field's own type is generic.
        if (!Throws<InvalidProgramException>(LoadWith(FieldInfoWrapper(s.FieldHandle, typeof(St<>).TypeHandle), Replace), InvalidProgram))
        {
            return 28;
        }
        St<string>.V = "unread";
        if (!Throws<InvalidProgramException>(() => LoadWith(FieldInfoWrapper(typeof(St<string>).GetField("V").FieldHandle, typeof(St<>).TypeHandle), Replace)(), InvalidProgram))
        {
            return 29;
        }

        // ---- Type position: `sizeof` asks for a class token, `ldtoken` for any of the three.
        DynamicMethod size = new DynamicMethod("Size", typeof(int), Type.EmptyTypes, typeof(Program).Module);
        ILGenerator sizeIl = size.GetILGenerator();
        sizeIl.Emit(OpCodes.Sizeof, typeof(int));
        sizeIl.Emit(OpCodes.Ret);
        Func<int> sizeOf = (Func<int>) size.CreateDelegate(typeof(Func<int>));
        Rewrite(Tokens(sizeIl), SlotOf(Tokens(sizeIl), o => o is RuntimeTypeHandle h && h.Equals(typeof(int).TypeHandle)), "not a type", Replace);
        if (!Throws<BadImageFormatException>(sizeOf, "Bad class token."))
        {
            return 30;
        }

        DynamicMethod token = new DynamicMethod("Token", typeof(RuntimeTypeHandle), Type.EmptyTypes, typeof(Program).Module);
        ILGenerator tokenIl = token.GetILGenerator();
        tokenIl.Emit(OpCodes.Ldtoken, typeof(int));
        tokenIl.Emit(OpCodes.Ret);
        Func<RuntimeTypeHandle> tokenOf = (Func<RuntimeTypeHandle>) token.CreateDelegate(typeof(Func<RuntimeTypeHandle>));
        Rewrite(Tokens(tokenIl), SlotOf(Tokens(tokenIl), o => o is RuntimeTypeHandle h && h.Equals(typeof(int).TypeHandle)), "not a type", Replace);
        if (!Throws<BadImageFormatException>(() => { tokenOf(); return 0; }, HResultText))
        {
            return 31;
        }

        return 0;
    }
}
