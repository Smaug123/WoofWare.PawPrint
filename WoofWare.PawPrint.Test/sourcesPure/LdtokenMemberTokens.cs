using System;
using System.Linq.Expressions;
using System.Reflection;
using System.Threading.Tasks;

public class Box<T>
{
    public T Get() => default(T);
    public static U Stat<U>(U x) => x;
}

public static class Program
{
    // `ldtoken` of a *member* token. C# has no syntax for the instruction, but an expression tree
    // reaches it: Roslyn lowers each captured member access to `ldtoken <member>` followed by
    // `MethodBase.GetMethodFromHandle`, so the `MethodInfo` hanging off the resulting node is
    // exactly the handle the instruction pushed.
    //
    // The token kinds exercised here:
    //
    //   MemberReference, TypeReference parent      -- `Math.Abs`, `Task.get_CompletedTask`
    //   MemberReference, TypeSpecification parent  -- `Box<int>.Get`
    //   MethodSpecification over a MemberReference -- `Box<string>.Stat<int>`
    //   MethodSpecification over a MethodDef       -- `Ident<int>`
    //   FieldDefinition                            -- the array initialiser at the end
    //
    // Reference equality throughout, not `==`. CoreCLR hands back one `MethodDesc` for a method
    // however it is reached, and the reflection cache turns that into one `MethodInfo` object; an
    // implementation that minted a second, equal-but-distinct handle would satisfy an equality
    // check while breaking the identity .NET guarantees.
    //
    // Two shapes are deliberately absent, each for a gap of its own:
    //
    //  - A *field* named by a MemberReference (`string.Empty`, `Box<int>.Item`). The `ldtoken`
    //    half works -- `TestLdtokenMemberTokens` asserts both the TypeRef- and TypeSpec-parent
    //    field cases, including that the handle records the *defining* assembly -- but every route
    //    from a `RuntimeFieldHandle` back to a `FieldInfo` runs `RuntimeType.GetFieldInfo`, which
    //    reaches the unimplemented `RuntimeFieldHandle.AcquiresContextFromThis` whenever the field
    //    handle is the first thing to materialise its declaring type. That is independent of this
    //    file: a `FieldDefinition` `ldtoken` of a same-assembly static, which the interpreter has
    //    always supported, stops in the same place (measured). Un-park these when that InternalCall
    //    lands; the `Expression.Field` node's `.Member` is then the assertion to make.
    //  - `Compile()`. That is Reflection.Emit, a different feature; building the tree is what runs
    //    the `ldtoken`s.
    //
    // A bare MemberReference naming a *generic method* -- the typical-instantiation form -- is not
    // reachable from C# at all: an instantiation always arrives as a MethodSpec wrapping that row.
    // The interpreter refuses it explicitly, and `TestLdtokenMemberTokens` asserts the refusal.
    //
    // Exit 0 on success, otherwise the index of the first failing check.
    public static int Main()
    {
        // --- MemberReference, TypeReference parent. ---
        // Cross-assembly: the reference is written here, the definition lives in CoreLib, and the
        // handle must index CoreLib's tables rather than this assembly's.
        Expression<Func<int>> abs = () => Math.Abs(-1);
        MethodInfo absMethod = ((MethodCallExpression)abs.Body).Method;
        if (!ReferenceEquals(absMethod, typeof(Math).GetMethod("Abs", new[] { typeof(int) }))) return 1;
        if (absMethod.DeclaringType != typeof(Math)) return 2;

        // The shape that motivated this: `RequestDelegateFactory`'s class constructor does exactly
        // this, and stopped here.
        Expression<Func<Task>> completed = () => Task.CompletedTask;
        MemberInfo completedMember = ((MemberExpression)completed.Body).Member;
        if (!ReferenceEquals(completedMember, typeof(Task).GetProperty("CompletedTask"))) return 3;

        // --- MemberReference, TypeSpecification parent. ---
        // The declaring type is a constructed generic, so the handle must carry the instantiation.
        Expression<Func<Box<int>, int>> get = b => b.Get();
        MethodInfo getMethod = ((MethodCallExpression)get.Body).Method;
        if (!ReferenceEquals(getMethod, typeof(Box<int>).GetMethod("Get"))) return 4;
        if (getMethod.DeclaringType != typeof(Box<int>)) return 5;
        if (getMethod.ReturnType != typeof(int)) return 6;
        // A different instantiation is a different handle, which is what "carries the
        // instantiation" means; without this, dropping the instantiation entirely could pass.
        if (ReferenceEquals(getMethod, typeof(Box<string>).GetMethod("Get"))) return 7;

        // --- MethodSpecification over a MemberReference: both axes bound at once. ---
        // Declaring type from the reference's TypeSpec parent, method instantiation from the spec.
        Expression<Func<int, int>> stat = x => Box<string>.Stat<int>(x);
        MethodInfo statMethod = ((MethodCallExpression)stat.Body).Method;
        if (statMethod.DeclaringType != typeof(Box<string>)) return 8;
        if (!statMethod.IsGenericMethod) return 9;
        if (statMethod.IsGenericMethodDefinition) return 10;
        if (statMethod.GetGenericArguments()[0] != typeof(int)) return 11;
        if (!ReferenceEquals(statMethod, typeof(Box<string>).GetMethod("Stat").MakeGenericMethod(typeof(int)))) return 12;

        // --- MethodSpecification over a MethodDef. ---
        Expression<Func<int, int>> ident = x => Ident<int>(x);
        MethodInfo identMethod = ((MethodCallExpression)ident.Body).Method;
        if (identMethod.DeclaringType != typeof(Program)) return 13;
        if (identMethod.GetGenericArguments()[0] != typeof(int)) return 14;
        if (!ReferenceEquals(identMethod, typeof(Program).GetMethod("Ident").MakeGenericMethod(typeof(int)))) return 15;

        // --- FieldDefinition, the shape that already worked: an array initialiser blob. ---
        // A control, so a change that broke the pre-existing arm while fixing the new ones shows up
        // here rather than only in the interpreter's own tests.
        int[] data = { 9, 8, 7, 6, 5 };
        if (data.Length != 5 || data[0] != 9 || data[4] != 5) return 16;

        // The whole point of the identity assertions: the guest reaches one member by two routes
        // and must get one object.
        Expression<Func<int>> absAgain = () => Math.Abs(-2);
        if (!ReferenceEquals(((MethodCallExpression)absAgain.Body).Method, absMethod)) return 17;

        return 0;
    }

    public static T Ident<T>(T x) => x;

}
