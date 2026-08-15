using System;
using System.Reflection.Emit;

public class Program
{
    // `ldstr` in a dynamic method's body, whose operand names an entry in the method's
    // `DynamicScope` rather than a UserString row in the module the method is scoped to. The two
    // are indistinguishable by inspection -- `index | 0x70000000` is a perfectly well-formed
    // UserString token naming some unrelated real row -- so this is about resolving against the
    // right universe, and then about *which object* comes back.
    //
    // The identity rules, from `GlobalStringLiteralMap::GetInternedString`
    // (`vm/stringliteralmap.cpp:403`): it is handed the scope's own `STRINGREF` and, on a miss,
    // stores *that object* via `AddInternedString(pString)` (line 431). Contrast the ordinary
    // metadata-literal path at line 396, `AddStringLiteral(EEStringData*)`, which allocates. So
    // dynamic `ldstr` interns by value, with the emitting guest's object as the candidate on miss.
    // Neither "always return the emitted object" nor "look up by value, allocating on miss" is
    // right; each is wrong on one side, and checks 2 and 3 below are the two sides.
    //
    // Timing is guest-visible too, and check 4 is the one that pins it: the interning happens at
    // first JIT, so between two methods carrying distinct-but-equal strings it is the first to be
    // *invoked* that wins, not the first to be minted.
    //
    // Every expectation here was measured on the host's real .NET before being written down --
    // impure cases get no automatic differential oracle, so the numbers are a claim that was
    // checked once by hand rather than one the harness re-establishes.
    //
    // Returns 0 on success, or the number of the first check that failed.

    /// Defeats the C# compiler's constant folding: `string.Concat("zz", "1")` on two literals
    /// would be folded to the literal `"zz1"` and interned at load, which is the opposite of what
    /// every check below needs.
    private static string Piece(string s)
    {
        return s;
    }

    /// A freshly-allocated string spelling <paramref name="a"/> followed by <paramref name="b"/>,
    /// not interned and not reference-equal to anything else.
    ///
    /// Taken as two halves rather than as the whole value on purpose. A single-argument `Fresh` would
    /// put `"zz1"` in this assembly's user-string heap as a compiled literal, and the very first
    /// `ldstr` of it anywhere would intern it -- so the "nothing has interned this yet" checks
    /// below would silently be testing the hit path instead. Both halves must also be non-empty:
    /// `string.Concat` returns the other operand unchanged when either is empty.
    private static string Fresh(string a, string b)
    {
        return string.Concat(Piece(a), Piece(b));
    }

    /// `ldstr <scope token>; ret`, as a `Func&lt;string&gt;`.
    private static Func<string> Emitting(string s)
    {
        DynamicMethod dm = new DynamicMethod("Lit", typeof(string), new Type[0], typeof(Program).Module);
        ILGenerator il = dm.GetILGenerator();
        il.Emit(OpCodes.Ldstr, s);
        il.Emit(OpCodes.Ret);
        return (Func<string>) dm.CreateDelegate(typeof(Func<string>));
    }

    public static int Main()
    {
        // 1. The characters survive the round trip through the scope at all.
        string fresh = Fresh("hel", "lo");
        if (Emitting(fresh)() != "hello")
        {
            return 1;
        }

        // 2. Miss path: nothing has interned "zz1", so the object the guest emitted becomes the
        // interned instance and comes back by reference. An implementation that looked the value
        // up and allocated a fresh string on miss would return an equal-but-distinct object.
        string missA = Fresh("zz", "1");
        if (!ReferenceEquals(Emitting(missA)(), missA))
        {
            return 2;
        }

        // 3. Hit path: "pqr" is a compiled literal in this assembly and so is already interned.
        // Emitting a distinct equal object must yield the *literal*, not the emitted object. An
        // implementation that just pushed whatever the scope held would return the latter.
        string literal = "pqr";
        string copy = Fresh("pq", "r");
        if (ReferenceEquals(copy, literal))
        {
            // The premise of the check, not the check: `Fresh` must really produce a new object.
            return 3;
        }

        if (!ReferenceEquals(Emitting(copy)(), literal))
        {
            return 4;
        }

        // 4. Timing. Both methods are minted (CreateDelegate => GetMethodDescriptor) before either
        // is invoked, and the one invoked *first* is the one whose string gets interned. Interning
        // at mint would make `mintedFirst` win instead.
        string mintedFirst = Fresh("zz", "2");
        string mintedSecond = Fresh("zz", "2");
        Func<string> first = Emitting(mintedFirst);
        Func<string> second = Emitting(mintedSecond);

        string fromSecond = second();
        string fromFirst = first();

        if (!ReferenceEquals(fromSecond, mintedSecond))
        {
            return 5;
        }

        if (!ReferenceEquals(fromFirst, mintedSecond))
        {
            return 6;
        }

        // 5. Control for check 4: with invocation in mint order, the minted-first string wins --
        // so check 4 is measuring invocation order, not "the second one always wins".
        string ctlFirst = Fresh("zz", "3");
        string ctlSecond = Fresh("zz", "3");
        Func<string> ctlA = Emitting(ctlFirst);
        Func<string> ctlB = Emitting(ctlSecond);

        if (!ReferenceEquals(ctlA(), ctlFirst))
        {
            return 7;
        }

        if (!ReferenceEquals(ctlB(), ctlFirst))
        {
            return 8;
        }

        // 6. The same delegate invoked twice keeps returning the one interned instance.
        Func<string> repeat = Emitting(Fresh("zz", "4"));
        if (!ReferenceEquals(repeat(), repeat()))
        {
            return 9;
        }

        // 7. The characters are read when the instruction runs, not when the method was minted. A
        // `System.String`'s data is mutable through an unsafe pointer, and a guest can mint a method
        // carrying one and *then* change it. Real .NET materialises the literal at first JIT, so it
        // is the mutated value that gets interned -- measured on the host, where `emitted` below
        // comes back reference-equal and a later method carrying the new value gets that same
        // object. An implementation that snapshotted the characters at mint would key the intern
        // table under the old value and hand out a second object for the new one.
        string mutable_ = Fresh("m", "old");
        Func<string> readsMutable = Emitting(mutable_);

        unsafe
        {
            fixed (char* p = mutable_)
            {
                p[0] = 'n';
            }
        }

        // `mutable_` now spells "nold".
        if (!ReferenceEquals(readsMutable(), mutable_))
        {
            return 10;
        }

        string sameAsMutated = Fresh("n", "old");

        if (ReferenceEquals(sameAsMutated, mutable_))
        {
            // Premise: `Fresh` really did build a separate object.
            return 11;
        }

        if (!ReferenceEquals(Emitting(sameAsMutated)(), mutable_))
        {
            return 12;
        }

        return 0;
    }
}
