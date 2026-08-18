// The four culture-sensitive arms of `String.Equals(string, StringComparison)` and the static
// `String.Equals(string, string, StringComparison)`. The ordinal arms, the null handling and
// the argument validation are in `StringEqualsComparison.cs`; these arms are split out because
// they reach a different comparer, and because what may safely be asserted about them is
// narrower.
//
// The arms are `CompareInfo.Compare(a, b, options) == 0`, where the `StartsWith`/`EndsWith`
// siblings used `IsPrefix`/`IsSuffix`. Under the guest's
// DOTNET_SYSTEM_GLOBALIZATION_INVARIANT=1, `CompareInfo.Compare`'s span overload short-circuits
// ahead of any ICU call and normalises the options, so exactly two comparers run:
//
//  * `CompareOptions.None` (CurrentCulture, InvariantCulture) ->
//    `ReadOnlySpan<char>.SequenceCompareTo` -> `SpanHelpers.SequenceCompareTo(ref char, int,
//    ref char, int)`. On a 64-bit target that walks a 4-char (nuint-wide) block loop, then one
//    2-char `int` step, then a 1-char `CompareTo` tail, and finally returns `lengthDelta` — so
//    two strings where one is a prefix of the other compare unequal without any character
//    mismatch being found. `TestNoneStepShapes` covers lengths 1..9, which is every combination
//    of those steps, with a mismatch landing in each; `TestNoneLengthDelta` covers the
//    prefix/`lengthDelta` exit.
//  * `CompareOptions.IgnoreCase` (CurrentCultureIgnoreCase, InvariantCultureIgnoreCase) ->
//    `Ordinal.CompareStringIgnoreCase`. This is a *different* function from the
//    `Ordinal.EqualsIgnoreCase` the OrdinalIgnoreCase arm uses, and here it is the main path
//    rather than a non-ASCII bail-out: a plain char-at-a-time `while` loop that folds only
//    `[A-Za-z]`, then `return lengthA - lengthB` when it runs out of input.
//    `TestIgnoreCaseStepShapes` covers the fold, the letter-gating, and that length exit.
//
// Three things bound what this file can claim.
//
// First, only `Compare(...) == 0` is observable through `Equals`, so a comparer wrong in *sign*
// alone would be invisible here. What is asserted is the equality verdict.
//
// Second, the differential oracle runs the guest without DOTNET_SYSTEM_GLOBALIZATION_INVARIANT,
// so it collates with the host's ICU while PawPrint collates with the invariant tables above.
// The `CurrentCulture` arms therefore depend on the *host's* culture, and that dependence is not
// hypothetical: with the operands below and no intervention, this file fails under
// `LC_ALL=th_TH.UTF-8` with exit code 6, because Thai's ICU tailoring makes '!' an ignorable
// character and so collates "Hello, world" equal to "Hello, world!", where PawPrint's invariant
// comparer sees different lengths. So `Main` pins `CultureInfo.CurrentCulture` to
// `CultureInfo.InvariantCulture` before running any of the pinned sections, which makes both
// runtimes use a locale-independent collation for those arms. Verified: with the pin, the file
// passes under both the default host locale and `th_TH.UTF-8`.
//
// What that leaves assumed is only that ICU's *root* collation agrees with ordinal comparison on
// the alphabet used here — which is why the alphabet is still curated, and the curation is
// load-bearing rather than incidental:
//
//  * Printable ASCII only. ICU treats several C0 control characters as completely ignorable, so
//    `"a\u0001"` would collate *equal* to `"a"` under the oracle, while `SequenceCompareTo`
//    under PawPrint sees different lengths and so calls them unequal.
//  * No 'i' or 'I' anywhere, so that nothing here depends on the one ASCII case mapping that is
//    locale-tailored ('i' upper-cases to 'İ' under tr-TR and az-Latn). The alphabet is drawn
//    from a..h / A..H for that reason.
//  * No expansions, ignorables or locale-specific orderings — no 'æ', no combining marks, no
//    non-ASCII at all.
//
// Third, `TestHostCulture` deliberately runs *before* the pin, and asserts only that a string
// compares equal to a distinct object with identical content. That is true under every collation
// — identical input yields identical sort keys — so it is safe to assert against whatever culture
// the host actually has, and it exercises the two `CurrentCulture` arms under a real tailoring
// rather than only under the root one.
//
// None of this amounts to asserting that PawPrint agrees with an invariant-mode *real* .NET
// process on arbitrary input; this harness has no way to run the oracle in that mode.

using System;
using System.Globalization;
using System.Runtime.CompilerServices;

public class Program
{
    [MethodImpl(MethodImplOptions.NoInlining)]
    private static string Id(string s)
    {
        return s;
    }

    // A heap copy, so `ReferenceEquals` is false and the comparer actually runs. Both
    // short-circuits ahead of the switch (`ReferenceEquals` in `Equals`, then `span1 == span2`
    // inside `CompareInfo.Compare`, then `Unsafe.AreSame` inside `SequenceCompareTo`) test
    // reference identity, so without this every equal-pair case would answer without comparing
    // a single character.
    [MethodImpl(MethodImplOptions.NoInlining)]
    private static string Copy(string s)
    {
        return new string(s.AsSpan());
    }

    // Runs before `Main` pins the culture, so this is the host's real tailoring — whatever it
    // happens to be. Only the locale-universal direction is asserted: a string compares equal to
    // a distinct object holding the same characters, which holds under every collation. The
    // *unequal* direction is not asserted here, because whether two different strings collate
    // apart depends on the tailoring (see the header's Thai '!' case).
    private static int TestHostCulture()
    {
        string s = Id("Hello, world");
        string copy = Copy(s);

        if (ReferenceEquals(s, copy)) return 1;

        if (!s.Equals(copy, StringComparison.CurrentCulture)) return 2;
        if (!s.Equals(copy, StringComparison.CurrentCultureIgnoreCase)) return 3;
        if (!String.Equals(s, copy, StringComparison.CurrentCulture)) return 4;
        if (!String.Equals(s, copy, StringComparison.CurrentCultureIgnoreCase)) return 5;
        // A null argument short-circuits ahead of the comparer, so its answer cannot depend on
        // the tailoring either.
        if (s.Equals(null, StringComparison.CurrentCulture)) return 6;
        if (!String.Equals(null, null, StringComparison.CurrentCultureIgnoreCase)) return 7;

        return 0;
    }

    private static int TestCurrentCulture()
    {
        string s = Id("Hello, world");
        string copy = Copy(s);

        if (ReferenceEquals(s, copy)) return 1;

        // Reached with the culture pinned, so these are root-collation answers.
        if (!s.Equals(copy, StringComparison.CurrentCulture)) return 2;
        if (s.Equals(Id("hello, world"), StringComparison.CurrentCulture)) return 3;
        if (!s.Equals(Id("hello, world"), StringComparison.CurrentCultureIgnoreCase)) return 4;
        if (s.Equals(Id("hello, worlX"), StringComparison.CurrentCultureIgnoreCase)) return 5;
        // Unlike the ordinal arms these have no `Length` short-circuit, so a length difference
        // is the comparer's own answer rather than an early return.
        if (s.Equals(Id("Hello, world!"), StringComparison.CurrentCulture)) return 6;
        if (s.Equals(Id("Hello, worl"), StringComparison.CurrentCulture)) return 7;
        if (s.Equals(Id("HELLO, WORLD!"), StringComparison.CurrentCultureIgnoreCase)) return 8;

        if (!String.Equals(s, copy, StringComparison.CurrentCulture)) return 9;
        if (String.Equals(s, Id("hello, world"), StringComparison.CurrentCulture)) return 10;
        if (!String.Equals(s, Id("hello, world"), StringComparison.CurrentCultureIgnoreCase)) return 11;
        if (String.Equals(s, Id("hello, worlX"), StringComparison.CurrentCultureIgnoreCase)) return 12;

        return 0;
    }

    private static int TestInvariantCulture()
    {
        string s = Id("Hello, world");
        string copy = Copy(s);

        if (!s.Equals(copy, StringComparison.InvariantCulture)) return 1;
        if (s.Equals(Id("hello, world"), StringComparison.InvariantCulture)) return 2;
        if (!s.Equals(Id("hello, world"), StringComparison.InvariantCultureIgnoreCase)) return 3;
        if (s.Equals(Id("hello, worlX"), StringComparison.InvariantCultureIgnoreCase)) return 4;
        if (s.Equals(Id("Hello, world!"), StringComparison.InvariantCulture)) return 5;
        if (s.Equals(Id("Hello, worl"), StringComparison.InvariantCulture)) return 6;

        if (!String.Equals(s, copy, StringComparison.InvariantCulture)) return 7;
        if (String.Equals(s, Id("hello, world"), StringComparison.InvariantCulture)) return 8;
        if (!String.Equals(s, Id("hello, world"), StringComparison.InvariantCultureIgnoreCase)) return 9;
        if (String.Equals(s, Id("hello, worlX"), StringComparison.InvariantCultureIgnoreCase)) return 10;
        // Empty strings reach the comparer with zero length, so neither loop runs and the
        // answer is `lengthDelta` alone.
        if (!Id("").Equals(Id(""), StringComparison.InvariantCulture)) return 11;
        if (!Id("").Equals(Id(""), StringComparison.InvariantCultureIgnoreCase)) return 12;
        if (Id("").Equals(Id("a"), StringComparison.InvariantCulture)) return 13;
        if (Id("a").Equals(Id(""), StringComparison.InvariantCulture)) return 14;

        return 0;
    }

    // `SpanHelpers.SequenceCompareTo` on a 64-bit target: floor(L/4) turns of the nuint block
    // loop, then at most one 2-char `int` step, then at most a 1-char tail. Lengths 1..9 cover
    // every combination (L=8 is two block turns; L=9 is two turns plus a tail). Each length
    // appears once equal — via `Copy`, so the walk really runs — and once mismatching in its
    // final position, and the longer ones also mismatch in their first, so a walk that skipped
    // either end would be caught.
    //
    // The alphabet is a..h / A..H plus digits: printable ASCII, no 'i'/'I'.
    private static int TestNoneStepShapes()
    {
        if (!Id("a").Equals(Copy(Id("a")), StringComparison.InvariantCulture)) return 1;
        if (Id("a").Equals(Id("b"), StringComparison.InvariantCulture)) return 2;
        if (!Id("ab").Equals(Copy(Id("ab")), StringComparison.InvariantCulture)) return 3;
        if (Id("ab").Equals(Id("ac"), StringComparison.InvariantCulture)) return 4;
        if (!Id("abc").Equals(Copy(Id("abc")), StringComparison.InvariantCulture)) return 5;
        if (Id("abc").Equals(Id("abd"), StringComparison.InvariantCulture)) return 6;
        if (!Id("abcd").Equals(Copy(Id("abcd")), StringComparison.InvariantCulture)) return 7;
        if (Id("abcd").Equals(Id("abce"), StringComparison.InvariantCulture)) return 8;
        if (!Id("abcde").Equals(Copy(Id("abcde")), StringComparison.InvariantCulture)) return 9;
        if (Id("abcde").Equals(Id("abcdf"), StringComparison.InvariantCulture)) return 10;
        if (!Id("abcdef").Equals(Copy(Id("abcdef")), StringComparison.InvariantCulture)) return 11;
        if (Id("abcdef").Equals(Id("abcdeg"), StringComparison.InvariantCulture)) return 12;
        if (!Id("abcdefg").Equals(Copy(Id("abcdefg")), StringComparison.InvariantCulture)) return 13;
        if (Id("abcdefg").Equals(Id("abcdefh"), StringComparison.InvariantCulture)) return 14;
        if (!Id("abcdefgh").Equals(Copy(Id("abcdefgh")), StringComparison.InvariantCulture)) return 15;
        if (Id("abcdefgh").Equals(Id("abcdefga"), StringComparison.InvariantCulture)) return 16;
        if (!Id("abcdefgh2").Equals(Copy(Id("abcdefgh2")), StringComparison.InvariantCulture)) return 17;
        if (Id("abcdefgh2").Equals(Id("abcdefgh3"), StringComparison.InvariantCulture)) return 18;
        // Mismatch in the first position, at each of the interesting lengths: inside the first
        // block turn, inside the second, and inside the 2-char step.
        if (Id("abcd").Equals(Id("bbcd"), StringComparison.InvariantCulture)) return 19;
        if (Id("abcdefgh").Equals(Id("bbcdefgh"), StringComparison.InvariantCulture)) return 20;
        if (Id("abcdef").Equals(Id("abcdff"), StringComparison.InvariantCulture)) return 21;
        // Mismatch inside the second block turn but not the first.
        if (Id("abcdefgh").Equals(Id("abcdafgh"), StringComparison.InvariantCulture)) return 22;

        return 0;
    }

    // The `return lengthDelta` exit: every compared character agrees, and the strings are
    // unequal only because one runs out. Both directions, and at lengths that make the
    // divergence fall in the block loop, the 2-char step and the tail respectively.
    private static int TestNoneLengthDelta()
    {
        if (Id("abcd").Equals(Id("abcdef"), StringComparison.InvariantCulture)) return 1;
        if (Id("abcdef").Equals(Id("abcd"), StringComparison.InvariantCulture)) return 2;
        if (Id("a").Equals(Id("ab"), StringComparison.InvariantCulture)) return 3;
        if (Id("ab").Equals(Id("a"), StringComparison.InvariantCulture)) return 4;
        if (Id("abcdefgh").Equals(Id("abcdefghab"), StringComparison.InvariantCulture)) return 5;
        if (Id("abcdefghab").Equals(Id("abcdefgh"), StringComparison.InvariantCulture)) return 6;
        if (String.Equals(Id("abcd"), Id("abcdef"), StringComparison.InvariantCulture)) return 7;
        if (String.Equals(Id("abcdef"), Id("abcd"), StringComparison.CurrentCulture)) return 8;

        return 0;
    }

    // `Ordinal.CompareStringIgnoreCase`'s ASCII loop. Its fold is letter-gated —
    // `(charA | 0x20) == (charB | 0x20) && char.IsAsciiLetter(charA)` — so the pairs that share
    // the 0x20 bit without being letters are the discriminating cases, exactly as for the
    // OrdinalIgnoreCase arm but through different code. Its length exit is
    // `return lengthA - lengthB`, reached when the loop consumes all of the shorter string.
    private static int TestIgnoreCaseStepShapes()
    {
        // The fold itself, at a range of lengths.
        if (!Id("a").Equals(Id("A"), StringComparison.InvariantCultureIgnoreCase)) return 1;
        if (!Id("ab").Equals(Id("AB"), StringComparison.InvariantCultureIgnoreCase)) return 2;
        if (!Id("abcd").Equals(Id("ABCD"), StringComparison.InvariantCultureIgnoreCase)) return 3;
        if (!Id("abcdefgh").Equals(Id("ABCDEFGH"), StringComparison.InvariantCultureIgnoreCase)) return 4;
        if (!Id("aBcDeFgH").Equals(Id("AbCdEfGh"), StringComparison.InvariantCultureIgnoreCase)) return 5;
        // Mismatch at the first, a middle and the last position.
        if (Id("abcd").Equals(Id("Bbcd"), StringComparison.InvariantCultureIgnoreCase)) return 6;
        if (Id("abcd").Equals(Id("ABgD"), StringComparison.InvariantCultureIgnoreCase)) return 7;
        if (Id("abcd").Equals(Id("ABCG"), StringComparison.InvariantCultureIgnoreCase)) return 8;
        // The `return lengthA - lengthB` exit: equal ignoring case as far as the shorter goes.
        if (Id("abcd").Equals(Id("ABCDEF"), StringComparison.InvariantCultureIgnoreCase)) return 9;
        if (Id("abcdef").Equals(Id("ABCD"), StringComparison.InvariantCultureIgnoreCase)) return 10;
        // Letter-gating: '[' (0x5B) and '{' (0x7B) share the 0x20 bit but are not letters, so a
        // blanket `| 0x20` fold would wrongly call these equal. Likewise '@' (0x40) vs '`'
        // (0x60), and a letter against a non-letter sharing its low bits.
        if (Id("[").Equals(Id("{"), StringComparison.InvariantCultureIgnoreCase)) return 11;
        if (Id("[[[[").Equals(Id("{{{{"), StringComparison.InvariantCultureIgnoreCase)) return 12;
        if (Id("@").Equals(Id("`"), StringComparison.InvariantCultureIgnoreCase)) return 13;
        if (Id("a").Equals(Id("{"), StringComparison.InvariantCultureIgnoreCase)) return 14;
        // ... and the same characters must still compare equal to themselves.
        if (!Id("[[[[").Equals(Copy(Id("[[[[")), StringComparison.InvariantCultureIgnoreCase)) return 15;
        if (!Id("@").Equals(Copy(Id("@")), StringComparison.InvariantCultureIgnoreCase)) return 16;
        // Digits are not letters either, so they compare exactly.
        if (!Id("ab23").Equals(Id("AB23"), StringComparison.InvariantCultureIgnoreCase)) return 17;
        if (Id("ab23").Equals(Id("AB24"), StringComparison.InvariantCultureIgnoreCase)) return 18;
        // The static overload, and the CurrentCulture spelling of the same option.
        if (!String.Equals(Id("abcdefgh"), Id("ABCDEFGH"), StringComparison.InvariantCultureIgnoreCase)) return 19;
        if (String.Equals(Id("abcdefgh"), Id("ABCDEFGG"), StringComparison.InvariantCultureIgnoreCase)) return 20;
        if (!String.Equals(Id("abcd"), Id("ABCD"), StringComparison.CurrentCultureIgnoreCase)) return 21;
        if (String.Equals(Id("[[[["), Id("{{{{"), StringComparison.CurrentCultureIgnoreCase)) return 22;

        return 0;
    }

    // The offsets keep every failure distinguishable while staying inside the 8 bits a process
    // exit code has: the largest value any branch below can return is 94.
    public static int Main(string[] args)
    {
        // Before the pin: the host's real culture, asserted only where the answer cannot depend
        // on it.
        int result = TestHostCulture();
        if (result != 0) return result;

        // Everything below compares against ICU's root collation in the oracle rather than the
        // host's tailoring of it. See the header: without this the file is host-locale-dependent,
        // and measurably fails under th-TH.
        CultureInfo.CurrentCulture = CultureInfo.InvariantCulture;

        result = TestCurrentCulture();
        if (result != 0) return 8 + result;

        result = TestInvariantCulture();
        if (result != 0) return 22 + result;

        result = TestNoneStepShapes();
        if (result != 0) return 38 + result;

        result = TestNoneLengthDelta();
        if (result != 0) return 62 + result;

        result = TestIgnoreCaseStepShapes();
        if (result != 0) return 72 + result;

        return 0;
    }
}
