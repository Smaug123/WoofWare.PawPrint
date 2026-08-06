using System;

namespace BoxedPrimitiveInstanceCallTest
{
    // `box` of a bare primitive stores it inside the boxed type's *own* single instance field
    // (`System.Int64::m_value`, `System.Boolean::m_value`, ...), so the `this` byref the runtime
    // synthesises for a virtual or interface call on a boxed receiver addresses that wrapper
    // rather than the primitive inside it. Every primitive's instance methods open with
    // `ldarg.0; ldind.<width>`, so each such call pops a value type into a primitive slot.
    //
    // `EvalStackValue.toCliTypeCoerced` only knew how to view a value type at int32 and native-int
    // widths, so `((object) 1L).ToString()` — and the same shape at every other width — failed
    // outright. Comparing against the direct (non-boxed) call keeps the expectations honest:
    // the direct call goes through `ldloca` on a plain primitive cell, which always worked.
    class Program
    {
        static int Main(string[] args)
        {
            // --- ldind.i8 ---
            long l = 1234567890123L;
            object bl = l;
            if (bl.GetHashCode() != l.GetHashCode()) { return 1; }
            if (bl.ToString() != l.ToString()) { return 2; }

            ulong ul = 18000000000000000000UL;
            object bul = ul;
            if (bul.GetHashCode() != ul.GetHashCode()) { return 3; }

            // --- ldind.u1 / ldind.i1 ---
            byte by = 200;
            object bby = by;
            if (bby.GetHashCode() != by.GetHashCode()) { return 4; }

            sbyte sb = -56;
            object bsb = sb;
            if (bsb.GetHashCode() != sb.GetHashCode()) { return 5; }

            // --- ldind.i2 / ldind.u2 ---
            short sh = -300;
            object bsh = sh;
            if (bsh.GetHashCode() != sh.GetHashCode()) { return 6; }

            ushort us = 60000;
            object bus = us;
            if (bus.GetHashCode() != us.GetHashCode()) { return 7; }

            // --- ldind.i4 / ldind.u4, which already worked and must keep working ---
            int i = -70000;
            object bi = i;
            if (bi.GetHashCode() != i.GetHashCode()) { return 8; }

            uint ui = 4000000000u;
            object bui = ui;
            if (bui.GetHashCode() != ui.GetHashCode()) { return 9; }

            // --- bool and char cells, which are not `CliType.Numeric` at rest ---
            bool bo = true;
            object bbo = bo;
            if (bbo.GetHashCode() != bo.GetHashCode()) { return 10; }
            if (bbo.ToString() != "True") { return 11; }

            char c = 'q';
            object bc = c;
            if (bc.GetHashCode() != c.GetHashCode()) { return 12; }

            // --- ldind.r8 / ldind.r4 ---
            double d = 2.25;
            object bd = d;
            if (bd.GetHashCode() != d.GetHashCode()) { return 13; }

            float f = 1.5f;
            object bf = f;
            if (bf.GetHashCode() != f.GetHashCode()) { return 14; }

            // --- interface dispatch on a boxed receiver reaches the same byref; this is the
            //     shape that first surfaced the gap, via `Int64::TryFormat`. ---
            ISpanFormattable sf = (ISpanFormattable)bl;
            Span<char> buf = stackalloc char[32];
            int written;
            if (!sf.TryFormat(buf, out written, default, null)) { return 15; }
            if (written != 13) { return 16; }
            if (new string(buf.Slice(0, written)) != "1234567890123") { return 17; }

            IFormattable fo = (IFormattable)bl;
            if (fo.ToString(null, null) != "1234567890123") { return 18; }

            // --- Int64::Equals(object) also reads `this` with ldind.i8 ---
            if (!bl.Equals(1234567890123L)) { return 19; }
            if (bl.Equals(7L)) { return 20; }

            return 0;
        }
    }
}
