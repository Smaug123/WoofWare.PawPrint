using System;

namespace ArrayPrimitiveLikeElementReadTest
{
    enum EnumSByte : sbyte { Zero = 0, Neg = -3, Max = 127 }
    enum EnumByte : byte { Zero = 0, Big = 200 }
    enum EnumShort : short { Zero = 0, Neg = -3, Max = 32767 }
    enum EnumUShort : ushort { Zero = 0, Big = 40000 }
    enum EnumInt : int { Zero = 0, Neg = -70000 }
    enum EnumLong : long { Zero = 0, Neg = -5000000000L }

    // `newarr` zero-fills cells with the element type's *declared* CliType: `CliType.Bool` for
    // bool[], and a primitive-like value type for nint[]/nuint[]/enum arrays. The concrete-width
    // `ldelem.*` opcodes must accept these declared forms, not only `CliType.Numeric`; and
    // since `stelem.*` stamps the opcode's raw primitive over the cell, only a read of a
    // still-declared-form cell (a fresh array, or one just cleared) exercises this.
    //
    // Every read below is a plain `ldelem.*` on an array; no Span, no byref, no generics. The
    // read-before-any-write cases are the ones at stake.
    class Program
    {
        static int Main(string[] args)
        {
            // --- bool: ldelem.u1 against CliType.Bool cells ---
            bool[] b = new bool[3];
            if (b[0] || b[1] || b[2]) { return 1; }

            b[1] = true;
            if (!b[1] || b[0] || b[2]) { return 2; }

            b[1] = false;
            if (b[1]) { return 3; }

            // --- nint / nuint: ldelem.i against primitive-like IntPtr cells ---
            nint[] ni = new nint[3];
            if (ni[0] != 0 || ni[2] != 0) { return 4; }

            ni[0] = 5;
            ni[1] = -7;
            if (ni[0] != 5 || ni[1] != -7 || ni[2] != 0) { return 5; }

            nuint[] nu = new nuint[2];
            if (nu[0] != 0) { return 6; }

            nu[0] = 9;
            if (nu[0] != 9 || nu[1] != 0) { return 7; }

            // --- enums over every integral underlying type ---
            EnumSByte[] esb = new EnumSByte[2];
            if (esb[0] != EnumSByte.Zero) { return 8; }
            esb[0] = EnumSByte.Neg;
            if (esb[0] != EnumSByte.Neg || (sbyte)esb[0] != -3) { return 9; }
            esb[1] = EnumSByte.Max;
            if ((sbyte)esb[1] != 127) { return 10; }

            EnumByte[] eb = new EnumByte[2];
            if (eb[0] != EnumByte.Zero) { return 11; }
            eb[0] = EnumByte.Big;
            if ((byte)eb[0] != 200) { return 12; }

            EnumShort[] esh = new EnumShort[2];
            if (esh[0] != EnumShort.Zero) { return 13; }
            esh[0] = EnumShort.Neg;
            if ((short)esh[0] != -3) { return 14; }
            esh[1] = EnumShort.Max;
            if ((short)esh[1] != 32767) { return 15; }

            EnumUShort[] eus = new EnumUShort[2];
            if (eus[0] != EnumUShort.Zero) { return 16; }
            eus[0] = EnumUShort.Big;
            if ((ushort)eus[0] != 40000) { return 17; }

            EnumInt[] ei = new EnumInt[2];
            if (ei[0] != EnumInt.Zero) { return 18; }
            ei[0] = EnumInt.Neg;
            if ((int)ei[0] != -70000) { return 19; }

            EnumLong[] el = new EnumLong[2];
            if (el[0] != EnumLong.Zero) { return 20; }
            el[0] = EnumLong.Neg;
            if ((long)el[0] != -5000000000L) { return 21; }

            // --- char: ldelem.u2 against CliType.Char cells ---
            char[] c = new char[2];
            if (c[0] != '\0') { return 22; }
            c[0] = 'q';
            if (c[0] != 'q' || c[1] != '\0') { return 23; }

            // --- sign/zero-extension edges on the raw numeric element types, which already
            // worked and must keep working ---
            sbyte[] sb = new sbyte[2];
            sb[0] = -56;
            if (sb[0] != -56) { return 24; }

            byte[] by = new byte[2];
            by[0] = 200;
            if (by[0] != 200) { return 25; }

            short[] sh = new short[2];
            sh[0] = -300;
            if (sh[0] != -300) { return 26; }

            ushort[] us = new ushort[2];
            us[0] = 60000;
            if (us[0] != 60000) { return 27; }

            // (`Array.Clear` would be the natural way to put a written cell back into its
            // declared form, but it bottoms out in the unimplemented
            // `SpanHelpers.ClearWithoutReferences` P/Invoke. `Span<T>.Clear` reaches the same
            // state and is covered by the sibling SpanBulkWritePrimitiveLike.cs.)

            // --- float element types, for the r4/r8 arms of the same projection ---
            float[] f = new float[2];
            if (f[0] != 0.0f) { return 31; }
            f[0] = 1.5f;
            if (f[0] != 1.5f) { return 32; }

            double[] d = new double[2];
            if (d[0] != 0.0) { return 33; }
            d[0] = 2.25;
            if (d[0] != 2.25) { return 34; }

            return 0;
        }
    }
}
