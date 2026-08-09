namespace HelloWorldApp
{
    // Roslyn compiles a constant array initialiser of four or more elements into a `newarr`
    // plus `RuntimeHelpers.InitializeArray` against an RVA field in <PrivateImplementationDetails>.
    // Enums are eligible: CoreCLR normalises an enum's MethodTable element type to its
    // underlying integer, so the "element type must be primitive" gate in InitializeArray
    // admits them.
    enum ByteEnum : byte
    {
        Zero = 0,
        One = 1,
        Big = 200,
    }

    enum ShortEnum : short
    {
        Neg = -3,
        Zero = 0,
        Big = 30000,
    }

    enum IntEnum
    {
        A = 0,
        B = 1,
        C = -1,
        D = 1000000,
    }

    enum LongEnum : long
    {
        A = 0,
        B = -5,
        C = 9000000000L,
    }

    class Program
    {
        static int Main(string[] args)
        {
            ByteEnum[] bytes = new[] { ByteEnum.Big, ByteEnum.Zero, ByteEnum.One, ByteEnum.Big, ByteEnum.One };
            if (bytes.Length != 5) return 1;
            if (bytes[0] != ByteEnum.Big) return 2;
            if (bytes[1] != ByteEnum.Zero) return 3;
            if (bytes[2] != ByteEnum.One) return 4;
            if (bytes[3] != ByteEnum.Big) return 5;
            if (bytes[4] != ByteEnum.One) return 6;

            ShortEnum[] shorts = new[] { ShortEnum.Big, ShortEnum.Neg, ShortEnum.Zero, ShortEnum.Big, ShortEnum.Neg };
            if (shorts[0] != ShortEnum.Big) return 7;
            if (shorts[1] != ShortEnum.Neg) return 8;
            if (shorts[2] != ShortEnum.Zero) return 9;
            if (shorts[3] != ShortEnum.Big) return 10;
            if (shorts[4] != ShortEnum.Neg) return 11;

            IntEnum[] ints = new[] { IntEnum.D, IntEnum.C, IntEnum.B, IntEnum.A, IntEnum.D };
            if (ints[0] != IntEnum.D) return 12;
            if (ints[1] != IntEnum.C) return 13;
            if (ints[2] != IntEnum.B) return 14;
            if (ints[3] != IntEnum.A) return 15;
            if (ints[4] != IntEnum.D) return 16;

            LongEnum[] longs = new[] { LongEnum.C, LongEnum.B, LongEnum.A, LongEnum.C, LongEnum.B };
            if (longs[0] != LongEnum.C) return 17;
            if (longs[1] != LongEnum.B) return 18;
            if (longs[2] != LongEnum.A) return 19;
            if (longs[3] != LongEnum.C) return 20;
            if (longs[4] != LongEnum.B) return 21;

            // The underlying integer must round-trip too, not just enum-to-enum comparison.
            if ((byte)bytes[0] != 200) return 22;
            if ((short)shorts[1] != -3) return 23;
            if ((int)ints[1] != -1) return 24;
            if ((long)longs[0] != 9000000000L) return 25;

            // A jagged array of enums: the shape Newtonsoft.Json's JsonWriter cctor builds.
            IntEnum[][] jagged = new[]
            {
                new[] { IntEnum.A, IntEnum.B, IntEnum.C, IntEnum.D, IntEnum.A },
                new[] { IntEnum.D, IntEnum.C, IntEnum.B, IntEnum.A, IntEnum.D },
            };
            if (jagged[0][3] != IntEnum.D) return 26;
            if (jagged[1][0] != IntEnum.D) return 27;
            if (jagged[1][4] != IntEnum.D) return 28;

            return 0;
        }
    }
}
