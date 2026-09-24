// A method with more locals than a one-byte index can name. Roslyn addresses a local below 255
// with a short form (`ldloc.s`, `ldloca.s`, `stloc.s`) and every local from 255 up with the long
// forms (`ldloc`, `ldloca`, `stloc`), whose index is two bytes. The checks write to slots whose
// index is 256 more than a slot they then read, so an index truncated to its low byte would
// clobber the wrong local.

public class Program
{
    public struct Pair
    {
        public int A;
        public long B;
    }

    static void Bump(ref int x)
    {
        x += 1000;
    }

    static int Run(int seed)
    {
        int l0 = seed;
        int l1 = l0 + 1;
        int l2 = l1 + 1;
        int l3 = l2 + 1;
        int l4 = l3 + 1;
        int l5 = l4 + 1;
        int l6 = l5 + 1;
        int l7 = l6 + 1;
        int l8 = l7 + 1;
        int l9 = l8 + 1;
        int l10 = l9 + 1;
        int l11 = l10 + 1;
        int l12 = l11 + 1;
        int l13 = l12 + 1;
        int l14 = l13 + 1;
        int l15 = l14 + 1;
        int l16 = l15 + 1;
        int l17 = l16 + 1;
        int l18 = l17 + 1;
        int l19 = l18 + 1;
        int l20 = l19 + 1;
        int l21 = l20 + 1;
        int l22 = l21 + 1;
        int l23 = l22 + 1;
        int l24 = l23 + 1;
        int l25 = l24 + 1;
        int l26 = l25 + 1;
        int l27 = l26 + 1;
        int l28 = l27 + 1;
        int l29 = l28 + 1;
        int l30 = l29 + 1;
        int l31 = l30 + 1;
        int l32 = l31 + 1;
        int l33 = l32 + 1;
        int l34 = l33 + 1;
        int l35 = l34 + 1;
        int l36 = l35 + 1;
        int l37 = l36 + 1;
        int l38 = l37 + 1;
        int l39 = l38 + 1;
        int l40 = l39 + 1;
        int l41 = l40 + 1;
        int l42 = l41 + 1;
        int l43 = l42 + 1;
        int l44 = l43 + 1;
        int l45 = l44 + 1;
        int l46 = l45 + 1;
        int l47 = l46 + 1;
        int l48 = l47 + 1;
        int l49 = l48 + 1;
        int l50 = l49 + 1;
        int l51 = l50 + 1;
        int l52 = l51 + 1;
        int l53 = l52 + 1;
        int l54 = l53 + 1;
        int l55 = l54 + 1;
        int l56 = l55 + 1;
        int l57 = l56 + 1;
        int l58 = l57 + 1;
        int l59 = l58 + 1;
        int l60 = l59 + 1;
        int l61 = l60 + 1;
        int l62 = l61 + 1;
        int l63 = l62 + 1;
        int l64 = l63 + 1;
        int l65 = l64 + 1;
        int l66 = l65 + 1;
        int l67 = l66 + 1;
        int l68 = l67 + 1;
        int l69 = l68 + 1;
        int l70 = l69 + 1;
        int l71 = l70 + 1;
        int l72 = l71 + 1;
        int l73 = l72 + 1;
        int l74 = l73 + 1;
        int l75 = l74 + 1;
        int l76 = l75 + 1;
        int l77 = l76 + 1;
        int l78 = l77 + 1;
        int l79 = l78 + 1;
        int l80 = l79 + 1;
        int l81 = l80 + 1;
        int l82 = l81 + 1;
        int l83 = l82 + 1;
        int l84 = l83 + 1;
        int l85 = l84 + 1;
        int l86 = l85 + 1;
        int l87 = l86 + 1;
        int l88 = l87 + 1;
        int l89 = l88 + 1;
        int l90 = l89 + 1;
        int l91 = l90 + 1;
        int l92 = l91 + 1;
        int l93 = l92 + 1;
        int l94 = l93 + 1;
        int l95 = l94 + 1;
        int l96 = l95 + 1;
        int l97 = l96 + 1;
        int l98 = l97 + 1;
        int l99 = l98 + 1;
        int l100 = l99 + 1;
        int l101 = l100 + 1;
        int l102 = l101 + 1;
        int l103 = l102 + 1;
        int l104 = l103 + 1;
        int l105 = l104 + 1;
        int l106 = l105 + 1;
        int l107 = l106 + 1;
        int l108 = l107 + 1;
        int l109 = l108 + 1;
        int l110 = l109 + 1;
        int l111 = l110 + 1;
        int l112 = l111 + 1;
        int l113 = l112 + 1;
        int l114 = l113 + 1;
        int l115 = l114 + 1;
        int l116 = l115 + 1;
        int l117 = l116 + 1;
        int l118 = l117 + 1;
        int l119 = l118 + 1;
        int l120 = l119 + 1;
        int l121 = l120 + 1;
        int l122 = l121 + 1;
        int l123 = l122 + 1;
        int l124 = l123 + 1;
        int l125 = l124 + 1;
        int l126 = l125 + 1;
        int l127 = l126 + 1;
        int l128 = l127 + 1;
        int l129 = l128 + 1;
        int l130 = l129 + 1;
        int l131 = l130 + 1;
        int l132 = l131 + 1;
        int l133 = l132 + 1;
        int l134 = l133 + 1;
        int l135 = l134 + 1;
        int l136 = l135 + 1;
        int l137 = l136 + 1;
        int l138 = l137 + 1;
        int l139 = l138 + 1;
        int l140 = l139 + 1;
        int l141 = l140 + 1;
        int l142 = l141 + 1;
        int l143 = l142 + 1;
        int l144 = l143 + 1;
        int l145 = l144 + 1;
        int l146 = l145 + 1;
        int l147 = l146 + 1;
        int l148 = l147 + 1;
        int l149 = l148 + 1;
        int l150 = l149 + 1;
        int l151 = l150 + 1;
        int l152 = l151 + 1;
        int l153 = l152 + 1;
        int l154 = l153 + 1;
        int l155 = l154 + 1;
        int l156 = l155 + 1;
        int l157 = l156 + 1;
        int l158 = l157 + 1;
        int l159 = l158 + 1;
        int l160 = l159 + 1;
        int l161 = l160 + 1;
        int l162 = l161 + 1;
        int l163 = l162 + 1;
        int l164 = l163 + 1;
        int l165 = l164 + 1;
        int l166 = l165 + 1;
        int l167 = l166 + 1;
        int l168 = l167 + 1;
        int l169 = l168 + 1;
        int l170 = l169 + 1;
        int l171 = l170 + 1;
        int l172 = l171 + 1;
        int l173 = l172 + 1;
        int l174 = l173 + 1;
        int l175 = l174 + 1;
        int l176 = l175 + 1;
        int l177 = l176 + 1;
        int l178 = l177 + 1;
        int l179 = l178 + 1;
        int l180 = l179 + 1;
        int l181 = l180 + 1;
        int l182 = l181 + 1;
        int l183 = l182 + 1;
        int l184 = l183 + 1;
        int l185 = l184 + 1;
        int l186 = l185 + 1;
        int l187 = l186 + 1;
        int l188 = l187 + 1;
        int l189 = l188 + 1;
        int l190 = l189 + 1;
        int l191 = l190 + 1;
        int l192 = l191 + 1;
        int l193 = l192 + 1;
        int l194 = l193 + 1;
        int l195 = l194 + 1;
        int l196 = l195 + 1;
        int l197 = l196 + 1;
        int l198 = l197 + 1;
        int l199 = l198 + 1;
        int l200 = l199 + 1;
        int l201 = l200 + 1;
        int l202 = l201 + 1;
        int l203 = l202 + 1;
        int l204 = l203 + 1;
        int l205 = l204 + 1;
        int l206 = l205 + 1;
        int l207 = l206 + 1;
        int l208 = l207 + 1;
        int l209 = l208 + 1;
        int l210 = l209 + 1;
        int l211 = l210 + 1;
        int l212 = l211 + 1;
        int l213 = l212 + 1;
        int l214 = l213 + 1;
        int l215 = l214 + 1;
        int l216 = l215 + 1;
        int l217 = l216 + 1;
        int l218 = l217 + 1;
        int l219 = l218 + 1;
        int l220 = l219 + 1;
        int l221 = l220 + 1;
        int l222 = l221 + 1;
        int l223 = l222 + 1;
        int l224 = l223 + 1;
        int l225 = l224 + 1;
        int l226 = l225 + 1;
        int l227 = l226 + 1;
        int l228 = l227 + 1;
        int l229 = l228 + 1;
        int l230 = l229 + 1;
        int l231 = l230 + 1;
        int l232 = l231 + 1;
        int l233 = l232 + 1;
        int l234 = l233 + 1;
        int l235 = l234 + 1;
        int l236 = l235 + 1;
        int l237 = l236 + 1;
        int l238 = l237 + 1;
        int l239 = l238 + 1;
        int l240 = l239 + 1;
        int l241 = l240 + 1;
        int l242 = l241 + 1;
        int l243 = l242 + 1;
        int l244 = l243 + 1;
        int l245 = l244 + 1;
        int l246 = l245 + 1;
        int l247 = l246 + 1;
        int l248 = l247 + 1;
        int l249 = l248 + 1;
        int l250 = l249 + 1;
        int l251 = l250 + 1;
        int l252 = l251 + 1;
        int l253 = l252 + 1;
        int l254 = l253 + 1;
        int l255 = l254 + 1;
        int l256 = l255 + 1;
        int l257 = l256 + 1;
        int l258 = l257 + 1;
        int l259 = l258 + 1;
        int l260 = l259 + 1;
        int l261 = l260 + 1;
        int l262 = l261 + 1;
        int l263 = l262 + 1;
        int l264 = l263 + 1;
        int l265 = l264 + 1;
        int l266 = l265 + 1;
        int l267 = l266 + 1;
        int l268 = l267 + 1;
        int l269 = l268 + 1;
        int l270 = l269 + 1;
        int l271 = l270 + 1;
        int l272 = l271 + 1;
        int l273 = l272 + 1;
        int l274 = l273 + 1;
        int l275 = l274 + 1;
        int l276 = l275 + 1;
        int l277 = l276 + 1;
        int l278 = l277 + 1;
        int l279 = l278 + 1;
        int l280 = l279 + 1;
        int l281 = l280 + 1;
        int l282 = l281 + 1;
        int l283 = l282 + 1;
        int l284 = l283 + 1;
        int l285 = l284 + 1;
        int l286 = l285 + 1;
        int l287 = l286 + 1;
        int l288 = l287 + 1;
        int l289 = l288 + 1;
        int l290 = l289 + 1;
        int l291 = l290 + 1;
        int l292 = l291 + 1;
        int l293 = l292 + 1;
        int l294 = l293 + 1;
        int l295 = l294 + 1;
        int l296 = l295 + 1;
        int l297 = l296 + 1;
        int l298 = l297 + 1;
        int l299 = l298 + 1;
        long big = l299 * 1000000000L;
        string text = "t";
        Pair pair = new Pair { A = l280, B = big };

        int sum =
            l0 + l1 + l2 + l3 + l4 + l5 + l6 + l7 + l8 + l9 + l10 + l11 + l12 + l13 + l14 +
            l15 + l16 + l17 + l18 + l19 + l20 + l21 + l22 + l23 + l24 + l25 + l26 + l27 + l28 + l29 +
            l30 + l31 + l32 + l33 + l34 + l35 + l36 + l37 + l38 + l39 + l40 + l41 + l42 + l43 + l44 +
            l45 + l46 + l47 + l48 + l49 + l50 + l51 + l52 + l53 + l54 + l55 + l56 + l57 + l58 + l59 +
            l60 + l61 + l62 + l63 + l64 + l65 + l66 + l67 + l68 + l69 + l70 + l71 + l72 + l73 + l74 +
            l75 + l76 + l77 + l78 + l79 + l80 + l81 + l82 + l83 + l84 + l85 + l86 + l87 + l88 + l89 +
            l90 + l91 + l92 + l93 + l94 + l95 + l96 + l97 + l98 + l99 + l100 + l101 + l102 + l103 + l104 +
            l105 + l106 + l107 + l108 + l109 + l110 + l111 + l112 + l113 + l114 + l115 + l116 + l117 + l118 + l119 +
            l120 + l121 + l122 + l123 + l124 + l125 + l126 + l127 + l128 + l129 + l130 + l131 + l132 + l133 + l134 +
            l135 + l136 + l137 + l138 + l139 + l140 + l141 + l142 + l143 + l144 + l145 + l146 + l147 + l148 + l149 +
            l150 + l151 + l152 + l153 + l154 + l155 + l156 + l157 + l158 + l159 + l160 + l161 + l162 + l163 + l164 +
            l165 + l166 + l167 + l168 + l169 + l170 + l171 + l172 + l173 + l174 + l175 + l176 + l177 + l178 + l179 +
            l180 + l181 + l182 + l183 + l184 + l185 + l186 + l187 + l188 + l189 + l190 + l191 + l192 + l193 + l194 +
            l195 + l196 + l197 + l198 + l199 + l200 + l201 + l202 + l203 + l204 + l205 + l206 + l207 + l208 + l209 +
            l210 + l211 + l212 + l213 + l214 + l215 + l216 + l217 + l218 + l219 + l220 + l221 + l222 + l223 + l224 +
            l225 + l226 + l227 + l228 + l229 + l230 + l231 + l232 + l233 + l234 + l235 + l236 + l237 + l238 + l239 +
            l240 + l241 + l242 + l243 + l244 + l245 + l246 + l247 + l248 + l249 + l250 + l251 + l252 + l253 + l254 +
            l255 + l256 + l257 + l258 + l259 + l260 + l261 + l262 + l263 + l264 + l265 + l266 + l267 + l268 + l269 +
            l270 + l271 + l272 + l273 + l274 + l275 + l276 + l277 + l278 + l279 + l280 + l281 + l282 + l283 + l284 +
            l285 + l286 + l287 + l288 + l289 + l290 + l291 + l292 + l293 + l294 + l295 + l296 + l297 + l298 + l299;
        if (sum != 300 * seed + 299 * 300 / 2) return 1;

        if (l0 != seed) return 2;
        if (l255 != seed + 255) return 3;
        if (l256 != seed + 256) return 4;
        if (l299 != seed + 299) return 5;

        l257 = -1;
        if (l257 != -1) return 6;
        if (l1 != seed + 1) return 7;

        Bump(ref l258);
        if (l258 != seed + 1258) return 8;
        if (l2 != seed + 2) return 9;

        if (big != (seed + 299) * 1000000000L) return 10;
        text = text + l299;
        if (text != "t" + (seed + 299)) return 11;

        pair.A = pair.A + 1;
        if (pair.A != seed + 281) return 12;
        if (pair.B != big) return 13;

        ref long r = ref big;
        r = -1L;
        if (big != -1L) return 14;

        for (int i = 0; i < 3; i++)
        {
            l299 += i;
        }
        if (l299 != seed + 302) return 15;

        return 0;
    }

    public static int Main(string[] argv)
    {
        return Run(17);
    }
}
