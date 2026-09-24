// A method with more parameters than a one-byte index can name. Roslyn addresses a parameter below
// 255 with a short form (`ldarg.s`, `ldarga.s`, `starg.s`) and every parameter from 255 up with
// the long forms (`ldarg`, `ldarga`, `starg`), whose index is two bytes. The checks write to slots
// whose index is 256 more than a slot they then read, so an index truncated to its low byte
// would clobber the wrong parameter.

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

    static int Wide(
        int p0, int p1, int p2, int p3, int p4, int p5, int p6, int p7, int p8, int p9,
        int p10, int p11, int p12, int p13, int p14, int p15, int p16, int p17, int p18, int p19,
        int p20, int p21, int p22, int p23, int p24, int p25, int p26, int p27, int p28, int p29,
        int p30, int p31, int p32, int p33, int p34, int p35, int p36, int p37, int p38, int p39,
        int p40, int p41, int p42, int p43, int p44, int p45, int p46, int p47, int p48, int p49,
        int p50, int p51, int p52, int p53, int p54, int p55, int p56, int p57, int p58, int p59,
        int p60, int p61, int p62, int p63, int p64, int p65, int p66, int p67, int p68, int p69,
        int p70, int p71, int p72, int p73, int p74, int p75, int p76, int p77, int p78, int p79,
        int p80, int p81, int p82, int p83, int p84, int p85, int p86, int p87, int p88, int p89,
        int p90, int p91, int p92, int p93, int p94, int p95, int p96, int p97, int p98, int p99,
        int p100, int p101, int p102, int p103, int p104, int p105, int p106, int p107, int p108, int p109,
        int p110, int p111, int p112, int p113, int p114, int p115, int p116, int p117, int p118, int p119,
        int p120, int p121, int p122, int p123, int p124, int p125, int p126, int p127, int p128, int p129,
        int p130, int p131, int p132, int p133, int p134, int p135, int p136, int p137, int p138, int p139,
        int p140, int p141, int p142, int p143, int p144, int p145, int p146, int p147, int p148, int p149,
        int p150, int p151, int p152, int p153, int p154, int p155, int p156, int p157, int p158, int p159,
        int p160, int p161, int p162, int p163, int p164, int p165, int p166, int p167, int p168, int p169,
        int p170, int p171, int p172, int p173, int p174, int p175, int p176, int p177, int p178, int p179,
        int p180, int p181, int p182, int p183, int p184, int p185, int p186, int p187, int p188, int p189,
        int p190, int p191, int p192, int p193, int p194, int p195, int p196, int p197, int p198, int p199,
        int p200, int p201, int p202, int p203, int p204, int p205, int p206, int p207, int p208, int p209,
        int p210, int p211, int p212, int p213, int p214, int p215, int p216, int p217, int p218, int p219,
        int p220, int p221, int p222, int p223, int p224, int p225, int p226, int p227, int p228, int p229,
        int p230, int p231, int p232, int p233, int p234, int p235, int p236, int p237, int p238, int p239,
        int p240, int p241, int p242, int p243, int p244, int p245, int p246, int p247, int p248, int p249,
        int p250, int p251, int p252, int p253, int p254, int p255, int p256, int p257, int p258, int p259,
        int p260, int p261, int p262, int p263, int p264, int p265, int p266, int p267, int p268, int p269,
        int p270, int p271, int p272, int p273, int p274, int p275, int p276, int p277, int p278, int p279,
        int p280, int p281, int p282, int p283, int p284, int p285, int p286, int p287, int p288, int p289,
        int p290, int p291, int p292, int p293, int p294, int p295, int p296, int p297, long p298, string p299,
        Pair p300)
    {
        if (p0 != 0) return 1;
        if (p255 != 255) return 2;
        if (p256 != 256) return 3;
        if (p297 != 297) return 4;
        if (p298 != 298L) return 5;
        if (p299 != "s299") return 6;
        if (p300.A != 300 || p300.B != 3000L) return 7;

        p257 = -1;
        if (p257 != -1) return 8;
        if (p1 != 1) return 9;

        Bump(ref p258);
        if (p258 != 1258) return 10;
        if (p2 != 2) return 11;

        p300.A = 42;
        if (p300.A != 42 || p300.B != 3000L) return 12;
        if (p44 != 44) return 13;

        p299 = p299 + "!";
        if (p299 != "s299!") return 14;

        ref long r = ref p298;
        r = -298L;
        if (p298 != -298L) return 15;
        if (p42 != 42) return 16;

        return 0;
    }

    int offset;

    // An instance method's `this` is argument 0, so its parameter q299 is argument 300.
    int Instance(
        int q0, int q1, int q2, int q3, int q4, int q5, int q6, int q7, int q8, int q9,
        int q10, int q11, int q12, int q13, int q14, int q15, int q16, int q17, int q18, int q19,
        int q20, int q21, int q22, int q23, int q24, int q25, int q26, int q27, int q28, int q29,
        int q30, int q31, int q32, int q33, int q34, int q35, int q36, int q37, int q38, int q39,
        int q40, int q41, int q42, int q43, int q44, int q45, int q46, int q47, int q48, int q49,
        int q50, int q51, int q52, int q53, int q54, int q55, int q56, int q57, int q58, int q59,
        int q60, int q61, int q62, int q63, int q64, int q65, int q66, int q67, int q68, int q69,
        int q70, int q71, int q72, int q73, int q74, int q75, int q76, int q77, int q78, int q79,
        int q80, int q81, int q82, int q83, int q84, int q85, int q86, int q87, int q88, int q89,
        int q90, int q91, int q92, int q93, int q94, int q95, int q96, int q97, int q98, int q99,
        int q100, int q101, int q102, int q103, int q104, int q105, int q106, int q107, int q108, int q109,
        int q110, int q111, int q112, int q113, int q114, int q115, int q116, int q117, int q118, int q119,
        int q120, int q121, int q122, int q123, int q124, int q125, int q126, int q127, int q128, int q129,
        int q130, int q131, int q132, int q133, int q134, int q135, int q136, int q137, int q138, int q139,
        int q140, int q141, int q142, int q143, int q144, int q145, int q146, int q147, int q148, int q149,
        int q150, int q151, int q152, int q153, int q154, int q155, int q156, int q157, int q158, int q159,
        int q160, int q161, int q162, int q163, int q164, int q165, int q166, int q167, int q168, int q169,
        int q170, int q171, int q172, int q173, int q174, int q175, int q176, int q177, int q178, int q179,
        int q180, int q181, int q182, int q183, int q184, int q185, int q186, int q187, int q188, int q189,
        int q190, int q191, int q192, int q193, int q194, int q195, int q196, int q197, int q198, int q199,
        int q200, int q201, int q202, int q203, int q204, int q205, int q206, int q207, int q208, int q209,
        int q210, int q211, int q212, int q213, int q214, int q215, int q216, int q217, int q218, int q219,
        int q220, int q221, int q222, int q223, int q224, int q225, int q226, int q227, int q228, int q229,
        int q230, int q231, int q232, int q233, int q234, int q235, int q236, int q237, int q238, int q239,
        int q240, int q241, int q242, int q243, int q244, int q245, int q246, int q247, int q248, int q249,
        int q250, int q251, int q252, int q253, int q254, int q255, int q256, int q257, int q258, int q259,
        int q260, int q261, int q262, int q263, int q264, int q265, int q266, int q267, int q268, int q269,
        int q270, int q271, int q272, int q273, int q274, int q275, int q276, int q277, int q278, int q279,
        int q280, int q281, int q282, int q283, int q284, int q285, int q286, int q287, int q288, int q289,
        int q290, int q291, int q292, int q293, int q294, int q295, int q296, int q297, int q298, int q299)
    {
        q256 = q256 + offset;
        Bump(ref q299);
        return q0 + q1 + q255 + q256 + q299;
    }

    public static int Main(string[] argv)
    {
        int wide = Wide(
            0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
            10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
            20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
            30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
            40, 41, 42, 43, 44, 45, 46, 47, 48, 49,
            50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
            60, 61, 62, 63, 64, 65, 66, 67, 68, 69,
            70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
            80, 81, 82, 83, 84, 85, 86, 87, 88, 89,
            90, 91, 92, 93, 94, 95, 96, 97, 98, 99,
            100, 101, 102, 103, 104, 105, 106, 107, 108, 109,
            110, 111, 112, 113, 114, 115, 116, 117, 118, 119,
            120, 121, 122, 123, 124, 125, 126, 127, 128, 129,
            130, 131, 132, 133, 134, 135, 136, 137, 138, 139,
            140, 141, 142, 143, 144, 145, 146, 147, 148, 149,
            150, 151, 152, 153, 154, 155, 156, 157, 158, 159,
            160, 161, 162, 163, 164, 165, 166, 167, 168, 169,
            170, 171, 172, 173, 174, 175, 176, 177, 178, 179,
            180, 181, 182, 183, 184, 185, 186, 187, 188, 189,
            190, 191, 192, 193, 194, 195, 196, 197, 198, 199,
            200, 201, 202, 203, 204, 205, 206, 207, 208, 209,
            210, 211, 212, 213, 214, 215, 216, 217, 218, 219,
            220, 221, 222, 223, 224, 225, 226, 227, 228, 229,
            230, 231, 232, 233, 234, 235, 236, 237, 238, 239,
            240, 241, 242, 243, 244, 245, 246, 247, 248, 249,
            250, 251, 252, 253, 254, 255, 256, 257, 258, 259,
            260, 261, 262, 263, 264, 265, 266, 267, 268, 269,
            270, 271, 272, 273, 274, 275, 276, 277, 278, 279,
            280, 281, 282, 283, 284, 285, 286, 287, 288, 289,
            290, 291, 292, 293, 294, 295, 296, 297, 298L, "s299",
            new Pair { A = 300, B = 3000L });
        if (wide != 0) return wide;

        var p = new Program { offset = 5 };
        int instance = p.Instance(
            0, 3, 6, 9, 12, 15, 18, 21, 24, 27,
            30, 33, 36, 39, 42, 45, 48, 51, 54, 57,
            60, 63, 66, 69, 72, 75, 78, 81, 84, 87,
            90, 93, 96, 99, 102, 105, 108, 111, 114, 117,
            120, 123, 126, 129, 132, 135, 138, 141, 144, 147,
            150, 153, 156, 159, 162, 165, 168, 171, 174, 177,
            180, 183, 186, 189, 192, 195, 198, 201, 204, 207,
            210, 213, 216, 219, 222, 225, 228, 231, 234, 237,
            240, 243, 246, 249, 252, 255, 258, 261, 264, 267,
            270, 273, 276, 279, 282, 285, 288, 291, 294, 297,
            300, 303, 306, 309, 312, 315, 318, 321, 324, 327,
            330, 333, 336, 339, 342, 345, 348, 351, 354, 357,
            360, 363, 366, 369, 372, 375, 378, 381, 384, 387,
            390, 393, 396, 399, 402, 405, 408, 411, 414, 417,
            420, 423, 426, 429, 432, 435, 438, 441, 444, 447,
            450, 453, 456, 459, 462, 465, 468, 471, 474, 477,
            480, 483, 486, 489, 492, 495, 498, 501, 504, 507,
            510, 513, 516, 519, 522, 525, 528, 531, 534, 537,
            540, 543, 546, 549, 552, 555, 558, 561, 564, 567,
            570, 573, 576, 579, 582, 585, 588, 591, 594, 597,
            600, 603, 606, 609, 612, 615, 618, 621, 624, 627,
            630, 633, 636, 639, 642, 645, 648, 651, 654, 657,
            660, 663, 666, 669, 672, 675, 678, 681, 684, 687,
            690, 693, 696, 699, 702, 705, 708, 711, 714, 717,
            720, 723, 726, 729, 732, 735, 738, 741, 744, 747,
            750, 753, 756, 759, 762, 765, 768, 771, 774, 777,
            780, 783, 786, 789, 792, 795, 798, 801, 804, 807,
            810, 813, 816, 819, 822, 825, 828, 831, 834, 837,
            840, 843, 846, 849, 852, 855, 858, 861, 864, 867,
            870, 873, 876, 879, 882, 885, 888, 891, 894, 897);
        if (instance != 0 + 3 + 765 + (768 + 5) + (897 + 1000)) return 20;

        return 0;
    }
}
