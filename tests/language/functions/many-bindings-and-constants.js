/*---
description: Functions with more than 255 bindings and constants run correctly
features: [let-const, arrow-functions]
---*/

// Register and constant indices above 255 do not fit an instruction's 8-bit
// operands, so the compiler prefixes those instructions with OP_WIDE. Every
// test here reaches such an index.

const manyLocals = () => {
  const v0 = 0;
  const v1 = 1;
  const v2 = 2;
  const v3 = 3;
  const v4 = 4;
  const v5 = 5;
  const v6 = 6;
  const v7 = 7;
  const v8 = 8;
  const v9 = 9;
  const v10 = 10;
  const v11 = 11;
  const v12 = 12;
  const v13 = 13;
  const v14 = 14;
  const v15 = 15;
  const v16 = 16;
  const v17 = 17;
  const v18 = 18;
  const v19 = 19;
  const v20 = 20;
  const v21 = 21;
  const v22 = 22;
  const v23 = 23;
  const v24 = 24;
  const v25 = 25;
  const v26 = 26;
  const v27 = 27;
  const v28 = 28;
  const v29 = 29;
  const v30 = 30;
  const v31 = 31;
  const v32 = 32;
  const v33 = 33;
  const v34 = 34;
  const v35 = 35;
  const v36 = 36;
  const v37 = 37;
  const v38 = 38;
  const v39 = 39;
  const v40 = 40;
  const v41 = 41;
  const v42 = 42;
  const v43 = 43;
  const v44 = 44;
  const v45 = 45;
  const v46 = 46;
  const v47 = 47;
  const v48 = 48;
  const v49 = 49;
  const v50 = 50;
  const v51 = 51;
  const v52 = 52;
  const v53 = 53;
  const v54 = 54;
  const v55 = 55;
  const v56 = 56;
  const v57 = 57;
  const v58 = 58;
  const v59 = 59;
  const v60 = 60;
  const v61 = 61;
  const v62 = 62;
  const v63 = 63;
  const v64 = 64;
  const v65 = 65;
  const v66 = 66;
  const v67 = 67;
  const v68 = 68;
  const v69 = 69;
  const v70 = 70;
  const v71 = 71;
  const v72 = 72;
  const v73 = 73;
  const v74 = 74;
  const v75 = 75;
  const v76 = 76;
  const v77 = 77;
  const v78 = 78;
  const v79 = 79;
  const v80 = 80;
  const v81 = 81;
  const v82 = 82;
  const v83 = 83;
  const v84 = 84;
  const v85 = 85;
  const v86 = 86;
  const v87 = 87;
  const v88 = 88;
  const v89 = 89;
  const v90 = 90;
  const v91 = 91;
  const v92 = 92;
  const v93 = 93;
  const v94 = 94;
  const v95 = 95;
  const v96 = 96;
  const v97 = 97;
  const v98 = 98;
  const v99 = 99;
  const v100 = 100;
  const v101 = 101;
  const v102 = 102;
  const v103 = 103;
  const v104 = 104;
  const v105 = 105;
  const v106 = 106;
  const v107 = 107;
  const v108 = 108;
  const v109 = 109;
  const v110 = 110;
  const v111 = 111;
  const v112 = 112;
  const v113 = 113;
  const v114 = 114;
  const v115 = 115;
  const v116 = 116;
  const v117 = 117;
  const v118 = 118;
  const v119 = 119;
  const v120 = 120;
  const v121 = 121;
  const v122 = 122;
  const v123 = 123;
  const v124 = 124;
  const v125 = 125;
  const v126 = 126;
  const v127 = 127;
  const v128 = 128;
  const v129 = 129;
  const v130 = 130;
  const v131 = 131;
  const v132 = 132;
  const v133 = 133;
  const v134 = 134;
  const v135 = 135;
  const v136 = 136;
  const v137 = 137;
  const v138 = 138;
  const v139 = 139;
  const v140 = 140;
  const v141 = 141;
  const v142 = 142;
  const v143 = 143;
  const v144 = 144;
  const v145 = 145;
  const v146 = 146;
  const v147 = 147;
  const v148 = 148;
  const v149 = 149;
  const v150 = 150;
  const v151 = 151;
  const v152 = 152;
  const v153 = 153;
  const v154 = 154;
  const v155 = 155;
  const v156 = 156;
  const v157 = 157;
  const v158 = 158;
  const v159 = 159;
  const v160 = 160;
  const v161 = 161;
  const v162 = 162;
  const v163 = 163;
  const v164 = 164;
  const v165 = 165;
  const v166 = 166;
  const v167 = 167;
  const v168 = 168;
  const v169 = 169;
  const v170 = 170;
  const v171 = 171;
  const v172 = 172;
  const v173 = 173;
  const v174 = 174;
  const v175 = 175;
  const v176 = 176;
  const v177 = 177;
  const v178 = 178;
  const v179 = 179;
  const v180 = 180;
  const v181 = 181;
  const v182 = 182;
  const v183 = 183;
  const v184 = 184;
  const v185 = 185;
  const v186 = 186;
  const v187 = 187;
  const v188 = 188;
  const v189 = 189;
  const v190 = 190;
  const v191 = 191;
  const v192 = 192;
  const v193 = 193;
  const v194 = 194;
  const v195 = 195;
  const v196 = 196;
  const v197 = 197;
  const v198 = 198;
  const v199 = 199;
  const v200 = 200;
  const v201 = 201;
  const v202 = 202;
  const v203 = 203;
  const v204 = 204;
  const v205 = 205;
  const v206 = 206;
  const v207 = 207;
  const v208 = 208;
  const v209 = 209;
  const v210 = 210;
  const v211 = 211;
  const v212 = 212;
  const v213 = 213;
  const v214 = 214;
  const v215 = 215;
  const v216 = 216;
  const v217 = 217;
  const v218 = 218;
  const v219 = 219;
  const v220 = 220;
  const v221 = 221;
  const v222 = 222;
  const v223 = 223;
  const v224 = 224;
  const v225 = 225;
  const v226 = 226;
  const v227 = 227;
  const v228 = 228;
  const v229 = 229;
  const v230 = 230;
  const v231 = 231;
  const v232 = 232;
  const v233 = 233;
  const v234 = 234;
  const v235 = 235;
  const v236 = 236;
  const v237 = 237;
  const v238 = 238;
  const v239 = 239;
  const v240 = 240;
  const v241 = 241;
  const v242 = 242;
  const v243 = 243;
  const v244 = 244;
  const v245 = 245;
  const v246 = 246;
  const v247 = 247;
  const v248 = 248;
  const v249 = 249;
  const v250 = 250;
  const v251 = 251;
  const v252 = 252;
  const v253 = 253;
  const v254 = 254;
  const v255 = 255;
  const v256 = 256;
  const v257 = 257;
  const v258 = 258;
  const v259 = 259;
  const v260 = 260;
  const v261 = 261;
  const v262 = 262;
  const v263 = 263;
  const v264 = 264;
  const v265 = 265;
  const v266 = 266;
  const v267 = 267;
  const v268 = 268;
  const v269 = 269;
  const v270 = 270;
  const v271 = 271;
  const v272 = 272;
  const v273 = 273;
  const v274 = 274;
  const v275 = 275;
  const v276 = 276;
  const v277 = 277;
  const v278 = 278;
  const v279 = 279;
  const v280 = 280;
  const v281 = 281;
  const v282 = 282;
  const v283 = 283;
  const v284 = 284;
  const v285 = 285;
  const v286 = 286;
  const v287 = 287;
  const v288 = 288;
  const v289 = 289;
  const v290 = 290;
  const v291 = 291;
  const v292 = 292;
  const v293 = 293;
  const v294 = 294;
  const v295 = 295;
  const v296 = 296;
  const v297 = 297;
  const v298 = 298;
  const v299 = 299;
  let sum = 0;
  sum = v299 + v298 * 2 - v255;
  const read = () => v299 + v297;
  return [sum, read(), v0, v150];
};

const manyStrings = () => [
  "s0",
  "s1",
  "s2",
  "s3",
  "s4",
  "s5",
  "s6",
  "s7",
  "s8",
  "s9",
  "s10",
  "s11",
  "s12",
  "s13",
  "s14",
  "s15",
  "s16",
  "s17",
  "s18",
  "s19",
  "s20",
  "s21",
  "s22",
  "s23",
  "s24",
  "s25",
  "s26",
  "s27",
  "s28",
  "s29",
  "s30",
  "s31",
  "s32",
  "s33",
  "s34",
  "s35",
  "s36",
  "s37",
  "s38",
  "s39",
  "s40",
  "s41",
  "s42",
  "s43",
  "s44",
  "s45",
  "s46",
  "s47",
  "s48",
  "s49",
  "s50",
  "s51",
  "s52",
  "s53",
  "s54",
  "s55",
  "s56",
  "s57",
  "s58",
  "s59",
  "s60",
  "s61",
  "s62",
  "s63",
  "s64",
  "s65",
  "s66",
  "s67",
  "s68",
  "s69",
  "s70",
  "s71",
  "s72",
  "s73",
  "s74",
  "s75",
  "s76",
  "s77",
  "s78",
  "s79",
  "s80",
  "s81",
  "s82",
  "s83",
  "s84",
  "s85",
  "s86",
  "s87",
  "s88",
  "s89",
  "s90",
  "s91",
  "s92",
  "s93",
  "s94",
  "s95",
  "s96",
  "s97",
  "s98",
  "s99",
  "s100",
  "s101",
  "s102",
  "s103",
  "s104",
  "s105",
  "s106",
  "s107",
  "s108",
  "s109",
  "s110",
  "s111",
  "s112",
  "s113",
  "s114",
  "s115",
  "s116",
  "s117",
  "s118",
  "s119",
  "s120",
  "s121",
  "s122",
  "s123",
  "s124",
  "s125",
  "s126",
  "s127",
  "s128",
  "s129",
  "s130",
  "s131",
  "s132",
  "s133",
  "s134",
  "s135",
  "s136",
  "s137",
  "s138",
  "s139",
  "s140",
  "s141",
  "s142",
  "s143",
  "s144",
  "s145",
  "s146",
  "s147",
  "s148",
  "s149",
  "s150",
  "s151",
  "s152",
  "s153",
  "s154",
  "s155",
  "s156",
  "s157",
  "s158",
  "s159",
  "s160",
  "s161",
  "s162",
  "s163",
  "s164",
  "s165",
  "s166",
  "s167",
  "s168",
  "s169",
  "s170",
  "s171",
  "s172",
  "s173",
  "s174",
  "s175",
  "s176",
  "s177",
  "s178",
  "s179",
  "s180",
  "s181",
  "s182",
  "s183",
  "s184",
  "s185",
  "s186",
  "s187",
  "s188",
  "s189",
  "s190",
  "s191",
  "s192",
  "s193",
  "s194",
  "s195",
  "s196",
  "s197",
  "s198",
  "s199",
  "s200",
  "s201",
  "s202",
  "s203",
  "s204",
  "s205",
  "s206",
  "s207",
  "s208",
  "s209",
  "s210",
  "s211",
  "s212",
  "s213",
  "s214",
  "s215",
  "s216",
  "s217",
  "s218",
  "s219",
  "s220",
  "s221",
  "s222",
  "s223",
  "s224",
  "s225",
  "s226",
  "s227",
  "s228",
  "s229",
  "s230",
  "s231",
  "s232",
  "s233",
  "s234",
  "s235",
  "s236",
  "s237",
  "s238",
  "s239",
  "s240",
  "s241",
  "s242",
  "s243",
  "s244",
  "s245",
  "s246",
  "s247",
  "s248",
  "s249",
  "s250",
  "s251",
  "s252",
  "s253",
  "s254",
  "s255",
  "s256",
  "s257",
  "s258",
  "s259",
  "s260",
  "s261",
  "s262",
  "s263",
  "s264",
  "s265",
  "s266",
  "s267",
  "s268",
  "s269",
  "s270",
  "s271",
  "s272",
  "s273",
  "s274",
  "s275",
  "s276",
  "s277",
  "s278",
  "s279",
  "s280",
  "s281",
  "s282",
  "s283",
  "s284",
  "s285",
  "s286",
  "s287",
  "s288",
  "s289",
  "s290",
  "s291",
  "s292",
  "s293",
  "s294",
  "s295",
  "s296",
  "s297",
  "s298",
  "s299",
];

const manyNames = () => ({
  k0: 0,
  k1: 3,
  k2: 6,
  k3: 9,
  k4: 12,
  k5: 15,
  k6: 18,
  k7: 21,
  k8: 24,
  k9: 27,
  k10: 30,
  k11: 33,
  k12: 36,
  k13: 39,
  k14: 42,
  k15: 45,
  k16: 48,
  k17: 51,
  k18: 54,
  k19: 57,
  k20: 60,
  k21: 63,
  k22: 66,
  k23: 69,
  k24: 72,
  k25: 75,
  k26: 78,
  k27: 81,
  k28: 84,
  k29: 87,
  k30: 90,
  k31: 93,
  k32: 96,
  k33: 99,
  k34: 102,
  k35: 105,
  k36: 108,
  k37: 111,
  k38: 114,
  k39: 117,
  k40: 120,
  k41: 123,
  k42: 126,
  k43: 129,
  k44: 132,
  k45: 135,
  k46: 138,
  k47: 141,
  k48: 144,
  k49: 147,
  k50: 150,
  k51: 153,
  k52: 156,
  k53: 159,
  k54: 162,
  k55: 165,
  k56: 168,
  k57: 171,
  k58: 174,
  k59: 177,
  k60: 180,
  k61: 183,
  k62: 186,
  k63: 189,
  k64: 192,
  k65: 195,
  k66: 198,
  k67: 201,
  k68: 204,
  k69: 207,
  k70: 210,
  k71: 213,
  k72: 216,
  k73: 219,
  k74: 222,
  k75: 225,
  k76: 228,
  k77: 231,
  k78: 234,
  k79: 237,
  k80: 240,
  k81: 243,
  k82: 246,
  k83: 249,
  k84: 252,
  k85: 255,
  k86: 258,
  k87: 261,
  k88: 264,
  k89: 267,
  k90: 270,
  k91: 273,
  k92: 276,
  k93: 279,
  k94: 282,
  k95: 285,
  k96: 288,
  k97: 291,
  k98: 294,
  k99: 297,
  k100: 300,
  k101: 303,
  k102: 306,
  k103: 309,
  k104: 312,
  k105: 315,
  k106: 318,
  k107: 321,
  k108: 324,
  k109: 327,
  k110: 330,
  k111: 333,
  k112: 336,
  k113: 339,
  k114: 342,
  k115: 345,
  k116: 348,
  k117: 351,
  k118: 354,
  k119: 357,
  k120: 360,
  k121: 363,
  k122: 366,
  k123: 369,
  k124: 372,
  k125: 375,
  k126: 378,
  k127: 381,
  k128: 384,
  k129: 387,
  k130: 390,
  k131: 393,
  k132: 396,
  k133: 399,
  k134: 402,
  k135: 405,
  k136: 408,
  k137: 411,
  k138: 414,
  k139: 417,
  k140: 420,
  k141: 423,
  k142: 426,
  k143: 429,
  k144: 432,
  k145: 435,
  k146: 438,
  k147: 441,
  k148: 444,
  k149: 447,
  k150: 450,
  k151: 453,
  k152: 456,
  k153: 459,
  k154: 462,
  k155: 465,
  k156: 468,
  k157: 471,
  k158: 474,
  k159: 477,
  k160: 480,
  k161: 483,
  k162: 486,
  k163: 489,
  k164: 492,
  k165: 495,
  k166: 498,
  k167: 501,
  k168: 504,
  k169: 507,
  k170: 510,
  k171: 513,
  k172: 516,
  k173: 519,
  k174: 522,
  k175: 525,
  k176: 528,
  k177: 531,
  k178: 534,
  k179: 537,
  k180: 540,
  k181: 543,
  k182: 546,
  k183: 549,
  k184: 552,
  k185: 555,
  k186: 558,
  k187: 561,
  k188: 564,
  k189: 567,
  k190: 570,
  k191: 573,
  k192: 576,
  k193: 579,
  k194: 582,
  k195: 585,
  k196: 588,
  k197: 591,
  k198: 594,
  k199: 597,
  k200: 600,
  k201: 603,
  k202: 606,
  k203: 609,
  k204: 612,
  k205: 615,
  k206: 618,
  k207: 621,
  k208: 624,
  k209: 627,
  k210: 630,
  k211: 633,
  k212: 636,
  k213: 639,
  k214: 642,
  k215: 645,
  k216: 648,
  k217: 651,
  k218: 654,
  k219: 657,
  k220: 660,
  k221: 663,
  k222: 666,
  k223: 669,
  k224: 672,
  k225: 675,
  k226: 678,
  k227: 681,
  k228: 684,
  k229: 687,
  k230: 690,
  k231: 693,
  k232: 696,
  k233: 699,
  k234: 702,
  k235: 705,
  k236: 708,
  k237: 711,
  k238: 714,
  k239: 717,
  k240: 720,
  k241: 723,
  k242: 726,
  k243: 729,
  k244: 732,
  k245: 735,
  k246: 738,
  k247: 741,
  k248: 744,
  k249: 747,
  k250: 750,
  k251: 753,
  k252: 756,
  k253: 759,
  k254: 762,
  k255: 765,
  k256: 768,
  k257: 771,
  k258: 774,
  k259: 777,
  k260: 780,
  k261: 783,
  k262: 786,
  k263: 789,
  k264: 792,
  k265: 795,
  k266: 798,
  k267: 801,
  k268: 804,
  k269: 807,
  k270: 810,
  k271: 813,
  k272: 816,
  k273: 819,
  k274: 822,
  k275: 825,
  k276: 828,
  k277: 831,
  k278: 834,
  k279: 837,
  k280: 840,
  k281: 843,
  k282: 846,
  k283: 849,
  k284: 852,
  k285: 855,
  k286: 858,
  k287: 861,
  k288: 864,
  k289: 867,
  k290: 870,
  k291: 873,
  k292: 876,
  k293: 879,
  k294: 882,
  k295: 885,
  k296: 888,
  k297: 891,
  k298: 894,
  k299: 897,
});

describe("more than 255 bindings or constants", () => {
  test("locals above register 255 are read, combined and captured", () => {
    expect(manyLocals()).toEqual([640, 596, 0, 150]);
  });

  test("string constants above index 255 load in order", () => {
    const strings = manyStrings();
    expect(strings.length).toBe(300);
    expect(strings[0]).toBe("s0");
    expect(strings[256]).toBe("s256");
    expect(strings[299]).toBe("s299");
  });

  test("property names above constant index 255 define and read back", () => {
    const object = manyNames();
    expect(Object.keys(object).length).toBe(300);
    expect(object.k0).toBe(0);
    expect(object.k256).toBe(768);
    expect(object.k299).toBe(897);
  });
});
