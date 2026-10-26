unit Goccia.NumberExponentiation;

{$I Goccia.inc}

interface

function NumberExponentiation(const ABase, AExponent: Double): Double;

implementation

uses
  Math,

  NumberBits;

{ Copyright (c) 2018-2026, Arm Limited.

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.

  Port of the double-precision pow of Arm optimized-routines (MIT OR
  Apache-2.0 WITH LLVM-exception, used here under MIT), commit
  f5205d212ac67727689176b7fcfaef7864d898a5: math/pow.c blob
  041aff7be7fc872b72c3f3b5e246c38e4c33c8b5, math/pow_log_data.c blob
  2a4c250d85c3b7715e84c513ed1e6c9daa628ce2, math/exp_data.c blob
  c20b1b2d3e06c6137ee9b122f2202af14eb04742. glibc (since 2.28) and musl ship
  the same algorithm. The port takes the path without fused multiply-add, so
  every target computes the same bits, and round-to-nearest, the only
  rounding mode ECMAScript has. Worst-case error 0.54 ULP; a result that is
  exactly representable, such as an integer power, comes out exact. The
  tables hold the upstream values as IEEE 754 bit patterns; a literal above
  $7FFFFFFFFFFFFFFF needs its UInt64 cast in FPC and Delphi alike. }

const
  POW_LOG_TABLE_BITS = 7;
  POW_LOG_TABLE_SIZE = 1 shl POW_LOG_TABLE_BITS;
  EXP_TABLE_BITS = 7;
  EXP_TABLE_SIZE = 1 shl EXP_TABLE_BITS;
  // log: x = 2^k z with z in [OFF, 2 OFF), split into the table's
  // subintervals.
  POW_LOG_OFF: UInt64 = $3FE6955500000000;
  // exp: the sign of the result, added to the exponent of its scale.
  EXP_SIGN_BIAS = $800 shl EXP_TABLE_BITS;
  SIGN_BIT: UInt64 = UInt64(1) shl 63;
  // The exponent field of a double's top 12 bits.
  EXPONENT_FIELD = $7FF;
  ABS_MASK: UInt64 = (UInt64(1) shl 63) - 1;
  ONE_BITS: UInt64 = $3FF0000000000000;
  // Masks that clear the low 32 and the low 27 bits.
  LOW_32_CLEAR: UInt64 = not UInt64($FFFFFFFF);
  LOW_27_CLEAR: UInt64 = not UInt64($7FFFFFF);
  TWO_POW_1009: UInt64 = $7F00000000000000;
  TWO_POW_MINUS_1022: UInt64 = $0010000000000000;
  TWO_POW_52: UInt64 = $4330000000000000;
  // Exponent fields (top 12 bits without the sign) that bound the inputs
  // the main path takes: 2^-54, 512 and 1024 for exp's argument, 2^-65 and
  // 2^63 for |y|.
  TOP_TWO_POW_MINUS_54 = $3C9;
  TOP_512 = $408;
  TOP_1024 = $409;
  TOP_TWO_POW_MINUS_65 = $3BE;
  TOP_TWO_POW_63 = $43E;

  POW_LOG_LN2HI: UInt64 = UInt64($3FE62E42FEFA3800);
  POW_LOG_LN2LO: UInt64 = UInt64($3D2EF35793C76730);
  POW_LOG_POLY: array[0..6] of UInt64 = (
    UInt64($BFE0000000000000),
    UInt64($BFE5555555555560),
    UInt64($3FE0000000000006),
    UInt64($3FE999999959554E),
    UInt64($BFE555555529A47A),
    UInt64($BFF2495B9B4845E9),
    UInt64($3FF0002B8B263FC3));
  // invc, logc, logctail for each of the 128 subintervals.
  POW_LOG_TABLE: array[0..POW_LOG_TABLE_SIZE * 3 - 1] of UInt64 = (
    UInt64($3FF6A00000000000), UInt64($BFD62C82F2B9C800), UInt64($3CFAB42428375680),
    UInt64($3FF6800000000000), UInt64($BFD5D1BDBF580800), UInt64($BD1CA508D8E0F720),
    UInt64($3FF6600000000000), UInt64($BFD5767717455800), UInt64($BD2362A4D5B6506D),
    UInt64($3FF6400000000000), UInt64($BFD51AAD872DF800), UInt64($BCE684E49EB067D5),
    UInt64($3FF6200000000000), UInt64($BFD4BE5F95777800), UInt64($BD041B6993293EE0),
    UInt64($3FF6000000000000), UInt64($BFD4618BC21C6000), UInt64($3D13D82F484C84CC),
    UInt64($3FF5E00000000000), UInt64($BFD404308686A800), UInt64($3CDC42F3ED820B3A),
    UInt64($3FF5C00000000000), UInt64($BFD3A64C55694800), UInt64($3D20B1C686519460),
    UInt64($3FF5A00000000000), UInt64($BFD347DD9A988000), UInt64($3D25594DD4C58092),
    UInt64($3FF5800000000000), UInt64($BFD2E8E2BAE12000), UInt64($3D267B1E99B72BD8),
    UInt64($3FF5600000000000), UInt64($BFD2895A13DE8800), UInt64($3D15CA14B6CFB03F),
    UInt64($3FF5600000000000), UInt64($BFD2895A13DE8800), UInt64($3D15CA14B6CFB03F),
    UInt64($3FF5400000000000), UInt64($BFD22941FBCF7800), UInt64($BD165A242853DA76),
    UInt64($3FF5200000000000), UInt64($BFD1C898C1699800), UInt64($BD1FAFBC68E75404),
    UInt64($3FF5000000000000), UInt64($BFD1675CABABA800), UInt64($3D1F1FC63382A8F0),
    UInt64($3FF4E00000000000), UInt64($BFD1058BF9AE4800), UInt64($BD26A8C4FD055A66),
    UInt64($3FF4C00000000000), UInt64($BFD0A324E2739000), UInt64($BD0C6BEE7EF4030E),
    UInt64($3FF4A00000000000), UInt64($BFD0402594B4D000), UInt64($BCF036B89EF42D7F),
    UInt64($3FF4A00000000000), UInt64($BFD0402594B4D000), UInt64($BCF036B89EF42D7F),
    UInt64($3FF4800000000000), UInt64($BFCFB9186D5E4000), UInt64($3D0D572AAB993C87),
    UInt64($3FF4600000000000), UInt64($BFCEF0ADCBDC6000), UInt64($3D2B26B79C86AF24),
    UInt64($3FF4400000000000), UInt64($BFCE27076E2AF000), UInt64($BD172F4F543FFF10),
    UInt64($3FF4200000000000), UInt64($BFCD5C216B4FC000), UInt64($3D21BA91BBCA681B),
    UInt64($3FF4000000000000), UInt64($BFCC8FF7C79AA000), UInt64($3D27794F689F8434),
    UInt64($3FF4000000000000), UInt64($BFCC8FF7C79AA000), UInt64($3D27794F689F8434),
    UInt64($3FF3E00000000000), UInt64($BFCBC286742D9000), UInt64($3D194EB0318BB78F),
    UInt64($3FF3C00000000000), UInt64($BFCAF3C94E80C000), UInt64($3CBA4E633FCD9066),
    UInt64($3FF3A00000000000), UInt64($BFCA23BC1FE2B000), UInt64($BD258C64DC46C1EA),
    UInt64($3FF3A00000000000), UInt64($BFCA23BC1FE2B000), UInt64($BD258C64DC46C1EA),
    UInt64($3FF3800000000000), UInt64($BFC9525A9CF45000), UInt64($BD2AD1D904C1D4E3),
    UInt64($3FF3600000000000), UInt64($BFC87FA06520D000), UInt64($3D2BBDBF7FDBFA09),
    UInt64($3FF3400000000000), UInt64($BFC7AB890210E000), UInt64($3D2BDB9072534A58),
    UInt64($3FF3400000000000), UInt64($BFC7AB890210E000), UInt64($3D2BDB9072534A58),
    UInt64($3FF3200000000000), UInt64($BFC6D60FE719D000), UInt64($BD10E46AA3B2E266),
    UInt64($3FF3000000000000), UInt64($BFC5FF3070A79000), UInt64($BD1E9E439F105039),
    UInt64($3FF3000000000000), UInt64($BFC5FF3070A79000), UInt64($BD1E9E439F105039),
    UInt64($3FF2E00000000000), UInt64($BFC526E5E3A1B000), UInt64($BD20DE8B90075B8F),
    UInt64($3FF2C00000000000), UInt64($BFC44D2B6CCB8000), UInt64($3D170CC16135783C),
    UInt64($3FF2C00000000000), UInt64($BFC44D2B6CCB8000), UInt64($3D170CC16135783C),
    UInt64($3FF2A00000000000), UInt64($BFC371FC201E9000), UInt64($3CF178864D27543A),
    UInt64($3FF2800000000000), UInt64($BFC29552F81FF000), UInt64($BD248D301771C408),
    UInt64($3FF2600000000000), UInt64($BFC1B72AD52F6000), UInt64($BD2E80A41811A396),
    UInt64($3FF2600000000000), UInt64($BFC1B72AD52F6000), UInt64($BD2E80A41811A396),
    UInt64($3FF2400000000000), UInt64($BFC0D77E7CD09000), UInt64($3D0A699688E85BF4),
    UInt64($3FF2400000000000), UInt64($BFC0D77E7CD09000), UInt64($3D0A699688E85BF4),
    UInt64($3FF2200000000000), UInt64($BFBFEC9131DBE000), UInt64($BD2575545CA333F2),
    UInt64($3FF2000000000000), UInt64($BFBE27076E2B0000), UInt64($3D2A342C2AF0003C),
    UInt64($3FF2000000000000), UInt64($BFBE27076E2B0000), UInt64($3D2A342C2AF0003C),
    UInt64($3FF1E00000000000), UInt64($BFBC5E548F5BC000), UInt64($BD1D0C57585FBE06),
    UInt64($3FF1C00000000000), UInt64($BFBA926D3A4AE000), UInt64($3D253935E85BAAC8),
    UInt64($3FF1C00000000000), UInt64($BFBA926D3A4AE000), UInt64($3D253935E85BAAC8),
    UInt64($3FF1A00000000000), UInt64($BFB8C345D631A000), UInt64($3D137C294D2F5668),
    UInt64($3FF1A00000000000), UInt64($BFB8C345D631A000), UInt64($3D137C294D2F5668),
    UInt64($3FF1800000000000), UInt64($BFB6F0D28AE56000), UInt64($BD269737C93373DA),
    UInt64($3FF1600000000000), UInt64($BFB51B073F062000), UInt64($3D1F025B61C65E57),
    UInt64($3FF1600000000000), UInt64($BFB51B073F062000), UInt64($3D1F025B61C65E57),
    UInt64($3FF1400000000000), UInt64($BFB341D7961BE000), UInt64($3D2C5EDACCF913DF),
    UInt64($3FF1400000000000), UInt64($BFB341D7961BE000), UInt64($3D2C5EDACCF913DF),
    UInt64($3FF1200000000000), UInt64($BFB16536EEA38000), UInt64($3D147C5E768FA309),
    UInt64($3FF1000000000000), UInt64($BFAF0A30C0118000), UInt64($3D2D599E83368E91),
    UInt64($3FF1000000000000), UInt64($BFAF0A30C0118000), UInt64($3D2D599E83368E91),
    UInt64($3FF0E00000000000), UInt64($BFAB42DD71198000), UInt64($3D1C827AE5D6704C),
    UInt64($3FF0E00000000000), UInt64($BFAB42DD71198000), UInt64($3D1C827AE5D6704C),
    UInt64($3FF0C00000000000), UInt64($BFA77458F632C000), UInt64($BD2CFC4634F2A1EE),
    UInt64($3FF0C00000000000), UInt64($BFA77458F632C000), UInt64($BD2CFC4634F2A1EE),
    UInt64($3FF0A00000000000), UInt64($BFA39E87B9FEC000), UInt64($3CF502B7F526FEAA),
    UInt64($3FF0A00000000000), UInt64($BFA39E87B9FEC000), UInt64($3CF502B7F526FEAA),
    UInt64($3FF0800000000000), UInt64($BF9F829B0E780000), UInt64($BD2980267C7E09E4),
    UInt64($3FF0800000000000), UInt64($BF9F829B0E780000), UInt64($BD2980267C7E09E4),
    UInt64($3FF0600000000000), UInt64($BF97B91B07D58000), UInt64($BD288D5493FAA639),
    UInt64($3FF0400000000000), UInt64($BF8FC0A8B0FC0000), UInt64($BCDF1E7CF6D3A69C),
    UInt64($3FF0400000000000), UInt64($BF8FC0A8B0FC0000), UInt64($BCDF1E7CF6D3A69C),
    UInt64($3FF0200000000000), UInt64($BF7FE02A6B100000), UInt64($BD19E23F0DDA40E4),
    UInt64($3FF0200000000000), UInt64($BF7FE02A6B100000), UInt64($BD19E23F0DDA40E4),
    UInt64($3FF0000000000000), UInt64($0000000000000000), UInt64($0000000000000000),
    UInt64($3FF0000000000000), UInt64($0000000000000000), UInt64($0000000000000000),
    UInt64($3FEFC00000000000), UInt64($3F80101575890000), UInt64($BD10C76B999D2BE8),
    UInt64($3FEF800000000000), UInt64($3F90205658938000), UInt64($BD23DC5B06E2F7D2),
    UInt64($3FEF400000000000), UInt64($3F98492528C90000), UInt64($BD2AA0BA325A0C34),
    UInt64($3FEF000000000000), UInt64($3FA0415D89E74000), UInt64($3D0111C05CF1D753),
    UInt64($3FEEC00000000000), UInt64($3FA466AED42E0000), UInt64($BD2C167375BDFD28),
    UInt64($3FEE800000000000), UInt64($3FA894AA149FC000), UInt64($BD197995D05A267D),
    UInt64($3FEE400000000000), UInt64($3FACCB73CDDDC000), UInt64($BD1A68F247D82807),
    UInt64($3FEE200000000000), UInt64($3FAEEA31C006C000), UInt64($BD0E113E4FC93B7B),
    UInt64($3FEDE00000000000), UInt64($3FB1973BD1466000), UInt64($BD25325D560D9E9B),
    UInt64($3FEDA00000000000), UInt64($3FB3BDF5A7D1E000), UInt64($3D2CC85EA5DB4ED7),
    UInt64($3FED600000000000), UInt64($3FB5E95A4D97A000), UInt64($BD2C69063C5D1D1E),
    UInt64($3FED400000000000), UInt64($3FB700D30AEAC000), UInt64($3CEC1E8DA99DED32),
    UInt64($3FED000000000000), UInt64($3FB9335E5D594000), UInt64($3D23115C3ABD47DA),
    UInt64($3FECC00000000000), UInt64($3FBB6AC88DAD6000), UInt64($BD1390802BF768E5),
    UInt64($3FECA00000000000), UInt64($3FBC885801BC4000), UInt64($3D2646D1C65AACD3),
    UInt64($3FEC600000000000), UInt64($3FBEC739830A2000), UInt64($BD2DC068AFE645E0),
    UInt64($3FEC400000000000), UInt64($3FBFE89139DBE000), UInt64($BD2534D64FA10AFD),
    UInt64($3FEC000000000000), UInt64($3FC1178E8227E000), UInt64($3D21EF78CE2D07F2),
    UInt64($3FEBE00000000000), UInt64($3FC1AA2B7E23F000), UInt64($3D2CA78E44389934),
    UInt64($3FEBA00000000000), UInt64($3FC2D1610C868000), UInt64($3D039D6CCB81B4A1),
    UInt64($3FEB800000000000), UInt64($3FC365FCB0159000), UInt64($3CC62FA8234B7289),
    UInt64($3FEB400000000000), UInt64($3FC4913D8333B000), UInt64($3D25837954FDB678),
    UInt64($3FEB200000000000), UInt64($3FC527E5E4A1B000), UInt64($3D2633E8E5697DC7),
    UInt64($3FEAE00000000000), UInt64($3FC6574EBE8C1000), UInt64($3D19CF8B2C3C2E78),
    UInt64($3FEAC00000000000), UInt64($3FC6F0128B757000), UInt64($BD25118DE59C21E1),
    UInt64($3FEAA00000000000), UInt64($3FC7898D85445000), UInt64($BD1C661070914305),
    UInt64($3FEA600000000000), UInt64($3FC8BEAFEB390000), UInt64($BD073D54AAE92CD1),
    UInt64($3FEA400000000000), UInt64($3FC95A5ADCF70000), UInt64($3D07F22858A0FF6F),
    UInt64($3FEA000000000000), UInt64($3FCA93ED3C8AE000), UInt64($BD28724350562169),
    UInt64($3FE9E00000000000), UInt64($3FCB31D8575BD000), UInt64($BD0C358D4EACE1AA),
    UInt64($3FE9C00000000000), UInt64($3FCBD087383BE000), UInt64($BD2D4BC4595412B6),
    UInt64($3FE9A00000000000), UInt64($3FCC6FFBC6F01000), UInt64($BCF1EC72C5962BD2),
    UInt64($3FE9600000000000), UInt64($3FCDB13DB0D49000), UInt64($BD2AFF2AF715B035),
    UInt64($3FE9400000000000), UInt64($3FCE530EFFE71000), UInt64($3CC212276041F430),
    UInt64($3FE9200000000000), UInt64($3FCEF5ADE4DD0000), UInt64($BCCA211565BB8E11),
    UInt64($3FE9000000000000), UInt64($3FCF991C6CB3B000), UInt64($3D1BCBECCA0CDF30),
    UInt64($3FE8C00000000000), UInt64($3FD07138604D5800), UInt64($3CF89CDB16ED4E91),
    UInt64($3FE8A00000000000), UInt64($3FD0C42D67616000), UInt64($3D27188B163CEAE9),
    UInt64($3FE8800000000000), UInt64($3FD1178E8227E800), UInt64($BD2C210E63A5F01C),
    UInt64($3FE8600000000000), UInt64($3FD16B5CCBACF800), UInt64($3D2B9ACDF7A51681),
    UInt64($3FE8400000000000), UInt64($3FD1BF99635A6800), UInt64($3D2CA6ED5147BDB7),
    UInt64($3FE8200000000000), UInt64($3FD214456D0EB800), UInt64($3D0A87DEBA46BAEA),
    UInt64($3FE7E00000000000), UInt64($3FD2BEF07CDC9000), UInt64($3D2A9CFA4A5004F4),
    UInt64($3FE7C00000000000), UInt64($3FD314F1E1D36000), UInt64($BD28E27AD3213CB8),
    UInt64($3FE7A00000000000), UInt64($3FD36B6776BE1000), UInt64($3D116ECDB0F177C8),
    UInt64($3FE7800000000000), UInt64($3FD3C25277333000), UInt64($3D183B54B606BD5C),
    UInt64($3FE7600000000000), UInt64($3FD419B423D5E800), UInt64($3D08E436EC90E09D),
    UInt64($3FE7400000000000), UInt64($3FD4718DC271C800), UInt64($BD2F27CE0967D675),
    UInt64($3FE7200000000000), UInt64($3FD4C9E09E173000), UInt64($BD2E20891B0AD8A4),
    UInt64($3FE7000000000000), UInt64($3FD522AE0738A000), UInt64($3D2EBE708164C759),
    UInt64($3FE6E00000000000), UInt64($3FD57BF753C8D000), UInt64($3D1FADEDEE5D40EF),
    UInt64($3FE6C00000000000), UInt64($3FD5D5BDDF596000), UInt64($BD0A0B2A08A465DC));
  EXP_INV_LN2_N: UInt64 = UInt64($40671547652B82FE);
  EXP_NEG_LN2_HI_N: UInt64 = UInt64($BF762E42FEFA0000);
  EXP_NEG_LN2_LO_N: UInt64 = UInt64($BD0CF79ABC9E3B3A);
  EXP_SHIFT: UInt64 = UInt64($4338000000000000);
  EXP_POLY: array[0..3] of UInt64 = (
    UInt64($3FDFFFFFFFFFFDBD),
    UInt64($3FC555555555543C),
    UInt64($3FA55555CF172B91),
    UInt64($3F81111167A4D017));
  // tail and scale bits of 2^(i/128) for each i.
  EXP_TABLE: array[0..EXP_TABLE_SIZE * 2 - 1] of UInt64 = (
    UInt64($0000000000000000), UInt64($3FF0000000000000),
    UInt64($3C9B3B4F1A88BF6E), UInt64($3FEFF63DA9FB3335),
    UInt64($BC7160139CD8DC5D), UInt64($3FEFEC9A3E778061),
    UInt64($BC905E7A108766D1), UInt64($3FEFE315E86E7F85),
    UInt64($3C8CD2523567F613), UInt64($3FEFD9B0D3158574),
    UInt64($BC8BCE8023F98EFA), UInt64($3FEFD06B29DDF6DE),
    UInt64($3C60F74E61E6C861), UInt64($3FEFC74518759BC8),
    UInt64($3C90A3E45B33D399), UInt64($3FEFBE3ECAC6F383),
    UInt64($3C979AA65D837B6D), UInt64($3FEFB5586CF9890F),
    UInt64($3C8EB51A92FDEFFC), UInt64($3FEFAC922B7247F7),
    UInt64($3C3EBE3D702F9CD1), UInt64($3FEFA3EC32D3D1A2),
    UInt64($BC6A033489906E0B), UInt64($3FEF9B66AFFED31B),
    UInt64($BC9556522A2FBD0E), UInt64($3FEF9301D0125B51),
    UInt64($BC5080EF8C4EEA55), UInt64($3FEF8ABDC06C31CC),
    UInt64($BC91C923B9D5F416), UInt64($3FEF829AAEA92DE0),
    UInt64($3C80D3E3E95C55AF), UInt64($3FEF7A98C8A58E51),
    UInt64($BC801B15EAA59348), UInt64($3FEF72B83C7D517B),
    UInt64($BC8F1FF055DE323D), UInt64($3FEF6AF9388C8DEA),
    UInt64($3C8B898C3F1353BF), UInt64($3FEF635BEB6FCB75),
    UInt64($BC96D99C7611EB26), UInt64($3FEF5BE084045CD4),
    UInt64($3C9AECF73E3A2F60), UInt64($3FEF54873168B9AA),
    UInt64($BC8FE782CB86389D), UInt64($3FEF4D5022FCD91D),
    UInt64($3C8A6F4144A6C38D), UInt64($3FEF463B88628CD6),
    UInt64($3C807A05B0E4047D), UInt64($3FEF3F49917DDC96),
    UInt64($3C968EFDE3A8A894), UInt64($3FEF387A6E756238),
    UInt64($3C875E18F274487D), UInt64($3FEF31CE4FB2A63F),
    UInt64($3C80472B981FE7F2), UInt64($3FEF2B4565E27CDD),
    UInt64($BC96B87B3F71085E), UInt64($3FEF24DFE1F56381),
    UInt64($3C82F7E16D09AB31), UInt64($3FEF1E9DF51FDEE1),
    UInt64($BC3D219B1A6FBFFA), UInt64($3FEF187FD0DAD990),
    UInt64($3C8B3782720C0AB4), UInt64($3FEF1285A6E4030B),
    UInt64($3C6E149289CECB8F), UInt64($3FEF0CAFA93E2F56),
    UInt64($3C834D754DB0ABB6), UInt64($3FEF06FE0A31B715),
    UInt64($3C864201E2AC744C), UInt64($3FEF0170FC4CD831),
    UInt64($3C8FDD395DD3F84A), UInt64($3FEEFC08B26416FF),
    UInt64($BC86A3803B8E5B04), UInt64($3FEEF6C55F929FF1),
    UInt64($BC924AEDCC4B5068), UInt64($3FEEF1A7373AA9CB),
    UInt64($BC9907F81B512D8E), UInt64($3FEEECAE6D05D866),
    UInt64($BC71D1E83E9436D2), UInt64($3FEEE7DB34E59FF7),
    UInt64($BC991919B3CE1B15), UInt64($3FEEE32DC313A8E5),
    UInt64($3C859F48A72A4C6D), UInt64($3FEEDEA64C123422),
    UInt64($BC9312607A28698A), UInt64($3FEEDA4504AC801C),
    UInt64($BC58A78F4817895B), UInt64($3FEED60A21F72E2A),
    UInt64($BC7C2C9B67499A1B), UInt64($3FEED1F5D950A897),
    UInt64($3C4363ED60C2AC11), UInt64($3FEECE086061892D),
    UInt64($3C9666093B0664EF), UInt64($3FEECA41ED1D0057),
    UInt64($3C6ECCE1DAA10379), UInt64($3FEEC6A2B5C13CD0),
    UInt64($3C93FF8E3F0F1230), UInt64($3FEEC32AF0D7D3DE),
    UInt64($3C7690CEBB7AAFB0), UInt64($3FEEBFDAD5362A27),
    UInt64($3C931DBDEB54E077), UInt64($3FEEBCB299FDDD0D),
    UInt64($BC8F94340071A38E), UInt64($3FEEB9B2769D2CA7),
    UInt64($BC87DECCDC93A349), UInt64($3FEEB6DAA2CF6642),
    UInt64($BC78DEC6BD0F385F), UInt64($3FEEB42B569D4F82),
    UInt64($BC861246EC7B5CF6), UInt64($3FEEB1A4CA5D920F),
    UInt64($3C93350518FDD78E), UInt64($3FEEAF4736B527DA),
    UInt64($3C7B98B72F8A9B05), UInt64($3FEEAD12D497C7FD),
    UInt64($3C9063E1E21C5409), UInt64($3FEEAB07DD485429),
    UInt64($3C34C7855019C6EA), UInt64($3FEEA9268A5946B7),
    UInt64($3C9432E62B64C035), UInt64($3FEEA76F15AD2148),
    UInt64($BC8CE44A6199769F), UInt64($3FEEA5E1B976DC09),
    UInt64($BC8C33C53BEF4DA8), UInt64($3FEEA47EB03A5585),
    UInt64($BC845378892BE9AE), UInt64($3FEEA34634CCC320),
    UInt64($BC93CEDD78565858), UInt64($3FEEA23882552225),
    UInt64($3C5710AA807E1964), UInt64($3FEEA155D44CA973),
    UInt64($BC93B3EFBF5E2228), UInt64($3FEEA09E667F3BCD),
    UInt64($BC6A12AD8734B982), UInt64($3FEEA012750BDABF),
    UInt64($BC6367EFB86DA9EE), UInt64($3FEE9FB23C651A2F),
    UInt64($BC80DC3D54E08851), UInt64($3FEE9F7DF9519484),
    UInt64($BC781F647E5A3ECF), UInt64($3FEE9F75E8EC5F74),
    UInt64($BC86EE4AC08B7DB0), UInt64($3FEE9F9A48A58174),
    UInt64($BC8619321E55E68A), UInt64($3FEE9FEB564267C9),
    UInt64($3C909CCB5E09D4D3), UInt64($3FEEA0694FDE5D3F),
    UInt64($BC7B32DCB94DA51D), UInt64($3FEEA11473EB0187),
    UInt64($3C94ECFD5467C06B), UInt64($3FEEA1ED0130C132),
    UInt64($3C65EBE1ABD66C55), UInt64($3FEEA2F336CF4E62),
    UInt64($BC88A1C52FB3CF42), UInt64($3FEEA427543E1A12),
    UInt64($BC9369B6F13B3734), UInt64($3FEEA589994CCE13),
    UInt64($BC805E843A19FF1E), UInt64($3FEEA71A4623C7AD),
    UInt64($BC94D450D872576E), UInt64($3FEEA8D99B4492ED),
    UInt64($3C90AD675B0E8A00), UInt64($3FEEAAC7D98A6699),
    UInt64($3C8DB72FC1F0EAB4), UInt64($3FEEACE5422AA0DB),
    UInt64($BC65B6609CC5E7FF), UInt64($3FEEAF3216B5448C),
    UInt64($3C7BF68359F35F44), UInt64($3FEEB1AE99157736),
    UInt64($BC93091FA71E3D83), UInt64($3FEEB45B0B91FFC6),
    UInt64($BC5DA9B88B6C1E29), UInt64($3FEEB737B0CDC5E5),
    UInt64($BC6C23F97C90B959), UInt64($3FEEBA44CBC8520F),
    UInt64($BC92434322F4F9AA), UInt64($3FEEBD829FDE4E50),
    UInt64($BC85CA6CD7668E4B), UInt64($3FEEC0F170CA07BA),
    UInt64($3C71AFFC2B91CE27), UInt64($3FEEC49182A3F090),
    UInt64($3C6DD235E10A73BB), UInt64($3FEEC86319E32323),
    UInt64($BC87C50422622263), UInt64($3FEECC667B5DE565),
    UInt64($3C8B1C86E3E231D5), UInt64($3FEED09BEC4A2D33),
    UInt64($BC91BBD1D3BCBB15), UInt64($3FEED503B23E255D),
    UInt64($3C90CC319CEE31D2), UInt64($3FEED99E1330B358),
    UInt64($3C8469846E735AB3), UInt64($3FEEDE6B5579FDBF),
    UInt64($BC82DFCD978E9DB4), UInt64($3FEEE36BBFD3F37A),
    UInt64($3C8C1A7792CB3387), UInt64($3FEEE89F995AD3AD),
    UInt64($BC907B8F4AD1D9FA), UInt64($3FEEEE07298DB666),
    UInt64($BC55C3D956DCAEBA), UInt64($3FEEF3A2B84F15FB),
    UInt64($BC90A40E3DA6F640), UInt64($3FEEF9728DE5593A),
    UInt64($BC68D6F438AD9334), UInt64($3FEEFF76F2FB5E47),
    UInt64($BC91EEE26B588A35), UInt64($3FEF05B030A1064A),
    UInt64($3C74FFD70A5FDDCD), UInt64($3FEF0C1E904BC1D2),
    UInt64($BC91BDFBFA9298AC), UInt64($3FEF12C25BD71E09),
    UInt64($3C736EAE30AF0CB3), UInt64($3FEF199BDD85529C),
    UInt64($3C8EE3325C9FFD94), UInt64($3FEF20AB5FFFD07A),
    UInt64($3C84E08FD10959AC), UInt64($3FEF27F12E57D14B),
    UInt64($3C63CDAF384E1A67), UInt64($3FEF2F6D9406E7B5),
    UInt64($3C676B2C6C921968), UInt64($3FEF3720DCEF9069),
    UInt64($BC808A1883CCB5D2), UInt64($3FEF3F0B555DC3FA),
    UInt64($BC8FAD5D3FFFFA6F), UInt64($3FEF472D4A07897C),
    UInt64($BC900DAE3875A949), UInt64($3FEF4F87080D89F2),
    UInt64($3C74A385A63D07A7), UInt64($3FEF5818DCFBA487),
    UInt64($BC82919E2040220F), UInt64($3FEF60E316C98398),
    UInt64($3C8E5A50D5C192AC), UInt64($3FEF69E603DB3285),
    UInt64($3C843A59AC016B4B), UInt64($3FEF7321F301B460),
    UInt64($BC82D52107B43E1F), UInt64($3FEF7C97337B9B5F),
    UInt64($BC892AB93B470DC9), UInt64($3FEF864614F5A129),
    UInt64($3C74B604603A88D3), UInt64($3FEF902EE78B3FF6),
    UInt64($3C83C5EC519D7271), UInt64($3FEF9A51FBC74C83),
    UInt64($BC8FF7128FD391F0), UInt64($3FEFA4AFA2A490DA),
    UInt64($BC8DAE98E223747D), UInt64($3FEFAF482D8E67F1),
    UInt64($3C8EC3BC41AA2008), UInt64($3FEFBA1BEE615A27),
    UInt64($3C842B94C3A9EB32), UInt64($3FEFC52B376BBA97),
    UInt64($3C8A64A931D185EE), UInt64($3FEFD0765B6E4540),
    UInt64($BC8E37BAE43BE3ED), UInt64($3FEFDBFDAD9CBE14),
    UInt64($3C77893B4D91CD9D), UInt64($3FEFE7C1819E90D8),
    UInt64($3C5305C14160CC89), UInt64($3FEFF3C22B8F71F1));

{$IFDEF FPC}
  {$PUSH}
{$ENDIF}
{$R-}{$Q-}

function Top12(const AValue: Double): UInt32; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := UInt32(DoubleToBits(AValue) shr 52);
end;

// Returns 0 if not an integer, 1 if odd and 2 if even. ABits is a non-zero
// finite Number.
function CheckInteger(const ABits: UInt64): Integer;
var
  Exponent: Integer;
begin
  Exponent := Integer((ABits shr 52) and EXPONENT_FIELD);
  if Exponent < $3FF then
    Exit(0);
  if Exponent > $3FF + 52 then
    Exit(2);
  if (ABits and ((UInt64(1) shl ($3FF + 52 - Exponent)) - 1)) <> 0 then
    Exit(0);
  if (ABits and (UInt64(1) shl ($3FF + 52 - Exponent))) <> 0 then
    Exit(1);
  Result := 2;
end;

// log(x) as Result + ATail, where ATail carries about 15 more bits. ABits is
// a positive x, normalized in the subnormal range so its exponent field is
// below zero.
function LogInline(const ABits: UInt64; out ATail: Double): Double;
var
  Z, R, Kd, InvC, LogC, LogCTail, Hi, T1, T2, Lo, Lo1, Lo2, Lo3, Lo4, P: Double;
  ZHi, ZLo, RHi, RLo, Ar, Ar2, Ar3, ArHi, ArHi2, Y: Double;
  A0, A1, A2, A3, A4, A5, A6: Double;
  Tmp, Iz: UInt64;
  I, K: Integer;
begin
  // x = 2^k z, where z is in [OFF, 2 OFF) and exact. The ith subinterval
  // contains z, and c is near its centre.
  Tmp := ABits - POW_LOG_OFF;
  I := Integer((Tmp shr (52 - POW_LOG_TABLE_BITS)) and (POW_LOG_TABLE_SIZE - 1));
  // The top 12 bits of tmp as a signed number (an arithmetic shift).
  K := Integer(Tmp shr 52);
  if K >= $800 then
    Dec(K, $1000);
  Iz := ABits - (Tmp and (UInt64($FFF) shl 52));
  Z := BitsToDouble(Iz);
  Kd := K;

  // log(x) = k ln2 + log(c) + log1p(z/c - 1).
  InvC := BitsToDouble(POW_LOG_TABLE[I * 3]);
  LogC := BitsToDouble(POW_LOG_TABLE[I * 3 + 1]);
  LogCTail := BitsToDouble(POW_LOG_TABLE[I * 3 + 2]);

  // Split z so that rhi, rlo and rhi * rhi are exact and |rlo| <= |r|.
  ZHi := BitsToDouble((Iz + (UInt64(1) shl 31)) and LOW_32_CLEAR);
  ZLo := Z - ZHi;
  RHi := ZHi * InvC - 1.0;
  RLo := ZLo * InvC;
  R := RHi + RLo;

  // k ln2 + log(c) + r.
  T1 := Kd * BitsToDouble(POW_LOG_LN2HI) + LogC;
  T2 := T1 + R;
  Lo1 := Kd * BitsToDouble(POW_LOG_LN2LO) + LogCTail;
  Lo2 := T1 - T2 + R;

  A0 := BitsToDouble(POW_LOG_POLY[0]);
  A1 := BitsToDouble(POW_LOG_POLY[1]);
  A2 := BitsToDouble(POW_LOG_POLY[2]);
  A3 := BitsToDouble(POW_LOG_POLY[3]);
  A4 := BitsToDouble(POW_LOG_POLY[4]);
  A5 := BitsToDouble(POW_LOG_POLY[5]);
  A6 := BitsToDouble(POW_LOG_POLY[6]);
  Ar := A0 * R;
  Ar2 := R * Ar;
  Ar3 := R * Ar2;
  // k ln2 + log(c) + r + A0 r^2.
  ArHi := A0 * RHi;
  ArHi2 := RHi * ArHi;
  Hi := T2 + ArHi2;
  Lo3 := RLo * (Ar + ArHi);
  Lo4 := T2 - Hi + ArHi2;
  // p = log1p(r) - r - A0 r^2.
  P := Ar3 * (A1 + R * A2 + Ar2 * (A3 + R * A4 + Ar2 * (A5 + R * A6)));
  Lo := Lo1 + Lo2 + Lo3 + Lo4 + P;
  Y := Hi + Lo;
  ATail := Hi - Y + Lo;
  Result := Y;
end;

// scale (1 + ATmp) where the exponent of scale, ASBits, may have overflowed
// or underflowed. AKi is the k of the argument reduction: positive when the
// result may overflow, negative when it may underflow.
function ExpSpecialCase(const ATmp: Double; ASBits: UInt64;
  const AKi: UInt64): Double;
var
  Scale, Y, Hi, Lo, One: Double;
begin
  if (AKi and $80000000) = 0 then
  begin
    // k > 0: the exponent of scale may have overflowed by up to 460.
    ASBits := ASBits - (UInt64(1009) shl 52);
    Scale := BitsToDouble(ASBits);
    Y := Scale + Scale * ATmp;
    Exit(Y * BitsToDouble(TWO_POW_1009));
  end;
  // k < 0: round in the subnormal range only once.
  ASBits := ASBits + (UInt64(1022) shl 52);
  Scale := BitsToDouble(ASBits);
  Y := Scale + Scale * ATmp;
  if Abs(Y) < 1.0 then
  begin
    // Round y to the precision it keeps once scaled into the subnormal
    // range, so the result is not rounded twice.
    One := 1.0;
    if Y < 0.0 then
      One := -1.0;
    Lo := Scale - Y + Scale * ATmp;
    Hi := One + Y;
    Lo := One - Hi + Y + Lo;
    Y := Hi + Lo;
    Y := Y - One;
    // The sign of a zero result.
    if Y = 0.0 then
      Y := BitsToDouble(ASBits and SIGN_BIT);
  end;
  Result := BitsToDouble(TWO_POW_MINUS_1022) * Y;
end;

// sign exp(AX + AXTail), where |AXTail| < 2^-8/N and |AXTail| <= |AX|.
// ASignBias is EXP_SIGN_BIAS for a negative result, else 0.
function ExpInline(const AX, AXTail: Double; const ASignBias: UInt32): Double;
var
  AbsTop: UInt32;
  Ki, Idx, Top, SBits: UInt64;
  Kd, Z, R, R2, Scale, Tail, Tmp, Shift: Double;
begin
  AbsTop := Top12(AX) and EXPONENT_FIELD;
  if (AbsTop < TOP_TWO_POW_MINUS_54) or (AbsTop >= TOP_512) then
  begin
    if AbsTop < TOP_TWO_POW_MINUS_54 then
    begin
      // exp(tiny) rounds to 1.
      if ASignBias <> 0 then
        Exit(-(1.0 + AX));
      Exit(1.0 + AX);
    end;
    if AbsTop >= TOP_1024 then
    begin
      // Overflow or underflow; an infinite or NaN argument cannot reach
      // here.
      if (DoubleToBits(AX) shr 63) <> 0 then
      begin
        if ASignBias <> 0 then
          Exit(BitsToDouble(SIGN_BIT));
        Exit(0.0);
      end;
      if ASignBias <> 0 then
        Exit(-Infinity);
      Exit(Infinity);
    end;
    // A large argument takes ExpSpecialCase below.
    AbsTop := 0;
  end;

  // exp(x) = 2^(k/N) exp(r), with exp(r) in [2^(-1/2N), 2^(1/2N)]: x is
  // ln2/N k + r, with an integer k and r in [-ln2/2N, ln2/2N].
  Shift := BitsToDouble(EXP_SHIFT);
  Z := BitsToDouble(EXP_INV_LN2_N) * AX;
  Kd := Z + Shift;
  Ki := DoubleToBits(Kd);
  Kd := Kd - Shift;
  R := AX + Kd * BitsToDouble(EXP_NEG_LN2_HI_N) +
    Kd * BitsToDouble(EXP_NEG_LN2_LO_N);
  R := R + AXTail;
  // 2^(k/N) ~= scale (1 + tail).
  Idx := 2 * (Ki and (EXP_TABLE_SIZE - 1));
  Top := (Ki + ASignBias) shl (52 - EXP_TABLE_BITS);
  Tail := BitsToDouble(EXP_TABLE[Idx]);
  // A valid scale only while -1023 N < k < 1024 N.
  SBits := EXP_TABLE[Idx + 1] + Top;
  // exp(x) = 2^(k/N) exp(r) ~= scale + scale (tail + exp(r) - 1).
  R2 := R * R;
  Tmp := Tail + R + R2 * (BitsToDouble(EXP_POLY[0]) +
    R * BitsToDouble(EXP_POLY[1])) + R2 * R2 *
    (BitsToDouble(EXP_POLY[2]) + R * BitsToDouble(EXP_POLY[3]));
  if AbsTop = 0 then
    Exit(ExpSpecialCase(Tmp, SBits, Ki));
  Scale := BitsToDouble(SBits);
  Result := Scale + Scale * Tmp;
end;

// x^y for a finite, non-zero x and a finite, non-zero y, where y is an
// integer when x is negative.
function FinitePower(const AX, AY: Double): Double;
var
  Ix, Iy: UInt64;
  TopX, TopY: UInt32;
  SignBias: UInt32;
  Hi, Lo, YHi, YLo, LHi, LLo, EHi, ELo: Double;
begin
  SignBias := 0;
  Ix := DoubleToBits(AX);
  Iy := DoubleToBits(AY);
  TopX := Top12(AX);
  TopY := Top12(AY);
  if (TopX = 0) or (TopX >= EXPONENT_FIELD) or
     ((TopY and EXPONENT_FIELD) < TOP_TWO_POW_MINUS_65) or
     ((TopY and EXPONENT_FIELD) >= TOP_TWO_POW_63) then
  begin
    // x negative or subnormal, or |y| < 2^-65 or |y| >= 2^63.
    if (Ix shr 63) <> 0 then
    begin
      if CheckInteger(Iy) = 1 then
        SignBias := EXP_SIGN_BIAS;
      Ix := Ix and ABS_MASK;
      TopX := TopX and EXPONENT_FIELD;
    end;
    if ((TopY and EXPONENT_FIELD) < TOP_TWO_POW_MINUS_65) or
       ((TopY and EXPONENT_FIELD) >= TOP_TWO_POW_63) then
    begin
      // y is not odd here: an |y| below 2^-65 is not an integer and one of
      // 2^63 or more is even.
      if Ix = ONE_BITS then
        Exit(1.0);
      if (TopY and EXPONENT_FIELD) < TOP_TWO_POW_MINUS_65 then
      begin
        // |y| < 2^-65: x^y ~= 1 + y log(x).
        if Ix > ONE_BITS then
          Exit(1.0 + AY);
        Exit(1.0 - AY);
      end;
      if (Ix > ONE_BITS) = (TopY < $800) then
        Exit(Infinity);
      Exit(0.0);
    end;
    if TopX = 0 then
    begin
      // Normalize a subnormal x so its exponent field goes below zero.
      Ix := DoubleToBits(AX * BitsToDouble(TWO_POW_52)) and ABS_MASK;
      Ix := Ix - (UInt64(52) shl 52);
    end;
  end;

  Hi := LogInline(Ix, Lo);
  // y log(x) as EHi + ELo, splitting y and log(x) so the products are
  // exact.
  YHi := BitsToDouble(Iy and LOW_27_CLEAR);
  YLo := AY - YHi;
  LHi := BitsToDouble(DoubleToBits(Hi) and LOW_27_CLEAR);
  LLo := Hi - LHi + Lo;
  EHi := YHi * LHi;
  ELo := YLo * LHi + AY * LLo;
  Result := ExpInline(EHi, ELo, SignBias);
end;

{$IFDEF FPC}
  {$POP}
{$ENDIF}

function IsOddIntegralNumber(const AValue: Double): Boolean; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := not IsNan(AValue) and not IsInfinite(AValue) and
    (Frac(AValue) = 0.0) and (Frac(AValue / 2.0) <> 0.0);
end;

function CanonicalNaN: Double; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := BitsToDouble(CANONICAL_FLOAT64_NAN_BITS);
end;

// ES2026 §6.1.6.1.3 Number::exponentiate(base, exponent)
function NumberExponentiation(const ABase, AExponent: Double): Double;
var
  AbsoluteBase: Double;
begin
  // ES2026 §6.1.6.1.3 steps 1-3: exponent zero takes precedence.
  if IsNan(AExponent) then
    Exit(CanonicalNaN);
  if AExponent = 0.0 then
    Exit(1.0);
  if IsNan(ABase) then
    Exit(CanonicalNaN);

  // ES2026 §6.1.6.1.3 steps 4-5: infinite bases preserve odd signs.
  if IsInfinite(ABase) then
  begin
    if ABase > 0.0 then
    begin
      if AExponent > 0.0 then
        Exit(Infinity);
      Exit(0.0);
    end;

    if AExponent > 0.0 then
    begin
      if IsOddIntegralNumber(AExponent) then
        Exit(NegInfinity);
      Exit(Infinity);
    end;
    if IsOddIntegralNumber(AExponent) then
      Exit(BitsToDouble(UInt64(1) shl 63));
    Exit(0.0);
  end;

  // ES2026 §6.1.6.1.3 steps 6-7: signed-zero bases preserve odd signs.
  if ABase = 0.0 then
  begin
    if not IsNegativeZero(ABase) then
    begin
      if AExponent > 0.0 then
        Exit(0.0);
      Exit(Infinity);
    end;

    if AExponent > 0.0 then
    begin
      if IsOddIntegralNumber(AExponent) then
        Exit(BitsToDouble(UInt64(1) shl 63));
      Exit(0.0);
    end;
    if IsOddIntegralNumber(AExponent) then
      Exit(NegInfinity);
    Exit(Infinity);
  end;

  // ES2026 §6.1.6.1.3 steps 9-10: infinite exponents compare |base| to 1.
  if IsInfinite(AExponent) then
  begin
    AbsoluteBase := Abs(ABase);
    if AbsoluteBase = 1.0 then
      Exit(CanonicalNaN);
    if (AbsoluteBase > 1.0) = (AExponent > 0.0) then
      Exit(Infinity);
    Exit(0.0);
  end;

  // ES2026 §6.1.6.1.3 step 12: negative base requires an integral exponent.
  if (ABase < 0.0) and (Frac(AExponent) <> 0.0) then
    Exit(CanonicalNaN);

  // ES2026 §6.1.6.1.3 step 13: implementation-approximated exponentiation.
  // x * x and the square root are correctly rounded, and V8 answers these
  // two exponents with them too.
  if AExponent = 2.0 then
    Exit(ABase * ABase);
  if AExponent = 0.5 then
    Exit(Sqrt(ABase));
  Result := FinitePower(ABase, AExponent);
end;

end.
