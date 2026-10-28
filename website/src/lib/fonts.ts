import localFont from "next/font/local";

/*
 * Self-hosted web fonts. The woff2 files in src/fonts are the exact files
 * Google Fonts serves for these families (see src/fonts/README.md), so the
 * production build no longer fetches anything from fonts.googleapis.com or
 * fonts.gstatic.com.
 *
 * Google splits each family into one file per unicode subset and the browser
 * downloads a subset only when a page uses a character in its unicode-range.
 * next/font requires literal options, so every subset is its own call. Only
 * the latin call of each family preloads. The family names, the three
 * --font-* CSS variables and the size-adjusted fallback faces live in
 * src/fonts/fonts.css, because next/font/local would otherwise derive the
 * family name used by its variable from the JavaScript constant name.
 */
import "../fonts/fonts.css";

const jetbrainsMonoLatin = localFont({
  src: [
    { path: "../fonts/jetbrains-mono/latin.woff2", weight: "400" },
    { path: "../fonts/jetbrains-mono/latin.woff2", weight: "500" },
    { path: "../fonts/jetbrains-mono/latin.woff2", weight: "700" },
  ],
  display: "swap",
  declarations: [
    { prop: "font-family", value: "JetBrains Mono" },
    {
      prop: "unicode-range",
      value:
        "U+0000-00FF, U+0131, U+0152-0153, U+02BB-02BC, U+02C6, U+02DA, U+02DC, U+0304, U+0308, U+0329, U+2000-206F, U+20AC, U+2122, U+2191, U+2193, U+2212, U+2215, U+FEFF, U+FFFD",
    },
  ],
  preload: true,
  adjustFontFallback: false,
});

const jetbrainsMonoLatinExt = localFont({
  src: [
    { path: "../fonts/jetbrains-mono/latin-ext.woff2", weight: "400" },
    { path: "../fonts/jetbrains-mono/latin-ext.woff2", weight: "500" },
    { path: "../fonts/jetbrains-mono/latin-ext.woff2", weight: "700" },
  ],
  display: "swap",
  declarations: [
    { prop: "font-family", value: "JetBrains Mono" },
    {
      prop: "unicode-range",
      value:
        "U+0100-02BA, U+02BD-02C5, U+02C7-02CC, U+02CE-02D7, U+02DD-02FF, U+0304, U+0308, U+0329, U+1D00-1DBF, U+1E00-1E9F, U+1EF2-1EFF, U+2020, U+20A0-20AB, U+20AD-20C4, U+2113, U+2C60-2C7F, U+A720-A7FF",
    },
  ],
  preload: false,
  adjustFontFallback: false,
});

const jetbrainsMonoCyrillic = localFont({
  src: [
    { path: "../fonts/jetbrains-mono/cyrillic.woff2", weight: "400" },
    { path: "../fonts/jetbrains-mono/cyrillic.woff2", weight: "500" },
    { path: "../fonts/jetbrains-mono/cyrillic.woff2", weight: "700" },
  ],
  display: "swap",
  declarations: [
    { prop: "font-family", value: "JetBrains Mono" },
    {
      prop: "unicode-range",
      value: "U+0301, U+0400-045F, U+0490-0491, U+04B0-04B1, U+2116",
    },
  ],
  preload: false,
  adjustFontFallback: false,
});

const jetbrainsMonoCyrillicExt = localFont({
  src: [
    { path: "../fonts/jetbrains-mono/cyrillic-ext.woff2", weight: "400" },
    { path: "../fonts/jetbrains-mono/cyrillic-ext.woff2", weight: "500" },
    { path: "../fonts/jetbrains-mono/cyrillic-ext.woff2", weight: "700" },
  ],
  display: "swap",
  declarations: [
    { prop: "font-family", value: "JetBrains Mono" },
    {
      prop: "unicode-range",
      value:
        "U+0460-052F, U+1C80-1C8A, U+20B4, U+2DE0-2DFF, U+A640-A69F, U+FE2E-FE2F",
    },
  ],
  preload: false,
  adjustFontFallback: false,
});

const jetbrainsMonoGreek = localFont({
  src: [
    { path: "../fonts/jetbrains-mono/greek.woff2", weight: "400" },
    { path: "../fonts/jetbrains-mono/greek.woff2", weight: "500" },
    { path: "../fonts/jetbrains-mono/greek.woff2", weight: "700" },
  ],
  display: "swap",
  declarations: [
    { prop: "font-family", value: "JetBrains Mono" },
    {
      prop: "unicode-range",
      value:
        "U+0370-0377, U+037A-037F, U+0384-038A, U+038C, U+038E-03A1, U+03A3-03FF",
    },
  ],
  preload: false,
  adjustFontFallback: false,
});

const jetbrainsMonoVietnamese = localFont({
  src: [
    { path: "../fonts/jetbrains-mono/vietnamese.woff2", weight: "400" },
    { path: "../fonts/jetbrains-mono/vietnamese.woff2", weight: "500" },
    { path: "../fonts/jetbrains-mono/vietnamese.woff2", weight: "700" },
  ],
  display: "swap",
  declarations: [
    { prop: "font-family", value: "JetBrains Mono" },
    {
      prop: "unicode-range",
      value:
        "U+0102-0103, U+0110-0111, U+0128-0129, U+0168-0169, U+01A0-01A1, U+01AF-01B0, U+0300-0301, U+0303-0304, U+0308-0309, U+0323, U+0329, U+1EA0-1EF9, U+20AB",
    },
  ],
  preload: false,
  adjustFontFallback: false,
});

const ibmPlexSansLatin = localFont({
  src: [
    { path: "../fonts/ibm-plex-sans/latin.woff2", weight: "400" },
    { path: "../fonts/ibm-plex-sans/latin.woff2", weight: "500" },
    { path: "../fonts/ibm-plex-sans/latin.woff2", weight: "600" },
    { path: "../fonts/ibm-plex-sans/latin.woff2", weight: "700" },
  ],
  display: "swap",
  declarations: [
    { prop: "font-family", value: "IBM Plex Sans" },
    {
      prop: "unicode-range",
      value:
        "U+0000-00FF, U+0131, U+0152-0153, U+02BB-02BC, U+02C6, U+02DA, U+02DC, U+0304, U+0308, U+0329, U+2000-206F, U+20AC, U+2122, U+2191, U+2193, U+2212, U+2215, U+FEFF, U+FFFD",
    },
  ],
  preload: true,
  adjustFontFallback: false,
});

const ibmPlexSansLatinExt = localFont({
  src: [
    { path: "../fonts/ibm-plex-sans/latin-ext.woff2", weight: "400" },
    { path: "../fonts/ibm-plex-sans/latin-ext.woff2", weight: "500" },
    { path: "../fonts/ibm-plex-sans/latin-ext.woff2", weight: "600" },
    { path: "../fonts/ibm-plex-sans/latin-ext.woff2", weight: "700" },
  ],
  display: "swap",
  declarations: [
    { prop: "font-family", value: "IBM Plex Sans" },
    {
      prop: "unicode-range",
      value:
        "U+0100-02BA, U+02BD-02C5, U+02C7-02CC, U+02CE-02D7, U+02DD-02FF, U+0304, U+0308, U+0329, U+1D00-1DBF, U+1E00-1E9F, U+1EF2-1EFF, U+2020, U+20A0-20AB, U+20AD-20C4, U+2113, U+2C60-2C7F, U+A720-A7FF",
    },
  ],
  preload: false,
  adjustFontFallback: false,
});

const ibmPlexSansCyrillic = localFont({
  src: [
    { path: "../fonts/ibm-plex-sans/cyrillic.woff2", weight: "400" },
    { path: "../fonts/ibm-plex-sans/cyrillic.woff2", weight: "500" },
    { path: "../fonts/ibm-plex-sans/cyrillic.woff2", weight: "600" },
    { path: "../fonts/ibm-plex-sans/cyrillic.woff2", weight: "700" },
  ],
  display: "swap",
  declarations: [
    { prop: "font-family", value: "IBM Plex Sans" },
    {
      prop: "unicode-range",
      value: "U+0301, U+0400-045F, U+0490-0491, U+04B0-04B1, U+2116",
    },
  ],
  preload: false,
  adjustFontFallback: false,
});

const ibmPlexSansCyrillicExt = localFont({
  src: [
    { path: "../fonts/ibm-plex-sans/cyrillic-ext.woff2", weight: "400" },
    { path: "../fonts/ibm-plex-sans/cyrillic-ext.woff2", weight: "500" },
    { path: "../fonts/ibm-plex-sans/cyrillic-ext.woff2", weight: "600" },
    { path: "../fonts/ibm-plex-sans/cyrillic-ext.woff2", weight: "700" },
  ],
  display: "swap",
  declarations: [
    { prop: "font-family", value: "IBM Plex Sans" },
    {
      prop: "unicode-range",
      value:
        "U+0460-052F, U+1C80-1C8A, U+20B4, U+2DE0-2DFF, U+A640-A69F, U+FE2E-FE2F",
    },
  ],
  preload: false,
  adjustFontFallback: false,
});

const ibmPlexSansGreek = localFont({
  src: [
    { path: "../fonts/ibm-plex-sans/greek.woff2", weight: "400" },
    { path: "../fonts/ibm-plex-sans/greek.woff2", weight: "500" },
    { path: "../fonts/ibm-plex-sans/greek.woff2", weight: "600" },
    { path: "../fonts/ibm-plex-sans/greek.woff2", weight: "700" },
  ],
  display: "swap",
  declarations: [
    { prop: "font-family", value: "IBM Plex Sans" },
    {
      prop: "unicode-range",
      value:
        "U+0370-0377, U+037A-037F, U+0384-038A, U+038C, U+038E-03A1, U+03A3-03FF",
    },
  ],
  preload: false,
  adjustFontFallback: false,
});

const ibmPlexSansVietnamese = localFont({
  src: [
    { path: "../fonts/ibm-plex-sans/vietnamese.woff2", weight: "400" },
    { path: "../fonts/ibm-plex-sans/vietnamese.woff2", weight: "500" },
    { path: "../fonts/ibm-plex-sans/vietnamese.woff2", weight: "600" },
    { path: "../fonts/ibm-plex-sans/vietnamese.woff2", weight: "700" },
  ],
  display: "swap",
  declarations: [
    { prop: "font-family", value: "IBM Plex Sans" },
    {
      prop: "unicode-range",
      value:
        "U+0102-0103, U+0110-0111, U+0128-0129, U+0168-0169, U+01A0-01A1, U+01AF-01B0, U+0300-0301, U+0303-0304, U+0308-0309, U+0323, U+0329, U+1EA0-1EF9, U+20AB",
    },
  ],
  preload: false,
  adjustFontFallback: false,
});

const instrumentSerifLatin = localFont({
  src: [
    {
      path: "../fonts/instrument-serif/latin.woff2",
      weight: "400",
      style: "normal",
    },
    {
      path: "../fonts/instrument-serif/latin-italic.woff2",
      weight: "400",
      style: "italic",
    },
  ],
  display: "swap",
  declarations: [
    { prop: "font-family", value: "Instrument Serif" },
    {
      prop: "unicode-range",
      value:
        "U+0000-00FF, U+0131, U+0152-0153, U+02BB-02BC, U+02C6, U+02DA, U+02DC, U+0304, U+0308, U+0329, U+2000-206F, U+20AC, U+2122, U+2191, U+2193, U+2212, U+2215, U+FEFF, U+FFFD",
    },
  ],
  preload: true,
  adjustFontFallback: false,
});

const instrumentSerifLatinExt = localFont({
  src: [
    {
      path: "../fonts/instrument-serif/latin-ext.woff2",
      weight: "400",
      style: "normal",
    },
    {
      path: "../fonts/instrument-serif/latin-ext-italic.woff2",
      weight: "400",
      style: "italic",
    },
  ],
  display: "swap",
  declarations: [
    { prop: "font-family", value: "Instrument Serif" },
    {
      prop: "unicode-range",
      value:
        "U+0100-02BA, U+02BD-02C5, U+02C7-02CC, U+02CE-02D7, U+02DD-02FF, U+0304, U+0308, U+0329, U+1D00-1DBF, U+1E00-1E9F, U+1EF2-1EFF, U+2020, U+20A0-20AB, U+20AD-20C4, U+2113, U+2C60-2C7F, U+A720-A7FF",
    },
  ],
  preload: false,
  adjustFontFallback: false,
});

/**
 * Every font face. next/font/local registers a face as a side effect of the
 * call; this list only keeps each call referenced.
 */
export const fontFaces = [
  jetbrainsMonoLatin,
  jetbrainsMonoLatinExt,
  jetbrainsMonoCyrillic,
  jetbrainsMonoCyrillicExt,
  jetbrainsMonoGreek,
  jetbrainsMonoVietnamese,
  ibmPlexSansLatin,
  ibmPlexSansLatinExt,
  ibmPlexSansCyrillic,
  ibmPlexSansCyrillicExt,
  ibmPlexSansGreek,
  ibmPlexSansVietnamese,
  instrumentSerifLatin,
  instrumentSerifLatinExt,
];
