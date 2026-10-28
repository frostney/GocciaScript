# Self-hosted fonts

These files are served from this site's own origin so the build and the
browser never contact fonts.googleapis.com or fonts.gstatic.com. They are wired
up in `src/lib/fonts.ts` and `src/fonts/fonts.css` (web fonts) and `src/app/opengraph-image.tsx` (OG
image). All three families are licensed under the SIL Open Font License 1.1;
each family folder carries its licence text in `OFL.txt`.

## Web fonts (`next/font/local`)

The woff2 files are the exact bytes Google Fonts served for these requests
(unicode subsets as separate files, as Google splits them):

```text
https://fonts.googleapis.com/css2?family=JetBrains+Mono:wght@400;500;700&family=IBM+Plex+Sans:wght@400;500;600;700&family=Instrument+Serif:ital@0;1&display=swap
```

| Folder | Family | Files | Weights and styles declared |
| --- | --- | --- | --- |
| `jetbrains-mono/` | JetBrains Mono (variable wght file) | latin, latin-ext, cyrillic, cyrillic-ext, greek, vietnamese | 400, 500, 700 normal |
| `ibm-plex-sans/` | IBM Plex Sans (variable wght file) | latin, latin-ext, cyrillic, cyrillic-ext, greek, vietnamese | 400, 500, 600, 700 normal |
| `instrument-serif/` | Instrument Serif (static) | latin, latin-ext (normal and `-italic`) | 400 normal, 400 italic |

JetBrains Mono and IBM Plex Sans are variable fonts, so one file serves every
declared weight. The `@font-face` rules still list each weight separately, the
same as `next/font/google` did, so weight matching in the browser is unchanged.

To refresh a font, fetch the CSS above with a current Chrome user agent,
download the `woff2` URLs it lists, and replace the matching files.

## OG image fonts (`og/`)

`next/og` (Satori) cannot read woff2, so `opengraph-image.tsx` reads these TTF
files at build time instead of fetching them over the network:

- `ibm-plex-sans-600.ttf`: IBM Plex Sans, used at weight 600
- `instrument-serif-regular.ttf` and `instrument-serif-italic.ttf`: Instrument Serif
