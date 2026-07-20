# GocciaScript website

The marketing/docs site for GocciaScript — a Next.js 16 app under `website/`. Fumadocs reads `../README.md` and `../docs/**/*.md` directly, while Astryx provides the shared shell and generic controls. The warm-paper GocciaScript theme remains project-owned.

Bootstrapped with [`create-next-app`](https://nextjs.org/docs/app/api-reference/cli/create-next-app).

## Getting Started

First, run the development server:

```bash
bun install --frozen-lockfile
bun run dev
```

Open [http://localhost:3000](http://localhost:3000) with your browser to see the result.

Edit route UI under `src/app/` and `src/components/`. Edit documentation in the repository root; `source.config.ts` defines the collection and generated routes update automatically.

Fonts (Instrument Serif, IBM Plex Sans, and JetBrains Mono) are loaded through `next/font` in `src/app/layout.tsx`; the OG image generator (`src/app/opengraph-image.tsx`) embeds the matching display and body fonts.

## Learn More

To learn more about Next.js, take a look at the following resources:

- [Next.js Documentation](https://nextjs.org/docs) - learn about Next.js features and API.
- [Learn Next.js](https://nextjs.org/learn) - an interactive Next.js tutorial.

You can check out [the Next.js GitHub repository](https://github.com/vercel/next.js) - your feedback and contributions are welcome!

## Deploy on Vercel

The easiest way to deploy your Next.js app is to use the [Vercel Platform](https://vercel.com/new?utm_medium=default-template&filter=next.js&utm_source=create-next-app&utm_campaign=create-next-app-readme) from the creators of Next.js.

Check out our [Next.js deployment documentation](https://nextjs.org/docs/app/building-your-application/deploying) for more details.
