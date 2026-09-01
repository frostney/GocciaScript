import type { Metadata } from "next";
import Link from "next/link";

export const metadata: Metadata = {
  title: "404: Page Not Found",
  description: "The requested page could not be found.",
  robots: {
    index: false,
    follow: false,
  },
  // Explicitly unset canonical to prevent homepage canonical leak.
  // Next.js metadata merging keeps parent values unless explicitly overridden.
  alternates: {
    canonical: null,
  },
  // Explicitly override OpenGraph to prevent homepage claims on 404.
  openGraph: {
    title: "404: Page Not Found",
    description: "The requested page could not be found.",
    url: undefined,
  },
};

export default function NotFoundPage() {
  return (
    <div className="container py-16">
      <div className="max-w-2xl mx-auto text-center">
        <h1 className="text-6xl font-bold mb-4">404</h1>
        <p className="text-2xl mb-8">Page Not Found</p>
        <p className="text-ink-2 mb-8">
          The page you're looking for doesn't exist or has been moved.
        </p>
        <div className="flex gap-4 justify-center">
          <Link href="/" className="btn btn-primary">
            Go to Homepage
          </Link>
          <Link href="/docs" className="btn btn-ghost">
            Browse Documentation
          </Link>
        </div>
      </div>
    </div>
  );
}
