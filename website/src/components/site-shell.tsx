"use client";

import Image from "next/image";
import Link from "next/link";
import { usePathname } from "next/navigation";
import { useEffect, useLayoutEffect, useRef, useState } from "react";
import {
  CloseIcon,
  GithubIcon,
  MenuIcon,
  MoonIcon,
  StarIcon,
  SunIcon,
} from "@/components/icons";
import { formatStars, GITHUB_REPO_URL } from "@/lib/github";

const useIsoLayoutEffect =
  typeof window !== "undefined" ? useLayoutEffect : useEffect;

type IndicatorRect = {
  left: number;
  top: number;
  width: number;
  height: number;
};

type Theme = "cream" | "espresso";
type Tab = "home" | "install" | "docs" | "playground" | "sandbox";

const TABS: { id: Tab; label: string; href: string }[] = [
  { id: "home", label: "Home", href: "/" },
  { id: "install", label: "Install", href: "/installation" },
  { id: "docs", label: "Docs", href: "/docs" },
  { id: "playground", label: "Playground", href: "/playground" },
  { id: "sandbox", label: "Sandbox", href: "/sandbox" },
];

function activeTab(pathname: string): Tab {
  if (pathname === "/" || pathname === "") return "home";
  if (pathname.startsWith("/installation")) return "install";
  if (pathname.startsWith("/docs")) return "docs";
  if (pathname.startsWith("/playground")) return "playground";
  if (pathname.startsWith("/sandbox")) return "sandbox";
  return "home";
}

export function SiteShell({
  children,
  stars,
}: {
  children: React.ReactNode;
  stars?: number | null;
}) {
  const [theme, setTheme] = useState<Theme>("cream");
  const userOverrideRef = useRef(false);
  const skipNextThemeWriteRef = useRef(true);
  const pathname = usePathname() ?? "/";
  const active = activeTab(pathname);

  const tabBarRef = useRef<HTMLElement>(null);
  const [indicator, setIndicator] = useState<IndicatorRect | null>(null);
  // `data-ready` flips on once the *first* indicator transition lands so
  // the CSS can opt the element into transitions only after the initial
  // jump-to-position. Has to be state, not a ref — mutating a ref does
  // not re-render, so the attribute would never make it onto the DOM.
  const [indicatorReady, setIndicatorReady] = useState(false);
  const [menuOpen, setMenuOpen] = useState(false);

  useIsoLayoutEffect(() => {
    const bar = tabBarRef.current;
    if (!bar) return;
    const activeEl = bar.querySelector<HTMLElement>(
      '.nav-link[data-active="true"]',
    );
    if (!activeEl) return;
    const barRect = bar.getBoundingClientRect();
    const r = activeEl.getBoundingClientRect();
    setIndicator({
      left: r.left - barRect.left,
      top: r.top - barRect.top,
      width: r.width,
      height: r.height,
    });
  }, [active]);

  useEffect(() => {
    const onResize = () => {
      const bar = tabBarRef.current;
      if (!bar) return;
      const activeEl = bar.querySelector<HTMLElement>(
        '.nav-link[data-active="true"]',
      );
      if (!activeEl) return;
      const barRect = bar.getBoundingClientRect();
      const r = activeEl.getBoundingClientRect();
      setIndicator({
        left: r.left - barRect.left,
        top: r.top - barRect.top,
        width: r.width,
        height: r.height,
      });
    };
    window.addEventListener("resize", onResize);
    return () => window.removeEventListener("resize", onResize);
  }, []);

  // Close mobile menu on route change.
  // biome-ignore lint/correctness/useExhaustiveDependencies: only path change
  useEffect(() => {
    setMenuOpen(false);
  }, [pathname]);

  // Close mobile menu on Escape.
  useEffect(() => {
    if (!menuOpen) return;
    const onKey = (e: KeyboardEvent) => {
      if (e.key === "Escape") setMenuOpen(false);
    };
    window.addEventListener("keydown", onKey);
    return () => window.removeEventListener("keydown", onKey);
  }, [menuOpen]);

  // Mount once: sync state with the value the pre-paint script wrote, and
  // listen for system-theme changes (until the user makes an explicit choice).
  useEffect(() => {
    const fromDom = document.documentElement.dataset.theme;
    if (fromDom === "espresso" || fromDom === "cream") setTheme(fromDom);

    if (!window.matchMedia) return;
    const mq = window.matchMedia("(prefers-color-scheme: dark)");
    const onChange = (e: MediaQueryListEvent) => {
      if (!userOverrideRef.current) setTheme(e.matches ? "espresso" : "cream");
    };
    mq.addEventListener("change", onChange);
    return () => mq.removeEventListener("change", onChange);
  }, []);

  // Apply theme changes back to the DOM. Skipped on first commit because the
  // pre-paint script already set the correct value and the initial render's
  // state (cream) hasn't been reconciled yet.
  useEffect(() => {
    if (skipNextThemeWriteRef.current) {
      skipNextThemeWriteRef.current = false;
      return;
    }
    document.documentElement.dataset.theme = theme;
  }, [theme]);

  return (
    <div className="shell" data-screen-label={`app / ${active}`}>
      <header className="nav">
        <div className="container nav-inner">
          <Link href="/" className="brand">
            <Image src="/logo.png" alt="" width={36} height={36} priority />
            <span>
              Goccia<em>Script</em>
            </span>
          </Link>
          <button
            type="button"
            className="nav-mode"
            onClick={() => {
              userOverrideRef.current = true;
              setTheme(theme === "espresso" ? "cream" : "espresso");
            }}
            aria-label={
              theme === "espresso"
                ? "Switch to light mode"
                : "Switch to dark mode"
            }
            title={theme === "espresso" ? "Light mode" : "Dark mode"}
          >
            {theme === "espresso" ? (
              <SunIcon size={14} />
            ) : (
              <MoonIcon size={14} />
            )}
          </button>
          <nav
            id="primary-nav"
            className="nav-links"
            ref={tabBarRef}
            data-open={menuOpen}
          >
            {indicator && (
              <span
                className="nav-indicator"
                aria-hidden="true"
                style={{
                  transform: `translate(${indicator.left}px, ${indicator.top}px)`,
                  width: indicator.height ? indicator.width : 0,
                  height: indicator.height,
                }}
                onTransitionEnd={() => {
                  if (!indicatorReady) setIndicatorReady(true);
                }}
                data-ready={indicatorReady}
              />
            )}
            {TABS.map((t) => (
              <Link
                key={t.id}
                href={t.href}
                className="nav-link"
                data-active={active === t.id}
                // `aria-current="page"` is the WAI-ARIA contract for "this
                // is the page you're currently on" — screen readers announce
                // it; `data-active` is purely a styling hook and is not
                // exposed to assistive tech.
                aria-current={active === t.id ? "page" : undefined}
              >
                {t.label}
              </Link>
            ))}
            <a
              className="nav-cta nav-cta-github"
              href={GITHUB_REPO_URL}
              target="_blank"
              rel="noopener noreferrer"
            >
              <GithubIcon size={14} />
              <span>GitHub</span>
              {typeof stars === "number" && (
                <span
                  className="nav-stars"
                  role="img"
                  aria-label={`${stars} GitHub stars`}
                >
                  <StarIcon size={14} />
                  <span>{formatStars(stars)}</span>
                </span>
              )}
            </a>
          </nav>
          <button
            type="button"
            className="nav-hamburger"
            aria-label={menuOpen ? "Close menu" : "Open menu"}
            aria-expanded={menuOpen}
            aria-controls="primary-nav"
            onClick={() => setMenuOpen((o) => !o)}
          >
            {menuOpen ? <CloseIcon size={20} /> : <MenuIcon size={20} />}
          </button>
        </div>
      </header>

      <main className="flex-1">{children}</main>

      <footer className="footer">
        <div className="container footer-inner">
          <div>
            <div className="footer-brand">
              <Image src="/logo.png" alt="" width={28} height={28} />
              <span>
                Goccia<em className="text-accent italic">Script</em>
              </span>
            </div>
            <small className="block mt-2 max-w-[42ch]">
              A drop of JavaScript — a strict subset of ECMAScript 2027+,
              sandboxed by default.
            </small>
          </div>
        </div>
        <div className="mt-16">
          <div className="container">
            <div className="border-t border-rule-soft pt-6 text-[0.8rem] text-ink-3">
              <span>© 2026 GocciaScript contributors</span>
            </div>
          </div>
        </div>
      </footer>
    </div>
  );
}
