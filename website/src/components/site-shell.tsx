"use client";

import { AppShell } from "@astryxdesign/core/AppShell";
import { IconButton } from "@astryxdesign/core/IconButton";
import { LinkProvider } from "@astryxdesign/core/Link";
import { MobileNav } from "@astryxdesign/core/MobileNav";
import { SideNavItem, SideNavSection } from "@astryxdesign/core/SideNav";
import { TopNav, TopNavHeading, TopNavItem } from "@astryxdesign/core/TopNav";
import Image from "next/image";
import Link from "next/link";
import { usePathname } from "next/navigation";
import { useEffect, useRef, useState } from "react";
import { GithubIcon, MoonIcon, StarIcon, SunIcon } from "@/components/icons";
import { formatStars, GITHUB_REPO_URL } from "@/lib/github";

type Theme = "cream" | "espresso";

const TABS = [
  { label: "Home", href: "/" },
  { label: "Installation", href: "/installation" },
  { label: "Docs", href: "/docs" },
  { label: "Compatibility", href: "/compatibility" },
  { label: "Performance", href: "/performance" },
  { label: "Playground", href: "/playground" },
  { label: "Sandbox", href: "/sandbox" },
] as const;

function isActive(pathname: string, href: string): boolean {
  return href === "/" ? pathname === "/" : pathname.startsWith(href);
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
  const pathname = usePathname() ?? "/";

  useEffect(() => {
    const fromDom = document.documentElement.dataset.theme;
    if (fromDom === "espresso" || fromDom === "cream") setTheme(fromDom);

    const media = window.matchMedia?.("(prefers-color-scheme: dark)");
    if (!media) return;
    if (fromDom !== "espresso" && fromDom !== "cream") {
      setTheme(media.matches ? "espresso" : "cream");
    }
    const onChange = (event: MediaQueryListEvent) => {
      if (!userOverrideRef.current) {
        setTheme(event.matches ? "espresso" : "cream");
      }
    };
    media.addEventListener("change", onChange);
    return () => media.removeEventListener("change", onChange);
  }, []);

  useEffect(() => {
    document.documentElement.dataset.theme = theme;
  }, [theme]);

  const navigation = (
    <div className="astryx-desktop-nav">
      {TABS.map((tab) => (
        <TopNavItem
          key={tab.href}
          as={Link}
          href={tab.href}
          label={tab.label}
          isSelected={isActive(pathname, tab.href)}
        />
      ))}
    </div>
  );

  const mobileNavigation = (
    <MobileNav header="GocciaScript" label="Primary navigation" side="end">
      <SideNavSection title="Explore" isHeaderHidden>
        {TABS.map((tab) => (
          <SideNavItem
            key={tab.href}
            as={Link}
            href={tab.href}
            label={tab.label}
            isSelected={isActive(pathname, tab.href)}
          />
        ))}
        <SideNavItem
          href={GITHUB_REPO_URL}
          label="GitHub"
          icon={<GithubIcon size={16} />}
        />
      </SideNavSection>
    </MobileNav>
  );

  return (
    <LinkProvider component={Link}>
      <div className="shell" data-screen-label="GocciaScript website">
        <AppShell
          height="auto"
          variant="section"
          contentPadding={0}
          mobileNav={{ content: mobileNavigation, breakpoint: "lg" }}
          topNav={
            <TopNav
              className="site-top-nav"
              label="Primary navigation"
              heading={
                <TopNavHeading
                  as={Link}
                  logo={
                    <span className="astryx-brand">
                      <Image
                        src="/logo.png"
                        alt=""
                        width={36}
                        height={36}
                        priority
                      />
                      <span>
                        Goccia<em>Script</em>
                      </span>
                    </span>
                  }
                  logoLabel="GocciaScript home"
                  headingHref="/"
                />
              }
              startContent={navigation}
              endContent={
                <div className="astryx-nav-actions">
                  <IconButton
                    label={
                      theme === "espresso"
                        ? "Switch to light mode"
                        : "Switch to dark mode"
                    }
                    icon={
                      theme === "espresso" ? (
                        <SunIcon size={14} />
                      ) : (
                        <MoonIcon size={14} />
                      )
                    }
                    variant="ghost"
                    size="sm"
                    aria-pressed={theme === "espresso"}
                    onClick={() => {
                      userOverrideRef.current = true;
                      setTheme(theme === "espresso" ? "cream" : "espresso");
                    }}
                  />
                  <div className="astryx-desktop-actions">
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
                  </div>
                </div>
              }
            />
          }
        >
          {children}
        </AppShell>

        <footer className="footer">
          <div className="container footer-inner">
            <div className="footer-brand-row">
              <div className="footer-brand">
                <Image src="/logo.png" alt="" width={28} height={28} />
                <span>
                  Goccia<em className="text-accent italic">Script</em>
                </span>
              </div>
              <small className="text-ink-3 text-[0.82rem]">
                — A drop of JavaScript, sandboxed by default.
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
    </LinkProvider>
  );
}
