import {
  SideNav,
  SideNavHeading,
  SideNavItem,
  SideNavSection,
} from "@astryxdesign/core/SideNav";

export type DocsNavPage = {
  href: string;
  path: string;
  title: string;
};

const GROUPS = [
  { title: "Start here", matches: (path: string) => path === "README.md" },
  {
    title: "Guides & reference",
    matches: (path: string) => /^docs\/[^/]+\.md$/.test(path),
  },
  {
    title: "Contributing",
    matches: (path: string) => path.startsWith("docs/contributing/"),
  },
  {
    title: "Architecture decisions",
    matches: (path: string) => path === "docs/adr/README.md",
  },
] as const;

export function DocsNav({
  pages,
  activeHref,
}: {
  pages: DocsNavPage[];
  activeHref: string;
}) {
  return (
    <SideNav
      aria-label="Documentation"
      header={
        <SideNavHeading
          heading="Documentation"
          headingHref="/docs"
          subheading="From the repository"
        />
      }
    >
      {GROUPS.map((group) => {
        const items = pages.filter((page) => group.matches(page.path));
        if (items.length === 0) return null;
        return (
          <SideNavSection key={group.title} title={group.title}>
            {items.map((page) => (
              <SideNavItem
                key={page.href}
                href={page.href}
                label={page.title}
                isSelected={page.href === activeHref}
              />
            ))}
          </SideNavSection>
        );
      })}
    </SideNav>
  );
}
