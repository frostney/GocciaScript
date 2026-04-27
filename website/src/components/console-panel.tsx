import type { ReactNode } from "react";

/** Console-style output wrapper with a zsh/spaceship terminal feel.
 *
 *  Provides a slightly darker background than the surrounding editor
 *  panels so users immediately recognise the area as "console output".
 *  Drop `<AnimatedOutput>` (or any other output content) inside as
 *  `children`. Used across the playground, sandbox, and landing page
 *  hero for a consistent look. */
export function ConsolePanel({
  children,
  className = "",
}: {
  children: ReactNode;
  className?: string;
}) {
  return <div className={`console-panel ${className}`}>{children}</div>;
}
