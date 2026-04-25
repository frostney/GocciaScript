import type { SVGProps } from "react";

type IconProps = {
  size?: number;
  stroke?: string;
  fill?: string;
  sw?: number;
} & Omit<SVGProps<SVGSVGElement>, "stroke" | "fill">;

function Icon({
  size = 22,
  stroke = "currentColor",
  fill = "none",
  sw = 1.6,
  children,
  ...rest
}: IconProps & { children: React.ReactNode }) {
  return (
    <svg
      width={size}
      height={size}
      viewBox="0 0 24 24"
      fill={fill}
      stroke={stroke}
      strokeWidth={sw}
      strokeLinecap="round"
      strokeLinejoin="round"
      aria-hidden="true"
      {...rest}
    >
      {children}
    </svg>
  );
}

export function DropIcon({
  size = 22,
  fill = "currentColor",
}: {
  size?: number;
  fill?: string;
}) {
  return (
    <svg
      width={size}
      height={size}
      viewBox="0 0 24 24"
      fill={fill}
      aria-hidden="true"
    >
      <path d="M12 3 C 12 3, 5 11, 5 15.5 A 7 7 0 0 0 19 15.5 C 19 11, 12 3, 12 3 Z" />
    </svg>
  );
}

export const ShieldIcon = (p: IconProps) => (
  <Icon {...p}>
    <path d="M12 3 L 19 6 V 12 C 19 16.5 15.5 20 12 21 C 8.5 20 5 16.5 5 12 V 6 Z" />
    <path d="M9 12 l 2 2 l 4 -4" />
  </Icon>
);

export const ChipIcon = (p: IconProps) => (
  <Icon {...p}>
    <rect x="6" y="6" width="12" height="12" rx="1.5" />
    <path d="M9 3 v 3 M15 3 v 3 M9 18 v 3 M15 18 v 3 M3 9 h 3 M3 15 h 3 M18 9 h 3 M18 15 h 3" />
    <rect x="10" y="10" width="4" height="4" rx="0.5" />
  </Icon>
);

export const LeafIcon = (p: IconProps) => (
  <Icon {...p}>
    <path d="M4 20 C 4 10 10 4 20 4 C 20 14 14 20 4 20 Z" />
    <path d="M4 20 L 14 10" />
  </Icon>
);

export const StackIcon = (p: IconProps) => (
  <Icon {...p}>
    <path d="M12 3 L 21 8 L 12 13 L 3 8 Z" />
    <path d="M3 12 L 12 17 L 21 12" />
    <path d="M3 16 L 12 21 L 21 16" />
  </Icon>
);

export const ClockIcon = (p: IconProps) => (
  <Icon {...p}>
    <circle cx="12" cy="12" r="9" />
    <path d="M12 7 V 12 L 15 14" />
  </Icon>
);

export const RunIcon = (p: IconProps) => (
  <Icon {...p} fill="currentColor" sw={0}>
    <path d="M7 5 v 14 l 12 -7 z" />
  </Icon>
);

export const CopyIcon = (p: IconProps) => (
  <Icon {...p}>
    <rect x="8" y="8" width="12" height="12" rx="2" />
    <path d="M4 16 V 6 a 2 2 0 0 1 2 -2 h 10" />
  </Icon>
);

export const BookIcon = (p: IconProps) => (
  <Icon {...p}>
    <path d="M12 5 C 9 3.5 6 3.5 3 4 V 19 C 6 18.5 9 18.5 12 20 C 15 18.5 18 18.5 21 19 V 4 C 18 3.5 15 3.5 12 5 z" />
    <path d="M12 5 V 20" />
  </Icon>
);

export const PlayIcon = (p: IconProps) => (
  <Icon {...p}>
    <path d="M5 4 v 16 l 14 -8 z" />
  </Icon>
);

export const LockIcon = (p: IconProps) => (
  <Icon {...p}>
    <rect x="5" y="11" width="14" height="9" rx="2" />
    <path d="M8 11 V 7 a 4 4 0 0 1 8 0 V 11" />
  </Icon>
);

export const BotIcon = (p: IconProps) => (
  <Icon {...p}>
    <rect x="4" y="7" width="16" height="12" rx="3" />
    <circle cx="9" cy="13" r="1" fill="currentColor" />
    <circle cx="15" cy="13" r="1" fill="currentColor" />
    <path d="M12 3 V 7 M9 19 v 2 M15 19 v 2" />
  </Icon>
);

export const ArrowIcon = (p: IconProps) => (
  <Icon {...p}>
    <path d="M4 12 h 16 M14 6 l 6 6 l -6 6" />
  </Icon>
);

export const TerminalIcon = (p: IconProps) => (
  <Icon {...p}>
    <rect x="3" y="4" width="18" height="16" rx="2" />
    <path d="M7 9 l 3 3 l -3 3 M13 15 h 4" />
  </Icon>
);

export const SparkleIcon = (p: IconProps) => (
  <Icon {...p}>
    <path d="M12 3 l 2 6 l 6 2 l -6 2 l -2 6 l -2 -6 l -6 -2 l 6 -2 z" />
  </Icon>
);

export const GearIcon = (p: IconProps) => (
  <Icon {...p}>
    <circle cx="12" cy="12" r="3" />
    <path d="M12 3 v 2 M12 19 v 2 M3 12 h 2 M19 12 h 2 M5.6 5.6 l 1.4 1.4 M17 17 l 1.4 1.4 M5.6 18.4 l 1.4 -1.4 M17 7 l 1.4 -1.4" />
  </Icon>
);

export const MenuIcon = (p: IconProps) => (
  <Icon {...p}>
    <path d="M3 6h18M3 12h18M3 18h18" />
  </Icon>
);

export const CloseIcon = (p: IconProps) => (
  <Icon {...p}>
    <path d="M18 6L6 18M6 6l12 12" />
  </Icon>
);

export const SunIcon = (p: IconProps) => (
  <Icon {...p}>
    <circle cx="12" cy="12" r="4" />
    <path d="M12 2 v 2 M12 20 v 2 M2 12 h 2 M20 12 h 2 M4.6 4.6 l 1.4 1.4 M18 18 l 1.4 1.4 M4.6 19.4 l 1.4 -1.4 M18 6 l 1.4 -1.4" />
  </Icon>
);

export const MoonIcon = (p: IconProps) => (
  <Icon {...p}>
    <path d="M20 14.5 A 8 8 0 1 1 9.5 4 a 6 6 0 0 0 10.5 10.5 z" />
  </Icon>
);

export const StarIcon = (p: IconProps) => (
  <Icon {...p} fill="currentColor" sw={0}>
    <path d="M12 3 l 2.6 5.7 l 6.2 0.6 l -4.7 4.2 l 1.4 6.1 l -5.5 -3.3 l -5.5 3.3 l 1.4 -6.1 l -4.7 -4.2 l 6.2 -0.6 z" />
  </Icon>
);

export const GithubIcon = ({ size = 22 }: { size?: number }) => (
  <svg
    width={size}
    height={size}
    viewBox="0 0 24 24"
    fill="currentColor"
    aria-hidden="true"
  >
    <path d="M12 2C6.48 2 2 6.58 2 12.26c0 4.5 2.87 8.32 6.84 9.67.5.1.68-.22.68-.5v-1.7c-2.78.62-3.37-1.36-3.37-1.36-.46-1.18-1.11-1.5-1.11-1.5-.91-.63.07-.62.07-.62 1 .07 1.53 1.06 1.53 1.06.89 1.57 2.34 1.12 2.91.86.09-.66.35-1.12.63-1.38-2.22-.26-4.55-1.14-4.55-5.06 0-1.12.39-2.03 1.03-2.75-.1-.26-.45-1.3.1-2.7 0 0 .84-.27 2.75 1.05A9.42 9.42 0 0 1 12 6.84c.85 0 1.7.12 2.5.34 1.9-1.32 2.74-1.05 2.74-1.05.55 1.4.2 2.44.1 2.7.64.72 1.03 1.63 1.03 2.75 0 3.93-2.34 4.79-4.57 5.05.36.32.68.94.68 1.9v2.81c0 .27.18.6.69.5A10.04 10.04 0 0 0 22 12.26C22 6.58 17.52 2 12 2Z" />
  </svg>
);
