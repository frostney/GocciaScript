import type { Metadata } from "next";
import { PerformanceDashboard } from "@/components/performance-dashboard";
import { loadPerformanceDashboardData } from "@/lib/performance-dashboard";

export const dynamic = "force-dynamic";

export const metadata: Metadata = {
  title: "Performance Barometer",
  description:
    "GocciaScript's interpreter against QuickJS, the bar for its first performance target, on pinned AWFY and JetStream 3 workloads, with Node.js for scale.",
  alternates: { canonical: "/performance" },
  openGraph: {
    title: "Performance Barometer · GocciaScript",
    description:
      "GocciaScript's interpreter against QuickJS, the bar for its first performance target, with Node.js for scale.",
    url: "/performance",
  },
  twitter: {
    title: "Performance Barometer · GocciaScript",
    description:
      "GocciaScript's interpreter against QuickJS, the bar for its first performance target, with Node.js for scale.",
  },
};

export default async function PerformancePage() {
  const data = await loadPerformanceDashboardData();
  return <PerformanceDashboard data={data} />;
}
