import type { Metadata } from "next";
import { PerformanceDashboard } from "@/components/performance-dashboard";
import { loadPerformanceDashboardData } from "@/lib/performance-dashboard";

export const dynamic = "force-dynamic";

export const metadata: Metadata = {
  title: "Performance Barometer",
  description:
    "Directional GocciaScript performance trends against reference engines using pinned AWFY and JetStream 3 workloads.",
  alternates: { canonical: "/performance" },
  openGraph: {
    title: "Performance Barometer · GocciaScript",
    description:
      "Directional GocciaScript performance trends against QuickJS and Node.js reference measurements.",
    url: "/performance",
  },
  twitter: {
    title: "Performance Barometer · GocciaScript",
    description:
      "Directional GocciaScript performance trends against QuickJS and Node.js reference measurements.",
  },
};

export default async function PerformancePage() {
  const data = await loadPerformanceDashboardData();
  return <PerformanceDashboard data={data} />;
}
