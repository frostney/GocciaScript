import type { Metadata } from "next";
import { Test262Dashboard } from "@/components/test262-dashboard";
import { loadTest262DashboardData } from "@/lib/test262-dashboard";

export const dynamic = "force-dynamic";

export const metadata: Metadata = {
  title: "ECMAScript Compatibility",
  description:
    "test262 compatibility dashboard for GocciaScript, generated from main-branch CI reports.",
  alternates: { canonical: "/compatibility" },
  openGraph: {
    title: "ECMAScript Compatibility · GocciaScript",
    description:
      "test262 compatibility dashboard for GocciaScript, generated from main-branch CI reports.",
    url: "/compatibility",
  },
  twitter: {
    title: "ECMAScript Compatibility · GocciaScript",
    description:
      "test262 compatibility dashboard for GocciaScript, generated from main-branch CI reports.",
  },
};

export default async function CompatibilityPage() {
  const data = await loadTest262DashboardData();
  return <Test262Dashboard data={data} />;
}
