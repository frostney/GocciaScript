import { handleTestRequest } from "@/lib/goccia-api";

export const runtime = "nodejs";
export const dynamic = "force-dynamic";

export async function POST(req: Request) {
  return handleTestRequest(req);
}
