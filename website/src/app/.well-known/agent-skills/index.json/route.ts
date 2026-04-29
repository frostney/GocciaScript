import {
  AGENT_SKILLS_INDEX_PATH,
  buildAgentSkillsIndex,
} from "@/lib/agent-discovery";

export const dynamic = "force-dynamic";

export function GET(req: Request) {
  const origin = new URL(req.url).origin;
  const selfHref = new URL(AGENT_SKILLS_INDEX_PATH, origin).toString();

  return Response.json(buildAgentSkillsIndex(origin), {
    headers: {
      Link: `<${selfHref}>; rel="self"; type="application/json"`,
    },
  });
}
