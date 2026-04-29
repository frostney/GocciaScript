import {
  buildGocciaApiSkillMd,
  GOCCIA_API_SKILL_PATH,
} from "@/lib/agent-discovery";

export const dynamic = "force-dynamic";

export function GET(req: Request) {
  const origin = new URL(req.url).origin;
  const selfHref = new URL(GOCCIA_API_SKILL_PATH, origin).toString();

  return new Response(buildGocciaApiSkillMd(origin), {
    headers: {
      "Content-Type": "text/markdown; charset=utf-8",
      Link: `<${selfHref}>; rel="self"; type="text/markdown"`,
    },
  });
}
