import fs from "node:fs/promises";
import path from "node:path";
import { ImageResponse } from "next/og";

export const alt = "GocciaScript — A drop of JavaScript";
export const size = { width: 1200, height: 630 };
export const contentType = "image/png";

const FONT_URLS = {
  ibmPlexSans600:
    "https://fonts.gstatic.com/s/ibmplexsans/v23/zYXGKVElMYYaJe8bpLHnCwDKr932-G7dytD-Dmu1swZSAXcomDVmadSDNF5zAA.ttf",
  instrumentSerifRegular:
    "https://fonts.gstatic.com/s/instrumentserif/v5/jizBRFtNs2ka5fXjeivQ4LroWlx-2zI.ttf",
  instrumentSerifItalic:
    "https://fonts.gstatic.com/s/instrumentserif/v5/jizHRFtNs2ka5fXjeivQ4LroWlx-6zATiw.ttf",
};

async function fetchFont(url: string): Promise<ArrayBuffer> {
  const res = await fetch(url);
  if (!res.ok) throw new Error(`font fetch failed: ${url} (${res.status})`);
  return await res.arrayBuffer();
}

export default async function OpenGraphImage() {
  const [instrument, instrumentItalic, plex, logoBuf] = await Promise.all([
    fetchFont(FONT_URLS.instrumentSerifRegular),
    fetchFont(FONT_URLS.instrumentSerifItalic),
    fetchFont(FONT_URLS.ibmPlexSans600),
    fs.readFile(path.join(process.cwd(), "public", "logo.png")),
  ]);
  const logoDataUri = `data:image/png;base64,${logoBuf.toString("base64")}`;

  return new ImageResponse(
    <div
      style={{
        width: "100%",
        height: "100%",
        display: "flex",
        flexDirection: "column",
        justifyContent: "space-between",
        padding: "80px 96px",
        background: "#f4ecd8",
        backgroundImage:
          "radial-gradient(circle at 20% 10%, #ede3c8 0%, #f4ecd8 60%)",
        color: "#2a2419",
        fontFamily: "Instrument Serif",
      }}
    >
      <div style={{ display: "flex", alignItems: "center", gap: 22 }}>
        {/* biome-ignore lint/performance/noImgElement: ImageResponse renders to PNG, not HTML */}
        <img
          src={logoDataUri}
          alt=""
          width={80}
          height={80}
          style={{ display: "flex" }}
        />
        <div
          style={{
            display: "flex",
            fontSize: 56,
            letterSpacing: "-0.015em",
            lineHeight: 1,
          }}
        >
          Goccia
          <span
            style={{
              display: "flex",
              fontFamily: "Instrument Serif Italic",
              fontStyle: "italic",
              color: "#b8651b",
            }}
          >
            Script
          </span>
        </div>
      </div>

      <div style={{ display: "flex", flexDirection: "column", gap: 36 }}>
        <div
          style={{
            display: "flex",
            flexWrap: "wrap",
            gap: 22,
            fontSize: 144,
            lineHeight: 0.98,
            letterSpacing: "-0.025em",
          }}
        >
          <span style={{ display: "flex" }}>A</span>
          <span
            style={{
              display: "flex",
              fontFamily: "Instrument Serif Italic",
              fontStyle: "italic",
              color: "#b8651b",
            }}
          >
            drop
          </span>
          <span style={{ display: "flex" }}>of</span>
          <span style={{ display: "flex" }}>
            Java
            <span
              style={{
                display: "flex",
                fontFamily: "Instrument Serif Italic",
                fontStyle: "italic",
                color: "#4d4330",
              }}
            >
              Script
            </span>
            .
          </span>
        </div>
        <div
          style={{
            display: "flex",
            fontFamily: "IBM Plex Sans",
            fontSize: 32,
            fontWeight: 600,
            lineHeight: 1.35,
            color: "#4d4330",
            maxWidth: 980,
          }}
        >
          A strict subset of ECMAScript 2027+, sandboxed by default — built for
          AI agents and embedded apps.
        </div>
      </div>

      <div
        style={{
          display: "flex",
          justifyContent: "flex-end",
          alignItems: "center",
          fontFamily: "IBM Plex Sans",
          fontSize: 24,
          fontWeight: 600,
          color: "#7a6b4f",
          borderTop: "1px solid #c9b98e",
          paddingTop: 24,
        }}
      >
        <span style={{ display: "flex" }}>gocciascript.dev</span>
      </div>
    </div>,
    {
      ...size,
      fonts: [
        {
          name: "Instrument Serif",
          data: instrument,
          style: "normal",
          weight: 400,
        },
        {
          name: "Instrument Serif Italic",
          data: instrumentItalic,
          style: "italic",
          weight: 400,
        },
        { name: "IBM Plex Sans", data: plex, style: "normal", weight: 600 },
      ],
    },
  );
}
