"use client";

import { type CSSProperties, useMemo, useRef, useState } from "react";
import type {
  Test262CategorySummary,
  Test262DashboardData,
  Test262GroupCoverage,
  Test262TimelinePoint,
} from "@/lib/test262-dashboard";

const CATEGORY_COLORS: Record<string, string | undefined> = {
  total: "#2d4a2b",
  "built-ins": "#b8651b",
  language: "#4d7fb8",
  intl402: "#8b5fa8",
  staging: "#b45f67",
  harness: "#6f7d42",
};

const GENERATED_CATEGORY_COLORS = [
  "#2f6f73",
  "#8a6f2a",
  "#9a5d7a",
  "#5f6ea6",
  "#7d6b4f",
  "#627a33",
  "#9b5c40",
  "#4f7b5d",
];

type TimelineRange = "14d" | "30d" | "all";

const TIMELINE_RANGES: { value: TimelineRange; label: string; days: number }[] =
  [
    { value: "14d", label: "Last 14 days", days: 14 },
    { value: "30d", label: "Last 30 days", days: 30 },
    { value: "all", label: "All time", days: Number.POSITIVE_INFINITY },
  ];

function fmt(value: number): string {
  return value.toLocaleString("en-US");
}

function percent(rate: number): string {
  return `${(rate * 100).toFixed(1)}%`;
}

function minutes(seconds: number): string {
  return `${(seconds / 60).toFixed(1)} min`;
}

function passRate(passed: number, run: number): number {
  return run > 0 ? passed / run : 0;
}

export function formatTest262CategoryLabel(category: string): string {
  return category
    .split("-")
    .filter(Boolean)
    .map((part, index) =>
      index > 0 && part.length <= 3
        ? part
        : `${part.charAt(0).toUpperCase()}${part.slice(1)}`,
    )
    .join("-");
}

function categoryColor(category: string): string {
  const known = CATEGORY_COLORS[category];
  if (known) return known;

  let hash = 0;
  for (let i = 0; i < category.length; i++) {
    hash = (hash * 31 + category.charCodeAt(i)) >>> 0;
  }
  return GENERATED_CATEGORY_COLORS[hash % GENERATED_CATEGORY_COLORS.length];
}

function dateLabel(value: string): string {
  const date = new Date(value);
  if (Number.isNaN(date.getTime())) return value;
  return new Intl.DateTimeFormat("en-US", {
    month: "short",
    day: "numeric",
    year: "numeric",
    timeZone: "UTC",
  }).format(date);
}

function shortDate(value: string): string {
  const date = new Date(value);
  if (Number.isNaN(date.getTime())) return "";
  return new Intl.DateTimeFormat("en-US", {
    month: "short",
    day: "numeric",
    timeZone: "UTC",
  }).format(date);
}

function utcDayTime(value: string): number | null {
  const day = value.slice(0, 10);
  if (!/^\d{4}-\d{2}-\d{2}$/.test(day)) return null;
  const time = Date.parse(`${day}T00:00:00Z`);
  return Number.isNaN(time) ? null : time;
}

function timelineRangeLabel(points: Test262TimelinePoint[]): string {
  if (points.length === 0) return "No data";
  const first = points[0];
  const latest = points[points.length - 1];
  return `${dateLabel(first.createdAt)} through ${dateLabel(latest.createdAt)}`;
}

function filterTimeline(
  points: Test262TimelinePoint[],
  range: TimelineRange,
): Test262TimelinePoint[] {
  if (range === "all" || points.length === 0) return points;
  const selected = TIMELINE_RANGES.find((option) => option.value === range);
  const latestTime = utcDayTime(points[points.length - 1].createdAt);
  if (!selected || latestTime === null) return points;
  const startTime = latestTime - (selected.days - 1) * 24 * 60 * 60 * 1000;
  return points.filter((point) => {
    const pointTime = utcDayTime(point.createdAt);
    return pointTime !== null && pointTime >= startTime;
  });
}

function categoryRate(point: Test262TimelinePoint, category: string): number {
  if (category === "total") {
    return passRate(point.summary.passed, point.summary.totalRun);
  }
  const item = point.summary.byCategory.find((c) => c.category === category);
  return item ? passRate(item.passed, item.run) : 0;
}

function xForPoint(
  points: Test262TimelinePoint[],
  index: number,
  width: number,
  padX: number,
): number {
  const usableW = width - padX * 2;
  return points.length === 1
    ? width / 2
    : padX + (usableW * index) / (points.length - 1);
}

function coverageLine(label: string, passed: number, run: number): string {
  return `${label}: ${percent(passRate(passed, run))} (${fmt(passed)} passed / ${fmt(run)} run)`;
}

function categoryCoverageLine(category: Test262CategorySummary): string {
  return coverageLine(
    formatTest262CategoryLabel(category.category),
    category.passed,
    category.run,
  );
}

function coverageTooltipLines(point: Test262TimelinePoint): string[] {
  return [
    `${dateLabel(point.createdAt)} - run #${point.runNumber} - ${point.shortSha}`,
    coverageLine("Total", point.summary.passed, point.summary.totalRun),
    ...point.summary.byCategory.map(categoryCoverageLine),
  ];
}

export function formatTest262TimelineTooltip(
  point: Test262TimelinePoint,
): string {
  return coverageTooltipLines(point).join("\n");
}

export function formatTest262DurationTooltip(
  point: Test262TimelinePoint,
): string {
  return [
    `${dateLabel(point.createdAt)} - run #${point.runNumber}`,
    `Commit ${point.shortSha}`,
    `Pass rate ${percent(passRate(point.summary.passed, point.summary.totalRun))}`,
    `${fmt(point.summary.passed)} passed / ${fmt(point.summary.totalRun)} run`,
    `${fmt(point.summary.failed)} failed, ${fmt(point.summary.timeouts)} timeouts`,
    `Duration ${minutes(point.summary.durationSeconds)}`,
  ].join("\n");
}

function CoverageTooltipContent({ point }: { point: Test262TimelinePoint }) {
  const lines = coverageTooltipLines(point);
  const [title, ...coverageLines] = lines;
  return (
    <>
      <strong>{title}</strong>
      {coverageLines.map((line, index) => (
        <span key={index}>{line}</span>
      ))}
    </>
  );
}

function DurationTooltipContent({ point }: { point: Test262TimelinePoint }) {
  return (
    <>
      <strong>
        {dateLabel(point.createdAt)} · run #{point.runNumber}
      </strong>
      <span>Commit {point.shortSha}</span>
      <span>
        {percent(passRate(point.summary.passed, point.summary.totalRun))} pass
        rate
      </span>
      <span>
        {fmt(point.summary.passed)} passed / {fmt(point.summary.totalRun)} run
      </span>
      <span>
        {fmt(point.summary.failed)} failed · {fmt(point.summary.timeouts)}{" "}
        timeouts
      </span>
      <span>Duration {minutes(point.summary.durationSeconds)}</span>
    </>
  );
}

function linePath(
  points: Test262TimelinePoint[],
  yForPoint: (point: Test262TimelinePoint) => number,
  width: number,
  height: number,
  padX: number,
  padY: number,
): string {
  if (points.length === 0) return "";
  const usableH = height - padY * 2;
  return points
    .map((point, index) => {
      const x = xForPoint(points, index, width, padX);
      const y = padY + usableH * (1 - yForPoint(point));
      return `${index === 0 ? "M" : "L"} ${x.toFixed(2)} ${y.toFixed(2)}`;
    })
    .join(" ");
}

function ChartHoverTargets({
  points,
  width,
  height,
  padX,
  padY,
  tooltipId,
  labelForPoint,
  onActivate,
  onDeactivate,
}: {
  points: Test262TimelinePoint[];
  width: number;
  height: number;
  padX: number;
  padY: number;
  tooltipId: string;
  labelForPoint: (point: Test262TimelinePoint) => string;
  onActivate: (index: number, target: HTMLElement) => void;
  onDeactivate: () => void;
}) {
  const usableW = width - padX * 2;
  const step = points.length > 1 ? usableW / (points.length - 1) : usableW;
  return (
    <div className="compat-chart-hover-targets" aria-hidden={false}>
      {points.map((point, index) => {
        const x = xForPoint(points, index, width, padX);
        const left = points.length === 1 ? padX : Math.max(padX, x - step / 2);
        const right =
          points.length === 1
            ? width - padX
            : Math.min(width - padX, x + step / 2);
        return (
          <button
            key={`hover-${point.runId}`}
            type="button"
            className="compat-chart-hover-target"
            style={
              {
                "--target-left": `${(left / width) * 100}%`,
                "--target-top": `${(padY / height) * 100}%`,
                "--target-width": `${((right - left) / width) * 100}%`,
                "--target-height": `${((height - padY * 2) / height) * 100}%`,
              } as CSSProperties
            }
            aria-describedby={tooltipId}
            aria-label={labelForPoint(point)}
            onPointerEnter={(event) => onActivate(index, event.currentTarget)}
            onPointerLeave={onDeactivate}
            onFocus={(event) => onActivate(index, event.currentTarget)}
            onBlur={onDeactivate}
            onClick={(event) => onActivate(index, event.currentTarget)}
          />
        );
      })}
    </div>
  );
}

function ChartTooltip({
  id,
  point,
  left,
  top,
  variant,
}: {
  id: string;
  point: Test262TimelinePoint | null;
  left: number;
  top: number;
  variant: "coverage" | "duration";
}) {
  if (!point) return null;
  return (
    <div
      id={id}
      role="tooltip"
      className="compat-chart-tooltip"
      data-placement="below"
      style={
        {
          "--tooltip-left": `${Math.min(92, Math.max(8, left))}%`,
          "--tooltip-top": `${Math.min(92, Math.max(8, top))}%`,
        } as CSSProperties
      }
    >
      {variant === "coverage" ? (
        <CoverageTooltipContent point={point} />
      ) : (
        <DurationTooltipContent point={point} />
      )}
    </div>
  );
}

function categoriesForTimeline(points: Test262TimelinePoint[]): string[] {
  const names = new Set<string>();
  for (const point of points) {
    for (const category of point.summary.byCategory) {
      names.add(category.category);
    }
  }
  return Array.from(names).sort();
}

function CoverageTimeline({ points }: { points: Test262TimelinePoint[] }) {
  const shellRef = useRef<HTMLDivElement>(null);
  const [activeIndex, setActiveIndex] = useState<number | null>(null);
  const [activeLeft, setActiveLeft] = useState(50);
  if (points.length === 0) return null;
  const width = 920;
  const height = 320;
  const padX = 54;
  const padY = 38;
  const categories = categoriesForTimeline(points);
  const yTicks = [0, 0.25, 0.5, 0.75, 1];
  const activePoint =
    activeIndex === null ? null : (points[activeIndex] ?? null);
  const activeY = activePoint
    ? ((padY +
        (height - padY * 2) *
          (1 -
            passRate(
              activePoint.summary.passed,
              activePoint.summary.totalRun,
            ))) /
        height) *
      100
    : 50;
  const tooltipId = "test262-pass-rate-tooltip";
  const activatePoint = (index: number, target: HTMLElement) => {
    setActiveIndex(index);
    const shell = shellRef.current;
    if (!shell) return;
    const targetRect = target.getBoundingClientRect();
    const shellRect = shell.getBoundingClientRect();
    const centerX = targetRect.left + targetRect.width / 2 - shellRect.left;
    setActiveLeft((centerX / shellRect.width) * 100);
  };

  return (
    <section className="compat-section" aria-labelledby="test262-timeline">
      <div className="section-head">
        <p className="section-kicker">time diagram</p>
        <h2 id="test262-timeline">test262 pass rate over time</h2>
      </div>
      <div className="compat-chart-shell" ref={shellRef}>
        <div className="compat-chart-wrap">
          <div className="compat-chart-stage">
            <svg
              className="compat-chart"
              viewBox={`0 0 ${width} ${height}`}
              role="img"
              aria-labelledby="test262-chart-title test262-chart-desc"
            >
              <title id="test262-chart-title">
                test262 pass rate over time
              </title>
              <desc id="test262-chart-desc">
                Pass-rate lines for each top-level test262 category across the
                latest available main-branch CI result for each day.
              </desc>
              {yTicks.map((tick) => {
                const y = padY + (height - padY * 2) * (1 - tick);
                return (
                  <g key={tick}>
                    <line
                      x1={padX}
                      y1={y}
                      x2={width - padX}
                      y2={y}
                      className="compat-chart-grid"
                    />
                    <text x={12} y={y + 4} className="compat-chart-label">
                      {Math.round(tick * 100)}%
                    </text>
                  </g>
                );
              })}
              {points.map((point, index) => {
                const x = xForPoint(points, index, width, padX);
                return (
                  <g key={point.runId}>
                    <line
                      x1={x}
                      y1={padY}
                      x2={x}
                      y2={height - padY}
                      className="compat-chart-tick"
                    />
                    {(index === 0 ||
                      index === points.length - 1 ||
                      index % 4 === 0) && (
                      <text
                        x={x}
                        y={height - 10}
                        className="compat-chart-date"
                        textAnchor="middle"
                      >
                        {shortDate(point.createdAt)}
                      </text>
                    )}
                  </g>
                );
              })}
              {categories.map((category) => (
                <path
                  key={category}
                  d={linePath(
                    points,
                    (point) => categoryRate(point, category),
                    width,
                    height,
                    padX,
                    padY,
                  )}
                  className="compat-chart-line"
                  style={
                    {
                      "--series-color": categoryColor(category),
                    } as CSSProperties
                  }
                />
              ))}
              {points.map((point, index) =>
                categories.map((category) => {
                  const x = xForPoint(points, index, width, padX);
                  const y =
                    padY +
                    (height - padY * 2) * (1 - categoryRate(point, category));
                  return (
                    <circle
                      key={`${point.runId}-${category}`}
                      cx={x}
                      cy={y}
                      r={3}
                      className="compat-chart-point"
                      style={
                        {
                          "--series-color": categoryColor(category),
                        } as CSSProperties
                      }
                    />
                  );
                }),
              )}
            </svg>
            <ChartHoverTargets
              points={points}
              width={width}
              height={height}
              padX={padX}
              padY={padY}
              tooltipId={tooltipId}
              labelForPoint={formatTest262TimelineTooltip}
              onActivate={activatePoint}
              onDeactivate={() => setActiveIndex(null)}
            />
          </div>
        </div>
        <ChartTooltip
          id={tooltipId}
          point={activePoint}
          left={activeLeft}
          top={activeY}
          variant="coverage"
        />
      </div>
      <ul className="compat-legend" aria-label="Chart series">
        {categories.map((category) => (
          <li
            key={category}
            style={
              {
                "--series-color": categoryColor(category),
              } as CSSProperties
            }
          >
            <span aria-hidden="true" />
            {formatTest262CategoryLabel(category)}
          </li>
        ))}
      </ul>
    </section>
  );
}

function DurationTimeline({ points }: { points: Test262TimelinePoint[] }) {
  const shellRef = useRef<HTMLDivElement>(null);
  const [activeIndex, setActiveIndex] = useState<number | null>(null);
  const [activeLeft, setActiveLeft] = useState(50);
  if (points.length === 0) return null;
  const width = 920;
  const height = 300;
  const padX = 54;
  const padY = 38;
  const maxSeconds = Math.max(
    ...points.map((point) => point.summary.durationSeconds),
    1,
  );
  const yTicks = [0, 0.25, 0.5, 0.75, 1];
  const activePoint =
    activeIndex === null ? null : (points[activeIndex] ?? null);
  const activeY = activePoint
    ? ((padY +
        (height - padY * 2) *
          (1 - activePoint.summary.durationSeconds / maxSeconds)) /
        height) *
      100
    : 50;
  const tooltipId = "test262-runtime-tooltip";
  const activatePoint = (index: number, target: HTMLElement) => {
    setActiveIndex(index);
    const shell = shellRef.current;
    if (!shell) return;
    const targetRect = target.getBoundingClientRect();
    const shellRect = shell.getBoundingClientRect();
    const centerX = targetRect.left + targetRect.width / 2 - shellRect.left;
    setActiveLeft((centerX / shellRect.width) * 100);
  };

  return (
    <section className="compat-section" aria-labelledby="test262-duration">
      <div className="section-head">
        <p className="section-kicker">performance</p>
        <h2 id="test262-duration">test262 runtime over time</h2>
      </div>
      <div className="compat-chart-shell" ref={shellRef}>
        <div className="compat-chart-wrap">
          <div className="compat-chart-stage">
            <svg
              className="compat-chart"
              viewBox={`0 0 ${width} ${height}`}
              role="img"
              aria-labelledby="test262-duration-title test262-duration-desc"
            >
              <title id="test262-duration-title">
                test262 runtime over time
              </title>
              <desc id="test262-duration-desc">
                Duration in minutes for the latest available main-branch test262
                CI result for each day.
              </desc>
              {yTicks.map((tick) => {
                const y = padY + (height - padY * 2) * (1 - tick);
                return (
                  <g key={tick}>
                    <line
                      x1={padX}
                      y1={y}
                      x2={width - padX}
                      y2={y}
                      className="compat-chart-grid"
                    />
                    <text x={12} y={y + 4} className="compat-chart-label">
                      {minutes(maxSeconds * tick)}
                    </text>
                  </g>
                );
              })}
              {points.map((point, index) => {
                const x = xForPoint(points, index, width, padX);
                return (
                  <g key={point.runId}>
                    <line
                      x1={x}
                      y1={padY}
                      x2={x}
                      y2={height - padY}
                      className="compat-chart-tick"
                    />
                    {(index === 0 ||
                      index === points.length - 1 ||
                      index % 4 === 0) && (
                      <text
                        x={x}
                        y={height - 10}
                        className="compat-chart-date"
                        textAnchor="middle"
                      >
                        {shortDate(point.createdAt)}
                      </text>
                    )}
                  </g>
                );
              })}
              <path
                d={linePath(
                  points,
                  (point) => point.summary.durationSeconds / maxSeconds,
                  width,
                  height,
                  padX,
                  padY,
                )}
                className="compat-chart-line compat-chart-line-total"
                style={{ "--series-color": "#6f7d42" } as CSSProperties}
              />
              {points.map((point, index) => {
                const x = xForPoint(points, index, width, padX);
                const y =
                  padY +
                  (height - padY * 2) *
                    (1 - point.summary.durationSeconds / maxSeconds);
                return (
                  <circle
                    key={`duration-${point.runId}`}
                    cx={x}
                    cy={y}
                    r={4}
                    className="compat-chart-point"
                    style={{ "--series-color": "#6f7d42" } as CSSProperties}
                  />
                );
              })}
            </svg>
            <ChartHoverTargets
              points={points}
              width={width}
              height={height}
              padX={padX}
              padY={padY}
              tooltipId={tooltipId}
              labelForPoint={formatTest262DurationTooltip}
              onActivate={activatePoint}
              onDeactivate={() => setActiveIndex(null)}
            />
          </div>
        </div>
        <ChartTooltip
          id={tooltipId}
          point={activePoint}
          left={activeLeft}
          top={activeY}
          variant="duration"
        />
      </div>
    </section>
  );
}

function TimelineRangeControl({
  value,
  onChange,
  visiblePoints,
  totalPoints,
}: {
  value: TimelineRange;
  onChange: (value: TimelineRange) => void;
  visiblePoints: Test262TimelinePoint[];
  totalPoints: number;
}) {
  return (
    <section className="compat-timeline-control" aria-label="Timeline range">
      <div>
        <span>Timeline</span>
        <strong>{timelineRangeLabel(visiblePoints)}</strong>
        <small>
          {fmt(visiblePoints.length)} of {fmt(totalPoints)} daily points
        </small>
      </div>
      <fieldset className="compat-range-buttons">
        <legend>Timeline range</legend>
        {TIMELINE_RANGES.map((option) => (
          <button
            key={option.value}
            type="button"
            aria-pressed={value === option.value}
            onClick={() => onChange(option.value)}
          >
            {option.label}
          </button>
        ))}
      </fieldset>
    </section>
  );
}

function Metric({
  label,
  value,
  detail,
}: {
  label: string;
  value: string;
  detail?: string;
}) {
  return (
    <div className="compat-metric">
      <span>{label}</span>
      <strong>{value}</strong>
      {detail && <small>{detail}</small>}
    </div>
  );
}

function CategorySplit({ point }: { point: Test262TimelinePoint }) {
  return (
    <section className="compat-section" aria-labelledby="test262-categories">
      <div className="section-head">
        <p className="section-kicker">groups</p>
        <h2 id="test262-categories">Top-level test groups</h2>
      </div>
      <div className="compat-category-grid">
        {point.summary.byCategory.map((category) => {
          const rate = passRate(category.passed, category.run);
          return (
            <div className="compat-category" key={category.category}>
              <div className="compat-category-head">
                <h3>{formatTest262CategoryLabel(category.category)}</h3>
                <strong>{percent(rate)}</strong>
              </div>
              <div
                className="compat-bar"
                style={{ "--coverage-rate": percent(rate) } as CSSProperties}
                aria-hidden="true"
              />
              <p>
                {fmt(category.passed)} passed / {fmt(category.run)} run
              </p>
            </div>
          );
        })}
      </div>
    </section>
  );
}

function GroupTable({
  title,
  id,
  groups,
}: {
  title: string;
  id: string;
  groups: Test262GroupCoverage[];
}) {
  return (
    <section className="compat-group-table" aria-labelledby={id}>
      <h3 id={id}>{title}</h3>
      <table>
        <thead>
          <tr>
            <th>Group</th>
            <th>Pass rate</th>
            <th>Passing</th>
          </tr>
        </thead>
        <tbody>
          {groups.map((group) => (
            <tr key={group.key}>
              <th scope="row">
                <code>{group.key}</code>
              </th>
              <td>
                <span className="compat-rate-cell">
                  <span
                    className="compat-rate-bar"
                    style={
                      {
                        "--coverage-rate": percent(group.passRate),
                      } as CSSProperties
                    }
                    aria-hidden="true"
                  />
                  <strong>{percent(group.passRate)}</strong>
                </span>
              </td>
              <td>
                {fmt(group.passed)} / {fmt(group.attempted)}
              </td>
            </tr>
          ))}
        </tbody>
      </table>
    </section>
  );
}

function EmptyState({ data }: { data: Test262DashboardData }) {
  return (
    <main className="container compat-page">
      <section className="compat-hero">
        <div>
          <p className="section-kicker">ECMAScript compatibility</p>
          <h1>test262 dashboard</h1>
          <p>Generated from main-branch test262 reports published by CI.</p>
        </div>
        <div className="compat-empty">
          <strong>test262 data unavailable.</strong>
          <p>{data.message ?? "No test262 data is available right now."}</p>
          <a href={data.source.workflowUrl}>Open CI workflow</a>
        </div>
      </section>
    </main>
  );
}

export function Test262Dashboard({ data }: { data: Test262DashboardData }) {
  const [timelineRange, setTimelineRange] = useState<TimelineRange>("30d");
  const visibleTimeline = useMemo(
    () => filterTimeline(data.timeline, timelineRange),
    [data.timeline, timelineRange],
  );

  if (data.status !== "ready" || !data.latest) {
    return <EmptyState data={data} />;
  }

  const latest = data.latest;
  const totalRate = passRate(latest.summary.passed, latest.summary.totalRun);

  return (
    <main className="container compat-page">
      <section className="compat-hero">
        <div>
          <p className="section-kicker">ECMAScript compatibility</p>
          <h1>test262 dashboard</h1>
          <p>
            Loaded from published main-branch test262 reports in Vercel Blob and
            cached for 15 minutes. The timeline keeps the latest available
            result for each day.
          </p>
        </div>
        <div className="compat-hero-actions">
          <a href={latest.jsonUrl}>JSON result</a>
          <a href={latest.runUrl}>GitHub run</a>
        </div>
      </section>

      <section className="compat-metrics" aria-label="Latest test262 summary">
        <Metric
          label="Latest pass rate"
          value={percent(totalRate)}
          detail={`${fmt(latest.summary.passed)} / ${fmt(
            latest.summary.totalRun,
          )}`}
        />
        <Metric
          label="Failed"
          value={fmt(latest.summary.failed)}
          detail={`${fmt(latest.summary.timeouts)} timeouts`}
        />
        <Metric
          label="Latest run"
          value={`#${latest.runNumber}`}
          detail={`${latest.shortSha} · ${dateLabel(latest.createdAt)}`}
        />
      </section>

      <TimelineRangeControl
        value={timelineRange}
        onChange={setTimelineRange}
        visiblePoints={visibleTimeline}
        totalPoints={data.timeline.length}
      />
      <CoverageTimeline points={visibleTimeline} />
      <DurationTimeline points={visibleTimeline} />
      <CategorySplit point={latest} />

      <section className="compat-section">
        <div className="section-head">
          <p className="section-kicker">coverage extremes</p>
          <h2>Grouped by test262 path</h2>
          <p>
            Groups use the first two path segments and include at least{" "}
            {data.source.minGroupTests} attempted tests.
          </p>
        </div>
        <div className="compat-groups-grid compat-groups-grid-single">
          <GroupTable
            id="least-covered-groups"
            title="Five least covered groups"
            groups={data.leastCovered}
          />
        </div>
      </section>
    </main>
  );
}
