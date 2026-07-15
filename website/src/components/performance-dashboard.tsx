"use client";

import { useMemo, useState } from "react";
import type {
  PerformanceDashboardData,
  PerformanceSuite,
  PerformanceSuiteData,
  PerformanceTarget,
  PerformanceTimelinePoint,
} from "@/lib/performance-dashboard";

type TimelineRange = "14d" | "30d" | "all";

const RANGE_OPTIONS: Array<{
  value: TimelineRange;
  label: string;
  days: number;
}> = [
  { value: "14d", label: "Last 14 days", days: 14 },
  { value: "30d", label: "Last 30 days", days: 30 },
  { value: "all", label: "All time", days: Number.POSITIVE_INFINITY },
];

const SERIES = [
  {
    key: "quickjsRatio",
    label: "QuickJS reference",
    color: "var(--perf-quickjs)",
  },
  {
    key: "nodeRatio",
    label: "Node.js reference",
    color: "var(--perf-node)",
  },
] as const;

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
  if (Number.isNaN(date.getTime())) return value;
  return new Intl.DateTimeFormat("en-US", {
    month: "short",
    day: "numeric",
    timeZone: "UTC",
  }).format(date);
}

function ratio(value: number | null): string {
  return value === null ? "—" : `${value.toFixed(2)}×`;
}

function measurement(target: PerformanceTarget, value: number | null): string {
  if (value === null) return "—";
  if (target.unit === "score") return `${value.toFixed(2)} pts`;
  if (value >= 1_000_000) return `${(value / 1_000_000).toFixed(2)}s`;
  if (value >= 1_000) return `${(value / 1_000).toFixed(2)}ms`;
  return `${value.toFixed(2)}µs`;
}

function filterTimeline(
  points: PerformanceTimelinePoint[],
  range: TimelineRange,
): PerformanceTimelinePoint[] {
  if (range === "all" || points.length === 0) return points;
  const days = RANGE_OPTIONS.find((option) => option.value === range)?.days;
  const latest = Date.parse(points.at(-1)?.createdAt ?? "");
  if (!days || !Number.isFinite(latest)) return points;
  const start = latest - (days - 1) * 24 * 60 * 60 * 1000;
  return points.filter((point) => Date.parse(point.createdAt) >= start);
}

function suiteLabel(suite: PerformanceSuite): string {
  return suite === "awfy" ? "Are We Fast Yet" : "JetStream 3";
}

function pointLabel(point: PerformanceTimelinePoint): string {
  if (!point.complete) {
    return `${dateLabel(point.createdAt)}: incomplete; ${point.failedWorkloadCount} failed workload${point.failedWorkloadCount === 1 ? "" : "s"}`;
  }
  return `${dateLabel(point.createdAt)}: QuickJS ${ratio(point.quickjsRatio)}, Node.js ${ratio(point.nodeRatio)}`;
}

function timelineSegments(
  points: PerformanceTimelinePoint[],
): PerformanceTimelinePoint[][] {
  const segments: PerformanceTimelinePoint[][] = [];
  let current: PerformanceTimelinePoint[] = [];
  for (const point of points) {
    const previous = current.at(-1);
    if (
      !point.complete ||
      (previous && previous.compatibilityKey !== point.compatibilityKey)
    ) {
      if (current.length > 0) segments.push(current);
      current = [];
    }
    if (point.complete) current.push(point);
  }
  if (current.length > 0) segments.push(current);
  return segments;
}

function PerformanceChart({
  suite,
  points,
}: {
  suite: PerformanceSuite;
  points: PerformanceTimelinePoint[];
}) {
  if (points.length === 0) {
    return (
      <p className="perf-empty-chart">
        No retained {suiteLabel(suite)} reports yet.
      </p>
    );
  }
  const width = 920;
  const height = 320;
  const padX = 58;
  const padY = 40;
  const values = points.flatMap((point) =>
    [point.quickjsRatio, point.nodeRatio].filter(
      (value): value is number => value !== null,
    ),
  );
  const maximum = Math.max(1, ...values);
  const minimum = Math.min(1, ...values);
  const logMinimum = Math.log(Math.max(0.01, minimum));
  const logMaximum = Math.log(maximum);
  const logSpan = Math.max(0.2, logMaximum - logMinimum);
  const low = Math.exp(logMinimum - logSpan * 0.08);
  const high = Math.exp(logMaximum + logSpan * 0.08);
  const x = (index: number) =>
    points.length === 1
      ? width / 2
      : padX + ((width - padX * 2) * index) / (points.length - 1);
  const y = (value: number) =>
    padY +
    ((height - padY * 2) * (Math.log(high) - Math.log(value))) /
      (Math.log(high) - Math.log(low));
  const pointIndex = new Map(
    points.map((point, index) => [point.runId, index]),
  );
  const ticks = [low, Math.sqrt(low * high), high];
  const segments = timelineSegments(points);

  return (
    <div className="perf-chart-shell">
      <svg
        className="perf-chart"
        viewBox={`0 0 ${width} ${height}`}
        role="img"
        aria-labelledby={`${suite}-chart-title ${suite}-chart-description`}
      >
        <title
          id={`${suite}-chart-title`}
        >{`${suiteLabel(suite)} reference ratio over time`}</title>
        <desc id={`${suite}-chart-description`}>
          QuickJS and Node.js reference ratios on a logarithmic scale. One means
          aligned; values above one mean Goccia was proportionally slower. Lines
          break when a run is incomplete or measurement versions change.
        </desc>
        {ticks.map((tick) => (
          <g key={tick}>
            <line
              x1={padX}
              x2={width - padX}
              y1={y(tick)}
              y2={y(tick)}
              className="perf-chart-grid"
            />
            <text x={8} y={y(tick) + 4} className="perf-chart-label">
              {tick.toFixed(1)}×
            </text>
          </g>
        ))}
        <line
          x1={padX}
          x2={width - padX}
          y1={y(1)}
          y2={y(1)}
          className="perf-chart-parity"
        />
        {SERIES.flatMap((series) =>
          segments.map((segment, segmentIndex) => {
            const path = segment
              .map((point, index) => {
                const timelineIndex = pointIndex.get(point.runId) ?? 0;
                const value = point[series.key];
                return `${index === 0 ? "M" : "L"} ${x(timelineIndex).toFixed(2)} ${y(value ?? 1).toFixed(2)}`;
              })
              .join(" ");
            return (
              <path
                key={`${series.key}-${segmentIndex}`}
                d={path}
                fill="none"
                stroke={series.color}
                className="perf-chart-line"
              />
            );
          }),
        )}
        {points.map((point, index) =>
          point.complete ? (
            SERIES.map((series) => {
              const value = point[series.key];
              if (value === null) return null;
              return (
                <a
                  key={`${point.runId}-${series.key}`}
                  href={point.runUrl}
                  aria-label={`${series.label}. ${pointLabel(point)}. Open CI run.`}
                >
                  <circle
                    cx={x(index)}
                    cy={y(value)}
                    r={5}
                    fill={series.color}
                    className="perf-chart-point"
                  >
                    <title>{pointLabel(point)}</title>
                  </circle>
                </a>
              );
            })
          ) : (
            <a
              key={point.runId}
              href={point.runUrl}
              aria-label={`${pointLabel(point)}. Open CI run.`}
            >
              <path
                d={`M ${x(index) - 6} ${y(1) - 6} L ${x(index) + 6} ${y(1) + 6} M ${x(index) + 6} ${y(1) - 6} L ${x(index) - 6} ${y(1) + 6}`}
                className="perf-chart-failure"
              >
                <title>{pointLabel(point)}</title>
              </path>
            </a>
          ),
        )}
        <text x={padX} y={height - 10} className="perf-chart-date">
          {shortDate(points[0]?.createdAt ?? "")}
        </text>
        <text
          x={width - padX}
          y={height - 10}
          textAnchor="end"
          className="perf-chart-date"
        >
          {shortDate(points.at(-1)?.createdAt ?? "")}
        </text>
      </svg>
      <ul className="perf-legend" aria-label="Chart legend">
        {SERIES.map((series) => (
          <li key={series.key}>
            <span
              style={{ backgroundColor: series.color }}
              aria-hidden="true"
            />
            {series.label}
          </li>
        ))}
        <li>
          <span className="perf-legend-failure" aria-hidden="true">
            ×
          </span>
          Incomplete run
        </li>
      </ul>
    </div>
  );
}

function LatestMetric({
  suite,
  data,
}: {
  suite: PerformanceSuite;
  data: PerformanceSuiteData;
}) {
  const point = data.latestComplete;
  return (
    <article className="compat-metric perf-metric">
      <span>{suiteLabel(suite)}</span>
      <strong>{point ? ratio(point.quickjsRatio) : "—"}</strong>
      <small>QuickJS reference</small>
      <strong>{point ? ratio(point.nodeRatio) : "—"}</strong>
      <small>Node.js reference</small>
      <p>
        {point
          ? `${point.stale ? "Last complete" : "Latest"} · ${dateLabel(point.createdAt)}`
          : "No complete report"}
      </p>
    </article>
  );
}

function TimelineTable({
  suite,
  points,
}: {
  suite: PerformanceSuite;
  points: PerformanceTimelinePoint[];
}) {
  return (
    <details className="perf-data-details">
      <summary>{suiteLabel(suite)} chart data</summary>
      <div className="perf-table-scroll">
        <table>
          <thead>
            <tr>
              <th scope="col">Date</th>
              <th scope="col">QuickJS</th>
              <th scope="col">Node.js</th>
              <th scope="col">Status</th>
              <th scope="col">Commit</th>
            </tr>
          </thead>
          <tbody>
            {points.map((point) => (
              <tr key={point.runId}>
                <th scope="row">{dateLabel(point.createdAt)}</th>
                <td>{ratio(point.quickjsRatio)}</td>
                <td>{ratio(point.nodeRatio)}</td>
                <td>{point.complete ? "Complete" : "Incomplete"}</td>
                <td>
                  <a href={point.runUrl}>{point.shortSha}</a>
                </td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    </details>
  );
}

function WorkloadTable({
  suite,
  targets,
}: {
  suite: PerformanceSuite;
  targets: PerformanceTarget[];
}) {
  if (targets.length === 0) return null;
  return (
    <div className="compat-group-table perf-workloads">
      <h3>{suiteLabel(suite)}</h3>
      <div className="perf-table-scroll">
        <table>
          <thead>
            <tr>
              <th scope="col">Workload</th>
              <th scope="col">Goccia</th>
              <th scope="col">QuickJS</th>
              <th scope="col">Node.js</th>
              <th scope="col">QJS ratio</th>
              <th scope="col">Node ratio</th>
              <th scope="col">Status</th>
            </tr>
          </thead>
          <tbody>
            {targets.map((target) => (
              <tr key={target.name}>
                <th scope="row">{target.name}</th>
                <td>{measurement(target, target.goccia)}</td>
                <td>{measurement(target, target.quickjs)}</td>
                <td>{measurement(target, target.node)}</td>
                <td>{ratio(target.quickjsRatio)}</td>
                <td>{ratio(target.nodeRatio)}</td>
                <td>{target.failure ?? "Complete"}</td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    </div>
  );
}

export function PerformanceDashboard({
  data,
}: {
  data: PerformanceDashboardData;
}) {
  const [range, setRange] = useState<TimelineRange>("30d");
  const awfyPoints = useMemo(
    () => filterTimeline(data.awfy.timeline, range),
    [data.awfy.timeline, range],
  );
  const jetStreamPoints = useMemo(
    () => filterTimeline(data.jetstream.timeline, range),
    [data.jetstream.timeline, range],
  );

  if (data.status !== "ready") {
    return (
      <div className="compat-page container">
        <section className="compat-empty">
          <strong>Performance Barometer unavailable</strong>
          <p>{data.message}</p>
          <a href={data.source.workflowUrl}>View benchmark workflow</a>
        </section>
      </div>
    );
  }

  return (
    <div className="compat-page perf-page container">
      <section className="compat-hero perf-hero">
        <div>
          <p className="section-kicker">reference engine barometer</p>
          <h1>Performance Barometer</h1>
          <p>
            A directional north-star view of GocciaScript against QuickJS and
            Node.js using pinned AWFY and JetStream 3 workloads. Different
            runtimes have different goals; this is not a product ranking.
          </p>
        </div>
        <div className="compat-hero-actions">
          <a href={data.source.workflowUrl}>View CI workflow</a>
          <a href="/compatibility">ECMAScript compatibility</a>
        </div>
      </section>

      <section
        className="compat-metrics perf-metrics"
        aria-label="Latest reference ratios"
      >
        <LatestMetric suite="awfy" data={data.awfy} />
        <LatestMetric suite="jetstream" data={data.jetstream} />
        <article className="compat-metric perf-explainer">
          <span>How to read it</span>
          <strong>1.00×</strong>
          <small>aligned measurement</small>
          <p>Above 1.00× means Goccia was proportionally slower.</p>
        </article>
      </section>

      <section className="compat-timeline-control" aria-label="Timeline range">
        <div>
          <span>Retained main-branch reports</span>
          <strong>
            {range === "all"
              ? "All time"
              : RANGE_OPTIONS.find((option) => option.value === range)?.label}
          </strong>
          <small>
            Lines break when corpus, driver, subset, or reference-engine
            versions change.
          </small>
        </div>
        <fieldset className="compat-range-buttons">
          <legend>Choose timeline range</legend>
          {RANGE_OPTIONS.map((option) => (
            <button
              key={option.value}
              type="button"
              aria-pressed={range === option.value}
              onClick={() => setRange(option.value)}
            >
              {option.label}
            </button>
          ))}
        </fieldset>
      </section>

      {(["awfy", "jetstream"] as const).map((suite) => {
        const points = suite === "awfy" ? awfyPoints : jetStreamPoints;
        return (
          <section
            className="compat-section"
            key={suite}
            aria-labelledby={`${suite}-timeline-heading`}
          >
            <div className="section-head">
              <p className="section-kicker">north-star trend</p>
              <h2 id={`${suite}-timeline-heading`}>{suiteLabel(suite)}</h2>
            </div>
            <PerformanceChart suite={suite} points={points} />
            <TimelineTable suite={suite} points={points} />
          </section>
        );
      })}

      <section
        className="compat-section"
        aria-labelledby="latest-workloads-heading"
      >
        <div className="section-head">
          <p className="section-kicker">latest report detail</p>
          <h2 id="latest-workloads-heading">Workload measurements</h2>
        </div>
        <div className="perf-workload-grid">
          <WorkloadTable suite="awfy" targets={data.awfy.targets} />
          <WorkloadTable suite="jetstream" targets={data.jetstream.targets} />
        </div>
      </section>

      <p className="compat-note">
        Incomplete runs remain visible as gaps. The last complete measurement is
        labeled stale rather than carried forward as current.
      </p>
    </div>
  );
}
