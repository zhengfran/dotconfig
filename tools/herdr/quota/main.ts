// herdr-owned quota refresher.
//
// Runs the vendored provider adapters and writes a single cache that
// status-quota.sh reads. This is the source of truth: nothing here reads or
// writes pi's cache, and pi does not need to be installed or running.
//
// Usage: main.ts [--provider all|claude,codex,...] [--quiet]
import { mkdir, readFile, rename, writeFile } from "node:fs/promises";
import { homedir } from "node:os";
import { join } from "node:path";
import { adapters } from "./adapters/index.ts";
import { runtime } from "./runtime.ts";
import type { UsageSnapshot } from "./types.ts";

const ADAPTER_TIMEOUT_MS = 20_000;

function cachePath(): string {
  const base = process.env.XDG_CACHE_HOME ?? join(homedir(), ".cache");
  return join(base, "herdr-status", "quota.json");
}

function arg(name: string): string | undefined {
  const i = process.argv.indexOf(name);
  return i >= 0 ? process.argv[i + 1] : undefined;
}

type Cache = { version: number; snapshots: Record<string, UsageSnapshot> };

async function readCache(path: string): Promise<Cache> {
  try {
    const parsed = JSON.parse(await readFile(path, "utf8")) as Cache;
    if (parsed && typeof parsed === "object" && parsed.snapshots) return parsed;
  } catch {
    // A missing or corrupt cache is not an error: it is rebuilt from scratch.
  }
  return { version: 1, snapshots: {} };
}

async function main(): Promise<void> {
  const quiet = process.argv.includes("--quiet");
  const requested = (arg("--provider") ?? "all").toLowerCase();
  const wanted =
    requested === "all"
      ? adapters
      : adapters.filter((a) =>
          requested.split(",").map((s) => s.trim()).includes(a.id),
        );

  if (wanted.length === 0) {
    console.error(`no matching provider in "${requested}"`);
    process.exitCode = 2;
    return;
  }

  const results = await Promise.all(
    wanted.map(async (adapter) => {
      const controller = new AbortController();
      const timer = setTimeout(() => controller.abort(), ADAPTER_TIMEOUT_MS);
      try {
        const snapshot = await adapter.fetch(runtime, controller.signal);
        return { id: adapter.id, label: adapter.label, snapshot };
      } catch (error) {
        return {
          id: adapter.id,
          label: adapter.label,
          error: error instanceof Error ? error.message : String(error),
        };
      } finally {
        clearTimeout(timer);
      }
    }),
  );

  const path = cachePath();
  await mkdir(join(path, ".."), { recursive: true });
  const cache = await readCache(path);

  for (const result of results) {
    // A provider that failed keeps its previous snapshot rather than blanking
    // the segment; the status bar ages it out on its own staleness rules.
    if ("snapshot" in result && result.snapshot) {
      cache.snapshots[result.id] = result.snapshot;
    }
  }

  const tmp = `${path}.tmp`;
  await writeFile(tmp, `${JSON.stringify(cache, null, 2)}\n`, { mode: 0o600 });
  await rename(tmp, path);

  if (!quiet) {
    for (const result of results) {
      if ("snapshot" in result && result.snapshot) {
        const windows = result.snapshot.windows
          .map((w) => {
            if (w.unlimited) return `${w.label} unlimited`;
            if (w.usedPercent !== undefined)
              return `${w.label} ${Math.round(w.usedPercent)}% used`;
            if (w.used !== undefined) return `${w.label} ${w.used} used`;
            return w.label;
          })
          .join(", ");
        console.log(`ok      ${result.label}: ${windows || "no windows"}`);
      } else {
        console.log(`failed  ${result.label}: ${(result as { error: string }).error}`);
      }
    }
  }
}

await main();
