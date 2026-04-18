---
name: garmin-runcoach
description: Fetch, parse, and summarize Garmin Connect running and sleep data for coaching. Use when the user asks the coach/runcoach agent to read Garmin data, sync recent activities/sleep, update garmin-latest.json, generate training-volume/recovery summaries, or troubleshoot Garmin sync/login issues on this machine.
---

# Garmin Runcoach

Use this skill to pull Garmin Connect data into local JSON files, then summarize it into coach-friendly signals.

## Current machine setup

Use the working wrapper command:

```bash
garmin-runcoach all --days 14
```

Defaults on this machine:
- Wrapper: `/home/flanlc/.openclaw/workspace/scripts/garmin-runcoach`
- In PATH via: `/home/flanlc/.local/bin/garmin-runcoach`
- Python: `/home/flanlc/.openclaw/workspace/.venv/bin/python`
- JSON output: `/home/flanlc/.openclaw/workspace/runcoach/data/garmin-latest.json`
- Markdown output: `/home/flanlc/.openclaw/workspace/runcoach/data/garmin-summary.md`

## Credentials

Credential resolution order:
1. Existing `GARMIN_EMAIL` / `GARMIN_PASSWORD` env vars
2. `~/.netrc` entry for `connect.garmin.com` (fallback: `garmin`)

Do not assume a root-only `garmin.env` file exists.

## Common commands

Sync + summary:

```bash
garmin-runcoach all --days 30
```

Sync only:

```bash
garmin-runcoach sync --days 30 --out /home/flanlc/.openclaw/workspace/runcoach/data/garmin-latest.json
```

Summary only:

```bash
garmin-runcoach summary --in /home/flanlc/.openclaw/workspace/runcoach/data/garmin-latest.json --out /home/flanlc/.openclaw/workspace/runcoach/data/garmin-summary.md
```

## Coaching workflow

If the user asks to read/sync Garmin data,复盘, or arrange training:
1. Run sync or confirm `garmin-latest.json` is fresh.
2. Generate/read `garmin-summary.md`.
3. Base advice primarily on the summary.
4. If needed, supplement with recovery snapshots from `/home/flanlc/.openclaw/workspace/data/running/recovery/`.

## Troubleshooting

- If Garmin returns `429` or login issues, say clearly that it is Garmin rate limiting/login trouble.
- Do not misreport the problem as a credential permission error unless you actually verified that failure mode.
- The wrapper suppresses noisy fallback 429 lines when the overall sync still succeeds.
- If sync succeeds, trust the new files instead of stale March snapshots.
