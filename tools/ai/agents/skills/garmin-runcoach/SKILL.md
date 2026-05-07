---
name: garmin-runcoach
description: Fetch, parse, and summarize Garmin Connect running and sleep data for coaching. Use when the user asks the coach/runcoach agent to read Garmin data, sync recent activities/sleep, update garmin-latest.json, generate training-volume/recovery summaries, or troubleshoot Garmin sync/login issues on this machine.
---

# Garmin Runcoach

Pull Garmin Connect data into local JSON, then summarize it into coach-friendly markdown signals. The skill is **self-contained** — the wrapper and Python helpers live together under `scripts/`; no external paths.

## Quick start

```bash
garmin-runcoach all --days 14
```

Fetches the last 14 days and writes:
- `$DATA_DIR/garmin-latest.json` — raw activities + sleep
- `$DATA_DIR/garmin-summary.md` — coach summary

## Configuration (all optional)

Everything resolves from env vars with sensible defaults; **no path is hardcoded**.

| Env var | Default | What it does |
|---|---|---|
| `GARMIN_RUNCOACH_DATA_DIR` | `${XDG_DATA_HOME:-$HOME/.local/share}/garmin-runcoach` | Where JSON/MD outputs land |
| `GARMIN_RUNCOACH_PYTHON` | auto-detect (venv near data dir → `python3`) | Python with `garminconnect` installed |
| `GARMIN_EMAIL` / `GARMIN_PASSWORD` | read from `~/.netrc` if unset | Login |

## Credentials

Resolution order:
1. `GARMIN_EMAIL` + `GARMIN_PASSWORD` env vars
2. `~/.netrc` entry for `connect.garmin.com` (fallback host name: `garmin`)

Example `~/.netrc` (chmod 600):
```
machine connect.garmin.com
  login your.email@example.com
  password ***
```

## Python venv

The `garminconnect` package isn't in stdlib. Set up a venv once:

```bash
python3 -m venv "${XDG_DATA_HOME:-$HOME/.local/share}/garmin-runcoach/.venv"
"${XDG_DATA_HOME:-$HOME/.local/share}/garmin-runcoach/.venv/bin/pip" install garminconnect
```

The wrapper auto-detects this venv. Or point at any other venv via `GARMIN_RUNCOACH_PYTHON=/path/to/python`.

## Commands

```bash
garmin-runcoach sync    [--days N] [--out PATH]
garmin-runcoach summary [--in PATH] [--out PATH]
garmin-runcoach all     [--days N]
garmin-runcoach help
```

`sync` and `summary` default `--out` / `--in` to `$DATA_DIR/garmin-{latest.json,summary.md}`.

## Coaching workflow

If the user asks to read/sync Garmin data, 复盘, or arrange training:
1. Run `garmin-runcoach all --days N` (or confirm `garmin-latest.json` is fresh).
2. Read `garmin-summary.md`.
3. Base advice primarily on the summary.

## Troubleshooting

- **`429` from Garmin** — rate limit. Wait and retry; don't misreport as credential failure unless you've actually verified that mode.
- **Login error** — check `~/.netrc` is `chmod 600`, host is exactly `connect.garmin.com`, password not stale.
- **`no Python interpreter found`** — install `python3` or set `GARMIN_RUNCOACH_PYTHON=/path/to/python`.
- **`ModuleNotFoundError: garminconnect`** — in your chosen Python: `pip install garminconnect`.
- If sync succeeds, trust the new files; do not fall back to stale snapshots.
