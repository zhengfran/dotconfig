#!/usr/bin/env python3
"""Fetch Garmin activities + sleep summary and write to JSON.

Usage:
  source /home/flanlc/.openclaw/workspace/.venv/bin/activate
  export GARMIN_EMAIL=... GARMIN_PASSWORD=***
  python garmin_fetch.py --days 14 --out /home/flanlc/.openclaw/workspace/runcoach/data/garmin-latest.json
"""

from garminconnect import Garmin
import argparse
import json
import os
from datetime import datetime, timedelta, timezone


def utc_today():
    return datetime.now(timezone.utc).date()


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--days", type=int, default=14)
    ap.add_argument("--out", required=True)
    args = ap.parse_args()

    email = os.environ.get("GARMIN_EMAIL")
    password = os.environ.get("GARMIN_PASSWORD")
    if not email or not password:
        raise SystemExit("Missing GARMIN_EMAIL/GARMIN_PASSWORD env vars")

    client = Garmin(email, password)
    client.login()

    start = utc_today() - timedelta(days=args.days)
    end = utc_today()
    activities = client.get_activities_by_date(str(start), str(end))

    sleep = []
    d = start
    while d <= end:
        try:
            s = client.get_sleep_data(str(d))
        except Exception as e:
            s = {"date": str(d), "error": str(e)}
        sleep.append({"date": str(d), "data": s})
        d += timedelta(days=1)

    out = {
        "fetched_at_utc": datetime.now(timezone.utc).isoformat(),
        "range": {"start": str(start), "end": str(end)},
        "activities": activities,
        "sleep": sleep,
    }

    os.makedirs(os.path.dirname(args.out), exist_ok=True)
    with open(args.out, "w", encoding="utf-8") as f:
        json.dump(out, f, ensure_ascii=False, indent=2)

    print(f"Wrote {args.out} (activities={len(activities)}, sleep_days={len(sleep)})")


if __name__ == "__main__":
    main()
