#!/usr/bin/env python3
"""Summarize garmin-latest.json into a small coach-friendly markdown."""

import argparse
import json
import math
from pathlib import Path
from typing import Any, Dict, Iterable, Optional, Tuple


def _get(d: Any, path: Iterable[str]) -> Any:
    cur = d
    for k in path:
        if not isinstance(cur, dict) or k not in cur:
            return None
        cur = cur[k]
    return cur


def _first_number_in_tree(obj: Any, keys: Tuple[str, ...]) -> Optional[float]:
    if isinstance(obj, dict):
        for k, v in obj.items():
            if k in keys and isinstance(v, (int, float)) and not math.isnan(v):
                return float(v)
            found = _first_number_in_tree(v, keys)
            if found is not None:
                return found
    elif isinstance(obj, list):
        for it in obj:
            found = _first_number_in_tree(it, keys)
            if found is not None:
                return found
    return None


def m_to_km(m: Optional[float]) -> Optional[float]:
    return None if m is None else m / 1000.0


def sec_to_hms(s: Optional[float]) -> str:
    if s is None:
        return "?"
    s = int(round(s))
    h = s // 3600
    m = (s % 3600) // 60
    ss = s % 60
    if h:
        return f"{h}:{m:02d}:{ss:02d}"
    return f"{m}:{ss:02d}"


def pace_str(distance_m: Optional[float], duration_s: Optional[float]) -> str:
    if not distance_m or not duration_s or distance_m <= 0 or duration_s <= 0:
        return "?"
    pace_s_per_km = duration_s / (distance_m / 1000.0)
    mm = int(pace_s_per_km // 60)
    ss = int(round(pace_s_per_km % 60))
    return f"{mm}:{ss:02d}/km"


def parse_args():
    ap = argparse.ArgumentParser()
    ap.add_argument("--in", dest="inp", required=True)
    ap.add_argument("--out", required=True)
    return ap.parse_args()


def main():
    args = parse_args()
    data = json.loads(Path(args.inp).read_text(encoding="utf-8"))

    activities = data.get("activities") or []
    runs = [a for a in activities if _get(a, ("activityType", "typeKey")) == "running"]

    def key_start_gmt(a: Dict[str, Any]) -> str:
        return a.get("startTimeGMT") or a.get("startTimeLocal") or ""

    runs_sorted = sorted(runs, key=key_start_gmt)
    total_dist_m = sum(float(a.get("distance") or 0) for a in runs_sorted)
    total_dur_s = sum(float(a.get("duration") or 0) for a in runs_sorted)
    longest = max(runs_sorted, key=lambda a: float(a.get("distance") or 0), default=None)
    latest = runs_sorted[-1] if runs_sorted else None

    sleep_days = data.get("sleep") or []
    sleep_simple = []
    for item in sleep_days:
        date = item.get("date")
        sdata = item.get("data")
        seconds = (
            _first_number_in_tree(sdata, ("sleepTimeSeconds", "totalSleepSeconds", "sleepSeconds", "totalSleepTimeInSeconds"))
            if sdata is not None
            else None
        )
        sleep_simple.append((date, seconds))

    sleep_simple = [x for x in sleep_simple if x[0] is not None]
    sleep_simple_sorted = sorted(sleep_simple, key=lambda x: x[0])
    last3_sleep = sleep_simple_sorted[-3:]

    fetched_at = data.get("fetched_at_utc")
    r = data.get("range") or {}

    lines = []
    lines.append("# Garmin 摘要（跑步教练）")
    lines.append("")
    lines.append(f"- 数据更新时间(UTC): {fetched_at or '?'}")
    lines.append(f"- 覆盖范围: {r.get('start','?')} → {r.get('end','?')}")
    lines.append("")
    lines.append("## 跑步总览")
    lines.append(f"- 跑步次数: {len(runs_sorted)}")
    lines.append(f"- 总距离: {m_to_km(total_dist_m):.2f} km")
    lines.append(f"- 总时长: {sec_to_hms(total_dur_s)}")
    lines.append(f"- 平均配速(按总量粗算): {pace_str(total_dist_m, total_dur_s)}")

    if longest:
        lines.append("")
        lines.append("## 最长一跑")
        lines.append(
            f"- {longest.get('startTimeLocal') or longest.get('startTimeGMT') or '?'} | {m_to_km(float(longest.get('distance') or 0)):.2f} km | {sec_to_hms(float(longest.get('duration') or 0))} | 配速 {pace_str(float(longest.get('distance') or 0), float(longest.get('duration') or 0))}"
        )

    if latest:
        lines.append("")
        lines.append("## 最近一次跑步")
        dist = float(latest.get("distance") or 0)
        dur = float(latest.get("duration") or 0)
        ahr = latest.get("averageHR")
        mhr = latest.get("maxHR")
        lines.append(f"- {latest.get('activityName','running')} | {latest.get('startTimeLocal') or latest.get('startTimeGMT') or '?'}")
        lines.append(f"- 距离/时长: {m_to_km(dist):.2f} km / {sec_to_hms(dur)} | 配速 {pace_str(dist, dur)}")
        if ahr is not None:
            lines.append(f"- 心率: avg {ahr} / max {mhr if mhr is not None else '?'}")

    lines.append("")
    lines.append("## 最近睡眠（尽力解析，可能因 Garmin 字段变化缺失）")
    if last3_sleep:
        for date, sec in last3_sleep:
            lines.append(f"- {date}: {sec_to_hms(sec)}")
    else:
        lines.append("- (无)")

    outp = Path(args.out)
    outp.parent.mkdir(parents=True, exist_ok=True)
    outp.write_text("\n".join(lines) + "\n", encoding="utf-8")
    print(f"Wrote {outp}")


if __name__ == "__main__":
    main()
