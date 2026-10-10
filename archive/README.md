# Archive

Configs for tools I no longer use, kept for reference. Nothing in here is
linked by `scripts/setup-config.sh`, so it is never live.

To bring one back, `git mv` it to its old path and restore its block in
`setup-config.sh` (`git log --follow <path>` shows its full history).

## Retired

| Tool | Path | Platform | Used | Replaced by |
|---|---|---|---|---|
| AeroSpace | `wm/aerospace/` | macOS | 2024-12-25 – 2026-10-10 | OmniWM (`tools/wm/omniwm`) |

## Removed before this archive

These were deleted outright; restore them from git with
`git checkout <commit>^ -- <path>`.

| Tool | Path | Removed in |
|---|---|---|
| AstroNvim | `nvim/` | `a32c2f3` (2024-12-24) |
| Rime (pre-wanxiang) | `Rime/` | `b764672` (2022-11-25) |
