---
name: openclaw-capture
description: Send quick captures (ideas, activities, thoughts) to the user's OpenClaw inbox. Use this skill whenever the user wants to capture a thought, log an activity, jot something down, or says things like "记录一下", "capture this", "记一下", "capture idea", "capture activity", "log this", "发到openclaw", "记录". Even if the user doesn't mention OpenClaw by name — if they want to quickly save a fleeting thought or note an activity, this is the skill to use.
---

# OpenClaw Capture

Send a message to the user's personal OpenClaw capture inbox via its webhook API.

## How it works

OpenClaw is the user's personal capture tool — think of it as a quick inbox for fleeting thoughts, activity logs, and ideas. The user says something like "记录一下：刚跑完步" and you fire it off to OpenClaw so they don't lose it.

## Sending a capture

Use the Bash tool to POST to the OpenClaw webhook. The auth token lives in the `OPENCLAW_TOKEN` environment variable.

```bash
curl -s -X POST http://openclaw.flan.heiyu.space:18789/hooks/capture \
  -H "Content-Type: application/json" \
  -H "x-openclaw-token: $OPENCLAW_TOKEN" \
  -d '{"message": "<the user's message>"}'
```

## Guidelines

- Extract the core message from what the user said. If they say "记录一下我刚吃完午饭", the message is "刚吃完午饭". Strip the trigger phrase, keep the content.
- Preserve the user's original language — if they write in Chinese, send in Chinese. If English, send in English.
- Keep it short and natural. This is a quick capture, not a formal note.
- If the capture succeeds, confirm briefly (e.g., "Captured." or "已记录."). Don't over-explain.
- If `OPENCLAW_TOKEN` is not set, tell the user to set it: `export OPENCLAW_TOKEN="<token>"` in their shell config.
- If the request fails, show the error and suggest checking the endpoint or token.
