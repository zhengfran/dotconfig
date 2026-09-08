// Minimal Runtime for the vendored adapters. In pi this came from the package
// host; here the two methods the adapters actually use are backed directly by
// node:child_process, which is why the adapters needed no changes to run
// outside pi.
import { execFile } from "node:child_process";
import { access, constants } from "node:fs/promises";
import { delimiter, join } from "node:path";
import type { CommandResult, Runtime } from "./types.ts";

async function isExecutable(path: string): Promise<boolean> {
  try {
    await access(path, constants.X_OK);
    return true;
  } catch {
    return false;
  }
}

export const runtime: Runtime = {
  async resolveCommand(command: string): Promise<string | undefined> {
    if (command.includes("/")) {
      return (await isExecutable(command)) ? command : undefined;
    }
    for (const dir of (process.env.PATH ?? "").split(delimiter)) {
      if (!dir) continue;
      const candidate = join(dir, command);
      if (await isExecutable(candidate)) return candidate;
    }
    return undefined;
  },

  exec(command, args, options = {}): Promise<CommandResult> {
    return new Promise((resolve) => {
      execFile(
        command,
        args,
        {
          signal: options.signal,
          timeout: options.timeoutMs ?? 10_000,
          maxBuffer: options.maxBuffer ?? 1024 * 1024,
          env: options.env ?? process.env,
          encoding: "utf8",
        },
        (error, stdout, stderr) => {
          // A non-zero exit is data for the caller, not an exception: adapters
          // branch on `code` to decide whether to fall back to another source.
          const code =
            error && typeof (error as { code?: unknown }).code === "number"
              ? ((error as { code: number }).code)
              : error
                ? 1
                : 0;
          resolve({ stdout: stdout ?? "", stderr: stderr ?? "", code });
        },
      );
    });
  },
};
