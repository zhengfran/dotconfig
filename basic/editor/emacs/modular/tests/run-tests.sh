#!/usr/bin/env bash
# Run the project-tracking ERT suite headless.
#
# Loads the full modular init first (so the global `org-todo-keywords' with
# both the Task and Issue sequences are active, and the tracking functions in
# denote-config.el are defined), then runs the ERT tests in batch.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
modular="$(dirname "$here")"

emacs --batch \
      --init-directory "$modular" \
      -l init.el \
      -l "$here/project-tracking-tests.el" \
      -f ert-run-tests-batch-and-exit
