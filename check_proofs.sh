#!/usr/bin/env bash
# Mechanically verify the FormalJSTypes Lean proofs.
#
# This script wraps `lake build` so the entire formalization (syntax,
# small-step semantics, typing relation, and the Progress / Preservation
# theorem statements) is type-checked by Lean.  Any `sorry` left in the
# proofs will surface as a build warning while still passing kernel
# verification of all definitions.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="${REPO_ROOT}/formal"

if [ ! -d "${PROJECT_DIR}" ]; then
  echo "error: Lean project directory not found at ${PROJECT_DIR}" >&2
  exit 1
fi

# Ensure elan-managed toolchains are on PATH if installed in the default
# location, without overriding an explicit caller-provided PATH.
if [ -d "${HOME}/.elan/bin" ]; then
  export PATH="${HOME}/.elan/bin:${PATH}"
fi

if ! command -v lake >/dev/null 2>&1; then
  echo "error: 'lake' was not found on PATH. Install Lean 4 via elan first:" >&2
  echo "  curl https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh -sSf | sh" >&2
  exit 127
fi

cd "${PROJECT_DIR}"
exec lake build "$@"
