#!/usr/bin/env bash
# Mechanically verify the FormalMinfern Coq development.
#
# Wraps `make` inside `formal/coq/` so the entire project (skeleton in
# Phase 0, proofs as later phases land) is type-checked by the Coq
# kernel.  Any `Admitted` is reported as a build *warning* by Coq and
# tallied at the end so the maintainer can see how many proof
# obligations remain.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="${REPO_ROOT}/formal/coq"

if [ ! -d "${PROJECT_DIR}" ]; then
  echo "error: Coq project directory not found at ${PROJECT_DIR}" >&2
  exit 1
fi

if ! command -v coqc >/dev/null 2>&1; then
  echo "error: 'coqc' not found on PATH.  Install Coq:" >&2
  echo "  Debian/Ubuntu : sudo apt-get install coq" >&2
  echo "  macOS         : brew install coq" >&2
  echo "  opam          : opam install coq" >&2
  exit 127
fi

echo "Coq version: $(coqc --version | head -1)"
echo "Building ${PROJECT_DIR} ..."

cd "${PROJECT_DIR}"
make "$@"

# Tally outstanding `Admitted`s — each is a tracked proof obligation.
admit_count=$(grep -RE '^[[:space:]]*Admitted\.' theories/ 2>/dev/null | wc -l || true)
if [ "${admit_count}" -gt 0 ]; then
  echo
  echo "=========================================================="
  echo "  ${admit_count} outstanding 'Admitted' lemma(s) in theories/"
  echo "=========================================================="
fi

echo "Build OK."
