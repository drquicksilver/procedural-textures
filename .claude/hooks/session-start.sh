#!/bin/bash
# Prepare a Claude Code cloud session: install Stack and the snapshot's GHC,
# prebuild the Haskell dependencies and install the frontend's npm packages so
# `stack build`, `stack test` and `make test` work straight away.
#
# Stack's own installer and GitHub release downloads are not reachable from
# the cloud environment, so Stack comes from ghcup, which (like the GHC that
# Stack installs) downloads only from downloads.haskell.org.
set -euo pipefail

if [ "${CLAUDE_CODE_REMOTE:-}" != "true" ]; then
  exit 0
fi

export GHCUP_SKIP_UPDATE_CHECK=1
export PATH="$HOME/.ghcup/bin:$PATH"
# Gallery and site output contain non-ASCII text.
export LANG=C.UTF-8

if ! command -v ghcup > /dev/null; then
  mkdir -p "$HOME/.ghcup/bin"
  curl -fsSL https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup -o "$HOME/.ghcup/bin/ghcup"
  chmod +x "$HOME/.ghcup/bin/ghcup"
fi

if ! command -v stack > /dev/null; then
  ghcup install stack --set
fi

cd "$CLAUDE_PROJECT_DIR"
stack setup
stack build --only-dependencies --test --bench

# `npm ci`, like the Makefile, never rewrites the lockfile.
npm --prefix frontend ci --no-audit --no-fund

if [ -n "${CLAUDE_ENV_FILE:-}" ]; then
  echo "export PATH=\"$HOME/.ghcup/bin:\$PATH\"" >> "$CLAUDE_ENV_FILE"
  echo "export GHCUP_SKIP_UPDATE_CHECK=1" >> "$CLAUDE_ENV_FILE"
  echo "export LANG=C.UTF-8" >> "$CLAUDE_ENV_FILE"
fi
