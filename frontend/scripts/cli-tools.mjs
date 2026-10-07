import { execFileSync } from 'node:child_process'
import { join, resolve } from 'node:path'
import { root } from './paths.mjs'

// CI downloads the already tested CLI binaries; local development uses Stack's
// existing installation without triggering a build or changing its configuration.
export function cliDirectory() {
  return process.env.TEXTURE_BIN_DIR ? resolve(process.env.TEXTURE_BIN_DIR)
    : join(execFileSync('stack', ['path', '--local-install-root'], { cwd: join(root, '..'), encoding: 'utf8' }).trim(), 'bin')
}
