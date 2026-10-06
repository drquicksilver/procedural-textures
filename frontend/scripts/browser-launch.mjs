// Keep image/sample and editor runners on the same verified software backend.
export const softwareBackend = ['swiftshader', 'llvmpipe'].includes(process.env.GPU_BACKEND) ? process.env.GPU_BACKEND : null
export function browserArgs() {
  const args = process.env.CI ? ['--no-sandbox'] : []
  // Software JIT may exceed a hardware-oriented watchdog; suite/job timeouts remain.
  if (softwareBackend === 'swiftshader') args.push('--use-gl=angle', '--use-angle=swiftshader', '--enable-unsafe-swiftshader', '--disable-gpu-watchdog')
  if (softwareBackend === 'llvmpipe') args.push('--use-gl=angle', '--use-angle=gl', '--ignore-gpu-blocklist', '--disable-gpu-watchdog')
  return args
}
export function checkBackend(actual) {
  if (softwareBackend && !(softwareBackend === 'swiftshader' ? /SwiftShader/i : /llvmpipe/i).test(actual)) {
    throw new Error(`Expected ${softwareBackend}, got ${actual}`)
  }
}
