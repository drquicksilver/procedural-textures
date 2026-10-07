// Experimental compile/link study, not an alternative production renderer.
import { mkdirSync, writeFileSync } from 'node:fs'
import { join } from 'node:path'
import { gpuSession, root } from './gpu-session.mjs'
const session = await gpuSession()
try {
  const report = await session.page.evaluate(async () => {
    const { compileMaterial } = await import('/src/gpu/compiler.ts')
    const { vertexShader } = await import('/src/gpu/shaders.ts')
    const { default: metadata } = await import('/src/metadata.ts')
    const original = metadata.examples.find((e) => e.id === 'cumulus').document
    const results = []
    const strategies = ['eager-status', 'link-first', 'parallel-poll']
    for (let depth = 1; depth <= 3; depth++) {
      let texture = original.texture
      for (let i = 0; i < depth; i++) texture = { type: 'layer', top: { type: 'flat', colour: '#00000000' }, bottom: texture }
      const started = performance.now()
      const compiled = compileMaterial({ ...original, texture }, { shape: 'bitten-cube', renderMode: 'scene' })
      const hostMs = performance.now() - started
      // Rotate which strategy sees each previously unused source first. Driver
      // caches are not controlled; this is a stall probe, not a speedup study.
      for (const strategy of strategies.slice(depth - 1).concat(strategies.slice(0, depth - 1))) {
        const canvas = document.createElement('canvas'), gl = canvas.getContext('webgl2')
        if (!gl) throw new Error('Missing WebGL2')
        const extension = gl.getExtension('KHR_parallel_shader_compile')
        if (strategy === 'parallel-poll' && !extension) { results.push({ depth, strategy, supported: false }); gl.getExtension('WEBGL_lose_context')?.loseContext(); continue }
        const stages = [], shaders = []
        let program
        const measure = (stage, work) => {
          const start = performance.now(), result = work()
          stages.push({ stage, ms: performance.now() - start }); return result
        }
        const begin = performance.now()
        let frames = 0
        try {
          for (const [type, source] of [[gl.VERTEX_SHADER, vertexShader], [gl.FRAGMENT_SHADER, compiled.source]]) {
            const shader = gl.createShader(type); shaders.push(shader)
            gl.shaderSource(shader, source)
            measure('compile-submit', () => gl.compileShader(shader))
            if (strategy === 'eager-status' && !measure('shader-status', () => gl.getShaderParameter(shader, gl.COMPILE_STATUS))) throw new Error(gl.getShaderInfoLog(shader))
          }
          program = gl.createProgram(); for (const shader of shaders) gl.attachShader(program, shader)
          measure('link-submit', () => gl.linkProgram(program))
          if (strategy === 'parallel-poll') {
            while (!measure('completion-poll', () => gl.getProgramParameter(program, extension.COMPLETION_STATUS_KHR))) {
              if (performance.now() - begin > 30000 || gl.isContextLost()) throw new Error('Compile polling failed to complete')
              await new Promise((resolve) => requestAnimationFrame(resolve)); frames++
            }
          }
          if (!measure('link-status', () => gl.getProgramParameter(program, gl.LINK_STATUS))) throw new Error(gl.getProgramInfoLog(program))
          results.push({ depth, strategy, supported: true, hostMs, elapsedMs: performance.now() - begin, framesWhilePending: frames, longestCallMs: Math.max(...stages.map((s) => s.ms)), stages })
        } finally {
          gl.deleteProgram(program); for (const shader of shaders) gl.deleteShader(shader)
          gl.getExtension('WEBGL_lose_context')?.loseContext()
        }
      }
    }
    return { userAgent: navigator.userAgent, results }
  })
  const out = join(root, '../out/cold-start'); mkdirSync(out, { recursive: true })
  writeFileSync(join(out, 'chrome-compile-strategies.json'), JSON.stringify({ browser: await session.browser.version(), backend: session.backend, ...report }, null, 2) + '\n')
  console.log(JSON.stringify(report.results.map(({ stages, ...result }) => result), null, 2))
} finally { await session.close() }
