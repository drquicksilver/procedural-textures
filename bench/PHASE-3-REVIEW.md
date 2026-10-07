# Phase 2/3 review follow-up — 2026-10-06

The five review findings are corrected without changing document semantics,
image goldens or image tolerances. Twelve reference-generated GPU cases cover
constant ramp endpoints/interiors/composition for alpha -0.5, 0.5 and 2, plus
fBm zero/negative amplitude totals for all three styles. Instrumented shader
checks verify that an opaque covering layer performs zero hidden warps, while
visible matching branches share one evaluation and displaced domains stay
independent. Custom and empty gallery libraries build with valid preview links.

Compiler types now describe the validated, resolved existing language. Parameter
packing and GLSL decoding share layout constants and documented lane meanings.
Structural validation rules come from Haskell metadata separately from sliders.
This does not redesign the document format or choose Phase 4's core model.

## Cold-start investigation

Hardware Apple M1 Pro; Chrome 154.0.8037.57 / ANGLE Metal and Firefox 156.0.1 /
Apple M1. Raw [action traces and compile experiments](phase3-review/) are
retained. Browser processes/profiles were fresh, but OS/driver shader caches
were not cleared. These are bounded exploratory measurements, not a controlled
before/after speedup study. The baseline corresponds to the compiler-boundary
commit `58d856f`; the final run also includes the scheduling, thumbnail and
link-first changes. Do not attribute differences between runs solely to code.

The actual editor probe opens the library, selects Cumulus and Marble, then
changes the texture type to fBm. It records animation-frame gaps, instrumented
WebGL calls, busy-indicator opacity, and first viewer presentation through a DOM
mutation observer. It uses the built static app, not a minimal shader page.
Action `elapsedMs` includes an intentional 80 ms observation tail and browser
automation overhead; use `firstPresentationMs` for the first viewer update.
That is application presentation/submission, not a GPU-completion fence. The
early baseline probe lacks the final opacity/presentation/host-timing fields;
its frame flags and GL-call traces are retained as exploratory evidence.
Library opening may coincide with unrelated viewer refinement; a viewer
presentation during that action is not the dialog-opening time.

| Browser / action | First viewer update (ms) | Host generation (ms) | Compile/link (ms) | Render submission (ms) |
| --- | ---: | ---: | ---: | ---: |
| Chrome / Cumulus | 31 | 1.7 | 7.8 | 10.1 |
| Chrome / Marble | 31.6 | 0.6 | 6.6 | 7.8 |
| Chrome / structural fBm edit | 32.1 | 0.2 | 6.9 | 7.6 |
| Firefox / Cumulus | 65 | 0 | 18 | 23 |
| Firefox / Marble | 53 | 0 | 9 | 11 |
| Firefox / structural fBm edit | 32 | 1 | 7 | 9 |

Render submission includes compile/link, so these columns must not be summed.
Host generation is validation/resolution and shader/parameter construction;
feedback-key computation is earlier UI work and is included in action latency,
not this column. Firefox's millisecond clock makes zero a below-resolution
measurement. The initial Chrome fBm edit included a 227 ms long task and a
158.1 ms link-status call. The final cache-influenced Chrome run observed no
long tasks for these actions. Firefox lacks the Long Tasks API on this backend;
its empty long-task array does not demonstrate absence of stalls. Its recorded
frame gaps and instrumented calls remain usable. The earlier 1.22-second cold
render limitation remains documented; this study does not establish its removal.

## Shipped responsiveness changes

- Structural changes yield a rendering opportunity before GPU work and show
  preparation feedback immediately. The previous 150 ms appearance delay stays
  on ordinary warm activity but is removed for structural preparation. A browser
  regression arms at the actual edit and identifies the viewer shader, verifying
  a visible prior frame before compilation. Layout effects commit preparation
  state before the paint opportunity; scheduler tests retain one-frame warm work and cancellation.
- Thumbnail warming is limited to visible/nearby cards (128 px observer margin),
  preserving the existing debounce, viewer priority, eight-program bound and
  protected viewer program. Offscreen work is cancelled; scrolling into view
  renders the card. Browsers without IntersectionObserver retain the prior
  deferred behavior. A browser regression checks both offscreen deferral and
  scrolling; deterministic unit tests verify deferral, debounce and cancellation.
  This avoids speculative warming of the whole library.
- Both shaders are submitted and linked before querying status. Compile-status
  queries and annotated shader logs are obtained only after a link failure.
  This follows the [Khronos extension specification's best-practice guidance](https://registry.khronos.org/webgl/extensions/KHR_parallel_shader_compile/).
  It removes premature waits without changing rendering or failure diagnostics.

## Parallel compilation decision

Chrome exposes KHR_parallel_shader_compile; the tested Firefox backend does
not. The separate compile probe rotates eager status checks, link-first checks,
and non-blocking completion polling over three previously unused transparent-
layer structures around Cumulus. Each strategy uses a fresh context and deletes
its objects. Source reuse between strategies still benefits driver caches;
relative total times therefore cannot be interpreted as speedup factors.

When parallel polling was first to see the depth-three source, linking took
151.8 ms while yielding ten animation frames; the longest observed GL call was
below 0.1 ms at this clock resolution. First eager/link-first sources recorded
195.1/151.9 ms blocking calls. Polling is therefore a credible way to keep the
UI responsive during compilation on supporting browsers, not a way to eliminate
the compilation work. This probe does not measure delayed first-execution cost.

The production renderer remains synchronous, with paint-before-work feedback
and selective warming. The tested worst cold-start browser lacks the extension,
and a complete asynchronous production path would need pending-program bounds,
latest-document cancellation, priority, context-loss cleanup, error propagation
and export synchronization. The bounded study does not justify substituting an
experimental path for those lifecycle guarantees. No worker/OffscreenCanvas
rewrite or blanket precompilation was introduced. The probe is retained so a
future asynchronous path can be evaluated against explicit responsiveness and
lifecycle criteria as the Phase 4 language evolves.


## Final verification — 2026-10-07

`stack build` and all 475 Haskell tests pass; all 612 frontend tests and the
production build pass. Chrome's full 135-case GPU suite, mutation checks,
annotated compile-error diagnostics, lazy-warp instrumentation and context
restoration pass. Native Firefox and Safari each pass 135 cases and all 106
reference PNGs, retain at most eight programs and release every tracked object
on disposal; summaries are in [validation.json](phase3-review/validation.json).
The rebuilt static Pages artifact passes all 25 browser workflows beneath the
repository prefix, including delayed readiness, visible feedback before compile,
viewport-limited warming and scroll-to-render, editing/persistence, exports,
context restoration, unavailable WebGL2 and editor/gallery navigation.

The released application revision is `af33480`. Its [CI run](https://github.com/drquicksilver/procedural-textures/actions/runs/37581608615)
and [Pages release](https://github.com/drquicksilver/procedural-textures/actions/runs/37581608604)
both passed, including Linux software-GPU sample/image comparisons and the
assembled-artifact workflows. All 25 workflows also passed against the
[public deployment](https://drquicksilver.github.io/procedural-textures/) after
publishing (92.6 seconds). The final preparation regression identifies viewer
compilation, excluding unrelated thumbnail work; no tolerance or rendering gate
was relaxed to resolve the earlier release-check failure.
