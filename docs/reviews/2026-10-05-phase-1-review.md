# Review of the morning’s work — 2026-10-05

Reviewed 12 commits through `f94809b`, covering milestones 1.1–1.11, in an isolated worktree. The active working checkout was left untouched during the review.

Phase 1 is feature-complete and follows `PLAN.md` closely, but needs a small hardening pass before Phase 2. The strongest parts are the JSON source of truth, schema-driven inspector, golden coverage, measured rendering improvements, shared ramp vectors, and separate browser suite.

## Findings

1. **P2 — Failed saves can lead to lost edits.** In `frontend/src/app.tsx:134`, an autosave exception shows a toast but clears `pending`, allowing a library document to display “Saved”. This was reproduced by simulating a storage-quota failure. Opening another document (`app.tsx:175`) also continues after `flush()` fails and replaces the current history. Preserve the unsaved state and make switching documents depend on a successful save or an explicit discard.

2. **P2 — Focused undo leaves stale text in the inspector.** `frontend/src/components/fields.tsx:40` suppresses value synchronization while focused. Typing Columns `9` and pressing undo left the input showing `9` after the document reverted to `8`. The hex input uses the same pattern. External changes such as undo/redo need to update the field while preserving support for partially typed input.

3. **P2 — The request-body limit is enforced after reading the body.** `src/Server.hs:139` reads the complete body before checking its length. The configured cap rejects oversized documents but does not bound their read cost. Enforce the limit during body consumption.

4. **P3 — Integer arrow keys still bypass the minimum.** The new steppers correctly constrain their minus buttons, but the keyboard handler in `frontend/src/components/fields.tsx:74` bypasses that rule. ArrowDown was reproduced changing Columns from `1` to `0`. Apply the same normalization to typing, buttons, and keyboard nudges.

5. **P3 — Browser tests have a startup race.** `Makefile:32` waits one second rather than checking server readiness. The first review run failed all six tests with connection-refused errors; the rerun passed. Poll a lightweight endpoint with a deadline.

6. **P3 — Development port overrides break the proxy.** `make dev PORT=8081` changes the backend port, while the proxy in `frontend/vite.config.ts:10` remains fixed at 8080.

Two smaller workflow observations: `make test` still omits the frontend type-check/build despite being offered as the validation shortcut in `AGENTS.md`; the browser suite runs manually but is not wired into CI.

## Alignment with PLAN.md

Milestones 1.1–1.7 substantially implement the planned housekeeping, golden regression suite, JSON documents, rendering server, frontend architecture, interactive performance, and structural editor. Low-resolution requests are coalesced rather than cancelled, which is a reasonable deliberate adjustment. The performance target has recorded measurements, though benchmarks were not rerun during this review.

The later commits close the previous gaps:

- **1.8:** draggable ramp stops and shared evaluator vectors complete the colour/ramp widgets.
- **1.9:** schema-based preview handles cover direct manipulation.
- **1.10:** the library, import/export, migration path, and autosave implement persistence.
- **1.11:** gallery/documentation and the recorded local-only hosting decision cover the wrap-up.

Retain the Phase 1 completion marker, but fix the persistence issue before starting 3D work. The browser tests are a useful addition; extending them to cover focused undo and storage failure would address behaviours that the pure tree/history/library tests miss.

## Validation

- `stack build` passed.
- All 110 Haskell tests passed.
- All 61 frontend unit tests passed.
- `npm run build` passed.
- All six supplied browser smoke tests passed on rerun, after the initial server-startup failure.
- Three additional browser checks in the isolated review worktree reproduced findings 1, 2, and 4.
- Benchmarks were not rerun.

Line references describe the reviewed snapshot at `f94809b`.
