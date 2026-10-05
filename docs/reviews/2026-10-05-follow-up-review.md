# Follow-up review — 2026-10-05

Reviewed all 14 commits since `f94809b`, through `970ed05`, in an isolated worktree. The two latest merges have the same file tree as the tested snapshot at `4ccab73`. The active working checkout was untouched during the review.

## Findings

1. **P2 — Reopening the current library document can discard pending edits.** `frontend/src/app.tsx:250` flushes the current document, then opens the older `entry.document` captured by the library card. Saving Columns `9`, changing it to `10`, and reopening its card before autosave was reproduced reverting the editor to `9` and resetting undo history. This is a remaining persistence gap, rather than a newly introduced regression. After flushing, fetch the entry again—or leave the current document open when its own card is selected.

2. **P2 — Partial saves create duplicate example copies on retry.** `frontend/src/library.ts:147` writes the library entry before the working-state record. If the second write fails, `commit()` throws without returning the new library ID. The retry still treats the document as an example and creates another copy. A failure limited to the working-state write was reproduced leaving two copies instead of one after recovery. Preserve the allocated ID across partial failures, or make the save operation transactional. Repeated failures can consume more of the storage space needed for recovery.

## Assessment of the changes

The earlier review findings have fixes and useful regression coverage. The bounded body reader, focused-field synchronization, integer normalization, readiness polling, configurable proxy port, and CI browser job are all sensible changes.

The new functionality follows the expanded plan well:

- **1.12:** concrete named definitions avoid reference cycles; backend resolution reports missing paths; saved ramps are copied into documents; subtree replacement carries named ramps and resolves clashes.
- **1.13:** fractal noise is integrated through the schema, examples have golden coverage, and the Clouds/Marble image changes are explicitly documented.
- The approximate ramp-vector comparison is an appropriate fix for platform-dependent floating-point differences.

## Follow-ups for the plan and measurements

- `PLAN.md:274`, in milestone 2.2, still assigns JSON version 2 to 3D documents. Version 2 now means 2D documents with named ramps; the 3D migration should introduce a new version and cover both existing versions.
- Refresh the performance baseline for the expanded library. The recorded claim that Marble is the most expensive example predates the new fractal and layered textures; this review did not verify that it still holds.

## Validation

- `stack build` passed.
- All 292 Haskell tests passed.
- All 131 frontend unit tests passed.
- `npm run build` passed.
- All ten supplied browser tests passed.
- Two additional browser checks in the isolated worktree reproduced the persistence findings above.
- No production changes or commits were made during the review.

Line references describe the reviewed file tree at `4ccab73`, identical to `970ed05`.
