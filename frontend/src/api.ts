import { viewQuery, type ViewOptions, type ShapeOption } from './view'
import type { Example, LibraryRamp, Schema, TextureDocument } from './types'

/** An error reported by the server, with its explanation. */
export class ApiError extends Error {
  readonly status: number

  constructor(status: number, message: string) {
    super(message)
    this.status = status
  }
}

async function check(response: Response): Promise<Response> {
  if (response.ok) return response
  let message = `${response.status} ${response.statusText}`
  try {
    const body = await response.json()
    if (body && typeof body.error === 'string') message = body.error
  } catch {
    // Not JSON; keep the status line.
  }
  throw new ApiError(response.status, message)
}

import metadata from './metadata'
import { processDocument } from './document'

export async function fetchSchema(): Promise<Schema> { return metadata.schema as Schema }
export async function fetchExamples(): Promise<Example[]> { return metadata.examples as Example[] }
export async function fetchRamps(): Promise<LibraryRamp[]> { return metadata.ramps as LibraryRamp[] }

/** Render a document to a PNG blob of size×size pixels. */
export async function renderDocument(
  document: TextureDocument,
  size: number,
  signal?: AbortSignal,
  view?: ViewOptions,
): Promise<Blob> {
  const query = view ? `&${viewQuery(view)}` : ''
  const response = await fetch(`/api/render?size=${size}${query}`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(document),
    signal,
  })
  return (await check(response)).blob()
}

/** Validate, migrate and canonicalise locally, including reference checks. */
export async function migrateDocument(document: unknown): Promise<TextureDocument> { return processDocument(document) }
export async function fetchShapes(): Promise<ShapeOption[]> { return metadata.shapes.map(({ id, label }) => ({ id, label })) }
