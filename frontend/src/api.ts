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

export async function fetchSchema(): Promise<Schema> {
  return (await check(await fetch('/api/schema'))).json()
}

export async function fetchExamples(): Promise<Example[]> {
  return (await check(await fetch('/api/examples'))).json()
}

export async function fetchRamps(): Promise<LibraryRamp[]> {
  return (await check(await fetch('/api/ramps'))).json()
}

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

/** Bring a document of any supported version up to date, in canonical form. */
export async function migrateDocument(document: unknown): Promise<TextureDocument> {
  const response = await fetch('/api/migrate', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(document),
  })
  return (await check(response)).json()
}


export async function fetchShapes(): Promise<ShapeOption[]> {
  return (await check(await fetch('/api/shapes'))).json()
}
