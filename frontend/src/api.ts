import type { ShapeOption } from './view'
import type { Example, LibraryRamp, Schema, TextureDocument } from './types'

import metadata from './metadata'
import { processDocument } from './document'

export async function fetchSchema(): Promise<Schema> { return metadata.schema }
export async function fetchExamples(): Promise<Example[]> { return metadata.examples }
export async function fetchRamps(): Promise<LibraryRamp[]> { return metadata.ramps }

/** Validate, migrate and canonicalise locally, including reference checks. */
export async function migrateDocument(document: unknown): Promise<TextureDocument> { return processDocument(document) }
export async function fetchShapes(): Promise<ShapeOption[]> { return metadata.shapes.map(({ id, label }) => ({ id, label })) }
