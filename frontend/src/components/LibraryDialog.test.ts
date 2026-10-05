import { describe, expect, it } from 'vitest'
import type { Example } from '../types'
import { groupByCategory } from './LibraryDialog'

const example = (id: string, category?: string): Example => ({
  id,
  document: { version: 2, name: id, description: '', ...(category ? { category } : {}), texture: { type: 'flat', colour: '#000000ff' } },
})

describe('groupByCategory', () => {
  it('orders known categories first, then others, then uncategorised', () => {
    const groups = groupByCategory([example('a', 'weird'), example('b', 'pattern'), example('c'), example('d', 'natural'), example('e', 'pattern')])
    expect(groups.map((g) => [g.heading, g.members.map((m) => m.id)])).toEqual([
      ['Natural', ['d']],
      ['Pattern', ['b', 'e']],
      ['Weird', ['a']],
      ['Other', ['c']],
    ])
  })
})
