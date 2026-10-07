import { describe, expect, it } from 'vitest'
import type { Example } from '../types'
import { groupByCategory, groupByFamily, matchesExample } from './LibraryDialog'

const example = (id: string, category?: string): Example => ({
  id,
  document: { version: 2, name: id, description: '', ...(category ? { category } : {}), texture: { type: 'flat', colour: '#000000ff' } },
})

describe('groupByCategory', () => {
  it('orders known categories first, then others, then uncategorised', () => {
    const groups = groupByCategory([example('a', 'weird'), example('b', 'pattern'), example('c'), example('d', 'materials'), example('e', 'pattern')])
    expect(groups.map((g) => [g.heading, g.members.map((m) => m.id)])).toEqual([
      ['Materials', ['d']],
      ['Patterns & symmetry', ['b', 'e']],
      ['Weird', ['a']],
      ['Other', ['c']],
    ])
  })
})

it('keeps comparison families adjacent while featuring lower-ranked presets first', () => {
  const guided = (id: string, order: number, family?: string): Example => ({ ...example(id, 'colour'), document: { ...example(id, 'colour').document, guide: { role: 'comparison', tags: ['alpha'], order, ...(family ? { family } : {}) } } })
  const members = groupByCategory([guided('screen', 12, 'Blends'), guided('gradient', 0), guided('other', 11), guided('normal', 10, 'Blends')])[0].members
  expect(members.map((e) => e.id)).toEqual(['gradient', 'normal', 'screen', 'other'])
})

it('searches capabilities and opens matching families without changing their documents', () => {
  const e = example('tile', 'pattern')
  e.document.guide = { role: 'study', tags: ['Voronoi', '3D'], order: 1, family: 'Cell studies', hint: 'Inspect the edge mask.' }
  expect(matchesExample(e, 'voronoi edge')).toBe(true)
  expect(matchesExample(e, '3d cells')).toBe(false)
  expect(matchesExample(e, '  ')).toBe(true)
  expect(groupByFamily([e])).toEqual([{ family: 'Cell studies', members: [e] }])
})
