import {expect,it} from 'vitest'
import {fieldWork} from './field-work'
it('nested differential work stays above the preparation budget',()=>{
 let source:unknown={type:'noise'}
 for(let i=0;i<25;i++)source={type:'component',axis:'x',source:{type:'gradient',step:.02,source}}
 expect(fieldWork(source)).toBeGreaterThan(8_000_000)
 expect(fieldWork(source)).toBe(6**25)
})
