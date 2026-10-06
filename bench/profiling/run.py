#!/usr/bin/env python3
"""Reproduce the focused profile against the current JSON examples and evaluator."""
import argparse
import csv
from pathlib import Path
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[2]
HEADER = 'example,stage,size,batch,iterations,wall_ms,cpu_ms,gc_cpu_ms,allocated_mb,collections_per_iteration\n'


def run(command, **kwargs):
    return subprocess.run(command, cwd=ROOT, check=True, **kwargs)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--out', type=Path, default=ROOT / 'out/profiling')
    parser.add_argument('--experiments', action='store_true',
                        help='also measure isolated inlining and opaque-layer variants')
    args = parser.parse_args()
    if args.experiments and '{-# INLINE transformOctave #-}' in (ROOT / 'src/Texture.hs').read_text():
        parser.error('--experiments is the historical pre-optimisation study; use its recorded worktree patches to reproduce it. The current evaluator already includes these changes.')
    output = args.out.resolve()
    output.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix='texture-profile-') as directory:
        work = Path(directory)
        normal = work / 'normal'
        core = work / 'core'
        normal.mkdir()
        (core / 'src').mkdir(parents=True)
        run(['stack', 'exec', 'ghc', '--', '-O1', '-threaded', '-rtsopts',
             '-package', 'procedural-textures', '-outputdir', str(normal),
             'bench/profiling/Profile.hs', '-o', str(normal / 'probe')])
        run([str(normal / 'probe'), 'export-core', str(core / 'Core.hs')])
        # Texture depends on Render only for this synonym. Replacing the import
        # with the identical alias avoids a profiled JuicyPixels/vector rebuild.
        # All evaluator algorithms and parameters remain unchanged.
        for name in ['Texture', 'ColourRamps', 'OkLab', 'Colours', 'Perlin', 'Vector3']:
            source = (ROOT / 'src' / (name + '.hs')).read_text()
            if name == 'Texture':
                assert source.count('import Render (ImageFn)\n') == 1
                source = source.replace('import Render (ImageFn)\n', '')
                source = source.replace('data Texture\n',
                    'type ImageFn = Double -> Double -> Colour\n\ndata Texture\n', 1)
            (core / 'src' / (name + '.hs')).write_text(source)
        run(['stack', 'exec', 'ghc', '--', '-O1', '-prof', '-fprof-late', '-rtsopts',
             '-i' + str(core / 'src'), '-outputdir', str(core), str(core / 'Core.hs'),
             '-o', str(core / 'probe')])
        # Fixed survey cohort from the 2026-10-05 full-library baseline.
        names = ['cumulus', 'moss', 'rust', 'ice', 'water-ripples', 'tiger-fur',
                 'jupiter', 'marble', 'wood-knot', 'mountains']
        with (output / 'stages-n10.csv').open('w') as stream:
            stream.write(HEADER)
            for size in [512, 96]:
                for name in names:
                    for stage in ['render', 'encode', 'combined']:
                        print('normal', name, stage, size, flush=True)
                        iterations = 20 if size == 96 or (name == 'wood-knot' and stage == 'render') else 3
                        result = run([str(normal / 'probe'), name, stage, str(size),
                                      str(iterations), '5',
                                      '+RTS', '-T', '-N10', '-RTS'],
                                     capture_output=True, text=True)
                        stream.write(result.stdout)
                        stream.flush()
        raw = output / 'raw-core'
        raw.mkdir(exist_ok=True)
        with (output / 'core-costs.csv').open('w') as stream:
            writer = csv.writer(stream)
            writer.writerow(['example', 'cost_centre', 'module', 'time_percent', 'allocation_percent'])
            for name in names:
                print('cost-centres', name, flush=True)
                run([str(core / 'probe'), name, '512', '3', '+RTS', '-p',
                     '-po' + str(raw / name), '-RTS'])
                lines = (raw / (name + '.prof')).read_text().splitlines()
                start = next(i for i, line in enumerate(lines) if line.startswith('COST CENTRE ')) + 2
                for line in lines[start:]:
                    if not line.strip():
                        break
                    words = line.split()
                    writer.writerow([name, words[0], words[1], words[-2], words[-1]])
        if args.experiments:
            experiments(work, output, core, names)


def experiments(work, output, core, names):
    binding = next(line for line in (core / 'Core.hs').read_text().splitlines()
                   if line.startswith('textures = '))
    source = (ROOT / 'bench/profiling/Profile.hs').read_text()
    source = source.replace('import Examples (Example (..), loadExamples, defaultExamplesDirectory)\n',
                            'import ColourRamps\n')
    source = source.replace('import RampLibrary (loadRampLibrary, defaultRampsDirectory)\n', '')
    source = source.replace('import Resolve (resolveDocument)\n', '')
    source = source.replace('import Texture (Texture, textureToImageFn)',
                            'import Texture (Texture(..), NoiseStyle(..), textureToImageFn)')
    start = source.index('  ramps <- loadRampLibrary')
    end = source.index('  case args of', start)
    source = source[:start] + '  let ' + binding + '\n' + source[end:]
    verifier = '''{-# LANGUAGE PackageImports #-}
module Main (main) where
import Texture
import ColourRamps
import qualified "procedural-textures" Texture as Original
import Examples
import RampLibrary
import Resolve
import Render
import Codec.Picture (Image (imageData))
import Control.Monad (forM_)
''' + binding + '''
main = do
  ramps <- loadRampLibrary defaultRampsDirectory
  examples <- loadExamples defaultExamplesDirectory
  forM_ textures $ \\(name,texture) -> do
    let [example] = [e | e <- examples, exampleId e == name]
        original = either error id (resolveDocument ramps (exampleDocument example))
        a = imageData (renderImage 512 512 (textureToImageFn texture))
        b = imageData (renderImage 512 512 (Original.textureToImageFn original))
    if a == b then putStrLn (name <> ": byte-identical") else fail (name <> ": changed pixels")
'''
    for variant in ['base', 'inline', 'opaque']:
        directory = work / variant
        (directory / 'src').mkdir(parents=True)
        for module in ['Texture', 'ColourRamps', 'OkLab', 'Colours', 'Perlin', 'Vector3']:
            text = (core / 'src' / (module + '.hs')).read_text()
            if module == 'Texture' and variant != 'base':
                text = text.replace('transformOctave ::',
                                    '{-# INLINE transformOctave #-}\ntransformOctave ::', 1)
            if module == 'Texture' and variant == 'opaque':
                original = 'blend (r1, g1, b1, a1) (r2, g2, b2, a2) ='
                assert text.count(original) == 1
                text = text.replace(original, '''blend top@(_, _, _, a1) bottom
  | a1 == 1.0 = top
  | otherwise = blendGeneral top bottom

blendGeneral :: Colour -> Colour -> Colour
blendGeneral (r1, g1, b1, a1) (r2, g2, b2, a2) =''', 1)
            (directory / 'src' / (module + '.hs')).write_text(text)
        (directory / 'Probe.hs').write_text(source)
        (directory / 'Verify.hs').write_text(verifier)
        for target, filename, build in [('probe', 'Probe.hs', directory),
                                         ('verify', 'Verify.hs', directory / 'verify-build')]:
            run(['stack', 'exec', 'ghc', '--', '-O1', '-threaded', '-rtsopts',
                 '-package', 'procedural-textures', '-i' + str(directory / 'src'),
                 '-outputdir', str(build), str(directory / filename),
                 '-o', str(directory / target)])
        result = run([str(directory / 'verify'), '+RTS', '-N10', '-RTS'],
                     capture_output=True, text=True)
        (output / ('pixels-' + variant + '.txt')).write_text(result.stdout)
    with (output / 'experiments.csv').open('w') as stream:
        stream.write('variant,' + HEADER)
        for name in names:
            for variant in ['base', 'inline', 'opaque']:
                print('experiment', name, variant, flush=True)
                result = run([str(work / variant / 'probe'), name, 'combined', '512',
                              '3', '5', '+RTS', '-T', '-N10', '-RTS'],
                             capture_output=True, text=True)
                stream.write(''.join(variant + ',' + line + '\n'
                                     for line in result.stdout.splitlines()))
                stream.flush()


if __name__ == '__main__':
    main()
