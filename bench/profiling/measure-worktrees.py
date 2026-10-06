#!/usr/bin/env python3
"""Verify and measure six previously built, independent experiment worktrees."""
import argparse
import csv
from pathlib import Path
import random
import subprocess

VARIANTS = ['baseline', 'o2', 'inline', 'opaque', 'shared', 'batch', 'schedule']
EXAMPLES = ['cumulus', 'moss', 'rust', 'ice', 'water-ripples', 'tiger-fur',
            'jupiter', 'marble', 'wood-knot', 'mountains']
HEADER = ['variant', 'block', 'capabilities', 'example', 'stage', 'size', 'batch',
          'iterations', 'wall_ms', 'cpu_ms', 'gc_cpu_ms', 'allocated_mb',
          'collections_per_iteration']


def invoke(root, variant, arguments):
    directory = root / variant
    return subprocess.run([str(directory / 'out/probe/probe')] + arguments,
                          cwd=directory, check=True, capture_output=True, text=True)


def verify(root, output):
    with (output / 'pixel-verification.csv').open('w') as stream:
        writer = csv.writer(stream)
        writer.writerow(['variant', 'size', 'exact_images'])
        for size in [1, 3, 13, 96, 512]:
            directory = root / ('pixels-' + str(size))
            print('snapshot', size, flush=True)
            invoke(root, 'baseline', ['snapshot', str(directory), str(size),
                                     '+RTS', '-N10', '-RTS'])
            for variant in VARIANTS[1:]:
                print('verify', variant, size, flush=True)
                result = invoke(root, variant, ['verify', str(directory), str(size),
                                                '+RTS', '-N10', '-RTS'])
                assert len(result.stdout.splitlines()) == 67
                writer.writerow([variant, size, 67])
                stream.flush()


def sample(root, stream, writer, variant, block, capabilities, example, stage, size, iterations):
    print('measure', stage, size, block, example, variant, capabilities, flush=True)
    result = invoke(root, variant, [example, stage, str(size), str(iterations), '1',
                                   '+RTS', '-T', '-N' + str(capabilities), '-RTS'])
    for row in csv.reader(result.stdout.splitlines()):
        assert len(row) == 10
        writer.writerow([variant, block, capabilities] + row)
    stream.flush()


def cohort(root, stream, stage, size, iterations, blocks, rng):
    writer = csv.writer(stream)
    for block in blocks:
        examples = EXAMPLES.copy()
        rng.shuffle(examples)
        for example in examples:
            order = VARIANTS.copy()
            rng.shuffle(order)
            for variant in order:
                sample(root, stream, writer, variant, block, 10, example, stage, size, iterations)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--root', type=Path, required=True,
                        help='parent containing baseline and six treatment worktrees')
    parser.add_argument('--out', type=Path, default=Path('out/performance-worktrees'))
    args = parser.parse_args()
    root = args.root.resolve()
    output = args.out.resolve()
    output.mkdir(parents=True, exist_ok=True)
    verify(root, output)
    rng = random.Random(20261006)
    with (output / 'measurements.csv').open('w') as stream:
        csv.writer(stream).writerow(HEADER)
        for stage, size, iterations in [('combined', 512, 5), ('render', 512, 5),
                                         ('combined', 96, 40)]:
            cohort(root, stream, stage, size, iterations, range(1, 4), rng)
    # Full factorial controls: vary runtime capabilities for both chunk sizes.
    rng = random.Random(1234)
    with (output / 'scheduling.csv').open('w') as stream:
        writer = csv.writer(stream)
        writer.writerow(HEADER)
        for size, iterations in [(512, 5), (96, 40)]:
            for block in range(1, 4):
                cases = [(example, variant, caps)
                         for example in ['cumulus', 'moss', 'wood-knot']
                         for variant in ['baseline', 'schedule'] for caps in [4, 8, 10]]
                rng.shuffle(cases)
                for example, variant, caps in cases:
                    sample(root, stream, writer, variant, block, caps, example,
                           'combined', size, iterations)
    # Two confirmation rounds for the primary outcome, independently shuffled.
    with (output / 'measurements.csv').open('a') as stream:
        cohort(root, stream, 'combined', 512, 5, [4, 5], random.Random(2026100602))
    print('all measurements complete', flush=True)


if __name__ == '__main__':
    main()
