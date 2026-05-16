#!/usr/bin/env python3
"""Strip noise from a Renovate PR body.

Reads the PR body on stdin, writes the filtered body to stdout.

Currently strips:
  * List items of the form `- **deps:** ...`.
  * Sections whose heading is `##### Documentation`, up to (but not
    including) the next heading at the same or higher level, or the
    next `</details>` line, or end of input.
"""
import re
import sys

def strip(body: str) -> str:
    out = []
    skip_section = None
    for line in body.splitlines(keepends=True):
        for hashes in range(len(line)):
            if line[hashes] != '#':
                break
        if skip_section is not None:
            if 0 < hashes <= skip_section or line.rstrip() == "</details>":
                skip_section = None
            else:
                continue
        if hashes >= 5 and line[hashes:].strip() == "Documentation":
            skip_section = hashes
            continue
        if line.startswith("- **deps:** "):
            continue
        out.append(line)
    return "".join(out)


if __name__ == "__main__":
    sys.stdout.write(strip(sys.stdin.read()))
