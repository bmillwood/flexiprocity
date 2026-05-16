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

HEADING_RE = re.compile(r"^(#{1,6}) ")
DOC_SECTION_RE = re.compile(r"^##### Documentation\s*$")
DEPS_ITEM_RE = re.compile(r"^- \*\*deps:\*\* ")
DOC_SECTION_LEVEL = 5


def strip(body: str) -> str:
    out = []
    skipping = False
    for line in body.splitlines(keepends=True):
        if skipping:
            heading = HEADING_RE.match(line)
            if heading and len(heading.group(1)) <= DOC_SECTION_LEVEL:
                skipping = False
            elif line.rstrip() == "</details>":
                skipping = False
            else:
                continue
        if DOC_SECTION_RE.match(line):
            skipping = True
            continue
        if DEPS_ITEM_RE.match(line):
            continue
        out.append(line)
    return "".join(out)


if __name__ == "__main__":
    sys.stdout.write(strip(sys.stdin.read()))
