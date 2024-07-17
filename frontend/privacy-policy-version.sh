#!/usr/bin/env bash
git log -n 1 --pretty=format:%h "$(git rev-parse --show-toplevel)/frontend/src/PrivacyPolicy.elm"
