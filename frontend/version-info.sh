#!/usr/bin/env bash
echo "const latestPrivacyPolicy = '$(git log -n 1 --pretty=format:%h src/PrivacyPolicy.elm)';"
