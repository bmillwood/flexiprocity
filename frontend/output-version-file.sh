#!/usr/bin/env bash
echo "const latestPrivacyPolicy = '$("$(git rev-parse --show-toplevel)"/frontend/privacy-policy-version.sh)';"
