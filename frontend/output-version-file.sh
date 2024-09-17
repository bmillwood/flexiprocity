#!/usr/bin/env bash
if [ -n "$1" ]
then
  version=$1
else
  version=$("$(git rev-parse --show-toplevel)"/frontend/privacy-policy-version.sh)
fi
echo "const latestPrivacyPolicy = '$version';"
