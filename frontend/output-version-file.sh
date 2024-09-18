#!/usr/bin/env bash
if [ -n "$1" ]
then
  version=$1
else
  version=$(bash "$(git rev-parse --show-toplevel)"/frontend/privacy-policy-version.sh)
fi
[ -n "$version" ] || exit 1
echo "const latestPrivacyPolicy = '$version';"
