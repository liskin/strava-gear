#!/usr/bin/env bash

set -eu -o pipefail
set -x

make
make -C dep/strava-oauth

. ./.private.sh

trap 'pkill -P $$; wait' EXIT

# TODO: refresh token if possible
( sleep 1; sensible-browser http://127.0.0.1:5042/authorize; ) &
read -r token < <(cd dep/strava-oauth && exec ./.venv/bin/python api.py)
access_token=$(<<<"$token" jq -r '.access_token')
export STRAVA_ACCESS_TOKEN="$access_token"

./bin/strava-gear-cmdline
