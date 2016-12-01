#!/bin/sh

set -eu
hash=de8846e03bd35f4987de2121401851224e1e7270bf93dbd77d92466b1006451d
curl -L https://github.com/erlang/rebar3/releases/download/3.1.0/rebar3 -o rebar3
echo "$hash  rebar3" | sha256sum -c
chmod +x rebar3
