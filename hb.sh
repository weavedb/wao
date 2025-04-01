#!/bin/sh

# Check if a directory argument was provided
if [ $# -lt 1 ]; then
    echo "Usage: $0 <project_directory>"
    exit 1
fi

# Get the directory from the first argument
TARGET_DIR="$1"

# Navigate to the directory
cd "$TARGET_DIR" || {
    echo "Error: Failed to navigate to $TARGET_DIR"
    exit 1
}

rebar3 shell --eval "hb:start_mainnet(#{ port => 10000, priv_key_location => <<\"wallet.json\">>, bundler_httpsig => <<\"http://localhost:4001\">>, routes => [ #{ <<\"template\">> => <<\"/result/.*\">>, <<\"node\">> => #{ <<\"prefix\">> => <<\"http://localhost:4004\">> } }, #{ <<\"template\">> => <<\"/dry-run\">>, <<\"node\">> => #{ <<\"prefix\">> => <<\"http://localhost:4004\">> } }, #{ <<\"template\">> => <<\"/graphql\">>, <<\"node\">> => #{ <<\"prefix\">> => <<\"http://localhost:4000\">>, <<\"opts\">> => #{ http_client => gun } } }, #{ <<\"template\">> => <<\"/raw\">>, <<\"node\">> => #{ <<\"prefix\">> => <<\"http://localhost:4000\">>, <<\"opts\">> => #{ http_client => gun } } } ] })."
