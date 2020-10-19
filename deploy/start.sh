#!/usr/bin/env bash

# This is a sample docker entrypoint. It is used by the provided docker image
# to start up the backend.

# Relevant parts:
#  1. Start nginx AND our application
#  2. Relay SIGTERM and SIGINT to our haskell application so the docker container
#     can be stopped properly

# Start nginx for routing
nginx &

cd "/home/librarium"

echo "Running in $PWD"

# Start our backend
sudo -Eu librarium /home/librarium/backend "$1" "$2" "$3" &

# Save its PID so we can relay kill signals
HASKELL_PID="$!"

# The shell does not pass on exit signals, so we do it manually
# This ensures haskell is killed properly and the container can restart.
trap "kill $HASKELL_PID" exit INT TERM

# Wait for the subprocesses (= spawned by &) to finish
wait
