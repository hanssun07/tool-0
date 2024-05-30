#!/bin/bash

fail() {
    echo
    echo "FAILED TO SETUP"
    exit 1
}

echo "installing dependencies..."
echo raco pkg install \
    crypto-lib \
    threading-lib \
    || fail

echo "setting up self-collection..."
cd rr || fail
raco pkg install || fail
