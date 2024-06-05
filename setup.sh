#!/bin/bash

final() {
    rm "$have" "$skip" "$take" >/dev/null 2>&1
}

fail() {
    echo
    echo "FAILED TO SETUP"
    final
    exit 1
}

echo "checking dependencies..."
have=$(mktemp)
raco pkg show -a | cut -d' ' -f 2 > "$have"

skip=$(mktemp)
take=$(mktemp)

for i in \
    crypto-lib \
    threading-lib \
; do
    grep "^$i" "$have" >/dev/null && echo $i >> "$skip" || echo $i >> "$take"
done

if [ -s "$skip" ]
then
    echo "skipping installations for already-present packages: " $(cat "$skip")
fi
if [ -s "$take" ]
then
    echo "installing dependencies..."
    echo raco pkg install $(cat "$take")
    raco pkg install $(cat "$take") || fail
fi

if grep "^rr$" "$have" >/dev/null
then
    echo "self-collection already ready, skipping..."
else
    echo "setting up self-collection..."
    cd rr || fail
    raco pkg install || fail
fi

final
echo
echo "done!"
