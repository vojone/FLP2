#!/bin/bash

shopt -s extglob

TEST_DIR="tests/moves/*"

for CASE in $TEST_DIR; do
    echo -n "$CASE: "

    INPUT="$CASE/$(ls "$CASE" | grep ".*.in")"
    TARGET="$CASE/$(ls "$CASE" | grep ".*.out")"
    SCRIPT="$CASE/$(ls "$CASE" | grep ".*.pl")"

    if [ ! -f "$INPUT" ] || [ ! -f "$TARGET" ] || [ ! -f "$SCRIPT" ]; then
        echo "Incomplete test case!"
    else
        RESULT=$(swipl -s "$SCRIPT" <"$INPUT" 2>/dev/null)
        DIFFS=$(diff -Z "$TARGET" <(echo "$RESULT"))

        if [ -z "$DIFFS" ]; then
            printf "OK"
        else
            printf "FAIL\nExpected:\n%s\nGot:\n%s\n" "$(cat "$TARGET")" "$RESULT"
        fi
    fi

    echo
done

